use dynasmrt::{aarch64::Assembler, DynasmApi, dynasm, DynamicLabel, DynasmLabelApi};
use bit_field::BitField;
use std::cell::Cell;
use std::rc::Rc;
use crate::runtime::Runtime;
use crate::error;
use byteorder::{LittleEndian, ReadBytesExt};
use crate::translation::{VirtualReg, register_map_policy, Translation};
use std::collections::BTreeMap;

pub struct Codegen<'a> {
    a: Assembler,
    base_vpc: u64,
    spill: SpillMachine,
    raw: &'a [u8],
    v_offset_to_translation_offset: Box<[u32]>,
    exception_translation_offset_to_v_offset: BTreeMap<u32, u32>,
    relative_br_labels: Box<[DynamicLabel]>,
}

impl<'a> Codegen<'a> {
    pub fn new(base_vpc: u64, raw: &'a [u8]) -> Codegen<'a> {
        let mut a = Assembler::new().unwrap();
        let max_num_instructions = raw.len() / 2;
        let v_offset_to_translation_offset = vec![std::u32::MAX; max_num_instructions].into_boxed_slice();
        let relative_br_labels = (0..max_num_instructions).map(|_| a.new_dynamic_label()).collect::<Vec<_>>().into_boxed_slice();

        Codegen {
            a,
            base_vpc,
            spill: SpillMachine::new(),
            raw,
            v_offset_to_translation_offset,
            exception_translation_offset_to_v_offset: BTreeMap::new(),
            relative_br_labels,
        }
    }

    pub fn generate(&mut self) {
        let mut cursor = self.raw;
        let mut vpc = self.base_vpc;
        while let Ok(x) = cursor.read_u32::<LittleEndian>() {
            let inst_offset = ((vpc - self.base_vpc) / 2) as usize;
            let label = self.relative_br_labels[inst_offset];
            self.v_offset_to_translation_offset[inst_offset] = self.a.offset().0 as u32;

            dynasm!(self.a
                ; .arch aarch64
                ; =>label
            );
            self.emit_once(vpc, x);
            vpc += 4;
        }
        self.a.commit().expect("Codegen::generate: commit failed");
    }

    pub fn refine(self) -> Translation {
        Translation {
            backing: self.a,
            v_offset_to_translation_offset: self.v_offset_to_translation_offset,
            exception_translation_offset_to_v_offset: self.exception_translation_offset_to_v_offset,
        }
    }

    fn emit_once(&mut self, vpc: u64, inst: u32) {
        match i_op(inst) {
            0b0010011 => {
                // arith-i 64-bit
                let (rd, rs, rd_h, rs_h) = self.spill.map_register_tuple_w_r(&mut self.a, i_rd(inst) as _, i_rs(inst) as _);
                let imm = i_itype_imm(inst);

                self.emit_itype(vpc, inst, rd, rs, imm);

                self.spill.release_register_r(&mut self.a, rs_h);
                self.spill.release_register_w(&mut self.a, rd_h);
            }
            0b0011011 => {
                // arith-i 32-bit
                let funct3 = i_funct3(inst);
                if funct3 != 0b000 && funct3 != 0b001 && funct3 != 0b101 {
                    self.emit_ud(vpc, inst);
                    return;
                }

                let (rd, rs, rd_h, rs_h) = self.spill.map_register_tuple_w_r(&mut self.a, i_rd(inst) as _, i_rs(inst) as _);
                let imm = i_itype_imm(inst);

                self.emit_itype(vpc, inst, rd, rs, imm);

                self.spill.release_register_r(&mut self.a, rs_h);
                self.spill.release_register_w(&mut self.a, rd_h);
            }
            0b1100011 => {
                // conditional branch
                let funct3 = i_funct3(inst);
                let imm = i_btype_imm(inst);

                if funct3 == 0b010 || funct3 == 0b011 {
                    self.emit_ud(vpc, inst);
                } else {
                    // check branch target
                    let b_offset = imm as i32;
                    let dst_pc = ((vpc as i64) + (b_offset as i64)) as u64;

                    if dst_pc < self.base_vpc {
                        self.emit_exception(vpc, error::ERROR_REASON_BRANCH_OOB);
                        return;
                    }

                    let dst_offset = (dst_pc - self.base_vpc) / 2;
                    if dst_offset >= self.relative_br_labels.len() as u64 {
                        self.emit_exception(vpc, error::ERROR_REASON_BRANCH_OOB);
                        return;
                    }

                    let label = self.relative_br_labels[dst_offset as usize].clone();
                    let (rs, rt, rs_h, rt_h) = self.spill.map_register_tuple_r_r(&mut self.a, i_rs(inst) as _, i_rt(inst) as _);
                    self.emit_btype(vpc, inst, rs, rt, label);
                    self.spill.release_register_r(&mut self.a, rs_h);
                    self.spill.release_register_r(&mut self.a, rt_h);
                }
            }
            0b0110111 => {
                // lui
                let imm = i_utype_imm(inst);
                let (rd, rd_h) = self.spill.map_register_w(&mut self.a, i_rd(inst) as _, &[]);

                dynasm!(self.a
                    ; .arch aarch64
                    ; mov W(rd as u32), ((imm << 12) & 0xffff) as u64
                    ; movk W(rd as u32), (imm >> 4) as u32, lsl 16
                    ; sxtw X(rd as u32), W(rd as u32)
                );

                self.spill.release_register_w(&mut self.a, rd_h);
            }
            _ => self.emit_ud(vpc, inst)
        }
    }

    fn emit_btype(&mut self, vpc: u64, inst: u32, rs: usize, rt: usize, label: DynamicLabel) {
        match i_op(inst) {
            0b1100011 => {
                match i_funct3(inst) {
                    0b000 => {
                        // beq
                        dynasm!(self.a
                            ; .arch aarch64
                            ; cmp X(rs as u32), X(rt as u32)
                            ; b.eq =>label
                        );
                    }
                    0b001 => {
                        // bne
                        dynasm!(self.a
                            ; .arch aarch64
                            ; cmp X(rs as u32), X(rt as u32)
                            ; b.ne =>label
                        );
                    }
                    0b100 => {
                        // blt
                        dynasm!(self.a
                            ; .arch aarch64
                            ; cmp X(rs as u32), X(rt as u32)
                            ; b.lt =>label
                        );
                    }
                    0b101 => {
                        // bge
                        dynasm!(self.a
                            ; .arch aarch64
                            ; cmp X(rs as u32), X(rt as u32)
                            ; b.ge =>label
                        );
                    }
                    0b110 => {
                        // bltu
                        dynasm!(self.a
                            ; .arch aarch64
                            ; cmp X(rs as u32), X(rt as u32)
                            ; b.lo =>label
                        );
                    }
                    0b111 => {
                        // bgeu
                        dynasm!(self.a
                            ; .arch aarch64
                            ; cmp X(rs as u32), X(rt as u32)
                            ; b.hs =>label
                        );
                    }
                    _ => unreachable!()
                }

            }
            _ => unreachable!()
        }
    }

    fn emit_itype(&mut self, vpc: u64, inst: u32, rd: usize, rs: usize, imm: u32) {
        match i_op(inst) {
            0b0010011 => {
                // arith-i 64-bit
                match i_funct3(inst) {
                    0b000 => {
                        // addi
                        ld_simm16(&mut self.a, 30, imm);
                        dynasm!(self.a
                            ; .arch aarch64
                            ; add X(rd as u32), X(rs as u32), x30
                        );
                    }
                    0b010 => {
                        // slti
                        ld_simm16(&mut self.a, 30, imm);
                        dynasm!(self.a
                            ; .arch aarch64
                            ; cmp X(rs as u32), x30
                            ; cset X(rd as u32), lt
                        );
                    }
                    0b011 => {
                        // sltu
                        ld_simm16(&mut self.a, 30, imm);
                        dynasm!(self.a
                            ; .arch aarch64
                            ; cmp X(rs as u32), x30
                            ; cset X(rd as u32), lo
                        );
                    }
                    0b100 => {
                        // xori
                        ld_simm16(&mut self.a, 30, imm);
                        dynasm!(self.a
                            ; .arch aarch64
                            ; eor X(rd as u32), X(rs as u32), x30
                        );
                    }
                    0b110 => {
                        // ori
                        ld_simm16(&mut self.a, 30, imm);
                        dynasm!(self.a
                            ; .arch aarch64
                            ; orr X(rd as u32), X(rs as u32), x30
                        );
                    }
                    0b111 => {
                        // andi
                        ld_simm16(&mut self.a, 30, imm);
                        dynasm!(self.a
                            ; .arch aarch64
                            ; and X(rd as u32), X(rs as u32), x30
                        );
                    }
                    0b001 => {
                        // slli
                        dynasm!(self.a
                            ; .arch aarch64
                            ; lsl X(rd as u32), X(rs as u32), (imm & 0b111111)
                        );
                    }
                    0b101 => {
                        // srli/srai
                        if imm & 0b0100000_00000 == 0 {
                            // srli
                            dynasm!(self.a
                                ; .arch aarch64
                                ; lsr X(rd as u32), X(rs as u32), (imm & 0b111111)
                            );
                        } else {
                            // srai
                            dynasm!(self.a
                                ; .arch aarch64
                                ; asr X(rd as u32), X(rs as u32), (imm & 0b111111)
                            );
                        }
                    }
                    _ => unreachable!()
                }
            }
            0b0011011 => {
                // arith-i 32-bit
                match i_funct3(inst) {
                    0b000 => {
                        // addiw
                        ld_simm16(&mut self.a, 30, imm);
                        dynasm!(self.a
                            ; .arch aarch64
                            ; adds W(rd as u32), W(rs as u32), w30
                        );
                    }
                    0b001 => {
                        // slli
                        ld_simm16(&mut self.a, 30, imm);
                        dynasm!(self.a
                            ; .arch aarch64
                            ; lsl W(rd as u32), W(rs as u32), (imm & 0b11111)
                            ; sxtw X(rd as u32), W(rd as u32)
                        );
                    }
                    0b101 => {
                        // srli/srai
                        if imm & 0b0100000_00000 == 0 {
                            // srli
                            dynasm!(self.a
                                ; .arch aarch64
                                ; lsr W(rd as u32), W(rs as u32), (imm & 0b11111)
                                ; sxtw X(rd as u32), W(rd as u32)
                            );
                        } else {
                            // srai
                            dynasm!(self.a
                                ; .arch aarch64
                                ; asr W(rd as u32), W(rs as u32), (imm & 0b11111)
                                ; sxtw X(rd as u32), W(rd as u32)
                            );
                        }
                    }
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }

    fn emit_exception(&mut self, vpc: u64, reason: u16) {
        println!("static exception @ 0x{:016x}: reason {}", vpc, reason);

        ld_simm16(&mut self.a, 30, reason as u32);

        dynasm!(self.a
            ; .arch aarch64
            ; str x30, [X(runtime_reg() as u32), Runtime::offset_error_reason() as u32]

            ; ldr x30, [X(runtime_reg() as u32), Runtime::offset_exception_entry() as u32]
            ; blr x30
        );
        let translation_offset = self.a.offset().0 as u32;
        self.exception_translation_offset_to_v_offset.insert(translation_offset, (vpc - self.base_vpc) as u32);
    }

    fn emit_ud(&mut self, vpc: u64, _inst: u32) {
        self.emit_exception(vpc, error::ERROR_REASON_UNDEFINED_INSTRUCTION);
    }
}

struct SpillMachine {
    spilled_regs: Cell<u32>,
}

impl SpillMachine {
    fn new() -> SpillMachine {
        let mut spilled_regs: u32 = 0;

        // No spill
        spilled_regs.set_bit(0, true);
        spilled_regs.set_bit(2, true);
        spilled_regs.set_bit(30, true);

        SpillMachine {
            spilled_regs: Cell::new(spilled_regs),
        }
    }

    fn prepare_dont_touch(dont_touch: &[usize]) -> Vec<usize> {
        dont_touch.iter().filter_map(|x| match register_map_policy(*x) {
            VirtualReg::Native(i) => Some(i),
            VirtualReg::Memory(_) => None,
        }).collect()
    }

    fn map_register_r(&self, a: &mut Assembler, reg: usize, dont_touch: &[usize]) -> (usize, Option<SpillHandle>) {
        let dont_touch = Self::prepare_dont_touch(dont_touch);
        match register_map_policy(reg) {
            VirtualReg::Native(i) => (i, None),
            VirtualReg::Memory(memory_index) => {
                for i in 0..31 {
                    if self.spilled_regs.get().get_bit(i) {
                        continue;
                    }
                    if dont_touch.contains(&i) {
                        continue;
                    }
        
                    self.spilled_regs.update(|mut x| *x.set_bit(i, true));

                    let offset_spill_out = Runtime::offset_spill() + i * 8;
                    let offset_read_in = Runtime::offset_memory_regs() + memory_index * 8;

                    dynasm!(a
                        ; .arch aarch64
                        ; str X(i as u32), [X(runtime_reg() as u32), offset_spill_out as u32]
                        ; ldr X(i as u32), [X(runtime_reg() as u32), offset_read_in as u32]
                    );

                    return (i, Some(SpillHandle {
                        spill_reg_index: i,
                        spill_rtmem_index: memory_index,
                    }));
                }
                panic!("map_register_r: no available registers");
            }
        }
    }

    fn map_register_w(&self, a: &mut Assembler, reg: usize, dont_touch: &[usize]) -> (usize, Option<SpillHandle>) {
        let dont_touch = Self::prepare_dont_touch(dont_touch);
        match register_map_policy(reg) {
            VirtualReg::Native(i) => (i, None),
            VirtualReg::Memory(memory_index) => {
                for i in 0..31 {
                    if self.spilled_regs.get().get_bit(i) {
                        continue;
                    }
                    if dont_touch.contains(&i) {
                        continue;
                    }
        
                    self.spilled_regs.update(|mut x| *x.set_bit(i, true));

                    let offset_spill_out = Runtime::offset_spill() + i * 8;

                    dynasm!(a
                        ; .arch aarch64
                        ; str X(i as u32), [X(runtime_reg() as u32), offset_spill_out as u32]
                    );

                    return (i, Some(SpillHandle {
                        spill_reg_index: i,
                        spill_rtmem_index: memory_index,
                    }));
                }
                panic!("map_register_w: no available registers");
            }
        }
    }

    fn map_register_triple(&self, a: &mut Assembler, rd: usize, rs: usize, rt: usize) -> (usize, usize, usize, Option<SpillHandle>, Option<SpillHandle>, Option<SpillHandle>) {
        let (rs_1, mut rs_h) = self.map_register_r(a, rs, &[rd, rt]);

        let (rt_1, mut rt_h) = if rs == rt {
            (rs_1, None)
        } else {
            self.map_register_r(a, rt, &[rd, rs])
        };

        // Write release requires stronger writeback
        let (rd_1, rd_h) = if rd == rs {
            let rs_h = std::mem::replace(&mut rs_h, None);
            (rs_1, rs_h)
        } else if rd == rt {
            let rt_h = std::mem::replace(&mut rt_h, None);
            (rt_1, rt_h)
        } else {
            self.map_register_w(a, rd, &[rs, rt])
        };

        (rd_1, rs_1, rt_1, rd_h, rs_h, rt_h)
    }

    fn map_register_tuple_w_r(&self, a: &mut Assembler, rd: usize, rs: usize) -> (usize, usize, Option<SpillHandle>, Option<SpillHandle>) {
        let (rs_1, mut rs_h) = self.map_register_r(a, rs, &[rd]);

        // Write release requires stronger writeback
        let (rd_1, rd_h) = if rd == rs {
            let rs_h = std::mem::replace(&mut rs_h, None);
            (rs_1, rs_h)
        } else {
            self.map_register_w(a, rd, &[rs])
        };

        (rd_1, rs_1, rd_h, rs_h)
    }

    fn map_register_tuple_r_r(&self, a: &mut Assembler, rs: usize, rt: usize) -> (usize, usize, Option<SpillHandle>, Option<SpillHandle>) {
        let (rs_1, rs_h) = self.map_register_r(a, rs, &[rt]);

        let (rt_1, rt_h) = if rs == rt {
            (rs_1, None)
        } else {
            self.map_register_r(a, rt, &[rs])
        };

        (rs_1, rt_1, rs_h, rt_h)
    }

    fn release_register_r(&self, a: &mut Assembler, handle: Option<SpillHandle>) {
        if let Some(handle) = handle {
            assert!(self.spilled_regs.get().get_bit(handle.spill_reg_index));
            self.spilled_regs.update(|mut x| *x.set_bit(handle.spill_reg_index, false));
    
            let offset_spill_out = Runtime::offset_spill() + handle.spill_reg_index * 8;
    
            dynasm!(a
                ; .arch aarch64
                ; ldr X(handle.spill_reg_index as u32), [X(runtime_reg() as u32), offset_spill_out as u32]
            );
            std::mem::forget(handle);
        }
    }

    fn release_register_w(&self, a: &mut Assembler, handle: Option<SpillHandle>) {
        if let Some(handle) = handle {
            assert!(self.spilled_regs.get().get_bit(handle.spill_reg_index));
            self.spilled_regs.update(|mut x| *x.set_bit(handle.spill_reg_index, false));

            let offset_spill_out = Runtime::offset_spill() + handle.spill_reg_index * 8;
            let offset_read_in = Runtime::offset_memory_regs() + handle.spill_rtmem_index * 8;

            dynasm!(a
                ; .arch aarch64
                ; str X(handle.spill_reg_index as u32), [X(runtime_reg() as u32), offset_read_in as u32]
                ; ldr X(handle.spill_reg_index as u32), [X(runtime_reg() as u32), offset_spill_out as u32]
            );
            std::mem::forget(handle);
        }
    }
}

fn ld_simm16(a: &mut Assembler, rd: usize, imm: u32) {
    let imm = imm as i32;
    if imm < 0 {
        let imm = !(imm as u32);
        dynasm!(a
            ; .arch aarch64
            ; movn X(rd as u32), imm
        );
    } else {
        let imm = imm as u32;
        dynasm!(a
            ; .arch aarch64
            ; movz X(rd as u32), imm
        );
    }
}

fn runtime_reg() -> usize {
    2
}

struct SpillHandle {
    spill_reg_index: usize,
    spill_rtmem_index: usize,
}

impl Drop for SpillHandle {
    fn drop(&mut self) {
        panic!("SpillHandle dropped without proper release");
    }
}

fn i_op(i: u32) -> u32 {
    i & 0b1111111
}

fn i_funct3(i: u32) -> u32 {
    (i >> 12) & 0b111
}

fn i_rs(i: u32) -> u32 {
    (i >> 15) & 0b11111
}

fn i_rt(i: u32) -> u32 {
    (i >> 20) & 0b11111
}

fn i_rd(i: u32) -> u32 {
    (i >> 7) & 0b11111
}

fn i_itype_imm(i: u32) -> u32 {
    ((i as i32) >> 20) as u32
}

fn i_utype_imm(i: u32) -> u32 {
    i >> 12
}

fn i_btype_imm(inst: u32) -> u32 {
    sext13b((((inst >> 8) & 0b1111) << 1)
        | (((inst >> 7) & 0b1) << 11)
        | (((inst >> 25) & 0b111111) << 5)
        | (((inst >> 31) & 0b1) << 12)
    )
}

fn sext12b(src: u32) -> u32 {
    if src & (1 << 11) != 0 {
        src | !0b1111_1111_1111u32
    } else {
        src
    }
}

fn sext13b(src: u32) -> u32 {
    if src & (1 << 12) != 0 {
        src | !0b1_1111_1111_1111u32
    } else {
        src
    }
}