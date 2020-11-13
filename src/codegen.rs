use dynasmrt::{aarch64::Assembler, DynasmApi, dynasm};
use bit_field::BitField;
use std::cell::Cell;
use std::rc::Rc;
use crate::runtime::Runtime;
use crate::error;
use byteorder::{LittleEndian, ReadBytesExt};
use crate::translation::Translation;

pub struct Codegen<'a> {
    a: Assembler,
    base_vpc: u64,
    spill: SpillMachine,
    raw: &'a [u8],
    v_offset_to_translation_offset: Box<[u32]>,
}

impl<'a> Codegen<'a> {
    pub fn new(base_vpc: u64, raw: &'a [u8]) -> Codegen<'a> {
        let a = Assembler::new().unwrap();
        let offset_mapping_size = raw.len() / 2;
        let v_offset_to_translation_offset = vec![std::u32::MAX; offset_mapping_size].into_boxed_slice();

        Codegen {
            a,
            base_vpc,
            spill: SpillMachine::new(),
            raw,
            v_offset_to_translation_offset,
        }
    }

    pub fn generate(&mut self) {
        let mut cursor = self.raw;
        let mut vpc = self.base_vpc;
        while let Ok(x) = cursor.read_u32::<LittleEndian>() {
            self.v_offset_to_translation_offset[((vpc - self.base_vpc) / 2) as usize] = self.a.offset().0 as u32;
            self.emit_once(vpc, x);
            vpc += 4;
        }
    }

    pub fn refine(self) -> Translation {
        Translation {
            backing: self.a,
            v_offset_to_translation_offset: self.v_offset_to_translation_offset,
        }
    }

    fn emit_once(&mut self, vpc: u64, inst: u32) {
        match i_op(inst) {
            0b0010011 => {
                // arith-i 32-bit
                match i_funct3(inst) {
                    0b000 => {
                        // addi
                        let (rd, rs, rd_h, rs_h) = self.spill.map_register_tuple(&mut self.a, i_rd(inst) as _, i_rs(inst) as _);
                        let imm = i_itype_imm(inst);
                        dynasm!(self.a
                            ; .arch aarch64
                            ; adds W(rd as u32), W(rs as u32), (imm as u32)
                        );
                        self.spill.release_register_r(&mut self.a, rs_h);
                        self.spill.release_register_w(&mut self.a, rd_h);
                    }
                    _ => self.emit_ud(vpc, inst)
                }
            }
            _ => self.emit_ud(vpc, inst)
        }
    }

    fn emit_ud(&mut self, vpc: u64, _inst: u32) {
        dynasm!(self.a
            ; .arch aarch64

            ; mov x30, ((vpc & 0xffff) as u64)
            ; movk x30, (((vpc >> 16) & 0xffff) as u32), lsl 16
            ; movk x30, (((vpc >> 32) & 0xffff) as u32), lsl 32
            ; movk x30, (((vpc >> 48) & 0xffff) as u32), lsl 48
            ; str x30, [X(runtime_reg() as u32), Runtime::offset_error_vpc() as u32]

            ; mov x30, (error::ERROR_REASON_UNDEFINED_INSTRUCTION as u64)
            ; str x30, [X(runtime_reg() as u32), Runtime::offset_error_reason() as u32]

            ; ldr x30, [X(runtime_reg() as u32), Runtime::offset_exception_entry() as u32]
            ; blr x30
        );
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

    fn map_register_r(&self, a: &mut Assembler, reg: usize, dont_touch: &[usize]) -> (usize, Option<SpillHandle>) {
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

    fn map_register_tuple(&self, a: &mut Assembler, rd: usize, rs: usize) -> (usize, usize, Option<SpillHandle>, Option<SpillHandle>) {
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

#[derive(Copy, Clone, Debug)]
enum VirtualReg {
    Native(usize),
    Memory(usize),
}

fn register_map_policy(x: usize) -> VirtualReg {
    use VirtualReg::*;

    // Reserve x2 and x30
    match x {
        0 => Native(31), // zero
        1 => Native(0),
        2 => Native(1),
        3 => Memory(0),
        4 => Memory(1),
        x if x >= 5 && x <= 31 => Native(x - 2),
        _ => unreachable!(),
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
    i >> 20
}
