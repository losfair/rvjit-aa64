use dynasmrt::{aarch64::Assembler, DynasmApi, dynasm, DynamicLabel, DynasmLabelApi};
use bit_field::BitField;
use std::cell::Cell;
use std::rc::Rc;
use crate::runtime::Runtime;
use crate::error;
use byteorder::{LittleEndian, ReadBytesExt};
use crate::translation::{
    runtime_reg, rtstore_reg, trash_reg_w, zero_reg_r, temp1_reg,
    ExceptionPoint, VirtualReg, register_map_policy, Translation, JalrPatchPoint, LoadStorePatchPoint,
    RtSlot,
};
use std::collections::BTreeMap;
use log::debug;

pub struct Codegen<'a> {
    a: Assembler,
    spill: SpillMachine,
    raw: &'a [u8],
    v_offset_to_translation_offset: Box<[u32]>,
    exception_points: BTreeMap<u32, ExceptionPoint>,
    jalr_patch_points: BTreeMap<u32, JalrPatchPoint>,
    load_store_patch_points: BTreeMap<u32, LoadStorePatchPoint>,
    relative_br_labels: Box<[DynamicLabel]>,
    rtstore_template: Vec<u64>,
}

impl<'a> Codegen<'a> {
    pub fn new(raw: &'a [u8]) -> Codegen<'a> {
        let mut a = Assembler::new().unwrap();
        let max_num_instructions = raw.len() / 2;
        let v_offset_to_translation_offset = vec![std::u32::MAX; max_num_instructions].into_boxed_slice();
        let relative_br_labels = (0..max_num_instructions).map(|_| a.new_dynamic_label()).collect::<Vec<_>>().into_boxed_slice();

        Codegen {
            a,
            spill: SpillMachine::new(),
            raw,
            v_offset_to_translation_offset,
            exception_points: BTreeMap::new(),
            jalr_patch_points: BTreeMap::new(),
            load_store_patch_points: BTreeMap::new(),
            relative_br_labels,
            rtstore_template: Vec::new(),
        }
    }

    pub fn generate(&mut self) {
        let mut cursor = self.raw;
        let mut voff: u32 = 0;

        while let Ok(x) = cursor.read_u16::<LittleEndian>() {
            let inst_offset = (voff / 2) as usize;
            let label = self.relative_br_labels[inst_offset];

            self.v_offset_to_translation_offset[inst_offset] = self.a.offset().0 as u32;

            if x & 0b11 == 0b11 {
                // 4-byte instructions
                let next_label = self.relative_br_labels[inst_offset + 1];
                dynasm!(self.a
                    ; .arch aarch64
                    ; =>label
                    ; =>next_label
                );

                if let Ok(y) = cursor.read_u16::<LittleEndian>() {
                    let inst = ((y as u32) << 16) | (x as u32);
                    self.emit_once(voff, inst);
                } else {
                    self.emit_ud(voff);
                }

                voff += 4;
            } else {
                // 2-byte instructions
                dynasm!(self.a
                    ; .arch aarch64
                    ; =>label
                );

                match crate::rvc::decompress(x) {
                    Some(inst) => {
                        self.emit_once(voff, inst);
                    }
                    None => {
                        self.emit_ud(voff);
                    }
                }

                voff += 2;
            }
        }
    }

    pub fn refine(self) -> Translation {
        debug!("refine: code size = {} bytes. #(offsets) = {}", self.a.offset().0, self.v_offset_to_translation_offset.len());
        Translation {
            backing: self.a.finalize().unwrap(),
            v_offset_to_translation_offset: self.v_offset_to_translation_offset,
            exception_points: self.exception_points,
            jalr_patch_points: self.jalr_patch_points,
            load_store_patch_points: self.load_store_patch_points,
            rtstore_template: self.rtstore_template.into_boxed_slice(),
        }
    }

    fn prepare_rel_br(&mut self, voff: u32, offset: i32) -> Option<DynamicLabel> {
        let dst_voff = voff as u64 as i64 + offset as i64;
        if dst_voff < 0 {
            self.emit_exception(voff, error::ERROR_REASON_BRANCH_OOB);
            return None;
        }

        let dst_offset = (dst_voff as u64) / 2;
        if dst_offset >= self.relative_br_labels.len() as u64 {
            self.emit_exception(voff, error::ERROR_REASON_BRANCH_OOB);
            return None;
        }

        if offset < 0 {
            // Explicitly checked signals.
            dynasm!(self.a
                ; .arch aarch64
                ; ldr w30, [X(runtime_reg() as u32), Runtime::offset_signal() as u32]
                ; cbz w30, >ok
            );

            self.emit_exception(voff, error::ERROR_REASON_SIGNAL);

            dynasm!(self.a
                ; .arch aarch64
                ; ok:
            );
        }

        let label = self.relative_br_labels[dst_offset as usize].clone();
        Some(label)
    }

    fn emit_once(&mut self, voff: u32, inst: u32) {
        match i_op(inst) {
            0b0010011 => {
                // arith-i 64-bit
                let (rd, rs, rd_h, rs_h) = self.spill.map_register_tuple_w_r(&mut self.a, i_rd(inst) as _, i_rs(inst) as _);
                let imm = i_itype_imm(inst);

                self.emit_itype(voff, inst, rd, rs, imm);

                self.spill.release_register_r(&mut self.a, rs_h);
                self.spill.release_register_w(&mut self.a, rd_h);
            }
            0b0110011 => {
                // arith-r 64-bit
                let (rd, rs, rt, rd_h, rs_h, rt_h) = self.spill.map_register_triple(&mut self.a, i_rd(inst) as _, i_rs(inst) as _, i_rt(inst) as _);

                self.emit_rtype(voff, inst, rd, rs, rt);

                self.spill.release_register_r(&mut self.a, rs_h);
                self.spill.release_register_r(&mut self.a, rt_h);
                self.spill.release_register_w(&mut self.a, rd_h);
            }
            0b0011011 => {
                // arith-i 32-bit
                let funct3 = i_funct3(inst);
                if funct3 != 0b000 && funct3 != 0b001 && funct3 != 0b101 {
                    self.emit_ud(voff);
                    return;
                }

                let (rd, rs, rd_h, rs_h) = self.spill.map_register_tuple_w_r(&mut self.a, i_rd(inst) as _, i_rs(inst) as _);
                let imm = i_itype_imm(inst);

                self.emit_itype(voff, inst, rd, rs, imm);

                self.spill.release_register_r(&mut self.a, rs_h);
                self.spill.release_register_w(&mut self.a, rd_h);
            }
            0b0111011 => {
                // arith-r 32-bit
                let funct3 = i_funct3(inst);
                if funct3 == 0b010 || funct3 == 0b011 {
                    self.emit_ud(voff);
                    return;
                }

                let (rd, rs, rt, rd_h, rs_h, rt_h) = self.spill.map_register_triple(&mut self.a, i_rd(inst) as _, i_rs(inst) as _, i_rt(inst) as _);

                self.emit_rtype(voff, inst, rd, rs, rt);

                self.spill.release_register_r(&mut self.a, rs_h);
                self.spill.release_register_r(&mut self.a, rt_h);
                self.spill.release_register_w(&mut self.a, rd_h);
            }
            0b1100011 => {
                // conditional branch
                let funct3 = i_funct3(inst);
                let imm = i_btype_imm(inst);

                if funct3 == 0b010 || funct3 == 0b011 {
                    self.emit_ud(voff);
                } else {
                    let label = match self.prepare_rel_br(voff, imm as i32) {
                        Some(x) => x,
                        None => return
                    };

                    let (rs, rt, rs_h, rt_h) = self.spill.map_register_tuple_r_r(&mut self.a, i_rs(inst) as _, i_rt(inst) as _);

                    dynasm!(self.a
                        ; .arch aarch64
                        ; cmp X(rs as u32), X(rt as u32)
                    );

                    // We are leaving. Release handles now.
                    self.spill.release_register_r(&mut self.a, rs_h);
                    self.spill.release_register_r(&mut self.a, rt_h);

                    self.emit_btype_late(voff, inst, label);
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
            0b0010111 => {
                // auipc
                let imm = i_utype_imm(inst);
                let (rd, rd_h) = self.spill.map_register_w(&mut self.a, i_rd(inst) as _, &[]);

                let value = voff + (imm << 12);
                self.load_vpc(value, rd as _, 30);

                self.spill.release_register_w(&mut self.a, rd_h);
            }
            0b1101111 => {
                // jal
                let imm = i_jtype_imm(inst);
                let label = match self.prepare_rel_br(voff, imm as i32) {
                    Some(x) => x,
                    None => return
                };
                
                let (rd, rd_h) = self.spill.map_register_w(&mut self.a, i_rd(inst) as _, &[]);
                self.load_vpc(voff + 4, rd as _, 30);
                self.spill.release_register_w(&mut self.a, rd_h);

                dynasm!(self.a
                    ; .arch aarch64
                    ; b =>label
                );
            }
            0b1100111 => {
                // jalr
                let imm = i_itype_imm(inst);
                self.emit_jalr(voff, i_rd(inst) as _, i_rs(inst) as _, imm)
            }
            0b0000011 => {
                // load
                let raw_rd = i_rd(inst) as _;
                let raw_rs = i_rs(inst) as _;
                let imm = i_itype_imm(inst);

                match i_funct3(inst) {
                    0b000 => {
                        // lb
                        self.emit_load(voff, raw_rd, raw_rs, imm, 1, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrsb X(rd), [X(rs)]
                            );
                        });
                    }
                    0b001 => {
                        // lh
                        self.emit_load(voff, raw_rd, raw_rs, imm, 2, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrsh X(rd), [X(rs)]
                            );
                        });
                    }
                    0b010 => {
                        // lw
                        self.emit_load(voff, raw_rd, raw_rs, imm, 4, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrsw X(rd), [X(rs)]
                            );
                        });
                    }
                    0b011 => {
                        // ld
                        self.emit_load(voff, raw_rd, raw_rs, imm, 8, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldr X(rd), [X(rs)]
                            );
                        });
                    }
                    0b100 => {
                        // lbu
                        self.emit_load(voff, raw_rd, raw_rs, imm, 1, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrb W(rd), [X(rs)]
                            );
                        });
                    }
                    0b101 => {
                        // lhu
                        self.emit_load(voff, raw_rd, raw_rs, imm, 2, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrh W(rd), [X(rs)]
                            );
                        });
                    }
                    0b110 => {
                        // lwu
                        self.emit_load(voff, raw_rd, raw_rs, imm, 4, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldr W(rd), [X(rs)]
                            );
                        });
                    }
                    _ => self.emit_ud(voff)
                }
            }
            0b0100011 => {
                // store

                let raw_rs = i_rs(inst) as _;
                let raw_rt = i_rt(inst) as _;
                let imm = i_stype_imm(inst);

                match i_funct3(inst) {
                    0b000 => {
                        // sb
                        self.emit_store(voff, raw_rs, raw_rt, imm, 1, |this, rt, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; strb W(rt), [X(rs)]
                            );
                        });
                    }
                    0b001 => {
                        // sh
                        self.emit_store(voff, raw_rs, raw_rt, imm, 2, |this, rt, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; strh W(rt), [X(rs)]
                            );
                        });
                    }
                    0b010 => {
                        // sw
                        self.emit_store(voff, raw_rs, raw_rt, imm, 4, |this, rt, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; str W(rt), [X(rs)]
                            );
                        });
                    }
                    0b011 => {
                        // sd
                        self.emit_store(voff, raw_rs, raw_rt, imm, 8, |this, rt, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; str X(rt), [X(rs)]
                            );
                        });
                    }
                    _ => self.emit_ud(voff)
                }
            }
            0b1110011 => {
                // ecall/ebreak
                let imm = i_itype_imm(inst);
                if imm & 0b1 == 0 {
                    // ecall
                    self.emit_exception(voff, error::ERROR_REASON_ECALL);
                } else {
                    // ebreak
                    self.emit_exception(voff, error::ERROR_REASON_EBREAK);
                }
            }
            _ => self.emit_ud(voff)
        }
    }

    fn alloc_rtslot(&mut self, default_value: u64) -> RtSlot {
        let i = self.rtstore_template.len();
        self.rtstore_template.push(default_value);
        RtSlot(i as u32)
    }

    fn load_rtslot(&mut self, slot: RtSlot, dst_reg: u32) {
        ld_imm64_opt(&mut self.a, dst_reg as _, slot.0 as u64);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(dst_reg), X(rtstore_reg()), X(dst_reg), lsl 3 // 8-byte slots
            ; ldr X(dst_reg), [X(dst_reg)]
        );
    }

    fn load_rtslot_pair(&mut self, slot: RtSlot, dst_reg1: u32, dst_reg2: u32) {
        ld_imm64_opt(&mut self.a, dst_reg1 as _, slot.0 as u64);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(dst_reg1), X(rtstore_reg()), X(dst_reg1), lsl 3 // 8-byte slots
            ; ldp X(dst_reg1), X(dst_reg2), [X(dst_reg1)]
        );
    }

    fn load_vpc(&mut self, voff: u32, dst_reg: u32, temp_reg: u32) {
        ld_imm64_opt(&mut self.a, temp_reg as _, voff as u64);

        dynasm!(self.a
            ; .arch aarch64
            ; ldr X(dst_reg), [X(runtime_reg()), Runtime::offset_current_vbase() as u32]
            ; add X(dst_reg), X(dst_reg), X(temp_reg)
        );
    }

    fn emit_load<F: FnOnce(&mut Self, u32, u32)>(&mut self, voff: u32, raw_rd: u32, raw_rs: u32, imm: u32, access_size: u32, emit_inner: F) {
        let (rd, rs, rd_h, rs_h) = self.spill.map_register_tuple_w_r(&mut self.a, raw_rd as _, raw_rs as _);

        // Compute effective address
        ld_simm16(&mut self.a, trash_reg_w() as _, imm);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(trash_reg_w() as u32), X(trash_reg_w() as u32), X(rs as u32)
        );

        let lower_bound_slot = self.alloc_rtslot(std::u64::MAX);
        let upper_bound_slot = self.alloc_rtslot(0);
        let reloff_slot = self.alloc_rtslot(std::u64::MAX);

        self.load_rtslot_pair(lower_bound_slot, 30, temp1_reg());

        // Check bounds
        dynasm!(self.a
            ; .arch aarch64

            // Lower bound
            ; cmp X(trash_reg_w() as u32), x30
            ; b.lo >fallback

            // Upper bound
            ; cmp X(trash_reg_w() as u32), X(temp1_reg())
            ; b.hs >fallback
        );

        // v_addr + this_offset = real_addr
        self.load_rtslot(reloff_slot, 30);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(trash_reg_w() as u32), X(trash_reg_w() as u32), x30
        );

        // XXX: rd == trash_reg_w() is possible if rd is RISC-V zero register
        emit_inner(self, rd as _, trash_reg_w() as _);

        dynasm!(self.a
            ; .arch aarch64
            ; b >ok
            ; fallback:
        );

        self.emit_exception(voff, error::ERROR_REASON_LOAD_STORE_MISS);
        let pp = LoadStorePatchPoint {
            lower_bound_slot,
            upper_bound_slot,
            reloff_slot,
            rs: raw_rs as u32,
            rs_offset: imm as i32,
            access_size: access_size,
            is_store: false,
        };
        let asm_offset = self.a.offset().0;
        self.load_store_patch_points.insert(asm_offset as u32, pp);

        dynasm!(self.a
            ; .arch aarch64
            ; ok:
        );

        self.spill.release_register_r(&mut self.a, rs_h);
        self.spill.release_register_w(&mut self.a, rd_h);
    }

    fn emit_store<F: FnOnce(&mut Self, u32, u32)>(&mut self, voff: u32, raw_rs: u32, raw_rt: u32, imm: u32, access_size: u32, emit_inner: F) {
        let (rs, rt, rs_h, rt_h) = self.spill.map_register_tuple_r_r(&mut self.a, raw_rs as _, raw_rt as _);

        // Compute effective address
        ld_simm16(&mut self.a, trash_reg_w() as _, imm);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(trash_reg_w() as u32), X(trash_reg_w() as u32), X(rs as u32)
        );

        let lower_bound_slot = self.alloc_rtslot(std::u64::MAX);
        let upper_bound_slot = self.alloc_rtslot(0);
        let reloff_slot = self.alloc_rtslot(std::u64::MAX);

        self.load_rtslot_pair(lower_bound_slot, 30, temp1_reg());

        // Check bounds
        dynasm!(self.a
            ; .arch aarch64

            // Lower bound
            ; cmp X(trash_reg_w() as u32), x30
            ; b.lo >fallback

            // Upper bound
            ; cmp X(trash_reg_w() as u32), X(temp1_reg())
            ; b.hs >fallback
        );

        // v_addr + this_offset = real_addr
        self.load_rtslot(reloff_slot, 30);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(trash_reg_w() as u32), X(trash_reg_w() as u32), x30
        );

        emit_inner(self, rt as _, trash_reg_w() as _);

        dynasm!(self.a
            ; .arch aarch64
            ; b >ok
            ; fallback:
        );

        self.emit_exception(voff, error::ERROR_REASON_LOAD_STORE_MISS);
        let pp = LoadStorePatchPoint {
            lower_bound_slot,
            upper_bound_slot,
            reloff_slot,
            rs: raw_rs as u32,
            rs_offset: imm as i32,
            access_size: access_size,
            is_store: true,
        };
        let asm_offset = self.a.offset().0;
        self.load_store_patch_points.insert(asm_offset as u32, pp);

        dynasm!(self.a
            ; .arch aarch64
            ; ok:
        );

        self.spill.release_register_r(&mut self.a, rs_h);
        self.spill.release_register_r(&mut self.a, rt_h);
    }

    fn emit_jalr(&mut self, voff: u32, raw_rd: u32, raw_rs: u32, imm: u32) {
        let (rd, rs, rd_h, rs_h) = self.spill.map_register_tuple_w_r(&mut self.a, raw_rd as _, raw_rs as _);

        ld_simm16(&mut self.a, 30, imm);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(trash_reg_w() as u32), x30, X(rs as u32)
        );

        let lower_bound_slot = self.alloc_rtslot(std::u64::MAX);
        let upper_bound_slot = self.alloc_rtslot(0);
        let v2real_table_slot = self.alloc_rtslot(0);
        let machine_base_slot = self.alloc_rtslot(0);

        self.load_rtslot_pair(lower_bound_slot, 30, temp1_reg());

        // Check bounds
        dynasm!(self.a
            ; .arch aarch64

            // Upper bound
            ; cmp X(trash_reg_w() as u32), X(temp1_reg())
            ; b.hs >fallback

            // Lower bound
            ; cmp X(trash_reg_w() as u32), x30
            ; b.lo >fallback
            ; sub X(trash_reg_w() as u32), X(trash_reg_w() as u32), x30
        );

        self.load_rtslot_pair(v2real_table_slot, 30, temp1_reg());
        dynasm!(self.a
            ; .arch aarch64

            // Load real offset from v2real table
            ; lsr X(trash_reg_w() as u32), X(trash_reg_w() as u32), 1
            ; add x30, x30, X(trash_reg_w() as u32), lsl 2
            ; ldr W(trash_reg_w() as u32), [x30]

            // Check empty entries
            ; cmn W(trash_reg_w() as u32), 1
            ; b.eq >fallback // branch if value == u32::MAX

            // Compute actual address by base + offset
            ; add x30, X(trash_reg_w() as u32), X(temp1_reg())
            ; b >ok

            ; fallback:
        );

        let pp = JalrPatchPoint {
            lower_bound_slot,
            upper_bound_slot,
            v2real_table_slot,
            machine_base_slot,
            rd: raw_rd as u32,
            rs: raw_rs as u32,
            rs_offset: imm as i32,
        };
        self.emit_exception(voff, error::ERROR_REASON_JALR_MISS);
        let asm_offset = self.a.offset().0;
        self.jalr_patch_points.insert(asm_offset as u32, pp);

        dynasm!(self.a
            ; .arch aarch64
            ; ok:
        );

        // XXX:
        //
        // Loading link pc earlier right after reading `rs` doesn't work because the Rust side depends on the
        // previous `rs` value to detemine the jump target, and when `rs == rd`, `rs` is overwritten before falling
        // back to Rust.
        //
        // I spent an afternoon finding this bug. Be careful with the Rust side too :|
        //
        // The current solution isn't optimal though, since there is one unconditional branch.
        //
        // Still need to find a fix.
        self.load_vpc(voff + 4, rd as _, temp1_reg());

        self.spill.release_register_r(&mut self.a, rs_h);
        self.spill.release_register_w(&mut self.a, rd_h);

        dynasm!(self.a
            ; .arch aarch64
            ; br x30
        );
    }

    fn emit_rtype(&mut self, voff: u32, inst: u32, rd: usize, rs: usize, rt: usize) {
        match i_op(inst) {
            0b0110011 => {
                match i_funct3(inst) {
                    0b000 => {
                        // add/sub/mul
                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0100000 => {
                                // sub
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; sub X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                            0b0000001 => {
                                // mul
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; mul X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                            _ => {
                                // add
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting add. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; add X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                        }
                    }
                    0b001 => {
                        // sll/mulh
                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0000001 => {
                                // mulh
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; smulh X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                            _ => {
                                // sll
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting sll. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; lsl X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                        }
                    }
                    0b010 => {
                        // slt/mulhsu
                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0000001 => {
                                // mulhsu
                                debug!("emit_rtype: mulhsu not yet implemented. Replacing with mulh. voff = 0x{:016x}", voff);
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; smulh X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                            _ => {
                                // slt
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting slt. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; cmp X(rs as u32), X(rt as u32)
                                    ; cset X(rd as u32), lt
                                );
                            }
                        }
                    }
                    0b011 => {
                        // sltu/mulhu

                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0000001 => {
                                // mulhu
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; umulh X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                            _ => {
                                // sltu
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting sltu. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; cmp X(rs as u32), X(rt as u32)
                                    ; cset X(rd as u32), lo
                                );
                            }
                        }
                    }
                    0b100 => {
                        // xor/div

                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0000001 => {
                                // div
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; sdiv X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                            _ => {
                                // xor
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting xor. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; eor X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                        }
                    }
                    0b101 => {
                        // srl/sra/divu
                        // TODO: Check behavior when shifting by more than 32 bits

                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0000001 => {
                                // divu
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; sdiv X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                            0b0100000 => {
                                // sra
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; asr X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                            _ => {
                                // srl
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting srl. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; lsr X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                        }
                    }
                    0b110 => {
                        // or/rem

                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0000001 => {
                                // rem
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; sdiv X(rd as u32), X(rs as u32), X(rt as u32)
                                    ; msub X(rd as u32), X(rd as u32), X(rt as u32), X(rs as u32)
                                );
                            }
                            _ => {
                                // or
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting or. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; orr X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                        }
                    }
                    0b111 => {
                        // and/remu

                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0000001 => {
                                // remu
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; udiv X(rd as u32), X(rs as u32), X(rt as u32)
                                    ; msub X(rd as u32), X(rd as u32), X(rt as u32), X(rs as u32)
                                );
                            }
                            _ => {
                                // and
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting and. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; and X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
            0b0111011 => {
                match i_funct3(inst) {
                    0b000 => {
                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0100000 => {
                                // subw
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; sub W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                            0b0000001 => {
                                // mulw
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; mul W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                            _ => {
                                // addw
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting addw. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; add W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                        }
                    }
                    0b001 => {
                        let funct7 = i_funct7(inst);
                        match funct7 {
                            _ => {
                                // sllw
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting sllw. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; lsl W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                        }
                    }
                    0b100 => {
                        let funct7 = i_funct7(inst);
                        match funct7 {
                            _ => {
                                // divw
                                if funct7 != 0b0000001 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting divw. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; sdiv W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                        }
                    }
                    0b101 => {
                        // TODO: Check behavior when shifting by more than 32 bits

                        let funct7 = i_funct7(inst);
                        match funct7 {
                            0b0100000 => {
                                // sraw
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; asr W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                            0b0000001 => {
                                // divuw
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; udiv W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                            _ => {
                                // srlw
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting srlw. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; lsr W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                        }
                    }
                    0b110 => {
                        let funct7 = i_funct7(inst);
                        match funct7 {
                            _ => {
                                // remw
                                if funct7 != 0b0000001 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting remw. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; sdiv W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; msub W(rd as u32), W(rd as u32), W(rt as u32), W(rs as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                        }
                    }
                    0b111 => {
                        let funct7 = i_funct7(inst);
                        match funct7 {
                            _ => {
                                // remuw
                                if funct7 != 0b0000001 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting remuw. voff = 0x{:016x}", voff);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; udiv W(rd as u32), W(rs as u32), W(rt as u32)
                                    ; msub W(rd as u32), W(rd as u32), W(rt as u32), W(rs as u32)
                                    ; sxtw X(rd as u32), W(rd as u32)
                                );
                            }
                        }
                    }
                    _ => unreachable!()
                }
            }
            _ => unreachable!()
        }
    }

    fn emit_btype_late(&mut self, voff: u32, inst: u32, label: DynamicLabel) {
        match i_op(inst) {
            0b1100011 => {
                match i_funct3(inst) {
                    0b000 => {
                        // beq
                        dynasm!(self.a
                            ; .arch aarch64
                            ; b.eq =>label
                        );
                    }
                    0b001 => {
                        // bne
                        dynasm!(self.a
                            ; .arch aarch64
                            ; b.ne =>label
                        );
                    }
                    0b100 => {
                        // blt
                        dynasm!(self.a
                            ; .arch aarch64
                            ; b.lt =>label
                        );
                    }
                    0b101 => {
                        // bge
                        dynasm!(self.a
                            ; .arch aarch64
                            ; b.ge =>label
                        );
                    }
                    0b110 => {
                        // bltu
                        dynasm!(self.a
                            ; .arch aarch64
                            ; b.lo =>label
                        );
                    }
                    0b111 => {
                        // bgeu
                        dynasm!(self.a
                            ; .arch aarch64
                            ; b.hs =>label
                        );
                    }
                    _ => unreachable!()
                }

            }
            _ => unreachable!()
        }
    }

    fn emit_itype(&mut self, voff: u32, inst: u32, rd: usize, rs: usize, imm: u32) {
        match i_op(inst) {
            0b0010011 => {
                // arith-i 64-bit
                match i_funct3(inst) {
                    0b000 => {
                        // addi
                        if imm as i32 >= 0 {
                            dynasm!(self.a
                                ; .arch aarch64
                                ; add X(rd as u32), X(rs as u32), imm
                            );
                        } else {
                            dynasm!(self.a
                                ; .arch aarch64
                                ; sub X(rd as u32), X(rs as u32), (-(imm as i32)) as u32
                            );
                        }
                    }
                    0b010 => {
                        // slti
                        if imm as i32 >= 0 {
                            dynasm!(self.a
                                ; .arch aarch64
                                ; cmp X(rs as u32), imm
                                ; cset X(rd as u32), lt
                            );
                        } else {
                            dynasm!(self.a
                                ; .arch aarch64
                                ; cmn X(rs as u32), (-(imm as i32)) as u32
                                ; cset X(rd as u32), lt
                            );
                        }
                    }
                    0b011 => {
                        // sltu
                        if imm as i32 >= 0 {
                            dynasm!(self.a
                                ; .arch aarch64
                                ; cmp X(rs as u32), imm
                                ; cset X(rd as u32), lo
                            );
                        } else {
                            dynasm!(self.a
                                ; .arch aarch64
                                ; cmn X(rs as u32), (-(imm as i32)) as u32
                                ; cset X(rd as u32), lo
                            );
                        }
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
                        if imm as i32 >= 0 {
                            dynasm!(self.a
                                ; .arch aarch64
                                ; add W(rd as u32), W(rs as u32), imm
                                ; sxtw X(rd as u32), W(rd as u32)
                            );
                        } else {
                            dynasm!(self.a
                                ; .arch aarch64
                                ; sub W(rd as u32), W(rs as u32), (-(imm as i32)) as u32
                                ; sxtw X(rd as u32), W(rd as u32)
                            );
                        }
                    }
                    0b001 => {
                        // slli
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

    fn emit_exception(&mut self, voff: u32, reason: u16) {
        //debug!("static exception @ 0x{:016x}: reason {}", voff, reason);

        dynasm!(self.a
            ; .arch aarch64

            ; ldr x30, [X(runtime_reg() as u32), Runtime::offset_exception_entry() as u32]
            ; blr x30
        );
        let translation_offset = self.a.offset().0 as u32;
        let spill_mask = self.spill.spilled_regs.get();
        self.exception_points.insert(translation_offset, ExceptionPoint {
            v_offset: voff,
            spill_mask,
            reason,
        });
    }

    fn emit_ud(&mut self, voff: u32) {
        self.emit_exception(voff, error::ERROR_REASON_UNDEFINED_INSTRUCTION);
    }
}

struct SpillMachine {
    spilled_regs: Cell<u32>,
}

impl SpillMachine {
    fn new() -> SpillMachine {
        let mut spilled_regs: u32 = 0;

        // No spill
        spilled_regs.set_bit(2, true);
        spilled_regs.set_bit(26, true);
        spilled_regs.set_bit(27, true);
        spilled_regs.set_bit(28, true);
        spilled_regs.set_bit(29, true);
        spilled_regs.set_bit(30, true);

        SpillMachine {
            spilled_regs: Cell::new(spilled_regs),
        }
    }

    fn prepare_dont_touch(dont_touch: &[usize]) -> Vec<usize> {
        dont_touch.iter().filter_map(|x| match register_map_policy(*x) {
            VirtualReg::Native(i) => Some(i),
            VirtualReg::Memory(_) => None,
            VirtualReg::Zero => None,
        }).collect()
    }

    fn mk_temp(&self, a: &mut Assembler, dont_touch: &[usize]) -> (usize, TempSpillHandle) {
        let dont_touch = Self::prepare_dont_touch(dont_touch);

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

            return (i, TempSpillHandle {
                spill_reg_index: i,
            });
        }

        panic!("SpillMachine::mk_temp: no register available");
    }

    fn release_temp(&self, a: &mut Assembler, handle: TempSpillHandle) {
        assert!(self.spilled_regs.get().get_bit(handle.spill_reg_index));
        self.spilled_regs.update(|mut x| *x.set_bit(handle.spill_reg_index, false));

        let offset_spill_out = Runtime::offset_spill() + handle.spill_reg_index * 8;

        dynasm!(a
            ; .arch aarch64
            ; ldr X(handle.spill_reg_index as u32), [X(runtime_reg() as u32), offset_spill_out as u32]
        );
        std::mem::forget(handle);
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
            VirtualReg::Zero => (zero_reg_r(), None),
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
            VirtualReg::Zero => (trash_reg_w(), None),
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
        let (rd_1, rd_h) = if rd == rs && rd != 0 {
            let rs_h = std::mem::replace(&mut rs_h, None);
            (rs_1, rs_h)
        } else if rd == rt && rd != 0 {
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
        let (rd_1, rd_h) = if rd == rs && rd != 0 {
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

pub(crate) fn ld_simm16<A: DynasmApi>(a: &mut A, rd: usize, imm: u32) {
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

pub(crate) fn ld_imm64_opt<A: DynasmApi>(a: &mut A, rd: usize, imm: u64) {
    dynasm!(
        a
        ; .arch aarch64
        ; movz X(rd as u32), (imm & 0xffff) as u32
    );

    if ((imm >> 16) & 0xffff) != 0 {
        dynasm!(
            a
            ; .arch aarch64
            ; movk X(rd as u32), ((imm >> 16) & 0xffff) as u32, lsl 16
        );
    }

    if ((imm >> 32) & 0xffff) != 0 {
        dynasm!(
            a
            ; .arch aarch64
            ; movk X(rd as u32), ((imm >> 32) & 0xffff) as u32, lsl 32
        );
    }

    if ((imm >> 48) & 0xffff) != 0 {
        dynasm!(
            a
            ; .arch aarch64
            ; movk X(rd as u32), ((imm >> 48) & 0xffff) as u32, lsl 48
        );
    }
}

pub(crate) fn ld_imm64<A: DynasmApi>(a: &mut A, rd: usize, imm: u64) {
    dynasm!(
        a
        ; .arch aarch64
        ; movz X(rd as u32), (imm & 0xffff) as u32
        ; movk X(rd as u32), ((imm >> 16) & 0xffff) as u32, lsl 16
        ; movk X(rd as u32), ((imm >> 32) & 0xffff) as u32, lsl 32
        ; movk X(rd as u32), ((imm >> 48) & 0xffff) as u32, lsl 48
    )
}

pub(crate) fn ld_simm32<A: DynasmApi>(a: &mut A, rd: usize, imm: u32) {
    if rd == 31 {
        // xzr
        return;
    }

    dynasm!(
        a
        ; .arch aarch64
        ; movz X(rd as u32), (imm & 0xffff) as u32
        ; movk X(rd as u32), ((imm >> 16) & 0xffff) as u32, lsl 16
        ; sxtw X(rd as u32), W(rd as u32)
    )
}

struct TempSpillHandle {
    spill_reg_index: usize,
}

impl Drop for TempSpillHandle {
    fn drop(&mut self) {
        panic!("TempSpillHandle dropped without proper release");
    }
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

fn i_jtype_imm(inst: u32) -> u32 {
    sext21b((((inst >> 12) & 0b11111111) << 12)
        | (((inst >> 20) & 0b1) << 11)
        | (((inst >> 21) & 0b1111111111) << 1)
        | (((inst >> 31) & 0b1) << 20)
    )
}

fn i_btype_imm(inst: u32) -> u32 {
    sext13b((((inst >> 8) & 0b1111) << 1)
        | (((inst >> 7) & 0b1) << 11)
        | (((inst >> 25) & 0b111111) << 5)
        | (((inst >> 31) & 0b1) << 12)
    )
}

fn i_stype_imm(inst: u32) -> u32 {
    sext12b(((inst >> 7) & 0b11111) | (((inst >> 25) & 0b1111111) << 5))
}

fn i_funct7(inst: u32) -> u32 {
    inst >> 25
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

fn sext21b(src: u32) -> u32 {
    if src & (1 << 20) != 0 {
        src | !0b111_111_111_111_111_111_111u32
    } else {
        src
    }
}