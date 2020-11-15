use dynasmrt::{aarch64::Assembler, DynasmApi, dynasm, DynamicLabel, DynasmLabelApi};
use bit_field::BitField;
use std::cell::Cell;
use std::rc::Rc;
use crate::runtime::Runtime;
use crate::error;
use byteorder::{LittleEndian, ReadBytesExt};
use crate::translation::{ExceptionPoint, VirtualReg, register_map_policy, Translation, JalrPatchPoint, LoadStorePatchPoint};
use std::collections::BTreeMap;
use log::debug;

pub struct Codegen<'a> {
    a: Assembler,
    base_vpc: u64,
    spill: SpillMachine,
    raw: &'a [u8],
    v_offset_to_translation_offset: Box<[u32]>,
    exception_points: BTreeMap<u32, ExceptionPoint>,
    jalr_patch_points: BTreeMap<u32, JalrPatchPoint>,
    load_store_patch_points: BTreeMap<u32, LoadStorePatchPoint>,
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
            exception_points: BTreeMap::new(),
            jalr_patch_points: BTreeMap::new(),
            load_store_patch_points: BTreeMap::new(),
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
            exception_points: self.exception_points,
            jalr_patch_points: self.jalr_patch_points,
            load_store_patch_points: self.load_store_patch_points,
        }
    }

    fn prepare_rel_br(&mut self, vpc: u64, offset: i32) -> Option<DynamicLabel> {
        let dst_pc = ((vpc as i64) + (offset as i64)) as u64;

        if dst_pc < self.base_vpc {
            self.emit_exception(vpc, error::ERROR_REASON_BRANCH_OOB);
            return None;
        }

        let dst_offset = (dst_pc - self.base_vpc) / 2;
        if dst_offset >= self.relative_br_labels.len() as u64 {
            self.emit_exception(vpc, error::ERROR_REASON_BRANCH_OOB);
            return None;
        }

        let label = self.relative_br_labels[dst_offset as usize].clone();
        Some(label)
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
            0b0110011 => {
                // arith-r 64-bit
                let (rd, rs, rt, rd_h, rs_h, rt_h) = self.spill.map_register_triple(&mut self.a, i_rd(inst) as _, i_rs(inst) as _, i_rt(inst) as _);

                self.emit_rtype(vpc, inst, rd, rs, rt);

                self.spill.release_register_r(&mut self.a, rs_h);
                self.spill.release_register_r(&mut self.a, rt_h);
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
            0b0111011 => {
                // arith-r 32-bit
                let funct3 = i_funct3(inst);
                if funct3 == 0b010 || funct3 == 0b011 {
                    self.emit_ud(vpc, inst);
                    return;
                }

                let (rd, rs, rt, rd_h, rs_h, rt_h) = self.spill.map_register_triple(&mut self.a, i_rd(inst) as _, i_rs(inst) as _, i_rt(inst) as _);

                self.emit_rtype(vpc, inst, rd, rs, rt);

                self.spill.release_register_r(&mut self.a, rs_h);
                self.spill.release_register_r(&mut self.a, rt_h);
                self.spill.release_register_w(&mut self.a, rd_h);
            }
            0b1100011 => {
                // conditional branch
                let funct3 = i_funct3(inst);
                let imm = i_btype_imm(inst);

                if funct3 == 0b010 || funct3 == 0b011 {
                    self.emit_ud(vpc, inst);
                } else {
                    let label = match self.prepare_rel_br(vpc, imm as i32) {
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

                    self.emit_btype_late(vpc, inst, label);
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

                let value = vpc + ((imm << 12) as i32 as i64 as u64);
                ld_imm64(&mut self.a, rd, value);

                self.spill.release_register_w(&mut self.a, rd_h);
            }
            0b1101111 => {
                // jal
                let imm = i_jtype_imm(inst);
                let label = match self.prepare_rel_br(vpc, imm as i32) {
                    Some(x) => x,
                    None => return
                };
                
                let (rd, rd_h) = self.spill.map_register_w(&mut self.a, i_rd(inst) as _, &[]);
                ld_imm64(&mut self.a, rd, vpc + 4);
                self.spill.release_register_w(&mut self.a, rd_h);

                dynasm!(self.a
                    ; .arch aarch64
                    ; b =>label
                );
            }
            0b1100111 => {
                // jalr
                let imm = i_itype_imm(inst);
                self.emit_jalr(vpc, i_rd(inst) as _, i_rs(inst) as _, imm)
            }
            0b0000011 => {
                // load
                let raw_rd = i_rd(inst) as _;
                let raw_rs = i_rs(inst) as _;
                let imm = i_itype_imm(inst);

                match i_funct3(inst) {
                    0b000 => {
                        // lb
                        self.emit_load(vpc, raw_rd, raw_rs, imm, 1, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrsb X(rd), [X(rs)]
                            );
                        });
                    }
                    0b001 => {
                        // lh
                        self.emit_load(vpc, raw_rd, raw_rs, imm, 2, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrsh X(rd), [X(rs)]
                            );
                        });
                    }
                    0b010 => {
                        // lw
                        self.emit_load(vpc, raw_rd, raw_rs, imm, 4, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrsw X(rd), [X(rs)]
                            );
                        });
                    }
                    0b011 => {
                        // ld
                        self.emit_load(vpc, raw_rd, raw_rs, imm, 8, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldr X(rd), [X(rs)]
                            );
                        });
                    }
                    0b100 => {
                        // lbu
                        self.emit_load(vpc, raw_rd, raw_rs, imm, 1, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrb W(rd), [X(rs)]
                            );
                        });
                    }
                    0b101 => {
                        // lhu
                        self.emit_load(vpc, raw_rd, raw_rs, imm, 2, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldrh W(rd), [X(rs)]
                            );
                        });
                    }
                    0b110 => {
                        // lwu
                        self.emit_load(vpc, raw_rd, raw_rs, imm, 4, |this, rd, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; ldr W(rd), [X(rs)]
                            );
                        });
                    }
                    _ => self.emit_ud(vpc, inst)
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
                        self.emit_store(vpc, raw_rs, raw_rt, imm, 1, |this, rt, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; strb W(rt), [X(rs)]
                            );
                        });
                    }
                    0b001 => {
                        // sh
                        self.emit_store(vpc, raw_rs, raw_rt, imm, 2, |this, rt, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; strh W(rt), [X(rs)]
                            );
                        });
                    }
                    0b010 => {
                        // sw
                        self.emit_store(vpc, raw_rs, raw_rt, imm, 4, |this, rt, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; str W(rt), [X(rs)]
                            );
                        });
                    }
                    0b011 => {
                        // sd
                        self.emit_store(vpc, raw_rs, raw_rt, imm, 8, |this, rt, rs| {
                            dynasm!(this.a
                                ; .arch aarch64
                                ; str X(rt), [X(rs)]
                            );
                        });
                    }
                    _ => self.emit_ud(vpc, inst)
                }
            }
            0b1110011 => {
                // ecall/ebreak
                let imm = i_itype_imm(inst);
                if imm & 0b1 == 0 {
                    // ecall
                    self.emit_exception(vpc, error::ERROR_REASON_ECALL);
                } else {
                    // ebreak
                    self.emit_exception(vpc, error::ERROR_REASON_EBREAK);
                }
            }
            _ => self.emit_ud(vpc, inst)
        }
    }

    fn emit_load<F: FnOnce(&mut Self, u32, u32)>(&mut self, vpc: u64, raw_rd: u32, raw_rs: u32, imm: u32, access_size: u32, emit_inner: F) {
        let (rd, rs, rd_h, rs_h) = self.spill.map_register_tuple_w_r(&mut self.a, raw_rd as _, raw_rs as _);
        let (t0, t0_h) = self.spill.mk_temp(&mut self.a, &[raw_rd as _, raw_rs as _]);

        // Compute effective address
        ld_simm16(&mut self.a, t0 as _, imm);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(t0 as u32), X(t0 as u32), X(rs as u32)
        );

        // Upper bound
        // XXX: Substract load size when patching
        let pp_upper = self.a.offset().0;
        ld_imm64(&mut self.a, 30, 0); // PATCH
        dynasm!(self.a
            ; .arch aarch64
            ; cmp X(t0 as u32), x30
            ; b.hs >fallback
        );

        // Lower bound
        let pp_lower = self.a.offset().0;
        ld_imm64(&mut self.a, 30, std::u64::MAX); // PATCH
        dynasm!(self.a
            ; .arch aarch64
            ; cmp X(t0 as u32), x30
            ; b.lo >fallback
        );

        // v_addr + this_offset = real_addr
        let pp_reloff = self.a.offset().0;
        ld_imm64(&mut self.a, 30, std::u64::MAX); // PATCH
        dynasm!(self.a
            ; .arch aarch64
            ; add X(t0 as u32), X(t0 as u32), x30
        );

        emit_inner(self, rd as _, t0 as _);

        dynasm!(self.a
            ; .arch aarch64
            ; b >ok
            ; fallback:
        );

        self.emit_exception(vpc, error::ERROR_REASON_LOAD_STORE_MISS);
        let pp = LoadStorePatchPoint {
            lower_bound_offset: pp_lower as _,
            upper_bound_offset: pp_upper as _,
            reloff_offset: pp_reloff as _,
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

        self.spill.release_temp(&mut self.a, t0_h);
        self.spill.release_register_r(&mut self.a, rs_h);
        self.spill.release_register_w(&mut self.a, rd_h);
    }

    fn emit_store<F: FnOnce(&mut Self, u32, u32)>(&mut self, vpc: u64, raw_rs: u32, raw_rt: u32, imm: u32, access_size: u32, emit_inner: F) {
        let (rs, rt, rs_h, rt_h) = self.spill.map_register_tuple_r_r(&mut self.a, raw_rs as _, raw_rt as _);
        let (t0, t0_h) = self.spill.mk_temp(&mut self.a, &[raw_rs as _, raw_rt as _]);

        // Compute effective address
        ld_simm16(&mut self.a, t0 as _, imm);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(t0 as u32), X(t0 as u32), X(rs as u32)
        );

        // Upper bound
        // XXX: Substract load size when patching
        let pp_upper = self.a.offset().0;
        ld_imm64(&mut self.a, 30, 0); // PATCH
        dynasm!(self.a
            ; .arch aarch64
            ; cmp X(t0 as u32), x30
            ; b.hs >fallback
        );

        // Lower bound
        let pp_lower = self.a.offset().0;
        ld_imm64(&mut self.a, 30, std::u64::MAX); // PATCH
        dynasm!(self.a
            ; .arch aarch64
            ; cmp X(t0 as u32), x30
            ; b.lo >fallback
        );

        // v_addr + this_offset = real_addr
        let pp_reloff = self.a.offset().0;
        ld_imm64(&mut self.a, 30, std::u64::MAX); // PATCH
        dynasm!(self.a
            ; .arch aarch64
            ; add X(t0 as u32), X(t0 as u32), x30
        );

        emit_inner(self, rt as _, t0 as _);

        dynasm!(self.a
            ; .arch aarch64
            ; b >ok
            ; fallback:
        );

        self.emit_exception(vpc, error::ERROR_REASON_LOAD_STORE_MISS);
        let pp = LoadStorePatchPoint {
            lower_bound_offset: pp_lower as _,
            upper_bound_offset: pp_upper as _,
            reloff_offset: pp_reloff as _,
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

        self.spill.release_temp(&mut self.a, t0_h);
        self.spill.release_register_r(&mut self.a, rs_h);
        self.spill.release_register_r(&mut self.a, rt_h);
    }

    fn emit_jalr(&mut self, vpc: u64, raw_rd: u32, raw_rs: u32, imm: u32) {
        let (rd, rs, rd_h, rs_h) = self.spill.map_register_tuple_w_r(&mut self.a, raw_rd as _, raw_rs as _);
        let (t0, t0_h) = self.spill.mk_temp(&mut self.a, &[raw_rd as _, raw_rs as _]);

        // XXX: Don't use rd as buffer! In case rd == xzr.
        let (t1, t1_h) = self.spill.mk_temp(&mut self.a, &[raw_rd as _, raw_rs as _]);

        ld_simm16(&mut self.a, 30, imm);
        dynasm!(self.a
            ; .arch aarch64
            ; add X(t1 as u32), x30, X(rs as u32) // use rd as a buffer here
        );

        // Upper bound
        let pp_upper = self.a.offset().0;
        ld_imm64(&mut self.a, 30, 0); // PATCH
        dynasm!(self.a
            ; .arch aarch64
            ; cmp X(t1 as u32), x30
            ; b.hs >fallback
        );

        // Lower bound
        let pp_lower = self.a.offset().0;
        ld_imm64(&mut self.a, 30, std::u64::MAX); // PATCH
        dynasm!(self.a
            ; .arch aarch64
            ; cmp X(t1 as u32), x30
            ; b.lo >fallback
            ; sub X(t1 as u32), X(t1 as u32), x30
        );

        // V/real offset table
        let pp_v2real_table = self.a.offset().0;
        ld_imm64(&mut self.a, 30, 0); // PATCH

        // Load real offset from v2real table
        dynasm!(self.a
            ; .arch aarch64
            ; lsr X(t1 as u32), X(t1 as u32), 1
            ; lsl X(t1 as u32), X(t1 as u32), 2
            ; add x30, X(t1 as u32), x30
            ; ldr W(t0 as u32), [x30]
        );

        // Machine code base
        let pp_machine_base = self.a.offset().0;
        ld_imm64(&mut self.a, 30, 0); // PATCH
        dynasm!(self.a
            ; .arch aarch64
            ; add x30, X(t0 as u32), x30 // compute actual address by base + offset
            ; b >ok
        );

        dynasm!(self.a
            ; .arch aarch64
            ; fallback:
        );

        let pp = JalrPatchPoint {
            lower_bound_offset: pp_lower as u32,
            upper_bound_offset: pp_upper as u32,
            v2real_table_offset: pp_v2real_table as u32,
            machine_base_offset: pp_machine_base as u32,
            rd: raw_rd as u32,
            rs: raw_rs as u32,
            rs_offset: imm as i32,
        };
        self.emit_exception(vpc, error::ERROR_REASON_JALR_MISS);
        let asm_offset = self.a.offset().0;
        self.jalr_patch_points.insert(asm_offset as u32, pp);

        dynasm!(self.a
            ; .arch aarch64
            ; ok:
        );
        ld_imm64(&mut self.a, rd, vpc + 4);

        self.spill.release_temp(&mut self.a, t0_h);
        self.spill.release_temp(&mut self.a, t1_h);
        self.spill.release_register_r(&mut self.a, rs_h);
        self.spill.release_register_w(&mut self.a, rd_h);

        dynasm!(self.a
            ; .arch aarch64
            ; br x30
        );
        
    }

    fn emit_rtype(&mut self, vpc: u64, inst: u32, rd: usize, rs: usize, rt: usize) {
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting add. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting sll. vpc = 0x{:016x}", vpc);
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
                                debug!("emit_rtype: mulhsu not yet implemented. Replacing with mulh. vpc = 0x{:016x}", vpc);
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; smulh X(rd as u32), X(rs as u32), X(rt as u32)
                                );
                            }
                            _ => {
                                // slt
                                if funct7 != 0b0000000 {
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting slt. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting sltu. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting xor. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting srl. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting or. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting and. vpc = 0x{:016x}", vpc);
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
                                    ; subs W(rd as u32), W(rs as u32), W(rt as u32)
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting addw. vpc = 0x{:016x}", vpc);
                                }
                                dynasm!(self.a
                                    ; .arch aarch64
                                    ; adds W(rd as u32), W(rs as u32), W(rt as u32)
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting sllw. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting divw. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting srlw. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting remw. vpc = 0x{:016x}", vpc);
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
                                    debug!("emit_rtype: bad funct7 but we cannot fail here. emitting remuw. vpc = 0x{:016x}", vpc);
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

    fn emit_btype_late(&mut self, vpc: u64, inst: u32, label: DynamicLabel) {
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
        debug!("static exception @ 0x{:016x}: reason {}", vpc, reason);

        ld_simm16(&mut self.a, 30, reason as u32);

        dynasm!(self.a
            ; .arch aarch64
            ; str x30, [X(runtime_reg() as u32), Runtime::offset_error_reason() as u32]

            ; ldr x30, [X(runtime_reg() as u32), Runtime::offset_exception_entry() as u32]
            ; blr x30
        );
        let translation_offset = self.a.offset().0 as u32;
        let v_offset = (vpc - self.base_vpc) as u32;
        let spill_mask = self.spill.spilled_regs.get();
        self.exception_points.insert(translation_offset, ExceptionPoint {
            v_offset,
            spill_mask,
        });
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

pub(crate) fn ld_simm16<A: DynasmApi>(a: &mut A, rd: usize, imm: u32) {
    if rd == 31 {
        // xzr
        return;
    }

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

pub(crate) fn ld_imm64<A: DynasmApi>(a: &mut A, rd: usize, imm: u64) {
    if rd == 31 {
        // xzr
        return;
    }

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

fn runtime_reg() -> usize {
    2
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