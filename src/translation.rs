use dynasmrt::{AssemblyOffset, aarch64::Assembler};
use crate::codegen::{self, Codegen};
use std::collections::BTreeMap;
use crate::error::ExecError;
use crate::runtime::Section;

pub struct Translation {
    pub v_offset_to_translation_offset: Box<[u32]>,
    pub exception_points: BTreeMap<u32, ExceptionPoint>,
    pub jalr_patch_points: BTreeMap<u32, JalrPatchPoint>,
    pub load_store_patch_points: BTreeMap<u32, LoadStorePatchPoint>,
    pub backing: Assembler,
}

#[derive(Copy, Clone, Debug)]
pub struct ExceptionPoint {
    pub v_offset: u32,
    pub spill_mask: u32,
}

#[derive(Copy, Clone, Debug)]
pub struct JalrPatchPoint {
    pub lower_bound_offset: u32,
    pub upper_bound_offset: u32,
    pub v2real_table_offset: u32,
    pub machine_base_offset: u32,
    pub rd: u32,
    pub rs: u32,
    pub rs_offset: i32,
}

#[derive(Copy, Clone, Debug)]
pub struct LoadStorePatchPoint {
    pub lower_bound_offset: u32,
    pub upper_bound_offset: u32,
    pub reloff_offset: u32,
    pub rs: u32,
    pub rs_offset: i32,
    pub access_size: u32,
    pub is_store: bool,
}

impl Translation {
    pub fn new(base_v: u64, raw: &[u8]) -> Self {
        let mut cg = Codegen::new(base_v, raw);
        cg.generate();
        cg.refine()
    }

    pub fn translate_v_offset(&self, v_offset: u32) -> Option<u32> {
        let index = (v_offset / 2) as usize;
        if index >= self.v_offset_to_translation_offset.len() {
            None
        } else if self.v_offset_to_translation_offset[index] == std::u32::MAX {
            None
        } else {
            Some(self.v_offset_to_translation_offset[index])
        }
    }

    pub fn get_exception_point(&self, exc_offset: u32) -> Option<ExceptionPoint> {
        self.exception_points.get(&exc_offset).cloned()
    }

    pub fn get_jalr_patch_point(&self, exc_offset: u32) -> Option<JalrPatchPoint> {
        self.jalr_patch_points.get(&exc_offset).cloned()
    }

    pub fn get_load_store_patch_point(&self, exc_offset: u32) -> Option<LoadStorePatchPoint> {
        self.load_store_patch_points.get(&exc_offset).cloned()
    }

    pub fn patch_load_store(&mut self, exc_offset: u32, target_section: &Section) {
        let pp = match self.get_load_store_patch_point(exc_offset) {
            Some(x) => x,
            None => panic!("Translation::patch_load_store: cannot find patch point"),
        };

        let lower_bound = target_section.base_v;
        let upper_bound = target_section.base_v + target_section.data.get().len() as u64 - (pp.access_size - 1) as u64;
        let reloff = (target_section.data.get().as_ptr() as u64).wrapping_sub(target_section.base_v);

        self.backing.alter(|m| {
            m.goto(AssemblyOffset(pp.lower_bound_offset as _));
            codegen::ld_imm64(m, 30, lower_bound);

            m.goto(AssemblyOffset(pp.upper_bound_offset as _));
            codegen::ld_imm64(m, 30, upper_bound);

            m.goto(AssemblyOffset(pp.reloff_offset as _));
            codegen::ld_imm64(m, 30, reloff);
        }).unwrap();
    }

    pub fn patch_jalr(&mut self, exc_offset: u32, target_base_v: u64, target: Option<&Translation>) {
        let pp = match self.get_jalr_patch_point(exc_offset) {
            Some(x) => x,
            None => panic!("Translation::patch_jalr: cannot find patch point"),
        };

        let target = if let Some(x) = target {
            x
        } else {
            &*self
        };
        
        let lower_bound = target_base_v;
        let upper_bound = target_base_v + ((target.v_offset_to_translation_offset.len() * 2) as u64);
        let v2real_table = target.v_offset_to_translation_offset.as_ptr() as u64;
        let machine_base = target.backing.reader().lock().ptr(AssemblyOffset(0)) as u64;

        self.backing.alter(|m| {
            m.goto(AssemblyOffset(pp.lower_bound_offset as _));
            codegen::ld_imm64(m, 30, lower_bound);

            m.goto(AssemblyOffset(pp.upper_bound_offset as _));
            codegen::ld_imm64(m, 30, upper_bound);

            m.goto(AssemblyOffset(pp.v2real_table_offset as _));
            codegen::ld_imm64(m, 30, v2real_table);

            m.goto(AssemblyOffset(pp.machine_base_offset as _));
            codegen::ld_imm64(m, 30, machine_base);
        }).unwrap();
    }
}

#[derive(Copy, Clone, Debug)]
pub enum VirtualReg {
    Native(usize),
    Memory(usize),
}

pub fn register_map_policy(x: usize) -> VirtualReg {
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