use dynasmrt::{AssemblyOffset, ExecutableBuffer};
use crate::codegen::{self, Codegen};
use std::collections::BTreeMap;
use crate::error::ExecError;

pub struct Translation {
    pub v_offset_to_translation_offset: Box<[u32]>,
    pub exception_points: BTreeMap<u32, ExceptionPoint>,
    pub jalr_patch_points: BTreeMap<u32, JalrPatchPoint>,
    pub load_store_patch_points: BTreeMap<u32, LoadStorePatchPoint>,
    pub rtstore_template: Box<[u64]>,
    pub backing: ExecutableBuffer,
}

#[derive(Copy, Clone, Debug)]
pub struct ExceptionPoint {
    pub v_offset: u32,
    pub spill_mask: u32,
    pub reason: u16,
}

#[derive(Copy, Clone, Debug)]
pub struct JalrPatchPoint {
    pub lower_bound_slot: RtSlot,
    pub upper_bound_slot: RtSlot,
    pub v2real_table_slot: RtSlot,
    pub machine_base_slot: RtSlot,
    pub rd: u32,
    pub rs: u32,
    pub rs_offset: i32,
}

#[derive(Copy, Clone, Debug)]
pub struct LoadStorePatchPoint {
    pub lower_bound_slot: RtSlot,
    pub upper_bound_slot: RtSlot,
    pub reloff_slot: RtSlot,
    pub rs: u32,
    pub rs_offset: i32,
    pub access_size: u32,
    pub is_store: bool,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct RtSlot(pub u32);

impl Translation {
    pub fn new(raw: &[u8]) -> Self {
        let mut cg = Codegen::new(raw);
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

    pub fn patch_load_store(
        &self, 
        exc_offset: u32,
        rtstore: &mut [u64],
        target_base_v: u64,
        target_base_real: u64,
        target_len: u64,
    ) {
        let pp = match self.get_load_store_patch_point(exc_offset) {
            Some(x) => x,
            None => panic!("Translation::patch_load_store: cannot find patch point"),
        };

        let lower_bound = target_base_v;
        let upper_bound = target_base_v + target_len - (pp.access_size - 1) as u64;
        let reloff = target_base_real.wrapping_sub(target_base_v);

        rtstore[pp.lower_bound_slot.0 as usize] = lower_bound;
        rtstore[pp.upper_bound_slot.0 as usize] = upper_bound;
        rtstore[pp.reloff_slot.0 as usize] = reloff;
    }

    pub fn try_invalidate_load_store(&self, exc_offset: u32, rtstore: &mut [u64]) {
        let pp = match self.get_load_store_patch_point(exc_offset) {
            Some(x) => x,
            None => return,
        };

        rtstore[pp.lower_bound_slot.0 as usize] = std::u64::MAX;
        rtstore[pp.upper_bound_slot.0 as usize] = 0;
        rtstore[pp.reloff_slot.0 as usize] = std::u64::MAX;
    }

    pub fn patch_jalr(&self, exc_offset: u32, rtstore: &mut [u64], target_base_v: u64, target: Option<&Translation>) {
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
        let machine_base = target.backing.ptr(AssemblyOffset(0)) as u64;
        
        rtstore[pp.lower_bound_slot.0 as usize] = lower_bound;
        rtstore[pp.upper_bound_slot.0 as usize] = upper_bound;
        rtstore[pp.v2real_table_slot.0 as usize] = v2real_table;
        rtstore[pp.machine_base_slot.0 as usize] = machine_base;
    }
}

#[derive(Copy, Clone, Debug)]
pub enum VirtualReg {
    Zero,
    Native(usize),
    Memory(usize),
}

pub fn register_map_policy(x: usize) -> VirtualReg {
    use VirtualReg::*;

    match x {
        0 => Zero,
        1 => Native(0),
        2 => Native(1),
        3 => Memory(0),
        4 => Memory(1),
        x if x >= 5 && x <= 27 => Native(x - 2), // native x3-x25 available
        28 => Memory(2),
        29 => Memory(3),
        30 => Memory(4),
        31 => Memory(5),
        _ => unreachable!(),
    }
}

unsafe fn flush_cache_range(start: usize, len: usize) {
    // FIXME: We are assuming cacheline size == 64 here.
    for i in (0..len).step_by(64) {
        let addr = start + i;
        llvm_asm!("ic ivau, $0" :: "r"(addr) :: "volatile");
    }
}

pub const fn runtime_reg() -> u32 {
    2
}

pub const fn rtstore_reg() -> u32 {
    26
}

pub const fn temp1_reg() -> u32 {
    27
}

pub const fn zero_reg_r() -> usize {
    28
}

pub const fn trash_reg_w() -> usize {
    29
}
