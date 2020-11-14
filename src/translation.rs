use dynasmrt::aarch64::Assembler;
use crate::codegen::Codegen;
use std::collections::BTreeMap;

#[repr(C)]
pub struct Translation {
    pub v_offset_to_translation_offset: Box<[u32]>,
    pub exception_translation_offset_to_v_offset: BTreeMap<u32, u32>,
    pub backing: Assembler,
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

    pub fn translate_exception_offset(&self, exc_offset: u32) -> Option<u32> {
        self.exception_translation_offset_to_v_offset.get(&exc_offset).cloned()
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