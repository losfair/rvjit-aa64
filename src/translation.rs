use dynasmrt::aarch64::Assembler;
use crate::codegen::Codegen;

#[repr(C)]
pub struct Translation {
    pub v_offset_to_translation_offset: Box<[u32]>,
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
        } else {
            Some(self.v_offset_to_translation_offset[index])
        }
    }
}