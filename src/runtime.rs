use crate::config::*;
use bitflags::bitflags;
use std::collections::BTreeMap;
use std::sync::Arc;
use crate::translation::Translation;
use dynasmrt::AssemblyOffset;
use std::sync::Mutex;

#[repr(C)]
pub struct Runtime {
    pub error_vpc: u64,
    pub error_reason: u64,
    pub exception_entry: unsafe extern "C" fn () -> !,
    pub memory_regs: [u64; 4],
    pub spill: [u64; 32],
    pub guest_save: [u64; 32],
    pub sections: BTreeMap<u64, SectionRef>,
}

impl Runtime {
    pub const fn offset_error_vpc() -> usize {
        0
    }

    pub const fn offset_error_reason() -> usize {
        Self::offset_error_vpc() + 8
    }

    pub const fn offset_exception_entry() -> usize {
        Self::offset_error_reason() + 8
    }

    pub const fn offset_memory_regs() -> usize {
        Self::offset_exception_entry() + 8
    }

    pub const fn offset_spill() -> usize {
        Self::offset_memory_regs() + 4 * 8
    }

    pub const fn offset_guest_save() -> usize {
        Self::offset_spill() + 32 * 8
    }
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            error_vpc: 0,
            error_reason: 0,
            exception_entry: crate::entry_exit::_rvjit_guest_exception,
            memory_regs: [0; 4],
            spill: [0; 32],
            guest_save: [0; 32],
            sections: BTreeMap::new(),
        }
    }

    pub fn lookup_section(&self, vpc: u64) -> Option<SectionRef> {
        let target = self.sections.range(..=vpc).rev().next();
        match target {
            Some((_, x)) if x.section.base_v + (x.section.data.len() as u64) > vpc => {
                Some(x.clone())
            }
            _ => None
        }
    }

    pub fn run(&mut self, vpc: u64) {
        let section = self.lookup_section(vpc).expect("Runtime::run: section not found");
        if !section.flags.contains(SectionFlags::X) {
            panic!("Runtime::run: bad section permissions");
        }
        section.section.run(self, vpc);
    }
}

#[derive(Clone)]
pub struct SectionRef {
    section: Arc<Section>,
    flags: SectionFlags,
}

#[repr(C)]
pub struct Section {
    pub base_v: u64,
    pub translation: Box<Mutex<Option<Translation>>>,
    pub data: [u8],
}

impl Section {
    pub fn run(&self, rt: &mut Runtime, vpc: u64) {
        let v_offset = vpc.checked_sub(self.base_v).expect("Section::run: invalid address") as u32;
        let mut translation = self.translation.lock().unwrap();
        if translation.is_none() {
            *translation = Some(Translation::new(self.base_v, &self.data));
        }
        let translation = translation.as_ref().unwrap();
        match translation.translate_v_offset(v_offset) {
            Some(x) => {
                let executor = translation.backing.reader();
                let executor = executor.lock();
                let ptr = executor.ptr(AssemblyOffset(0));
                unsafe {
                    crate::entry_exit::_rvjit_enter_guest(rt, ptr as u64);
                }
            }
            None => {
                panic!("Section::run: invalid v-offset");
            }
        }
    }
}

bitflags! {
    pub struct SectionFlags: u16 {
        const R = 1;
        const W = 2;
        const X = 4;
        const COW = 8;
    }
}