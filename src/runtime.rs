use crate::config::*;
use bitflags::bitflags;
use std::collections::BTreeMap;
use std::sync::Arc;
use crate::translation::{Translation, VirtualReg, register_map_policy};
use dynasmrt::AssemblyOffset;
use std::sync::Mutex;
use crate::error::ExecError;

#[repr(C)]
pub struct Runtime {
    pending_jalr_target: u64,
    error_reason: u64,
    exception_entry: unsafe extern "C" fn () -> !,
    memory_regs: [u64; 4],
    spill: [u64; 32],
    guest_save: [u64; 32],
    sections: Box<BTreeMap<u64, Arc<Section>>>,
}

impl Runtime {
    pub const fn offset_pending_jalr_target() -> usize {
        0
    }

    pub const fn offset_error_reason() -> usize {
        Self::offset_pending_jalr_target() + 8
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
            pending_jalr_target: 0,
            error_reason: 0,
            exception_entry: crate::entry_exit::_rvjit_guest_exception,
            memory_regs: [0; 4],
            spill: [0; 32],
            guest_save: [0; 32],
            sections: Box::new(BTreeMap::new()),
        }
    }

    pub fn run(&mut self, vpc: u64) -> Result<u64, ExecError> {
        let section = self.lookup_section(vpc).expect("Runtime::run: section not found");
        if !section.flags.contains(SectionFlags::X) {
            panic!("Runtime::run: bad section permissions");
        }
        let exit_vpc = section.run(self, vpc)?;
        Ok(exit_vpc)
    }

    pub fn error_reason(&self) -> u16 {
        self.error_reason as u16
    }

    pub fn read_register(&self, index: usize) -> u64 {
        assert!(index < 32, "Runtime::read_register: invalid index");
        match register_map_policy(index) {
            VirtualReg::Native(i) => self.guest_save[i],
            VirtualReg::Memory(i) => self.memory_regs[i],
        }
    }

    pub fn write_register(&mut self, index: usize, value: u64) {
        assert!(index < 32, "Runtime::write_register: invalid index");
        match register_map_policy(index) {
            VirtualReg::Native(i) => {
                self.guest_save[i] = value;
            }
            VirtualReg::Memory(i) => {
                self.memory_regs[i] = value;
            }
        }
    }

    pub fn lookup_section(&self, addr: u64) -> Option<Arc<Section>> {
        let target = self.sections.range(..=addr).rev().next();
        match target {
            Some((_, x)) if x.base_v + (x.data.len() as u64) > addr => {
                Some(x.clone())
            }
            _ => None
        }
    }

    pub fn add_section(&mut self, section: Arc<Section>) -> bool {
        let section_start = section.base_v;

        // Section alignment
        if section_start % 4096 != 0 {
            return false;
        }

        let section_end = match section_start.checked_add(section.data.len() as u64) {
            Some(x) => x,
            None => return false,
        };

        // Overlapping case 1
        if let Some((_, s)) = self.sections.range(..=section_start).rev().next() {
            if s.base_v + s.data.len() as u64 > section_start {
                return false;
            }
        }

        // Overlapping case 2
        if let Some((_, s)) = self.sections.range(section_start..).next() {
            if s.base_v < section_end {
                return false;
            }
        }

        self.sections.insert(section_start, section);
        return true;
    }

    pub fn remove_section(&mut self, addr: u64) -> bool {
        let target = self.sections.range(..=addr).rev().next();
        match target {
            Some((&k, x)) if x.base_v + (x.data.len() as u64) > addr => {
                self.sections.remove(&k);
                true
            }
            _ => false
        }
    }

    pub fn debug_print_registers(&self) {
        for i in 0..32 {
            let v = self.read_register(i);
            println!("x{} = 0x{:016x}", i, v);
        }
    }
}

pub struct Section {
    pub base_v: u64,
    pub flags: SectionFlags,
    pub data: Vec<u8>,
    translation: Box<Mutex<Option<Translation>>>,
}

impl Section {
    pub fn new(base_v: u64, flags: SectionFlags, data: Vec<u8>) -> Self {
        Self {
            base_v,
            flags,
            data,
            translation: Box::new(Mutex::new(None)),
        }
    }

    fn run(&self, rt: &mut Runtime, vpc: u64) -> Result<u64, ExecError> {
        if !self.flags.contains(SectionFlags::X) {
            return Err(ExecError::NoX);
        }
    
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

                let raw_offset = rt.guest_save[30] - (ptr as u64);
                let exit_v_offset = translation.translate_exception_offset(raw_offset as u32).expect("Section::run: cannot find exception offset");
                let exit_vpc = self.base_v + (exit_v_offset as u64);
                Ok(exit_vpc)
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