use crate::config::*;
use std::collections::BTreeMap;
use std::sync::Arc;
use crate::translation::{Translation, VirtualReg, register_map_policy};
use dynasmrt::AssemblyOffset;
use std::sync::Mutex;
use crate::error::{self, ExecError};
use bit_field::BitField;
use crate::section::{SectionData, SectionFlags};

#[repr(C)]
pub struct Runtime {
    error_data: u64,
    error_reason: u64,
    exception_entry: unsafe extern "C" fn () -> !,
    memory_regs: [u64; 4],
    spill: [u64; 32], // FIXME: only the first 256 bytes of Runtime are directly addressable so latter spill locations may not be accessible
    guest_save: [u64; 32],

    pub vpc: u64,
    sections: Box<BTreeMap<u64, Arc<Section>>>,
}

impl Runtime {
    pub const fn offset_error_data() -> usize {
        0
    }

    pub const fn offset_error_reason() -> usize {
        Self::offset_error_data() + 8
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
            error_data: 0,
            error_reason: 0,
            exception_entry: crate::entry_exit::_rvjit_guest_exception,
            memory_regs: [0; 4],
            spill: [0; 32],
            guest_save: [0; 32],

            vpc: 0,
            sections: Box::new(BTreeMap::new()),
        }
    }

    pub fn run(&mut self) -> Result<(), ExecError> {
        self.vpc &= !1u64; // to match the behavior of JIT

        loop {
            let section = match self.lookup_section(self.vpc) {
                Some(x) => x,
                None => return Err(ExecError::BadPC),
            };
            let e = section.run(self).unwrap_err();
            match e {
                ExecError::Retry => {
                    
                }
                _ => return Err(e)
            }
        }
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
            Some((_, x)) if x.base_v + (x.data.get().len() as u64) > addr => {
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

        let section_end = match section_start.checked_add(section.data.get().len() as u64) {
            Some(x) => x,
            None => return false,
        };

        // Overlapping case 1
        if let Some((_, s)) = self.sections.range(..=section_start).rev().next() {
            if s.base_v + s.data.get().len() as u64 > section_start {
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
            Some((&k, x)) if x.base_v + (x.data.get().len() as u64) > addr => {
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

    fn unspill(&mut self, mask: u32) {
        for i in 0..31 {
            if mask.get_bit(i) {
                self.guest_save[i] = self.spill[i];
            }
        }
    }
}

pub struct Section {
    pub base_v: u64,
    pub data: SectionData,
    translation: Box<Mutex<Option<Translation>>>,
}

impl Section {
    pub fn new(base_v: u64, data: SectionData) -> Self {
        Self {
            base_v,
            data,
            translation: Box::new(Mutex::new(None)),
        }
    }

    fn ensure_translation(&self) -> Result<(), ExecError> {
        if !self.data.get_flags().contains(SectionFlags::X) {
            return Err(ExecError::NoX);
        }

        let mut translation = self.translation.lock().unwrap();
        if translation.is_none() {
            *translation = Some(Translation::new(self.base_v, self.data.get()));
        }

        Ok(())
    }

    fn run(&self, rt: &mut Runtime) -> Result<(), ExecError> {
        self.ensure_translation()?;
    
        let v_offset = rt.vpc.checked_sub(self.base_v).expect("Section::run: invalid address") as u32;
        let mut translation = self.translation.lock().unwrap();
        let translation = translation.as_mut().unwrap();
        match translation.translate_v_offset(v_offset) {
            Some(x) => {
                let real_offset = AssemblyOffset(x as _);
                let ptr;
                let base;

                {
                    let executor = translation.backing.reader();
                    let executor = executor.lock();
                    base = executor.ptr(AssemblyOffset(0)) as u64;
                    ptr = executor.ptr(real_offset) as u64;
                    unsafe {
                        crate::entry_exit::_rvjit_enter_guest(rt, ptr);
                    }
                }

                let exc_offset = (rt.guest_save[30] - base) as u32;
                let exc_point = translation.get_exception_point(exc_offset).expect("Section::run: cannot find exception point");
                let exit_vpc = self.base_v + (exc_point.v_offset as u64);

                println!("EXIT at vpc 0x{:016x}", exit_vpc);

                rt.vpc = exit_vpc;
                rt.unspill(exc_point.spill_mask);
                rt.debug_print_registers();

                match self.handle_exit(rt, translation, exc_offset) {
                    Ok(_) => {
                        panic!("unexpected Ok from handle_exit");
                    }
                    Err(e) => {
                        Err(e)
                    }
                }
            }
            None => {
                panic!("Section::run: invalid v-offset");
            }
        }
    }

    fn handle_exit(&self, rt: &mut Runtime, translation: &mut Translation, exc_offset: u32) -> Result<(), ExecError> {
        match rt.error_reason as u16 {
            error::ERROR_REASON_UNDEFINED_INSTRUCTION => {
                Err(ExecError::UndefinedInstruction)
            }
            error::ERROR_REASON_BRANCH_OOB => {
                Err(ExecError::BranchOob)
            }
            error::ERROR_REASON_JALR_MISS => {
                let pp = translation.get_jalr_patch_point(exc_offset).expect("handle_exit: cannot get jalr patch point");
                let jalr_target = rt.read_register(pp.rs as _) + (pp.rs_offset as i64 as u64);
                println!("JALR miss. Jump target = 0x{:016x}", jalr_target);

                let target_section = match rt.lookup_section(jalr_target) {
                    Some(x) => x,
                    None => return Err(ExecError::BadPC),
                };

                // Fast path for jumping into the same section.
                if target_section.base_v == self.base_v {
                    // TODO: Better way of detecting equivalence?
                    translation.patch_jalr(exc_offset, self.base_v, None);
                }

                let link_vpc = rt.vpc + 4;
                rt.write_register(pp.rd as _, link_vpc);
                rt.vpc = jalr_target;

                Err(ExecError::Retry)
            }
            error::ERROR_REASON_LOAD_STORE_MISS => {
                let pp = translation.get_load_store_patch_point(exc_offset).expect("handle_exit: cannot get load-store patch point");
                let addr = rt.read_register(pp.rs as _) + (pp.rs_offset as i64 as u64);
                println!("load/store miss. target = 0x{:016x}", addr);

                let target_section = match rt.lookup_section(addr) {
                    Some(x) => x,
                    None => return Err(ExecError::BadLoadStoreAddress),
                };

                // TODO: bookkeeping
                if pp.is_store && !target_section.data.get_flags().contains(SectionFlags::W) {
                    return Err(ExecError::BadLoadStoreFlags);
                }

                if !pp.is_store && !target_section.data.get_flags().contains(SectionFlags::R) {
                    return Err(ExecError::BadLoadStoreFlags);
                }

                translation.patch_load_store(exc_offset, &target_section);
                Err(ExecError::Retry)
            }
            _ => {
                panic!("Unknown error reason: {}", rt.error_reason);
            }
        }
    }
}
