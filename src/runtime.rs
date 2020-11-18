use crate::config::*;
use std::collections::{BTreeMap, BTreeSet};
use std::sync::{Arc, RwLockWriteGuard, RwLockReadGuard};
use crate::translation::{Translation, VirtualReg, register_map_policy};
use dynasmrt::AssemblyOffset;
use std::sync::Mutex;
use crate::error::{self, ExecError};
use bit_field::BitField;
use crate::section::{SectionData, SectionFlags};
use log::debug;
use std::time::SystemTime;
use crate::section::SectionRegistry;
use std::cell::Cell;
use std::sync::RwLock;
use std::marker::PhantomPinned;
use std::pin::Pin;

pub struct MtRuntime {
    signal_handles: Mutex<BTreeSet<*const Cell<u32>>>,
    section_registry: RwLock<SectionRegistry>,

    /// A mutex that should be taken by the thread that wants to take `signal_handles`,
    /// before calling `set_stop_all_threads`.
    signal_acquired: RwLock<()>,
}

unsafe impl Send for MtRuntime {}
unsafe impl Sync for MtRuntime {}

impl MtRuntime {
    fn set_stop_all_threads(&self) {
        let signal_handles = self.signal_handles.lock().unwrap();
        for handle in signal_handles.iter() {
            unsafe {
                (**handle).set(1);
            }
        }
    }

    fn clear_stop_all_threads(&self) {
        let signal_handles = self.signal_handles.lock().unwrap();
        for handle in signal_handles.iter() {
            unsafe {
                (**handle).set(0);
            }
        }
    }

    pub fn new(init_tid: u64) -> Arc<Self> {
        Arc::new(Self {
            signal_handles: Mutex::new(BTreeSet::new()),
            section_registry: RwLock::new(SectionRegistry::new(init_tid)),
            signal_acquired: RwLock::new(()),
        })
    }

    pub fn write_section_registry<'a>(&'a self) -> RwLockWriteGuard<'a, SectionRegistry> {
        let _acq = self.signal_acquired.write().unwrap();
        self.set_stop_all_threads();
        let x = self.section_registry.write().unwrap();
        drop(_acq);
        self.clear_stop_all_threads();
        x
    }

    pub fn read_section_registry<'a>(&'a self) -> RwLockReadGuard<'a, SectionRegistry> {
        self.section_registry.read().unwrap()
    }

    pub fn with_memory<F: FnOnce(*const [u8]) -> R, R>(&self, start: u64, len: Option<usize>, f: F) -> Result<R, ExecError> {
        let registry = self.section_registry.read().unwrap();
        let (base_v, section) = registry.lookup_section(start)?;
        let ro = unsafe { &*section.get() };
        let m = &ro[(start - base_v) as usize..];

        let m = if let Some(len) = len {
            if m.len() < len {
                return Err(ExecError::BadMemDerefAddr);
            }
            &m[..len]
        } else {
            m
        };

        // Still pass a raw pointer to the callback, since multiple mutable references can be created
        // and we want the user to be aware of it.
        Ok(f(m))
    }

    pub fn with_memory_mut<F: FnOnce(*mut [u8]) -> R, R>(&self, start: u64, len: Option<usize>, f: F) -> Result<R, ExecError> {
        let registry = self.section_registry.read().unwrap();
        let (base_v, section) = registry.lookup_section(start)?;
        let rw = if let Some(x) = section.get_mut() {
            unsafe {
                &mut *x
            }
        } else {
            return Err(ExecError::BadMemDerefFlags);
        };
        let m = &mut rw[(start - base_v) as usize..];

        let m = if let Some(len) = len {
            if m.len() < len {
                return Err(ExecError::BadMemDerefAddr);
            }
            &mut m[..len]
        } else {
            m
        };

        // Still pass a raw pointer to the callback, since multiple mutable references can be created
        // and we want the user to be aware of it.
        Ok(f(m))
    }
}

#[repr(C)]
pub struct Runtime {
    error_data: u64,
    _error_reason: u32,
    signal: Cell<u32>,
    exception_entry: unsafe extern "C" fn () -> !,
    memory_regs: [u64; 7],
    current_vbase: u64,
    spill: [u64; 32], // FIXME: only the first 256 bytes of Runtime are directly addressable so latter spill locations may not be accessible
    guest_save: [u64; 32],

    /// Return Address Stack. Holds machine (real) addresses.
    /// 
    /// Not directly addressable - needs `add`.
    ras: [u64; 64],

    pub vpc: u64,
    mt: Arc<MtRuntime>,
    tid: u64,

    /// A runtime cannot be moved since its `signal` address is kept in `mt`.
    _pin: PhantomPinned,
}

impl Drop for Runtime {
    fn drop(&mut self) {
        assert!(self.mt.signal_handles.lock().unwrap().remove(&(&self.signal as *const _)));
    }
}

impl Runtime {
    pub const fn offset_error_data() -> usize {
        0
    }

    pub const fn offset_error_reason() -> usize {
        Self::offset_error_data() + 8
    }

    pub const fn offset_signal() -> usize {
        Self::offset_error_reason() + 4
    }

    pub const fn offset_exception_entry() -> usize {
        Self::offset_signal() + 4
    }

    pub const fn offset_memory_regs() -> usize {
        Self::offset_exception_entry() + 8
    }

    pub const fn offset_current_vbase() -> usize {
        Self::offset_memory_regs() + 7 * 8
    }

    pub const fn offset_spill() -> usize {
        Self::offset_current_vbase() + 8
    }

    pub const fn offset_guest_save() -> usize {
        Self::offset_spill() + 32 * 8
    }

    pub const fn offset_ras() -> usize {
        Self::offset_guest_save() + 32 * 8
    }
}

impl Runtime {
    /// Creates a pin-boxed `Runtime`.
    /// 
    /// TODO: Pinning is required since the address of `signal` is registered to `mt`.
    pub fn new(mt: Arc<MtRuntime>, tid: u64) -> Box<Self> {
        let rt = Box::new(Self {
            error_data: 0,
            _error_reason: 0,
            signal: Cell::new(0),
            exception_entry: crate::entry_exit::_rvjit_guest_exception,
            memory_regs: [0; 7],
            current_vbase: 0,
            spill: [0; 32],
            guest_save: [0; 32],
            ras: [0; 64],

            vpc: 0,
            mt: mt.clone(),
            tid,
            _pin: PhantomPinned,
        });

        assert!(mt.signal_handles.lock().unwrap().insert(&rt.as_ref().signal));
        rt
    }

    pub fn run(&mut self) -> Result<(), ExecError> {
        self.vpc &= !1u64; // to match the behavior of JIT

        loop {
            let e = self.run_section().unwrap_err();
            match e {
                ExecError::Retry => {
                    
                }
                _ => return Err(e)
            }
        }
    }

    pub fn read_register(&self, index: usize) -> u64 {
        assert!(index < 32, "Runtime::read_register: invalid index");
        match register_map_policy(index) {
            VirtualReg::Native(i) => self.guest_save[i],
            VirtualReg::Memory(i) => self.memory_regs[i],
            VirtualReg::Zero => 0,
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
            VirtualReg::Zero => {}
        }
    }

    pub fn debug_print_registers(&self) {
        for i in 0..32 {
            let v = self.read_register(i);
            debug!("x{} = 0x{:016x}", i, v);
        }
    }

    pub fn mt(&self) -> &Arc<MtRuntime> {
        &self.mt
    }

    fn unspill(&mut self, mask: u32) {
        for i in 0..31 {
            if mask.get_bit(i) {
                self.guest_save[i] = self.spill[i];
            }
        }
    }

    pub fn run_section(&mut self) -> Result<(), ExecError> {
        let mt = self.mt.clone();
        let registry_r = mt.section_registry.read().unwrap();
        let (base_v, section) = registry_r.lookup_section(self.vpc)?;
        let translation = section.get_translation()?;
        let v_offset = (self.vpc - base_v) as u32;

        let real_offset = match translation.translate_v_offset(v_offset) {
            Some(x) => x,
            None => return Err(ExecError::MissingTranslation),
        };
        let rtstore = registry_r.get_thread_rtstore(self.tid, base_v)?;

        // Environment initialization
        self.guest_save[crate::translation::rtstore_reg() as usize] = rtstore.as_ptr() as u64;
        self.current_vbase = base_v;

        let base = translation.backing.ptr(AssemblyOffset(0)) as u64;
        let ptr = translation.backing.ptr(AssemblyOffset(real_offset as _)) as u64;
        unsafe {
            crate::entry_exit::_rvjit_enter_guest(self, ptr);
        }

        let exc_offset = (self.guest_save[30] - base) as u32;
        let exc_point = translation.get_exception_point(exc_offset).expect("Runtime::run_section: cannot find exception point");
        let exit_vpc = base_v + (exc_point.v_offset as u64);

        debug!("EXIT at vpc 0x{:016x}", exit_vpc);

        self.vpc = exit_vpc;
        self.unspill(exc_point.spill_mask);
        //rt.debug_print_registers();

        drop(registry_r);

        match self.handle_exit(exc_offset, exc_point.reason) {
            Ok(_) => {
                panic!("unexpected Ok from handle_exit");
            }
            Err(e) => {
                Err(e)
            }
        }
    }

    fn handle_exit(&mut self, exc_offset: u32, reason: u16) -> Result<(), ExecError> {

        match reason {
            error::ERROR_REASON_UNDEFINED_INSTRUCTION => {
                Err(ExecError::UndefinedInstruction)
            }
            error::ERROR_REASON_BRANCH_OOB => {
                Err(ExecError::BranchOob)
            }
            error::ERROR_REASON_JALR_MISS => {
                let mt = self.mt.clone();
                let mut registry = mt.write_section_registry();

                let (base_v, section) = registry.lookup_section(self.vpc)?;
                let translation = section.get_translation()?.clone();

                let pp = translation.get_jalr_patch_point(exc_offset).expect("handle_exit: cannot get jalr patch point");
                let jalr_target = self.read_register(pp.rs as _) + (pp.rs_offset as i64 as u64);
                debug!("JALR miss. Jump target = 0x{:016x}", jalr_target);

                let (target_base_v, target_section) = match registry.lookup_section(jalr_target) {
                    Ok(x) => x,
                    Err(_) => return Err(ExecError::BadPC),
                };
                let rtstore = registry.get_thread_rtstore_mut(self.tid, base_v)?;

                // Fast path for jumping into the same section.
                if target_base_v == base_v {
                    // Whether we got a table lookup failure
                    match translation.translate_v_offset((jalr_target - target_base_v) as u32) {
                        Some(_) => {}
                        None => {
                            return Err(ExecError::MissingTranslation);
                        }
                    }
                    let start_time = SystemTime::now();
                    translation.patch_jalr(exc_offset, rtstore, base_v, None);
                    let end_time = SystemTime::now();
                    debug!("patch_jalr duration: {:?}", end_time.duration_since(start_time));
                }

                let link_vpc = self.vpc + 4;
                self.write_register(pp.rd as _, link_vpc);
                self.vpc = jalr_target;

                Err(ExecError::Retry)
            }
            error::ERROR_REASON_LOAD_STORE_MISS => {
                let mt = self.mt.clone();
                let mut registry = mt.write_section_registry();

                let (base_v, section) = registry.lookup_section(self.vpc)?;
                let translation = section.get_translation()?.clone();

                let pp = translation.get_load_store_patch_point(exc_offset).expect("handle_exit: cannot get load-store patch point");
                let addr = self.read_register(pp.rs as _) + (pp.rs_offset as i64 as u64);
                debug!("load/store miss. target = 0x{:016x}", addr);

                let (target_base_v, target_section) = match registry.lookup_section(addr) {
                    Ok(x) => x,
                    Err(_) => return Err(ExecError::BadLoadStoreAddress),
                };

                if pp.is_store && !target_section.get_flags().contains(SectionFlags::W) {
                    return Err(ExecError::BadLoadStoreFlags);
                }

                if !pp.is_store && !target_section.get_flags().contains(SectionFlags::R) {
                    return Err(ExecError::BadLoadStoreFlags);
                }

                let target_base_real = target_section.get().as_ptr() as u64;
                let target_len = target_section.get().len() as u64;
                let rtstore = registry.get_thread_rtstore_mut(self.tid, base_v)?;

                let start_time = SystemTime::now();
                translation.patch_load_store(exc_offset, rtstore, target_base_v, target_base_real, target_len);
                let end_time = SystemTime::now();
                debug!("patch_load_store duration: {:?}", end_time.duration_since(start_time));
                Err(ExecError::Retry)
            }
            error::ERROR_REASON_ECALL => {
                self.vpc += 4;
                Err(ExecError::Ecall)
            }
            error::ERROR_REASON_EBREAK => {
                self.vpc += 4;
                Err(ExecError::Ebreak)
            }
            error::ERROR_REASON_SIGNAL => {
                // don't race section_registry with the signal sender
                self.mt.signal_acquired.read().unwrap();

                Err(ExecError::InterruptSignal)
            }
            _ => {
                panic!("Unknown error reason: {}", reason);
            }
        }

    }
}
