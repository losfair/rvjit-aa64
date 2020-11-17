use std::cell::UnsafeCell;
use bitflags::bitflags;
use std::sync::Arc;
use crate::translation::Translation;
use crate::error::ExecError;
use crate::tcache::TranslationCache;
use std::collections::BTreeMap;

pub struct SectionRegistry {
    sections: BTreeMap<u64, SectionData>,
    threads: BTreeMap<u64, ThreadLocal>,
}

#[derive(Clone)]
struct ThreadLocal {
    rtstores: BTreeMap<u64, Box<[u64]>>,
}

impl SectionRegistry {
    pub fn new(init_tid: u64) -> SectionRegistry {
        let mut threads = BTreeMap::new();
        threads.insert(init_tid, ThreadLocal {
            rtstores: BTreeMap::new(),
        });
        SectionRegistry {
            sections: BTreeMap::new(),
            threads,
        }
    }

    pub fn get_thread_rtstore(&self, tid: u64, base_v: u64) -> Result<&[u64], ExecError> {
        match self.threads.get(&tid) {
            Some(th) => {
                match th.rtstores.get(&base_v) {
                    Some(x) => Ok(x),
                    None => Err(ExecError::BadSection),
                }
            }
            None => Err(ExecError::BadThread),
        }
    }

    pub fn get_thread_rtstore_mut(&mut self, tid: u64, base_v: u64) -> Result<&mut [u64], ExecError> {
        match self.threads.get_mut(&tid) {
            Some(th) => {
                match th.rtstores.get_mut(&base_v) {
                    Some(x) => Ok(x),
                    None => Err(ExecError::BadSection),
                }
            }
            None => Err(ExecError::BadThread),
        }
    }

    pub fn add_section(&mut self, base_v: u64, section: SectionData) -> Result<(), ExecError> {
        let section_start = base_v;

        // Section alignment. Intentionally not required to align to page size.
        if section_start % 4 != 0 {
            return Err(ExecError::BadSection);
        }

        let section_end = match section_start.checked_add(section.get().len() as u64) {
            Some(x) => x,
            None => return Err(ExecError::BadSection),
        };

        // Overlapping case 1
        if let Some((&target_base_v, s)) = self.sections.range(..=section_start).rev().next() {
            if target_base_v + s.get().len() as u64 > section_start {
                return Err(ExecError::BadSection);
            }
        }

        // Overlapping case 2
        if let Some((&target_base_v, s)) = self.sections.range(section_start..).next() {
            if target_base_v < section_end {
                return Err(ExecError::BadSection);
            }
        }

        if let Some(ref t) = section.translation {
            for (_, th) in self.threads.iter_mut() {
                th.rtstores.insert(section_start, t.rtstore_template.clone());
            }
        }
        self.sections.insert(section_start, section);

        Ok(())
    }

    pub fn remove_section(&mut self, base_v: u64) -> Result<(), ExecError> {
        let section = match self.sections.get(&base_v) {
            Some(x) => x,
            None => return Err(ExecError::BadSection),
        };

        if let Some(ref t) = section.translation {
            for (_, th) in self.threads.iter_mut() {
                th.rtstores.remove(&base_v).expect("remove_section: rtstore not found");
            }
        }

        self.sections.remove(&base_v);

        self.jit_rtstore_full_flush();
        Ok(())
    }

    pub fn lookup_section(&self, addr: u64) -> Result<(u64, &SectionData), ExecError> {
        let target = self.sections.range(..=addr).rev().next();
        match target {
            Some((&target_base_v, section)) if target_base_v + (section.get().len() as u64) > addr => {
                Ok((target_base_v, section))
            }
            _ => Err(ExecError::BadSection)
        }
    }

    pub fn set_section_flags(&mut self, base_v: u64, flags: SectionFlags, c: &TranslationCache) -> Result<(), ExecError> {
        let section = match self.sections.get_mut(&base_v) {
            Some(x) => x,
            None => return Err(ExecError::BadSection),
        };
        section.set_flags(c, flags)?;

        if let Some(ref t) = section.translation {
            for (_, th) in self.threads.iter_mut() {
                th.rtstores.insert(base_v, t.rtstore_template.clone());
            }
        } else {
            for (_, th) in self.threads.iter_mut() {
                th.rtstores.remove(&base_v);
            }
        }

        self.jit_rtstore_full_flush();
        Ok(())
    }

    /// Flush rtstores of all threads.
    /// 
    /// TODO: Fine-grained flushing
    fn jit_rtstore_full_flush(&mut self) {
        for (_, th) in self.threads.iter_mut() {
            for (base_v, rtstore) in th.rtstores.iter_mut() {
                let section = self.sections.get(&base_v).expect("jit_rtstore_full_flush: section not found");
                let translation = section.translation.as_ref().expect("jit_rtstore_full_flush: translation not found");
                *rtstore = translation.rtstore_template.clone();
            }
        }
    }
}

/// Shared section data with safety guarantees.
pub struct SectionData {
    backing: Arc<SharedData>,
    flags: SectionFlags,
    translation: Option<Arc<Translation>>,
}

#[repr(transparent)]
struct SharedData(pub UnsafeCell<[u8]>);
unsafe impl Send for SharedData {}
unsafe impl Sync for SharedData {}

impl SharedData {
    fn new(data: Box<[u8]>) -> Arc<Self> {
        let data = Arc::from(data);
        let data = unsafe {
            std::mem::transmute::<Arc<[u8]>, Arc<SharedData>>(data)
        };
        data
    }

    fn deep_clone(&self) -> Arc<Self> {
        let data = unsafe { &*self.0.get() }.to_vec().into_boxed_slice();
        SharedData::new(data)
    }
}

impl SectionData {
    pub fn new(data: Vec<u8>, c: &TranslationCache, flags: SectionFlags) -> Result<Self, ExecError> {
        check_flags(flags)?;
        let mut me = Self {
            backing: SharedData::new(data.into_boxed_slice()),
            flags,
            translation: None,
        };
        me.populate_flags(c);
        Ok(me)
    }

    fn populate_flags(&mut self, c: &TranslationCache) {
        // Unique ownership for writable sections. TODO: COW
        if self.flags.contains(SectionFlags::W) {
            if Arc::strong_count(&self.backing) > 1 {
                self.backing = self.backing.deep_clone();
            }
            self.translation = None;
        }

        if self.flags.contains(SectionFlags::X) {
            let data = self.get();
            let hash = crate::tcache::hash(data);
            let t = match c.get(&hash) {
                Some(x) => x,
                None => {
                    let t = Arc::new(Translation::new(data));
                    c.insert(hash, t.clone());
                    t
                }
            };
            self.translation = Some(t);
        }
    }

    fn set_flags(&mut self, c: &TranslationCache, flags: SectionFlags) -> Result<(), ExecError> {
        check_flags(flags)?;
        self.flags = flags;
        self.populate_flags(c);
        Ok(())
    }

    pub fn get_flags(&self) -> SectionFlags {
        self.flags
    }

    pub fn get(&self) -> &[u8] {
        unsafe {
            &*self.backing.0.get()
        }
    }

    pub fn get_mut(&self) -> Option<*mut [u8]> {
        if self.flags.contains(SectionFlags::W) {
            Some(self.backing.0.get())
        } else {
            None
        }
    }

    pub fn get_translation(&self) -> Result<&Arc<Translation>, ExecError> {
        match self.translation {
            Some(ref x) => Ok(x),
            None => Err(ExecError::NoX),
        }
    }
}

impl Clone for SectionData {
    fn clone(&self) -> Self {
        // Unique ownership for writable sections
        if self.flags.contains(SectionFlags::W) {
            Self {
                backing: self.backing.deep_clone(),
                flags: self.flags,
                translation: None,
            }
        } else {
            Self {
                backing: self.backing.clone(),
                flags: self.flags,
                translation: self.translation.clone(),
            }
        }
    }
}

bitflags! {
    pub struct SectionFlags: u16 {
        const R = 1;
        const W = 2;
        const X = 4;
    }
}

fn check_flags(f: SectionFlags) -> Result<(), ExecError> {
    if f.contains(SectionFlags::W) && f.contains(SectionFlags::X) {
        return Err(ExecError::WX);
    }

    Ok(())
}
