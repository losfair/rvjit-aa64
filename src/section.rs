use std::cell::UnsafeCell;
use bitflags::bitflags;
use std::sync::Arc;
use crate::translation::Translation;
use crate::error::ExecError;
use crate::tcache::TranslationCache;

/// Shared section data with safety guarantees.
pub struct SectionData {
    backing: Arc<SharedData>,
    flags: SectionFlags,
    translation: Option<LocalTranslation>,
}

#[derive(Clone)]
struct LocalTranslation {
    t: Arc<Translation>,
    rtstore: Box<[u64]>,
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
    pub fn new(data: Vec<u8>, flags: SectionFlags) -> Result<Self, ExecError> {
        check_flags(flags)?;
        Ok(Self {
            backing: SharedData::new(data.into_boxed_slice()),
            flags,
            translation: None,
        })
    }

    pub fn set_flags(&mut self, flags: SectionFlags) -> Result<(), ExecError> {
        check_flags(flags)?;
        self.flags = flags;

        // Unique ownership for writable sections. TODO: COW
        if self.flags.contains(SectionFlags::W) {
            if Arc::strong_count(&self.backing) > 1 {
                self.backing = self.backing.deep_clone();
            }
            self.translation = None;
        }

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

    pub fn get_mut(&self) -> Option<&mut [u8]> {
        if self.flags.contains(SectionFlags::W) {
            Some(unsafe {
                &mut *self.backing.0.get()
            })
        } else {
            None
        }
    }

    pub fn translation(&mut self, c: &TranslationCache) -> Result<(Arc<Translation>, &mut [u64]), ExecError> {
        if !self.flags.contains(SectionFlags::X) {
            return Err(ExecError::NoX);
        }

        if self.translation.is_none() {
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
            let rtstore = t.rtstore_template.clone();
            self.translation = Some(LocalTranslation {
                t,
                rtstore,
            });
        }

        let local_t = self.translation.as_mut().unwrap();
        let t = local_t.t.clone();
        Ok((t, &mut *local_t.rtstore))
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
