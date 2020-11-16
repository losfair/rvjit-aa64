use std::cell::UnsafeCell;
use bitflags::bitflags;
use std::sync::Arc;

/// Shared section data with safety guarantees.
pub struct SectionData {
    backing: Arc<SharedData>,
    flags: SectionFlags,
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
    pub fn new(data: Vec<u8>, flags: SectionFlags) -> Self {
        Self {
            backing: SharedData::new(data.into_boxed_slice()),
            flags,
        }
    }

    pub fn set_flags(&mut self, flags: SectionFlags) {
        self.flags = flags;

        // Unique ownership for writable sections. TODO: COW
        if self.flags.contains(SectionFlags::W) {
            if Arc::strong_count(&self.backing) > 1 {
                self.backing = self.backing.deep_clone();
            }
        }
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
}

impl Clone for SectionData {
    fn clone(&self) -> Self {
        // Unique ownership for writable sections
        if self.flags.contains(SectionFlags::W) {
            Self {
                backing: self.backing.deep_clone(),
                flags: self.flags,
            }
        } else {
            Self {
                backing: self.backing.clone(),
                flags: self.flags,
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