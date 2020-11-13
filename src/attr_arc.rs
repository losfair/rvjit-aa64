use std::num::NonZeroU64;
use std::sync::Arc;
use std::ops::Deref;
use std::marker::PhantomData;

#[repr(transparent)]
pub struct AttributedArc<T, Attr>(NonZeroU64, PhantomData<T>, PhantomData<Attr>);

impl<T, Attr: Copy> AttributedArc<T, Attr> {
    pub fn new(p: Arc<T>, data: Attr) -> Self {
        assert!(std::mem::size_of::<Attr>() == 2, "AttributedArc::new: bad attr size");

        let attr: u16 = unsafe { std::mem::transmute_copy(&data) };
        let raw = (Arc::into_raw(p) as u64) & 0xffff_ffff_ffffu64;
        AttributedArc(unsafe {
            NonZeroU64::new_unchecked(raw | ((attr as u64) << 48))
        }, PhantomData, PhantomData)
    }
}

impl<T, Attr> Clone for AttributedArc<T, Attr> {
    fn clone(&self) -> Self {
        AttributedArc::new(self.clone_arc(), self.get_data)
    }
}

impl<T, Attr> AttributedArc<T, Attr> {
    fn get_inner(&self) -> u64 {
        (((self.0.get() << 16) as i64) >> 16) as u64
    }

    pub fn get_data(&self) -> Attr {
        let value = (self.0.get() >> 48) as u16;
        let attr: Attr = unsafe { std::mem::transmute_copy(&value) };
        attr
    }

    pub fn into_arc(self) -> Arc<T> {
        let ret = unsafe {
            Arc::from_raw(self.get_inner() as *mut T)
        };
        std::mem::forget(self);
        ret
    }

    pub fn clone_arc(&self) -> Arc<T> {
        let arc = unsafe {
            Arc::from_raw(self.get_inner() as *mut T)
        };
        let ret = arc.clone();
        Arc::into_raw(arc);
        ret
    }
}

impl<T, Attr> Drop for AttributedArc<T, Attr> {
    fn drop(&mut self) {
        unsafe {
            Arc::from_raw(self.get_inner() as *mut T);
        }
    }
}

impl<T, Attr> Deref for AttributedArc<T, Attr> {
    type Target = T;
    fn deref(&self) -> &T {
        unsafe {
            &*(self.get_inner() as *mut T)
        }
    }
}
