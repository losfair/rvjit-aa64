use crate::runtime::Runtime;

global_asm!(include_str!("entry_exit.asm"));

extern "C" {
    #[allow(improper_ctypes)]
    pub fn _rvjit_enter_guest(rt: &mut Runtime, entry: u64);
    pub fn _rvjit_guest_exception() -> !;
}
