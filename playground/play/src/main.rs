#![no_std]
#![no_main]
#![feature(global_asm, llvm_asm)]

use core::panic::PanicInfo;

global_asm!(include_str!("entry.asm"));

#[panic_handler]
fn on_panic(_info: &PanicInfo) -> ! {
    loop {}
}
