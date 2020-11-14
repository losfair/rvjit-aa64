extern crate rvjit_aa64;

use rvjit_aa64::runtime::{Runtime, Section, SectionFlags};
use std::sync::Arc;

static IMAGE: &'static [u8] = include_bytes!("../../playground/play.bin");

fn main() {
    let section = Section::new(0x10000, SectionFlags::R | SectionFlags::X, IMAGE.to_vec());
    let mut rt = Runtime::new();
    assert!(rt.add_section(Arc::new(section)));
    let exit_vpc = rt.run(0x10000).unwrap();
    println!("Return. VPC = 0x{:016x} Reason = {}", exit_vpc, rt.error_reason());
    rt.debug_print_registers();
}
