extern crate rvjit_aa64;

use rvjit_aa64::runtime::{Runtime, Section, SectionFlags};
use std::sync::Arc;

static IMAGE: &'static [u8] = include_bytes!("../../playground/play.bin");

fn main() {
    let section = Section::new(0x10000, SectionFlags::R | SectionFlags::X, IMAGE.to_vec());
    let mut rt = Runtime::new();
    assert!(rt.add_section(Arc::new(section)));
    rt.run(0x10000).unwrap();
}
