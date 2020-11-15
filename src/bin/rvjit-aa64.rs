extern crate rvjit_aa64;

use rvjit_aa64::runtime::{Runtime, Section};
use rvjit_aa64::section::{SectionData, SectionFlags};
use std::sync::Arc;

static IMAGE: &'static [u8] = include_bytes!("../../playground/play.bin");

fn main() {
    let section = Section::new(0x10000, SectionData::new(IMAGE.to_vec(), SectionFlags::R | SectionFlags::X));
    let mut rt = Runtime::new();
    assert!(rt.add_section(Arc::new(section)));

    rt.vpc = 0x10000;
    rt.run().unwrap();
}
