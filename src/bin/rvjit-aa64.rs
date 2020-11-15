extern crate rvjit_aa64;

use rvjit_aa64::runtime::{Runtime, Section};
use rvjit_aa64::section::{SectionData, SectionFlags};
use std::sync::Arc;

static IMAGE: &'static [u8] = include_bytes!("../../playground/play.bin");

fn main() {
    let section_code = Section::new(0x10000, SectionData::new(IMAGE.to_vec(), SectionFlags::R | SectionFlags::X));
    let section_data = Section::new(0x20000, SectionData::new(vec![42u8; 65536], SectionFlags::R | SectionFlags::W));
    let mut rt = Runtime::new();
    assert!(rt.add_section(Arc::new(section_code)));
    assert!(rt.add_section(Arc::new(section_data)));

    rt.vpc = 0x10000;
    rt.run().unwrap();
}
