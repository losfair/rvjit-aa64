extern crate rvjit_aa64;

use rvjit_aa64::runtime::{Runtime, Section};
use rvjit_aa64::section::{SectionData, SectionFlags};
use rvjit_aa64::error::ExecError;
use std::sync::Arc;
use std::io::Write;
use log::debug;

static IMAGE: &'static [u8] = include_bytes!("../../playground/play.bin");

fn main() {
    env_logger::init();

    let section_code = Section::new(0x10000, SectionData::new(IMAGE.to_vec(), SectionFlags::R | SectionFlags::X));
    let section_data = Section::new(0x20000, SectionData::new(vec![42u8; 65536], SectionFlags::R | SectionFlags::W));
    let mut rt = Runtime::new();
    assert!(rt.add_section(Arc::new(section_code)));
    assert!(rt.add_section(Arc::new(section_data)));

    let mut host = Host::new();

    rt.vpc = 0x10000;
    loop {
        let e = rt.run().unwrap_err();
        match e {
            ExecError::Ecall => {
                debug!("Ecall at 0x{:016x}", rt.vpc);
                host.handle_ecall(&mut rt);
            }
            ExecError::Ebreak => {
                debug!("Ebreak at 0x{:016x}", rt.vpc);
                break;
            }
            _ => panic!("Runtime error: {:?}", e),
        }
    }
}

struct Host {

}

impl Host {
    fn new() -> Host {
        Host {

        }
    }

    fn handle_ecall(&mut self, rt: &mut Runtime) {
        let syscall_num = rt.read_register(10);
        match syscall_num {
            0 => {
                let c = rt.read_register(11) as u8;
                let mut stdout = std::io::stdout();
                stdout.write_all(&[c]).unwrap();
            }
            _ => panic!("handle_ecall: invalid syscall")
        }
    }
}