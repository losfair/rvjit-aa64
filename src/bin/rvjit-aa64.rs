extern crate rvjit_aa64;

use rvjit_aa64::runtime::{Runtime, Section};
use rvjit_aa64::section::{SectionData, SectionFlags};
use rvjit_aa64::error::ExecError;
use std::sync::Arc;
use std::io::{Read, Write};
use log::debug;
use rvjit_aa64::elf;
use std::fs::File;
use std::time::SystemTime;

const STACK_SIZE: usize = 65536;

fn main() {
    env_logger::init();

    let path = std::env::args().nth(1).expect("expecting path");

    let mut rt = Runtime::new();
    let mut elf_image_file = File::open(&path).unwrap();
    let mut elf_image = vec![0u8; 0];
    elf_image_file.read_to_end(&mut elf_image).unwrap();

    assert!(rt.add_section(Arc::new(Section::new(0x7fff00000000 - (STACK_SIZE as u64), SectionData::new(vec![0u8; STACK_SIZE], SectionFlags::R | SectionFlags::W).unwrap()))));

    elf::load_sections(&mut rt, &elf_image).unwrap();

    let mut host = Host::new();

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
            _ => {
                rt.debug_print_registers();
                panic!("Runtime error: {:?}", e);
            }
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
            1 => {
                let millis = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_millis();
                rt.write_register(10, millis as u64);
            }
            _ => panic!("handle_ecall: invalid syscall")
        }
    }
}