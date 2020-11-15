use crate::runtime::{Runtime, Section};
use crate::section::{SectionData, SectionFlags};
use anyhow::Result;
use xmas_elf::{ElfFile, program::{Type, Flags, SegmentData}};
use std::sync::Arc;
use thiserror::Error;
use log::debug;

#[derive(Error, Debug)]
pub enum LoadError {
    #[error("elf backend error: {0}")]
    Backend(&'static str),

    #[error("elf image has W+X section")]
    WXViolation,

    #[error("section rejected by runtime")]
    SectionRejected,
}

pub fn load(rt: &mut Runtime, image: &[u8]) -> Result<()> {
    let file = ElfFile::new(image).map_err(LoadError::Backend)?;
    for segment in file.program_iter() {
        match segment.get_type().map_err(LoadError::Backend)? {
            Type::Load => {
                let flags = segment.flags();
                let data = segment.get_data(&file).map_err(LoadError::Backend)?;
                let vaddr = segment.virtual_addr();

                match data {
                    SegmentData::Undefined(data) => {
                        let flags = convert_flags(flags)?;
                        debug!("loading data of length {} at vaddr 0x{:016x}. flags = {:?}", data.len(), vaddr, flags);
                        let section = Section::new(vaddr, SectionData::new(
                            data.to_vec(),
                            flags,
                        ));
                        if !rt.add_section(Arc::new(section)) {
                            return Err(LoadError::SectionRejected.into());
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    Ok(())
}

fn convert_flags(f: Flags) -> Result<SectionFlags> {
    if f.is_write() && f.is_execute() {
        return Err(LoadError::WXViolation.into());
    }

    let mut out = SectionFlags::empty();
    if f.is_read() {
        out |= SectionFlags::R;
    }
    if f.is_write() {
        out |= SectionFlags::W;
    }
    if f.is_execute() {
        out |= SectionFlags::X;
    }

    Ok(out)
}