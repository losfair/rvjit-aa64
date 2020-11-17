use crate::runtime::{Runtime, Section};
use crate::section::{SectionData, SectionFlags};
use anyhow::Result;
use xmas_elf::{sections::ShType, ElfFile, program::{Type, Flags, SegmentData}};
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

    #[error("invalid elf")]
    InvalidElf,
}

/// Load ELF in section mode
pub fn load_sections(rt: &mut Runtime, image: &[u8]) -> Result<()> {
    let file = ElfFile::new(image).map_err(LoadError::Backend)?;
    for sec in file.section_iter() {
        let flags = sec.flags();
        let mut target_flags = SectionFlags::empty();
        let mut alloc = false;

        if flags & 0x1 != 0 {
            // write
            target_flags |= SectionFlags::W;
        }
        if flags & 0x2 != 0 {
            // alloc
            target_flags |= SectionFlags::R;
            alloc = true;
        }
        if flags & 0x4 != 0 {
            // exec
            if target_flags.contains(SectionFlags::W) {
                return Err(LoadError::WXViolation.into());
            }
            target_flags |= SectionFlags::X;
        }

        if alloc {
            let vaddr = sec.address();
            debug!("loading section of length {} at vaddr 0x{:016x}. flags = {:?}", sec.size(), vaddr, target_flags);

            if sec.size() == 0 {
                continue;
            }
            if vaddr % 4 != 0 {
                return Err(LoadError::InvalidElf.into());
            }

            let data = match sec.get_type().map_err(LoadError::Backend)? {
                ShType::ProgBits | ShType::PreInitArray | ShType::InitArray | ShType::FiniArray => sec.raw_data(&file).to_vec(),
                ShType::Null => {
                    continue;
                }
                ShType::StrTab => {
                    continue;
                }
                ShType::NoBits => {
                    // FIXME: DoS possible
                    vec![0u8; sec.size() as _]
                }
                x => {
                    debug!("unknown section type {:?}. ignoring.", x);
                    continue;
                }
            };

            let section = Section::new(vaddr, SectionData::new(
                data,
                target_flags,
            )?);
            if !rt.add_section(Arc::new(section)) {
                return Err(LoadError::SectionRejected.into());
            }
        }
    }

    let entry = file.header.pt2.entry_point();
    rt.vpc = entry;

    Ok(())
}

/// Load ELF in segment mode
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
                        let mut data = data.to_vec();

                        // FIXME: DoS possible
                        if data.len() < segment.mem_size() as usize {
                            data.resize(segment.mem_size() as usize, 0);
                        }

                        let section = Section::new(vaddr, SectionData::new(
                            data,
                            flags,
                        )?);
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

    let entry = file.header.pt2.entry_point();
    rt.vpc = entry;

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