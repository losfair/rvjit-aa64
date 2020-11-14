use thiserror::Error;

#[derive(Error, Debug)]
pub enum JitError {
    #[error("unknown jit error")]
    Unknown,
}

pub const ERROR_REASON_UNDEFINED_INSTRUCTION: u16 = 1;
pub const ERROR_REASON_BRANCH_OOB: u16 = 2;
