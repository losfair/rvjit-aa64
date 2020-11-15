use thiserror::Error;

#[derive(Error, Debug)]
pub enum ExecError {
    #[error("unknown jit error")]
    Unknown,

    #[error("attempting to execute code in an NX section")]
    NoX,

    #[error("undefined instruction")]
    UndefinedInstruction,

    #[error("branch out of bounds")]
    BranchOob,

    #[error("bad instruction pointer")]
    BadPC,

    #[error("global jump")]
    GlobalJump,
}

pub(crate) const ERROR_REASON_UNDEFINED_INSTRUCTION: u16 = 1;
pub(crate) const ERROR_REASON_BRANCH_OOB: u16 = 2;
pub(crate) const ERROR_REASON_JALR_MISS: u16 = 3;
