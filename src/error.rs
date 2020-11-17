use thiserror::Error;

#[derive(Error, Debug)]
pub enum ExecError {
    #[error("unknown jit error")]
    Unknown,

    #[error("attempting to execute code in an NX section")]
    NoX,

    #[error("W+X section not allowed")]
    WX,

    #[error("undefined instruction")]
    UndefinedInstruction,

    #[error("branch out of bounds")]
    BranchOob,

    #[error("bad instruction pointer")]
    BadPC,

    #[error("missing translation for instruction pointer")]
    MissingTranslation,

    #[error("bad load/store address")]
    BadLoadStoreAddress,

    #[error("bad load/store flags")]
    BadLoadStoreFlags,

    #[error("ecall")]
    Ecall,

    #[error("ebreak")]
    Ebreak,

    #[error("retry")]
    Retry,

    #[error("bad memory dereference address")]
    BadMemDerefAddr,

    #[error("bad memory dereference flags")]
    BadMemDerefFlags,
}

pub(crate) const ERROR_REASON_UNDEFINED_INSTRUCTION: u16 = 1;
pub(crate) const ERROR_REASON_BRANCH_OOB: u16 = 2;
pub(crate) const ERROR_REASON_JALR_MISS: u16 = 3;
pub(crate) const ERROR_REASON_LOAD_STORE_MISS: u16 = 4;
pub(crate) const ERROR_REASON_ECALL: u16 = 5;
pub(crate) const ERROR_REASON_EBREAK: u16 = 6;