use std::fmt::Display;

pub mod allocator;
pub mod defs;
pub mod interpret;
pub mod runtime;
pub mod value;
pub mod vptr;

pub type IpretResult<T> = Result<T, IpretError>;

#[derive(Debug, Clone)]
pub enum IpretError {
    Unimplemented,
    Misc(&'static str),
    MiscOwned(String),
}

impl Display for IpretError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unimplemented => write!(f, "Interpreter error: Not implemented"),
            Self::Misc(msg) => write!(f, "Interpreter error: Misc: {}", msg),
            Self::MiscOwned(msg) => write!(f, "Interpreter error: Misc: {}", msg),
        }
    }
}
