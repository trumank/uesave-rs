#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("bad magic value (GVAS)")]
    BadMagic(),
    #[error("unknown array type: {0}")]
    UnknownArrayType(String),
    #[error("unknown SetProperty type: {0}")]
    UnknownSetType(String),
    #[error("unknown PropertyMeta type: {0}")]
    UnknownPropertyMeta(String),
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
    #[error("{0}")]
    Other(&'static str),
}
