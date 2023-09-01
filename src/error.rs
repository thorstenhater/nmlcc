use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error, Debug)]
pub enum Error {
    #[error("I/O error: {}", .source)]
    Io {
        #[from]
        source: std::io::Error,
    },
    #[error("XML error: {}", .source)]
    Xml {
        #[from]
        source: roxmltree::Error,
    },
    #[error("NMODL exporter error: {}", .what)]
    Nmodl { what: String },
    #[error("ACC exporter error: {}", .what)]
    Acc { what: String },
    #[error("Unit error: {}", .what)]
    Unit { what: String },
    #[error("NML2 error: {}", .what)]
    Nml { what: String },
    #[error("LEMS error: {}", .what)]
    Lems { what: String },
    #[error("Parse error: {}", .what)]
    Parse { what: String },
}

#[macro_export]
macro_rules! nml2_error {
    ($($arg:tt)*) => {{
        Error::Nml { what: format!($($arg)*) }
    }};
}

#[macro_export]
macro_rules! nmodl_error {
    ($($arg:tt)*) => {{
        Error::Nml { what: format!($($arg)*) }
    }};
}

#[macro_export]
macro_rules! parse_error {
    ($($arg:tt)*) => {{
        Error::Parse { what: format!($($arg)*) }
    }};
}
