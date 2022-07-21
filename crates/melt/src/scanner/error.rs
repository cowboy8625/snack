use super::Span;
use thiserror::Error;

#[allow(dead_code)]
#[derive(Debug, Error, Clone)]
pub enum Error {
    #[error("{0}: unknown char '{1}'")]
    UnknowChar(Span, char),
    #[error("{0}: unknown error return none")]
    UnknowError(Span),
}
