use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum TinyLangError {
    #[error("parser error {0:?}")]
    ParserError(ParseError),
    #[error("runtime error {0:?}")]
    RuntimeError(RuntimeError),
}

impl From<ParseError> for TinyLangError {
    fn from(value: ParseError) -> Self {
        TinyLangError::ParserError(value)
    }
}

impl From<RuntimeError> for TinyLangError {
    fn from(value: RuntimeError) -> Self {
        TinyLangError::RuntimeError(value)
    }
}

#[derive(Debug, Error, PartialEq)]
pub enum RuntimeError {
    #[error("generic error {0:?}")]
    Generic(String),
    #[error("variable not defined {0:?}")]
    VariableNotDefined(String),
    #[error("Invalid Lang Type")]
    InvalidLangType,
    #[error("Identifier is not a Number")]
    ExpectingNumber,
}

#[derive(Debug, Error, PartialEq)]
pub enum ParseError {
    #[error("Error while parsing code {0:?}")]
    Generic(String),
    #[error("Invalid node type {0:?}")]
    InvalidNode(String),
}
