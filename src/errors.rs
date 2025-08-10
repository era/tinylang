use thiserror::Error;

#[derive(Debug, Error, PartialEq)]
pub enum TinyLangError {
    #[error("parser error {0:?}")]
    ParserError(#[from] ParseError),
    #[error("runtime error {0:?}")]
    RuntimeError(#[from] RuntimeError),
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
    #[error("Identifier is Nil")]
    IdentifierIsNil,
    #[error("conditions on a if must result in a bool")]
    ExpectingBool,
}

#[derive(Debug, Error, PartialEq)]
pub enum ParseError {
    #[error("Error while parsing code {0:?}")]
    Generic(String),
    #[error("Invalid node type {0:?}")]
    InvalidNode(String),
    #[error("There is no matching if for the else")]
    NoMatchingIf,
    #[error("There is no matching end for the loop")]
    NoMatchingFor,
}
