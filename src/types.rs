use crate::errors::RuntimeError;
use std::fmt;
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Sub};

#[derive(Clone, PartialEq)]
pub enum TinyLangTypes {
    String(String),
    Numeric(f64),
    Bool(bool),
    Nil,
}

impl Div for TinyLangTypes {
    type Output = Result<TinyLangTypes, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        let lhs: f64 = self.try_into()?;
        let rhs: f64 = rhs.try_into()?;
        Ok(TinyLangTypes::Numeric(lhs / rhs))
    }
}

impl Mul for TinyLangTypes {
    type Output = Result<TinyLangTypes, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        let lhs: f64 = self.try_into()?;
        let rhs: f64 = rhs.try_into()?;
        Ok(TinyLangTypes::Numeric(lhs * rhs))
    }
}

impl Add for TinyLangTypes {
    type Output = Result<TinyLangTypes, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        let lhs: f64 = self.try_into()?;
        let rhs: f64 = rhs.try_into()?;
        Ok(TinyLangTypes::Numeric(lhs + rhs))
    }
}

impl Sub for TinyLangTypes {
    type Output = Result<TinyLangTypes, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        let lhs: f64 = self.try_into()?;
        let rhs: f64 = rhs.try_into()?;
        Ok(TinyLangTypes::Numeric(lhs - rhs))
    }
}

impl Neg for TinyLangTypes {
    type Output = Result<TinyLangTypes, RuntimeError>;

    fn neg(self) -> Self::Output {
        let lhs: f64 = self.try_into()?;
        Ok(TinyLangTypes::Numeric(-lhs))
    }
}

impl Display for TinyLangTypes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TinyLangTypes::Numeric(e) => write!(f, "{}", e),
            TinyLangTypes::String(e) => write!(f, "{}", e),
            TinyLangTypes::Bool(e) => write!(f, "{}", e),
            TinyLangTypes::Nil => write!(f, "Nil"),
        }
    }
}

impl TryInto<f64> for TinyLangTypes {
    type Error = RuntimeError;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            TinyLangTypes::String(_) => Err(RuntimeError::InvalidLangType),
            TinyLangTypes::Bool(_) => Err(RuntimeError::InvalidLangType),
            TinyLangTypes::Numeric(f) => Ok(f),
            TinyLangTypes::Nil => Err(RuntimeError::InvalidLangType),
        }
    }
}

impl From<TinyLangTypes> for String {
    fn from(val: TinyLangTypes) -> Self {
        val.to_string()
    }
}

impl From<String> for TinyLangTypes {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for TinyLangTypes {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

impl From<i32> for TinyLangTypes {
    fn from(value: i32) -> Self {
        Self::Numeric(value.into())
    }
}

impl From<f64> for TinyLangTypes {
    fn from(value: f64) -> Self {
        Self::Numeric(value)
    }
}

impl From<f32> for TinyLangTypes {
    fn from(value: f32) -> Self {
        Self::Numeric(value.into())
    }
}

impl From<bool> for TinyLangTypes {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}
