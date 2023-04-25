use crate::errors::RuntimeError;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::sync::Arc;

pub type FuncArguments = Vec<TinyLangTypes>;
pub type Function = dyn Fn(FuncArguments, &HashMap<String, TinyLangTypes>) -> TinyLangTypes;
pub type State = HashMap<String, TinyLangTypes>;

/// Represents the types supported by the template
#[derive(Clone)]
pub enum TinyLangTypes {
    String(String),
    /// any integer or float is represented as a f64
    Numeric(f64),
    Bool(bool),
    /// Any function will receive the parameters as an array
    /// so sum(1, 2, 3, 4) will be transformed as sum(vec![1, 2, 3, 4])
    /// all functions must return a TinyLangTypes, if yours does not return
    /// anything use the TinyLangTypes::Nil type.
    /// Users cannot declare functions inside the template file.
    /// The function also receives the global state
    Function(Arc<Box<Function>>),
    /// Represents a Vector to be iterated using the for loop
    /// it cannot be created inside the template file.
    Vec(Arc<Vec<TinyLangTypes>>),
    Nil,
}

impl PartialEq for TinyLangTypes {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TinyLangTypes::String(a), TinyLangTypes::String(b)) => a == b,
            (TinyLangTypes::Numeric(a), TinyLangTypes::Numeric(b)) => a == b,
            (TinyLangTypes::Bool(a), TinyLangTypes::Bool(b)) => a == b,
            (TinyLangTypes::Vec(a), TinyLangTypes::Vec(b)) => a == b,
            // Ignore the Function variant in the comparison
            (TinyLangTypes::Nil, TinyLangTypes::Nil) => true,
            _ => false,
        }
    }
}

impl PartialOrd for TinyLangTypes {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (TinyLangTypes::String(a), TinyLangTypes::String(b)) => a.partial_cmp(b),
            (TinyLangTypes::Numeric(a), TinyLangTypes::Numeric(b)) => a.partial_cmp(b),
            (TinyLangTypes::Bool(a), TinyLangTypes::Bool(b)) => a.partial_cmp(b),
            // Ignore the Function variant in the comparison
            (TinyLangTypes::Nil, TinyLangTypes::Nil) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

macro_rules! math_operation {
    ($x:ident, $y:ident, $op:tt) => {{
        let lhs: f64 = $x.try_into()?;
        let rhs: f64 = $y.try_into()?;
        Ok(TinyLangTypes::Numeric(lhs $op rhs))
    }};
}

impl Div for TinyLangTypes {
    type Output = Result<TinyLangTypes, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        math_operation!(self, rhs, /)
    }
}

impl Mul for TinyLangTypes {
    type Output = Result<TinyLangTypes, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        math_operation!(self, rhs, *)
    }
}

impl Add for TinyLangTypes {
    type Output = Result<TinyLangTypes, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        math_operation!(self, rhs, +)
    }
}

impl Sub for TinyLangTypes {
    type Output = Result<TinyLangTypes, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        math_operation!(self, rhs, -)
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
            TinyLangTypes::Function(_) => write!(f, "Function"),
            TinyLangTypes::Vec(v) => write!(f, "Vector with {}", v.len()),
        }
    }
}

impl TryInto<f64> for TinyLangTypes {
    type Error = RuntimeError;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            TinyLangTypes::Numeric(f) => Ok(f),
            _ => Err(RuntimeError::InvalidLangType),
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
