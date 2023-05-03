use crate::errors::RuntimeError;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::sync::Arc;

pub type FuncArguments = Vec<TinyLangType>;
pub type Function = fn(FuncArguments, &HashMap<String, TinyLangType>) -> TinyLangType;
pub type State = HashMap<String, TinyLangType>;

/// Represents the types supported by the template
#[derive(Clone)]
pub enum TinyLangType {
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
    Function(Arc<Function>),
    /// Represents a Vector to be iterated using the for loop
    /// it cannot be created inside the template file.
    Vec(Arc<Vec<TinyLangType>>),
    /// Each instance of an object has their own "vtable"
    Object(State),
    Nil,
}

impl PartialEq for TinyLangType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TinyLangType::String(a), TinyLangType::String(b)) => a == b,
            (TinyLangType::Numeric(a), TinyLangType::Numeric(b)) => a == b,
            (TinyLangType::Bool(a), TinyLangType::Bool(b)) => a == b,
            (TinyLangType::Vec(a), TinyLangType::Vec(b)) => a == b,
            (TinyLangType::Object(a), TinyLangType::Object(b)) => a == b,
            // Ignore the Function variant in the comparison
            (TinyLangType::Nil, TinyLangType::Nil) => true,
            _ => false,
        }
    }
}

impl PartialOrd for TinyLangType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (TinyLangType::String(a), TinyLangType::String(b)) => a.partial_cmp(b),
            (TinyLangType::Numeric(a), TinyLangType::Numeric(b)) => a.partial_cmp(b),
            (TinyLangType::Bool(a), TinyLangType::Bool(b)) => a.partial_cmp(b),
            // Ignore the Function and Object variant in the comparison
            (TinyLangType::Nil, TinyLangType::Nil) => Some(Ordering::Equal),
            _ => None,
        }
    }
}

macro_rules! math_operation {
    ($x:ident, $y:ident, $op:tt) => {{
        let lhs: f64 = $x.try_into()?;
        let rhs: f64 = $y.try_into()?;
        Ok(TinyLangType::Numeric(lhs $op rhs))
    }};
}

impl Div for TinyLangType {
    type Output = Result<TinyLangType, RuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        math_operation!(self, rhs, /)
    }
}

impl Mul for TinyLangType {
    type Output = Result<TinyLangType, RuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        math_operation!(self, rhs, *)
    }
}

impl Add for TinyLangType {
    type Output = Result<TinyLangType, RuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        math_operation!(self, rhs, +)
    }
}

impl Sub for TinyLangType {
    type Output = Result<TinyLangType, RuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        math_operation!(self, rhs, -)
    }
}

impl Neg for TinyLangType {
    type Output = Result<TinyLangType, RuntimeError>;

    fn neg(self) -> Self::Output {
        let lhs: f64 = self.try_into()?;
        Ok(TinyLangType::Numeric(-lhs))
    }
}

impl Display for TinyLangType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TinyLangType::Numeric(e) => write!(f, "{}", e),
            TinyLangType::String(e) => write!(f, "{}", e),
            TinyLangType::Bool(e) => write!(f, "{}", e),
            TinyLangType::Nil => write!(f, "Nil"),
            TinyLangType::Function(_) => write!(f, "Function"),
            TinyLangType::Object(_) => write!(f, "Object"),
            TinyLangType::Vec(v) => write!(f, "Vector with {} elements", v.len()),
        }
    }
}

impl TryInto<f64> for TinyLangType {
    type Error = RuntimeError;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            TinyLangType::Numeric(f) => Ok(f),
            _ => Err(RuntimeError::InvalidLangType),
        }
    }
}

impl From<TinyLangType> for String {
    fn from(val: TinyLangType) -> Self {
        val.to_string()
    }
}

impl From<String> for TinyLangType {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for TinyLangType {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

impl From<i32> for TinyLangType {
    fn from(value: i32) -> Self {
        Self::Numeric(value.into())
    }
}

impl From<f64> for TinyLangType {
    fn from(value: f64) -> Self {
        Self::Numeric(value)
    }
}

impl From<f32> for TinyLangType {
    fn from(value: f32) -> Self {
        Self::Numeric(value.into())
    }
}

impl From<bool> for TinyLangType {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl From<State> for TinyLangType {
    fn from(value: State) -> Self {
        Self::Object(value)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::sync::Mutex;
    use std::thread;

    #[test]
    fn test_if_state_is_send_sync() {
        let state = Arc::new(Mutex::new(State::new()));
        thread::spawn(move || state);
    }
}
