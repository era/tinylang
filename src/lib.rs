extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod errors;
mod parser;
pub mod types;
#[cfg(target_family = "wasm")]
mod wasm;

#[cfg(target_family = "wasm")]
pub use wasm::eval_wasm;

pub use parser::eval;
