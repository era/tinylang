use crate::parser::eval;
use crate::types::TinyLangType;
use gloo_utils::format::JsValueSerdeExt;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
#[derive(Serialize, Deserialize, Copy, Clone)]
pub enum InternalType {
    Numeric = 0,
    String = 1,
    Vec = 2,
}

#[derive(Serialize, Deserialize)]
pub struct JsType {
    pub name: String,
    pub value: String,
    pub internal_type: InternalType,
}

fn parse_js(internal_type: InternalType, val: String) -> Result<TinyLangType, &'static str> {
    match internal_type {
        InternalType::Numeric => Ok(TinyLangType::Numeric(
            val.parse::<f64>().map_err(|_| "Invalid Number")?,
        )),
        InternalType::String => Ok(TinyLangType::String(val)),
        InternalType::Vec => {
            let mut tinylang_vector = Vec::new();
            let internal_vector: Vec<JsType> =
                serde_json::from_str(&val).map_err(|_| "Invalid Vector")?;
            for internal_element in internal_vector {
                tinylang_vector.push(parse_js(
                    internal_element.internal_type,
                    internal_element.value,
                )?);
            }
            Ok(TinyLangType::Vec(tinylang_vector))
        }
    }
}

#[wasm_bindgen]
pub fn eval_wasm(input: &str, state: JsValue) -> String {
    let js_state: Vec<JsType> = match state
        .into_serde()
        .map_err(|e| format!("could not transform the state into TinyLang object {:?}", e))
    {
        Ok(v) => v,
        Err(e) => return e.to_string(),
    };

    let mut state = HashMap::new();

    for var in js_state {
        match parse_js(var.internal_type, var.value) {
            Ok(v) => state.insert(var.name, v),
            Err(e) => return e.to_string(),
        };
    }

    match eval(input, state) {
        Ok(s) => s,
        Err(e) => e.to_string(),
    }
}
