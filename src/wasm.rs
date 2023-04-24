use std::collections::HashMap;
use wasm_bindgen::intern;
use gloo_utils::format::JsValueSerdeExt;
use serde::{Serialize, Deserialize};
use wasm_bindgen::prelude::*;
use std::sync::Arc;
use crate::eval;
use crate::types::TinyLangTypes;


#[wasm_bindgen]
#[derive(Serialize, Deserialize, Copy, Clone)]
pub enum InternalType {
    Numeric = 0,
    String = 1,
    Vec = 2
}

#[derive(Serialize, Deserialize)]
pub struct JsType {
    pub value: String,
    pub internal_type: InternalType
}

fn parse_js(val: JsType) -> Result<TinyLangTypes, &'static str> {
    match val.internal_type {
        InternalType::Numeric => Ok(TinyLangTypes::Numeric(val.value.parse::<f64>().map_err(|_| "Invalid Number")?)),
        InternalType::String => Ok(TinyLangTypes::String(val.value)),
        InternalType::Vec => {
            let mut tinylang_vector = Vec::new();
            let internal_vector: Vec<JsType> = serde_json::from_str(&val.value).map_err(|_| "Invalid Vector")?;
            for internal_element in internal_vector {
                tinylang_vector.push(parse_js(internal_element)?);
            }
            Ok(TinyLangTypes::Vec(Arc::new(tinylang_vector)))
        },
    }

}

#[wasm_bindgen]
pub fn eval_wasm(input: &str, state: JsValue) -> String {
    let js_state: Vec<(String, JsType)> = state.into_serde().unwrap();
    let mut state = HashMap::new();

    for (var_name, var_value) in js_state {
        match parse_js(var_value) {
            Ok(v) => state.insert(var_name, v),
            Err(e) => return e.to_string(),
        };
    }

    match eval(input, state) {
        Ok(s) => s,
        Err(e) => e.to_string(),
    }
}