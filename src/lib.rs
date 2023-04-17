pub mod errors;
pub mod types;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use once_cell::sync::Lazy;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use std::collections::HashMap;

use crate::errors::{ParseError, RuntimeError, TinyLangError};
use crate::types::TinyLangTypes;

#[derive(Parser)]
#[grammar = "../grammar/template_lang.pest"]
struct TemplateLangParser;

type State = HashMap<String, TinyLangTypes>;

// explanation on Pratt parsers in Rust:
// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
static PRATT_PARSER_MATH: Lazy<PrattParser<Rule>> = Lazy::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::prefix(Rule::neg))
});

pub fn eval(input: &str, state: State) -> Result<String, TinyLangError> {
    let pairs = parse(input)?.next().unwrap().into_inner();

    let mut output = String::new();

    for pair in pairs {
        let current_output = match pair.as_rule() {
            Rule::html => pair.as_str().to_string(),
            Rule::print => visit_print(pair.into_inner(), &state)?,
            Rule::invalid => format!("Invalid exp: {}", pair.as_span().as_str()),
            _ => todo!(),
        };
        output.push_str(&current_output);
    }

    Ok(output)
}

fn parse(input: &str) -> Result<Pairs<Rule>, ParseError> {
    // this is G which contains children which are
    // either html chars, print statements or dynamic statements
    TemplateLangParser::parse(Rule::g, input).map_err(|e| ParseError::Generic(e.to_string()))
}

fn visit_print(node: Pairs<Rule>, state: &State) -> Result<String, TinyLangError> {
    let mut result = String::new();
    for child in node {
        let child_result = match child.as_rule() {
            Rule::exp => visit_exp(child.into_inner(), state)?,
            _ => {
                return Err(ParseError::InvalidNode(
                    format!("print statement does not accept {:?}", child),
                )
                    .into())
            }
        };
        result.push_str(&child_result.to_string());
    }
    Ok(result)
}

fn visit_exp(node: Pairs<Rule>, state: &State) -> Result<TinyLangTypes, TinyLangError> {
    let first_child = node.into_iter().next().unwrap();
    match first_child.as_rule() {
        Rule::literal => visit_literal(first_child.into_inner()),
        Rule::math => Ok(TinyLangTypes::Numeric(
            visit_math(first_child.into_inner(), state)?,
        )),
        Rule::identifier => visit_identifier(first_child, state),
        _ => Err(ParseError::InvalidNode(format!(
            "visit_exp was called with an invalid node {:?}",
            first_child
        ))
            .into()),
    }
}

fn visit_identifier(node: Pair<Rule>, state: &State) -> Result<TinyLangTypes, TinyLangError> {
    let key = node.as_span().as_str();
    match state.get(key) {
        Some(value) => Ok(value.to_owned()),
        None => Err(RuntimeError::VariableNotDefined(format!("{key} is not defined")).into()),
    }
}

fn visit_math(pairs: Pairs<Rule>, state: &State) -> Result<f64, TinyLangError> {
    //TODO properly handle errors
    let result = PRATT_PARSER_MATH
        .map_primary(|primary| match primary.as_rule() {
            Rule::integer | Rule::float => primary.as_str().parse().unwrap(),
            Rule::identifier => visit_identifier(primary, state)
                .unwrap()
                .try_into() // this can fail when the variable is actually a bool or a string
                .unwrap(),
            Rule::math => visit_math(primary.into_inner(), state).unwrap(), // from "(" ~ math ~ ")"
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::neg => -rhs,
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| match op.as_rule() {
            Rule::add => lhs + rhs,
            Rule::sub => lhs - rhs,
            Rule::mul => lhs * rhs,
            Rule::div => lhs / rhs,
            _ => unreachable!(),
        })
        .parse(pairs);

    Ok(result)
}

fn visit_literal(node: Pairs<Rule>) -> Result<TinyLangTypes, TinyLangError> {
    let child = match node.into_iter().next() {
        Some(child) => child,
        None => {
            return Err(ParseError::InvalidNode(
                "visit_lang_types was called with an empty node".to_string()
            )
                .into())
        }
    };
    //TODO handle errors
    match child.as_rule() {
        Rule::integer | Rule::float => Ok(TinyLangTypes::Numeric(child.as_str().parse().unwrap())),
        Rule::bool => Ok(TinyLangTypes::Bool(child.as_str().parse().unwrap())),
        Rule::string => {
            // we need to remove the ' from start and end of the string
            let mut string = child.as_str().to_string();
            string.remove(0);
            string.remove(string.len() - 1);
            Ok(string.into())
        }
        _ => Err(ParseError::InvalidNode(format!(
            "visit_lang_types was called with an invalid node {:?}",
            child
        ))
            .into()),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_basic_html() {
        let result = eval("<html>", HashMap::default()).unwrap();
        assert_eq!("<html>", result.as_str());
    }

    #[test]
    fn test_empty_string() {
        let result = eval("", HashMap::default()).unwrap();
        assert_eq!("", result.as_str());
    }
    #[test]
    fn test_html_with_spaces() {
        let result = eval("something nice here", HashMap::default()).unwrap();
        assert_eq!("something nice here", result.as_str());
    }
    #[test]
    fn test_literal_print_stmt_with_html() {
        let result = eval("the coolest number is {{ 12 }}", HashMap::default()).unwrap();
        assert_eq!("the coolest number is 12", result.as_str());
    }

    #[test]
    fn test_literal_print_stmt() {
        let result = eval("{{ 12 }}", HashMap::default()).unwrap();
        assert_eq!("12", result.as_str());
    }

    #[test]
    fn test_literal_float_print_stmt() {
        let result = eval("{{ 12.4 }}", HashMap::default()).unwrap();
        assert_eq!("12.4", result.as_str());
    }

    #[test]
    fn test_literal_float_math_print_stmt() {
        let result = eval("{{ 12.4 + 4.6 }}", HashMap::default()).unwrap();
        assert_eq!("17", result.as_str());
    }

    #[test]
    fn test_math_add_print_stmt() {
        let result = eval("{{ 12 + 1 }}", HashMap::default()).unwrap();
        assert_eq!("13", result.as_str());
    }
    #[test]
    fn test_math_neg_print_stmt() {
        let result = eval("{{ -12 + 1 }}", HashMap::default()).unwrap();
        assert_eq!("-11", result.as_str());
    }

    #[test]
    fn test_math_mul_print_stmt() {
        let result = eval("{{ 12 * 2 }}", HashMap::default()).unwrap();
        assert_eq!("24", result.as_str())
    }

    #[test]
    fn test_right_assoc_math_print_stmt() {
        let result = eval("{{ 1 + 12 / 2 - 2 }}", HashMap::default()).unwrap();
        assert_eq!("5", result.as_str())
    }

    #[test]
    fn test_identifier_print_stmt() {
        let result = eval(
            "{{ a }}",
            HashMap::from([("a".into(), TinyLangTypes::Numeric(5 as f64))]),
        )
            .unwrap();
        assert_eq!("5", result.as_str())
    }

    #[test]
    fn test_identifier_math_print_stmt() {
        let result = eval(
            "{{ a * 2 + a}}",
            HashMap::from([("a".into(), TinyLangTypes::Numeric(5 as f64))]),
        )
            .unwrap();
        assert_eq!("15", result.as_str())
    }

    #[test]
    fn test_bool_print_stmt() {
        let result = eval("{{ true }}", HashMap::default()).unwrap();
        assert_eq!("true", result.as_str())
    }

    #[test]
    fn test_string_print_stmt() {
        let result = eval("{{ 'something' }}", HashMap::default()).unwrap();
        assert_eq!("something", result.as_str())
    }

    #[test]
    fn test_invalid_stmt() {
        let result = eval("abc {{ 1 2 3 }} {{1}}", HashMap::default()).unwrap();
        assert_eq!("abc Invalid exp: {{ 1 2 3 }} {{1}}", result.as_str())
    }
}