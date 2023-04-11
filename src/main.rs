extern crate pest;
#[macro_use]
extern crate pest_derive;

use once_cell::sync::Lazy;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;

#[derive(Parser)]
#[grammar = "../grammar/template_lang.pest"]
struct TemplateLangParser;

#[derive(Debug)]
pub enum ParseError {
    Generic(String),
    InvalidNode(String),
}

// explanation on Pratt parsers in Rust:
// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
const PRATT_PARSER_MATH: Lazy<PrattParser<Rule>> = Lazy::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::prefix(Rule::neg))
});

pub fn eval(input: &str) -> Result<String, ParseError> {
    let pairs = parse(input)?.next().unwrap().into_inner();
    let mut output = String::new();

    for pair in pairs {
        let current_output = match pair.as_rule() {
            Rule::html => pair.as_str().to_string(),
            Rule::print => visit_print(pair.into_inner())?,
            _ => todo!(),
        };
        output.push_str(&current_output);
    }

    Ok(output)
}

fn visit_print(node: Pairs<Rule>) -> Result<String, ParseError> {
    let mut result = String::new();
    for child in node {
        let child_result = match child.as_rule() {
            Rule::exp => visit_exp(child.into_inner())?,
            _ => {
                return Err(ParseError::InvalidNode(
                    format!("print statement does not accept {:?}", child).to_owned(),
                ))
            }
        };
        result.push_str(&child_result);
    }
    Ok(result)
}

fn visit_exp(node: Pairs<Rule>) -> Result<String, ParseError> {
    //TODO make it better
    let first_child = node.into_iter().next().unwrap();
    match first_child.as_rule() {
        Rule::lang_types => visit_lang_types(first_child.into_inner()),
        Rule::math => Ok(visit_math(first_child.into_inner())?.to_string()),
        _ => Err(ParseError::InvalidNode(format!(
            "visit_exp was called with an invalid node {:?}",
            first_child
        ))),
    }
}

fn visit_math(pairs: Pairs<Rule>) -> Result<f32, ParseError> {
    let result = PRATT_PARSER_MATH
        .map_primary(|primary| match primary.as_rule() {
            Rule::integer | Rule::float => primary.as_str().parse().unwrap(),
            Rule::math => visit_math(primary.into_inner()).unwrap(), // from "(" ~ math ~ ")"
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

fn visit_lang_types<'a>(node: Pairs<Rule>) -> Result<String, ParseError> {
    for child in node {
        // we know for now that this resolves to a single child
        return match child.as_rule() {
            Rule::integer | Rule::float => Ok(child.as_str().to_string()),
            _ => Err(ParseError::InvalidNode(format!(
                "visit_lang_types was called with an invalid node {:?}",
                child
            ))),
        };
    }
    Err(ParseError::InvalidNode(format!(
        "visit_lang_types was called with an empty node"
    )))
}

pub fn parse(input: &str) -> Result<Pairs<Rule>, ParseError> {
    // this is G which contains children which are
    // either html chars, print statements or dynamic statements
    TemplateLangParser::parse(Rule::g, input).map_err(|e| ParseError::Generic(e.to_string()))
}

fn main() {}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_basic_html() {
        let result = eval("<html>").unwrap();
        assert_eq!("<html>", result.as_str());
    }

    #[test]
    fn test_empty_string() {
        let result = eval("").unwrap();
        assert_eq!("", result.as_str());
    }
    #[test]
    fn test_html_with_spaces() {
        let result = eval("something nice here").unwrap();
        assert_eq!("something nice here", result.as_str());
    }
    #[test]
    fn test_literal_print_stmt_with_html() {
        let result = eval("the coolest number is {{ 12 }}").unwrap();
        assert_eq!("the coolest number is 12", result.as_str());
    }

    #[test]
    fn test_literal_print_stmt() {
        let result = eval("{{ 12 }}").unwrap();
        assert_eq!("12", result.as_str());
    }

    #[test]
    fn test_literal_float_print_stmt() {
        let result = eval("{{ 12.4 }}").unwrap();
        assert_eq!("12.4", result.as_str());
    }

    #[test]
    fn test_literal_float_math_print_stmt() {
        let result = eval("{{ 12.4 + 4 }}").unwrap();
        assert_eq!("16.4", result.as_str());
    }

    #[test]
    fn test_math_add_print_stmt() {
        let result = eval("{{ 12 + 1 }}").unwrap();
        assert_eq!("13", result.as_str());
    }
    #[test]
    fn test_math_neg_print_stmt() {
        let result = eval("{{ -12 + 1 }}").unwrap();
        assert_eq!("-11", result.as_str());
    }

    #[test]
    fn test_math_mul_print_stmt() {
        let result = eval("{{ 12 * 2 }}").unwrap();
        assert_eq!("24", result.as_str())
    }

    #[test]
    fn test_right_assoc_math_print_stmt() {
        let result = eval("{{ 1 + 12 / 2 - 2 }}").unwrap();
        assert_eq!("5", result.as_str())
    }
}
