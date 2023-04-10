extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::{Pair, Pairs};
use pest::Parser;

#[derive(Parser)]
#[grammar = "../grammar/template_lang.pest"]
struct TemplateLangParser;

#[derive(Debug)]
pub enum ParseError {
    Generic(String),
    InvalidNode(String),
}

pub fn eval(input: &str) -> Result<String, ParseError> {
    let pairs = parse(input)?.next().unwrap().into_inner();
    let mut output = String::new();
    // println!("{:?}", pairs);
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
            Rule::exp =>visit_exp(child.into_inner())?,
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
    // println!("{:?}", node);
    //TODO make it better
    let first_child = node.into_iter().next().unwrap();
    match first_child.as_rule() {
        Rule::lang_types => visit_lang_types(first_child.into_inner()),
        // Rule::op_add => visit_op_add(first_child.into_inner()),
        //TODO Rule::op_add =>
        _ => Err(ParseError::InvalidNode(format!("visit_exp was called with an invalid node {:?}", first_child)))
    }
}

fn visit_op_add<'a>(node: Pairs<Rule>) -> Result<String, ParseError> {
    //TODO next live
    // first lhs
    // second operation (two choices)
    // third rhs
    // TODO
    todo!()
}

fn visit_lang_types<'a>(node: Pairs<Rule>) -> Result<String, ParseError> {
    for child in node {
        // we know for now that this resolves to a single child
        return match child.as_rule() {
            Rule::integer => Ok(child.as_str().to_string()),
            _ => Err(ParseError::InvalidNode(format!("visit_lang_types was called with an invalid node {:?}", child)))
        };
    }
    Err(ParseError::InvalidNode(format!("visit_lang_types was called with an empty node")))
}

pub fn parse(input: &str) -> Result<Pairs<Rule>, ParseError> {
    // this is G which contains children which are
    // either html chars, print statments or dynamic statements
    TemplateLangParser::parse(Rule::g, input).map_err(|e| {
        // println!("{:?}", e);
        ParseError::Generic(e.to_string())
    })
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
    fn test_literal_print_stmt() {
        let result = eval("{{ 12 }}").unwrap();
        assert_eq!("12", result.as_str());
    }
}
