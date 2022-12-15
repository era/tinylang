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

    for pair in pairs {
        let current_output = match pair.as_rule() {
            //TODO allocating a string everytime (refact)
            Rule::html => pair.as_str().into(),
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
            Rule::exp => child.as_str(),
            _ => {
                return Err(ParseError::InvalidNode(
                    format!("print statement does not accept {:?}", child).to_owned(),
                ))
            }
        };
        result.push_str(child_result);
    }
    Ok(result)
}

pub fn parse(input: &str) -> Result<Pairs<Rule>, ParseError> {
    // this is G which contains children which are
    // either html chars, print statments or dynamic statements
    TemplateLangParser::parse(Rule::g, input).map_err(|e| {
        println!("{:?}", e);
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
