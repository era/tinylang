use once_cell::sync::Lazy;
use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc, Op, PrattParser};
use pest::Parser;
use std::borrow::Cow;
use std::sync::Arc;
use std::vec::IntoIter;

use crate::errors::{ParseError, RuntimeError, TinyLangError};
use crate::types::{State, TinyLangType};

#[derive(Parser)]
#[grammar = "../grammar/template_lang.pest"]
struct TemplateLangParser;

const EMPTY_STRING_COW: Cow<str> = Cow::Borrowed("");

// explanation on Pratt parsers in Rust:
// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
static PRATT_PARSER_OP_EXP: Lazy<PrattParser<Rule>> = Lazy::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::op_and, Assoc::Left) | Op::infix(Rule::op_or, Assoc::Left))
        .op(Op::infix(Rule::op_eq, Assoc::Left)
            | Op::infix(Rule::op_neq, Assoc::Left)
            | Op::infix(Rule::op_eg, Assoc::Left)
            | Op::infix(Rule::op_g, Assoc::Left)
            | Op::infix(Rule::op_el, Assoc::Left)
            | Op::infix(Rule::op_l, Assoc::Left))
        .op(Op::infix(Rule::add, Assoc::Left) | Op::infix(Rule::sub, Assoc::Left))
        .op(Op::infix(Rule::mul, Assoc::Left) | Op::infix(Rule::div, Assoc::Left))
        .op(Op::prefix(Rule::neg))
});

/// Since we parse and execute the flow control at same time
/// we need to know if we are just parsing static data or
/// if we are in a loop where we may need to replay later
enum ParseState<'a> {
    Static(Cow<'a, str>),
    Dynamic(Loop<'a>),
}
/// Runtime of our language
struct Runtime<'a> {
    /// Everytime we encounter an if or a for loop we add a boolean here
    /// if we should output the content or not
    /// when we encounter an `end` we `pop` one element
    should_output: Vec<bool>,
    /// Everytime we encounter a flow control that requires an end we add it here
    /// so we know if the user input is invalid
    needs_end: Vec<Rule>,
    /// we add the loops that are active here, so we can replay later
    loops: Vec<Loop<'a>>,
}

impl Runtime<'_> {
    pub fn new() -> Self {
        Self {
            should_output: Vec::new(),
            needs_end: Vec::new(),
            loops: Vec::new(),
        }
    }

    pub fn is_output_enabled(&self) -> bool {
        (!self.should_output.is_empty() && *(self.should_output.last().unwrap()))
            || self.should_output.is_empty()
    }
}

struct Loop<'a> {
    variable_name: String,
    vector: IntoIter<TinyLangType>,
    pairs: Vec<Pair<'a, Rule>>,
    old_state_for_var: TinyLangType,
}

impl Loop<'_> {
    fn new(
        variable_name: String,
        original_value: Vec<TinyLangType>,
        old_state_for_var: TinyLangType,
    ) -> Self {
        Self {
            vector: original_value.into_iter(),
            pairs: Vec::new(),
            old_state_for_var,
            variable_name,
        }
    }

    fn still_valid(&self) -> bool {
        self.vector.len() > 0
    }

    fn next(&mut self) -> Option<TinyLangType> {
        self.vector.next()
    }

    fn replay_loop<'a>(mut self, state: &mut State) -> Result<Cow<'a, str>, TinyLangError> {
        let mut output = String::new();
        self.pairs.remove(0);
        let mut runtime = Runtime::new();

        for item in self.vector {
            state.insert(self.variable_name.to_string(), item);

            for pair in &self.pairs {
                output.push_str(&process_pair(pair.clone(), state, &mut runtime)?);
            }
        }
        state.insert(self.variable_name.to_string(), self.old_state_for_var);
        Ok(output.into())
    }
}

/// eval receives the &str to parse and the State.
/// You should put in the State any function or variable that your templates
/// should have access to.
/// Important to know that there is only one global scope. For loops may overwrite
/// variables outside of it.
/// For now there is no construct to assign new values to variables besides for loops.
///
/// Any char will be in the Result unless it's a "dynamic" or a "print".
/// You can use `{{ 1 }}` to print any expression. Or you can use
/// `{% if expression %} {% else %} {% end %}` to control the template display.
/// You can use `{% for item in array %} {% end %}`.
///
/// Check `crate::types::TinyLangTypes` for details of all the types build-in the language.
pub fn eval(input: &str, mut state: State) -> Result<String, TinyLangError> {
    let pairs = parse(input)?.next().unwrap().into_inner();
    let mut output = String::new();

    let mut runtime = Runtime::new();

    for pair in pairs {
        output.push_str(&process_pair(pair, &mut state, &mut runtime)?);
    }

    Ok(output)
}

fn process_pair<'a>(
    pair: Pair<'a, Rule>,
    state: &mut State,
    runtime: &mut Runtime<'a>,
) -> Result<Cow<'a, str>, TinyLangError> {
    let cloned_pair = pair.clone();

    let current_output = visit_generic(pair, state, runtime)?;
    let current_output = match current_output {
        ParseState::Static(s) => s,
        ParseState::Dynamic(l) => {
            if !runtime.loops.is_empty() {
                runtime
                    .loops
                    .last_mut()
                    .unwrap()
                    .pairs
                    .extend(l.pairs.clone());
            }
            if runtime.is_output_enabled() {
                l.replay_loop(state)?
            } else {
                EMPTY_STRING_COW
            }
        }
    };
    if !runtime.loops.is_empty() {
        // we must put evey thing we cloned on the current loop
        // on the previous one, since we may need to replay it again
        runtime.loops.last_mut().unwrap().pairs.push(cloned_pair);
    }
    if runtime.is_output_enabled() {
        Ok(current_output)
    } else {
        Ok(EMPTY_STRING_COW)
    }
}

fn visit_generic<'a>(
    pair: Pair<'a, Rule>,
    state: &mut State,
    runtime: &mut Runtime<'a>,
) -> Result<ParseState<'a>, TinyLangError> {
    // if we are currently not outputting the result
    // we don't need to visit the Rule::html or Rule::print
    // since we would just throw the result away
    let current_output = match (pair.as_rule(), runtime.is_output_enabled()) {
        (Rule::html, true) => pair.as_str().into(),
        (Rule::print, true) => visit_print(pair.into_inner(), state)?.into(),
        (Rule::dynamic, _) => {
            let result = visit_dynamic(pair.into_inner(), state, runtime)?;
            match result {
                Some(l) => return Ok(ParseState::Dynamic(l)),
                None => EMPTY_STRING_COW,
            }
        }
        (Rule::invalid, _) => {
            return Err(TinyLangError::ParserError(ParseError::InvalidNode(
                format!("Invalid exp: {}", pair.as_span().as_str()),
            )))
        }
        (_, false) => EMPTY_STRING_COW,
        _ => unreachable!(),
    };
    Ok(ParseState::Static(current_output))
}

fn parse(input: &str) -> Result<Pairs<Rule>, ParseError> {
    // this is G which contains children which are
    // either html chars, print statements or dynamic statements
    TemplateLangParser::parse(Rule::g, input).map_err(|e| ParseError::Generic(e.to_string()))
}

fn visit_dynamic<'a>(
    mut dynamic: Pairs<'a, Rule>,
    state: &mut State,
    runtime: &mut Runtime<'a>,
) -> Result<Option<Loop<'a>>, TinyLangError> {
    if let Some(dynamic) = dynamic.next() {
        // if we are not outputting we don't need to process anything for the runtime
        // but we should still visit flow_* rules since we need to check if they have
        // their corresponding {%end%}
        match (dynamic.as_rule(), runtime.is_output_enabled()) {
            (Rule::exp, true) => {
                let _ = visit_exp(dynamic.into_inner(), state)?;
            }
            (Rule::flow_if, true) => {
                runtime.needs_end.push(Rule::flow_if);
                runtime.should_output.push(visit_if(dynamic, state)?);
            }
            (Rule::flow_if, false) => {
                runtime.needs_end.push(Rule::flow_if);
                runtime.should_output.push(false);
            }
            (Rule::flow_else, _) => {
                //handle case where there is a else without if
                let last_if = match runtime.should_output.pop() {
                    Some(b) => b,
                    None => return Err(TinyLangError::ParserError(ParseError::NoMatchingIf)),
                };
                runtime.should_output.push(!last_if);
            }
            (Rule::flow_end, _) => {
                let rule = match runtime.needs_end.pop() {
                    Some(b) => b,
                    None => return Err(TinyLangError::ParserError(ParseError::NoMatchingFor)),
                };
                if rule == Rule::flow_if {
                    runtime.should_output.pop();
                } else {
                    runtime.should_output.pop();
                    let loop_struct = runtime.loops.pop();
                    return Ok(loop_struct);
                }
            }
            (Rule::flow_for, true) => {
                let for_loop = visit_for(dynamic.into_inner(), state, true)?;
                runtime.needs_end.push(Rule::flow_for);
                runtime.should_output.push(for_loop.still_valid());
                runtime.loops.push(for_loop);
            }
            (Rule::flow_for, false) => {
                // if we are not outputting the value, it means we are inside a if false { HERE }
                // if that is the case, we should not halt the execution due to errors on the type of the loop
                // since it's likely the user did something like if myvar != Nil { for a in myvar {} }
                let for_loop = visit_for(dynamic.into_inner(), state, false)?;
                runtime.needs_end.push(Rule::flow_for);
                // we should keep not outputting
                runtime.should_output.push(false);
                runtime.loops.push(for_loop);
            }
            (_, false) => (),
            _ => {
                return Err(ParseError::InvalidNode(format!(
                    "dynamic statement does not accept {:?}",
                    dynamic
                ))
                .into())
            }
        };
    }

    Ok(None)
}

fn visit_for<'a>(
    mut node: Pairs<'a, Rule>,
    state: &mut State,
    ignore_error: bool,
) -> Result<Loop<'a>, TinyLangError> {
    let identifier_node = node.next().unwrap();
    let identifier_name = identifier_node.as_span().as_str();
    let identifier = visit_identifier(identifier_node, state)?;
    let original_value = visit_exp(node.next().unwrap().into_inner(), state)?;
    // we should ignore errors if we are not outputting anything
    // because that means we should not execute dynamic statements
    let original_value: Arc<Vec<TinyLangType>> = match (original_value, ignore_error) {
        (TinyLangType::Vec(v), _) => v,
        (_, false) => return Err(TinyLangError::RuntimeError(RuntimeError::InvalidLangType)),
        (_, true) => Arc::new(Vec::new()),
    };

    let mut loop_struct = Loop::new(
        identifier_name.into(),
        (*original_value).clone(),
        identifier,
    );

    state.insert(
        identifier_name.to_string(),
        loop_struct.next().unwrap_or(TinyLangType::Nil),
    );

    Ok(loop_struct)
}

fn visit_if(node_if: Pair<Rule>, state: &mut State) -> Result<bool, TinyLangError> {
    let condition = match visit_exp(node_if.into_inner(), state)? {
        TinyLangType::Bool(b) => b,
        _ => return Err(TinyLangError::RuntimeError(RuntimeError::ExpectingBool)),
    };

    Ok(condition)
}

fn visit_print(mut node: Pairs<Rule>, state: &mut State) -> Result<String, TinyLangError> {
    let child = node.next().unwrap();
    let print_value = match child.as_rule() {
        Rule::exp => visit_exp(child.into_inner(), state)?,
        _ => {
            return Err(ParseError::InvalidNode(format!(
                "print statement does not accept {:?}",
                child
            ))
            .into())
        }
    };

    Ok(print_value.to_string())
}

fn visit_exp(node: Pairs<Rule>, state: &mut State) -> Result<TinyLangType, TinyLangError> {
    let first_child = node.into_iter().next().unwrap();
    match first_child.as_rule() {
        Rule::dot => visit_dot(first_child.into_inner(), state),
        Rule::literal => visit_literal(first_child.into_inner()),
        Rule::op_exp => visit_op_exp(first_child.into_inner(), state),
        Rule::identifier => visit_identifier(first_child, state),
        Rule::function_call => visit_function_call(first_child.into_inner(), state),
        Rule::exp => visit_exp(first_child.into_inner(), state),
        _ => Err(ParseError::InvalidNode(format!(
            "visit_exp was called with an invalid node {:?}",
            first_child
        ))
        .into()),
    }
}

fn visit_function_call(
    mut nodes: Pairs<Rule>,
    state: &mut State,
) -> Result<TinyLangType, TinyLangError> {
    let function = visit_identifier(nodes.next().unwrap(), state)?;
    let mut params = Vec::new();

    for node in nodes {
        let param = visit_exp(node.into_inner(), state)?;
        params.push(param);
    }

    match function {
        TinyLangType::Function(f) => Ok(f(params, state)),
        TinyLangType::Nil => Err(TinyLangError::RuntimeError(RuntimeError::IdentifierIsNil)),
        _ => Err(TinyLangError::RuntimeError(RuntimeError::InvalidLangType)),
    }
}

fn visit_identifier(node: Pair<Rule>, state: &mut State) -> Result<TinyLangType, TinyLangError> {
    let key = node.as_span().as_str();
    match state.get(key) {
        Some(value) => Ok(value.clone()),
        None => Ok(TinyLangType::Nil),
    }
}
fn visit_dot(mut pairs: Pairs<Rule>, state: &mut State) -> Result<TinyLangType, TinyLangError> {
    let mut object = visit_identifier(pairs.next().unwrap(), state)?;
    for p in pairs {
        object = match object {
            TinyLangType::Object(ref mut s) => visit_identifier(p, s)?,
            _ => return Err(TinyLangError::RuntimeError(RuntimeError::InvalidLangType)),
        };
    }
    Ok(object)
}

fn visit_op_exp(pairs: Pairs<Rule>, state: &mut State) -> Result<TinyLangType, TinyLangError> {
    PRATT_PARSER_OP_EXP
        .map_primary(|primary| match primary.as_rule() {
            Rule::dot => visit_dot(primary.into_inner(), state),
            Rule::literal => visit_literal(primary.into_inner()),
            Rule::identifier => visit_identifier(primary, state),
            Rule::op_exp => visit_op_exp(primary.into_inner(), state), // from "(" ~ op_exp ~ ")"
            _ => unreachable!(),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::neg => Ok((-(rhs?))?),
            _ => unreachable!(),
        })
        .map_infix(|lhs, op, rhs| {
            let result = match op.as_rule() {
                Rule::add => lhs? + rhs?,
                Rule::sub => lhs? - rhs?,
                Rule::mul => lhs? * rhs?,
                Rule::div => lhs? / rhs?,
                Rule::op_eq => Ok(TinyLangType::Bool(lhs? == rhs?)),
                Rule::op_neq => Ok(TinyLangType::Bool(lhs? != rhs?)),
                Rule::op_and | Rule::op_or => visit_logical_op(op.as_rule(), lhs?, rhs?),
                Rule::op_eg => Ok(TinyLangType::Bool(lhs? >= rhs?)),
                Rule::op_el => Ok(TinyLangType::Bool(lhs? <= rhs?)),
                Rule::op_g => Ok(TinyLangType::Bool(lhs? > rhs?)),
                Rule::op_l => Ok(TinyLangType::Bool(lhs? < rhs?)),
                _ => unreachable!(),
            };

            result.map_err(TinyLangError::RuntimeError)
        })
        .parse(pairs)
}

fn visit_logical_op(
    op: Rule,
    lhs: TinyLangType,
    rhs: TinyLangType,
) -> Result<TinyLangType, RuntimeError> {
    match (op, lhs, rhs) {
        (Rule::op_and, TinyLangType::Bool(lhs), TinyLangType::Bool(rhs)) => {
            Ok(TinyLangType::Bool(lhs && rhs))
        }
        (Rule::op_or, TinyLangType::Bool(lhs), TinyLangType::Bool(rhs)) => {
            Ok(TinyLangType::Bool(lhs || rhs))
        }
        _ => Err(RuntimeError::InvalidLangType),
    }
}

fn visit_literal(node: Pairs<Rule>) -> Result<TinyLangType, TinyLangError> {
    let child = match node.into_iter().next() {
        Some(child) => child,
        None => {
            return Err(ParseError::InvalidNode(
                "visit_lang_types was called with an empty node".to_string(),
            )
            .into())
        }
    };
    match child.as_rule() {
        // cannot fail given our grammar (well, this can still fail
        // because it could be a huge number, but let's ignore this for
        // now)
        Rule::integer | Rule::float => Ok(TinyLangType::Numeric(child.as_str().parse().unwrap())),
        // cannot fail given our grammar
        Rule::bool => Ok(TinyLangType::Bool(child.as_str().parse().unwrap())),
        Rule::string => {
            // we need to remove the ' from start and end of the string
            let str_val = child.as_str();
            let string = &str_val[1..str_val.len() - 1];
            Ok(string.into())
        }
        Rule::nil => Ok(TinyLangType::Nil),
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
    use std::collections::HashMap;
    use std::sync::Arc;

    #[test]
    fn test_dot_op() {
        let mut a_object = State::new();
        a_object.insert("b".into(), 3.into());
        let result = eval(
            "{{ a.b }}",
            HashMap::from([("a".into(), TinyLangType::Object(a_object))]),
        )
        .unwrap();
        assert_eq!("3", result.as_str());
    }

    #[test]
    fn test_dot_op_math() {
        let mut a_object = State::new();
        a_object.insert("b".into(), 3.into());
        let result = eval(
            "{{ 3 + a.b }}",
            HashMap::from([("a".into(), TinyLangType::Object(a_object))]),
        )
        .unwrap();
        assert_eq!("6", result.as_str());
    }

    #[test]
    fn test_dot_op_multiples() {
        let mut a_object = State::new();
        a_object.insert("b".into(), 3.into());
        let mut b_object = State::new();
        b_object.insert("b".into(), a_object.into());
        let result = eval(
            "{{ a.b.b }}",
            HashMap::from([("a".into(), TinyLangType::Object(b_object))]),
        )
        .unwrap();
        assert_eq!("3", result.as_str());
    }

    #[test]
    fn test_dot_op_math_two_objects() {
        let mut a_object = State::new();
        a_object.insert("b".into(), 3.into());
        let result = eval(
            "{{ a.b + a.b }}",
            HashMap::from([("a".into(), TinyLangType::Object(a_object))]),
        )
        .unwrap();
        assert_eq!("6", result.as_str());
    }

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
            HashMap::from([("a".into(), TinyLangType::Numeric(5_f64))]),
        )
        .unwrap();
        assert_eq!("5", result.as_str())
    }

    #[test]
    fn test_identifier_math_print_stmt() {
        let result = eval(
            "{{ a_a * 2 + a_a}}",
            HashMap::from([("a_a".into(), TinyLangType::Numeric(5_f64))]),
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
        let result = eval("abc {{ 1 2 3 }} {{1}}", HashMap::default());
        assert_eq!(
            Err(TinyLangError::ParserError(ParseError::InvalidNode(
                "Invalid exp: {{ 1 2 3 }} {{1}}".into()
            ))),
            result
        );
    }

    #[test]
    fn test_invalid_math_stmt() {
        let result = eval(
            "{{ a * 2 + a}}",
            HashMap::from([("a".into(), TinyLangType::String("abc".into()))]),
        );
        assert_eq!(
            Err(TinyLangError::RuntimeError(RuntimeError::InvalidLangType)),
            result
        );
    }

    #[test]
    fn test_comp_eq_stmt() {
        let result = eval("{{ 1 == 1 }}", HashMap::default()).unwrap();
        assert_eq!("true", result.as_str())
    }

    #[test]
    fn test_comp_neq_stmt() {
        let result = eval("{{ 1 != 1 }}", HashMap::default()).unwrap();
        assert_eq!("false", result.as_str())
    }

    #[test]
    fn test_comp_eq_str_stmt() {
        let result = eval("{{ 'a' == 'a' }}", HashMap::default()).unwrap();
        assert_eq!("true", result.as_str())
    }

    #[test]
    fn test_comp_eq_str_ident_stmt() {
        let result = eval(
            "{{ 'a' == 'a' }}",
            HashMap::from([("a".into(), TinyLangType::String("abc".into()))]),
        )
        .unwrap();
        assert_eq!("true", result.as_str())
    }

    #[test]
    fn test_comp_neq_and_stmt() {
        let result = eval("{{ 1 != 3 and 1 != 2 }}", HashMap::default()).unwrap();
        assert_eq!("true", result.as_str())
    }

    #[test]
    fn test_comp_neq_or_stmt() {
        let result = eval("{{ 1 != 1 or 1 != 2 }}", HashMap::default()).unwrap();
        assert_eq!("true", result.as_str())
    }

    #[test]
    fn test_undefined_var_stmt() {
        let result = eval("{{ a }}", HashMap::default()).unwrap();
        assert_eq!("Nil", result.as_str())
    }

    #[test]
    fn test_nil_literal_stmt() {
        let result = eval("{{ Nil }}", HashMap::default()).unwrap();
        assert_eq!("Nil", result.as_str())
    }

    #[test]
    fn test_eg_stmt() {
        let result = eval("{{ 1 >= 1 }}", HashMap::default()).unwrap();
        assert_eq!("true", result.as_str())
    }

    #[test]
    fn test_g_stmt() {
        let result = eval("{{ 4 > 1 }}", HashMap::default()).unwrap();
        assert_eq!("true", result.as_str())
    }

    #[test]
    fn test_function_call() {
        // , 2, 3, 3 * 2
        let result = eval(
            "{{ f(1) }}",
            HashMap::from([(
                "f".into(),
                TinyLangType::Function(Arc::new(|args, _state| args.get(0).unwrap().clone())),
            )]),
        )
        .unwrap();
        assert_eq!("1", result.as_str())
    }

    #[test]
    fn test_function_dyn_call() {
        // , 2, 3, 3 * 2
        let result = eval(
            "{% f(1) %}",
            HashMap::from([(
                "f".into(),
                TinyLangType::Function(Arc::new(|args, _state| args.get(0).unwrap().clone())),
            )]),
        )
        .unwrap();
        assert_eq!("", result.as_str())
    }

    #[test]
    fn test_if() {
        let template = r#"
        {% if 1 == 1 %}
        this is true
        {% end %}
        abc
        "#;
        let expected = r#"this is true
        abc
        "#;
        let result = eval(template, HashMap::default()).unwrap();
        assert_eq!(expected, result.as_str())
    }

    #[test]
    fn test_if_else() {
        let template = r#"
        {% if 1 != 1 %}
        this is true
        {%else%}
        over here
        {% end %}
        abc
        "#;
        let expected = r#"over here
        abc
        "#;
        let result = eval(template, HashMap::default()).unwrap();
        assert_eq!(expected, result.as_str())
    }

    #[test]
    fn test_else_without_if() {
        let template = r#"
        {%else%}
        over here
        {% end %}
        abc
        "#;
        let result = eval(template, HashMap::default());
        assert_eq!(
            Err(TinyLangError::ParserError(ParseError::NoMatchingIf)),
            result
        );
    }

    #[test]
    fn test_for() {
        let template = r#"
        {% for a in b %}
        repeat
        {{a}}
        {% end %}
        abc
        "#;
        let expected = r#"repeat
        1repeat
        2repeat
        3abc
        "#;
        let result = eval(
            template,
            HashMap::from([(
                "b".into(),
                TinyLangType::Vec(Arc::new(vec![1.into(), 2.into(), 3.into()])),
            )]),
        )
        .unwrap();
        assert_eq!(expected.replace(' ', ""), result.replace(' ', ""))
    }

    #[test]
    fn test_for_in_a_for() {
        let template = r#"
        {% for a in b %}
        repeat
            {% for a in b %}
1
               {% for a in b %}
               2
               {%end%}
            {%end%}
        {% end %}
        abc
        "#;
        let expected = r#"repeat
            1
            2
            2
            2
            1
            2
            2
            2
            1
            2
            2
            2
        repeat
            1
            2
            2
            2
            1
            2
            2
            2
            1
            2
            2
            2
        repeat
            1
            2
            2
            2
            1
            2
            2
            2
            1
            2
            2
            2
        abc
        "#;
        let result = eval(
            template,
            HashMap::from([(
                "b".into(),
                TinyLangType::Vec(Arc::new(vec![1.into(), 2.into(), 3.into()])),
            )]),
        )
        .unwrap();
        assert_eq!(expected.replace(' ', ""), result.replace(' ', ""))
    }
}
