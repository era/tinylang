use std::collections::HashMap;
use std::sync::Arc;
use tinylang::eval;
use tinylang::types::{FuncArguments, State, TinyLangType};

#[test]
fn print_statement() {
    // anything between {{ }} will be printed
    // numbers
    assert_eq!(eval("{{ 1 + 1 }}", HashMap::default()).unwrap(), "2");
    // strings
    assert_eq!(eval("{{ 'wow' }}", HashMap::default()).unwrap(), "wow");
    // variables
    assert_eq!(
        eval(
            "{{ a }}",
            HashMap::from([("a".into(), "this is a value".into())])
        )
        .unwrap(),
        "this is a value"
    );

    // function calls
    // any function will receive the arguments to it as an Vec<TinyLangType>
    // and it will also receive the state which was passed to the `eval` function
    // this allows the function to be able to invoke eval again. Allowing users
    // to create a `render_partial` function which is not built-in the language.
    assert_eq!(
        eval(
            "{{ a() }}",
            HashMap::from([(
                "a".into(),
                TinyLangType::Function(Arc::new(|_args: FuncArguments, _state: &State| {
                    "this is the value".into()
                }))
            )])
        )
        .unwrap(),
        "this is the value"
    );
}

#[test]
fn dynamic_syntax() {
    // {% %} is mainly used to control what is displayed or not in your final result
    // but you can also call functions (only really useful if they have side-effects)

    let this_is_a_if = r#"{%if false %} will not show this {% else %} will show this{% end %}"#;
    assert_eq!(
        eval(this_is_a_if, HashMap::default()).unwrap(),
        "will show this"
    );

    // you cannot create vectors from withing the template, you need to create it on the RUst side and
    // pass it on State (HashMap<String, TinyLangType>)
    let vec_example = vec![1.into(), 2.into()];

    // the for is useful to iterate over a vector
    let this_is_a_for = r#"{% for n in numbers %}{{ n }},{%end%}"#;
    assert_eq!(
        eval(
            this_is_a_for,
            HashMap::from([("numbers".into(), vec_example.into())])
        )
        .unwrap(),
        "1,2,"
    );
}

#[test]
fn state() {
    // you can pass a State to the eval function
    // which will allow people to use them in the templates
    // the State is just a HashMap<String, TinyLangType>
    // the TinyLangType itself implements the `From` trait which allow you
    // to just create a hashmap and call `into()` for the values
    let _state_example: HashMap<String, TinyLangType> =
        HashMap::from([("a".into(), "this is a value".into())]);
    // you can check the TinyLangType documentation for each existing type
    // but one important to talk is "objects"
    // objects are really just a HashMap<String, TinyLangType>
    // where each key is accessed with the `.` operator, example:
    let my_object_state = HashMap::from([("property".into(), "this is a value".into())]);
    let the_state_for_eval = HashMap::from([("object".into(), my_object_state.into())]);

    assert_eq!(
        eval("{{ object.property }}", the_state_for_eval).unwrap(),
        "this is a value"
    );
}
