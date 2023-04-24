use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::sync::Arc;
use tinylang::eval;
use tinylang::types::TinyLangTypes;

static TEMPLATE: &str = "benches/for.template";
static TEMPLATE_NO_FOR: &str = "benches/no_vars.template";
static TEMPLATE_IF_FALSE: &str = "benches/a_lot_of_no_output.template";

fn read_file_to_string(file_path: &str) -> Result<String, std::io::Error> {
    let mut file = File::open(file_path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn criterion_benchmark(c: &mut Criterion) {
    let template = read_file_to_string(TEMPLATE).unwrap();
    let template_str = template.as_str();
    let mut state = HashMap::default();
    state.insert(
        "items".into(),
        TinyLangTypes::Vec(Arc::new(vec!["a".into(), "b".into(), "c".into()])),
    );

    c.bench_function("basic_template_parse", move |b| {
        let state = state.clone();
        b.iter(move || {
            let state_cloned = state.clone();
            eval(black_box(template_str), black_box(state_cloned))
        });
    });

    let template = read_file_to_string(TEMPLATE_NO_FOR).unwrap();
    let template_str = template.as_str();

    c.bench_function("basic_template_parse_no_state", move |b| {
        b.iter(move || eval(black_box(template_str), black_box(HashMap::default())));
    });

    let template = read_file_to_string(TEMPLATE_IF_FALSE).unwrap();
    let template_str = template.as_str();

    c.bench_function("basic_template_parse_if_no_output", move |b| {
        b.iter(move || eval(black_box(template_str), black_box(HashMap::default())));
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
