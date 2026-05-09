use criterion::{criterion_group, criterion_main, BatchSize, Criterion, Throughput};
use std::collections::HashMap;
use std::fs;
use std::hint::black_box;
use tinylang::eval;
use tinylang::types::TinyLangType;

fn bench_simple(c: &mut Criterion) {
    let mut group = c.benchmark_group("simple");

    let static_html = "<html><body><h1>Hello, World!</h1><p>Some content here.</p></body></html>";
    group.throughput(Throughput::Bytes(static_html.len() as u64));
    group.bench_function("static_html", |b| {
        b.iter(|| eval(black_box(static_html), HashMap::default()))
    });

    let single_var = "Hello, {{ name }}!";
    let state_var = HashMap::from([("name".into(), TinyLangType::String("World".into()))]);
    group.throughput(Throughput::Bytes(single_var.len() as u64));
    group.bench_function("single_var", |b| {
        b.iter_batched(
            || state_var.clone(),
            |s| eval(black_box(single_var), s),
            BatchSize::SmallInput,
        )
    });

    let math = "{{ 1 + 2 * 3 / 4 - 1 }}";
    group.throughput(Throughput::Bytes(math.len() as u64));
    group.bench_function("math_expr", |b| {
        b.iter(|| eval(black_box(math), HashMap::default()))
    });

    let if_true = "{% if 1 == 1 %}yes{% end %}";
    group.throughput(Throughput::Bytes(if_true.len() as u64));
    group.bench_function("if_true", |b| {
        b.iter(|| eval(black_box(if_true), HashMap::default()))
    });

    let if_false = "{% if 1 != 1 %}yes{% end %}";
    group.throughput(Throughput::Bytes(if_false.len() as u64));
    group.bench_function("if_false", |b| {
        b.iter(|| eval(black_box(if_false), HashMap::default()))
    });

    group.finish();
}

fn bench_no_state(c: &mut Criterion) {
    let no_vars = fs::read_to_string("benches/no_vars.template").unwrap();
    let if_all_false = fs::read_to_string("benches/a_lot_of_no_output.template").unwrap();

    let mut group = c.benchmark_group("no_state");

    group.throughput(Throughput::Bytes(no_vars.len() as u64));
    group.bench_function("no_vars", |b| {
        b.iter(|| eval(black_box(&no_vars), HashMap::default()))
    });

    group.throughput(Throughput::Bytes(if_all_false.len() as u64));
    group.bench_function("if_all_false", |b| {
        b.iter(|| eval(black_box(&if_all_false), HashMap::default()))
    });

    group.finish();
}

fn bench_with_state(c: &mut Criterion) {
    let template = fs::read_to_string("benches/for.template").unwrap();
    let state = HashMap::from([(
        "items".into(),
        TinyLangType::Vec(vec!["a".into(), "b".into(), "c".into()]),
    )]);

    let mut group = c.benchmark_group("with_state");

    group.throughput(Throughput::Bytes(template.len() as u64));
    group.bench_function("nested_for_if", |b| {
        b.iter_batched(
            || state.clone(),
            |s| eval(black_box(&template), s),
            BatchSize::SmallInput,
        )
    });

    group.finish();
}

criterion_group!(benches, bench_simple, bench_no_state, bench_with_state);
criterion_main!(benches);
