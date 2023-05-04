# TinyLang

TinyLang is a simple template language done while streaming at twitch with @Karreiro. 

One of the design decisions is that you cannot assign new values to variables 
inside the template file. So you cannot write something like `{% a = 2 %}`, you
have to actually setup the state on the Rust side. One small exception is the for loop
which does assign and create a new variable to be used inside the template.

## State 

WIP

| Feature      | State | Syntax |
| ----------- | ----------- | ----------- |
| HTML        | ✅       | `any char`|
| Variables   | ✅        |  lowercase alphanumeric e.g. (`myvar`)|
| Logical operators| ✅        | `and`, `or`|
| Print Statements `{{}}`   | ✅        | `{{ 1 + 2 + 3 }}`|
| Bool, String, Numeric types   | ✅        | `true`, `false`, `'a string'`, `3`|
| if/else   | ✅       | `{% if true %}` `{% else %}`  `{% end %}`|
| loops   | ✅       | `{%for item in array %}` `{% end %}`|
| function calls   | ✅        | `my_function('abc')`|
| Dot operator   | ✅       | `something.something_else`|

### Other things missing

- Benchmarks
- Documentation

## Examples

Anything not inside `{{ }}` (print statement) and `{% %}` (dynamic statement) will be in the output.

The language support:

Numbers (integers and floats), String (e.g. `'a string'`), bool (`true` and `false`).

It's possible to use Vectors (`Vec<TinyLangType>`) and Objects (a variable with a `HashMap<String, TinyLangType>`),
but it's not possible to construct them inside a template file. It's only possible to create
those types on the Rust side. You can check the examples on how to use the language on the UnitTests at tests/examples.rs . 

## Using as a library

You can call the `eval` method which receives the template (`&str`) and the `State` (variables and functions accessible inside the template). It returns
the parsed string or an error.


You can also try it out at: https://tinylang.elias.tools


