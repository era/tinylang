# TinyLang

TinyLang is a simple template language done while streaming at twitch with @Karreiro. 

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

## Using as a library

You can call the `eval` method which receives the `&str` and the `State` (variables and functions). It returns
the parsed string or an error.


You can also try it out at: https://tinylang.elias.tools


