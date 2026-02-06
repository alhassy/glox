// Tests for error reporting - both parse-time and runtime errors

import error_formatter
import expr.{Divides, Literal, Number, Op, Plus, String, Times, Variable}
import gleam/int
import gleam/list
import gleam/option.{Some}
import gleam/string
import gleeunit
import parser_combinators.{Span, Success, SyntaxError}
import program.{ExprStatement, IO, Print, Program, Statement, VarDecl}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn program_eval_error_test() {
  let test_cases = [
    #("two prints and a no-op", "print 38 / 2; 44 * true; print \"bye\";", [
      "PRINTED 19",
      "PRINTED ┌─ Runtime error at line 1, columns 20-23
          |
        1 | print 38 / 2; 44 * true; print \"bye\";
          |                    ^^^^
          |     Cannot apply '*' to number and boolean; right operand must be number",
      "PRINTED \"bye\"",
    ]),
  ]

  use #(description, input, expected_side_effects) <- list.each(test_cases)
  let assert Success(found: program, ..) = program.parse(input) as description
  let transparent_io =
    IO(print: fn(str) { "PRINTED " <> str }, do_nothing: fn() { "skip" })
  let actual_side_effects = program.eval(transparent_io, input, program)
  assert actual_side_effects == expected_side_effects |> list.map(dedent)
    as description
}

pub fn program_eval_success_test() {
  let test_cases = [
    #("one print statement", "print 2 * 7;", ["PRINTED 14"]),
    #("two prints and a no-op", "print 38 / 2; 44 * 4; print \"bye\";", [
      "PRINTED 19",
      "skip",
      "PRINTED \"bye\"",
    ]),
  ]

  use #(description, input, expected_side_effects) <- list.each(test_cases)
  let assert Success(found: program, ..) = program.parse(input) as description
  let transparent_io =
    IO(print: fn(str) { "PRINTED " <> str }, do_nothing: fn() { "skip" })
  let actual_side_effects = program.eval(transparent_io, input, program)
  assert expected_side_effects == actual_side_effects as description
}

pub fn program_parser_success_test() {
  let test_cases = [
    #(
      "one print statement",
      "print 1;",
      Program([Statement(Print(Literal(Number(1.0), Span(1, 7, 1))))], []),
    ),
    #(
      "two prints and a no-op, and test spacing",
      "print 38 / 2;  print 44 * 4;    print \"bye\"  ;",
      Program(
        [
          Statement(
            Print(Op(
              Divides,
              [
                Literal(Number(38.0), Span(1, 7, 2)),
                Literal(Number(2.0), Span(1, 12, 1)),
              ],
              Span(1, 7, 6),
            )),
          ),
          Statement(
            Print(Op(
              Times,
              [
                Literal(Number(44.0), Span(1, 22, 2)),
                Literal(Number(4.0), Span(1, 27, 1)),
              ],
              Span(1, 22, 6),
            )),
          ),
          Statement(Print(Literal(String("bye"), Span(1, 39, 5)))),
        ],
        [],
      ),
    ),
    #(
      "global variable declaration then print call involving string catenation",
      "var name = \"James!\"; print \"Hello \" + name; ",
      Program(
        [
          VarDecl("name", Some(Literal(String("James!"), Span(1, 12, 8)))),
          Statement(
            Print(Op(
              Plus,
              [
                Literal(String("Hello "), Span(1, 28, 8)),
                Variable("name", Span(1, 39, 4)),
              ],
              Span(1, 28, 15),
            )),
          ),
        ],
        [],
      ),
    ),
    #(
      "a malformed variable declaration doesn't prevent parsing subsequent valid statements",
      "var name \"James!\" /* whoops, forgot the =!*/ print \"Bye\"; ",
      // The malformed var decl causes errors but we recover and parse the print statement
      Program([Statement(Print(Literal(String("Bye"), Span(1, 52, 5))))], [
        SyntaxError(
          "Expected to see an `=` for variable assignment",
          Span(1, 10, 1),
        ),
      ]),
    ),
    #(
      "`variable` is a valid identifier and not the start of a `var` declaration!",
      "print; variable;",
      // The malformed var decl causes errors but we recover and parse the print statement
      Program([Statement(ExprStatement(Variable("variable", Span(1, 8, 8))))], [
        SyntaxError(
          "A print clause is of the form ` print <expr>; `, you're missing the expr!",
          Span(1, 6, 1),
        ),
      ]),
    ),
    #(
      "three different kinds of errors",
      "var name /* missing semicolon*/ print /* missing expr*/; 1 - /* missing right operand! */;",
      Program([], [
        SyntaxError(
          "Expected to see an `=` for variable assignment",
          Span(1, 33, 1),
        ),
        SyntaxError(
          "A print clause is of the form ` print <expr>; `, you're missing the expr!",
          Span(1, 38, 1),
        ),
        SyntaxError(
          "Expected a number, string, or expression after operator",
          Span(1, 90, 1),
        ),
      ]),
    ),
  ]

  use #(description, input, expected_program) <- list.each(test_cases)
  let assert Success(found:, remaining:) = program.parse(input) as description
  assert remaining.unconsumed == "" as "All input consumed by program parser"
  assert found == expected_program as description
}

pub fn program_parser_error_test() {
  // These test cases have syntax errors that result in error recovery.
  // The parser returns Success with collected errors instead of ParseError.
  let test_cases = [
    #(
      "unterminated print statement",
      "print 1",
      "┌─ Syntax error at line 1, column 8
       |
     1 | print 1
       |        ^
       |     I expected to see a semicolon here 🤔",
    ),
    #(
      "unterminated expr statement",
      "1 + 3",
      "┌─ Syntax error at line 1, column 6
       |
     1 | 1 + 3
       |      ^
       |     I expected to see a semicolon here 🤔",
    ),
    #(
      "extra input after expression",
      "true & then some extra input;",
      "┌─ Syntax error at line 1, column 6
       |
     1 | true & then some extra input;
       |      ^
       |     I expected to see a semicolon here 🤔",
    ),
    #(
      "var decl without termining semi",
      "var name = 123",
      "┌─ Syntax error at line 1, column 15
       |
     1 | var name = 123
       |               ^
       |     I expected to see a semicolon here 🤔",
    ),
    #(
      "trailing `=` in var decl without initalizer",
      "var name =",
      "┌─ Syntax error at line 1, column 11
       |
     1 | var name =
       |           ^
       |     I expected to see an expression here 😖",
    ),
    #(
      "variable decl but missing `=` between name and value",
      "var name 123",
      "┌─ Syntax error at line 1, column 10
       |
     1 | var name 123
       |          ^
       |     Expected to see an `=` for variable assignment",
    ),
  ]

  use #(description, input, expected_formatted) <- list.each(test_cases)
  let assert Success(
    found: Program(declarations: [], errors: [first_error, ..]),
    ..,
  ) = program.parse(input)
    as description
  let formatted =
    error_formatter.format_error(
      kind: "Syntax error",
      message: first_error.message,
      source: input,
      at: first_error.at,
    )
  assert formatted == dedent(expected_formatted) as description
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Removes common leading indentation from multi-line strings.
/// The first line's indentation is ignored (it's usually on the same line as the quote).
/// All other lines have the minimum common indentation stripped.
fn dedent(s: String) -> String {
  case string.split(s, "\n") {
    [] -> s
    [first] -> first
    [first, ..rest] -> {
      // Find minimum indentation among non-empty lines
      let min_indent =
        rest
        |> list.filter(fn(line) { string.trim(line) != "" })
        |> list.map(fn(line) { count_leading_spaces(line) })
        |> list.fold(999_999, int.min)

      // Strip that indentation from each line
      let dedented_rest =
        rest
        |> list.map(fn(line) { string.drop_start(line, min_indent) })

      [first, ..dedented_rest]
      |> string.join("\n")
    }
  }
}

fn count_leading_spaces(s: String) -> Int {
  s
  |> string.to_graphemes
  |> list.take_while(fn(c) { c == " " })
  |> list.length
}
