// Tests for error reporting - both parse-time and runtime errors

import error_formatter
import evaluator
import expr_parser as parser
import gleam/int
import gleam/list
import gleam/string
import gleeunit
import parser_combinators.{Error as ParseError, Success}
import type_error.{RuntimeError}

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// Expr Parser Errors
// ============================================================================

pub fn parser_error_formatted_test() {
  let test_cases = [
    #(
      "unterminated string",
      "\"wor",
      "┌─ Syntax error at line 1, column 1
       |
     1 | \"wor
       | ^
       |     Whoops, it seems like you forgot to terminate this string literal!",
    ),
    #(
      "unmatched paren",
      "(1 + 2",
      "┌─ Syntax error at line 1, column 1
       |
     1 | (1 + 2
       | ^
       |     Hmm, looks like you forgot to close this parenthesis!",
    ),
    #(
      "unmatched paren with leading whitespace",
      "   (1 + 2",
      "┌─ Syntax error at line 1, column 4
       |
     1 |    (1 + 2
       |    ^
       |     Hmm, looks like you forgot to close this parenthesis!",
    ),
    #(
      "empty parens",
      "(",
      "┌─ Syntax error at line 1, column 2
       |
     1 | (
       |  ^
       |     Expected expression after '('",
    ),
    #(
      "missing operator between values",
      "1 2",
      "┌─ Syntax error at line 1, column 3
       |
     1 | 1 2
       |   ^
       |     Expected one of `*`, `/` before `2`",
    ),
    #(
      "unexpected character at column 5",
      "1 + @ 2",
      "┌─ Syntax error at line 1, column 5
       |
     1 | 1 + @ 2
       |     ^
       |     Expected a number, string, or expression after operator",
    ),
    #(
      "error on second line",
      "1 +\n@ 2",
      "┌─ Syntax error at line 2, column 1
       |
     2 | @ 2
       | ^
       |     Expected a number, string, or expression after operator",
    ),
    #(
      "multiline error on line 3",
      "1 +\n2 *\n@ 3",
      "┌─ Syntax error at line 3, column 1
       |
     3 | @ 3
       | ^
       |     Expected a number, string, or expression after operator",
    ),
    #(
      "extra input on line 3",
      "1 +\n2 *\n3 oops",
      "┌─ Syntax error at line 3, columns 3-6
       |
     3 | 3 oops
       |   ^^^^
       |     Expected one of `*`, `/` before `oops`",
    ),
    #(
      "trailing operator at EOF",
      "1 +",
      "┌─ Syntax error at line 1, column 3
       |
     1 | 1 +
       |   ^
       |     Expected a number, string, or expression after operator",
    ),
    #(
      "unterminated block comment",
      "1 + /* hola",
      "┌─ Syntax error at line 1, columns 5-6
       |
     1 | 1 + /* hola
       |     ^^
       |     Unterminated `/* ... */`",
    ),
    #(
      "unterminated multiline block comment",
      "1 + /* hola\nmundo",
      "┌─ Syntax error at line 1, columns 5-6
       |
     1 | 1 + /* hola
       |     ^^
       |     Unterminated `/* ... */`",
    ),
  ]

  use #(description, input, expected_formatted) <- test_each(test_cases)
  let assert ParseError(message, span, committed) = parser.parse(input)
    as description
  let formatted =
    error_formatter.format_error(
      kind: "Syntax error",
      message:,
      source: input,
      at: span,
    )
  assert formatted == dedent(expected_formatted) as description
  assert committed == True as description
}

// ============================================================================
// Expr Evaluation Runtime Errors
// ============================================================================

pub fn runtime_error_message_test() {
  let test_cases = [
    // Unary type errors
    #(
      "! on string",
      "!\"hola\"",
      "┌─ Runtime error at line 1, columns 2-7
       |
     1 | !\"hola\"
       |  ^^^^^^
       |     Cannot apply '!' to string; expected boolean",
    ),
    #(
      "! on number",
      "!1",
      "┌─ Runtime error at line 1, column 2
       |
     1 | !1
       |  ^
       |     Cannot apply '!' to number; expected boolean",
    ),
    #(
      "! on number in larger expression",
      "!1 + !3",
      "┌─ Runtime error at line 1, column 2
       |
     1 | !1 + !3
       |  ^
       |     Cannot apply '!' to number; expected boolean",
    ),
    #(
      "! on number on line 2",
      "1 + 2 +\n  !123",
      "┌─ Runtime error at line 2, columns 4-6
       |
     2 |   !123
       |    ^^^
       |     Cannot apply '!' to number; expected boolean",
    ),
    #(
      "! on number with block comment",
      "! /* hello world! */ 123",
      "┌─ Runtime error at line 1, columns 22-24
       |
     1 | ! /* hello world! */ 123
       |                      ^^^
       |     Cannot apply '!' to number; expected boolean",
    ),
    #(
      "- on string",
      "-\"hello\"",
      "┌─ Runtime error at line 1, columns 2-8
       |
     1 | -\"hello\"
       |  ^^^^^^^
       |     Cannot apply '-' to string; expected number",
    ),
    // Binary type errors - right operand wrong
    #(
      "number - string",
      "1 - \"1\"",
      "┌─ Runtime error at line 1, columns 5-7
       |
     1 | 1 - \"1\"
       |     ^^^
       |     Cannot apply '-' to number and string; right operand must be number",
    ),
    #(
      "number < string",
      "1 < \"hello\"",
      "┌─ Runtime error at line 1, columns 5-11
       |
     1 | 1 < \"hello\"
       |     ^^^^^^^
       |     Cannot apply '<' to number and string; right operand must be number",
    ),
    // Binary type errors - left operand wrong
    #(
      "nil < nil (left wrong)",
      "nil < nil",
      "┌─ Runtime error at line 1, columns 1-3
       |
     1 | nil < nil
       | ^^^
       |     Cannot apply '<' to nil; left operand must be number",
    ),
    #(
      "string - number (left wrong)",
      "\"a\" - 1",
      "┌─ Runtime error at line 1, columns 1-3
       |
     1 | \"a\" - 1
       | ^^^
       |     Cannot apply '-' to string; left operand must be number",
    ),
    // Binary type errors - both types valid but not together
    #(
      "boolean <= number (invalid combination)",
      "true <= 3",
      "┌─ Runtime error at line 1, column 9
       |
     1 | true <= 3
       |         ^
       |     Cannot apply '<=' to boolean and number; accepted combinations are (number, number) or (boolean, boolean)",
    ),
    // Grouping
    #(
      "grouped boolean < number",
      "(1 < 2) < 3",
      "┌─ Runtime error at line 1, columns 1-7
       |
     1 | (1 < 2) < 3
       | ^^^^^^^
       |     Cannot apply '<' to boolean; left operand must be number",
    ),
  ]

  use #(description, input, expected_formatted) <- test_each(test_cases)
  let assert Success(parsed_expr, _) = parser.parse(input) as description
  let assert Error(RuntimeError(msg, span)) = evaluator.eval(parsed_expr)
    as description
  let formatted =
    error_formatter.format_error(
      kind: "Runtime error",
      message: msg,
      source: input,
      at: span,
    )
  assert formatted == dedent(expected_formatted) as description
}

// ============================================================================
// Helper Functions
// ============================================================================

fn test_each(test_cases: List(a), run_test: fn(a) -> Nil) -> Nil {
  list.each(test_cases, run_test)
}

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
