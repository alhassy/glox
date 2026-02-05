// Tests for error reporting - both parse-time and runtime errors

import error_formatter
import expr.{Literal, Number}
import gleam/int
import gleam/list
import gleam/string
import gleeunit
import parser_combinators.{Error as ParseError, Span, Success}
import program.{Print, Program}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn program_parser_success_test() {
  let test_cases = [
    #(
      "one print statement",
      "print 1;",
      Program([Print(Literal(Number(1.0), Span(1, 7, 1)))]),
    ),
  ]

  use #(description, input, expected_program) <- list.each(test_cases)
  let assert Success(found:, remaining:) = program.parse(input) as description
  assert remaining.unconsumed == "" as "All input consumed by program parser"
  assert found == expected_program as description
}

pub fn program_parser_error_test() {
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
  ]

  use #(description, input, expected_formatted) <- list.each(test_cases)
  let assert ParseError(message, span, committed) = program.parse(input)
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
