import gleam/list
import gleam/order
import gleam/string
import gleeunit
import gleeunit/should
import parser_combinators.{
  Error, Success, identifier, input_from_string, number,
  string_literal,
}
import error_formatter

pub fn main() -> Nil {
  gleeunit.main()
}

// ============================================================================
// Number Parser Tests
// ============================================================================

pub fn number_parser_test() {
  test_parser(number, [
    // Integers
    #("single digit", "1", 1.0, ""),
    #("multiple digits", "123", 123.0, ""),
    // Floats
    #("simple float", "1.0", 1.0, ""),
    #("float with decimals", "1.23", 1.23, ""),
    // Trailing text
    #("with trailing text", "1and more", 1.0, "and more"),
    #("float with trailing", "123.45xyz", 123.45, "xyz"),
  ])
}

pub fn number_parser_failure_test() {
  let test_cases = [
    #("letter fails", "a"),
    #("letter then digit fails", "a1"),
  ]

  use #(description, input) <- test_each(test_cases)
  let assert Error(_, _, _) =
    number()(input_from_string(input)) as description
  Nil
}

// ============================================================================
// Identifier Parser Tests
// ============================================================================

pub fn identifier_parser_test() {
  test_parser(identifier, [
    #("simple", "orchid+123", "orchid", "+123"),
    #("with digit", "orchid1 - 2", "orchid1", " - 2"),
    #("with underscore", "my_var", "my_var", ""),
    #("starting with underscore", "_private", "_private", ""),
  ])
}

// ============================================================================
// String Literal Parser Tests
// ============================================================================

pub fn string_literal_parser_test() {
  test_parser(string_literal, [
    #("simple string", "\"hello\"", "hello", ""),
    #("empty string", "\"\"", "", ""),
  ])
}

pub fn string_literal_unterminated_test() {
  // Unterminated strings should give a clear error message
  let source = "\"wor"
  let assert Error(message, span, committed) =
    string_literal()(input_from_string(source))

  let formatted =
    error_formatter.format_error(kind: "Syntax error", message:, source:, at: span)
  formatted
  |> should.equal(
    "┌─ Syntax error at line 1, column 1
  |
1 | \"wor
  | ^
  |     Whoops, it seems like you forgot to terminate this string literal!",
  )

  committed |> should.be_true
}

// ============================================================================
// Character Ordering Test
// ============================================================================

pub fn is_letter_ordering_test() {
  // Verify the assumption about character ordering used in is_letter
  string.compare("A", "a")
  |> should.equal(order.Lt)
}

// ============================================================================
// Helper Functions
// ============================================================================

fn test_each(test_cases: List(a), run_test: fn(a) -> Nil) -> Nil {
  list.each(test_cases, run_test)
}

fn test_parser(
  parser: fn() -> parser_combinators.Parser(a),
  test_cases: List(#(String, String, a, String)),
) -> Nil {
  use #(description, input, expected_value, expected_remaining) <- test_each(
    test_cases,
  )
  let assert Success(value, remaining) =
    parser()(input_from_string(input)) as description
  assert value == expected_value as description
  assert remaining.unconsumed == expected_remaining as description
}
