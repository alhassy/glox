import gleam/list
import gleam/option.{None, Some}
import gleam/order
import gleam/string
import gleeunit
import scanner.{
  type Step, Assignment, AtMost, Comma, Comment, Division, Dot, Equal,
  GreaterThan, Identifer, LeftBrace, LeftParen, LessThan, Literal, Minus,
  Negation, Number, Operator, Plus, Punctuation, RightBrace, RightParen,
  Semicolon, Step, String, Times, Whitespace, parse_number, scan_tokens,
  split_on_identifier, split_on_numeric,
}

pub fn main() -> Nil {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn hello_world_test() {
  let name = "Joe"
  let greeting = "Hello, " <> name <> "!"

  assert greeting == "Hello, Joe!"
}

pub fn scan_tokens_test() {
  assert scan_tokens(",;{<=", 0)
    == Ok([
      Punctuation(Comma, 0),
      Punctuation(Semicolon, 0),
      Punctuation(LeftBrace, 0),
      Operator(AtMost, 0),
    ])
    as "Super simple token scanning test"

  assert scan_tokens("!*+-/=<> <= == // operators", 0)
    == Ok([
      Operator(Negation, 0),
      Operator(Times, 0),
      Operator(Plus, 0),
      Operator(Minus, 0),
      Operator(Division, 0),
      Punctuation(Assignment, 0),
      Operator(LessThan, 0),
      Operator(GreaterThan, 0),
      Punctuation(Whitespace, 0),
      Operator(AtMost, 0),
      Punctuation(Whitespace, 0),
      Operator(Equal, 0),
      Punctuation(Whitespace, 0),
      Punctuation(Comment, 0),
    ])
    as "Operators can be scanned"

  assert scan_tokens("(( )){} // grouping stuff", 0)
    == Ok([
      Punctuation(LeftParen, 0),
      Punctuation(LeftParen, 0),
      Punctuation(Whitespace, 0),
      Punctuation(RightParen, 0),
      Punctuation(RightParen, 0),
      Punctuation(LeftBrace, 0),
      Punctuation(RightBrace, 0),
      Punctuation(Whitespace, 0),
      Punctuation(Comment, 0),
    ])
    as "Grouping punctuation can be scanned"

  assert scan_tokens("\"Hello\" + \"World\"", 0)
    == Ok([
      Literal(String("Hello"), 0),
      Punctuation(Whitespace, 0),
      Operator(Plus, 0),
      Punctuation(Whitespace, 0),
      Literal(String("World"), 0),
    ])
    as "String literals can be scanned"

  assert scan_tokens("12 + 4", 0)
    == Ok([
      Literal(Number(12.0), 0),
      Punctuation(Whitespace, 0),
      Operator(Plus, 0),
      Punctuation(Whitespace, 0),
      Literal(Number(4.0), 0),
    ])
    as "Numbers can be scanned"

  assert scan_tokens(".1", 0)
    == Ok([Punctuation(Dot, 0), Literal(Number(1.0), 0)])
    as "No leading dot in number literal syntax"

  assert scan_tokens("1.", 0)
    == Ok([Literal(Number(1.0), 0), Punctuation(Dot, 0)])
    as "No trailing dot in number literal syntax"

  assert scan_tokens("-12", 0)
    == Ok([Operator(Minus, 0), Literal(Number(12.0), 0)])
    as "Negative numbers are not literals, but expressions"

  assert scan_tokens("or", 0) == Ok([Literal(Identifer("or"), 0)])
    as "Reserved keyword: or"

  assert scan_tokens("orchid", 0) == Ok([Literal(Identifer("orchid"), 0)])
    as "Identifier: orchid"
}

pub fn parse_number_test() {
  assert parse_number("1") == 1.0 |> from_float |> Some
  assert parse_number("123") == 123.0 |> from_float |> Some

  assert parse_number("1.0") == 1.0 |> from_float |> Some
  assert parse_number("1.23") == 1.23 |> from_float |> Some

  assert parse_number("a") == None
  assert parse_number("a1") == None

  assert parse_number("1and more") == Step(1.0, "and more") |> Some
}

fn from_float(x: Float) -> Step(Float) {
  Step(x, "")
}

pub fn split_on_numeric_test() {
  assert split_on_numeric("") == #("", "")
  assert split_on_numeric("1") == #("1", "")
  assert split_on_numeric("1.") == #("1", ".")
    as "Trailing dots are not part of the number syntax"
  assert split_on_numeric("1.23and more") == #("1.23", "and more")
}

pub fn split_on_identifier_test() {
  assert string.compare("A", "a") == order.Lt
  assert split_on_identifier("orchid+123") == #("orchid", "+123")
  assert split_on_identifier("orchid1 - 2") == #("orchid1", " - 2")
}
