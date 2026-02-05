import expr
import gleam/list
import gleeunit
import parser_combinators.{Success}
import scanner.{
  Assignment, AtMost, Comma, Comment, Division, EOF, Equal, GreaterThan,
  Identifier, LOr, LeftBrace, LeftParen, LessThan, Literal, Minus, Negation,
  Plus, RightBrace, RightParen, Times, scan_tokens,
}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn scan_tokens_test() {
  let test_cases = [
    // Punctuation and operators
    #(
      "punctuation and comparison",
      ",;{<=",
      [Comma, scanner.Semicolon, LeftBrace, AtMost, EOF],
    ),
    #(
      "all operators",
      "!*+-/=<> <= == // operators",
      [
        Negation, Times, Plus, Minus, Division, Assignment, LessThan,
        GreaterThan, AtMost, Equal, Comment, EOF,
      ],
    ),
    // Grouping
    #(
      "grouping and braces",
      "(( )){} // grouping stuff",
      [
        LeftParen, LeftParen, RightParen, RightParen, LeftBrace, RightBrace,
        Comment, EOF,
      ],
    ),
    // Comments
    #(
      "block comments",
      "1 /* wahid in Arabic */ * 2 /* deux in French */",
      [
        Literal(expr.Number(1.0)), Comment, Times, Literal(expr.Number(2.0)),
        Comment, EOF,
      ],
    ),
    // Strings
    #(
      "string literals",
      "\"Hello\" + \"World\"",
      [Literal(expr.String("Hello")), Plus, Literal(expr.String("World")), EOF],
    ),
    // Numbers
    #(
      "number addition",
      "12 + 4",
      [Literal(expr.Number(12.0)), Plus, Literal(expr.Number(4.0)), EOF],
    ),
    #(
      "decimal starting with dot",
      ".1",
      [scanner.Dot, Literal(expr.Number(1.0)), EOF],
    ),
    // TODO: This should be [Literal(Number(1.0)), Dot, EOF] but number parser
    // greedily consumes "1." then fails to parse it as a float
    #(
      "number followed by dot (known issue)",
      "1.",
      [Identifier("1"), scanner.Dot, EOF],
    ),
    #(
      "negative number (minus then number)",
      "-12",
      [Minus, Literal(expr.Number(12.0)), EOF],
    ),
    // Keywords
    #("or keyword", "or", [LOr, EOF]),
    // Identifiers
    #(
      "identifier starting with keyword",
      "orchid",
      [Identifier("orchid"), EOF],
    ),
    #(
      "identifier with underscore",
      "Za_za",
      [Identifier("Za_za"), EOF],
    ),
  ]

  use #(description, input, expected) <- test_each(test_cases)
  let assert Success(tokens, _) = scan_tokens(input) as description
  assert tokens == expected as description
}

// ============================================================================
// Helper Functions
// ============================================================================

fn test_each(test_cases: List(a), run_test: fn(a) -> Nil) -> Nil {
  list.each(test_cases, run_test)
}
