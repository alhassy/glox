import expr.{
  AtMost, Binary, Boolean, Divides, Equals, Grouping, Literal, Minus, Number,
  Plus, Times,
}
import gleeunit
import parse.{Success}
import scanner.{Operator, scan_tokens}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn parse_equality_test() {
  assert run(parse.equality, "1 * 2  <=  3 / 4    ==   true")
    == Success(
      Binary(
        Equals,
        Binary(
          AtMost,
          Binary(Times, Literal(Number(1.0)), Literal(Number(2.0))),
          Binary(Divides, Literal(Number(3.0)), Literal(Number(4.0))),
        ),
        Literal(Boolean(True)),
      ),
      [],
    )
}

pub fn parse_comparison_test() {
  assert run(parse.comparison, "1 * 2 <= 3 / 4 + 5")
    == Success(
      Binary(
        AtMost,
        Binary(Times, Literal(Number(1.0)), Literal(Number(2.0))),
        Binary(
          Plus,
          Binary(Divides, Literal(Number(3.0)), Literal(Number(4.0))),
          Literal(Number(5.0)),
        ),
      ),
      [],
    )
}

pub fn parse_term_test() {
  assert run(parse.term, "1 * 2 - 3 / 4 + 5")
    == Success(
      Binary(
        Plus,
        Binary(
          Minus,
          Binary(Times, Literal(Number(1.0)), Literal(Number(2.0))),
          Binary(Divides, Literal(Number(3.0)), Literal(Number(4.0))),
        ),
        Literal(Number(5.0)),
      ),
      [],
    )
}

pub fn parse_factor_test() {
  assert run(parse.factor, "(123)")
    == Success(Grouping(Literal(Number(123.0))), [])
  assert run(parse.factor, "(123)*4")
    == Success(
      Binary(Times, Grouping(Literal(Number(123.0))), Literal(Number(4.0))),
      [],
    )
  let one_times_two_div_3_times_4 =
    Success(
      Binary(
        Times,
        Binary(
          Divides,
          Binary(Times, Literal(Number(1.0)), Literal(Number(2.0))),
          Literal(Number(3.0)),
        ),
        Literal(Number(4.0)),
      ),
      [],
    )
  assert run(parse.factor, "1*2/3*4") == one_times_two_div_3_times_4
  // Whitespace don't matter 🥳
  assert run(parse.factor, "1 * 2  /3* 4") == one_times_two_div_3_times_4
}

pub fn parse_unary_test() {
  let assert Ok(tokens) = scan_tokens("-(123)", 0)
  assert tokens
    == [
      Operator(scanner.Minus, 0),
      scanner.Punctuation(scanner.LeftParen, 0),
      scanner.Literal(scanner.Number(123.0), 0),
      scanner.Punctuation(scanner.RightParen, 0),
    ]
  assert parse.expr()(tokens)
    == parse.Success(
      expr.Unary(
        expr.NumericNegation,
        expr.Grouping(expr.Literal(expr.Number(123.0))),
      ),
      [],
    )
    as "Parsing  -(123)  expr"

  expect_parse_error(
    parse.expr,
    "-+(123)",
    "Expected a unary op `! , -` but saw +",
  )
}

pub fn parse_parenthesised_test() {
  assert run(parse.expr, "(123)")
    == parse.Success(expr.Grouping(expr.Literal(expr.Number(123.0))), [])
    as "Parsing  (123)  expr"
}

pub fn parse_literal_test() {
  assert run(parse.primary, "123")
    == parse.Success(expr.Literal(expr.Number(123.0)), [])
    as "Parsing  123  literal"

  assert run(parse.primary, "nil") == parse.Success(expr.Literal(expr.Nil), [])
    as "Parsing  nil  literal"

  assert run(parse.primary, "true")
    == parse.Success(expr.Literal(expr.Boolean(True)), [])
    as "Parsing  true  literal"
}

pub fn parse_literal_fails_result_in_informative_messages_test() {
  expect_parse_error(
    parse.primary,
    "apple",
    "Expected a literal `Number , String , true , false , nil` but saw identifier `apple`",
  )
  expect_parse_error(
    parse.primary,
    "*",
    "Expected a literal `Number , String , true , false , nil` but saw *",
  )
  expect_parse_error(
    parse.primary,
    "var x = 123;",
    "Expected a literal `Number , String , true , false , nil` but saw keyword `var`",
  )
}

pub fn parse_binary_operator_test() {
  let assert Ok(tokens) = scan_tokens("<=", 0)
  assert tokens == [Operator(scanner.AtMost, 0)] as "Scanning at-most operator"
  assert parse.binary_operator()(tokens) == parse.Success(expr.AtMost, [])
    as "scannar.AtMost converts to expr.AtMost"
}

pub fn parse_binary_op_fails_result_in_informative_messages_test() {
  // Unary boolean negation is not a binary operator
  expect_parse_error(
    parse.binary_operator,
    "!",
    "Expected a binary operator `== , != , < , <= , > , >= , +  , -  , * , /` but saw !",
  )
}

fn expect_parse_error(parser, input, err_msg) {
  assert run(parser, input) == parse.Error(err_msg)
}

fn run(parser, input) {
  let assert Ok(tokens) = scan_tokens(input, 0)
  parser()(tokens)
}
