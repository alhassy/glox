import expr.{
  AtMost, Binary, Boolean, Divides, Equals, Grouping, LessThan, Literal, Minus,
  NotEquals, Number, Plus, Times,
}
import gleeunit
import parser
import parser_combinators.{Error, Success}
import scanner.{Operator, scan_tokens}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn parse_expr_test() {
  assert run(parser.equality, "1 * 2  !=   (2 < (0 - 0))")
    == Success(
      Binary(
        NotEquals,
        Binary(Times, Literal(Number(1.0)), Literal(Number(2.0))),
        Grouping(Binary(
          LessThan,
          Literal(Number(2.0)),
          Grouping(Binary(Minus, Literal(Number(0.0)), Literal(Number(0.0)))),
        )),
      ),
      [],
    )

  // We parse `1 * 2` as valid input and leave the rest as unconsumed input.
  assert run(parser.equality, "1 * 2 3 4")
    == Success(Binary(Times, Literal(Number(1.0)), Literal(Number(2.0))), [
      scanner.Literal(scanner.Number(3.0), 0),
      scanner.Literal(scanner.Number(4.0), 0),
    ])

  // We parse `1` as valid input and leave the rest as unconsumed input.
  assert run(parser.equality, "1 * apple")
    == Success(Literal(Number(1.0)), [
      Operator(scanner.Times, 0),
      scanner.Literal(scanner.Identifer("apple"), 0),
    ])
}

pub fn parse_equality_test() {
  assert run(parser.equality, "1 * 2  <=  3 / 4    ==   true")
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
  assert run(parser.comparison, "1 * 2 <= 3 / 4 + 5")
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
  assert run(parser.term, "1 * 2 - 3 / 4 + 5")
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
  assert run(parser.factor, "(123)")
    == Success(Grouping(Literal(Number(123.0))), [])
  assert run(parser.factor, "(123)*4")
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
  assert run(parser.factor, "1*2/3*4") == one_times_two_div_3_times_4
  // Whitespace don't matter 🥳
  assert run(parser.factor, "1 * 2  /3* 4") == one_times_two_div_3_times_4
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
  assert parser.expr()(tokens)
    == Success(
      expr.Unary(
        expr.NumericNegation,
        expr.Grouping(expr.Literal(expr.Number(123.0))),
      ),
      [],
    )
    as "Parsing  -(123)  expr"

  expect_parse_error(
    parser.expr,
    "-+(123)",
    "Expected a unary op `! , -` but saw +",
  )
}

pub fn parse_parenthesised_test() {
  assert run(parser.expr, "(123)")
    == Success(expr.Grouping(expr.Literal(expr.Number(123.0))), [])
    as "Parsing  (123)  expr"
}

pub fn parse_literal_test() {
  assert run(parser.expr, "123")
    == Success(expr.Literal(expr.Number(123.0)), [])
    as "Parsing  123  literal"

  assert run(parser.expr, "nil") == Success(expr.Literal(expr.Nil), [])
    as "Parsing  nil  literal"

  assert run(parser.expr, "true")
    == Success(expr.Literal(expr.Boolean(True)), [])
    as "Parsing  true  literal"
}

pub fn parse_literal_fails_result_in_informative_messages_test() {
  expect_parse_error(
    parser.expr,
    "apple",
    "Expected a unary op `! , -` but saw identifier `apple`",
  )
  expect_parse_error(parser.expr, "*", "Expected a unary op `! , -` but saw *")
  expect_parse_error(
    parser.expr,
    "var x = 123;",
    "Expected a unary op `! , -` but saw keyword `var`",
  )
}

fn expect_parse_error(parser, input, err_msg) {
  assert run(parser, input) == Error(err_msg)
}

fn run(parser, input) {
  let assert Ok(tokens) = scan_tokens(input, 0)
  parser()(tokens)
}
