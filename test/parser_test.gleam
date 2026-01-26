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
  // Valid complete expression
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

  // NEW BEHAVIOR: `1 * 2 3 4` now ERRORS because after parsing `1 * 2`,
  // we find `3` (a NUMBER token) without a separator before it
  // Context-aware checking: NUMBER tokens can never be operators!
  assert run(parser.equality, "1 * 2 3 4")
    == Error(
      "Expected separator between values \n Expected the binary operator `/` or `*` but saw 3.0",
      True,
    )

  // Same for `1 1` - second `1` is a NUMBER without separator
  assert run(parser.equality, "1 1")
    == Error(
      "Expected separator between values \n Expected the binary operator `/` or `*` but saw 1.0",
      True,
    )

  // But `1 * 2 - 3` still works because `-` is NOT a primary token
  assert run(parser.equality, "1 * 2 - 3")
    == Success(
      Binary(
        Minus,
        Binary(Times, Literal(Number(1.0)), Literal(Number(2.0))),
        Literal(Number(3.0)),
      ),
      [],
    )

  // `1 * apple` ERRORS because after parsing `*` successfully,
  // we MUST parse a factor but `apple` is not valid.
  assert run(parser.equality, "1 * apple")
    == Error(
      "Expected an open parens but saw identifier `apple` "
        <> "\n Expected a literal `Number , String , true , false , nil` but saw identifier `apple` "
        <> "\n Expected a unary op `! , -` but saw identifier `apple`",
      committed: True,
    )
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
    "Expected an open parens but saw - "
      <> "\n Expected a literal `Number , String , true , false , nil` but saw - "
      <> "\n Expected an open parens but saw + "
      <> "\n Expected a literal `Number , String , true , false , nil` but saw + "
      <> "\n Expected a unary op `! , -` but saw +",
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
    "Expected an open parens but saw identifier `apple` \n Expected a literal `Number , String , true , false , nil` but saw identifier `apple` \n Expected a unary op `! , -` but saw identifier `apple`",
  )
  expect_parse_error(
    parser.expr,
    "*",
    "Expected an open parens but saw * \n Expected a literal `Number , String , true , false , nil` but saw * \n Expected a unary op `! , -` but saw *",
  )
  expect_parse_error(
    parser.expr,
    "var x = 123;",
    "Expected an open parens but saw keyword `var` \n Expected a literal `Number , String , true , false , nil` but saw keyword `var` \n Expected a unary op `! , -` but saw keyword `var`",
  )
}

fn expect_parse_error(parser, input, err_msg) {
  assert run(parser, input) == Error(err_msg, committed: False)
}

fn run(parser, input) {
  let assert Ok(tokens) = scan_tokens(input, 0)
  parser()(tokens)
}
