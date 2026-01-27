import evaluator
import expr.{Boolean, Number, String}
import gleeunit
import parser
import parser_combinators.{Success}
import scanner.{scan_tokens}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn eval_happy_test() {
  assert eval("1 / 0") == Ok(Number(0.0))

  // Since 0 < 5
  assert eval("1 + 2 - 3 < 5") == Ok(Boolean(True))

  assert eval("1 + \"hola\"") == Ok(String("1.0hola"))
}

pub fn boolean_negation_happy_test() {
  assert evaluator.eval(expr.Unary(
      expr.BooleanNegation,
      expr.Literal(expr.Boolean(True)),
    ))
    == Ok(Boolean(False))

  assert eval("!true") == Ok(Boolean(False))
}

pub fn boolean_negation_sad_test() {
  assert evaluator.eval(expr.Unary(
      expr.BooleanNegation,
      expr.Literal(expr.String("hola")),
    ))
    == Error(
      "Canot apply operation BooleanNegation to Literal(String(\"hola\"))",
    )

  assert eval("!\"hola\"")
    == Error(
      "Canot apply operation BooleanNegation to Literal(String(\"hola\"))",
    )
}

fn eval(input) {
  let assert Ok(tokens) = scan_tokens(input, 0)
  let assert Success(found: expr, unconsumed: _) = parser.expr()(tokens)
  evaluator.eval(expr)
}
