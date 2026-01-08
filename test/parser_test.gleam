import expr
import gleeunit
import parse
import scanner.{Operator, scan_tokens}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn parse_literal_test() {
  let assert Ok(tokens) = scan_tokens("123", 0)
  assert parse.expr()(tokens)
    == parse.Success(expr.Literal(expr.Number(123.0)), [])
    as "Parsing  123  literal"

  let assert Ok(tokens) = scan_tokens("nil", 0)
  assert parse.expr()(tokens) == parse.Success(expr.Literal(expr.Nil), [])
    as "Parsing  nil  literal"

  let assert Ok(tokens) = scan_tokens("true", 0)
  assert tokens == [scanner.Keyword(scanner.LTrue, 0)]
  assert parse.expr()(tokens)
    == parse.Success(expr.Literal(expr.Boolean(True)), [])
    as "Parsing  true  literal"
}

pub fn parse_binary_operator_test() {
  let assert Ok(tokens) = scan_tokens("<=", 0)
  assert tokens == [Operator(scanner.AtMost, 0)] as "Scanning at-most operator"
  assert parse.binary_operator()(tokens) == parse.Success(expr.AtMost, [])
    as "scannar.AtMost converts to expr.AtMost"
  assert parse.binary_operator()([Operator(scanner.Negation, 0)])
    == parse.Error(
      "Expected a binary operator:  == , != , < , <= , > , >= , +  , -  , * , /",
    )
    as "Unary ops are not binary ops"
}
