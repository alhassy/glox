import expr
import gleeunit
import parse
import scanner.{Operator, scan_tokens}

pub fn main() -> Nil {
  gleeunit.main()
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
