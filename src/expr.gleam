import parser_combinators.{type Span}

/// A Gleam representation of the following grammar.
/// ```
/// expression     → literal
///               | unary
///               | binary
///               | grouping ;
///
/// literal        → NUMBER | STRING | "true" | "false" | "nil" ;
/// grouping       → "(" expression ")" ;
/// unary          → ( "-" | "!" ) expression ;
/// binary         → expression operator expression ;
/// operator       → "==" | "!=" | "<" | "<=" | ">" | ">="
///                | "+"  | "-"  | "*" | "/" ;
/// ```
pub type Expr {
  Variable(name: String, span: Span)
  Literal(value: Literal, span: Span)
  Op(op: Operator, operands: List(Expr), span: Span)
  Grouping(expr: Expr, span: Span)
  /// The value of an assignment "l = r" is whatever the value of "r" is.
  Assign(name: String, expr: Expr, span: Span)
}

pub type Literal {
  Number(value: Float)
  String(value: String)
  Boolean(value: Bool)
  Nil
}

pub type Operator {
  NumericNegation
  BooleanNegation
  Equals
  NotEquals
  LessThan
  AtMost
  GreaterThan
  AtLeast
  Plus
  Minus
  Times
  Divides
}
