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
  Literal(Literal)
  Unary(op: UnaryOp, expr: Expr)
  Binary(op: BinaryOp, left: Expr, right: Expr)
  Grouping(Expr)
}

pub type Literal {
  Number(value: Float)
  String(value: String)
  Boolean(value: Bool)
  Nil
}

pub type UnaryOp {
  NumericNegation
  BooleanNegation
}

pub type BinaryOp {
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
