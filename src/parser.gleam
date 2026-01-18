import expr.{type Expr}
import gleam
import gleam/option.{type Option, None, Some}
import parser_combinators.{type Parser} as parse
import scanner.{type Token, token_to_string} as s

fn expecting(
  option: Option(value),
  it token: Token,
  to_be expectation: String,
) -> Result(value, String) {
  option
  |> option.to_result(expected(it: token, to_be: expectation))
}

fn expected(to_be expected: String, it token: Token) -> String {
  "Expected " <> expected <> " but saw " <> token_to_string(token)
}

/// A Gleam parser for the following grammar.
/// ```
/// expression     → equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary
///                | primary ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil"
///                | "(" expression ")" ;
/// ```
pub fn expr() -> Parser(Token, expr.Expr) {
  equality()
}

/// Implement grammar rule  `equality       → comparison ( ( "!=" | "==" ) comparison )* ;`
pub fn equality() -> Parser(Token, Expr) {
  parse.many_with_seperator(
    value_parser: comparison(),
    seperator_parser: parse.one_token() |> parse.choose(token_as_equals_or_non),
    combiner: fn(l, op, r) { expr.Binary(op, l, r) },
  )
}

/// Implement grammar rule  `comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;`
pub fn comparison() -> Parser(Token, Expr) {
  parse.many_with_seperator(
    value_parser: term(),
    seperator_parser: parse.one_token() |> parse.choose(token_as_comparison),
    combiner: fn(l, op, r) { expr.Binary(op, l, r) },
  )
}

/// Implement grammar rule `term           → factor ( ( "-" | "+" ) factor )* ;`
pub fn term() -> Parser(Token, Expr) {
  parse.many_with_seperator(
    value_parser: factor(),
    seperator_parser: parse.one_token() |> parse.choose(token_as_minus_or_plus),
    combiner: fn(l, op, r) { expr.Binary(op, l, r) },
  )
}

/// Implement grammar rule `factor         → unary ( ( "/" | "*" ) unary )* `
pub fn factor() -> Parser(Token, Expr) {
  parse.many_with_seperator(
    value_parser: unary(),
    seperator_parser: parse.one_token() |> parse.choose(token_as_div_or_mult),
    combiner: fn(l, op, r) { expr.Binary(op, l, r) },
  )
}

fn token_as_div_or_mult(it: Token) -> Result(expr.BinaryOp, String) {
  case it {
    s.Operator(s.Division, _) -> Some(expr.Divides)
    s.Operator(s.Times, _) -> Some(expr.Times)
    _ -> None
  }
  |> expecting(it, to_be: "the binary operator `/` or `*`")
}

fn token_as_minus_or_plus(it: Token) -> Result(expr.BinaryOp, String) {
  case it {
    s.Operator(s.Minus, _) -> Some(expr.Minus)
    s.Operator(s.Plus, _) -> Some(expr.Plus)
    _ -> None
  }
  |> expecting(it, to_be: "the binary operator `-` or `+`")
}

fn token_as_comparison(it: Token) -> Result(expr.BinaryOp, String) {
  case it {
    s.Operator(s.LessThan, _) -> Some(expr.LessThan)
    s.Operator(s.AtMost, _) -> Some(expr.AtMost)
    s.Operator(s.GreaterThan, _) -> Some(expr.GreaterThan)
    s.Operator(s.AtLeast, _) -> Some(expr.AtLeast)
    _ -> None
  }
  |> expecting(it, to_be: "a comparison operator `>, >=, <, <=`")
}

fn token_as_equals_or_non(it: Token) -> Result(expr.BinaryOp, String) {
  case it {
    s.Operator(s.Equal, _) -> Some(expr.Equals)
    s.Operator(s.NotEqual, _) -> Some(expr.NotEquals)
    _ -> None
  }
  |> expecting(it, to_be: "an equality operator `==` or `!=`")
}

/// Implement grammar rule `unary   →   ( "!" | "-" ) unary  |  primary`
pub fn unary() -> Parser(Token, expr.Expr) {
  parse.one_token()
  |> parse.choose(token_as_expr_unary_op)
  |> parse.then(fn(op) { unary() |> parse.map(expr.Unary(op, _)) })
  |> parse.or(primary())
}

fn token_as_expr_unary_op(it: Token) -> Result(expr.UnaryOp, String) {
  case it {
    s.Operator(s.Negation, _) -> expr.BooleanNegation |> Some
    s.Operator(s.Minus, _) -> expr.NumericNegation |> Some
    _ -> None
  }
  |> expecting(it, to_be: "a unary op `! , -`")
}

pub fn primary() -> Parser(Token, expr.Expr) {
  // Do we have a literal expression?
  {
    parse.one_token()
    |> parse.choose(token_as_expr_literal)
    |> parse.map(expr.Literal)
  }
  // Or a parenthesised expression?
  |> parse.or({
    use it <- get(parse.one_token())
    use <- asserting(it, is_left_parens, ie: "an open parens")
    use expr <- get(expr())
    use it <- get(parse.one_token())
    use <- asserting(it, is_right_parens, ie: "a closing parens")
    parse.return(expr.Grouping(expr))
  })
}

fn is_left_parens(token: Token) -> Bool {
  case token {
    s.Punctuation(s.LeftParen, _) -> True
    _ -> False
  }
}

fn is_right_parens(token: Token) -> Bool {
  case token {
    s.Punctuation(s.RightParen, _) -> True
    _ -> False
  }
}

fn token_as_expr_literal(it: Token) -> Result(expr.Literal, String) {
  case it {
    s.Literal(s.Number(value), _) -> value |> expr.Number |> Some
    s.Literal(s.String(value), _) -> value |> expr.String |> Some
    s.Keyword(s.LNil, _) -> expr.Nil |> Some
    s.Keyword(s.LTrue, _) -> True |> expr.Boolean |> Some
    s.Keyword(s.LFalse, _) -> False |> expr.Boolean |> Some
    _ -> None
  }
  |> expecting(it, to_be: "a literal `Number , String , true , false , nil`")
}

/// Instead of writing `use <- when(it |> is_foo, it |> expected("foo", _))`, write `use <- expecting_(it, is_foo, ie:, "foo")`
fn asserting(
  it: Token,
  predicate: fn(Token) -> Bool,
  continuation: fn() -> fn(List(a)) -> parse.ParseResult(a, b),
  ie msg: String,
) -> fn(List(a)) -> parse.ParseResult(a, b) {
  parse.when(predicate(it), expected(msg, it), continuation)
}

/// An alias for `then` that makes code involving `use` more readable
fn get(first, callback) {
  parse.then(first, callback)
}
