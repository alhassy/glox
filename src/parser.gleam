import expr.{type Expr}
import gleam/list
import gleam/option.{type Option, None, Some}
import parser_combinators.{type Parser} as parse
import scanner.{type Token, token_to_string} as s

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

/// Check if a token is a "primary" token that can never be an operator.
/// Used for context-aware strict checking in `many_with_seperator`.
///
/// ## Primary Tokens
///
/// A token is "primary" if it can **only** start a new value expression and
/// can **never** be an operator (binary or unary) in any context.
///
/// ### Primary tokens in Lox:
/// - **NUMBER** (e.g., `123`, `45.67`) - always a literal value
/// - **STRING** (e.g., `"hello"`) - always a literal value
/// - **IDENTIFIER** (e.g., `apple`, `foo`) - variable reference (not valid in Lox yet, but never an operator)
/// - **TRUE/FALSE/NIL** - boolean and null literals
/// - **LEFT_PAREN** `(` - starts a grouping expression
///
/// ### Non-primary tokens (ambiguous):
/// - **MINUS** `-` - can be binary operator (`1 - 2`) OR unary operator (`-3`)
/// - **BANG** `!` - can be unary operator (`!true`) in some contexts
/// - **RIGHT_PAREN** `)` - ends expression, handled separately
/// - **SEMICOLON**, **COMMA** - statement/argument separators, not value starters
///
/// ## Why this matters
///
/// When parsing `1 * 2 X`:
/// - If `X` is `3` (NUMBER) → definitely missing operator! Error.
/// - If `X` is `-` (MINUS) → could be valid at higher precedence. Don't error yet.
///
/// This distinction is what makes context-aware strict checking work without
/// breaking valid expressions like `1 * 2 - 3`.
fn is_primary_token(token: Token) -> Bool {
  case token {
    s.Literal(s.Number(_), _) -> True
    s.Literal(s.String(_), _) -> True
    s.Literal(s.Identifer(_), _) -> True
    s.Keyword(s.LTrue, _) -> True
    s.Keyword(s.LFalse, _) -> True
    s.Keyword(s.LNil, _) -> True
    s.Punctuation(s.LeftParen, _) -> True
    _ -> False
  }
}

/// Helper to parse binary operator chains: `value (op value)*`
/// Parses first value, then zero or more (operator, value) pairs, and folds them into a Binary expression tree.
fn binary_op_chain(
  value_parser: Parser(Token, Expr),
  seperator_parser: Parser(Token, expr.BinaryOp),
) -> Parser(Token, Expr) {
  use #(first, pairs) <- parse.then(parse.many_with_seperator(
    value_parser: value_parser,
    seperator_parser: seperator_parser,
    is_unexpected: is_primary_token,
  ))
  parse.return(
    list.fold(pairs, first, fn(left, pair) {
      let #(op, right) = pair
      expr.Binary(op, left, right)
    }),
  )
}

/// Implement grammar rule  `equality       → comparison ( ( "!=" | "==" ) comparison )* ;`
pub fn equality() -> Parser(Token, Expr) {
  binary_op_chain(
    comparison(),
    parse.one_token() |> parse.choose(token_as_equals_or_non),
  )
}

/// Implement grammar rule  `comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;`
pub fn comparison() -> Parser(Token, Expr) {
  binary_op_chain(
    term(),
    parse.one_token() |> parse.choose(token_as_comparison),
  )
}

/// Implement grammar rule `term           → factor ( ( "-" | "+" ) factor )* ;`
pub fn term() -> Parser(Token, Expr) {
  binary_op_chain(
    factor(),
    parse.one_token() |> parse.choose(token_as_minus_or_plus),
  )
}

/// Implement grammar rule `factor         → unary ( ( "/" | "*" ) unary )* `
pub fn factor() -> Parser(Token, Expr) {
  binary_op_chain(
    unary(),
    parse.one_token() |> parse.choose(token_as_div_or_mult),
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
  |> parse.or(literal())
  |> parse.or(grouping())
}

fn token_as_expr_unary_op(it: Token) -> Result(expr.UnaryOp, String) {
  case it {
    s.Operator(s.Negation, _) -> expr.BooleanNegation |> Some
    s.Operator(s.Minus, _) -> expr.NumericNegation |> Some
    _ -> None
  }
  |> expecting(it, to_be: "a unary op `! , -`")
}

fn literal() {
  parse.one_token()
  |> parse.choose(token_as_expr_literal)
  |> parse.map(expr.Literal)
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

// Parse a parenthesised expression?
fn grouping() -> Parser(Token, expr.Expr) {
  use it <- get(parse.one_token())
  use <- asserting(it, is_left_parens, ie: "an open parens")
  use expr <- get(expr())
  use it <- get(parse.one_token())
  use <- asserting(it, is_right_parens, ie: "a closing parens")
  parse.return(expr.Grouping(expr))
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

/// An alias for `then` that makes code involving `use` more readable
fn get(first, callback) {
  parse.then(first, callback)
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
