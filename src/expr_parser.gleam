import expr.{
  type Expr, type Operator, AtLeast, AtMost, Boolean, BooleanNegation, Divides,
  Equals, GreaterThan, Grouping, LessThan, Literal, Minus, Nil as LitNil,
  NotEquals, Number, NumericNegation, Op, Plus, String, Times, Variable,
}
import gleam/list
import gleam/string
import parser_combinators.{type Parser} as parse

/// A Gleam parser for the following grammar.
/// ```
/// expression     → equality ;
/// assignment     → IDENTIFIER "=" assignment | equality ;
/// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
/// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
/// term           → factor ( ( "-" | "+" ) factor )* ;
/// factor         → unary ( ( "/" | "*" ) unary )* ;
/// unary          → ( "!" | "-" ) unary
///                | primary ;
/// primary        → NUMBER | STRING | "true" | "false" | "nil"
///                | "(" expression ")" ;
/// ```
/// Parse an expression from source text with position tracking for error messages.
///
/// ## Example
/// ```gleam
/// case parse("1 + @ 2") {
///   parse.Success(expr, _) -> // ... use expr
///   parse.Error(msg, line, col, _) ->
///     io.println(parsetime_error.format_error(msg, source, line, col))
/// }
/// ```
pub fn parse(source: String) -> parse.ParseResult(Expr) {
  source
  |> parse.input_from_string
  |> parser()
}

pub fn parser() -> Parser(Expr) {
  use _ <- parse.get(skip_ws())
  use expr <- parse.get(expression())
  use _ <- parse.get(skip_ws())
  parse.return(expr)
}

// ----------------------------------------------------------------------------
// Whitespace handling
// ----------------------------------------------------------------------------

// Discard all whitespace, if there is any at all.
pub fn skip_ws() -> parse.Parser(Nil) {
  parse.whitespace()
  |> parse.or(parse.line_comment("//"))
  |> parse.or(parse.text_delimited("/*", "*/"))
  |> parse.star
  |> parse.map(fn(_) { Nil })
}

/// Recognize at least one piece of whitespace
pub fn some_ws() -> parse.Parser(Nil) {
  parse.whitespace()
  |> parse.or(parse.line_comment("//"))
  |> parse.or(parse.text_delimited("/*", "*/"))
  |> parse.plus
  |> parse.map(fn(_) { Nil })
}

/// Parse a token and consume/skip trailing whitespace & comments.
///
/// A lexeme is a meaningful unit of syntax (like a number, identifier, or operator)
/// followed by any whitespace. This is the standard approach to handling whitespace
/// in parsers: consume it after each token, not before.
///
/// ```gleam
/// // Parse "42" and skip any trailing spaces/tabs/newlines
/// lexeme(number())
/// ```
fn lexeme(parser: parse.Parser(a)) -> parse.Parser(a) {
  use result <- parse.get(parser)
  use _ <- parse.get(skip_ws())
  parse.return(result)
}

// ----------------------------------------------------------------------------
// Grammar rules
// ----------------------------------------------------------------------------

fn expression() -> parse.Parser(Expr) {
  assignment()
}

const get = parse.then

/// assignment     → IDENTIFIER "=" assignment | equality ;
fn assignment() -> parse.Parser(Expr) {
  {
    use #(name, span) <- get(parse.identifier() |> parse.with_span |> lexeme)
    use _ <- get(parse.string("=") |> lexeme)
    use expr <- get(assignment())
    parse.return(expr.Assign(name, expr, span))
  }
  |> parse.or(equality())
}

fn equality() -> parse.Parser(Expr) {
  binary_chain(comparison(), [#("!=", NotEquals), #("==", Equals)])
}

fn comparison() -> parse.Parser(Expr) {
  let ops = [
    #("<=", AtMost),
    #(">=", AtLeast),
    #("<", LessThan),
    #(">", GreaterThan),
  ]
  binary_chain(term(), ops)
}

fn term() -> parse.Parser(Expr) {
  binary_chain(factor(), [#("+", Plus), #("-", Minus)])
}

fn factor() -> parse.Parser(Expr) {
  binary_chain(unary(), [#("*", Times), #("/", Divides)])
}

/// Binary operator chain.
/// Implements: value (op value)*
fn binary_chain(
  value_parser: parse.Parser(Expr),
  ops: List(#(String, Operator)),
) -> parse.Parser(Expr) {
  parse.left_assoc(
    over: value_parser,
    operators: operator_parser(ops),
    or_error: "Expected a number, string, or expression after operator",
  )
  |> parse.map_with_span(binary_ops)
}

// Make an `Expr` out of `first op1 second op2 third op3 ...`
fn binary_ops(
  first_and_pairs: #(Expr, List(#(Operator, Expr))),
  span: parse.Span,
) -> Expr {
  let #(first, pairs) = first_and_pairs
  list.fold(pairs, first, fn(left, pair) {
    let #(op, right) = pair
    Op(op, [left, right], span)
  })
}

/// Operator parser that also detects missing operators between values
fn operator_parser(
  ops: List(#(String, a)),
) -> fn(parse.Input) -> parse.ParseResult(a) {
  let expected =
    ops
    |> list.map(fn(pair) { "`" <> pair.0 <> "`" })
    |> string.join(", ")

  parse.tokenize(ops)
  |> lexeme
  |> parse.or({
    use next <- parse.get(parse.peek())
    case unexpected_char(next) {
      False -> parse.error("")
      True ->
        parse.word()
        |> parse.then_with_span(fn(token, span) {
          parse.abort(
            "Expected one of " <> expected <> " before `" <> token <> "`",
          )
          |> parse.at(span)
        })
    }
  })
}

fn unexpected_char(c: String) -> Bool {
  parse.is_digit(c) || c == "\"" || parse.is_alphanumeric(c) || c == "("
}

/// `! /* hello world*/ true` ==> `Boolean(False)`
fn unary() -> parse.Parser(Expr) {
  [#("!", BooleanNegation), #("-", NumericNegation)]
  |> parse.tokenize
  |> lexeme
  |> parse.then_with_span(fn(op, span) {
    unary() |> parse.map(fn(inner) { Op(op, [inner], span) })
  })
  |> parse.or(primary())
}

fn primary() -> parse.Parser(Expr) {
  literal()
  |> parse.or(grouping())
  |> lexeme
}

fn literal() {
  parse.ordered_choice([
    parse.number() |> parse.map(Number),
    parse.string_literal() |> parse.map(String),
  ])
  |> parse.map_with_span(fn(s, span) { Literal(s, span) })
  |> parse.or(keyword_literal())
}

fn keyword_literal() -> Parser(Expr) {
  parse.identifier()
  |> parse.then_with_span(fn(id, span) {
    case id {
      "true" -> parse.return(Literal(Boolean(True), span:))
      "false" -> parse.return(Literal(Boolean(False), span:))
      "nil" -> parse.return(Literal(LitNil, span:))
      _ -> parse.return(Variable(name: id, span:))
    }
  })
}

fn grouping() -> parse.Parser(Expr) {
  {
    use #(_, open_span) <- parse.commit(parse.string("(") |> parse.with_span())
    use inner <- parse.require(expression(), "Expected expression after '('")
    use _ <- parse.require_at(
      expect: parse.string(")"),
      reprimand: "Hmm, looks like you forgot to close this parenthesis!",
      at: open_span,
    )
    parse.return(inner)
  }
  |> parse.map_with_span(fn(inner, span) { Grouping(inner, span) })
}
