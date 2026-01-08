import expr
import gleam/option.{type Option, None, Some}
import scanner.{type Token}

/// A parser really has two jobs:
///
/// Given a valid sequence of tokens, produce a corresponding syntax tree.
/// 
/// Given an invalid sequence of tokens, detect any errors and tell the user about their mistakes.
/// 
/// Don’t underestimate how important the second job is! In modern IDEs and editors, 
/// the parser is constantly reparsing code—often while the user is still editing it
/// —in order to syntax highlight and support things like auto-complete. 
/// That means it will encounter code in incomplete, half-wrong states all the time.
/// 
/// When the user doesn’t realize the syntax is wrong, it is up to the parser to 
/// help guide them back onto the right path. The way it reports errors is a large 
/// part of your language’s user interface.
/// 
/// While the source may not be valid code, it’s still a valid input to the 
/// parser because users use the parser to learn what syntax is allowed.
/// 
/// (1) Report as many distinct errors as there are. Aborting after the first error is easy to implement, 
/// but it’s annoying for users if every time they fix what they think is the one error in a file,
///  a new one appears. They want to see them all.
/// 
/// (2) Minimize cascaded errors. Once a single error is found, the parser no longer really knows what’s 
/// going on. It tries to get itself back on track and keep going, but if it gets confused, 
/// it may report a slew of ghost errors that don’t indicate other real problems in the code.
///  When the first error is fixed, those phantoms disappear, because they reflect only the 
/// parser’s own confusion. Cascaded errors are annoying because they can scare the user into 
/// thinking their code is in a worse state than it is.
/// 
/// The last two points are in tension. We want to report as many separate errors as we can, 
/// but we don’t want to report ones that are merely side effects of an earlier one.
/// 
/// The way a parser responds to an error and keeps going to look for later errors is called (syntax) error recovery. 
/// For example, in the panic mode strategy, when a parse encounters an error ---eg a missing closing quote for a string literal---
/// then it tries to discard the tokens for the string literal and keep parsing with the remaining tokens.
/// Usually, the amount of tokens it discards are determined by a grammar rule; so if the string literal appears in a statement rule,
/// we discard that rule and move to the next statement and continue parsing from there. 
/// This dropping tokens and moving on process is called synchronization.
///
pub type ParseResult(token, value) {
  Success(found: value, unconsumed: List(token))

  /// This reports an error at a given token. It shows the token’s location and the token itself. 
  /// This will come in handy later since we use tokens throughout the interpreter to track locations in code.
  /// 
  /// This is a simple sentinel class we use to unwind the parser. 
  /// The error() method returns the error instead of throwing it because we want to let the calling 
  /// method inside the parser decide whether to unwind or not. 
  /// Some parse errors occur in places where the parser isn’t likely to get into a weird state 
  /// and we don’t need to synchronize. In those places, we simply report the error and keep on truckin’.
  Error(
    // line: Int,
    // token: token,
    message: String,
  )
}

/// Synchronizing a recursive descent parser
/// 
/// With recursive descent, the parser’s state—which rules it is in the middle of recognizing
/// —is not stored explicitly in fields. Instead, we use Java’s own call stack to track what 
/// the parser is doing. Each rule in the middle of being parsed is a call frame on the stack. 
/// In order to reset that state, we need to clear out those call frames.
/// 
/// The natural way to do that in Java is exceptions. When we want to synchronize,
///  we throw that ParseError object. Higher up in the method for the grammar rule we are 
/// synchronizing to, we’ll catch it. Since we synchronize on statement boundaries, 
/// we’ll catch the exception there. After the exception is caught, the parser is in the right state. 
/// All that’s left is to synchronize the tokens.
/// 
/// We want to discard tokens until we’re right at the beginning of the next statement. 
/// That boundary is pretty easy to spot—it’s one of the main reasons we picked it. 
/// After a semicolon, we’re _probably_ finished with a statement. Most statements start with a 
/// keyword —for, if, return, var, etc. 
/// When the next token is any of those, we’re probably about to start a statement.
/// 
/// (I say “probably” because we could hit a semicolon separating clauses in a for loop. 
/// Our synchronization isn’t perfect, but that’s OK. We’ve already reported the first error precisely, 
/// so everything after that is kind of “best effort”.)
fn synchronize() {
  todo
}

/// 1. Get next token, if possible; else exit.
/// 2. If token is SEMICOLON, excellent we're at a boundary so exit.
/// 3. If we're at the start of a new expression, ie we see `class, fun, var, for, if, while, print, return`,
///    then exit since we have now dropped all tokens related to the previous parse error and are now safe to continue parsing again.
///    Otherwise, if we're not at the start of a new expression, then we're still part of the syntactically errenous expression, so
///    discard the current token: That is, do nothing with it and go to step 1.
/// 
/// It discards tokens until it thinks it has found a statement boundary. 
/// After catching a ParseError, we’ll call this and then we are hopefully back in sync. 
/// When it works well, we have discarded tokens that would have likely caused cascaded errors 
/// anyway, and now we can parse the rest of the file starting at the next statement.
pub type Parser(token, value) =
  fn(List(token)) -> ParseResult(token, value)

/// A Gleam representation of the following grammar.
/// ```
/// expression     → literal
///               | unary
///               | binary
///               | grouping ;
///
/// literal        → NUMBER | STRING | true | false | nil ;
/// grouping       → ( expression ) ;
/// unary          → ( - | ! ) expression ;
/// binary         → expression operator expression ;
/// operator       → == | != | < | <= | > | >=
///                | +  | -  | * | / ;
/// ```
/// 
/// 
pub fn expr() -> Parser(Token, expr.Expr) {
  literal()
  // |> or(binary_expr())
}

pub fn literal() -> Parser(Token, expr.Expr) {
  one_token()
  |> choose(
    token_as_expr_literal,
    "Expected a literal: Number , String , true , false , nil",
  )
  |> map(expr.Literal)
}

fn token_as_expr_literal(t: Token) -> Option(expr.Literal) {
  case t {
    scanner.Literal(lexeme, _) ->
      case lexeme {
        scanner.Number(value) -> value |> expr.Number |> Some
        scanner.String(value) -> value |> expr.String |> Some
        scanner.Identifer(_) -> None
      }
    scanner.Keyword(lexeme, _) ->
      case lexeme {
        scanner.LNil -> expr.Nil |> Some
        scanner.LTrue -> True |> expr.Boolean |> Some
        scanner.LFalse -> False |> expr.Boolean |> Some
        _ -> None
      }
    _ -> None
  }
}

pub fn binary_expr() -> Parser(Token, expr.Expr) {
  todo
}

pub fn binary_operator() -> Parser(Token, expr.BinaryOp) {
  one_token()
  |> choose(
    token_as_expr_binary_op,
    "Expected a binary operator:  == , != , < , <= , > , >= , +  , -  , * , /",
  )
}

fn token_as_expr_binary_op(t: Token) -> Option(expr.BinaryOp) {
  case t {
    scanner.Operator(lexeme, _) ->
      case lexeme {
        scanner.AtLeast -> Some(expr.AtLeast)
        scanner.AtMost -> Some(expr.AtMost)
        scanner.Division -> Some(expr.Divides)
        scanner.Equal -> Some(expr.Equals)
        scanner.GreaterThan -> Some(expr.GreaterThan)
        scanner.LessThan -> Some(expr.LessThan)
        scanner.Minus -> Some(expr.Minus)
        scanner.Plus -> Some(expr.Plus)
        scanner.Times -> Some(expr.Times)
        // Unary operators
        scanner.Negation -> None
        scanner.NotEqual -> None
      }
    _ -> None
  }
}

fn one_token() -> Parser(Token, Token) {
  fn(tokens) {
    case tokens {
      [t, ..ts] -> Success(found: t, unconsumed: ts)
      [] -> Error("No token to parse!")
    }
  }
}

fn filter(
  parser: Parser(token, value),
  predicate: fn(value) -> Bool,
  failure_message: String,
) -> Parser(token, value) {
  fn(tokens) {
    case parser(tokens) {
      err -> err
      Success(found:, unconsumed:) as done ->
        case predicate(found) {
          True -> done
          False -> Error(failure_message)
        }
    }
  }
}

/// Applies a partial map to the result of a parser.
/// This is essentially both a `filter` and `map`.
fn choose(
  parser: Parser(token, value_a),
  selector: fn(value_a) -> Option(value_b),
  failure_message: String,
) -> Parser(token, value_b) {
  fn(tokens) {
    case parser(tokens) {
      Error(msg) -> Error(msg)
      Success(found:, unconsumed:) ->
        case selector(found) {
          Some(b) -> Success(b, unconsumed)
          None -> Error(failure_message)
        }
    }
  }
}

fn map(
  parser: Parser(token, value_a),
  mapper: fn(value_a) -> value_b,
) -> Parser(token, value_b) {
  fn(tokens) {
    case parser(tokens) {
      Error(msg) -> Error(msg)
      Success(found, unconsumed) -> Success(mapper(found), unconsumed)
    }
  }
}

/// Run the `main` parser but if it fails then run the `fallback` parser.
fn or(main: Parser(a, b), fallback: Parser(a, b)) -> Parser(a, b) {
  fn(tokens) {
    case main(tokens) {
      // TODO: We probably want to aggregate errors!
      Error(_) -> fallback(tokens)
      success -> success
    }
  }
}
