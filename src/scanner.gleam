/// This is a nifty example of using monadic parsers for lexers, but otherwise is no longer used in this project 😅

/// The "scanner" module is concerned with converting a stream of characters into a semantic list of "tokens".

import expr
import gleam/list
import parser_combinators.{
  type ParseResult, type Parser, Error, Span, Success, error, identifier,
  input_from_string, line_comment, map, number, or, return, star, string_literal,
  text_delimited, then, tokenize, whitespace,
}

// The first step in any compiler or interpreter is scanning. The scanner takes in raw source code as a series of characters and groups it into a series of chunks we call tokens. These are the meaningful “words” and “punctuation” that make up the language’s grammar.

// Scanning is a good starting point for us too because the code isn’t very hard—pretty much a switch statement with delusions of grandeur. It will help us warm up before we tackle some of the more interesting material later. By the end of this chapter, we’ll have a full-featured, fast scanner that can take any string of Lox source code and produce the tokens that we’ll feed into the parser in the next chapter.

// The phrase `var language = "gleam`";` has the sequence of meaningful lexemes ["var", "language", "=", "\"gleam\"", ";"].
// Moreoever, each lexeme has additional useful info; e.g., "var" is a lexme that happens to be a reserved keyword, whereas "language" is an identifier.
// So let's create a type to capture a lexeme and additionally what kind it is.
pub type Token {
  Literal(expr.Literal)
  Identifier(String)
  // Operators 
  Plus
  Minus
  Times
  Division
  Negation
  Equal
  NotEqual
  LessThan
  GreaterThan
  AtMost
  AtLeast
  // Punctuation
  LeftParen
  RightParen
  LeftBrace
  RightBrace
  Comma
  Dot
  Semicolon
  Assignment
  Comment
  Whitespace
  EOF
  // Keywords
  LVar
  LAnd
  LOr
  LIf
  LElse
  LWhile
  LFor
  LFun
  LReturn
  LClass
  LThis
  LSuper
  LPrint
}

// Now that we know what we’re trying to produce, let’s, well, produce it. The core of the scanner is a loop. 
// Starting at the first character of the source code, the scanner figures out what lexeme the character belongs to, 
// and consumes it and any following characters that are part of that lexeme. When it reaches the end of that lexeme, it emits a token.

// Then it loops back and does it again, starting from the very next character in the source code. It keeps doing that,
// eating characters and occasionally, uh, excreting tokens, until it reaches the end of the input.

// The rules that determine how a particular language groups characters into lexemes are called its **lexical grammar**.

// pub type Scanner {
//  Scanner(source: String, cursor: Cursor, tokens: List(Token))
// }

/// Denotes where are we currently looking in the source code.
/// start - points to the start of the first character of the lexeme being scanned
/// current - points to the character currently being considered
/// line - points to the line that contains `current`; useful for Token location information
// pub type Cursor {
//  Cursor(start: Int, current: Int, line: Int)
// }

/// Recognizing Lexemes <-- 🤔 We build this up incrementally, either via unit tests or via the Lox Repl viz glox.gleam
///
/// In each turn of the loop, we scan a single token. This is the real heart of the scanner. We'll start simple.
/// Imagine if every lexeme were only a single character long. All you would need to do is consume the next
/// character and pick a token type for it. Several lexemes are only a single character in Lox, so let's start with those.
///
/// Lexical errors: Before we get too far in, let's take a moment to think about errors at the lexical level.
/// What happens if a user throws a source file containing some characters Lox doesn't use, like @#^, at our interpreter?
/// Right now, those characters get silently discarded. They aren't used by the Lox language, but that doesn't mean the
/// interpreter can pretend they aren't there. Instead, we report an error.
///
pub fn scan_tokens(source: String) -> ParseResult(List(Token)) {
  let input = input_from_string(source)
  case star(scan_lexeme())(input) {
    Error(msg, span, committed) -> Error(msg, span, committed)
    Success(tokens, remaining) ->
      case remaining.unconsumed {
        "" ->
          Success(tokens |> filter_whitespace |> list.append([EOF]), remaining)
        _ ->
          Error(
            "Unexpected input remaining",
            Span(remaining.line, remaining.column, 1),
            False,
          )
      }
  }
}

fn filter_whitespace(tokens: List(Token)) -> List(Token) {
  list.filter(tokens, fn(t) { t != Whitespace })
}

/// Scan a single lexeme from input.
/// Returns a Parser that produces a Token.
fn scan_lexeme() -> Parser(Token) {
  // Combine all parsers with appropriate precedence
  { whitespace() |> map(fn(_) { Whitespace }) }
  |> or(comment_token())
  |> or(punctuation_and_operators())
  |> or(literal_token())
  |> or(identifier_or_keyword_token())
  |> or(error("Unexpected character"))
}

/// Parses line comments (`//...`) and block comments (`/* ... */`).
fn comment_token() -> Parser(Token) {
  line_comment("//")
  |> or(text_delimited("/*", "*/"))
  |> map(fn(_) { Comment })
}

fn punctuation_and_operators() {
  tokenize([
    // Multi-char operators (maximal munch)
    #("!=", NotEqual),
    #("==", Equal),
    #("<=", AtMost),
    #(">=", AtLeast),
    // Single-char operators
    #("!", Negation),
    #("=", Assignment),
    #("<", LessThan),
    #(">", GreaterThan),
    #("*", Times),
    #("+", Plus),
    #("-", Minus),
    #("/", Division),
    // Punctuation
    #("(", LeftParen),
    #(")", RightParen),
    #("{", LeftBrace),
    #("}", RightBrace),
    #(",", Comma),
    #(".", Dot),
    #(";", Semicolon),
  ])
}

/// Parses string and number literals.
fn literal_token() -> Parser(Token) {
  string_literal()
  |> map(fn(s) { Literal(expr.String(s)) })
  |> or(number() |> map(fn(n) { Literal(expr.Number(n)) }))
}

/// Parses identifiers and reserved keywords (including literal keywords like `nil`, `true`, `false`).
fn identifier_or_keyword_token() -> Parser(Token) {
  use id <- then(identifier())
  return(case id {
    "nil" -> Literal(expr.Nil)
    "true" -> Literal(expr.Boolean(True))
    "false" -> Literal(expr.Boolean(False))
    "var" -> LVar
    "and" -> LAnd
    "or" -> LOr
    "if" -> LIf
    "else" -> LElse
    "while" -> LWhile
    "for" -> LFor
    "fun" -> LFun
    "return" -> LReturn
    "class" -> LClass
    "this" -> LThis
    "super" -> LSuper
    "print" -> LPrint
    _ -> Identifier(id)
  })
}
