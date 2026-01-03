// The "scanner" module is concerned with converting a stream of characters into a semantic list of "tokens".

import error_handling.{type LError, LError}
import gleam/bool
import gleam/list
import gleam/pair

// Get type & constructor
import gleam/float
import gleam/int
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

// The first step in any compiler or interpreter is scanning. The scanner takes in raw source code as a series of characters and groups it into a series of chunks we call tokens. These are the meaningful “words” and “punctuation” that make up the language’s grammar.

// Scanning is a good starting point for us too because the code isn’t very hard—pretty much a switch statement with delusions of grandeur. It will help us warm up before we tackle some of the more interesting material later. By the end of this chapter, we’ll have a full-featured, fast scanner that can take any string of Lox source code and produce the tokens that we’ll feed into the parser in the next chapter.

// The phrase `var language = "gleam`";` has the sequence of meaningful lexemes ["var", "language", "=", "\"gleam\"", ";"].
// Moreoever, each lexeme has additional useful info; e.g., "var" is a lexme that happens to be a reserved keyword, whereas "language" is an identifier.
// So let's create a type to capture a lexeme and additionally what kind it is.
pub type Token {
  Keyword(lexeme: Keyword, location: Location)
  Literal(lexeme: Literal, location: Location)
  Punctuation(lexeme: Punctuation, location: Location)
  Operator(lexeme: Operator, location: Location)
}

/// For now, just keep track of the line number that a lexme occurs on.
pub type Location =
  Int

/// Everything is prefixed with `L` (for "Lox") to avoid conflicts with Gleam constructors; eg LTrue is for Lox, whereas True is a Gleam Boolean.
pub type Keyword {
  LVar
  LTrue
  LFalse
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
  LNil
  LPrint
}

pub type Literal {
  Identifer(value: String)
  String(value: String)
  Number(value: Float)
}

pub type Punctuation {
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
  // This exists to make the parser easier to implement, like everything else lol
}

pub type Operator {
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
/// In each turn of the loop, we scan a single token. This is the real heart of the scanner. We’ll start simple. 
/// Imagine if every lexeme were only a single character long. All you would need to do is consume the next
/// character and pick a token type for it. Several lexemes are only a single character in Lox, so let’s start with those.
/// 
/// Lexical errors: Before we get too far in, let’s take a moment to think about errors at the lexical level. 
/// What happens if a user throws a source file containing some characters Lox doesn’t use, like @#^, at our interpreter? 
/// Right now, those characters get silently discarded. They aren’t used by the Lox language, but that doesn’t mean the 
/// interpreter can pretend they aren’t there. Instead, we report an error.
/// 
pub fn scan_tokens(source: String, line: Int) -> Result(List(Token), LError) {
  use step <- result.try(scan_lexeme(source, line))
  let token = step.found
  let unconsumed_source = step.unconsumed
  use tokens <- result.map(case unconsumed_source {
    "" -> Ok([])
    _ -> scan_tokens(unconsumed_source, line)
  })
  [token, ..tokens]
}

/// Denotes the result of parsing a single item of type `t`
pub type Step(t) {
  Step(found: t, unconsumed: String)
}

/// text - start of substring being inspected
fn scan_lexeme(text: String, line: Int) -> Result(Step(Token), LError) {
  case text {
    // Either success, or else an error: Unknown char
    "" -> Ok(Step(Punctuation(EOF, line), ""))
    // Whitespace
    " " <> more -> Ok(Step(Punctuation(Whitespace, line), more))
    "\t" <> more -> Ok(Step(Punctuation(Whitespace, line), more))
    "\r" <> more -> Ok(Step(Punctuation(Whitespace, line), more))
    "\n" <> more -> Ok(Step(Punctuation(Whitespace, line), more))
    // Simple single chars
    "(" <> more -> Ok(Step(Punctuation(LeftParen, line), more))
    ")" <> more -> Ok(Step(Punctuation(RightParen, line), more))
    "{" <> more -> Ok(Step(Punctuation(LeftBrace, line), more))
    "}" <> more -> Ok(Step(Punctuation(RightBrace, line), more))
    "," <> more -> Ok(Step(Punctuation(Comma, line), more))
    "." <> more -> Ok(Step(Punctuation(Dot, line), more))
    ";" <> more -> Ok(Step(Punctuation(Semicolon, line), more))
    // We have single-character lexemes working, but that doesn’t cover all of Lox’s operators. 
    // What about !? It’s a single character, right? Sometimes, yes, but if the very next character is an equals sign, 
    // then we should instead create a != lexeme. Note that the ! and = are not two independent operators. 
    // You can’t write ! = in Lox and have it behave like an inequality operator. That’s why we need to scan != as a single lexeme. 
    // Likewise, <, >, and = can all be followed by = to create the other equality and comparison operators.
    // 
    // 🌻 Maxmial Munch Rule: When two lexical grammar rules can both match a chunk of code that the scanner is looking at, 
    // whichever one matches the most characters wins. E.g., `<=` should be scanned as a single `<=` token and not `<` followed by `=`;
    // likewise `orchid` should be scanned as the single identifier `orchid` and not as the reserved keyword `or` followed by the identifier `chid`.
    "!=" <> more -> Ok(Step(Operator(NotEqual, line), more))
    "!" <> more -> Ok(Step(Operator(Negation, line), more))
    "==" <> more -> Ok(Step(Operator(Equal, line), more))
    "=" <> more -> Ok(Step(Punctuation(Assignment, line), more))
    "<=" <> more -> Ok(Step(Operator(AtMost, line), more))
    "<" <> more -> Ok(Step(Operator(LessThan, line), more))
    ">=" <> more -> Ok(Step(Operator(AtLeast, line), more))
    ">" <> more -> Ok(Step(Operator(GreaterThan, line), more))
    "//" <> more ->
      case string.contains(more, "\n") {
        // comment on final line:
        False -> Ok(Step(Punctuation(Comment, line), ""))
        True -> {
          let assert Ok(#(_comment, more2)) = string.split_once(more, on: "\n")
          Ok(Step(Punctuation(Comment, line), more2))
        }
      }
    "/" <> more -> Ok(Step(Operator(Division, line), more))
    "*" <> more -> Ok(Step(Operator(Times, line), more))
    "+" <> more -> Ok(Step(Operator(Plus, line), more))
    "-" <> more -> Ok(Step(Operator(Minus, line), more))
    "\"" <> more ->
      case string.contains(more, "\"") {
        False -> Error(LError("Unterminated string", line))
        True -> {
          let assert Ok(#(string, more2)) = string.split_once(more, on: "\"")
          Ok(Step(Literal(String(string), line), more2))
        }
      }
    more ->
      more
      // Do we have a number?
      |> {
        parse_number()
        |> map(fn(number) { Literal(Number(number), line) })
      }
      // Or, do we have an identifier, which might be reserved as a keyword of the language.
      |> option.lazy_or(fn() {
        use Step(identifier, more2) <- option.then(parse_identifier()(more))
        {
          as_reserved_keyword(identifier)
          |> option.map(Keyword(_, line))
          |> option.lazy_or(fn() { Some(Literal(Identifer(identifier), line)) })
          |> option.map(Step(_, more2))
        }
      })
      |> option.to_result(LError("Unexpected character", line))
  }
}

/// Realize a string `x <> y`, where `x` is a number and `y` does not start with a digit, 
/// get `x` as a `Float` along with the unconsumed input `y`; if possible. 
/// ### Example: `"1.23and more"` is split into `Some(#(1.23, "and more"))`.
pub fn parse_number() -> Parser(Float) {
  parse_one_char
  |> such_that(fn(char, unconsumed) {
    let is_not_trailing_dot = !{ char == "." && string.is_empty(unconsumed) }
    let is_digit_or_decimal_point = char == "." || is_digit(char)
    is_not_trailing_dot && is_digit_or_decimal_point
  })
  |> star
  |> map(string.join(_, ""))
  // At this point we have parsed the longest possible numeric prefix as a string,
  // it remains to convert it to a float.
  |> then(fn(numeric) {
    numeric
    |> float.parse
    |> result.lazy_or(fn() { numeric |> int.parse |> result.map(int.to_float) })
    |> option.from_result
  })
}

/// Splits a string into `x <> y` where `x` is the largest alphanumberic prefix of the string.
/// ### Example: `"orchid+123"` is parsed into `#("orchid", "+123")`.
pub fn parse_identifier() -> Parser(String) {
  parse_one_char
  |> filter(is_alphanumeric)
  |> star
  |> map(string.join(_, ""))
}

type Parser(t) =
  fn(String) -> ParseResult(t)

type ParseResult(t) =
  Option(Step(t))

/// Try to parse a single character; e.g., `"hello"` maps to `#("h", "ello")`.
fn parse_one_char(str: String) -> ParseResult(String) {
  str
  |> string.pop_grapheme
  |> option.from_result
  |> option.map(fn(p) { Step(p.0, p.1) })
}

/// Parse using `parser` then apply `mapper` to the resulting parsed value
fn map(parser: Parser(a), mapper: fn(a) -> b) -> Parser(b) {
  fn(str) {
    use Step(a, unconsumed) <- option.then(parser(str))
    Some(Step(mapper(a), unconsumed))
  }
}

/// Parse using `parser` then apply `mapper` to the resulting parsed value
fn then(parser: Parser(a), mapper: fn(a) -> Option(b)) -> Parser(b) {
  fn(str) {
    use Step(a, unconsumed) <- option.then(parser(str))
    use b <- option.then(mapper(a))
    Some(Step(b, unconsumed))
  }
}

/// Parse using `parser` but only succeed if `predicate` is holds for the parsed value
fn filter(parser, predicate) -> Parser(t) {
  // Equivalently: such_that(parser, fn(t, _unconsumed) { predicate(t) })
  fn(str) {
    use Step(t, _) as step <- option.then(parser(str))
    use <- bool.guard(when: !predicate(t), return: None)
    Some(step)
  }
}

/// Parse using `parser` but only succeed if `predicate` is holds for the parsed value & unconsumed input.
/// This is like `filter` but the given predicate has access to both the parsed value & unconsumed input.
fn such_that(parser, relation) -> Parser(t) {
  fn(str) {
    use Step(t, unconsumed) as step <- option.then(parser(str))
    use <- bool.guard(when: !relation(t, unconsumed), return: None)
    Some(step)
  }
}

/// Parses the largest prefix of a string that is parserable by `parser`
fn star(parser: Parser(t)) -> Parser(List(t)) {
  fn(str) {
    use Step(t, unconsumed) <- option.then(parser(str))
    unconsumed
    |> star(parser)
    |> option.map(fn(step) { Step([t, ..step.found], step.unconsumed) })
    |> option.lazy_or(fn() { Some(Step([t], unconsumed)) })
  }
}

fn is_alphanumeric(c: String) -> Bool {
  c == "_" || is_digit(c) || is_letter(c)
}

fn is_digit(c: String) -> Bool {
  string.length(c) == 1 && { c |> int.parse |> result.is_ok }
}

fn is_letter(c: String) -> Bool {
  // Note: "A" < "Z" < "a" < "z"
  string.length(c) == 1 && str_at_most("A", c) && str_at_most(c, "z")
}

/// Check if `l <= r` for strings `l` and `r`
fn str_at_most(l, r) -> Bool {
  list.contains([order.Lt, order.Eq], string.compare(l, r))
}

fn as_reserved_keyword(str) -> Option(Keyword) {
  case str {
    "var" -> Some(LVar)
    "true" -> Some(LTrue)
    "false" -> Some(LFalse)
    "and" -> Some(LAnd)
    "or" -> Some(LOr)
    "if" -> Some(LIf)
    "else" -> Some(LElse)
    "while" -> Some(LWhile)
    "for" -> Some(LFor)
    "fun" -> Some(LFun)
    "return" -> Some(LReturn)
    "class" -> Some(LClass)
    "this" -> Some(LThis)
    "super" -> Some(LSuper)
    "nil" -> Some(LNil)
    "print" -> Some(LPrint)
    _ -> None
  }
}
