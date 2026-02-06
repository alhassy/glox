import gleam
import gleam/float
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

/// Position-aware parsing with detailed error messages
///
/// + It is surprising that only one combinator needs to be aware of the position, namely `pop`.
/// + Neato!
// ================================

/// Source position information for error reporting and AST annotation; `length` is the length of the parsed fragment.
pub type Span {
  Span(line: Int, column: Int, length: Int)
}

/// Input with position tracking for detailed error messages.
pub type Input {
  Input(
    /// Remaining input to parse
    unconsumed: String,
    /// Current line number (1-indexed)
    line: Int,
    /// Current column number (1-indexed)
    column: Int,
    /// Original source (for error display)
    source: String,
  )
}

/// Create input from a source string.
pub fn input_from_string(source: String) -> Input {
  Input(unconsumed: source, line: 1, column: 1, source:)
}

/// Create a span at the current input position with length 1.
fn span_here(input: Input) -> Span {
  Span(input.line, input.column, 1)
}

/// Advance after consuming a character (handles newlines automatically).
pub fn advance(input: Input, consumed: String, remaining: String) -> Input {
  case consumed {
    "\n" ->
      Input(..input, unconsumed: remaining, line: input.line + 1, column: 1)
    _ -> Input(..input, unconsumed: remaining, column: input.column + 1)
  }
}

/// Result of parsing - errors include location.
pub type ParseResult(value) {
  Success(found: value, remaining: Input)
  Error(message: String, at: Span, committed: Bool)
}

/// A parser that tracks position.
///
/// A parser really has two jobs:
///
/// 1. Given valid input, produce a corresponding syntax tree.
/// 2. Given invalid input, detect errors and tell the user about their mistakes.
///
/// Don't underestimate how important the second job is! In modern IDEs and editors,
/// the parser is constantly reparsing code—often while the user is still editing it.
/// That means it will encounter code in incomplete, half-wrong states all the time.
pub type Parser(value) =
  fn(Input) -> ParseResult(value)

// ============================================================================
// Core Combinators
// ============================================================================

/// Consume and return one grapheme, updating position (handles newlines).
/// 
/// “pop” resembles the “.” regex: It's any single (character, token) item.
pub fn pop() -> Parser(String) {
  fn(input: Input) {
    case string.pop_grapheme(input.unconsumed) {
      Ok(#(c, rest)) -> Success(c, input |> advance(c, rest))
      gleam.Error(_) -> error("Unexpected end of input")(input)
    }
  }
}

/// Peek at the next grapheme without consuming.
///
/// ## When to use `peek` vs `negative_lookahead`
///
/// - Use `peek()` when you need to look at the next character and make decisions based on its value
/// - Use `negative_lookahead(p)` when you need to ensure a multi-character pattern is NOT present
pub fn peek() -> Parser(String) {
  fn(input: Input) {
    case string.first(input.unconsumed) {
      Ok(c) -> Success(c, input)
      gleam.Error(_) -> error("Unexpected end of input")(input)
    }
  }
}

/// Succeeds only at end of input; otherwise aborts.
pub fn eof() -> Parser(Nil) {
  use input <- get(inspect_input(fn(i) { i }))
  case input.unconsumed {
    "" -> return(Nil)
    _ ->
      abort("Unexpected input after expression")
      |> at(Span(input.line, input.column, string.length(input.unconsumed)))
  }
}

/// Negative lookahead: succeeds if the given parser would FAIL, fails if it would SUCCEED.
/// Never consumes any input — just peeks ahead.
///
/// This is useful for parsing "everything until X" patterns, where you want to
/// consume characters one at a time as long as you're NOT looking at X.
///
/// In regex, this is equivalent to `(?!pattern)`.
///
/// ## When to use `negative_lookahead` vs `peek`
///
/// - Use `peek()` when you need to look at the next character and make decisions based on its value
/// - Use `negative_lookahead(p)` when you need to ensure a multi-character pattern is NOT present
///
/// ## Examples
///
/// Parse characters until we see `*/` (block comment).
/// This mimics the imperative pattern `while not looking at end: get next char`:
/// ```gleam
/// star({
///   use _ <- get(negative_lookahead(string("*/")))
///   pop()
/// })
/// ```
///
/// Parse an identifier that is NOT a keyword:
/// ```gleam
/// use _ <- get(negative_lookahead(string("if")))
/// use _ <- get(negative_lookahead(string("else")))
/// identifier()
/// ```
pub fn negative_lookahead(parser: Parser(a)) -> Parser(Nil) {
  fn(input: Input) {
    case parser(input) {
      Success(..) -> Error("unexpected match", span_here(input), False)
      Error(..) -> Success(Nil, input)
    }
  }
}

/// Parser that always succeeds with the given value.
pub fn return(value: a) -> Parser(a) {
  fn(input) { Success(value, input) }
}

/// Returns the current position (line, column) without consuming input.
///
/// Use this to capture position information for building spans while staying
/// within the monadic parser interface, rather than directly accessing Input:
///
/// ```gleam
/// // Instead of:
/// fn my_parser() -> Parser(MyExpr) {
///   fn(input: Input) {
///     let start_line = input.line
///     let start_col = input.column
///     let parser = { ... }
///     parser(input)
///   }
/// }
///
/// // Prefer:
/// fn my_parser() -> Parser(MyExpr) {
///   use #(start_line, start_col) <- get(position())
///   ...
/// }
/// ```
pub fn position() -> Parser(#(Int, Int)) {
  inspect_input(fn(input) { #(input.line, input.column) })
}

/// Inspect the input state without consuming anything.
///
/// Useful for making decisions based on input state (e.g., checking for EOF):
/// ```gleam
/// use at_eof <- get(inspect_input(fn(input) { input.unconsumed == "" }))
/// case at_eof {
///   True -> abort("Missing operand") |> at(op_span)
///   False -> abort("Missing operand")
/// }
/// ```
pub fn inspect_input(f: fn(Input) -> a) -> Parser(a) {
  fn(input) { Success(f(input), input) }
}

/// Parse a value and return it along with its source span.
///
/// This simplifies parsers that need to track the span of what they consume,
/// avoiding manual position tracking:
///
/// ```gleam
/// // Instead of:
/// fn string_lit() -> Parser(Expr) {
///   use #(line, col) <- get(position())
///   use s <- get(string_literal())
///   let span = Span(line, col, string.length(s) + 2)  // manual length calc!
///   return(Literal(String(s), span))
/// }
///
/// // Prefer:
/// fn string_lit() -> Parser(Expr) {
///   use #(s, span) <- get(with_span(string_literal()))
///   return(Literal(String(s), span))
/// }
/// ```
pub fn with_span(parser: Parser(a)) -> Parser(#(a, Span)) {
  use #(start_line, start_col) <- get(position())
  use value <- get(parser)
  use #(_, end_col) <- get(position())
  return(#(value, Span(start_line, start_col, int.max(1, end_col - start_col))))
}

/// Parse a value and transform it along with its span.
///
/// Combines `with_span` and `map` for convenience:
/// ```gleam
/// // Instead of:
/// use #(s, span) <- get(string_literal() |> with_span())
/// return(Literal(String(s), span))
///
/// // Prefer:
/// string_literal() |> map_with_span(fn(s, span) { Literal(String(s), span) })
/// ```
pub fn map_with_span(parser: Parser(a), f: fn(a, Span) -> b) -> Parser(b) {
  use #(value, span) <- get(with_span(parser))
  return(f(value, span))
}

/// Like `map_with_span`, but the function returns a `Parser` for further chaining.
///
/// Useful when the transformation can fail or needs to branch:
/// ```gleam
/// identifier()
/// |> then_with_span(fn(id, span) {
///   case id {
///     "true" -> return(Literal(Boolean(True), span))
///     _ -> abort("Unknown identifier") |> at(span)
///   }
/// })
/// ```
pub fn then_with_span(
  parser: Parser(a),
  f: fn(a, Span) -> Parser(b),
) -> Parser(b) {
  use #(value, span) <- get(with_span(parser))
  f(value, span)
}

/// Parser that always fails with an uncommitted error.
pub fn error(message: String) -> Parser(a) {
  fn(input: Input) { Error(message, span_here(input), committed: False) }
}

/// Abort with a committed error (prevents backtracking).
///
/// Use this when you've detected a real syntax error that must be reported;
/// e.g., you wrote a double-quote but did not terminate it, then we should abort
/// instead of trying another parse rule.
pub fn abort(message: String) -> Parser(a) {
  // No backtracking via any ambient `or`! 😁
  use <- cut()
  // No continuing via any ambient `then`! 😁
  error(message)
}

/// Transform the error fields (message, span, committed) if parser fails.
fn map_error(
  parser: Parser(a),
  transform: fn(String, Span, Bool) -> #(String, Span, Bool),
) -> Parser(a) {
  fn(input) {
    case parser(input) {
      Error(msg, span, committed) -> {
        let #(msg2, span2, committed2) = transform(msg, span, committed)
        Error(msg2, span2, committed2)
      }
      success -> success
    }
  }
}

/// Override the error span, preserving message and committed status.
pub fn at(parser: Parser(a), span: Span) -> Parser(a) {
  parser |> map_error(fn(msg, _, committed) { #(msg, span, committed) })
}

/// Sequence two parsers: run first, then continuation with result.
/// 
/// I personally like to alias `then` as `get` in the local modules that import the
/// parser; I find `get` to be rather suggestive and makes the resulting functional
/// code look surprisngly imperative!
pub fn then(parser: Parser(a), continuation: fn(a) -> Parser(b)) -> Parser(b) {
  fn(input) {
    case parser(input) {
      Error(msg, span, committed) -> Error(msg, span, committed)
      Success(value, remaining) -> continuation(value)(remaining)
    }
  }
}

/// Alias for `then`, for use with `use` syntax.
pub fn get(parser, continuation) {
  then(parser, continuation)
}

/// Try main parser, fall back to alternative if uncommitted error.
///
/// **Commit semantics**: If `main` returns `Error(..., committed: True)`, we've committed
/// to that parse path and will NOT try `fallback`. Similarly, if fallback returns a
/// committed error, we propagate only that error (no concatenation).
///
/// **Require pattern**: Use `parser |> or(abort(...))` to require a parser to succeed,
/// aborting with no backtracking if it fails. This commits to the error, preventing
/// future `or` calls from trying alternatives.
/// 
/// ### Properties of `or`
/// ```
/// |                  Parsers                   |        Arithmetical Analogy         |
/// |--------------------------------------------+-------------------------------------|
/// |                   p or q                   |                a + b                |
/// |               error or q = q               |              0 + b = b              |
/// |             abort or q = abort             |              ∞ + b = ∞              |
/// |          p or q   ≠    q or p         (!!) |          a + b   =   b + a          |
/// | (p or q) then r = (p then r) or (q then r) | (a + b) × c   =   (a × c) + (b × c) |
/// |                                            |                                     |
/// ```
pub fn or(main: Parser(a), fallback: Parser(a)) -> Parser(a) {
  fn(input) {
    case main(input) {
      Success(..) as ok -> ok
      Error(committed: True, ..) as committed -> committed
      Error(msg1, _, False) ->
        case fallback(input) {
          Success(..) as ok -> ok
          Error(committed: True, ..) as committed -> committed
          Error(msg2, span, False) -> Error(msg2 <> " \n " <> msg1, span, False)
        }
    }
  }
}

/// Try parsers in order until one succeeds.
///
/// This is the classic "ordered choice" from PEG grammars. Each parser is tried
/// in sequence; the first successful parse wins. If a parser commits (e.g., after
/// seeing a distinctive token), later alternatives are not tried.
///
/// Use this when you have multiple alternatives at a grammar choice point:
/// ```gleam
/// // Instead of:
/// number()
/// |> or(string_lit())
/// |> or(keyword_literal())
/// |> or(grouping())
///
/// // Prefer:
/// ordered_choice([number(), string_lit(), keyword_literal(), grouping()])
/// ```
///
/// The list should be non-empty. An empty list always fails.
pub fn ordered_choice(parsers: List(Parser(a))) -> Parser(a) {
  case parsers {
    [] -> error("No alternatives in ordered_choice")
    [first, ..rest] -> list.fold(rest, first, or)
  }
}

/// Transform the result of a parser.
pub fn map(parser: Parser(a), mapper: fn(a) -> b) -> Parser(b) {
  fn(input) {
    case parser(input) {
      Error(msg, span, committed) -> Error(msg, span, committed)
      Success(value, remaining) -> Success(mapper(value), remaining)
    }
  }
}

/// Filter parser results, failing if predicate returns False.
fn filter(
  parser: Parser(a),
  predicate: fn(a) -> Bool,
  message: String,
) -> Parser(a) {
  fn(input) {
    case parser(input) {
      Error(..) as err -> err
      Success(value, ..) as success ->
        case predicate(value) {
          True -> success
          False -> error(message)(input)
        }
    }
  }
}

/// Optionally parse something. Returns `Some(value)` if parser succeeds, `None` otherwise.
///
/// Always succeeds — either with the parsed value wrapped in `Some`, or with `None`.
///
/// Example uses
/// + Trailing tokens like commas or newlines
/// + Optional parts of a grammar, eg variable assignments with optional initializer: `var <identifier> (= <expr>)?`.
/// 
/// This combinator corresponds to the `?` regex modifier.
/// 
/// ## Example
///
/// Optionally consume a trailing newline:
/// ```gleam
/// use _ <- get(maybe(string("\n")))
/// ```
///
/// Capture an optional prefix:
/// ```gleam
/// use prefix <- get(maybe(string("0x")))
/// case prefix {
///   Some(_) -> parse_hex_number()
///   None -> parse_decimal_number()
/// }
/// ```
pub fn maybe(parser: Parser(a)) -> Parser(Option(a)) {
  parser |> map(Some) |> or(return(None))
}

/// Parse zero or more occurrences.
pub fn star(parser: Parser(a)) -> Parser(List(a)) {
  {
    use value <- get(parser)
    use rest <- get(parser |> star)
    return([value, ..rest])
  }
  |> or(return([]))
}

/// Parse one or more occurrences.
fn plus(parser: Parser(a)) -> Parser(List(a)) {
  use first <- get(parser)
  use rest <- get(parser |> star)
  return([first, ..rest])
}

/// Parse a left-associative chain: `operand (operator operand)*`
///
/// After successfully parsing an operator, commits to requiring the next operand.
/// This is the classic pattern for parsing binary expressions like arithmetic:
///
/// ```gleam
/// // Parse: term (('+' | '-') term)*
/// left_assoc(
///   over: term(),
///   operators: string_map([("+", Plus), ("-", Minus)]) |> lexeme,
///   or_error: "Expected expression after operator",
/// )
/// |> map(fn(#(first, pairs)) {
///   list.fold(pairs, first, fn(left, #(op, right)) { Binary(op, left, right) })
/// })
/// ```
///
/// Returns the first operand and a list of (operator, operand) pairs, allowing
/// the caller to fold them into whatever AST structure they need.
pub fn left_assoc(
  over operand: Parser(a),
  operators operator: Parser(op),
  or_error missing_operand_message: String,
) -> Parser(#(a, List(#(op, a)))) {
  use first <- get(operand)
  use pairs <- get(
    {
      use #(op, op_span) <- commit(operator |> with_span)
      use error_span <- get(
        inspect_input(fn(input) {
          let at_eof = input.unconsumed == ""
          case at_eof {
            True -> op_span
            False -> span_here(input)
          }
        }),
      )
      use right <- require_at(
        expect: operand,
        reprimand: missing_operand_message,
        at: error_span,
      )
      return(#(op, right))
    }
    |> star,
  )
  return(#(first, pairs))
}

/// Commit to this parse path after the parser succeeds (like Prolog's `!` cut).
///
/// Once `parser` succeeds, any errors from the `continuation` are marked as
/// committed, preventing backtracking to try alternative parses.
///
/// ```gleam
/// // After seeing '(', we're committed to parsing a grouping expression
/// use _ <- commit(string("("))
/// use expr <- require(expression(), "Expected expression")
/// use _ <- require(string(")"), "Unclosed parenthesis")
/// return(Grouping(expr))
/// ```
///
/// This is analogous to Prolog's cut operator `!`, which prunes the search tree.
/// In parsing terms: once we've seen enough to know which grammar rule applies,
/// we commit to it rather than backtracking to try other rules on failure.
///
/// Note: This combinator requires access to "the rest of the computation" to mark
/// subsequent errors as committed. This is naturally expressed via Gleam's `use`
/// syntax (or Haskell's monadic `do`/`>>=`), which reifies the continuation.
/// 
/// ## Motivation for introducing this combinator :: Error Tracking
/// 
/// /// **Semantics**:
/// 1. If `parser` fails → propagate error as-is (not committed yet, we never entered this path)
/// 2. If `parser` succeeds → run `continuation(value)`, marking any errors as `committed: True`
///
/// ## Examples
///
/// ```gleam
/// // Without commit: tries all alternatives, confusing errors
/// let statement = if_stmt |> or(while_stmt) |> or(other_stmt)
/// // Input: "if 123" (invalid expr)
/// // Error: "Expected identifier" (tries other_stmt after if_stmt fails!)
///
/// // With commit: stops at first match, clear errors
/// let if_stmt = {
///   use _ <- commit(keyword("if"))  // After parsing "if", we're committed!
///   use expr <- get(expression())   // If this fails, error is committed
///   use stmt <- get(statement())
///   return(IfStmt(expr, stmt))
/// }
/// let statement = if_stmt |> or(while_stmt) |> or(other_stmt)
/// // Input: "if 123"
/// // Error: "Expected expression" (clear! doesn't try other alternatives)
pub fn commit(parser: Parser(a), continuation: fn(a) -> Parser(b)) -> Parser(b) {
  use value <- get(parser)
  use <- cut()
  continuation(value)
}

/// Cut: commit to the current parse path (no backtracking on subsequent errors).
/// 
/// ### This is akin to `if-then-else`
/// 
/// “Cut” only makes a difference when “or” is used; otherwise it's use makes no
/// difference. 
/// 
/// 1. For example,
///   ```
///   { use x₁ <- get(p₁)
///     use x₂ <- get(p₂)
///     q(x₁, x₂)
///   }
///   ```
///   means if *both* p₁ and p₂ succeed, then we execute `q`; ie
///   ```
///   if p₁ && p₂
///   then q(x₁, x₂)
///   else fail
///   ```
///   (This is an anology ---since it's not clear where the `xᵢ` come from!)
/// 
/// 2. We can avoid the implicit failure (since parsers can fail) by using “or”:
///   ```
///   { use x₁ <- get(p₁)
///     use x₂ <- get(p₂)
///     q(x₁, x₂)
///   } |> or(r)
///   ```
///   is analogous to
///   ```
///   if p₁ && p₂       
///   then q(x₁, x₂)
///   else r
///   ```
/// 3. If we place a cut point
///   ```
///   { use x₁ <- get(p₁)
///     use <- cut()
///     use x₂ <- get(p₂)
///     q(x₁, x₂)
///   } |> or(r)
///   ```
///   this is analogous to
///   ```
///   if p₁
///   then { if p₂ then q(x₁, x₂) else fail }
///   else r
///   ```
///   This differs from (2) which tries `r` if either `p₁` or `p₂`
///   fails, whereas this fragment (3) tries `r` only if `p₁` fails!
/// 
/// That is, "cut" splits up a conjunction `p₁ && … && pₙ`
/// to decide which parts `pᵢ` can recover gracefully from failure via `r`
/// and which we are comitted to trying our best and crashing hard if any of them fails.
/// That is, (3) can be thought of as analgous to
/// ```java
/// try {
///  var x₁ = p₁ // Can recover safely from this dangerous method
///  var x₂ = p₂ // This crashes the entire program, if it fails
///  q(x₁, x₂)
/// }
/// catch(ExceptionFromP1Only) {
///   r
/// }
/// ```
/// With this analogy, the cut delimits which "exceptions" are caught.
/// This analogy nicely demonstrates that "cut" only makes a difference when
/// "or" is present: Without the "catch" clause, the "cut" point is not visible.
/// 
/// Finally, just as the branches of an `if-then-else` can be swapped using a negation,
/// so too we can move the `cut` at the cost of introducing a `negative_lookahead`:
///   ```
///   { use x₁ <- get(p₁)
///     use <- cut()
///     use x₂ <- get(p₂)
///     q(x₁, x₂)
///   } |> or(r)
///   ```
/// is the same as
/// ```
/// { use _ <- get(negative_lookahead(p₁))
///   use <- cut()
///   r
/// } |> or({ use x₁ <- get(p₁)
///           use x₂ <- get(p₂)
///           q(x₁, x₂)
///         })
/// ```
/// This is inadvisable however, since it can be costly to peek at the input just to check that p₁ indeed fails.
/// In-contrast, in the original code, `cut` tries `p₁` and if it succeeds then we commit to the path we're on: If we fail, then we fail
/// and no ambient `or` derails us from our failure. But if `p₁` fails, then we recover nicely via `r`.
/// 
/// ### More
///
/// Any errors from the continuation will be marked as committed, preventing
/// `or` from trying alternatives. Like Prolog's `!` operator.
///
/// ```gleam
/// use _ <- get(string("("))
/// use <- cut()  // After seeing '(', we're committed
/// use expr <- get(expression())
/// ...
/// ```
///
/// **Duality with `require`**: `cut` commits errors *after* it (in the continuation),
/// while `require` commits errors *before* it (from the parser). Together they
/// provide complete control over where backtracking is prevented.
pub fn cut(continuation: fn() -> Parser(a)) -> Parser(a) {
  continuation() |> committing
}

/// Mark any error from this parser as committed (prevents backtracking via `or`).
fn committing(parser: Parser(a)) -> Parser(a) {
  parser |> map_error(fn(msg, span, _) { #(msg, span, True) })
}

/// Require a parser to succeed, aborting with message if it fails.
///
/// Designed for use with `use`-syntax after committing to a parse path.
/// Example: `use _ <- require(string("\""), "Unterminated string literal")`
///
/// If the parser already committed (has a more specific error), that error
/// is preserved. Otherwise, the provided message is used at the current position.
///
/// **Duality with `cut`**: `require` commits errors *before* it (from the parser),
/// while `cut` commits errors *after* it (in the continuation). Together they
/// provide complete control over where backtracking is prevented.
/// 
/// ### Tldr: This is akin to `assert`
/// An `assert` clause in Gleam crashes the program if it's false; likewise
/// this combinator short-circuits a `use`-pipeline if the given parser fails.
pub fn require(
  parser: Parser(a),
  message: String,
  continuation: fn(a) -> Parser(b),
) -> Parser(b) {
  parser |> or_abort(message) |> then(continuation)
}

/// Like `require`, but reports the error at a specific position.
///
/// Useful when the error should point to an earlier location (e.g., unclosed paren
/// should point to the opening paren, not where we ran out of input).
///
/// Preserves committed errors (which are more specific) from the inner parser.
/// 
/// ### This is essentially Gleam's `assert parser as message at span`
pub fn require_at(
  expect parser: Parser(a),
  reprimand message: String,
  at span: Span,
  continuation continuation: fn(a) -> Parser(b),
) -> Parser(b) {
  parser |> or_abort_at(message, span) |> then(continuation)
}

/// Require parser to succeed, aborting with message if it fails (uncommitted).
///
/// Committed errors from the parser pass through unchanged.
/// 
/// This is similar to `bool.guard`; eg `use inner <- get(expression() |> parse.or_abort("Expected expression after '('"))`
/// this line aborts the remainder of the `use` clause if `expression()` parser fails.
pub fn or_abort(parser: Parser(a), message: String) -> Parser(a) {
  parser |> or(abort(message))
}

/// Like `required`, but sets the span for the abort error.
///
/// Committed errors from the parser pass through unchanged (with their own span).
pub fn or_abort_at(parser: Parser(a), message: String, span: Span) -> Parser(a) {
  parser |> or(abort(message) |> at(span))
}

// ============================================================================
// Identifier Parsing - useful for lexical analysis
// ============================================================================

/// Parse the next word (non-whitespace chunk).
///
/// Tries to parse an identifier first; falls back to a single character.
/// Useful for error messages that need to show what unexpected token was found.
/// 
/// “word”  resembles the regex “\w”
pub fn word() -> Parser(String) {
  identifier() |> or(pop())
}

/// Parse an identifier (alphanumeric string, at least one char).
pub fn identifier() -> Parser(String) {
  pop()
  |> filter(is_alphanumeric, "Expected alphanumeric")
  |> plus
  |> map(string.concat)
}

pub fn is_alphanumeric(c: String) -> Bool {
  c == "_" || is_digit(c) || is_letter(c)
}

pub fn is_digit(c: String) -> Bool {
  string.length(c) == 1 && { c |> int.parse |> result.is_ok }
}

fn is_letter(c: String) -> Bool {
  string.length(c) == 1 && str_at_most("A", c) && str_at_most(c, "z")
}

fn str_at_most(l, r) -> Bool {
  list.contains([order.Lt, order.Eq], string.compare(l, r))
}

// ============================================================================
// String Parsers
// ============================================================================

/// Match a specific string (one or more characters).
///
/// Fails with "Expected 'x'" if the next character doesn't match.
/// 
/// “string(s)” resembles the regex that matches “s” precisely.
pub fn string(expected: String) -> Parser(String) {
  case string.to_graphemes(expected) {
    [] -> return("")
    [first, ..rest] -> {
      use c <- get(
        pop() |> filter(fn(c) { c == first }, "Expected '" <> first <> "'"),
      )
      use cs <- get(string(string.concat(rest)))
      return(c <> cs)
    }
  }
}

/// Match one whitespace character.
pub fn whitespace() -> Parser(String) {
  pop()
  |> filter(
    fn(c) { c == " " || c == "\t" || c == "\r" || c == "\n" },
    "Expected whitespace",
  )
}

/// Match from a list of string patterns (maximal munch).
///
/// Implements **maximal munch**: patterns are automatically sorted by length (longest first),
/// so longer matches take priority over shorter ones with the same prefix.
///
/// **Note**: This uses string matching without word boundaries. For example,
/// `tokenize([#("true", True)])` would match the prefix of "truefalse",
/// leaving "false" unparsed. If you need word boundaries (e.g., for keywords),
/// use `identifier()` and check the result instead.
pub fn tokenize(mappings: List(#(String, a))) -> Parser(a) {
  let source = fn(p: #(String, a)) { p.0 }
  let token = fn(p: #(String, a)) { p.1 }
  mappings
  // sort longest soruce text first for maximal munch
  |> list.sort(fn(a, b) { string.compare(source(b), source(a)) })
  // Try to parse the source text as a string literal, then replace it by the given token
  |> list.map(fn(it) { string(source(it)) |> map(fn(_) { token(it) }) })
  |> ordered_choice
}

/// Parse a number (integer or float).
/// 
/// “digit” resembles the regex “[0-9]”
pub fn number() -> Parser(Float) {
  pop()
  |> filter(fn(c) { c == "." || is_digit(c) }, "Expected digit or '.'")
  |> plus
  |> map(string.concat)
  |> then(fn(numeric) {
    case
      numeric
      |> float.parse
      |> result.lazy_or(fn() {
        numeric |> int.parse |> result.map(int.to_float)
      })
    {
      Ok(n) -> return(n)
      gleam.Error(_) -> error("Invalid number: " <> numeric)
    }
  })
}

/// Parse a double-quoted string literal.
pub fn string_literal() -> Parser(String) {
  delimited(
    from: string("\""),
    to: string("\""),
    get: pop(),
    or_error: "Whoops, it seems like you forgot to terminate this string literal!",
  )
  |> map(string.concat)
}

/// Parse text between `begin` and `end` delimiters, collecting raw text.
///
/// Returns the content between the delimiters (not including the delimiters themselves).
/// Useful for parsing comments, strings with custom delimiters, etc.
///
/// If the end delimiter is not found, reports a helpful error pointing to the opening delimiter.
pub fn text_delimited(begin: String, end: String) -> Parser(String) {
  delimited(
    from: string(begin),
    to: string(end),
    get: pop(),
    or_error: "Unterminated `" <> begin <> " ... " <> end <> "`",
  )
  |> map(string.concat)
}

/// Implement `begin body* end`
///
/// + If `begin` is actually parsed, then we commit to parsing this delimited expression: No backtracking!
/// + If `end` is not found, reports error at the position of `begin` with the given message.
pub fn delimited(
  from begin_parser: Parser(begin),
  get body_parser: Parser(body),
  to end_parser: Parser(end),
  or_error message: String,
) -> Parser(List(body)) {
  use #(_, open_span) <- commit(begin_parser |> with_span)
  use body <- get(
    star({
      use _ <- get(negative_lookahead(end_parser))
      body_parser
    }),
  )
  use _ <- require_at(expect: end_parser, reprimand: message, at: open_span)
  return(body)
}

/// Parse a line comment starting with the given prefix until end of line.
///
/// Matches the prefix, then consumes all characters until a newline or end of input.
/// The newline IS consumed (if present).
pub fn line_comment(prefix: String) -> Parser(String) {
  use _ <- get(string(prefix))
  use text <- get(
    star({
      use _ <- get(negative_lookahead(string("\n")))
      pop()
    }),
  )
  // A line comment at the end of a file is not terminated by a newline
  use _ <- get(maybe(string("\n")))
  return(string.concat(text))
}
