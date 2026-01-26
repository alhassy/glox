import gleam

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

  /// This reports an error at a given token. It shows the token's location and the token itself.
  /// This will come in handy later since we use tokens throughout the interpreter to track locations in code.
  ///
  /// This is a simple sentinel class we use to unwind the parser.
  /// The error() method returns the error instead of throwing it because we want to let the calling
  /// method inside the parser decide whether to unwind or not.
  /// Some parse errors occur in places where the parser isn't likely to get into a weird state
  /// and we don't need to synchronize. In those places, we simply report the error and keep on truckin'.
  ///
  /// The `committed` flag implements Prolog's cut operator (!): When `True`, the `or` combinator
  /// will NOT try alternatives. This prevents backtracking after commitment points.
  Error(
    // line: Int,
    // If we have a token in-hand, but it's not what we expect, then let's report it in the erorr message
    //  while_looking_at: Option(token),
    message: String,
    committed: Bool,
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

/// The parser that always fails, with the given `reason`.
/// The error is not committed (allows backtracking via `or`).
pub fn error(reason) -> Parser(token, value) {
  fn(_input) { Error(reason, committed: False) }
}

/// The parser that always fails with a **committed** error (prevents backtracking).
///
/// Like `error(reason)`, but the error is committed, so `or` will NOT try alternatives.
/// Use this when you've detected a real syntax error that must be reported.
///
/// ## Examples
///
/// ```gleam
/// // Without abort: tries alternatives, confusing errors
/// { identifier_parser } |> or({ number_parser })
/// // Input: "+" → Error("Expected identifier \n Expected number")
///
/// // With abort: stops immediately, clear error
/// {
///   use next <- get(peek())
///   case next {
///     Operator -> abort("Expected value but saw operator")
///     _ -> identifier_parser
///   }
/// }
/// // Input: "+" → Error("Expected value but saw operator")
/// ```
pub fn abort(reason) -> Parser(token, value) {
  fn(_input) { Error(reason, committed: True) }
}

/// Commit: Mark errors as committed after the parser succeeds (like Prolog's ! operator).
///
/// tldr:
///   - If first_parser fails → error is NOT committed (allows or to try when_error)
///   - If first_parser succeeds → we're committed, any subsequent failure is non-backtrackable
/// 
/// This is a higher-order combinator designed for use with `use`-syntax. After `parser`
/// succeeds, runs `continuation` with the parsed value. Any errors from the continuation
/// are marked as committed (no backtracking).
///
/// **Semantics**:
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
/// ```
pub fn commit(
  parser: Parser(token, a),
  continuation: fn(a) -> Parser(token, b),
) -> Parser(token, b) {
  fn(tokens) {
    case parser(tokens) {
      Error(msg, committed) -> Error(msg, committed)
      // Propagate parser error as-is
      Success(found, unconsumed) -> {
        // Parser succeeded, now we're committed to this parse path
        case continuation(found)(unconsumed) {
          Error(msg, committed: _) -> Error(msg, committed: True)
          // Mark as committed!
          success -> success
        }
      }
    }
  }
}

/// Implement the grammar rule `value (seperator value)*`
/// then combine the results via `combiner`.
///
/// ## Parameters
///
/// - `value_parser` - Parser for the values being separated
/// - `seperator_parser` - Parser for the separator tokens (operators like `+`, `-`, etc.)
/// - `combiner` - Function to combine `left separator right` into a result
/// - `primary_checker` - Function that returns `True` if a token can **never** be an operator
///   (used for context-aware error detection)
///
/// ## Error Handling Strategy
///
/// This combinator uses **two-level error detection** to catch different kinds of mistakes:
///
/// ### 1. Separator succeeded but value failed → propagate error
///
/// When we successfully parse a separator (like `+`), we commit to parsing the following value.
/// If that fails, we propagate the error instead of rolling back.
///
/// ```gleam
/// "1 + apple"  // After parsing `+`, must parse a value
///              // `apple` fails → Error("Expected a unary op... but saw identifier")
/// ```
///
/// ### 2. No separator found → context-aware checking for missing operators
///
/// After parsing a value, if we don't find a separator, we check the next token using
/// `primary_checker`:
///
/// - **Primary token** (returns `True`) → ERROR: "Expected operator between values"
/// - **Non-primary token or end of input** (returns `False`) → SUCCESS (done parsing at this level)
///
/// #### Why we need context-aware checking (the operator ambiguity problem)
///
/// We can't simply check "can another value be parsed?" because of **operator ambiguity** in
/// expression grammars. Some tokens (like `-` and `!`) can be both:
/// 1. Binary operators (separators at this level)
/// 2. Unary operators (part of values at lower levels)
///
/// **Example of the problem:**
///
/// ```gleam
/// "1 * 2 - 3"  // After factor() parses "1 * 2"
///              // Can we parse another factor from "- 3"?
///              // YES! Because `-` starts a unary expression (like -3)
///              // But `-` is actually the BINARY MINUS at the term level!
///              // If we naively error here, we break valid code.
/// ```
///
/// **Solution: Primary token checking via `primary_checker`**
///
/// The `primary_checker` function identifies tokens that can **never** be operators at any
/// precedence level (e.g., NUMBER, STRING, IDENTIFIER, LEFT_PAREN). So:
///
/// ```gleam
/// "1 * 2 3"    // After "1 * 2", next token is `3` (NUMBER)
///              // primary_checker(3) → True (NUMBER is primary)
///              // → Error("Expected operator between values")
///
/// "1 * 2 - 3"  // After "1 * 2", next token is `-` (OPERATOR)
///              // primary_checker(-) → False (OPERATOR is not primary)
///              // → Success, let term() handle the `-`
/// ```
///
/// ## Examples
///
/// ```gleam
/// // Valid expressions
/// many_with_seperator(factor, plus_or_minus, combine, is_primary)("1 + 2 + 3")
/// // → Success(((1 + 2) + 3), [])
///
/// many_with_seperator(factor, times_or_div, combine, is_primary)("1 * 2 - 3")
/// // → Success((1 * 2), [-, 3])  // `-` handled at term level
///
/// // Invalid expressions
/// many_with_seperator(factor, times_or_div, combine, is_primary)("1 * 2 3")
/// // → Error("Expected operator between values")  // `3` is primary!
///
/// many_with_seperator(factor, times_or_div, combine, is_primary)("1 * apple")
/// // → Error("Expected a unary op... but saw identifier")  // separator committed
/// ```
/// 
/// Note: `is_unexpected` should be "disjoint" from `value_parser` and `seperator_parser`.
pub fn many_with_seperator(
  value_parser value_parser: Parser(token, value),
  seperator_parser seperator_parser: Parser(token, seperator),
  is_unexpected is_unexpected: fn(token) -> Bool,
) -> Parser(token, #(value, List(#(seperator, value)))) {
  use first <- then(value_parser)
  use pairs <- then(
    // Try to parse separator + value
    {
      use sep <- commit(seperator_parser)
      // We're committed to the rest of this parser; ie no trailing seperators allowed!
      use right <- get(value_parser)
      return(#(sep, right))
    }
    // If separator fails, check if next token is something unexpected
    |> or({
      use next_token <- get(peek_one_token())
      // The message `""` doesn't matter because `star` **ignores the message in uncommitted errors**.
      // It just sees `Error(_, committed: False)` and thinks "OK, we're done here" and stops.
      //
      // The error message ONLY matters when the error propagates to the user. Since `star`
      // catches all uncommitted errors and converts them to success, the message never reaches
      // anyone.
      //
      // **Key insight**: `star` treats ALL uncommitted errors as "done parsing", regardless
      // of what the error message says. The message is irrelevant.      
      use <- when(is_unexpected(next_token), "")
      abort("Expected separator between values")
    })
    |> star,
  )

  // Return first value and list of (separator, value) pairs
  return(#(first, pairs))
}

/// Parse zero or more occurrences of `parser`, collecting results in a list.
///
/// ## What it does (simple version)
///
/// Imagine you're collecting marbles from a jar. You keep taking marbles one by one
/// until either:
/// - You run out of marbles (that's OK! Just stop)
/// - Something goes seriously wrong (that's an error! Stop and tell someone)
///
/// `star` does the same thing with parsing: keep parsing until you can't anymore,
/// then return what you collected.
///
/// ## The three cases (how it handles different results)
///
/// 1. **`Success(value, rest)`** → "Found a marble! Keep going."
///    - Adds `value` to the list and recursively parses `rest`
///
/// 2. **`Error(_, committed: False)`** → "No more marbles, that's fine!"
///    - Stops gracefully and returns everything collected so far
///    - **This is the key trick**: uncommitted error = "I'm done (successfully)"
///
/// 3. **`Error(msg, committed: True)`** → "Something's broken!"
///    - Propagates the error immediately (real problem that must be reported)
///
/// ## The `error("")` trick (important!)
///
/// When you want to tell `star` "I'm done parsing, please stop", you return an
/// **uncommitted error** (even if nothing went wrong). Think of it as a secret
/// handshake that means "all done here, thanks!"
///
/// ```gleam
/// star({
///   digit_parser           // Try to parse a digit
///   |> or(error(""))       // If not a digit, signal "I'm done" (not an error!)
/// })
/// // Input: "123abc" → Success([1, 2, 3], "abc")  ← Stopped at 'a', no error!
/// ```
///
/// The empty string `""` is just a placeholder - `star` ignores uncommitted errors
/// and just stops collecting.
///
/// ## Why this matters: Error message combining
///
/// The trick enables `or` to combine error messages from multiple alternatives:
///
/// ```gleam
/// star({
///   // Try parsing separator + value
///   { use sep <- commit(separator_parser)
///     use val <- get(value_parser)
///     return(#(sep, val)) }
///   // If separator fails, check WHY
///   |> or({
///     use next <- get(peek())
///     case next {
///       PrimaryToken -> abort("Expected separator")  // Real error!
///       OperatorToken -> halt_star_repetition()  // Done parsing (this is fine)
///     }
///   })
/// })
/// ```
///
/// When all alternatives fail:
/// - Separator failed: `Error("Expected `/` but saw 3", committed: False)`
/// - Primary check: `Error("Expected separator...", committed: True)`
/// - `or` combines them: `"Expected separator... \n Expected `/` but saw 3"`
/// - Committed error propagates with BOTH messages!
///
/// ## Examples
///
/// ### Example 1: Parse digits (simple)
/// ```gleam
/// star(digit_parser)
/// // "123abc" → Success([1, 2, 3], "abc")
/// // "abc" → Success([], "abc")  ← Zero digits is OK!
/// ```
///
/// ### Example 2: Parse with a stopping condition
/// ```gleam
/// star({
///   operator_parser  // Try to parse operator
///   |> or({
///     use next <- get(peek())
///     case next {
///       Semicolon -> halt_star_repetition()  // Stop here (done with expression)
///       _ -> abort("Expected operator")  // Real error
///     }
///   })
/// })
/// ```
///
/// ### Example 3: The separator+value pattern (actual use case)
/// See `many_with_seperator` for a real example of this pattern in action!
///
/// ## Common pattern: When to use halt_star_repetition vs abort
///
/// - **`halt_star_repetition()`**: "I'm done parsing, this is expected"
///   - Use when: End of input, wrong precedence level, valid stopping point
///   - Effect: `star` stops gracefully, returns collected results
///
/// - **`abort(msg)`**: "Something's wrong!"
///   - Use when: Missing required syntax, invalid token where valid one expected
///   - Effect: Propagates immediately, tells user what went wrong
fn star(parser: Parser(token, a)) -> Parser(token, List(a)) {
  {
    use value <- get(parser)
    use rest <- get(star(parser))
    return([value, ..rest])
  }
  |> or(return([]))
}

pub fn one_token() -> Parser(token, token) {
  fn(tokens) {
    case tokens {
      [t, ..ts] -> Success(found: t, unconsumed: ts)
      [] -> Error("No token to parse!", committed: False)
    }
  }
}

/// Like `one_token` but only peeks at the next token and does not consume any input at all!
pub fn peek_one_token() -> Parser(token, token) {
  fn(tokens) {
    case tokens {
      [t, ..] -> Success(found: t, unconsumed: tokens)
      [] -> Error("No token to parse!", committed: False)
    }
  }
}

/// The parser that always succeeds and returns the given `token`
pub fn return(found) -> Parser(token, value) {
  fn(unconsumed) { Success(found, unconsumed) }
}

/// This is essentially `filter` but aimed at `use`-syntax.
/// ### Example: `filter(p, f, msg) == { use x <- p; use <- when(f(x), msg); return(x) }
pub fn when(
  condition: Bool,
  message: String,
  continue: fn() -> Parser(token, value),
) -> Parser(token, value) {
  case condition {
    True -> continue()
    False -> error(message)
  }
}

fn filter(
  parser: Parser(token, value),
  predicate: fn(value) -> Bool,
  failure_message: String,
) -> Parser(token, value) {
  fn(tokens) {
    case parser(tokens) {
      Success(found, _) as done ->
        case predicate(found) {
          True -> done
          False -> Error(failure_message, committed: False)
        }
      err -> err
    }
  }
}

/// This is essentially `choose` but targeted at making `use`-syntax more readable
/// This is essentially `then`/`get` but targetted at `Result` values!
/// # More precisely
/// `use x <- get(p); use c <- unwrap_result(f(x)); k(c)` == `p |> choose(f) |> then(k)`
fn unwrap_result(
  result: Result(b, String),
  continue: fn(b) -> Parser(token, c),
) -> Parser(token, c) {
  case result {
    gleam.Error(failure_message) -> fn(_unconsumed) {
      Error(failure_message, committed: False)
    }
    Ok(b) -> continue(b)
  }
}

/// Applies a partial map to the result of a parser.
/// This is essentially both a `filter` and `map`.
pub fn choose(
  parser: Parser(token, value_a),
  selector: fn(value_a) -> Result(value_b, String),
) -> Parser(token, value_b) {
  fn(tokens) {
    case parser(tokens) {
      Error(msg, committed) -> Error(msg, committed)
      // Propagate error with commit flag
      Success(found:, unconsumed:) ->
        case selector(found) {
          Ok(found) -> Success(found, unconsumed)
          gleam.Error(failure_message) ->
            Error(failure_message, committed: False)
        }
    }
  }
}

pub fn map(
  parser: Parser(token, value_a),
  mapper: fn(value_a) -> value_b,
) -> Parser(token, value_b) {
  fn(tokens) {
    case parser(tokens) {
      Error(msg, committed) -> Error(msg, committed)
      // Propagate error with commit flag
      Success(found, unconsumed) -> Success(mapper(found), unconsumed)
    }
  }
}

/// Run the `main` parser but if it fails then run the `fallback` parser.
///
/// **Commit semantics**: If `main` returns `Error(..., committed: True)`, we've committed
/// to that parse path and will NOT try `fallback`. This prevents backtracking after commitment points.
pub fn or(main: Parser(a, b), fallback: Parser(a, b)) -> Parser(a, b) {
  fn(tokens) {
    case main(tokens) {
      Error(_, committed: True) as committed_error -> committed_error
      // Don't try fallback after commit!
      Error(msg, committed: False) ->
        case fallback(tokens) {
          Error(second_message, committed: second_committed) ->
            // Show all messages when things fail! (In reverse order).
            Error(second_message <> " \n " <> msg, committed: second_committed)
          success -> success
        }
      success -> success
    }
  }
}

/// Parser an `a`-value then execute a parser-producing callback to obtain a `b`-value.
pub fn then(
  first: Parser(token, a),
  then: fn(a) -> Parser(token, b),
) -> Parser(token, b) {
  fn(tokens) {
    case first(tokens) {
      Error(message, committed) -> Error(message, committed)
      // Propagate error with commit flag
      Success(found, unconsumed) -> then(found)(unconsumed)
    }
  }
}

/// An alias for `then` that makes code involving `use` more readable
fn get(first, callback) {
  then(first, callback)
}

/// Gets a parser that succeeds if and only if the original parser has consumed all of its input.
pub fn input_all_consumed(parser: Parser(token, value)) -> Parser(token, value) {
  fn(tokens) {
    case parser(tokens) {
      Success(_, [_, ..]) -> Error("Expected end of input", committed: False)
      all_good -> all_good
    }
  }
}
