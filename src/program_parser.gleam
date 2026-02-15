import enviornment
import expr_parser
import gleam/bool
import gleam/list
import parser_combinators.{type Parser} as parse
import program.{
  type Declaration, type Program, type Statement, ExprStatement, Print, Program,
  Statement, VarDecl,
}

const get = parse.then

pub fn parse(source: String) -> parse.ParseResult(Program) {
  source
  |> parse.input_from_string
  |> program()
}

/// A parser for programs
pub fn program() -> Parser(Program) {
  use #(decls, errors) <- get(parse.synchronize(
    declaration() |> as_lexeme,
    discard_garbage_chars_until_at_parserable_input(),
  ))
  use _ <- get(parse.eof())
  parse.return(Program(decls, errors, enviornment.new()))
}

/// Executes the given parser and ignores whitespace before and after it.
fn as_lexeme(parser: parse.Parser(a)) -> parse.Parser(a) {
  use _ <- parse.get(expr_parser.skip_ws())
  use result <- parse.get(parser)
  use _ <- parse.get(expr_parser.skip_ws())
  parse.return(result)
}

// A parser for declarations
pub fn declaration() -> Parser(Declaration) {
  variable() |> parse.or(statement() |> parse.map(Statement))
}

// A parser for statements
pub fn variable() -> Parser(Declaration) {
  use _ <- get(parse.string("var"))
  use _ <- parse.get(expr_parser.some_ws())
  use name <- get(parse.identifier())
  use value <- get(
    {
      use _ <- parse.get(expr_parser.some_ws())
      // If user places anything after `var <name>`, then it better be `=`! 😁
      use _ <- parse.require(
        parse.string("="),
        "Expected to see an `=` for variable assignment",
      )
      use expr <- parse.require(
        expr_parser.parser(),
        "I expected to see an expression here 😖",
      )
      parse.return(expr)
    }
    |> parse.maybe,
  )
  use _ <- parse.require(
    parse.string(";"),
    "I expected to see a semicolon here 🤔",
  )
  parse.return(VarDecl(name:, value:))
}

// A parser for statements
pub fn statement() -> Parser(Statement) {
  print() |> parse.or(expr())
}

// A parser for `print expr` clauses
pub fn print() -> Parser(Statement) {
  use _ <- get(parse.string("print"))
  use expr <- parse.require(
    expr_parser.parser(),
    "A print clause is of the form ` print <expr>; `, you're missing the expr!",
  )
  use _ <- parse.require(
    parse.string(";"),
    "I expected to see a semicolon here 🤔",
  )
  parse.return(Print(expr))
}

// A parser for `expr;` clauses
pub fn expr() -> Parser(Statement) {
  use expr <- get(expr_parser.parser())
  use _ <- parse.require(
    parse.string(";"),
    "I expected to see a semicolon here 🤔",
  )
  parse.return(ExprStatement(expr))
}

/// Discard tokens we're at a statement boundary. 
/// + After catching a ParseError, using `parse.or`, we’ll call this and
///   then we are hopefully back in sync. 
/// When it works well, we have discarded tokens that would have likely caused cascaded errors anyway,
/// and now we can parse the rest of the file starting at the next statement.
/// 
/// NOTE: This is a Parser(Nil) since this parser only modifies the input stream,
/// without actually parsing anything meaningful.
fn discard_garbage_chars_until_at_parserable_input() -> Parser(Nil) {
  // ⚠️ This method has an implicit precondition: it's called on input that already failed to start a valid declaration.
  // The first character(s) it sees are guaranteed NOT to be the start of a valid declaration 
  // - those would have been parsed by the previous attempt.

  // Get the next char...
  use next_char <- get(parse.pop())
  use is_eof <- get(parse.inspect_input(fn(it) { it.unconsumed == "" }))
  use <- bool.guard(when: is_eof, return: parse.return(Nil))
  // If we're not at the end of the file...
  // and the next char is a semicolon, then we're done error recovery and can parse safely again
  use <- bool.guard(when: next_char == ";", return: parse.return(Nil))
  // OK, so at this point, we know `next_char` is a garbage character, so let's check
  // if we've reached something parserable now.

  // Check if the remaining input starts with a statement keyword (without consuming); 
  // if so, then we're done error recovery and can parse safely again
  let keyword = ["class", "fun", "var", "for", "if", "while", "print", "return"]
  let declaration_boundary =
    keyword
    |> list.map(parse.the_identifier)
    |> parse.ordered_choice
  use at_boundary <- get(declaration_boundary |> parse.peeking)
  use <- bool.guard(when: at_boundary, return: parse.return(Nil))
  // Otherwise, keep consuming tokens until we're at a declaration boundary!
  // negative_lookahead succeeded = no keyword found, keep synchronizing
  discard_garbage_chars_until_at_parserable_input()
}
