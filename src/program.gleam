/// A Gleam module to work with the following grammar. 
/// ```
/// program        → declaration* EOF ;
/// declaration    → varDecl | statement ;
/// statement      → exprStmt | printStmt 
/// exprStmt       → expression ";"
/// printStmt      → "print" expression ";"
/// varDecl        → "var" IDENTIFIER ( "=" expression )? ";"
/// ```
/// 
import builtin
import error_formatter
import evaluator
import expr.{type Expr, type Literal}
import expr_parser
import gleam/bool
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import parser_combinators.{
  type Parser, type Span, type SyntaxError, Error as ParseErr,
  Success as ParseSuccess,
} as parse

// ## Type ##############################################################################

pub type Program {
  Program(declarations: List(Declaration), errors: List(SyntaxError))
}

/// Declarations of methods, classes, and global variables.
pub type Declaration {
  VarDecl(name: String, value: Option(Expr))
  Statement(Statement)
}

pub type Statement {
  /// Denotes `expression ;`
  ExprStatement(expr: Expr)
  // Denotes `print expr;`
  Print(expr: Expr)
}

// ## Parser ##############################################################################

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
  parse.return(Program(decls, errors))
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

// ## Evaluator ##############################################################################

/// A type used for the purposes of testing the evaluator; manual dependency injection.
/// ### Example
/// ```
/// let usual_io = IO(io.print, fn() { Nil })
/// ```
pub type IO(i, o) {
  IO(print: fn(i) -> o, do_nothing: fn() -> o)
}

pub fn parse_and_evaluate(my_io: IO(String, side_effect_type), source: String) {
  let parse_result =
    source
    |> parse.input_from_string
    |> program()
  case parse_result {
    parse.Success(found: program, ..) -> eval(my_io, source, program)
    parse.Error(message:, at:, ..) -> [
      error_formatter.format_error(kind: "Syntax error", message:, source:, at:)
      |> my_io.print,
    ]
  }
}

/// Evaluation of a program is the execution of a bunch of statements, which produces side-effects
/// and returns no value. The IO argument is useful for testing purposes
pub fn eval(
  my_io: IO(String, side_effect_type),
  source,
  program,
) -> List(side_effect_type) {
  let Program(declarations:, errors:) = program
  // First, print any syntax errors that were recovered from
  let syntax_error_outputs =
    list.map(errors, fn(err) {
      error_formatter.format_error(
        kind: "Syntax error",
        message: err.message,
        source:,
        at: err.at,
      )
      |> my_io.print
    })
  // Then evaluate the successfully parsed declarations
  let eval_outputs =
    list.map(declarations, fn(decl) {
      case decl {
        VarDecl(name: _, value: _) ->
          todo as "VarDecl evaluation not implemented"
        Statement(Print(expr:)) ->
          case eval_expr_as_string(source, expr) {
            Error(string) | Ok(string) -> string |> my_io.print
          }
        // Expression statements have no side-effects, yet!
        // However, we should still evaluate them in-case they have errors.
        Statement(ExprStatement(expr:)) ->
          case eval_expr_as_string(source, expr) {
            Error(whoops) -> whoops |> my_io.print
            Ok(_) -> my_io.do_nothing()
          }
      }
    })
  list.append(syntax_error_outputs, eval_outputs)
}

/// @param source Used to point to the precise part of the source-code that caused an expression-related runtime error
fn eval_expr_as_string(source, expr: Expr) {
  evaluator.eval(expr)
  |> result.map(literal_to_string)
  |> result.map_error(fn(err) {
    error_formatter.format_error(
      kind: "Runtime error",
      message: err.message,
      source:,
      at: err.span,
    )
  })
}

fn literal_to_string(literal: Literal) {
  case literal {
    expr.Boolean(value:) -> bool.to_string(value)
    expr.Nil -> "nil"
    expr.Number(value:) -> builtin.number_to_string(value)
    expr.String(value:) -> "\"" <> value <> "\""
  }
}
