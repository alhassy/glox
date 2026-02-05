/// A Gleam module to work with the following grammar. 
/// ```
/// program        → statement* EOF 
/// statement      → exprStmt | printStmt 
/// exprStmt       → expression ";"
/// printStmt      → "print" expression ";"
/// ```
/// 
import builtin
import error_formatter
import evaluator
import expr.{type Expr, type Literal}
import expr_parser
import gleam/bool
import gleam/io
import gleam/list
import gleam/result
import parser_combinators.{type Parser, type Span} as parse

// ## Type ##############################################################################

pub type Program {
  // Denotes statements `S1; S2; ...; Sn`
  Program(statements: List(Statement))
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
  use stmts <- get(
    {
      use _ <- parse.get(expr_parser.skip_ws())
      use s <- get(statement())
      use _ <- parse.get(expr_parser.skip_ws())
      parse.return(s)
    }
    |> parse.star(),
  )
  use _ <- get(parse.eof())
  parse.return(Program(stmts))
}

//  
//  use expr <- parse.get(expression())
//  use _ <- parse.get(skip_ws())

// A parser for statements
pub fn statement() -> Parser(Statement) {
  print() |> parse.or(expr())
}

// A parser for `print expr` clauses
pub fn print() -> Parser(Statement) {
  use _ <- get(parse.string("print"))
  use expr <- get(expr_parser.parser())
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
  let Program(statements:) = program
  use stmt <- list.map(statements)
  case stmt {
    Print(expr:) ->
      case eval_expr_as_string(source, expr) {
        Error(string) | Ok(string) -> string |> my_io.print
      }
    // Expression statements have no side-effects, yet!
    // However, we should still evaluate them in-case they have errors.
    ExprStatement(expr:) ->
      case eval_expr_as_string(source, expr) {
        Error(whoops) -> whoops |> my_io.print
        Ok(_) -> my_io.do_nothing()
      }
  }
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
