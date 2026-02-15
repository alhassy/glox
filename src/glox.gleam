import enviornment
import error_formatter
import expr.{type Literal, Boolean, Number, String as LoxString}
import expr_evaluator
import expr_parser as parser
import gleam/io
import gleam/string
import parser_combinators.{Error as ParseError, Success}
import type_error.{RuntimeError}

import argv

// A simple way to load CLI args: gleam add argv

/// reads a line of input from the user on the command line and returns the result
@external(erlang, "io", "get_line")
pub fn read_line(prompt: String) -> String

/// When given a single arg, execute that Lox script; if no args are given, start a REPL session.
pub fn main() {
  case argv.load().arguments {
    [] -> run_repl_prompt()
    [path_to_lox_script] -> run_file(path_to_lox_script)
    _ -> io.println("Usage: gleam run [script]")
  }
}

pub fn run_file(path_to_lox_script) {
  io.println("Running Lox code for " <> path_to_lox_script <> " ...")
  panic as "This is not yet implemented 😅"
}

/// Run a prompt where users can enter and execute code one line at a time.
pub fn run_repl_prompt() {
  let source = read_line("lox > ")
  case string.trim(source) {
    "exit" -> Nil
    _ -> {
      case parser.parse(source) {
        Success(parsed_expr, _) ->
          case expr_evaluator.eval(parsed_expr, enviornment.new()) {
            Ok(value) -> format_value(value)
            Error(RuntimeError(message, span)) ->
              error_formatter.format_error(
                kind: "Runtime error",
                message:,
                source:,
                at: span,
              )
          }
        ParseError(message, span, _) ->
          error_formatter.format_error(
            kind: "Syntax error",
            message:,
            source:,
            at: span,
          )
      }
      |> io.println
      run_repl_prompt()
    }
  }
}

/// Format a Lox value for display in the REPL
fn format_value(value: Literal) -> String {
  case value {
    LoxString(s) -> "\"" <> s <> "\""
    Number(n) -> expr_evaluator.number_to_string(n)
    Boolean(True) -> "true"
    Boolean(False) -> "false"
    expr.Nil -> "nil"
  }
}
