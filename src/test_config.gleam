/// Test configuration for glox regression tests.
///
/// This module defines the transform functions and configured framework
/// used by both the test runner and expectation updater.
///
/// # CLI Usage
///   gleam test                         # Run all regression tests
///   gleam run -m update_expectations   # Regenerate expected values
///
/// # Related Files
///   - test/regression_test.gleam - Test runner (must be in test/ for gleam test)
///   - src/update_expectations.gleam - CLI updater (must be in src/ for gleam run)

import computation.{IO}
import enviornment
import error_formatter
import gleam/list
import gleam/string
import parser_combinators.{Success}
import pprint
import program.{Program}
import program_evaluator
import program_parser
import regression_framework

/// The configured regression framework for Lox tests.
pub fn framework() -> regression_framework.RegressionFramework {
  regression_framework.new()
  |> regression_framework.with_test_dir("test/specs")
  |> regression_framework.with_dimension("parsing", parse_lox)
  |> regression_framework.with_dimension("evaluation", run_lox)
}

/// Parse Lox code and return the AST as a pretty-printed string.
/// Declarations shown as "- Decl(...)", errors shown with full formatting.
pub fn parse_lox(input: String) -> String {
  case program_parser.parse(input) {
    Success(found: program, ..) -> {
      let Program(declarations:, errors:, ..) = program
      let decl_strs =
        declarations
        |> list.map(fn(d) { "- " <> pprint.format(d) })
      let error_strs =
        errors
        |> list.map(fn(err) {
          "- "
          <> error_formatter.format_error(
            kind: "Syntax error",
            message: err.message,
            source: input,
            at: err.at,
          )
        })
      list.append(decl_strs, error_strs) |> string.join("\n")
    }
    parser_combinators.Error(message:, at:, ..) ->
      "- "
      <> error_formatter.format_error(
        kind: "Syntax error",
        message:,
        source: input,
        at:,
      )
  }
}

/// Run Lox code and return the output as a string.
/// Prints are shown as "PRINTED <value>", errors as "ERROR <message>".
/// Environment is shown at the end if there are variables.
pub fn run_lox(input: String) -> String {
  let transparent_io =
    IO(print: fn(str) { "PRINTED " <> str }, error: fn(str) { "ERROR " <> str })

  case program_parser.parse(input) {
    Success(found: program, ..) -> {
      let #(final_env, outputs) =
        program_evaluator.eval(transparent_io, input, program)
      let output_str = string.join(outputs, "\n")
      // Only show ENV if there are variables
      let env_str = case enviornment.to_list(final_env) {
        [] -> ""
        bindings -> "\nENV: " <> pprint.format(bindings)
      }
      output_str <> env_str
    }
    parser_combinators.Error(message:, at:, ..) -> {
      "PARSE ERROR: " <> message <> " at " <> string.inspect(at)
    }
  }
}
