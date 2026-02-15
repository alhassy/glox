import computation.{type EffectfulComputation, type IO}
import error_formatter
import expr.{type Literal, Literal, Nil as LNil}
import gleam/list
import gleam/option
import parser_combinators as parse
import program.{
  type Declaration, ExprStatement, Print, Program, Statement, VarDecl,
}
import program_parser as parser

pub fn parse_and_evaluate(my_io: IO(io_output), source: String) {
  let parse_result =
    source
    |> parse.input_from_string
    |> parser.program()
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
pub fn eval(my_io: IO(io_output), source, program) -> List(io_output) {
  let Program(declarations:, errors:, enviornment:) = program
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
  let #(_env, io_outputs) =
    eval_declarations(declarations)
    |> computation.execute(source, enviornment, my_io)
  // enviornment, my_io, source, 
  list.append(syntax_error_outputs, io_outputs)
}

fn eval_declarations(
  declarations: List(Declaration),
) -> EffectfulComputation(io, Literal) {
  case declarations {
    [] -> computation.return(LNil)
    [decl, ..more] -> {
      use _ <- computation.then(eval_declaration(decl))
      eval_declarations(more)
    }
  }
}

/// 💢 We shouldn't need this!
/// Instead, we should get the span of a variable declaration 
/// and attach it to the `name` of the variable
const dummy_span = parse.Span(0, 0, 0)

fn eval_declaration(
  declaration: Declaration,
) -> EffectfulComputation(io, Literal) {
  case declaration {
    VarDecl(name:, value: initializer) ->
      initializer
      |> option.unwrap(Literal(LNil, dummy_span))
      |> computation.eval_expr
      |> computation.then(fn(value) { computation.update_scope(name, value) })
    Statement(Print(expr:)) ->
      expr
      |> computation.eval_expr
      |> computation.println
      |> computation.map(fn(_) { LNil })
    // Expression statements have no side-effects, yet!
    // However, we should still evaluate them in-case they have errors.
    Statement(ExprStatement(expr:)) -> expr |> computation.eval_expr
  }
}
