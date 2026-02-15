import builtin
import enviornment.{type Enviornment}
import error_formatter
import evaluator
import expr.{type Expr, type Literal}
import gleam/bool
import gleam/io
import gleam/list
import gleam/result

/// Denotes a computation that when run may produce some IO outputs along with a final 'return value'.
/// + This is a reader monad (since it reads some input state) and a writer monad (since it updates some incoming state).
/// 
///  An "effectiful computation" may fail (whence `Result`) or it may succeed producing a new
/// enviornment along with some executed IO commands.
/// 
/// /// @param source Used to point to the precise part of the source-code that caused an expression-related runtime error
/// (See `eval_expr` method below!)
pub opaque type EffectfulComputation(io_output, value) {
  EffectfulComputation(
    run: fn(SourceCode, Enviornment, IO(io_output)) ->
      Result(#(Enviornment, List(io_output), value), ErrorMessage),
  )
}

type SourceCode =
  String

type ErrorMessage =
  String

/// A type used for the purposes of testing the evaluator; manual dependency injection.
/// ### Example
/// ```
/// let usual_io = IO(print: io.print, error: io.print)
/// ```
pub type IO(output) {
  IO(print: fn(String) -> output, error: fn(String) -> output)
}

pub fn execute(
  computation: EffectfulComputation(io_output, Literal),
  source: String,
  env0: Enviornment,
  io: IO(io_output),
) -> #(Enviornment, List(io_output)) {
  case computation.run(source, env0, io) {
    Error(msg) -> #(env0, [io.error(msg)])
    Ok(#(env1, io_outputs, _literal)) -> #(env1, io_outputs)
  }
}

// Side-effect free computation producing the given `value`
pub fn return(value: a) -> EffectfulComputation(io, a) {
  fn(_source, env, _io) { Ok(#(env, [], value)) }
  |> EffectfulComputation
}

pub fn then(
  computation: EffectfulComputation(io, a),
  mapper: fn(a) -> EffectfulComputation(io, b),
) -> EffectfulComputation(io, b) {
  fn(source: String, env, io) {
    use #(env1, effects1, value1) <- result.try(computation.run(source, env, io))
    use #(env2, effects2, value2) <- result.map(mapper(value1).run(
      source,
      env1,
      io,
    ))
    #(
      enviornment.merge_into(env2, env1),
      list.append(effects1, effects2),
      value2,
    )
  }
  |> EffectfulComputation
}

pub fn map(
  computation: EffectfulComputation(io, a),
  mapper: fn(a) -> b,
) -> EffectfulComputation(io, b) {
  fn(source: String, env, io) {
    use #(env2, effects, value) <- result.map(computation.run(source, env, io))
    #(env2, effects, mapper(value))
  }
  |> EffectfulComputation
}

pub fn update_scope(name, value) -> EffectfulComputation(io, Literal) {
  fn(_src, env, _io) { Ok(#(enviornment.set(env, name, value), [], value)) }
  |> EffectfulComputation
}

pub fn eval_expr(expr: Expr) -> EffectfulComputation(io, Literal) {
  fn(source: String, env: Enviornment, _io) {
    evaluator.eval(expr, env)
    |> result.map_error(fn(err) {
      error_formatter.format_error(
        kind: "Runtime error",
        message: err.message,
        source:,
        at: err.span,
      )
    })
    |> result.map(fn(literal) { #(env, [], literal) })
  }
  |> EffectfulComputation
}

/// Since `EffectfulComputation` is a reader monad, it can expose anything it's reading; eg the source code.
pub fn source() -> EffectfulComputation(io, SourceCode) {
  fn(source, env, _io) { Ok(#(env, [], source)) }
  |> EffectfulComputation
}

/// Since `EffectfulComputation` is a reader monad, it can expose anything it's reading; eg the scope.
pub fn env() -> EffectfulComputation(io, Enviornment) {
  fn(_source, env, _io) { Ok(#(env, [], env)) }
  |> EffectfulComputation
}

/// Since `EffectfulComputation` is a reader monad, it can expose anything it's reading; eg the IO handler.
pub fn io() -> EffectfulComputation(io_output, IO(io_output)) {
  fn(_source, env, io) { Ok(#(env, [], io)) }
  |> EffectfulComputation
}

pub fn println(
  computation: EffectfulComputation(io, Literal),
) -> EffectfulComputation(io, Nil) {
  fn(source: String, env, io) {
    use #(env2, io_outputs, value) <- result.map(computation.run(
      source,
      env,
      io,
    ))
    #(env2, [io.print(literal_to_string(value)), ..io_outputs], Nil)
  }
  |> EffectfulComputation
}

fn literal_to_string(literal: Literal) -> String {
  case literal {
    expr.Boolean(value:) -> bool.to_string(value)
    expr.Nil -> "nil"
    expr.Number(value:) -> builtin.number_to_string(value)
    expr.String(value:) -> "\"" <> value <> "\""
  }
}
