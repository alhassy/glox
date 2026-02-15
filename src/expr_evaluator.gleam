import builtin
import enviornment.{type Enviornment}
import expr.{type Expr, type Literal}
import gleam/list
import gleam/result
import type_error.{type RuntimeError, RuntimeError}

type Value =
  Literal

/// How do we evaluate `"apple" - 2"? It should be an error, so instead of `Expr -> Value`, our evaluator
/// will be typed `Expr -> Result(Value, RuntimeError)`
/// 
/// + This does not modify the incoming environment. It only reads from the environment.
pub fn eval(e: Expr, env: Enviornment) -> Result(Value, RuntimeError) {
  case e {
    expr.Variable(name:, span:) ->
      env
      |> enviornment.get(name)
      |> result.map_error(fn(_) {
        RuntimeError(
          message: "Variable `" <> name <> "` is not in scope!",
          span:,
        )
      })
    expr.Literal(l, _span) -> l |> Ok
    expr.Grouping(inner, _span) -> eval(inner, env)
    expr.Op(op, operands, _span) -> {
      use vals <- result.try(list.try_map(operands, eval(_, env)))
      let b = builtin.of_operator(op)
      builtin.eval(b, vals)
      |> result.map_error(fn(_) { type_error.op(op, vals, operands) })
    }
  }
}

/// Re-export for glox.gleam
pub fn number_to_string(n: Float) -> String {
  builtin.number_to_string(n)
}
