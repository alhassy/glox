import builtin
import expr.{type Expr, type Literal}
import gleam/list
import gleam/result
import type_error.{type RuntimeError, RuntimeError}

type Value =
  Literal

/// How do we evaluate `"apple" - 2"? It should be an error, so instead of `Expr -> Value`, our evaluator
/// will be typed `Expr -> Result(Value, RuntimeError)`
pub fn eval(e: Expr) -> Result(Value, RuntimeError) {
  case e {
    expr.Variable(span:, ..) -> RuntimeError(message: "NOPE", span:) |> Error
    expr.Literal(l, _span) -> l |> Ok
    expr.Grouping(inner, _span) -> eval(inner)
    expr.Op(op, operands, _span) -> {
      use vals <- result.try(list.try_map(operands, eval))
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
