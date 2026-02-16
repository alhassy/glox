import builtin
import enviornment.{type Enviornment}
import expr.{type Expr, type Literal}
import gleam/list
import gleam/result
import state.{type State, State}
import type_error.{RuntimeError}

type Value =
  Literal

/// How do we evaluate `"apple" - 2"? 
/// 
/// It should be an error, so instead of `Expr -> Value`, our evaluator
/// will be typed `Expr -> Result(Value, RuntimeError)`. 
/// 
/// What about evaluating `x = y = z` which denotes `x = (y = z)` which updates variable `y`
/// to be `z` and also updates `x` to be `z`. So we need to evaluate variables in some incoming
/// scope but also to modify the resulting scope.
/// 
/// Hence, our evaluator is typed `Expr x Environment -> Result(#(Envirionment, Value), RuntimeError)`;
/// luckily for us, this is just the State monad with failures.
/// 
/// + This does not modify the incoming environment. It only reads from the environment.
pub fn eval(e: Expr) -> State(Enviornment, Value) {
  case e {
    expr.Variable(name:, span:) ->
      fn(env) {
        env
        |> enviornment.get(name)
        |> result.map(fn(value) { #(env, value) })
        |> result.map_error(fn(_) {
          RuntimeError(
            message: "Variable `" <> name <> "` is not in scope!",
            span:,
          )
        })
      }
      |> State

    expr.Assign(name, expr, _span) -> {
      use value <- state.then(eval(expr))
      state.return(value)
      |> state.map_outgoing_env(fn(env) { env |> enviornment.set(name, value) })
    }

    expr.Literal(l, _span) -> state.return(l)

    expr.Grouping(inner, _span) -> eval(inner)

    expr.Op(op, operands, _span) -> {
      let b = builtin.of_operator(op)
      use vals <- state.then(state.chain(list.map(operands, eval)))
      builtin.eval(b, vals)
      |> result.map_error(fn(_) { type_error.op(op, vals, operands) })
      |> state.return_or_error
    }
  }
}

/// Re-export for glox.gleam
pub fn number_to_string(n: Float) -> String {
  builtin.number_to_string(n)
}
