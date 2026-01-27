import expr.{type Expr, type Literal, Boolean, Nil, Number, String}
import gleam/float
import gleam/result
import gleam/string

type Value =
  Literal

/// An error encounterd during evaluation
type RuntimeError =
  String

/// How do we evaluate `"apple" - 2"? It should be an error, so instead of `Expr -> Value`, our evaluator
/// will be typed `Expr -> Result(Value, RuntimeError)`
pub fn eval(expr: Expr) -> Result(Value, RuntimeError) {
  case expr {
    expr.Literal(l) -> l |> Ok
    expr.Grouping(e) -> eval(e)
    expr.Unary(op, expr:) -> {
      use it <- result.try(eval(expr))
      case it {
        Boolean(value:) if op == expr.BooleanNegation -> Boolean(!value) |> Ok
        Number(value:) if op == expr.NumericNegation ->
          value |> float.negate |> Number |> Ok
        _ -> type_check_error(op, expr)
      }
    }
    //   expr.Unary(op: expr.BooleanNegation, expr:) -> todo
    expr.Binary(op:, left:, right:) -> {
      use ll <- result.try(eval(left))
      use rr <- result.try(eval(right))
      case ll, rr {
        _, _ if op == expr.Equals -> Boolean(ll == rr) |> Ok
        _, _ if op == expr.NotEquals -> Boolean(ll != rr) |> Ok
        // For booleans, `l <= r` means `l implies r`
        Boolean(value: l), Boolean(value: r) if op == expr.AtMost ->
          Boolean(!l || r) |> Ok

        String(value: l), String(value: r) if op == expr.Plus ->
          String(l <> r) |> Ok

        // In `l + r`, if `l` or `r` is a string then coerce the other to be a string too!
        String(value: l), Number(value: r) if op == expr.Plus ->
          String(l <> float.to_string(r)) |> Ok
        Number(value: l), String(value: r) if op == expr.Plus ->
          String(float.to_string(l) <> r) |> Ok

        Number(value: l), Number(value: r) if op == expr.Plus ->
          Number(l +. r) |> Ok
        // Division by 0 is 0 😁
        Number(value: l), Number(value: r) if op == expr.Divides ->
          Number(l /. r) |> Ok
        Number(value: l), Number(value: r) if op == expr.Minus ->
          Number(l -. r) |> Ok
        Number(value: l), Number(value: r) if op == expr.Times ->
          Number(l *. r) |> Ok
        Number(value: l), Number(value: r) if op == expr.AtLeast ->
          { l >=. r }
          |> Boolean
          |> Ok
        Number(value: l), Number(value: r) if op == expr.AtMost ->
          { l <=. r }
          |> Boolean
          |> Ok

        Number(value: l), Number(value: r) if op == expr.GreaterThan ->
          { l >. r }
          |> Boolean
          |> Ok
        Number(value: l), Number(value: r) if op == expr.LessThan ->
          { l <. r }
          |> Boolean
          |> Ok

        _, _ -> type_check_error2(op, left, right)
      }
    }
  }
}

fn type_check_error(op, expr) -> Result(Value, RuntimeError) {
  Error(
    "Canot apply operation "
    <> string.inspect(op)
    <> " to "
    <> string.inspect(expr),
  )
}

fn type_check_error2(op, expr1, expr2) -> Result(Value, RuntimeError) {
  Error(
    "Canot apply operation "
    <> string.inspect(op)
    <> " to args "
    <> string.inspect(expr1)
    <> " and "
    <> string.inspect(expr2),
  )
}
