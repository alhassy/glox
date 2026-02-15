import expr.{type Literal}
import gleam/list
import gleam/result

type VariableName =
  String

type Value =
  Literal

/// An assocation of variable names to their possible values
pub opaque type Enviornment {
  Enviornment(pairs: List(#(VariableName, Value)))
}

pub fn get(env: Enviornment, name: VariableName) -> Result(Value, Nil) {
  env.pairs |> list.find(fn(p) { p.0 == name }) |> result.map(fn(p) { p.1 })
}

pub fn set(env: Enviornment, name: VariableName, value: Value) -> Enviornment {
  [#(name, value), ..env.pairs] |> Enviornment
}

pub fn new() -> Enviornment {
  [] |> Enviornment
}

pub fn merge_into(incoming: Enviornment, base: Enviornment) -> Enviornment {
  list.append(incoming.pairs, base.pairs) |> Enviornment
}
