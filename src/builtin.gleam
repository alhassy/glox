import expr.{type Literal, Boolean, Number, String}
import gleam/float
import gleam/int
import gleam/list
import gleam/result
import lox_type.{type LoxType, TAny, TBoolean, TNumber, TString}

pub type BuiltIn {
  BuiltIn(name: String, overloads: List(Overload))
}

pub type Overload {
  Overload(arg_types: List(LoxType), eval: fn(List(Literal)) -> Literal)
}

/// Look up the type signatures for an operator.
pub fn of_operator(op: expr.Operator) -> BuiltIn {
  case op {
    // Unary operators
    expr.BooleanNegation ->
      built_in("!")
      |> overload([TBoolean], fn(args) {
        let assert [Boolean(value:)] = args
        Boolean(!value)
      })
    expr.NumericNegation ->
      built_in("-")
      |> overload([TNumber], fn(args) {
        let assert [Number(value:)] = args
        Number(float.negate(value))
      })
    // Equality works on any types
    expr.Equals ->
      built_in("==")
      |> overload([TAny, TAny], fn(args) {
        let assert [l, r] = args
        Boolean(l == r)
      })
    expr.NotEquals ->
      built_in("!=")
      |> overload([TAny, TAny], fn(args) {
        let assert [l, r] = args
        Boolean(l != r)
      })
    // Arithmetic: number x number
    expr.Minus ->
      built_in("-")
      |> overload([TNumber, TNumber], fn(args) {
        let assert [Number(value: l), Number(value: r)] = args
        Number(l -. r)
      })
    expr.Times ->
      built_in("*")
      |> overload([TNumber, TNumber], fn(args) {
        let assert [Number(value: l), Number(value: r)] = args
        Number(l *. r)
      })
    expr.Divides ->
      built_in("/")
      |> overload([TNumber, TNumber], fn(args) {
        let assert [Number(value: l), Number(value: r)] = args
        Number(l /. r)
      })
    // Plus is overloaded: number+number, string+string, string+number, number+string
    expr.Plus ->
      built_in("+")
      |> overload([TNumber, TNumber], fn(args) {
        let assert [Number(value: l), Number(value: r)] = args
        Number(l +. r)
      })
      |> overload([TString, TString], fn(args) {
        let assert [String(value: l), String(value: r)] = args
        String(l <> r)
      })
      |> overload([TString, TNumber], fn(args) {
        let assert [String(value: l), Number(value: r)] = args
        String(l <> number_to_string(r))
      })
      |> overload([TNumber, TString], fn(args) {
        let assert [Number(value: l), String(value: r)] = args
        String(number_to_string(l) <> r)
      })
    // Comparison: number x number
    expr.LessThan ->
      built_in("<")
      |> overload([TNumber, TNumber], fn(args) {
        let assert [Number(value: l), Number(value: r)] = args
        Boolean(l <. r)
      })
    expr.GreaterThan ->
      built_in(">")
      |> overload([TNumber, TNumber], fn(args) {
        let assert [Number(value: l), Number(value: r)] = args
        Boolean(l >. r)
      })
    // AtLeast/AtMost: number x number, OR boolean x boolean (implication)
    expr.AtLeast ->
      built_in(">=")
      |> overload([TNumber, TNumber], fn(args) {
        let assert [Number(value: l), Number(value: r)] = args
        Boolean(l >=. r)
      })
      |> overload([TBoolean, TBoolean], fn(args) {
        // For booleans, `l >= r` means `l or not r` (reverse implication)
        let assert [Boolean(value: l), Boolean(value: r)] = args
        Boolean(l || !r)
      })
    expr.AtMost ->
      built_in("<=")
      |> overload([TNumber, TNumber], fn(args) {
        let assert [Number(value: l), Number(value: r)] = args
        Boolean(l <=. r)
      })
      |> overload([TBoolean, TBoolean], fn(args) {
        // For booleans, `l <= r` means `l implies r`
        let assert [Boolean(value: l), Boolean(value: r)] = args
        Boolean(!l || r)
      })
  }
}

/// Create a built-in operator builder (no implementations yet)
fn built_in(name: String) -> BuiltIn {
  BuiltIn(name, [])
}

/// Add an overload with the given arg types and eval function.
fn overload(
  b: BuiltIn,
  types: List(LoxType),
  eval: fn(List(Literal)) -> Literal,
) -> BuiltIn {
  BuiltIn(b.name, list.append(b.overloads, [Overload(types, eval)]))
}

/// Convert a float to string, omitting `.0` for whole numbers.
/// e.g., 12.0 -> "12", 12.5 -> "12.5"
pub fn number_to_string(n: Float) -> String {
  let truncated = float.truncate(n)
  case int.to_float(truncated) == n {
    True -> int.to_string(truncated)
    False -> float.to_string(n)
  }
}

/// Evaluate the built-in with the given arguments (assumes type_matches is true)
pub fn eval(b: BuiltIn, args: List(Literal)) -> Result(Literal, Nil) {
  use sig <- result.try(find_matching_signature(b, args))
  Ok(sig.eval(args))
}

/// Find a matching signature for the given arguments
fn find_matching_signature(
  b: BuiltIn,
  args: List(Literal),
) -> Result(Overload, Nil) {
  b.overloads |> list.find(fn(sig) { signature_matches(sig, args) })
}

/// Check if the given arguments match a signature's type
fn signature_matches(sig: Overload, args: List(Literal)) -> Bool {
  list.length(sig.arg_types) == list.length(args)
  && list.zip(sig.arg_types, args)
  |> list.all(fn(pair) { lox_type.matches_value(pair.0, pair.1) })
}
