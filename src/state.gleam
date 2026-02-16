import gleam/result
import type_error.{type RuntimeError}

/// Denotes stateful computations that may fail
/// + i.e., State monad with failures
pub type State(env, value) {
  State(run: fn(env) -> Result(#(env, value), RuntimeError))
}

pub fn return(value: value_type) -> State(env, value_type) {
  State(fn(env) { Ok(#(env, value)) })
}

pub fn return_or_error(result: Result(value, RuntimeError)) -> State(env, value) {
  State(fn(env) { result |> result.map(fn(v) { #(env, v) }) })
}

pub fn then(
  f: State(env, value),
  g: fn(value) -> State(env, value2),
) -> State(env, value2) {
  fn(env0) {
    use #(env1, value1) <- result.try(f.run(env0))
    // Intentionally not eta-reducing the next 2 lines, for readability. 
    use #(env2, value2) <- result.map(g(value1).run(env1))
    #(env2, value2)
  }
  |> State
}

/// Execute the given computations in sequence, collecting their outputs as we go along.
pub fn chain(fs: List(State(env, value))) -> State(env, List(value)) {
  case fs {
    [] -> return([])
    [f, ..fs] -> {
      use value <- then(f)
      use values <- then(chain(fs))
      return([value, ..values])
    }
  }
}

/// Run the given stateful computation, then modify the outgoing enviornment according to `f`
pub fn map_outgoing_env(
  s: State(env, value),
  f: fn(env) -> env,
) -> State(env, value) {
  fn(env) {
    use #(env, value) <- result.map(s.run(env))
    #(f(env), value)
  }
  |> State
}
