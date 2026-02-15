import enviornment
import evaluator
import expr.{Boolean, Number, String}
import expr_parser as parser
import gleam/list
import gleeunit
import parser_combinators.{Success}

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn evaluate_expression_test() {
  let test_cases = [
    // Arithmetic
    #("division by zero returns 0", "1 / 0", Number(0.0)),
    #("addition", "1 + 2", Number(3.0)),
    #("subtraction", "5 - 3", Number(2.0)),
    #("multiplication", "4 * 4", Number(16.0)),
    #("division", "8 / 2", Number(4.0)),
    #("complex arithmetic", "1 + 2 - 3", Number(0.0)),
    // Comparison
    #("less than true", "1 + 2 - 3 < 5", Boolean(True)),
    #("less than false", "10 < 5", Boolean(False)),
    #("greater than", "10 > 5", Boolean(True)),
    #("at most true", "3 <= 3", Boolean(True)),
    #("at most false", "4 <= 3", Boolean(False)),
    #("at least true", "3 >= 3", Boolean(True)),
    #("at least false", "2 >= 3", Boolean(False)),
    // Equality
    #("equality true", "5 == 5", Boolean(True)),
    #("equality false", "5 == 6", Boolean(False)),
    #("not equals true", "5 != 6", Boolean(True)),
    #("not equals false", "5 != 5", Boolean(False)),
    // String concatenation
    #("string + string", "\"hello\" + \"world\"", String("helloworld")),
    #("number + string coercion", "1 + \"hola\"", String("1hola")),
    #("string + number coercion", "\"value: \" + 42", String("value: 42")),
    // Boolean negation
    #("!true", "!true", Boolean(False)),
    #("!false", "!false", Boolean(True)),
    #("double negation", "!!true", Boolean(True)),
    // Numeric negation
    #("negate number", "-5", Number(-5.0)),
    #("double negate number", "--5", Number(5.0)),
    // Boolean comparison (implication semantics)
    #("true <= true (T implies T)", "true <= true", Boolean(True)),
    #("true <= false (T implies F)", "true <= false", Boolean(False)),
    #("false <= true (F implies T)", "false <= true", Boolean(True)),
    #("false <= false (F implies F)", "false <= false", Boolean(True)),
    // Grouping
    #("grouping changes precedence", "(1 + 2) * 3", Number(9.0)),
    #("nested grouping", "((1 + 2))", Number(3.0)),
  ]

  use #(description, input, expected) <- test_each(test_cases)
  assert eval(input) == Ok(expected) as description
}

// ============================================================================
// Helper Functions
// ============================================================================

fn eval(input) {
  let assert Success(parsed_expr, _) = parser.parse(input)
  evaluator.eval(parsed_expr, enviornment.new())
}

fn test_each(test_cases: List(a), run_test: fn(a) -> Nil) -> Nil {
  list.each(test_cases, run_test)
}
