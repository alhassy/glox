import expr.{
  AtMost, Boolean, Divides, Equals, Grouping, LessThan, Literal, Minus,
  NotEquals, Number, Op, Plus, Times,
}
import gleam/list
import gleeunit
import parser
import parser_combinators.{Span, Success}

pub fn main() -> Nil {
  gleeunit.main()
}

/// Dummy span for comparing AST structure (ignores position)
const span = Span(0, 0, 0)

// ============================================================================
// Parser Tests
// ============================================================================

pub fn parse_expression_test() {
  let test_cases = [
    // Literals
    #("number literal", "123", Literal(Number(123.0), span)),
    #("true literal", "true", Literal(Boolean(True), span)),
    #("false literal", "false", Literal(Boolean(False), span)),
    #("nil literal", "nil", Literal(expr.Nil, span)),
    #("string literal", "\"hello\"", Literal(expr.String("hello"), span)),
    // Grouping
    #("grouping", "(123)", Grouping(Literal(Number(123.0), span), span)),
    // Unary
    #(
      "numeric negation",
      "-42",
      Op(expr.NumericNegation, [Literal(Number(42.0), span)], span),
    ),
    #(
      "boolean negation",
      "!true",
      Op(expr.BooleanNegation, [Literal(Boolean(True), span)], span),
    ),
    // Binary - basic
    #(
      "multiplication",
      "2 * 3",
      Op(
        Times,
        [Literal(Number(2.0), span), Literal(Number(3.0), span)],
        span,
      ),
    ),
    #(
      "less than",
      "1 < 2",
      Op(
        LessThan,
        [Literal(Number(1.0), span), Literal(Number(2.0), span)],
        span,
      ),
    ),
    #(
      "equality",
      "1 == 2",
      Op(
        Equals,
        [Literal(Number(1.0), span), Literal(Number(2.0), span)],
        span,
      ),
    ),
    // Binary - left associativity
    #(
      "term left associativity",
      "1 + 2 - 3",
      Op(
        Minus,
        [
          Op(
            Plus,
            [Literal(Number(1.0), span), Literal(Number(2.0), span)],
            span,
          ),
          Literal(Number(3.0), span),
        ],
        span,
      ),
    ),
    #(
      "factor left associativity",
      "1*2/3*4",
      Op(
        Times,
        [
          Op(
            Divides,
            [
              Op(
                Times,
                [Literal(Number(1.0), span), Literal(Number(2.0), span)],
                span,
              ),
              Literal(Number(3.0), span),
            ],
            span,
          ),
          Literal(Number(4.0), span),
        ],
        span,
      ),
    ),
    // Precedence
    #(
      "multiplication before addition",
      "1 * 2 + 3",
      Op(
        Plus,
        [
          Op(
            Times,
            [Literal(Number(1.0), span), Literal(Number(2.0), span)],
            span,
          ),
          Literal(Number(3.0), span),
        ],
        span,
      ),
    ),
    #(
      "factor before term",
      "1 * 2 - 3 / 4 + 5",
      Op(
        Plus,
        [
          Op(
            Minus,
            [
              Op(
                Times,
                [Literal(Number(1.0), span), Literal(Number(2.0), span)],
                span,
              ),
              Op(
                Divides,
                [Literal(Number(3.0), span), Literal(Number(4.0), span)],
                span,
              ),
            ],
            span,
          ),
          Literal(Number(5.0), span),
        ],
        span,
      ),
    ),
    #(
      "comparison before equality",
      "1 * 2  <=  3 / 4    ==   true",
      Op(
        Equals,
        [
          Op(
            AtMost,
            [
              Op(
                Times,
                [Literal(Number(1.0), span), Literal(Number(2.0), span)],
                span,
              ),
              Op(
                Divides,
                [Literal(Number(3.0), span), Literal(Number(4.0), span)],
                span,
              ),
            ],
            span,
          ),
          Literal(Boolean(True), span),
        ],
        span,
      ),
    ),
    // Grouping affects precedence
    #(
      "grouping in multiplication",
      "(123)*4",
      Op(
        Times,
        [Grouping(Literal(Number(123.0), span), span), Literal(Number(4.0), span)],
        span,
      ),
    ),
    // Complex nested expression
    #(
      "complex nested",
      "1 * 2  !=   (2 < (0 - 0))",
      Op(
        NotEquals,
        [
          Op(
            Times,
            [Literal(Number(1.0), span), Literal(Number(2.0), span)],
            span,
          ),
          Grouping(
            Op(
              LessThan,
              [
                Literal(Number(2.0), span),
                Grouping(
                  Op(
                    Minus,
                    [Literal(Number(0.0), span), Literal(Number(0.0), span)],
                    span,
                  ),
                  span,
                ),
              ],
              span,
            ),
            span,
          ),
        ],
        span,
      ),
    ),
    // Whitespace and comments
    #("leading/trailing whitespace", "  123  ", Literal(Number(123.0), span)),
    #(
      "line comment",
      "1 + 2 // this is a comment",
      Op(Plus, [Literal(Number(1.0), span), Literal(Number(2.0), span)], span),
    ),
    #(
      "block comments",
      "1 /* inline */ + /* another */ 2",
      Op(Plus, [Literal(Number(1.0), span), Literal(Number(2.0), span)], span),
    ),
    #(
      "block comment between unary op and operand",
      "! /* hello world */ true",
      Op(expr.BooleanNegation, [Literal(Boolean(True), span)], span),
    ),
  ]

  use #(description, input, expected) <- test_each(test_cases)
  let assert Success(found, _) = parser.parse(input) as description
  assert exprs_equal(found, expected) as description
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Compare two Expr values for structural equality, ignoring spans
fn exprs_equal(a: expr.Expr, b: expr.Expr) -> Bool {
  case a, b {
    Literal(v1, _), Literal(v2, _) -> v1 == v2
    Op(op1, operands1, _), Op(op2, operands2, _) ->
      op1 == op2 && operands_equal(operands1, operands2)
    Grouping(e1, _), Grouping(e2, _) -> exprs_equal(e1, e2)
    _, _ -> False
  }
}

fn operands_equal(a: List(expr.Expr), b: List(expr.Expr)) -> Bool {
  case a, b {
    [], [] -> True
    [x, ..xs], [y, ..ys] -> exprs_equal(x, y) && operands_equal(xs, ys)
    _, _ -> False
  }
}

fn test_each(test_cases: List(a), run_test: fn(a) -> Nil) -> Nil {
  list.each(test_cases, run_test)
}
