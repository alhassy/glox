// Runtime error handling and formatting utilities for Lox
//
// This module provides:
// 1. RuntimeError type for representing evaluation errors with position info
// 2. Declarative type signatures for operators
// 3. Type error diagnosis and message generation

import builtin
import expr.{type Expr, type Literal}
import gleam/list
import gleam/string
import lox_type.{type LoxType}
import parser_combinators.{type Span}

/// A runtime error with position information for nice error display
pub type RuntimeError {
  RuntimeError(message: String, span: Span)
}

/// Create a type error for an operator
pub fn op(
  operator: expr.Operator,
  vals: List(Literal),
  operands: List(Expr),
) -> RuntimeError {
  let op_builtin = builtin.of_operator(operator)
  let op_name = op_builtin.name
  let all_arg_types = all_arg_types(op_builtin)

  case vals, operands {
    // Unary operator error
    [operand_val], [operand] -> {
      let got = operand_val |> lox_type.of_value |> lox_type.to_string
      let expected = arg_types_to_string(op_builtin)
      RuntimeError(
        "Cannot apply '"
          <> op_name
          <> "' to "
          <> got
          <> "; expected "
          <> expected,
        get_span(operand),
      )
    }
    // Binary operator error
    [left_val, right_val], [left_expr, right_expr] -> {
      let left_type = left_val |> lox_type.of_value
      let right_type = right_val |> lox_type.of_value
      let #(error_span, error_message) =
        diagnose_binary_type_error(
          op_name,
          left_type,
          right_type,
          all_arg_types,
          left_expr,
          right_expr,
        )
      RuntimeError(error_message, error_span)
    }
    // Fallback for other arities
    _, _ -> {
      let got_types =
        vals
        |> list.map(fn(v) { v |> lox_type.of_value |> lox_type.to_string })
        |> string.join(", ")
      let assert [first_operand, ..] = operands
      RuntimeError(
        "Cannot apply '" <> op_name <> "' to (" <> got_types <> ")",
        get_span(first_operand),
      )
    }
  }
}

/// Get all arg_types as a list of lists
fn all_arg_types(b: builtin.BuiltIn) -> List(List(LoxType)) {
  b.overloads |> list.map(fn(sig) { sig.arg_types })
}

/// Format accepted types for a built-in
fn arg_types_to_string(b: builtin.BuiltIn) -> String {
  case b.overloads {
    [sig] -> sig.arg_types |> list.map(lox_type.to_string) |> string.join(" or ")
    sigs ->
      sigs
      |> list.map(fn(sig) {
        "("
        <> { sig.arg_types |> list.map(lox_type.to_string) |> string.join(", ") }
        <> ")"
      })
      |> string.join(" or ")
  }
}

/// Get the span from an expression
pub fn get_span(e: Expr) -> Span {
  case e {
    expr.Literal(_, span) -> span
    expr.Op(_, _, span) -> span
    expr.Grouping(_, span) -> span
  }
}

// ============================================================================
// Type Utilities
// ============================================================================

/// Format a list of arg_types as accepted type combinations
pub fn format_accepted_types(all_arg_types: List(List(LoxType))) -> String {
  all_arg_types
  |> list.map(fn(types) {
    case types {
      [left, right] ->
        "("
        <> lox_type.to_string(left)
        <> ", "
        <> lox_type.to_string(right)
        <> ")"
      _ -> ""
    }
  })
  |> list.unique
  |> string.join(" or ")
}

/// Diagnose which operand is wrong and generate a helpful error message
fn diagnose_binary_type_error(
  op_name: String,
  left_type: LoxType,
  right_type: LoxType,
  all_arg_types: List(List(LoxType)),
  left_expr: Expr,
  right_expr: Expr,
) -> #(Span, String) {
  // Check if left type matches any signature's left type
  let left_could_work =
    list.any(all_arg_types, fn(types) {
      case types {
        [expected_left, ..] -> lox_type.matches(left_type, expected_left)
        _ -> False
      }
    })

  // Check if right type matches any signature's right type
  let right_could_work =
    list.any(all_arg_types, fn(types) {
      case types {
        [_, expected_right] -> lox_type.matches(right_type, expected_right)
        _ -> False
      }
    })

  // Get expected types for error message
  let accepted = format_accepted_types(all_arg_types)

  case left_could_work, right_could_work {
    // Left is wrong type
    False, _ -> {
      let expected_left_types =
        all_arg_types
        |> list.filter_map(fn(types) {
          case types {
            [left, ..] -> Ok(left)
            _ -> Error(Nil)
          }
        })
        |> list.unique
        |> list.map(lox_type.to_string)
        |> string.join(" or ")
      #(
        get_span(left_expr),
        "Cannot apply '"
          <> op_name
          <> "' to "
          <> lox_type.to_string(left_type)
          <> "; left operand must be "
          <> expected_left_types,
      )
    }
    // Right is wrong type (left matches some signature)
    True, False -> {
      // Find what right types are valid given the left type
      let valid_right_types =
        all_arg_types
        |> list.filter(fn(types) {
          case types {
            [expected_left, _] -> lox_type.matches(left_type, expected_left)
            _ -> False
          }
        })
        |> list.filter_map(fn(types) {
          case types {
            [_, right] -> Ok(right)
            _ -> Error(Nil)
          }
        })
        |> list.unique
        |> list.map(lox_type.to_string)
        |> string.join(" or ")
      #(
        get_span(right_expr),
        "Cannot apply '"
          <> op_name
          <> "' to "
          <> lox_type.to_string(left_type)
          <> " and "
          <> lox_type.to_string(right_type)
          <> "; right operand must be "
          <> valid_right_types,
      )
    }
    // Both types exist in signatures but combination doesn't work
    True, True -> {
      #(
        get_span(right_expr),
        "Cannot apply '"
          <> op_name
          <> "' to "
          <> lox_type.to_string(left_type)
          <> " and "
          <> lox_type.to_string(right_type)
          <> "; accepted combinations are "
          <> accepted,
      )
    }
  }
}
