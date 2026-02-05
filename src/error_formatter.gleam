// Error handling and formatting utilities for Lox
//
// This module provides:
// 1. Error types (LError) for representing Lox errors
// 2. Error formatting utilities for parser combinators with source context

// Error Handling

// While we’re setting things up, another key piece of infrastructure is error handling. 
// Textbooks sometimes gloss over this because it’s more a practical matter than a formal computer science-y problem. 
// But if you care about making a language that’s actually usable, then handling errors gracefully is vital.

// The tools our language provides for dealing with errors make up a large portion of its user interface. When the user’s code is working, 
// they aren’t thinking about our language at all—their headspace is all about their program. 
// It’s usually only when things go wrong that they notice our implementation.

// When that happens, it’s up to us to give the user all the information they need to understand what went wrong and guide them gently
// back to where they are trying to go. Doing that well means thinking about error handling all through the implementation of our
// interpreter, starting now.

import gleam/int
import gleam/list
import gleam/string
import parser_combinators.{type Span}

// ============================================================================
// Parser Error Formatting
// ============================================================================

/// Format an error message with source context and pointer.
///
/// ## Example output:
/// ```
/// ┌─ Error at line 1, columns 5-9
///   |
/// 1 | 1 + hello
///   |     ^^^^^
///   |     Unknown identifier `hello`. Variables are not supported yet - only numbers, strings, true, false, and nil
/// ```
pub fn format_error(
  kind error_kind: String,
  message message: String,
  source source: String,
  at span: Span,
) -> String {
  let lines = string.split(source, "\n")
  let offending_code = case list.drop(lines, span.line - 1) {
    [l, ..] -> l
    [] -> ""
  }
  // The line number converted to a string (e.g., "1", "42", "123").
  let offending_line = int.to_string(span.line)
  // Spaces equal to the width of the line number.
  // This is used to align the decorative " <lineNumber> | " characters in the error output.
  let err_indent = string.repeat(" ", string.length(offending_line))
  let pointers_padding = string.repeat(" ", span.column - 1)
  let pointers = string.repeat("^", span.length)
  let end_col = span.column + span.length - 1

  // "┌─ Error at line 1, columns 5-9"
  {
    "┌─ " <> error_kind <> " at line "
    <> offending_line
    <> ", column"
    <> case span.length > 1 {
      True ->
        "s " <> int.to_string(span.column) <> "-" <> int.to_string(end_col)
      False -> " " <> int.to_string(span.column)
    }
  }
  <> { "\n" <> err_indent <> " |" }
  // "123 | true + hello"
  <> { "\n" <> offending_line <> " | " <> offending_code }
  //  " |     ^^^^^"
  <> { "\n" <> err_indent <> " | " <> pointers_padding <> pointers }
  <> { "\n" <> err_indent <> " |     " <> message }
}
