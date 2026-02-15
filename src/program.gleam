/// A Gleam module to work with the following grammar. 
/// ```
/// program        → declaration* EOF ;
/// declaration    → varDecl | statement ;
/// statement      → exprStmt | printStmt 
/// exprStmt       → expression ";"
/// printStmt      → "print" expression ";"
/// varDecl        → "var" IDENTIFIER ( "=" expression )? ";"
/// ```
/// 
import enviornment.{type Enviornment}
import expr.{type Expr}
import gleam/option.{type Option}
import parser_combinators.{type SyntaxError}

// ## Type ##############################################################################

/// The top level of a program isn’t a sequence of imperative statements. 
/// Instead, a program is a set of declarations which all come into being simultaneously. 
/// The implementation declares all of the names before looking at the bodies of any of the functions.
/// As such, we can support mutually-recursive methods or using function names before they're declared.
pub type Program {
  Program(
    declarations: List(Declaration),
    errors: List(SyntaxError),
    enviornment: Enviornment,
  )
}

/// Declarations of methods, classes, and global variables.
pub type Declaration {
  VarDecl(name: String, value: Option(Expr))
  Statement(Statement)
}

pub type Statement {
  /// Denotes `expression ;`
  ExprStatement(expr: Expr)
  // Denotes `print expr;`
  Print(expr: Expr)
}
