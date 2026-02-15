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
import computation.{type EffectfulComputation, type IO}
import enviornment.{type Enviornment}
import error_formatter
import expr.{type Expr, type Literal, Literal, Nil as LNil}
import expr_parser
import gleam/bool
import gleam/list
import gleam/option.{type Option}
import parser_combinators.{type Parser, type SyntaxError} as parse

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

// ## Parser ##############################################################################

const get = parse.then

pub fn parse(source: String) -> parse.ParseResult(Program) {
  source
  |> parse.input_from_string
  |> program()
}

/// A parser for programs
pub fn program() -> Parser(Program) {
  use #(decls, errors) <- get(parse.synchronize(
    declaration() |> as_lexeme,
    discard_garbage_chars_until_at_parserable_input(),
  ))
  use _ <- get(parse.eof())
  parse.return(Program(decls, errors, enviornment.new()))
}

/// Executes the given parser and ignores whitespace before and after it.
fn as_lexeme(parser: parse.Parser(a)) -> parse.Parser(a) {
  use _ <- parse.get(expr_parser.skip_ws())
  use result <- parse.get(parser)
  use _ <- parse.get(expr_parser.skip_ws())
  parse.return(result)
}

// A parser for declarations
pub fn declaration() -> Parser(Declaration) {
  variable() |> parse.or(statement() |> parse.map(Statement))
}

// A parser for statements
pub fn variable() -> Parser(Declaration) {
  use _ <- get(parse.string("var"))
  use _ <- parse.get(expr_parser.some_ws())
  use name <- get(parse.identifier())
  use value <- get(
    {
      use _ <- parse.get(expr_parser.some_ws())
      // If user places anything after `var <name>`, then it better be `=`! 😁
      use _ <- parse.require(
        parse.string("="),
        "Expected to see an `=` for variable assignment",
      )
      use expr <- parse.require(
        expr_parser.parser(),
        "I expected to see an expression here 😖",
      )
      parse.return(expr)
    }
    |> parse.maybe,
  )
  use _ <- parse.require(
    parse.string(";"),
    "I expected to see a semicolon here 🤔",
  )
  parse.return(VarDecl(name:, value:))
}

// A parser for statements
pub fn statement() -> Parser(Statement) {
  print() |> parse.or(expr())
}

// A parser for `print expr` clauses
pub fn print() -> Parser(Statement) {
  use _ <- get(parse.string("print"))
  use expr <- parse.require(
    expr_parser.parser(),
    "A print clause is of the form ` print <expr>; `, you're missing the expr!",
  )
  use _ <- parse.require(
    parse.string(";"),
    "I expected to see a semicolon here 🤔",
  )
  parse.return(Print(expr))
}

// A parser for `expr;` clauses
pub fn expr() -> Parser(Statement) {
  use expr <- get(expr_parser.parser())
  use _ <- parse.require(
    parse.string(";"),
    "I expected to see a semicolon here 🤔",
  )
  parse.return(ExprStatement(expr))
}

/// Discard tokens we're at a statement boundary. 
/// + After catching a ParseError, using `parse.or`, we’ll call this and
///   then we are hopefully back in sync. 
/// When it works well, we have discarded tokens that would have likely caused cascaded errors anyway,
/// and now we can parse the rest of the file starting at the next statement.
/// 
/// NOTE: This is a Parser(Nil) since this parser only modifies the input stream,
/// without actually parsing anything meaningful.
fn discard_garbage_chars_until_at_parserable_input() -> Parser(Nil) {
  // ⚠️ This method has an implicit precondition: it's called on input that already failed to start a valid declaration.
  // The first character(s) it sees are guaranteed NOT to be the start of a valid declaration 
  // - those would have been parsed by the previous attempt.

  // Get the next char...
  use next_char <- get(parse.pop())
  use is_eof <- get(parse.inspect_input(fn(it) { it.unconsumed == "" }))
  use <- bool.guard(when: is_eof, return: parse.return(Nil))
  // If we're not at the end of the file...
  // and the next char is a semicolon, then we're done error recovery and can parse safely again
  use <- bool.guard(when: next_char == ";", return: parse.return(Nil))
  // OK, so at this point, we know `next_char` is a garbage character, so let's check
  // if we've reached something parserable now.

  // Check if the remaining input starts with a statement keyword (without consuming); 
  // if so, then we're done error recovery and can parse safely again
  let keyword = ["class", "fun", "var", "for", "if", "while", "print", "return"]
  let declaration_boundary =
    keyword
    |> list.map(parse.the_identifier)
    |> parse.ordered_choice
  use at_boundary <- get(declaration_boundary |> parse.peeking)
  use <- bool.guard(when: at_boundary, return: parse.return(Nil))
  // Otherwise, keep consuming tokens until we're at a declaration boundary!
  // negative_lookahead succeeded = no keyword found, keep synchronizing
  discard_garbage_chars_until_at_parserable_input()
}

// ## Evaluator ##############################################################################

pub fn parse_and_evaluate(my_io: IO(io_output), source: String) {
  let parse_result =
    source
    |> parse.input_from_string
    |> program()
  case parse_result {
    parse.Success(found: program, ..) -> eval(my_io, source, program)
    parse.Error(message:, at:, ..) -> [
      error_formatter.format_error(kind: "Syntax error", message:, source:, at:)
      |> my_io.print,
    ]
  }
}

/// Evaluation of a program is the execution of a bunch of statements, which produces side-effects
/// and returns no value. The IO argument is useful for testing purposes
pub fn eval(my_io: IO(io_output), source, program) -> List(io_output) {
  let Program(declarations:, errors:, enviornment:) = program
  // First, print any syntax errors that were recovered from
  let syntax_error_outputs =
    list.map(errors, fn(err) {
      error_formatter.format_error(
        kind: "Syntax error",
        message: err.message,
        source:,
        at: err.at,
      )
      |> my_io.print
    })
  // Then evaluate the successfully parsed declarations
  let #(_env, io_outputs) =
    eval_declarations(declarations)
    |> computation.execute(source, enviornment, my_io)
  // enviornment, my_io, source, 
  list.append(syntax_error_outputs, io_outputs)
}

fn eval_declarations(
  declarations: List(Declaration),
) -> EffectfulComputation(io, Literal) {
  case declarations {
    [] -> computation.return(LNil)
    [decl, ..more] -> {
      use _ <- computation.then(eval_declaration(decl))
      eval_declarations(more)
    }
  }
}

/// 💢 We shouldn't need this!
/// Instead, we should get the span of a variable declaration 
/// and attach it to the `name` of the variable
const dummy_span = parse.Span(0, 0, 0)

fn eval_declaration(
  declaration: Declaration,
) -> EffectfulComputation(io, Literal) {
  case declaration {
    VarDecl(name:, value: initializer) ->
      initializer
      |> option.unwrap(Literal(LNil, dummy_span))
      |> computation.eval_expr
      |> computation.then(fn(value) { computation.update_scope(name, value) })
    Statement(Print(expr:)) ->
      expr
      |> computation.eval_expr
      |> computation.println
      |> computation.map(fn(_) { LNil })
    // Expression statements have no side-effects, yet!
    // However, we should still evaluate them in-case they have errors.
    Statement(ExprStatement(expr:)) -> expr |> computation.eval_expr
  }
}
