# glox: Gleam-based implementation of Lox

This is a fun project to learn Gleam, following https://craftinginterpreters.com/

# Usage

```sh
gleam run              # Start the glox REPL
gleam run my_file.lox  # Execute the Lox script `my_file.lox`
gleam test             # Run the tests
```

# Test the scanner
```sh
gleam run # Start the repl

lox > ,;{<=
  Ok([Punctuation(Comma, 0), Punctuation(Semicolon, 0), Punctuation(LeftBrace, 0), Operator(AtMost, 0)])

lox > +/*-
  Ok([Operator(Plus, 0), Operator(Division, 0), Operator(Times, 0), Operator(Minus, 0)])

lox > !*+-/=<> <= == // operators
  Ok([Operator(Negation, 0), Operator(Times, 0), Operator(Plus, 0), Operator(Minus, 0), Operator(Division, 0), Punctuation(Assignment, 0), Operator(LessThan, 0), Operator(GreaterThan, 0), Punctuation(Whitespace, 0), Operator(AtMost, 0), Punctuation(Whitespace, 0), Operator(Equal, 0), Punctuation(Whitespace, 0), Punctuation(Comment, 0)])  

lox > (( )){} // grouping stuff
  Ok([Punctuation(LeftParen, 0), Punctuation(LeftParen, 0), Punctuation(Whitespace, 0), Punctuation(RightParen, 0), Punctuation(RightParen, 0), Punctuation(LeftBrace, 0), Punctuation(RightBrace, 0), Punctuation(Whitespace, 0), Punctuation(Comment, 0)])  

lox > "Hello" + "World"
  Ok([Literal(String("Hello"), 0), Punctuation(Whitespace, 0), Operator(Plus, 0), Punctuation(Whitespace, 0), Literal(String("World"), 0)])
```

The third line above shows the resulting scanned tokens 😁

These examples are also codified as tests that can be executed with `gleam test`.