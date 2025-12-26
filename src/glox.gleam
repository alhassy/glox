import gleam/io
import gleam/string
import error_handling

import argv

// A simple way to load CLI args: gleam add argv

/// reads a line of input from the user on the command line and returns the result
@external(erlang, "io", "get_line")
pub fn read_line(prompt: String) -> String

/// When given a single arg, execute that Lox script; if no args are given, start a REPL session.
pub fn main() {
  // argv.load().arguments returns a List(String) of the arguments
  case argv.load().arguments {
    [] -> run_repl_prompt()
    [path_to_lox_script] -> run_file(path_to_lox_script)
    _ -> io.println("Usage: gleam run [script]")
  }
}

pub fn run_file(path_to_lox_script) {
  io.println("Running Lox code for " <> path_to_lox_script <> " ...")
}

/// Run a prompt where users can enter and execute code one line at a time.
pub fn run_repl_prompt() {
  let line = read_line("lox > ")
  run(line)
}

pub fn run(line) {
  case string.trim(line) {
    "exit" -> Nil
    _ -> {
      error_handling.report(0, "Unknown Command", line)
      run_repl_prompt()
    }
  }
  // TODO
  // List<Token> tokens = scanner.scanTokens();
  // for (Token token : tokens) System.out.println(token);
}