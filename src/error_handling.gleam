
import gleam/io
import gleam/int

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

pub fn error(line: Int, _where: String, message: String) {
  report(line, "", message)
}

/// Tells the user some syntax error occurred on a given line
pub fn report(line: Int, where: String, message: String) {
  io.println("[line " <> int.to_string(line) <> "] Error " <> where <> ": " <> message)
}