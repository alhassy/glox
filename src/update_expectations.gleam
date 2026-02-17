/// Update test expectations for all regression tests.
///
/// Run with: gleam run -m update_expectations
///
/// This reads each test case, runs the Lox code, and writes the actual
/// output back as the expected output. Use this when the interpreter
/// behavior changes intentionally.

import regression_framework
import test_config

pub fn main() {
  regression_framework.update_expectations(test_config.framework())
}
