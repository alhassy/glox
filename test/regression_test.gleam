/// Regression tests for glox using file-based test cases.
///
/// Test cases are stored in test/specs/*.yaml files.
/// Run with: gleam test
/// Update expectations with: gleam run -m update_expectations

import gleeunit
import regression_framework
import test_config

pub fn main() -> Nil {
  gleeunit.main()
}

pub fn regression_test() {
  regression_framework.run_tests(test_config.framework())
}
