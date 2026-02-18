/// A generic file-based regression test framework. (tldr: A poor man's "REPL" where REPL explorations are kept-around as specifications of desired behaviour.)
///
/// **This is   expect-tests  for Gleam.**
/// 
/// Snapshot testing allows you to perform assertions without having to write the expectation yourself. 
/// You write a test's input in a .yaml file, then run `gleam update_expectations` and the expected
/// value of the test are generated for you. Then `gleam test` verifies that the actual values match
/// the test file's expectations. 
/// 
/// Imagine doing a `assert f1(input) == expected && f2(input) == expected2 && ... && fN(input) == expectedN`
/// where you don’t have to take care of writing the expected outputs.
/// You instantiate a regression framework for functions f1, ..., fN; then write a bunch of tests by giving only
/// the inputs, then the framework produces the expectations for each test dimension f_i. Moreover, the input
/// and the output dimensions are next to each other so you can easily compare and constrast.
/// 
/// Test cases are stored as `.yaml` files with named dimensions under `expectations:`.
/// The framework is parameterized by dimension names and transform functions.
///
/// ```yaml
/// description: Basic addition test
///
/// input: |-
///   print 1 + 2;
///
/// expectations:
///   parsing: |-
///     Program(...)
///   evaluation: |-
///     PRINTED 3
/// ```
///
/// # Why Description?
/// When tests fail, the description helps identify what the test was checking.
/// Failures show a colored diff (using `gap`) highlighting exactly what changed:
/// ```
/// FAIL (evaluation): test/cases/variable_update.yaml
///   Description: Assignment expression (not yet supported, shows syntax error)
///   Expected:
/// PRINTED 1[4]  ← green highlights what's missing
///   Actual:
/// PRINTED 1[5]  ← red highlights what's different
/// ```
/// Without descriptions, you'd only see the file path and have to open the file
/// to understand what the test is actually verifying.
///
/// # Data Provider Pattern
/// This framework externalizes the "data provider" test pattern. In-code, you'd write:
/// ```gleam
/// let test_cases = [
///   #("one print", "print 2 * 7;", ["PRINTED 14"]),
///   #("two prints", "print 1; print 2;", ["PRINTED 1", "PRINTED 2"]),
/// ]
///
/// use #(description, input, expected) <- list.each(test_cases)
/// let actual = run(input)
/// assert actual == expected as description
/// ```
///
/// YAML files are the externalized version of the same `#(description, input, expected)` tuple:
/// - `description:` → first tuple element (shown on failure)
/// - `input:` → second tuple element (the source code)
/// - `expectations:` → third tuple element (what we compare against)
///
/// **Workflow for in-code data provider:**
/// 1. Write test with description, input, and a **dummy expected** (e.g., `[]`)
/// 2. Run `gleam test` to see the actual output in the failure message
/// 3. Copy-paste the actual value back as expected
/// ```gleam
/// // Step 1: dummy expected
/// #("my new test", "print 1 + 2;", []),
///
/// // Step 2: `gleam test` shows: Expected [] / Actual ["PRINTED 3"]
///
/// // Step 3: copy actual back
/// #("my new test", "print 1 + 2;", ["PRINTED 3"]),
/// ```
///
/// **Why multiple in-code data providers?** Each tests a different dimension:
/// 
/// In-code, you need **separate test functions** for success vs error cases, and for
/// each dimension (eval output, AST structure, formatted errors).
/// 
/// ```gleam
/// // program_eval_success_test: #(desc, input, List(String))
/// #("prints 14", "print 2*7;", ["PRINTED 14"])
///
/// // program_eval_error_test: #(desc, input, List(String)) - but for errors!
/// #("type error", "1 * true;", ["ERROR ..."])
///
/// // program_parser_success_test: #(desc, input, Program) - typed AST!
/// #("parses print", "print 1;", Program([Statement(Print(...))], [], env))
///
/// // program_parser_error_test: #(desc, input, String) - formatted error
/// #("missing semi", "print 1", "┌─ Syntax error...")
/// ``` 
/// **YAML as a unified data provider:** Each YAML file tests **multiple dimensions at once**.
/// The dimensions are defined by the client when creating the framework.
///
/// Dimensions can test typed structures via serialization (snapshot testing):
///   1. Transform input → typed value
///   2. Pretty-print/serialize → string
///   3. Compare string to expected dimension in YAML
///
/// If the strings match, the typed value must be correct.
///
/// One YAML file replaces what would be 2-4 separate in-code data provider entries.
/// The YAML must deserialize to a typed `TestCase`, so you still get validation that
/// the file structure is correct - just at runtime instead of compile-time.
///
/// **When to use each approach:**
/// - **In-code tuples**: Direct typed comparison (e.g., `actual_program == expected_program`).
///   Compile-time checking, but verbose for complex structures.
/// - **YAML files**: Typed comparison via serialization. One file tests multiple dimensions,
///   auto-updatable via `gleam run -m update_expectations`, easier to read/diff.
///
/// # Example Use Cases
///
///   The framework shines when you have:
///   - Multi-dimensional outputs (one input → multiple things to verify)
///   - Human-readable formatted output (error messages, reports, rendered text)
///   - Evolving specifications (behavior expected to change, need easy bulk updates)
///   - Domain-specific inputs (DSLs, configs, queries) where examples serve as documentation
/// 
/// The framework excels when you have **multi-dimensional outputs** where humans need to
/// read and review the specifications. Examples:
///
/// **JSON & Validation** - Input is a JSON payload, and output expectations
/// are the deseralisation of the JSON along with a validation check that happens after
/// deserilisation but is otherwise not written to the snapshot test.
/// 
/// **SQL Query Planner** - One query input, multiple dimensions:
/// - `parsed_ast`: The raw parse tree
/// - `optimized_plan`: After query optimization (filter pushdown, join reordering)
/// - `estimated_cost`: Row counts, estimated milliseconds
/// - `generated_sql`: Back to SQL after optimization
///
/// **Markdown Converter** - One document input, multiple outputs:
/// - `html`: The rendered HTML structure
/// - `accessibility_report`: Nesting depth, heading levels, issues found
/// - `text_extraction`: Plain text with formatting stripped
///
/// **Compiler Error Messages** - One source file, multiple analyses:
/// - `type_inference`: Inferred types for each expression
/// - `error_diagnostic`: The formatted error with source context and arrows
/// - `suggested_fixes`: Actionable suggestions for the user
///
/// **Log Analyzer** - One log dump, multiple insights:
/// - `parsed_entries`: Count, time range, services involved
/// - `pattern_detection`: "CASCADING FAILURE: db → payment → orders → frontend"
/// - `timeline`: ASCII visualization of event propagation
/// - `recommended_actions`: What to fix based on patterns
///
/// The common thread: **multiple meaningful perspectives** on the same input, where
/// exact formatting matters and the test files serve as living documentation.
/// 
///   These examples share:
///   1. Multiple meaningful perspectives on the same input (not just "here's the AST")
///   2. Human-readable formatted output where exact formatting matters
///   3. Cross-cutting concerns (parsing + analysis + rendering + diagnostics)
///   4. Living documentation - the YAML files serve as examples for users/developers
///   5. Graceful evolution - when one dimension changes intentionally, others stay stable
/// 
///   The framework excels when your tests are specifications that humans read, not just correctness checks that machines verify.
///
/// # Snapshot Testing Comparison
///
/// This framework is closely related to snapshot testing—both capture "golden" expected
/// outputs and compare against them. Key differences:
///
/// | Aspect | Classic Snapshots | This Framework |
/// |--------|-------------------|----------------|
/// | Dimensions | Single blob | Multiple named dimensions |
/// | Serialization | Automatic (framework decides) | Explicit (you control format) |
/// | Readability | Often opaque JSON/HTML | Human-curated, readable |
/// | Granularity | All-or-nothing updates | Per-dimension updates |
/// | Intent | Catch unintended changes | Document expected behavior |
///
/// **The philosophical difference:**
/// - Snapshot testing asks: "Did the output change?"
/// - This framework asks: "Does each dimension match our specification?"
///
/// Snapshots are defensive ("alert me if anything changes"). This framework is
/// prescriptive ("here's what each aspect should produce").
///
/// In short: **opinionated snapshot testing** for multi-faceted systems where the
/// test files themselves serve as documentation.
///
/// # The Dedent Problem
///
/// In-code data providers with multiline expected strings are awkward. To keep them
/// readable, you indent them with the surrounding code:
/// ```gleam
/// #(
///   "error on line 2",
///   "1 +\n@ 2",
///   "┌─ Syntax error at line 2, column 1
///        |
///      2 | @ 2
///        | ^
///        |     Expected expression after operator",
/// ),
/// ```
/// But this introduces unwanted leading whitespace! You need a `dedent()` helper
/// to strip the common indentation at runtime:
/// ```gleam
/// assert formatted == dedent(expected_formatted) as description
/// ```
///
/// **YAML eliminates this problem entirely.** The `|-` literal block style preserves
/// exact content without any indentation artifacts:
/// ```yaml
/// expectations:
///   evaluation: |-
///     ┌─ Syntax error at line 2, column 1
///      |
///    2 | @ 2
///      | ^
///      |     Expected expression after operator
/// ```
/// No dedent helper needed - the YAML parser handles it correctly.
///
/// **Workflow for YAML tests:**
/// 1. Create a new `.yaml` file with just `input:` and placeholder expectations:
///    ```yaml
///    description: My new test case
///    input: |-
///      print 1 + 2;
///    expectations:
///      parsing: |-
///        TODO
///      evaluation: |-
///        TODO
///    ```
/// 2. Run `gleam run -m update_expectations` to populate actual values
/// 3. Review the generated expectations to ensure they match intent
/// 4. Run `gleam test` to verify the test passes
///
/// This is the inverse of traditional TDD: you provide the input, let the system
/// compute the expected output, then lock it in as a regression test.
///
/// # Framework Setup
/// To use this framework, create three files. Why three?
///   - `gleam test` only discovers tests in `test/`
///   - `gleam run -m X` only works for modules in `src/`
///   - Both need access to the same framework configuration
///
/// So we need: config in `src/`, test runner in `test/`, CLI updater in `src/`.
///
/// 1. **Test config** (`src/my_test_config.gleam`):
///    Define transform functions and expose the configured framework.
///    ```gleam
///    import regression_framework
///
///    pub fn framework() -> regression_framework.RegressionFramework {
///      regression_framework.new()
///      |> regression_framework.with_test_dir("test/specs")
///      |> regression_framework.with_dimension("parsing", parse_it)
///      |> regression_framework.with_dimension("evaluation", run_it)
///    }
///
///    pub fn parse_it(input: String) -> String { ... }
///    pub fn run_it(input: String) -> String { ... }
///    ```
///
/// 2. **Test runner** (`test/regression_test.gleam`):
///    ```gleam
///    import my_test_config
///    import regression_framework
///
///    pub fn regression_test() {
///      regression_framework.run_tests(my_test_config.framework())
///    }
///    ```
///
/// 3. **Expectation updater** (`src/update_expectations.gleam`):
///    ```gleam
///    import my_test_config
///    import regression_framework
///
///    pub fn main() {
///      regression_framework.update_expectations(my_test_config.framework())
///    }
///    ```
///
/// # File Usage
/// ```gleam
/// let framework =
///   regression_framework.new()
///   |> regression_framework.with_test_dir("test/specs")
///   |> regression_framework.with_dimension("parsing", parse_fn)
///   |> regression_framework.with_dimension("evaluation", eval_fn)
///
/// regression_framework.run_test(framework, "test/specs/my_test.yaml")
/// ```
///
/// # Test Organization
/// Test files are collected recursively from subdirectories. Use folders purely
/// for organization - `test/cases/parsing/` and `test/cases/evaluation/` are all
/// collected together when running `run_all(framework, "test/cases")`.
///
/// # Related Files
///   - src/regression_framework.gleam - Core framework with run_test, update_expectation, run_all, update_all
///   - src/test_config.gleam - Transform functions and configured framework
///   - src/update_expectations.gleam - CLI tool to regenerate expectations
///   - test/regression_test.gleam - Test runner that loads all .yaml files
///   - test/specs/**/*.yaml - Test cases (recursive)
///
/// # CLI Usage:
///   gleam test                         # Run all tests including regression tests
///   gleam run -m update_expectations   # Regenerate expectations when behavior changes
///
/// # YAML Libraries
/// We use `yay` for parsing YAML but NOT `cymbal` for serialization. Why?
/// Cymbal doesn't support YAML literal block style (`|-`), which is essential for
/// readable multiline strings. Instead, cymbal quotes strings and escapes newlines:
///   parsing: "Program(\n  declarations: [..."
/// This makes test files hard to read and diff. Our custom serializer produces:
///   parsing: |-
///     Program(
///       declarations: [...
/// Using cymbal would only save ~5 lines of code anyway, not worth the tradeoff.
/// 
/// # What about Birdie?
///
/// [Birdie](https://github.com/giacomocavalieri/birdie) is Gleam's snapshot testing library.
/// + Here's an [excellent tutorial on Birdie](https://giacomocavalieri.me/writing/testing-can-be-fun-actually).
/// Here's how the two approaches compare:
///
/// **Birdie workflow:**
/// ```gleam
/// // test/my_test.gleam
/// pub fn parse_addition_test() {
///   parse("1 + 2;") |> pprint.format |> birdie.snap(title: "parse addition")
/// }
/// ```
/// ```
/// gleam test                  # First run fails, creates .new file
/// gleam run -m birdie         # Interactive review: accept/reject each snapshot
/// ```
/// Result: `birdie_snapshots/parse_addition.accepted` contains just the output.
///
/// **This framework's workflow:**
/// ```yaml
/// # test/specs/addition.yaml
/// description: Parse addition
/// input: |-
///   1 + 2;
/// expectations:
///   parsing: |-
///     TODO
/// ```
/// ```
/// gleam run -m update_expectations   # Bulk update all expectations
/// gleam test                         # Verify tests pass
/// ```
/// Result: Input and expected output live together in the same YAML file.
///
/// **Key differences:**
/// | Aspect | Birdie | This Framework |
/// |--------|--------|----------------|
/// | Input location | In test code | In YAML file with output |
/// | Snapshot location | Separate `birdie_snapshots/` dir | Same file as input |
/// | Update flow | Interactive (one-by-one) | Bulk (all at once) |
/// | Dimensions | One snap() call per dimension | Multiple dimensions per file |
/// | Review | Accept/reject each change | Review git diff |
///
/// **When to use Birdie:** Single-output tests where input is natural in code.
/// **When to use this framework:** Multi-dimensional tests where input+output
/// together form a specification document.
///
/// **Review phase:**
/// - Birdie has a dedicated interactive review CLI (`gleam run -m birdie`)
///   where you accept/reject each snapshot change one at a time.
/// - This framework relies on `git diff` as the review phase. After running
///   `gleam run -m update_expectations`, you review changes with your normal
///   git workflow (`git diff`, `git add -p`, etc.) before committing.
///
/// **Philosophical difference:**
/// - Birdie: Snapshots are an implementation detail; tests live in code
/// - This framework: YAML files *are* the tests; they serve as documentation/specification
/// 
/// Moreover, whereas Birdie requires unique titles for snapshots,
/// our framework uses plain old text files and so uniqueness is easy to
/// maintain. No need to look into gleam `*_test.gleam` files to ensure titles are unique.
/// Moreover, we can use directories to structure tests so that test file names only need
/// to be unique relative to the current directory! (Not globally unique like in Birdie.)
/// 
/// Unlike Birdie however, our framework requires a bit of setup: 
/// Users need to define which functions the framework will be computing,
/// declare a test file to run the snapshot tests, and declare a file to update the tests.
/// Moreover, this process has to be repeated for each family of interesting test dimensions.
/// In practice, this is not a problem.
/// 
/// Another intersting tool in a similar spirit is OCaml's 
/// [ppx_expect](https://github.com/janestreet/ppx_expect),
/// which is essentially Emac's [`C-u C-x C-e`](https://github.com/alhassy/repl-driven-development) for 
/// ["insert the actual value here"](https://ianthehenry.com/posts/my-kind-of-repl/).
/// 
/// # Personal Usage
/// 
/// I personally like to write a snapshot test to characterize the current behaviour of
/// something, then write a series of patches to change parts of the output until it matches
/// what I think it ought to be. Usually, I'll leave a note in the message like "TODO: This characterises
/// the current behaviour, but ideally X should happen" then axe the note once feature X is implemented.
/// 
/// However, one downside of snapshot testing is that it's too easy to write tests and so too easy
/// to write useless tests. As such, I insist on a "description" for each test to ensure that a high-level
/// description preceeds every test.
/// + Another way to mitigate this is to have some form of semantic equivalence on tests, then the test
///   framework can report an error whenever multiple tests are semantically identical; e.g., they're
///   somehow testing the same thing. This is a tricky problem. For fun, read about e-graphs.
/// 
/// Finally, an immediate benefit of snapshot tests is that they become a corpus of potentially interesting
/// examples of how certain features work ---which is helpful if you forget how those features work, or if
/// someone new is joining the dev team. As such, I tend to put a lot of effort into my snapshot tests (at work, at
/// least) with lots of comments explanining how things work. As a result, the snapshot tests become "live coding environments"
/// similar to Juypter Notebooks or Emacs' Org-babel blocks.
/// 
/// PS. The motivational reason for writing the framework is that I was testing a method called `eval` that produced
/// weird behaviour and I wanted to debug it by looking at the intermediary results of a method called `parse`, however
/// a simple `io.println` was not very helpful since that produced output for all tests. The framework let me define
/// these two dimensions against an input string so that I can see where things went off the rails.
/// 
/// ## Non-use-cases
/// Sometimes `assert actual == expected` cannot be written down practically since `expected` is hundreds of lines.
/// In such cases, you seralize `expected` and the test becomes `assert actual == deseralize("my-file.txt")`. We refer to
/// `my-file.txt` as containing a snapshot of the expectation. If this test fails, then the snapshot is out of date and should be
/// updated, or some code changes should be reverted.
/// 
/// If you need to do this once, then don't use the regression framework. If you need to do something like this many times,
/// then do use the framework.
/// 
import gap
import gap/styled_comparison.{StyledComparison}
import gleam/dict.{type Dict}
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import simplifile

/// https://hexdocs.pm/yay/ :: For parsing YAML
import yay

// =============================================================================
// TYPES
// =============================================================================

/// A test case with input, description, and named expectations.
type TestCase {
  TestCase(
    input: String,
    description: String,
    expectations: Dict(String, String),
  )
}

/// Result of running a single test.
type TestResult {
  Pass(path: String)
  Fail(
    path: String,
    dimension: String,
    description: String,
    expected: String,
    actual: String,
  )
  TestError(path: String, message: String)
}

/// The regression test framework with named dimensions and test directory.
/// Each dimension has a name and a transform function (which produces an output for a test's `input`).
pub type RegressionFramework {
  RegressionFramework(
    test_dir: String,
    dimensions: List(#(String, fn(String) -> String)),
  )
}

// =============================================================================
// FRAMEWORK BUILDER (fluent API)
// =============================================================================

/// Create a new empty regression framework.
/// Use `with_test_dir` and `with_dimension` to configure it.
///
/// Example:
/// ```gleam
/// let framework =
///   regression_framework.new()
///   |> regression_framework.with_test_dir("test/specs")
///   |> regression_framework.with_dimension("parsing", parse_lox)
///   |> regression_framework.with_dimension("evaluation", run_lox)
/// ```
pub fn new() -> RegressionFramework {
  RegressionFramework(test_dir: "", dimensions: [])
}

/// Set the test directory for the framework.
pub fn with_test_dir(
  framework: RegressionFramework,
  dir: String,
) -> RegressionFramework {
  RegressionFramework(..framework, test_dir: dir)
}

/// Add a dimension to the framework.
/// Dimensions are checked in the order they are added.
pub fn with_dimension(
  framework: RegressionFramework,
  name: String,
  transform: fn(String) -> String,
) -> RegressionFramework {
  RegressionFramework(
    ..framework,
    dimensions: list.append(framework.dimensions, [#(name, transform)]),
  )
}

// =============================================================================
// YAML PARSING (deserialization)
// =============================================================================

/// Parse a test file's contents into a TestCase using yay YAML parser.
/// Takes dimension names to know which expectations to extract.
/// Returns Error if the file format is invalid.
fn parse_test_file(
  content: String,
  dimension_names: List(String),
) -> Result(TestCase, String) {
  use docs <- result.try(
    yay.parse_string(content)
    |> result.map_error(fn(_) { "Failed to parse YAML" }),
  )
  use doc <- result.try(case docs {
    [first, ..] -> Ok(first)
    [] -> Error("Empty YAML document")
  })
  let root = yay.document_root(doc)

  use input <- result.try(
    yay.extract_string(root, "input")
    |> result.map_error(extraction_error_to_string),
  )
  use description <- result.try(
    yay.extract_string(root, "description")
    |> result.map_error(extraction_error_to_string),
  )
  use expectations <- result.try(extract_expectations(root, dimension_names))

  Ok(TestCase(input:, description:, expectations:))
}

/// Extract expectations for each dimension from YAML.
/// Missing or empty expectations are treated as empty strings.
fn extract_expectations(
  root: yay.Node,
  dimension_names: List(String),
) -> Result(Dict(String, String), String) {
  dimension_names
  |> list.try_fold(dict.new(), fn(acc, name) {
    let key = "expectations." <> name
    case yay.extract_string(root, key) {
      Ok(value) -> Ok(dict.insert(acc, name, value))
      // Missing or empty expectations are allowed - treat as empty string
      Error(yay.KeyMissing(..)) -> Ok(dict.insert(acc, name, ""))
      Error(yay.KeyValueEmpty(_)) -> Ok(dict.insert(acc, name, ""))
      Error(err) -> Error(extraction_error_to_string(err))
    }
  })
}

/// Convert yay extraction errors to strings
fn extraction_error_to_string(err: yay.ExtractionError) -> String {
  case err {
    yay.KeyMissing(key, failed_at_segment: _) -> "Missing key: " <> key
    yay.KeyValueEmpty(key) -> "Empty value for key: " <> key
    yay.KeyTypeMismatch(key, expected: _, found: _) ->
      "Type mismatch for key: " <> key
    yay.DuplicateKeysDetected(key, keys: _) -> "Duplicate keys: " <> key
  }
}

// =============================================================================
// YAML SERIALIZATION
// =============================================================================

/// Serialize a TestCase to YAML format with literal block style for multiline strings.
/// We use a custom serializer because cymbal doesn't support literal block style (`|-`).
/// Takes dimension names to preserve ordering in output.
/// Empty expectations are omitted from the output.
fn serialize_test_case(
  test_case: TestCase,
  dimension_names: List(String),
) -> String {
  let expectations =
    dimension_names
    |> list.filter_map(fn(name) {
      case dict.get(test_case.expectations, name) {
        Ok(value) if value != "" ->
          Ok("  " <> name <> ": |-\n" <> indent(value, 4))
        _ -> Error(Nil)
      }
    })
    |> string.join("\n")

  "description: "
  <> test_case.description
  <> "\n\ninput: |-\n"
  <> indent(test_case.input, 2)
  <> "\n\nexpectations:\n"
  <> expectations
  <> "\n"
}

/// Indent each line of a string by the given number of spaces.
fn indent(text: String, spaces: Int) -> String {
  let prefix = string.repeat(" ", spaces)
  text
  |> string.split("\n")
  |> list.map(fn(line) { prefix <> line })
  |> string.join("\n")
}

// =============================================================================
// TEST RUNNING
// =============================================================================

/// Run all tests, print results, and panic if any fail.
///
/// This is the main entry point for test runners. It runs all YAML test files
/// in the framework's test directory, prints each result, and panics if any
/// tests fail (which causes `gleam test` to report failure).
///
/// Example usage in `test/regression_test.gleam`:
/// ```gleam
/// import my_test_config
/// import regression_framework
///
/// pub fn regression_test() {
///   regression_framework.run_tests(my_test_config.framework())
/// }
/// ```
pub fn run_tests(framework: RegressionFramework) -> Nil {
  case for_each_test(framework, run_test(framework, _)) {
    Error(err) -> {
      io.println("Failed to read test directory: " <> string.inspect(err))
      panic as "Could not read test cases directory"
    }
    Ok(results) -> {
      // Print results for visibility
      list.each(results, fn(result) { io.println(format_result(result)) })

      // Assert all tests passed
      let failures =
        results
        |> list.filter(fn(r) {
          case r {
            Pass(_) -> False
            _ -> True
          }
        })

      case failures {
        [] -> Nil
        _ -> {
          io.println(
            "\n" <> string.inspect(list.length(failures)) <> " test(s) failed!",
          )
          panic as "Some regression tests failed"
        }
      }
    }
  }
}

/// Run a single test from a file path.
fn run_test(framework: RegressionFramework, path: String) -> TestResult {
  let dimension_names = list.map(framework.dimensions, fn(d) { d.0 })
  case simplifile.read(path) {
    Error(err) -> TestError(path:, message: simplifile.describe_error(err))
    Ok(content) ->
      case parse_test_file(content, dimension_names) {
        Error(msg) -> TestError(path:, message: msg)
        Ok(test_case) -> check_dimensions(framework.dimensions, test_case, path)
      }
  }
}

/// Check all dimensions in order, returning first failure or Pass.
fn check_dimensions(
  dimensions: List(#(String, fn(String) -> String)),
  test_case: TestCase,
  path: String,
) -> TestResult {
  case dimensions {
    [] -> Pass(path:)
    [#(name, transform), ..rest] -> {
      let actual = transform(test_case.input)
      case dict.get(test_case.expectations, name) {
        Error(Nil) ->
          TestError(path:, message: "Missing expectation for: " <> name)
        Ok(expected) ->
          case actual == expected {
            True -> check_dimensions(rest, test_case, path)
            False ->
              Fail(
                path:,
                dimension: name,
                description: test_case.description,
                expected:,
                actual:,
              )
          }
      }
    }
  }
}

// =============================================================================
// EXPECTATION UPDATING
// =============================================================================

/// Update all test expectations and print progress to stdout.
///
/// This is the main entry point for the `gleam run -m update_expectations` CLI.
/// It updates all YAML test files in the framework's test directory by running
/// the transform functions and writing the actual outputs as expected values.
///
/// Prints:
///   - The test directory being updated
///   - Number of tests updated successfully
///   - Any failures with error messages
///
/// Example usage in `src/update_expectations.gleam`:
/// ```gleam
/// import my_test_config
/// import regression_framework
///
/// pub fn main() {
///   regression_framework.update_expectations(my_test_config.framework())
/// }
/// ```
pub fn update_expectations(framework: RegressionFramework) -> Nil {
  io.println("Updating test expectations in " <> framework.test_dir <> "...")

  case for_each_test(framework, update_expectation(framework, _)) {
    Error(err) -> {
      io.println("Failed to read test directory: " <> string.inspect(err))
    }
    Ok(results) -> {
      let #(successes, failures) =
        results
        |> list.partition(fn(r) {
          case r {
            Ok(Nil) -> True
            Error(_) -> False
          }
        })

      io.println(
        "Updated "
        <> string.inspect(list.length(successes))
        <> " test(s) successfully.",
      )

      case failures {
        [] -> Nil
        _ -> {
          io.println(
            string.inspect(list.length(failures))
            <> " test(s) failed to update:",
          )
          list.each(failures, fn(r) {
            case r {
              Error(msg) -> io.println("  - " <> msg)
              Ok(_) -> Nil
            }
          })
        }
      }
    }
  }
}

/// Update all dimension expectations in a test file by running the transform functions.
fn update_expectation(
  framework: RegressionFramework,
  path: String,
) -> Result(Nil, String) {
  let dimension_names = list.map(framework.dimensions, fn(d) { d.0 })
  use content <- result.try(
    simplifile.read(path)
    |> result.map_error(fn(e) {
      "Failed to read " <> path <> ": " <> simplifile.describe_error(e)
    }),
  )
  use test_case <- result.try(
    parse_test_file(content, dimension_names)
    |> result.map_error(fn(e) { path <> ": " <> e }),
  )

  // Compute new expectations for each dimension
  let new_expectations =
    framework.dimensions
    |> list.fold(dict.new(), fn(acc, dim) {
      let #(name, transform) = dim
      dict.insert(acc, name, transform(test_case.input))
    })

  let updated = TestCase(..test_case, expectations: new_expectations)
  let new_content = serialize_test_case(updated, dimension_names)

  simplifile.write(path, new_content)
  |> result.map_error(fn(e) {
    "Failed to write " <> path <> ": " <> simplifile.describe_error(e)
  })
}

// =============================================================================
// FILE DISCOVERY
// =============================================================================

/// Run an operation on all test files in the framework's test directory.
fn for_each_test(
  framework: RegressionFramework,
  operation: fn(String) -> a,
) -> Result(List(a), simplifile.FileError) {
  use paths <- result.try(get_test_files(framework.test_dir))
  Ok(list.map(paths, operation))
}

/// Get all .yaml test files in a directory, recursively.
/// Subdirectories are purely for organization - all tests are collected.
fn get_test_files(dir: String) -> Result(List(String), simplifile.FileError) {
  use entries <- result.try(simplifile.read_directory(dir))
  entries
  |> list.try_fold([], fn(acc, entry) {
    let path = dir <> "/" <> entry
    case simplifile.is_directory(path) {
      Ok(True) -> {
        // Recursively get files from subdirectory
        use sub_files <- result.try(get_test_files(path))
        Ok(list.append(acc, sub_files))
      }
      Ok(False) ->
        case string.ends_with(entry, ".yaml") {
          True -> Ok([path, ..acc])
          False -> Ok(acc)
        }
      Error(err) -> Error(err)
    }
  })
}

// =============================================================================
// OUTPUT FORMATTING
// =============================================================================

/// Format a test result for display.
fn format_result(result: TestResult) -> String {
  case result {
    Pass(path) -> "PASS: " <> path
    Fail(path, dimension, description, expected, actual) -> {
      let StyledComparison(first: expected_styled, second: actual_styled) =
        gap.compare_strings(expected, actual)
        |> gap.to_styled
      "FAIL ("
      <> dimension
      <> "): "
      <> path
      <> "\n  Description: "
      <> description
      <> "\n  Expected:\n"
      <> expected_styled
      <> "\n  Actual:\n"
      <> actual_styled
    }
    TestError(path, message) -> "ERROR: " <> path <> " - " <> message
  }
}
