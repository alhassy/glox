import expr.{type Literal, Boolean, Number, String}

/// Represents a type in our type system
pub type LoxType {
  TNumber
  TString
  TBoolean
  TNil
  TAny
}

/// Format a LoxType as a human-readable string
pub fn to_string(t: LoxType) -> String {
  case t {
    TNumber -> "number"
    TString -> "string"
    TBoolean -> "boolean"
    TNil -> "nil"
    TAny -> "any"
  }
}

/// Convert a Literal value to its LoxType
pub fn of_value(v: Literal) -> LoxType {
  case v {
    Number(_) -> TNumber
    String(_) -> TString
    Boolean(_) -> TBoolean
    expr.Nil -> TNil
  }
}

/// Check if a value matches a type (TAny matches everything)
pub fn matches(value_type: LoxType, expected: LoxType) -> Bool {
  expected == TAny || value_type == expected
}

/// Check if a value matches a type (TAny matches everything)
pub fn matches_value(given_type: LoxType, given_value: Literal) -> Bool {
  given_type == TAny || of_value(given_value) == given_type
}
