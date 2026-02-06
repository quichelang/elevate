(identifier) @variable
(metavariable) @variable
(type_identifier) @type
(primitive_type) @type.builtin
(self) @variable.special
(field_identifier) @property
(shorthand_field_identifier) @property

(call_expression
  function: [
    (identifier) @function
    (scoped_identifier
      name: (identifier) @function)
    (field_expression
      field: (field_identifier) @function.method)
  ])

(generic_function
  function: [
    (identifier) @function
    (scoped_identifier
      name: (identifier) @function)
    (field_expression
      field: (field_identifier) @function.method)
  ])

(function_item name: (identifier) @function.definition)
(function_signature_item name: (identifier) @function.definition)

(macro_invocation
  macro: [
    (identifier) @function.special
    (scoped_identifier
      name: (identifier) @function.special)
  ])

(macro_invocation
  "!" @function.special)

; Assume uppercase names are types/enum constructors.
((identifier) @type
 (#match? @type "^[A-Z]"))

; Assume all-caps names are constants.
((identifier) @constant
 (#match? @constant "^_*[A-Z][A-Z\\d_]*$"))

(enum_variant name: (identifier) @type)

[
  "("
  ")"
  "{"
  "}"
  "["
  "]"
] @punctuation.bracket

(_
  .
  "<" @punctuation.bracket
  ">" @punctuation.bracket)

[
  "."
  ";"
  ","
  "::"
] @punctuation.delimiter

[
  "const"
  "enum"
  "fn"
  "impl"
  "pub"
  "static"
  "struct"
  "use"
] @keyword

((identifier) @keyword
 (#eq? @keyword "rust"))

[
  "else"
  "if"
  "match"
  "return"
  "while"
] @keyword.control

((identifier) @keyword.operator
 (#match? @keyword.operator "^(and|or|not)$"))

[
  (string_literal)
  (raw_string_literal)
  (char_literal)
] @string

(escape_sequence) @string.escape

(integer_literal) @number
(boolean_literal) @boolean

[
  (line_comment)
  (block_comment)
] @comment

[
  "!="
  "+"
  "+="
  "->"
  ".."
  "..="
  ":"
  "<"
  "<="
  "="
  "=="
  "=>"
  ">"
  ">="
  "|"
  "?"
] @operator

(unary_expression "!" @operator)
operator: "/" @operator

(parameter (identifier) @variable.parameter)
