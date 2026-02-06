# Editor Syntax Highlighting: Elevate

This repository now includes starter syntax-highlighting plugins for both VSCode and Zed.

## Layout

- VSCode extension: `editor-extensions/vscode-elevate`
- Zed extension: `editor-extensions/zed-elevate`

## VSCode

The VSCode plugin uses a TextMate grammar (`syntaxes/elevate.tmLanguage.json`) and a standard language configuration (`language-configuration.json`) for comments, brackets, and auto-closing behavior.

## Zed

The Zed plugin uses Rust tree-sitter parsing with Elevate-specific highlight query overrides. This is a pragmatic bridge until a dedicated `tree-sitter-elevate` grammar exists.

## Supported Elevate tokens covered

- Keywords: `rust`, `use`, `struct`, `enum`, `impl`, `fn`, `const`, `static`, `pub`
- Control flow: `if`, `else`, `while`, `match`, `return`
- Logical operators: `and`, `or`, `not`
- Booleans: `true`, `false`
- Symbols: `+`, `+=`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `=`, `=>`, `->`, `..`, `..=`, `?`, `!`, `|`, `::`
- Comments: `//`, `/* ... */`
- Strings: quoted strings and raw strings (`r#"..."#` style)
