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

- Declaration keywords: `rust`, `use`, `struct`, `enum`, `trait`, `impl`, `fn`, `let`, `const`, `static`, `pub`
- Control flow: `if`, `else`, `while`, `for`, `in`, `loop`, `break`, `continue`, `match`, `return`
- Logical operators: `and`, `or`, `not`
- Language constants: `true`, `false`, `Self`, `_`
- Symbols: `+`, `+=`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `=`, `=>`, `->`, `..`, `..=`, `?`, `!`, `|`, `@`, `::`
- Comments: `//`, `/* ... */`
- Strings: quoted strings, raw strings (`r#"..."#` style), char literals (`'x'`)
- Numbers: decimal, hex (`0x`), binary (`0b`)
