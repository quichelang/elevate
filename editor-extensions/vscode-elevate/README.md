# Elevate VSCode Syntax Extension

This extension adds syntax highlighting and editor configuration for Elevate source files (`.ers`).

## Features

- Declaration keywords: `rust`, `use`, `struct`, `enum`, `trait`, `impl`, `fn`, `let`, `const`, `static`, `pub`
- Control flow: `if`, `else`, `while`, `for`, `in`, `loop`, `break`, `continue`, `match`, `return`
- Logical operators: `and`, `or`, `not`
- Language constants: `true`, `false`, `Self`, `_`
- Symbolic operators: `+`, `+=`, `-`, `*`, `/`, `%`, `==`, `!=`, `<`, `<=`, `>`, `>=`, `..`, `..=`, `=>`, `->`, `?`, `!`, `|`, `@`
- Strings: quoted strings, raw strings (`r#"..."#` style), char literals (`'x'`)
- Numbers: decimal, hex (`0x`), binary (`0b`)
- Comments: line (`//`) and block (`/* ... */`)
- Function/type/constant/macro naming scopes for better theming

## Local development

1. Open this folder in VSCode:
   `editor-extensions/vscode-elevate`
2. Press `F5` to launch an Extension Development Host.
3. Open any `.ers` file to verify highlighting.

## Packaging

1. Install the packaging tool:
   `npm install -g @vscode/vsce`
2. Package the extension:
   `cd editor-extensions/vscode-elevate && vsce package`
