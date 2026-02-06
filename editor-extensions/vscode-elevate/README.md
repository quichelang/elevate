# Elevate VSCode Syntax Extension

This extension adds syntax highlighting and editor configuration for Elevate source files (`.ers`).

## Features

- Elevate keyword highlighting (`rust`, `use`, `struct`, `enum`, `impl`, `fn`, `const`, `static`, `pub`)
- Flow/control highlighting (`if`, `else`, `while`, `match`, `return`)
- Logical operators (`and`, `or`, `not`) and symbolic operators (`+`, `+=`, `==`, `!=`, `..`, `..=`, `=>`, `->`, `?`, `!`, etc.)
- Raw strings (`r"..."`, `r#"..."#`, `r##"..."##`, ...)
- Line/block comments (`//`, `/* ... */`)
- Function/type/constant naming scopes for better theming

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
