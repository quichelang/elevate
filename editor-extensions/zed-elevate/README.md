# Elevate Zed Syntax Extension

This extension adds syntax highlighting and language configuration for Elevate source files (`.ers`) in Zed.

## Design

- Uses the Rust tree-sitter grammar as the parsing backend.
- Adds Elevate-specific keyword highlighting via identifier predicates (`rust`, `trait`, `let`, `for`, `in`, `loop`, `break`, `continue`, `and`, `or`, `not`, `Self`).
- Covers all Elevate operators including `-`, `*`, `/`, `%`, `@`, `->`, `=>`, `..`, `..=`, `?`.
- Uses `.ers` as the language file suffix.

This approach keeps the extension lightweight and usable immediately while a dedicated `tree-sitter-elevate` grammar is still pending.

## Local development

1. Open Zed extensions directory:
   `~/.config/zed/extensions`
2. Symlink this extension directory:
   `ln -s /Volumes/Dev/code/jagtesh/elevate/editor-extensions/zed-elevate ~/.config/zed/extensions/elevate`
3. Restart Zed and open a `.ers` file.

## Next step (optional)

For stricter parsing and fully language-native captures, replace `grammar = "rust"` with a dedicated `tree-sitter-elevate` grammar registration once available.
