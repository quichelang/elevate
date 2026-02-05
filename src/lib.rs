pub mod ast;
pub mod codegen;
pub mod diag;
pub mod ir;
pub mod lexer;
pub mod parser;
pub mod passes;
pub mod source;

use std::fmt;

use ast::Module;
use diag::Diagnostic;
use ir::lowered::RustModule;
use ir::typed::TypedModule;

#[derive(Debug, Clone)]
pub struct CompilerOutput {
    pub typed: TypedModule,
    pub lowered: RustModule,
    pub rust_code: String,
}

#[derive(Debug, Clone)]
pub struct CompileError {
    pub diagnostics: Vec<Diagnostic>,
}

impl fmt::Display for CompileError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (index, diagnostic) in self.diagnostics.iter().enumerate() {
            if index > 0 {
                writeln!(f)?;
            }
            write!(f, "{diagnostic}")?;
        }
        Ok(())
    }
}

pub fn compile_source(source: &str) -> Result<CompilerOutput, CompileError> {
    let tokens = lexer::lex(source).map_err(|diagnostics| CompileError { diagnostics })?;
    let module = parser::parse_module(tokens).map_err(|diagnostics| CompileError { diagnostics })?;
    compile_ast(&module)
}

pub fn compile_ast(module: &Module) -> Result<CompilerOutput, CompileError> {
    let typed = passes::lower_to_typed(module).map_err(|diagnostics| CompileError { diagnostics })?;
    let lowered = passes::lower_to_rust(&typed);
    let rust_code = codegen::emit_rust_module(&lowered);

    Ok(CompilerOutput {
        typed,
        lowered,
        rust_code,
    })
}

#[cfg(test)]
mod tests {
    use super::compile_source;

    #[test]
    fn compile_smoke_test() {
        let source = r#"
            struct Point { x: i64; y: i64; }

            fn id(v: i64) -> i64 {
                return v;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("pub struct Point"));
        assert!(output.rust_code.contains("pub fn id"));
    }

    #[test]
    fn compile_supports_result_try_and_rust_use_imports() {
        let source = r#"
            rust use std::num::ParseIntError;

            fn parse_i64(input: String) -> Result<i64, ParseIntError> {
                return Result::Ok(1);
            }

            fn parse_value(input: String) -> Result<i64, ParseIntError> {
                const value = parse_i64(input)?;
                return Result::Ok(value);
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("use std::num::ParseIntError;"));
        assert!(output.rust_code.contains("parse_i64(input)?"));
    }

    #[test]
    fn compile_rejects_option_try_outside_option_return() {
        let source = r#"
            fn bad(v: Option<i64>) -> i64 {
                return v?;
            }
        "#;

        let error = compile_source(source).expect_err("expected compile error");
        assert!(
            error
                .to_string()
                .contains("The `?` operator on Option requires the function to return Option")
        );
    }
}
