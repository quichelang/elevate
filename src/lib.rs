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
    let typed = passes::lower_to_typed(module);
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
                v;
            }
        "#;

        let output = compile_source(source).expect("expected successful compile");
        assert!(output.rust_code.contains("pub struct Point"));
        assert!(output.rust_code.contains("pub fn id"));
    }
}
