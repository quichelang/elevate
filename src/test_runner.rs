use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::ast::{Item, Module};
use crate::CompileOptions;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TestSummary {
    pub source_root: PathBuf,
    pub generated_root: PathBuf,
    pub discovered_tests: usize,
    pub passed_tests: usize,
}

pub fn test_ers_crate(crate_root: &Path, options: &CompileOptions) -> Result<TestSummary, String> {
    let build = crate::crate_builder::transpile_ers_crate_with_options(crate_root, options)?;
    let src_root = build.source_root.join("src");
    let generated_src = build.generated_root.join("src");
    let discovered_tests = inject_test_wrappers(&src_root, &generated_src)?;

    let mut cmd = Command::new("cargo");
    cmd.arg("test")
        .arg("--manifest-path")
        .arg(build.generated_root.join("Cargo.toml"))
        .arg("--target-dir")
        .arg(build.source_root.join("target"));
    let status = cmd
        .status()
        .map_err(|error| format!("failed to run cargo test for generated crate: {error}"))?;
    if !status.success() {
        return Err(format!(
            "cargo test failed for generated crate {}",
            build.generated_root.display()
        ));
    }

    Ok(TestSummary {
        source_root: build.source_root,
        generated_root: build.generated_root,
        discovered_tests,
        passed_tests: discovered_tests,
    })
}

fn inject_test_wrappers(src_root: &Path, generated_src: &Path) -> Result<usize, String> {
    let mut discovered = 0usize;
    inject_dir(src_root, src_root, generated_src, &mut discovered)?;
    Ok(discovered)
}

fn inject_dir(
    root_src: &Path,
    current: &Path,
    generated_src: &Path,
    discovered: &mut usize,
) -> Result<(), String> {
    let entries = fs::read_dir(current)
        .map_err(|error| format!("failed to read directory {}: {error}", current.display()))?;
    for entry in entries {
        let entry = entry
            .map_err(|error| format!("failed to read directory entry in {}: {error}", current.display()))?;
        let path = entry.path();
        if path.is_dir() {
            inject_dir(root_src, &path, generated_src, discovered)?;
            continue;
        }
        if path.extension() != Some(OsStr::new("ers")) {
            continue;
        }
        let source = fs::read_to_string(&path)
            .map_err(|error| format!("failed to read {}: {error}", path.display()))?;
        let module = parse_module(&source, &path)?;
        let tests = discover_tests(&module, &path)?;
        if tests.is_empty() {
            continue;
        }
        *discovered += tests.len();

        let rel = path
            .strip_prefix(root_src)
            .map_err(|error| format!("failed to compute relative source path: {error}"))?;
        let mut generated_file = generated_src.join(rel);
        generated_file.set_extension("rs");
        let existing = fs::read_to_string(&generated_file)
            .map_err(|error| format!("failed to read generated file {}: {error}", generated_file.display()))?;
        let mut wrapped = annotate_test_cfg(&existing, &tests);
        wrapped.push_str(&render_wrappers(&tests));
        fs::write(&generated_file, wrapped.as_bytes()).map_err(|error| {
            format!(
                "failed to append test wrappers to {}: {error}",
                generated_file.display()
            )
        })?;
    }
    Ok(())
}

fn parse_module(source: &str, path: &Path) -> Result<Module, String> {
    let tokens = crate::lexer::lex(source)
        .map_err(|diagnostics| format!("failed to lex {}: {}", path.display(), join_diags(&diagnostics)))?;
    crate::parser::parse_module(tokens)
        .map_err(|diagnostics| format!("failed to parse {}: {}", path.display(), join_diags(&diagnostics)))
}

fn join_diags(diags: &[crate::diag::Diagnostic]) -> String {
    diags
        .iter()
        .map(ToString::to_string)
        .collect::<Vec<_>>()
        .join("; ")
}

fn discover_tests(module: &Module, path: &Path) -> Result<Vec<String>, String> {
    let mut out = Vec::new();
    for item in &module.items {
        match item {
            Item::Function(def) => {
                if def.name.starts_with("test_") {
                    if !def.params.is_empty() {
                        return Err(format!(
                            "test function `{}` in {} must not take parameters",
                            def.name,
                            path.display()
                        ));
                    }
                    out.push(def.name.clone());
                }
            }
            Item::Impl(def) => {
                for method in &def.methods {
                    if method.name.starts_with("test_") {
                        if !method.params.is_empty() {
                            return Err(format!(
                                "test method `{}::{}` in {} must not take parameters",
                                def.target,
                                method.name,
                                path.display()
                            ));
                        }
                        out.push(format!("{}::{}", def.target, method.name));
                    }
                }
            }
            Item::RustUse(_) | Item::Struct(_) | Item::Enum(_) | Item::Const(_) | Item::Static(_) => {}
        }
    }
    Ok(out)
}

fn render_wrappers(tests: &[String]) -> String {
    let mut out = String::new();
    out.push_str("\n#[cfg(test)]\nmod __elevate_tests {\n    use super::*;\n");
    for (index, test) in tests.iter().enumerate() {
        let wrapper = sanitize_wrapper_name(index, test);
        out.push_str("    #[test]\n");
        out.push_str(&format!("    fn {wrapper}() {{\n"));
        out.push_str(&format!("        {test}();\n"));
        out.push_str("    }\n");
    }
    out.push_str("}\n");
    out
}

fn sanitize_wrapper_name(index: usize, test: &str) -> String {
    let mut out = format!("__elevate_test_{index}_");
    for ch in test.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch.to_ascii_lowercase());
        } else {
            out.push('_');
        }
    }
    out
}

fn annotate_test_cfg(source: &str, tests: &[String]) -> String {
    let mut out = source.to_string();
    for test in tests {
        let name = test.rsplit("::").next().unwrap_or(test.as_str());
        let pub_sig = format!("pub fn {name}(");
        let plain_sig = format!("fn {name}(");
        if out.contains(&pub_sig) {
            out = out.replace(
                &pub_sig,
                &format!("#[cfg(test)]\n#[allow(dead_code)]\npub fn {name}("),
            );
            continue;
        }
        out = out.replace(
            &plain_sig,
            &format!("#[cfg(test)]\n#[allow(dead_code)]\nfn {name}("),
        );
    }
    out
}

#[cfg(test)]
mod tests {
    use std::env;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::test_ers_crate;
    use crate::CompileOptions;

    #[test]
    fn test_runner_discovers_and_executes_test_prefixed_functions() {
        let root = create_temp_dir("elevate-test-runner");
        std::fs::create_dir_all(root.join("src")).expect("create src should succeed");
        std::fs::write(
            root.join("Cargo.toml"),
            "[package]\nname = \"mini\"\nversion = \"0.1.0\"\nedition = \"2024\"\n\n[lib]\nname = \"mini\"\npath = \"src/lib.rs\"\n",
        )
        .expect("write manifest should succeed");
        std::fs::write(
            root.join("src/lib.ers"),
            "fn test_math() {\n    assert_eq(2 + 2, 4);\n}\n",
        )
        .expect("write source should succeed");

        let summary = test_ers_crate(&root, &CompileOptions::default()).expect("tests should pass");
        assert_eq!(summary.discovered_tests, 1);

        let generated = std::fs::read_to_string(summary.generated_root.join("src/lib.rs"))
            .expect("generated lib should exist");
        assert!(generated.contains("#[cfg(test)]"));
        assert!(generated.contains("test_math();"));
    }

    fn create_temp_dir(prefix: &str) -> std::path::PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time should be valid")
            .as_nanos();
        let path = env::temp_dir().join(format!("{prefix}-{nanos}"));
        std::fs::create_dir_all(&path).expect("temp dir create should succeed");
        path
    }
}
