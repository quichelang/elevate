use std::path::{Path, PathBuf};

use crate::crate_builder::GeneratedSourceLink;
use crate::diagnostics_catalog::{ElevateErrorCode, elevate_catalog_entry};

#[derive(Debug, Clone, PartialEq, Eq)]
struct ParsedRustcDiagnostic {
    level: String,
    code: Option<String>,
    message: String,
    path: Option<PathBuf>,
    line: Option<usize>,
    col: Option<usize>,
}

pub(crate) fn render_backend_build_failure(
    stderr: &str,
    generated_root: &Path,
    generated_links: &[GeneratedSourceLink],
    verbose_backend_diagnostics: bool,
) -> String {
    let diagnostics = parse_rustc_stderr(stderr, generated_root);
    if diagnostics.is_empty() {
        let fallback = elevate_catalog_entry(ElevateErrorCode::E9002);
        let mut out = format!(
            "[{}] {}: {}\nExpected: {}\nActual: {}\nFix: {}",
            fallback.code.as_str(),
            fallback.title,
            fallback.explanation,
            fallback.expected,
            fallback.actual,
            fallback.direct_fix_hint
        );
        if verbose_backend_diagnostics {
            out.push_str("\n\n--- raw rustc/cargo diagnostics ---\n");
            out.push_str(stderr.trim());
        }
        return out;
    }

    let mapped_errors = diagnostics
        .iter()
        .filter(|diag| diag.level == "error")
        .filter(|diag| {
            diag.path
                .as_ref()
                .is_some_and(|path| map_generated_to_source(path, generated_links).is_some())
        })
        .count();
    let total_errors = diagnostics.iter().filter(|diag| diag.level == "error").count();
    let classify_as_ice = mapped_errors > 0 && mapped_errors == total_errors;

    let mut out = String::new();
    if classify_as_ice {
        let ice = elevate_catalog_entry(ElevateErrorCode::E9001);
        out.push_str(&format!(
            "[{}] {}: {}\nExpected: {}\nActual: {}\nFix: {}\n",
            ice.code.as_str(),
            ice.title,
            ice.explanation,
            ice.expected,
            ice.actual,
            ice.direct_fix_hint
        ));
    }

    for diag in diagnostics.iter().filter(|diag| diag.level == "error") {
        let mapped_code = map_rustc_code_to_elevate(diag.code.as_deref(), &diag.message);
        let entry = elevate_catalog_entry(mapped_code);
        let location = diag
            .path
            .as_ref()
            .map(|path| {
                if let Some(mapped) = map_generated_to_source(path, generated_links) {
                    match (diag.line, diag.col) {
                        (Some(line), Some(col)) => {
                            format!("{}:{}:{}", mapped.display(), line, col)
                        }
                        _ => mapped.display().to_string(),
                    }
                } else {
                    match (diag.line, diag.col) {
                        (Some(line), Some(col)) => {
                            format!("{}:{}:{}", path.display(), line, col)
                        }
                        _ => path.display().to_string(),
                    }
                }
            })
            .unwrap_or_else(|| "location unavailable".to_string());

        out.push_str(&format!(
            "\n[{}] {}\nLocation: {}\nExpected: {}\nActual: {}\nWhy: {}\nFix: {}",
            entry.code.as_str(),
            entry.title,
            location,
            entry.expected,
            entry.actual,
            entry.explanation,
            entry.direct_fix_hint,
        ));
        out.push_str(&format!("\nBackend detail: {}", diag.message));
    }

    if verbose_backend_diagnostics {
        out.push_str("\n\n--- raw rustc/cargo diagnostics ---\n");
        out.push_str(stderr.trim());
    }

    out.trim().to_string()
}

fn map_generated_to_source(path: &Path, links: &[GeneratedSourceLink]) -> Option<PathBuf> {
    links
        .iter()
        .find(|link| link.generated_path == path)
        .map(|link| link.source_path.clone())
}

fn map_rustc_code_to_elevate(code: Option<&str>, message: &str) -> ElevateErrorCode {
    match code {
        Some("E0382") => ElevateErrorCode::E3001,
        Some("E0502") => ElevateErrorCode::E3002,
        Some("E0499") => ElevateErrorCode::E3003,
        Some("E0596") => ElevateErrorCode::E3004,
        Some("E0308") => ElevateErrorCode::E2001,
        Some("E0425") => ElevateErrorCode::E1002,
        _ => {
            let lower = message.to_ascii_lowercase();
            if lower.contains("moved") && lower.contains("use of") {
                ElevateErrorCode::E3001
            } else if lower.contains("cannot borrow") && lower.contains("mutable") {
                ElevateErrorCode::E3002
            } else if lower.contains("mismatched types") {
                ElevateErrorCode::E2001
            } else {
                ElevateErrorCode::E9002
            }
        }
    }
}

fn parse_rustc_stderr(stderr: &str, generated_root: &Path) -> Vec<ParsedRustcDiagnostic> {
    let mut out = Vec::new();
    let lines = stderr.lines().collect::<Vec<_>>();
    let mut index = 0usize;
    while index < lines.len() {
        let line = strip_ansi(lines[index]);
        if let Some((level, code, message)) = parse_diagnostic_header(&line) {
            let mut diag = ParsedRustcDiagnostic {
                level,
                code,
                message,
                path: None,
                line: None,
                col: None,
            };
            let mut probe = index + 1;
            while probe < lines.len() {
                let next = strip_ansi(lines[probe]);
                if parse_diagnostic_header(&next).is_some() {
                    break;
                }
                if let Some((path, line_no, col_no)) = parse_location_triplet(&next, generated_root) {
                    diag.path = Some(path);
                    diag.line = Some(line_no);
                    diag.col = Some(col_no);
                    break;
                }
                probe += 1;
            }
            out.push(diag);
            index = probe;
            continue;
        }
        index += 1;
    }
    out
}

fn strip_ansi(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut chars = input.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\u{1b}' && chars.peek() == Some(&'[') {
            chars.next();
            while let Some(next) = chars.next() {
                if next.is_ascii_alphabetic() {
                    break;
                }
            }
            continue;
        }
        out.push(ch);
    }
    out
}

fn parse_diagnostic_header(line: &str) -> Option<(String, Option<String>, String)> {
    let trimmed = line.trim_start();
    let (level, rest) = if let Some(rest) = trimmed.strip_prefix("error") {
        ("error".to_string(), rest)
    } else if let Some(rest) = trimmed.strip_prefix("warning") {
        ("warning".to_string(), rest)
    } else {
        return None;
    };

    let rest = rest.trim_start();
    let mut code = None;
    let mut message_part = rest;

    if let Some(without_bracket) = rest.strip_prefix('[')
        && let Some(end) = without_bracket.find(']')
    {
        code = Some(without_bracket[..end].trim().to_string());
        message_part = &without_bracket[end + 1..];
    }

    let message = message_part.trim_start_matches(':').trim().to_string();
    Some((level, code, message))
}

fn parse_location_triplet(line: &str, generated_root: &Path) -> Option<(PathBuf, usize, usize)> {
    let trimmed = line.trim_start();
    if !trimmed.starts_with("-->") {
        return None;
    }
    let rest = trimmed.trim_start_matches("-->").trim();
    let mut parts = rest.rsplitn(3, ':');
    let col = parts.next()?.parse::<usize>().ok()?;
    let line_no = parts.next()?.parse::<usize>().ok()?;
    let path_part = parts.next()?.trim();
    let path = PathBuf::from(path_part);
    let normalized = if path.is_absolute() {
        path
    } else {
        generated_root.join(path)
    };
    Some((normalized, line_no, col))
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::{map_rustc_code_to_elevate, parse_rustc_stderr, render_backend_build_failure};
    use crate::crate_builder::GeneratedSourceLink;
    use crate::diagnostics_catalog::ElevateErrorCode;

    #[test]
    fn rustc_code_mapping_uses_known_ownership_codes() {
        assert_eq!(
            map_rustc_code_to_elevate(Some("E0382"), "use of moved value"),
            ElevateErrorCode::E3001
        );
        assert_eq!(
            map_rustc_code_to_elevate(Some("E0308"), "mismatched types"),
            ElevateErrorCode::E2001
        );
    }

    #[test]
    fn parser_extracts_error_header_and_location() {
        let stderr = "error[E0382]: use of moved value: `text`\n --> src/lib.rs:9:18\n";
        let parsed = parse_rustc_stderr(stderr, PathBuf::from("/tmp/gen").as_path());
        assert_eq!(parsed.len(), 1);
        assert_eq!(parsed[0].code.as_deref(), Some("E0382"));
        assert_eq!(parsed[0].line, Some(9));
        assert_eq!(parsed[0].col, Some(18));
    }

    #[test]
    fn renderer_classifies_mapped_errors_as_ice() {
        let stderr = "error[E0382]: use of moved value: `text`\n --> src/lib.rs:9:18\n";
        let generated_root = PathBuf::from("/tmp/gen");
        let links = vec![GeneratedSourceLink {
            generated_path: generated_root.join("src/lib.rs"),
            source_path: PathBuf::from("/src/lib.ers"),
        }];
        let rendered = render_backend_build_failure(stderr, &generated_root, &links, false);
        assert!(rendered.contains("[E9001]"));
        assert!(rendered.contains("/src/lib.ers:9:18"));
    }
}
