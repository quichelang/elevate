#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ElevateErrorCode {
    E1001,
    E1002,
    E2001,
    E2002,
    E3001,
    E3002,
    E3003,
    E3004,
    E9001,
    E9002,
}

impl ElevateErrorCode {
    pub fn as_str(self) -> &'static str {
        match self {
            ElevateErrorCode::E1001 => "E1001",
            ElevateErrorCode::E1002 => "E1002",
            ElevateErrorCode::E2001 => "E2001",
            ElevateErrorCode::E2002 => "E2002",
            ElevateErrorCode::E3001 => "E3001",
            ElevateErrorCode::E3002 => "E3002",
            ElevateErrorCode::E3003 => "E3003",
            ElevateErrorCode::E3004 => "E3004",
            ElevateErrorCode::E9001 => "E9001",
            ElevateErrorCode::E9002 => "E9002",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DiagnosticSeverity {
    Error,
    Warning,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrontendDiagnosticGroup {
    pub elevate_codes: Vec<ElevateErrorCode>,
    pub frontend_code: String,
    pub title: String,
    pub explanation: String,
    pub expected: Option<String>,
    pub actual: Option<String>,
    pub direct_fix_hint: Option<String>,
    pub severity: DiagnosticSeverity,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FrontendDiagnosticProfile {
    pub language: String,
    pub groups: Vec<FrontendDiagnosticGroup>,
    pub passthrough_unmapped: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolvedCatalogEntry {
    pub language: String,
    pub code: String,
    pub severity: DiagnosticSeverity,
    pub title: String,
    pub explanation: String,
    pub expected: String,
    pub actual: String,
    pub direct_fix_hint: String,
    pub source_code: ElevateErrorCode,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ErrorCatalogEntry {
    pub language: &'static str,
    pub code: ElevateErrorCode,
    pub severity: DiagnosticSeverity,
    pub title: &'static str,
    pub explanation: &'static str,
    pub expected: &'static str,
    pub actual: &'static str,
    pub direct_fix_hint: &'static str,
}

pub fn elevate_error_catalog() -> Vec<ErrorCatalogEntry> {
    vec![
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E1001,
            severity: DiagnosticSeverity::Error,
            title: "Unexpected token",
            explanation: "The parser found a token that does not fit the grammar at this position.",
            expected: "A valid Elevate grammar token for this construct.",
            actual: "A token sequence that cannot continue the current parse rule.",
            direct_fix_hint: "Check delimiters and punctuation near the span; ensure blocks, tuples, and calls are closed correctly.",
        },
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E1002,
            severity: DiagnosticSeverity::Error,
            title: "Missing declaration",
            explanation: "A symbol is used before it is declared in visible scope.",
            expected: "A known function/type/value path available in scope.",
            actual: "An unknown symbol or unresolved path.",
            direct_fix_hint: "Define the symbol, import it, or fix the path spelling.",
        },
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E2001,
            severity: DiagnosticSeverity::Error,
            title: "Type mismatch",
            explanation: "An expression type does not match the required type for this position.",
            expected: "A value that matches the receiving type.",
            actual: "A value whose type cannot be coerced safely.",
            direct_fix_hint: "Align both sides of the operation with explicit conversion or consistent annotations.",
        },
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E2002,
            severity: DiagnosticSeverity::Error,
            title: "Capability mismatch",
            explanation: "A call requires an ownership/capability mode not provided by the current value.",
            expected: "Ownership or borrowing mode compatible with the callee signature.",
            actual: "A mode that would violate capability or mutability constraints.",
            direct_fix_hint: "Pass by value or adjust function signatures to match read-only vs mutable intent.",
        },
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E3001,
            severity: DiagnosticSeverity::Error,
            title: "Value moved and then reused",
            explanation: "A non-copy value is consumed, then used again later.",
            expected: "Either one consume, or an explicit clone/borrow before reuse.",
            actual: "A second use after ownership has been moved.",
            direct_fix_hint: "Use a read-only view for non-consuming calls, or clone explicitly before the consuming call.",
        },
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E3002,
            severity: DiagnosticSeverity::Error,
            title: "Conflicting borrows",
            explanation: "A mutable access overlaps with an active read-only access to the same value.",
            expected: "Non-overlapping access windows for read-only and mutable use.",
            actual: "A mutable and read-only borrow live at the same time.",
            direct_fix_hint: "Reorder operations so read-only views are no longer needed before mutation.",
        },
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E3003,
            severity: DiagnosticSeverity::Error,
            title: "Multiple mutable borrows",
            explanation: "The same value is mutably borrowed more than once concurrently.",
            expected: "Single active mutable access at a time.",
            actual: "Overlapping mutable access scopes.",
            direct_fix_hint: "Split the mutation sequence or narrow each mutable scope.",
        },
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E3004,
            severity: DiagnosticSeverity::Error,
            title: "Mutating read-only value",
            explanation: "A mutation is attempted through a read-only view.",
            expected: "A mutable capability for this value at this point.",
            actual: "Read-only access path used for mutation.",
            direct_fix_hint: "Pass by value or mutable access where mutation is intended.",
        },
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E9001,
            severity: DiagnosticSeverity::Error,
            title: "Invalid backend lowering",
            explanation: "Elevate produced Rust that rustc rejected after frontend/type checks succeeded.",
            expected: "Backend output that satisfies rustc borrow and type rules.",
            actual: "Generated Rust violates backend correctness contract.",
            direct_fix_hint: "Report this as an Elevate compiler bug and attach the generated crate plus diagnostic output.",
        },
        ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E9002,
            severity: DiagnosticSeverity::Error,
            title: "Backend translation gap",
            explanation: "A rustc diagnostic could not be mapped to a user-level source span.",
            expected: "A generated-location to source-location mapping.",
            actual: "Missing or ambiguous backend/source map relation.",
            direct_fix_hint: "Enable verbose backend diagnostics and report the raw rustc output.",
        },
    ]
}

pub fn elevate_catalog_entry(code: ElevateErrorCode) -> ErrorCatalogEntry {
    elevate_error_catalog()
        .into_iter()
        .find(|entry| entry.code == code)
        .unwrap_or_else(|| ErrorCatalogEntry {
            language: "elevate",
            code: ElevateErrorCode::E9002,
            severity: DiagnosticSeverity::Error,
            title: "Backend translation gap",
            explanation: "A rustc diagnostic could not be mapped to a user-level source span.",
            expected: "A generated-location to source-location mapping.",
            actual: "Missing or ambiguous backend/source map relation.",
            direct_fix_hint: "Enable verbose backend diagnostics and report the raw rustc output.",
        })
}

pub fn resolve_catalog_entry_for_frontend(
    code: ElevateErrorCode,
    profile: Option<&FrontendDiagnosticProfile>,
) -> Option<ResolvedCatalogEntry> {
    if let Some(profile) = profile
        && let Some(group) = profile
            .groups
            .iter()
            .find(|group| group.elevate_codes.contains(&code))
    {
        return Some(ResolvedCatalogEntry {
            language: profile.language.clone(),
            code: group.frontend_code.clone(),
            severity: group.severity,
            title: group.title.clone(),
            explanation: group.explanation.clone(),
            expected: group
                .expected
                .clone()
                .unwrap_or_else(|| "Behavior expected by this frontend".to_string()),
            actual: group
                .actual
                .clone()
                .unwrap_or_else(|| "Operation is invalid in this frontend context".to_string()),
            direct_fix_hint: group.direct_fix_hint.clone().unwrap_or_else(|| {
                "Adjust the source-level construct that triggered this backend translation issue."
                    .to_string()
            }),
            source_code: code,
        });
    }

    if profile.is_some_and(|profile| !profile.passthrough_unmapped) {
        return None;
    }

    let entry = elevate_catalog_entry(code);
    Some(ResolvedCatalogEntry {
        language: entry.language.to_string(),
        code: entry.code.as_str().to_string(),
        severity: entry.severity,
        title: entry.title.to_string(),
        explanation: entry.explanation.to_string(),
        expected: entry.expected.to_string(),
        actual: entry.actual.to_string(),
        direct_fix_hint: entry.direct_fix_hint.to_string(),
        source_code: code,
    })
}

pub fn infer_elevate_code_from_message(message: &str) -> ElevateErrorCode {
    let lower = message.to_ascii_lowercase();
    if lower.contains("unexpected") && lower.contains("token") {
        ElevateErrorCode::E1001
    } else if lower.contains("unknown function")
        || lower.contains("unknown method")
        || lower.contains("unresolved")
    {
        ElevateErrorCode::E1002
    } else if lower.contains("mismatch") && lower.contains("type") {
        ElevateErrorCode::E2001
    } else if lower.contains("capability") || lower.contains("borrow") {
        ElevateErrorCode::E2002
    } else if lower.contains("moved") && lower.contains("use") {
        ElevateErrorCode::E3001
    } else if lower.contains("cannot borrow") && lower.contains("mutable") {
        ElevateErrorCode::E3002
    } else if lower.contains("multiple mutable") {
        ElevateErrorCode::E3003
    } else if lower.contains("read-only") && lower.contains("mut") {
        ElevateErrorCode::E3004
    } else {
        ElevateErrorCode::E9002
    }
}

pub fn quiche_default_profile() -> FrontendDiagnosticProfile {
    FrontendDiagnosticProfile {
        language: "quiche".to_string(),
        groups: vec![
            FrontendDiagnosticGroup {
                elevate_codes: vec![ElevateErrorCode::E1001],
                frontend_code: "Q-SYNTAX-001".to_string(),
                title: "Invalid syntax".to_string(),
                explanation: "This source fragment does not match Quiche syntax.".to_string(),
                expected: Some("A valid Quiche statement or expression".to_string()),
                actual: Some("A token sequence the parser cannot continue".to_string()),
                direct_fix_hint: Some(
                    "Fix nearby punctuation/indentation and verify block structure.".to_string(),
                ),
                severity: DiagnosticSeverity::Error,
            },
            FrontendDiagnosticGroup {
                elevate_codes: vec![
                    ElevateErrorCode::E2001,
                    ElevateErrorCode::E2002,
                    ElevateErrorCode::E3001,
                    ElevateErrorCode::E3002,
                    ElevateErrorCode::E3003,
                    ElevateErrorCode::E3004,
                ],
                frontend_code: "Q-SEM-001".to_string(),
                title: "Invalid operation".to_string(),
                explanation:
                    "This operation is not valid in Quiche's simplified source semantics."
                        .to_string(),
                expected: Some("An operation allowed by Quiche runtime/ownership rules".to_string()),
                actual: Some("An operation requiring unsupported ownership/type behavior".to_string()),
                direct_fix_hint: Some(
                    "Rewrite the expression using Quiche-native control/data-flow constructs."
                        .to_string(),
                ),
                severity: DiagnosticSeverity::Error,
            },
        ],
        passthrough_unmapped: true,
    }
}

#[cfg(test)]
mod tests {
    use super::{
        DiagnosticSeverity, ElevateErrorCode, FrontendDiagnosticGroup, FrontendDiagnosticProfile,
        resolve_catalog_entry_for_frontend,
    };

    #[test]
    fn frontend_profile_can_group_multiple_elevate_codes() {
        let profile = FrontendDiagnosticProfile {
            language: "quiche".to_string(),
            groups: vec![FrontendDiagnosticGroup {
                elevate_codes: vec![ElevateErrorCode::E2001, ElevateErrorCode::E2002],
                frontend_code: "Q-TYPE-001".to_string(),
                title: "Invalid expression".to_string(),
                explanation: "This expression is not allowed in Quiche semantics.".to_string(),
                expected: Some("A valid Quiche expression shape".to_string()),
                actual: Some("An operation requiring unsupported type details".to_string()),
                direct_fix_hint: Some(
                    "Rewrite the expression using Quiche-native constructs.".to_string(),
                ),
                severity: DiagnosticSeverity::Error,
            }],
            passthrough_unmapped: true,
        };

        let resolved = resolve_catalog_entry_for_frontend(ElevateErrorCode::E2002, Some(&profile))
            .expect("profile should resolve grouped code");
        assert_eq!(resolved.language, "quiche");
        assert_eq!(resolved.code, "Q-TYPE-001");
        assert_eq!(resolved.source_code, ElevateErrorCode::E2002);
    }

    #[test]
    fn frontend_profile_can_disable_unmapped_passthrough() {
        let profile = FrontendDiagnosticProfile {
            language: "quiche".to_string(),
            groups: Vec::new(),
            passthrough_unmapped: false,
        };

        let resolved = resolve_catalog_entry_for_frontend(ElevateErrorCode::E1002, Some(&profile));
        assert!(resolved.is_none());
    }

    #[test]
    fn infer_code_from_message_defaults_for_frontend_bridge() {
        assert_eq!(
            super::infer_elevate_code_from_message("Unexpected token `:`"),
            ElevateErrorCode::E1001
        );
        assert_eq!(
            super::infer_elevate_code_from_message("Type mismatch in assignment"),
            ElevateErrorCode::E2001
        );
    }
}
