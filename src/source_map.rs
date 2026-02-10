use crate::diag::{Diagnostic, Span};

pub fn render_diagnostic(
    diagnostic: &Diagnostic,
    source_name: Option<&str>,
    source: Option<&str>,
) -> String {
    let location = format_location(diagnostic.span, source);
    if let Some(source_name) = source_name {
        format!("{} (at {}:{location})", diagnostic.message, source_name)
    } else {
        format!("{} (at {location})", diagnostic.message)
    }
}

pub fn format_location(span: Span, source: Option<&str>) -> String {
    if span.start == 0 && span.end == 0 {
        return "location unavailable".to_string();
    }
    if let Some(source) = source
        && let Some((sl, sc, el, ec)) = span_line_col(source, span.start, span.end)
    {
        return if sl == el && sc == ec {
            format!("{sl}:{sc}")
        } else if sl == el {
            format!("{sl}:{sc}-{ec}")
        } else {
            format!("{sl}:{sc}-{el}:{ec}")
        };
    }
    format!("byte range {}..{}", span.start, span.end)
}

pub fn span_line_col(
    source: &str,
    start: usize,
    end: usize,
) -> Option<(usize, usize, usize, usize)> {
    if start > source.len() || end > source.len() {
        return None;
    }
    let (sl, sc) = byte_to_line_col(source, start)?;
    let (el, ec) = byte_to_line_col(source, end)?;
    Some((sl, sc, el, ec))
}

pub fn byte_to_line_col(source: &str, byte: usize) -> Option<(usize, usize)> {
    if byte > source.len() {
        return None;
    }
    let mut line = 1usize;
    let mut col = 1usize;
    for (index, ch) in source.char_indices() {
        if index >= byte {
            return Some((line, col));
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    Some((line, col))
}

pub fn byte_to_line_col_clamped(source: &str, byte: usize) -> (usize, usize) {
    byte_to_line_col(source, byte.min(source.len())).unwrap_or((1, 1))
}
