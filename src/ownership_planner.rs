#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CloneDecision {
    Move,
    Clone { is_hot: bool },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ClonePlannerInput<'a> {
    pub ty: &'a str,
    pub is_consuming: bool,
    pub remaining_conflicting_uses: usize,
    pub forced_clone: bool,
    pub preserve_rebind_move: bool,
    pub clone_candidate: bool,
    pub loop_depth: usize,
}

pub fn decide_clone(input: ClonePlannerInput<'_>) -> CloneDecision {
    if !input.is_consuming || !input.clone_candidate {
        return CloneDecision::Move;
    }
    if input.forced_clone {
        return CloneDecision::Clone { is_hot: false };
    }
    if input.preserve_rebind_move {
        return CloneDecision::Move;
    }
    // Inside a loop body, values consumed in owned position must be cloned
    // because the loop may repeat and the value would be gone after the first
    // iteration.  This replaces the previous LOOP_BODY_WEIGHT inflation
    // approach, which caused false-positive clones on post-loop last-use sites.
    if input.loop_depth > 0 && input.remaining_conflicting_uses > 0 {
        return CloneDecision::Clone {
            is_hot: should_flag_hot_clone(input.ty, input.loop_depth),
        };
    }
    if input.remaining_conflicting_uses <= 1 {
        return CloneDecision::Move;
    }
    CloneDecision::Clone {
        is_hot: should_flag_hot_clone(input.ty, input.loop_depth),
    }
}

pub fn should_flag_hot_clone(ty: &str, loop_depth: usize) -> bool {
    if loop_depth == 0 {
        return false;
    }
    clone_cost_score(ty).saturating_mul(loop_depth) >= 4
}

pub fn clone_cost_score(ty: &str) -> usize {
    let trimmed = ty.trim();
    if parse_tuple_items(trimmed)
        .map(|items| {
            items
                .iter()
                .map(|item| clone_cost_score(item))
                .sum::<usize>()
        })
        .unwrap_or(0)
        > 0
    {
        return parse_tuple_items(trimmed)
            .map(|items| {
                items
                    .iter()
                    .map(|item| clone_cost_score(item))
                    .sum::<usize>()
            })
            .unwrap_or(0);
    }
    let (head, args) = split_type_head_and_args(trimmed);
    let base = last_path_segment(head);
    if is_copy_primitive_type(base) {
        return 0;
    }
    let child_score: usize = args.iter().map(|arg| clone_cost_score(arg)).sum();
    let self_score: usize = match base {
        "String" => 4,
        "Vec" => 4,
        "HashMap" | "BTreeMap" | "HashSet" | "BTreeSet" => 5,
        "Option" | "Result" => 2,
        _ => {
            if is_probably_nominal_type(base) {
                3
            } else {
                1
            }
        }
    };
    self_score.saturating_add(child_score)
}

fn split_type_head_and_args(ty: &str) -> (&str, Vec<&str>) {
    let mut depth = 0usize;
    let mut start = None;
    let mut end = None;
    for (index, ch) in ty.char_indices() {
        match ch {
            '<' => {
                if depth == 0 {
                    start = Some(index);
                }
                depth += 1;
            }
            '>' => {
                if depth > 0 {
                    depth -= 1;
                    if depth == 0 {
                        end = Some(index);
                    }
                }
            }
            _ => {}
        }
    }
    let (Some(start), Some(end)) = (start, end) else {
        return (ty.trim(), Vec::new());
    };
    let head = ty[..start].trim();
    let inner = &ty[start + 1..end];
    (head, split_top_level(inner, ','))
}

fn split_top_level(source: &str, delimiter: char) -> Vec<&str> {
    let mut items = Vec::new();
    let mut depth = 0usize;
    let mut last = 0usize;
    for (index, ch) in source.char_indices() {
        match ch {
            '<' | '(' | '[' => depth += 1,
            '>' | ')' | ']' => {
                if depth > 0 {
                    depth -= 1;
                }
            }
            _ if ch == delimiter && depth == 0 => {
                let item = source[last..index].trim();
                if !item.is_empty() {
                    items.push(item);
                }
                last = index + ch.len_utf8();
            }
            _ => {}
        }
    }
    let tail = source[last..].trim();
    if !tail.is_empty() {
        items.push(tail);
    }
    items
}

fn parse_tuple_items(ty: &str) -> Option<Vec<&str>> {
    let trimmed = ty.trim();
    if !trimmed.starts_with('(') || !trimmed.ends_with(')') {
        return None;
    }
    let inner = &trimmed[1..trimmed.len() - 1];
    Some(split_top_level(inner, ','))
}

fn last_path_segment(path: &str) -> &str {
    path.rsplit("::").next().unwrap_or(path)
}

fn is_copy_primitive_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "i8" | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f32"
            | "f64"
            | "bool"
            | "char"
    )
}

fn is_probably_nominal_type(type_name: &str) -> bool {
    type_name
        .chars()
        .next()
        .is_some_and(|ch| ch.is_ascii_uppercase())
}

#[cfg(test)]
mod tests {
    use super::{CloneDecision, ClonePlannerInput, clone_cost_score, decide_clone};

    #[test]
    fn planner_preserves_move_for_rebind_chain() {
        let decision = decide_clone(ClonePlannerInput {
            ty: "Canvas",
            is_consuming: true,
            remaining_conflicting_uses: 2,
            forced_clone: false,
            preserve_rebind_move: true,
            clone_candidate: true,
            loop_depth: 1,
        });
        assert_eq!(decision, CloneDecision::Move);
    }

    #[test]
    fn planner_marks_large_loop_clone_as_hot() {
        let decision = decide_clone(ClonePlannerInput {
            ty: "Vec<String>",
            is_consuming: true,
            remaining_conflicting_uses: 2,
            forced_clone: false,
            preserve_rebind_move: false,
            clone_candidate: true,
            loop_depth: 1,
        });
        assert_eq!(decision, CloneDecision::Clone { is_hot: true });
    }

    #[test]
    fn planner_clone_cost_prefers_copy_types() {
        assert_eq!(clone_cost_score("i64"), 0);
        assert!(clone_cost_score("String") > clone_cost_score("i64"));
    }
}
