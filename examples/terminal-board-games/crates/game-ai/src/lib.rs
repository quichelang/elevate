#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CandidateMove<M> {
    pub confidence: u8,
    pub move_data: M,
}

pub trait HintEngine<State> {
    type Hint;

    fn hint(&self, state: &State) -> Option<Self::Hint>;
}

pub trait Solver<State> {
    fn solve(&self, state: &State) -> Option<State>;
}

pub fn choose_highest_confidence<M>(
    candidates: impl IntoIterator<Item = CandidateMove<M>>,
) -> Option<CandidateMove<M>> {
    let mut best: Option<CandidateMove<M>> = None;
    for candidate in candidates {
        match best {
            Some(ref current) if current.confidence >= candidate.confidence => {}
            _ => best = Some(candidate),
        }
    }
    best
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn highest_confidence_wins() {
        let best = choose_highest_confidence([
            CandidateMove {
                confidence: 40,
                move_data: "a",
            },
            CandidateMove {
                confidence: 90,
                move_data: "b",
            },
            CandidateMove {
                confidence: 60,
                move_data: "c",
            },
        ])
        .expect("candidate");

        assert_eq!(best.move_data, "b");
    }
}
