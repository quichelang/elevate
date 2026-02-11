use std::collections::{HashMap, HashSet, VecDeque};

use super::slot_map::{SlotKey, SlotMap};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintEdge<E> {
    pub to: SlotKey,
    pub label: E,
}

#[derive(Debug, Clone)]
pub struct ConstraintGraph<N, E> {
    nodes: SlotMap<N>,
    edges: HashMap<SlotKey, Vec<ConstraintEdge<E>>>,
}

impl<N, E> Default for ConstraintGraph<N, E> {
    fn default() -> Self {
        Self::new()
    }
}

impl<N, E> ConstraintGraph<N, E> {
    pub fn new() -> Self {
        Self {
            nodes: SlotMap::new(),
            edges: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, node: N) -> SlotKey {
        self.nodes.insert(node)
    }

    pub fn get_node(&self, id: SlotKey) -> Option<&N> {
        self.nodes.get(id)
    }

    pub fn get_node_mut(&mut self, id: SlotKey) -> Option<&mut N> {
        self.nodes.get_mut(id)
    }

    pub fn add_edge(&mut self, from: SlotKey, to: SlotKey, label: E) -> bool {
        if !self.nodes.contains_key(from) || !self.nodes.contains_key(to) {
            return false;
        }
        self.edges
            .entry(from)
            .or_default()
            .push(ConstraintEdge { to, label });
        true
    }

    pub fn neighbors(&self, id: SlotKey) -> &[ConstraintEdge<E>] {
        self.edges.get(&id).map(Vec::as_slice).unwrap_or(&[])
    }

    pub fn reachable_bfs(&self, start: SlotKey) -> Vec<SlotKey> {
        if !self.nodes.contains_key(start) {
            return Vec::new();
        }
        let mut seen = HashSet::new();
        let mut queue = VecDeque::new();
        let mut out = Vec::new();

        seen.insert(start);
        queue.push_back(start);

        while let Some(next) = queue.pop_front() {
            out.push(next);
            if let Some(edges) = self.edges.get(&next) {
                for edge in edges {
                    if seen.insert(edge.to) {
                        queue.push_back(edge.to);
                    }
                }
            }
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::ConstraintGraph;

    #[test]
    fn constraint_graph_bfs_is_deterministic() {
        let mut graph = ConstraintGraph::<&str, &str>::new();
        let a = graph.add_node("a");
        let b = graph.add_node("b");
        let c = graph.add_node("c");
        let d = graph.add_node("d");
        assert!(graph.add_edge(a, b, "ab"));
        assert!(graph.add_edge(a, c, "ac"));
        assert!(graph.add_edge(b, d, "bd"));
        assert!(graph.add_edge(c, d, "cd"));
        let visited = graph.reachable_bfs(a);
        assert_eq!(visited, vec![a, b, c, d]);
    }
}
