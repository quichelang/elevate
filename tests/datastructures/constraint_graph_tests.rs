use elevate::data::constraint_graph::ConstraintGraph;

#[test]
fn constraint_graph_reachable_is_stable() {
    let mut graph = ConstraintGraph::<&str, &str>::new();
    let a = graph.add_node("a");
    let b = graph.add_node("b");
    let c = graph.add_node("c");
    let d = graph.add_node("d");
    assert!(graph.add_edge(a, b, "ab"));
    assert!(graph.add_edge(a, c, "ac"));
    assert!(graph.add_edge(c, d, "cd"));
    assert!(graph.add_edge(b, d, "bd"));
    assert_eq!(graph.reachable_bfs(a), vec![a, b, c, d]);
}
