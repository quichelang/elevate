use elevate::data::ordered_set::OrderedSet;

#[test]
fn ordered_set_has_deterministic_iteration() {
    let mut set = OrderedSet::new();
    assert!(set.insert("alpha"));
    assert!(set.insert("beta"));
    assert!(!set.insert("alpha"));
    assert_eq!(
        set.iter().copied().collect::<Vec<_>>(),
        vec!["alpha", "beta"]
    );
}
