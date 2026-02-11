use elevate::data::slot_map::SlotMap;

#[test]
fn slot_map_generation_invalidates_stale_keys() {
    let mut slots = SlotMap::new();
    let first = slots.insert("first");
    assert_eq!(slots.remove(first), Some("first"));
    assert!(slots.get(first).is_none());
    let second = slots.insert("second");
    assert_eq!(first.index, second.index);
    assert_ne!(first.generation, second.generation);
    assert_eq!(slots.get(second), Some(&"second"));
}
