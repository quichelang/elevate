use elevate::data::ordered_map::OrderedMap;

#[test]
fn ordered_map_insert_update_remove_and_order() {
    let mut map = OrderedMap::new();
    assert_eq!(map.insert("a", 1), None);
    assert_eq!(map.insert("b", 2), None);
    assert_eq!(map.insert("a", 3), Some(1));
    assert_eq!(map.get(&"a"), Some(&3));
    assert_eq!(map.keys().copied().collect::<Vec<_>>(), vec!["a", "b"]);
    assert_eq!(map.remove(&"a"), Some(3));
    assert_eq!(map.keys().copied().collect::<Vec<_>>(), vec!["b"]);
}
