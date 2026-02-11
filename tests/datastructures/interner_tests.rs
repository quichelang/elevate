use elevate::data::interner::StringInterner;

#[test]
fn interner_deduplicates_text() {
    let mut interner = StringInterner::new();
    let a = interner.get_or_intern("name");
    let b = interner.get_or_intern("other");
    let again = interner.get_or_intern("name");
    assert_eq!(a, again);
    assert_ne!(a, b);
    assert_eq!(interner.resolve(a), Some("name"));
    assert_eq!(interner.resolve(b), Some("other"));
}
