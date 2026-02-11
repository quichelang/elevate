use elevate::data::union_find::UnionFind;

#[test]
fn union_find_tracks_connectivity() {
    let mut uf = UnionFind::with_size(6);
    uf.union(0, 1);
    uf.union(2, 3);
    assert!(uf.connected(0, 1));
    assert!(uf.connected(2, 3));
    assert!(!uf.connected(0, 3));
    uf.union(1, 2);
    assert!(uf.connected(0, 3));
}
