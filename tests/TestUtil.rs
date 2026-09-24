#![allow(non_snake_case)]
use clod::ports::{
    DisjointSets::DisjointSets, Rounding::round_up, StringUtil::drop_chars, UniqueIds::UniqueIds,
};
#[test]
fn utilities() {
    assert_eq!(drop_chars(2, "abcd"), "cd");
    assert_eq!(round_up(9, 8), 16);
    let mut ids = UniqueIds::default();
    assert_ne!(ids.next("x"), ids.next("x"));
    let mut sets = DisjointSets::new(3);
    sets.union(0, 2);
    assert_eq!(sets.find(0), sets.find(2));
}
