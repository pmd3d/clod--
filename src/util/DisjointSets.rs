//! Disjoint-set helpers.
//!
//! `DisjointSet` mirrors the deliberately small, persistent representation used
//! by the F# compiler: an entry maps an element to its parent and absent entries
//! are roots.  `DisjointSets` is retained as a convenient indexed adapter.

use std::collections::BTreeMap;

pub type DisjointSet<T> = BTreeMap<T, T>;

pub fn init<T>() -> DisjointSet<T> {
    BTreeMap::new()
}

pub fn union<T: Ord>(x: T, y: T, sets: &mut DisjointSet<T>) {
    sets.insert(x, y);
}

pub fn find<T: Ord + Clone>(x: &T, sets: &DisjointSet<T>) -> T {
    match sets.get(x) {
        Some(parent) => find(parent, sets),
        None => x.clone(),
    }
}

pub fn is_empty<T>(sets: &DisjointSet<T>) -> bool {
    sets.is_empty()
}

#[derive(Debug, Clone)]
pub struct DisjointSets {
    sets: DisjointSet<usize>,
    size: usize,
}

impl DisjointSets {
    pub fn new(size: usize) -> Self {
        Self { sets: init(), size }
    }

    pub fn find(&self, x: usize) -> usize {
        assert!(x < self.size, "disjoint-set index out of bounds");
        find(&x, &self.sets)
    }

    pub fn union(&mut self, x: usize, y: usize) {
        assert!(x < self.size && y < self.size, "disjoint-set index out of bounds");
        union(x, y, &mut self.sets);
    }

    pub fn is_empty(&self) -> bool { is_empty(&self.sets) }
}
