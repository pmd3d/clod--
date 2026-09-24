//! Deterministic temporary and label generation.

use std::sync::atomic::{AtomicUsize, Ordering};

pub type Counter = usize;
pub const INITIAL_COUNTER: Counter = 0;

pub fn make_temporary(counter: Counter) -> (Counter, String) {
    (counter + 1, format!("tmp.{counter}"))
}
pub fn make_label(prefix: &str, counter: Counter) -> (Counter, String) {
    (counter + 1, format!("{prefix}.{counter}"))
}
pub fn make_named_temporary(prefix: &str, counter: Counter) -> (Counter, String) {
    make_label(prefix, counter)
}

static SHARED_COUNTER: AtomicUsize = AtomicUsize::new(INITIAL_COUNTER);
pub fn make_temporary_shared() -> String {
    let counter = SHARED_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("tmp.{counter}")
}
pub fn make_label_shared(prefix: &str) -> String {
    let counter = SHARED_COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("{prefix}.{counter}")
}
pub fn make_named_temporary_shared(prefix: &str) -> String { make_label_shared(prefix) }

#[derive(Default, Debug, Clone, Eq, PartialEq)]
pub struct UniqueIds(Counter);
impl UniqueIds {
    pub fn new(counter: Counter) -> Self { Self(counter) }
    pub fn counter(&self) -> Counter { self.0 }
    pub fn next(&mut self, prefix: &str) -> String {
        let (counter, name) = make_label(prefix, self.0);
        self.0 = counter;
        name
    }
    pub fn temporary(&mut self) -> String {
        let (counter, name) = make_temporary(self.0);
        self.0 = counter;
        name
    }
}
