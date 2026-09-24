//! Operations corresponding to the helpers in `ListUtil.fs`.

use std::cmp::Ordering;

pub fn try_max<T>(items: &[T], mut compare: impl FnMut(&T, &T) -> Ordering) -> Option<&T> {
    items.iter().max_by(|a, b| compare(a, b))
}

pub fn try_min<T>(items: &[T], mut compare: impl FnMut(&T, &T) -> Ordering) -> Option<&T> {
    items.iter().min_by(|a, b| compare(a, b))
}

pub fn make_list<T: Clone>(len: usize, value: T) -> Vec<T> { vec![value; len] }
pub fn try_last<T>(items: &[T]) -> Option<&T> { items.last() }

pub fn take<T: Clone>(count: isize, items: &[T]) -> Vec<T> {
    items.iter().take(count.max(0) as usize).cloned().collect()
}

pub fn take_drop<T: Clone>(count: isize, items: &[T]) -> (Vec<T>, Vec<T>) {
    let split = (count.max(0) as usize).min(items.len());
    (items[..split].to_vec(), items[split..].to_vec())
}
