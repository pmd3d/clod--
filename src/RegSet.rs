//! Sets of physical assembly registers.

use super::Assembly::AsmReg;
use std::collections::BTreeSet;

pub type RegSet = BTreeSet<AsmReg>;

pub fn empty() -> RegSet {
    BTreeSet::new()
}
pub fn add(reg: AsmReg, set: &mut RegSet) {
    set.insert(reg);
}
pub fn union(left: &RegSet, right: &RegSet) -> RegSet {
    left.union(right).copied().collect()
}
