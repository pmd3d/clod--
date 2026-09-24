//! Structure definitions used when computing C type layout.

use std::collections::BTreeMap;
use std::sync::{Mutex, MutexGuard, OnceLock};

use super::Types::Type;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MemberDef {
    pub member_type: Type,
    pub offset: usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructDef {
    pub alignment: usize,
    pub size: usize,
    pub members: BTreeMap<String, MemberDef>,
}

pub type TypeTable = BTreeMap<String, StructDef>;
static TYPE_TABLE: OnceLock<Mutex<TypeTable>> = OnceLock::new();

fn table() -> MutexGuard<'static, TypeTable> {
    TYPE_TABLE
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

pub fn add_struct_definition(tag: impl Into<String>, definition: StructDef) {
    table().insert(tag.into(), definition);
}
pub fn mem(tag: &str) -> bool { table().contains_key(tag) }
pub fn find(tag: &str) -> StructDef {
    table().get(tag).cloned().unwrap_or_else(|| panic!("structure definition not found: {tag}"))
}
pub fn get_members(tag: &str) -> Vec<MemberDef> {
    let mut members: Vec<_> = find(tag).members.into_values().collect();
    members.sort_by_key(|member| member.offset);
    members
}
pub fn get_member_types(tag: &str) -> Vec<Type> {
    get_members(tag).into_iter().map(|member| member.member_type).collect()
}
pub fn get_table() -> TypeTable { table().clone() }
pub fn set_table(new_table: TypeTable) { *table() = new_table; }
