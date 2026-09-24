//! Assembly-level symbol information accumulated by code generation.
#![allow(non_snake_case)]

use super::Assembly::{AsmReg, AsmType};
use super::RegSet::RegSet;
use std::collections::BTreeMap;
use std::sync::{Mutex, MutexGuard, OnceLock};

#[derive(Clone, Debug, PartialEq)]
pub struct AsmFunEntry {
    pub defined: bool,
    pub bytes_required: i32,
    pub return_on_stack: bool,
    pub param_regs: Vec<AsmReg>,
    pub return_regs: Vec<AsmReg>,
    pub callee_saved_regs_used: RegSet,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AsmObjEntry {
    pub t: AsmType,
    pub is_static: bool,
    pub constant: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AsmSymbolEntry {
    Fun(AsmFunEntry),
    Obj(AsmObjEntry),
}
pub type SymbolTable = BTreeMap<String, AsmSymbolEntry>;

static TABLE: OnceLock<Mutex<SymbolTable>> = OnceLock::new();
fn table() -> MutexGuard<'static, SymbolTable> {
    TABLE
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(|e| e.into_inner())
}
fn find(name: &str) -> AsmSymbolEntry {
    table()
        .get(name)
        .unwrap_or_else(|| panic!("assembly symbol not found: {name}"))
        .clone()
}

pub fn addFun(
    name: impl Into<String>,
    defined: bool,
    return_on_stack: bool,
    param_regs: Vec<AsmReg>,
    return_regs: Vec<AsmReg>,
) {
    table().insert(
        name.into(),
        AsmSymbolEntry::Fun(AsmFunEntry {
            defined,
            bytes_required: 0,
            return_on_stack,
            param_regs,
            return_regs,
            callee_saved_regs_used: RegSet::new(),
        }),
    );
}
pub fn addVar(name: impl Into<String>, t: AsmType, is_static: bool) {
    table().insert(
        name.into(),
        AsmSymbolEntry::Obj(AsmObjEntry {
            t,
            is_static,
            constant: false,
        }),
    );
}
pub fn addConstant(name: impl Into<String>, t: AsmType) {
    table().insert(
        name.into(),
        AsmSymbolEntry::Obj(AsmObjEntry {
            t,
            is_static: true,
            constant: true,
        }),
    );
}
pub fn setBytesRequired(name: &str, bytes: i32) {
    match table().get_mut(name).expect("assembly symbol not found") {
        AsmSymbolEntry::Fun(f) => f.bytes_required = bytes,
        _ => panic!("Internal error: not a function"),
    }
}
pub fn getBytesRequired(name: &str) -> i32 {
    match find(name) {
        AsmSymbolEntry::Fun(f) => f.bytes_required,
        _ => panic!("Internal error: not a function"),
    }
}
pub fn addCalleeSavedRegsUsed(name: &str, regs: &RegSet) {
    match table().get_mut(name).expect("assembly symbol not found") {
        AsmSymbolEntry::Fun(f) => f.callee_saved_regs_used.extend(regs),
        _ => panic!("Internal error: not a function"),
    }
}
pub fn getCalleeSavedRegsUsed(name: &str) -> RegSet {
    match find(name) {
        AsmSymbolEntry::Fun(f) => f.callee_saved_regs_used,
        _ => panic!("Internal error: not a function"),
    }
}
pub fn getSize(name: &str) -> i32 {
    match getType(name) {
        AsmType::Byte => 1,
        AsmType::Longword => 4,
        AsmType::Quadword | AsmType::Double => 8,
        AsmType::ByteArray(x) => x.size,
    }
}
pub fn getType(name: &str) -> AsmType {
    match find(name) {
        AsmSymbolEntry::Obj(o) => o.t,
        _ => panic!("Internal error: this is a function, not an object"),
    }
}
pub fn getAlignment(name: &str) -> i32 {
    match getType(name) {
        AsmType::Byte => 1,
        AsmType::Longword => 4,
        AsmType::Quadword | AsmType::Double => 8,
        AsmType::ByteArray(x) => x.alignment,
    }
}
pub fn isDefined(name: &str) -> bool {
    match find(name) {
        AsmSymbolEntry::Fun(f) => f.defined,
        _ => panic!("Internal error: not a function"),
    }
}
pub fn isStatic(name: &str) -> bool {
    match find(name) {
        AsmSymbolEntry::Obj(o) => o.is_static,
        _ => panic!("Internal error: functions don't have storage duration"),
    }
}
pub fn isConstant(name: &str) -> bool {
    match find(name) {
        AsmSymbolEntry::Obj(o) => o.constant,
        _ => panic!("Internal error: is_constant doesn't make sense for functions"),
    }
}
pub fn returnsOnStack(name: &str) -> bool {
    match find(name) {
        AsmSymbolEntry::Fun(f) => f.return_on_stack,
        _ => panic!("Internal error: this is an object, not a function"),
    }
}
pub fn paramRegsUsed(name: &str) -> Vec<AsmReg> {
    match find(name) {
        AsmSymbolEntry::Fun(f) => f.param_regs,
        _ => panic!("Internal error: not a function"),
    }
}
pub fn returnRegsUsed(name: &str) -> Vec<AsmReg> {
    match find(name) {
        AsmSymbolEntry::Fun(f) => f.return_regs,
        _ => panic!("Internal error: not a function"),
    }
}
pub fn getTable() -> SymbolTable {
    table().clone()
}
pub fn setTable(new_table: SymbolTable) {
    *table() = new_table;
}
