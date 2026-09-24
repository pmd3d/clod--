//! Rust port of the original `AssemblySymbols.fs` assemblysymbols module.
#![allow(non_snake_case)]

use std::collections::BTreeMap;
use std::sync::{Mutex, OnceLock};

use super::Assembly::{AsmReg, AsmType};

pub const COMPONENT: &str = "assemblysymbols";

enum Entry {
    Fun { defined: bool },
    Obj { constant: bool },
}

static TABLE: OnceLock<Mutex<BTreeMap<String, Entry>>> = OnceLock::new();

fn table() -> std::sync::MutexGuard<'static, BTreeMap<String, Entry>> {
    TABLE
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(|error| error.into_inner())
}

pub fn addFun(
    name: impl Into<String>,
    defined: bool,
    _return_on_stack: bool,
    _param_regs: Vec<AsmReg>,
    _return_regs: Vec<AsmReg>,
) {
    table().insert(name.into(), Entry::Fun { defined });
}

pub fn addVar(name: impl Into<String>, _t: AsmType, _is_static: bool) {
    table().insert(name.into(), Entry::Obj { constant: false });
}

pub fn addConstant(name: impl Into<String>, _t: AsmType) {
    table().insert(name.into(), Entry::Obj { constant: true });
}

pub fn isDefined(name: &str) -> bool {
    match table().get(name) {
        Some(Entry::Fun { defined }) => *defined,
        Some(Entry::Obj { .. }) => panic!("Internal error: not a function"),
        None => panic!("assembly symbol not found: {name}"),
    }
}

pub fn isConstant(name: &str) -> bool {
    match table().get(name) {
        Some(Entry::Obj { constant }) => *constant,
        Some(Entry::Fun { .. }) => {
            panic!("Internal error: is_constant doesn't make sense for functions")
        }
        None => panic!("assembly symbol not found: {name}"),
    }
}
