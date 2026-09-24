//! Identifier symbol table.
//!
//! This is a faithful Rust port of the original `Symbols.fs` module.  Like
//! that module, it owns one process-wide table; callers may snapshot and
//! restore the table when threading compiler state through the pipeline.
#![allow(non_snake_case)]

use std::collections::BTreeMap;
use std::sync::{Mutex, MutexGuard, OnceLock};

use super::Initializers::StaticInit;
use super::Types::Type;
use super::UniqueIds::UniqueIds;

#[derive(Clone, Debug, PartialEq)]
pub enum InitialValue {
    Tentative,
    Initial(Vec<StaticInit>),
    NoInitializer,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct FunAttr {
    pub defined: bool,
    pub global: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StaticAttr {
    pub init: InitialValue,
    pub global: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum IdentifierAttrs {
    FunAttr(FunAttr),
    StaticAttr(StaticAttr),
    ConstAttr(StaticInit),
    LocalAttr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SymbolEntry {
    pub symType: Type,
    pub attrs: IdentifierAttrs,
}

pub type SymbolTable = BTreeMap<String, SymbolEntry>;

static SYMBOL_TABLE: OnceLock<Mutex<SymbolTable>> = OnceLock::new();
static STRING_IDS: OnceLock<Mutex<UniqueIds>> = OnceLock::new();

fn table() -> MutexGuard<'static, SymbolTable> {
    // A compiler panic must not make the table inaccessible to a subsequent
    // compilation in the same process (notably, to the test runner).
    SYMBOL_TABLE
        .get_or_init(|| Mutex::new(BTreeMap::new()))
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn replace(name: impl Into<String>, entry: SymbolEntry) {
    // `insert`, rather than an operation that rejects duplicates, preserves
    // Map.add's replacement semantics from the F# implementation.
    table().insert(name.into(), entry);
}

pub fn addAutomaticVar(name: impl Into<String>, symType: Type) {
    replace(
        name,
        SymbolEntry {
            symType,
            attrs: IdentifierAttrs::LocalAttr,
        },
    );
}

pub fn addStaticVar(
    name: impl Into<String>,
    symType: Type,
    global: bool,
    init: InitialValue,
) {
    replace(
        name,
        SymbolEntry {
            symType,
            attrs: IdentifierAttrs::StaticAttr(StaticAttr { init, global }),
        },
    );
}

pub fn addFun(name: impl Into<String>, symType: Type, global: bool, defined: bool) {
    replace(
        name,
        SymbolEntry {
            symType,
            attrs: IdentifierAttrs::FunAttr(FunAttr { defined, global }),
        },
    );
}

/// Find a symbol, panicking just as F#'s `Map.find` does when it is absent.
pub fn get(name: &str) -> SymbolEntry {
    table()
        .get(name)
        .cloned()
        .unwrap_or_else(|| panic!("symbol not found: {name}"))
}

pub fn getOpt(name: &str) -> Option<SymbolEntry> {
    table().get(name).cloned()
}

fn stringType(s: &str) -> Type {
    // F# String.length counts UTF-16 code units.
    Type::Array(Box::new(Type::Char), s.encode_utf16().count() + 1)
}

/// Add a string constant using the caller's explicitly threaded counter.
pub fn addStringWithCounter(counter: &mut UniqueIds, s: impl Into<String>) -> String {
    let s = s.into();
    let str_id = counter.next("string");
    replace(
        str_id.clone(),
        SymbolEntry {
            symType: stringType(&s),
            attrs: IdentifierAttrs::ConstAttr(StaticInit::StringInit(s, true)),
        },
    );
    str_id
}

/// Add a string constant using the shared counter retained for legacy callers.
pub fn addString(s: impl Into<String>) -> String {
    let mut counter = STRING_IDS
        .get_or_init(|| Mutex::new(UniqueIds::default()))
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner());
    addStringWithCounter(&mut counter, s)
}

pub fn isGlobal(name: &str) -> bool {
    match get(name).attrs {
        IdentifierAttrs::LocalAttr | IdentifierAttrs::ConstAttr(_) => false,
        IdentifierAttrs::StaticAttr(attr) => attr.global,
        IdentifierAttrs::FunAttr(attr) => attr.global,
    }
}

pub fn bindings() -> Vec<(String, SymbolEntry)> {
    table()
        .iter()
        .map(|(name, entry)| (name.clone(), entry.clone()))
        .collect()
}

pub fn iter(mut f: impl FnMut(&str, &SymbolEntry)) {
    // Iterate over a snapshot so the callback does not run while the global
    // mutex is held and may itself safely call back into this module.
    for (name, entry) in bindings() {
        f(&name, &entry);
    }
}

pub fn getTable() -> SymbolTable {
    table().clone()
}

pub fn setTable(new_table: SymbolTable) {
    *table() = new_table;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ports_symbol_table_operations_and_string_interning() {
        setTable(BTreeMap::new());

        addAutomaticVar("item", Type::Int);
        assert_eq!(get("item").attrs, IdentifierAttrs::LocalAttr);
        assert!(!isGlobal("item"));

        // Adding the same name replaces its old binding.
        addFun("item", Type::Function(vec![], Box::new(Type::Int)), true, true);
        assert!(isGlobal("item"));
        assert!(matches!(get("item").attrs, IdentifierAttrs::FunAttr(_)));

        let mut ids = UniqueIds::default();
        let string_name = addStringWithCounter(&mut ids, "a😀");
        assert_eq!(string_name, "string.0");
        assert_eq!(get(&string_name).symType, Type::Array(Box::new(Type::Char), 4));
        assert_eq!(bindings().len(), 2);

        let snapshot = getTable();
        setTable(BTreeMap::new());
        assert_eq!(getOpt("item"), None);
        setTable(snapshot);

        let mut names = Vec::new();
        iter(|name, _| names.push(name.to_owned()));
        assert_eq!(names, vec!["item", "string.0"]);
    }
}
