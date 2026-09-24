#![allow(non_snake_case)]
#[test]
fn usage_names_the_rust_binary() {
    assert!(clod::USAGE.starts_with("Usage: clod--"));
}
