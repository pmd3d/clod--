#![allow(non_snake_case)]

use std::fs;
use std::process::Command;

#[test]
fn usage_names_the_rust_binary() {
    assert!(clod::USAGE.starts_with("Usage: clod--"));
}

#[test]
fn semantic_errors_are_reported_without_rust_panic_output() {
    let directory = std::env::temp_dir().join(format!("clod-program-test-{}", std::process::id()));
    let _ = fs::remove_dir_all(&directory);
    fs::create_dir(&directory).unwrap();
    let source = directory.join("invalid.c");
    fs::write(&source, "int main(void) { return missing; }\n").unwrap();

    let output = Command::new(env!("CARGO_BIN_EXE_clod--"))
        .arg(&source)
        .output()
        .unwrap();
    let stderr = String::from_utf8(output.stderr).unwrap();

    let _ = fs::remove_dir_all(&directory);
    assert!(!output.status.success());
    assert!(stderr.contains("Type error: Undeclared variable missing"));
    assert!(!stderr.contains("panicked at"), "{stderr}");
}
