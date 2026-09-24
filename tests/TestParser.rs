#![allow(non_snake_case)]
use clod::ports::Lex::lex;
#[test]
fn rejects_unknown_input() {
    assert!(lex("int main(void) { @ }").is_err());
}
