#![allow(non_snake_case)]
use clod::ports::{Lex::lex, Tokens::Token};
#[test]
fn leading_whitespace() {
    assert_eq!(lex("   return").unwrap(), vec![Token::Return]);
}
#[test]
fn full_program() {
    assert_eq!(
        lex("int main(void){return 0;}").unwrap(),
        vec![
            Token::Int,
            Token::Identifier("main".into()),
            Token::OpenParen,
            Token::Void,
            Token::CloseParen,
            Token::OpenBrace,
            Token::Return,
            Token::ConstInt(0),
            Token::Semicolon,
            Token::CloseBrace
        ]
    );
}
#[test]
fn longest_operator() {
    assert_eq!(
        lex("a--").unwrap(),
        vec![Token::Identifier("a".into()), Token::DoubleHyphen]
    );
}

#[test]
fn constants_chars_and_strings() {
    assert_eq!(
        lex("1; 2L; 3u; 4UL; 5.0; 'x'; \"hello\";").unwrap(),
        vec![
            Token::ConstInt(1),
            Token::Semicolon,
            Token::ConstLong(2),
            Token::Semicolon,
            Token::ConstUInt(3),
            Token::Semicolon,
            Token::ConstULong(4),
            Token::Semicolon,
            Token::ConstDouble(5.0),
            Token::Semicolon,
            Token::ConstChar("x".into()),
            Token::Semicolon,
            Token::StringLiteral("hello".into()),
            Token::Semicolon,
        ]
    );
}

#[test]
fn reports_the_unlexed_remainder() {
    assert_eq!(lex("return @").unwrap_err(), "@");
}
