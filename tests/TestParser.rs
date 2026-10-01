#![allow(non_snake_case)]
use clod::ports::Ast::{BinaryOperator, Declaration, Exp, Statement};
use clod::ports::Const::Constant;
use clod::ports::Lex::lex;
use clod::ports::Parse::{parse, parseConst, parseExp, parseStatement};
use clod::ports::TokStream::TokStream;
use clod::ports::Tokens::Token;

#[test]
fn parses_integer_constant_ranges() {
    let (signed, _) = parseConst(TokStream::ofList(vec![Token::ConstLong(
        4_611_686_018_427_387_904,
    )]))
    .unwrap();
    assert_eq!(signed, Constant::Long(4_611_686_018_427_387_904));

    let (unsigned, _) =
        parseConst(TokStream::ofList(vec![Token::ConstUInt(4_294_967_291)])).unwrap();
    assert_eq!(unsigned, Constant::UInt(4_294_967_291));

    let (unsigned_long, _) = parseConst(TokStream::ofList(vec![Token::ConstULong(
        18_446_744_073_709_551_611,
    )]))
    .unwrap();
    assert_eq!(unsigned_long, Constant::ULong(18_446_744_073_709_551_611));
}

#[test]
fn parses_expression_precedence_and_assignment_associativity() {
    let tokens = lex("a = 2 + 3 * 4;").unwrap();
    let (expression, rest) = parseExp(0, TokStream::ofList(tokens)).unwrap();
    assert_eq!(rest.peek(), Some(&Token::Semicolon));
    assert_eq!(
        expression,
        Exp::Assignment(
            Box::new(Exp::Var("a".into())),
            Box::new(Exp::Binary(
                BinaryOperator::Add,
                Box::new(Exp::Constant(Constant::Int(2))),
                Box::new(Exp::Binary(
                    BinaryOperator::Multiply,
                    Box::new(Exp::Constant(Constant::Int(3))),
                    Box::new(Exp::Constant(Constant::Int(4))),
                )),
            )),
        )
    );
}

#[test]
fn parses_return_statement() {
    let (statement, rest) = parseStatement(TokStream::ofList(vec![
        Token::Return,
        Token::ConstInt(4),
        Token::Semicolon,
    ]))
    .unwrap();
    assert!(rest.isEmpty());
    assert_eq!(
        statement,
        Statement::Return(Some(Exp::Constant(Constant::Int(4))))
    );
}

#[test]
fn parses_complete_program() {
    let program = parse(lex("int main(void) { return 0; }").unwrap()).unwrap();
    assert!(matches!(program.0.as_slice(), [Declaration::FunDecl(_)]));
}

#[test]
fn rejects_incomplete_declaration() {
    assert!(parse(vec![Token::Int]).is_err());
}

#[test]
fn rejects_unknown_input() {
    assert!(lex("int main(void) { @ }").is_err());
}

#[test]
fn reports_canonical_expected_token_and_name_errors() {
    assert_eq!(
        parseStatement(TokStream::ofList(vec![Token::Break, Token::CloseBrace])).unwrap_err(),
        "Expected Semicolon but found CloseBrace"
    );
    assert_eq!(
        parseConst(TokStream::ofList(vec![Token::Identifier("value".into())])).unwrap_err(),
        "Expected a constant token but found (Identifier value)"
    );
    assert_eq!(
        parseConst(TokStream::ofList(vec![])).unwrap_err(),
        "Unexpected end of file"
    );
}

#[test]
fn rejects_empty_structure_definitions_like_the_phase_11_parser() {
    assert_eq!(
        parse(vec![
            Token::Struct,
            Token::Identifier("empty".into()),
            Token::OpenBrace,
            Token::CloseBrace,
            Token::Semicolon,
        ]),
        Err("Expected a type specifier but found CloseBrace".into())
    );
}

#[test]
fn rejects_duplicate_storage_classes_and_invalid_types() {
    assert_eq!(
        parse(lex("static extern int value;").unwrap()),
        Err("Internal error - not a storage class".into())
    );
    assert_eq!(
        parse(lex("signed unsigned value;").unwrap()),
        Err("Invalid type specifier".into())
    );
}

#[test]
fn parses_casts_abstract_declarators_and_compound_initializers() {
    assert!(parse(
        lex("int main(void) { int a[2] = {1, 2,}; return sizeof(int (*)[2]) + (long) a[0]; }")
            .unwrap()
    )
    .is_ok());
}

#[test]
fn rejects_dimensions_outside_the_phase_11_signed_width() {
    assert_eq!(
        parse(vec![
            Token::Int,
            Token::Identifier("values".into()),
            Token::OpenBracket,
            Token::ConstULong(i64::MAX as u128 + 1),
            Token::CloseBracket,
            Token::Semicolon,
        ]),
        Err("Array dimension is out of range".into())
    );
}
