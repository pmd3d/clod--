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
