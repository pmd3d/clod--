#![allow(non_snake_case)]
use clod::ports::Ast::{
    BinaryOperator, Block, BlockItem, Declaration, Exp, Initializer, MemberDeclaration, Statement,
    StorageClass, StructDeclaration, VariableDeclaration,
};
use clod::ports::Const::Constant;
use clod::ports::Lex::lex;
use clod::ports::Parse::{parse, parseConst, parseExp, parseStatement};
use clod::ports::TokStream::TokStream;
use clod::ports::Tokens::Token;
use clod::ports::Types::Type;

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

#[test]
fn preserves_suffix_after_concatenated_strings_and_postfix_chains() {
    let (expression, rest) = parseExp(
        0,
        TokStream::ofList(lex("\"hello, \" \"world\"; trailing").unwrap()),
    )
    .unwrap();
    assert_eq!(expression, Exp::String("hello, world".into()));
    assert_eq!(
        rest.npeek(2),
        &[Token::Semicolon, Token::Identifier("trailing".into())]
    );

    let (expression, rest) =
        parseExp(0, TokStream::ofList(lex("items[1].node->value;").unwrap())).unwrap();
    assert_eq!(
        expression,
        Exp::Arrow(
            Box::new(Exp::Dot(
                Box::new(Exp::Subscript(
                    Box::new(Exp::Var("items".into())),
                    Box::new(Exp::Constant(Constant::Int(1))),
                )),
                "node".into(),
            )),
            "value".into(),
        )
    );
    assert_eq!(rest.peek(), Some(&Token::Semicolon));
}

#[test]
fn preserves_argument_and_conditional_error_order() {
    assert_eq!(
        parseExp(0, TokStream::ofList(lex("f(1, );").unwrap())).unwrap_err(),
        "Expected a primary expression but found CloseParen"
    );
    assert_eq!(
        parseExp(0, TokStream::ofList(lex("condition ? yes ; no").unwrap())).unwrap_err(),
        "Expected Colon but found Semicolon"
    );
}

#[test]
fn parses_multidimensional_declarators_and_nested_initializers() {
    let program =
        parse(lex("static long values[2][3] = {{1, 2, 3}, {4, 5, 6},};").unwrap()).unwrap();
    assert_eq!(
        program.0,
        vec![Declaration::VarDecl(VariableDeclaration {
            name: "values".into(),
            varType: Type::Array(Box::new(Type::Array(Box::new(Type::Long), 3)), 2,),
            init: Some(Initializer::CompoundInit(vec![
                Initializer::CompoundInit(vec![
                    Initializer::SingleInit(Exp::Constant(Constant::Int(1))),
                    Initializer::SingleInit(Exp::Constant(Constant::Int(2))),
                    Initializer::SingleInit(Exp::Constant(Constant::Int(3))),
                ]),
                Initializer::CompoundInit(vec![
                    Initializer::SingleInit(Exp::Constant(Constant::Int(4))),
                    Initializer::SingleInit(Exp::Constant(Constant::Int(5))),
                    Initializer::SingleInit(Exp::Constant(Constant::Int(6))),
                ]),
            ])),
            storageClass: Some(StorageClass::Static),
        })]
    );
}

#[test]
fn parses_abstract_array_declarators_with_the_phase_11_nesting() {
    let (expression, rest) = parseExp(
        0,
        TokStream::ofList(lex("sizeof(long (*)[2][3]);").unwrap()),
    )
    .unwrap();
    assert_eq!(
        expression,
        Exp::SizeOfT(Type::Pointer(Box::new(Type::Array(
            Box::new(Type::Array(Box::new(Type::Long), 3)),
            2,
        ))))
    );
    assert_eq!(rest.peek(), Some(&Token::Semicolon));
}

#[test]
fn parses_structure_members_and_mixed_block_items_in_order() {
    let program = parse(
        lex("struct pair { int left; unsigned long right; }; int f(void) { int x = 1; x = x + 1; return x; }")
            .unwrap(),
    )
    .unwrap();
    assert_eq!(program.0.len(), 2);
    assert_eq!(
        program.0[0],
        Declaration::StructDecl(StructDeclaration {
            tag: "pair".into(),
            members: vec![
                MemberDeclaration {
                    memberName: "left".into(),
                    memberType: Type::Int,
                },
                MemberDeclaration {
                    memberName: "right".into(),
                    memberType: Type::ULong,
                },
            ],
        })
    );
    let Declaration::FunDecl(function) = &program.0[1] else {
        panic!("expected a function declaration")
    };
    let Some(Block(items)) = &function.body else {
        panic!("expected a function body")
    };
    assert!(matches!(
        items.as_slice(),
        [
            BlockItem::Decl(_),
            BlockItem::Stmt(Statement::Expression(_)),
            BlockItem::Stmt(Statement::Return(_))
        ]
    ));
}

#[test]
fn reports_first_error_in_truncated_collections() {
    let cases = [
        (
            "int f(int first, );",
            "Expected a type specifier but found CloseParen",
        ),
        ("int values[2 ", "Unexpected end of file"),
        ("int value = {1, 2 ", "Unexpected end of file"),
        ("struct s { int member;", "Unexpected end of file"),
        ("int f(void) { int value;", "Unexpected end of file"),
    ];
    for (source, expected) in cases {
        assert_eq!(
            parse(lex(source).unwrap()),
            Err(expected.into()),
            "{source}"
        );
    }
}
