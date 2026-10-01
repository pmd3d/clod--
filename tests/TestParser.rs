#![allow(non_snake_case)]
use clod::ports::Ast::{
    BinaryOperator, Block, BlockItem, Declaration, Exp, ForInit, Initializer, MemberDeclaration,
    Statement, StorageClass, StructDeclaration, VariableDeclaration,
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

#[test]
fn reports_first_error_for_each_truncated_statement_delimiter() {
    let cases = [
        ("if (1 ", "Unexpected end of file"),
        ("if (1) return 0; else ", "Unexpected end of file"),
        ("while (1 ", "Unexpected end of file"),
        ("do return 0; while (1 ", "Unexpected end of file"),
        ("for (int i = 0; i < 2 ", "Unexpected end of file"),
        ("{ return 0;", "Unexpected end of file"),
    ];
    for (source, expected) in cases {
        assert_eq!(
            parseStatement(TokStream::ofList(lex(source).unwrap())).unwrap_err(),
            expected,
            "{source}"
        );
    }
}

#[test]
fn rejects_trailing_top_level_tokens_at_the_first_invalid_declaration() {
    assert_eq!(
        parse(lex("int value; return 0;").unwrap()),
        Err("Expected a type or storage-class specifier but found KWReturn".into())
    );
}

#[test]
fn parses_every_constant_variant_at_its_conversion_boundaries() {
    let cases = [
        (Token::ConstInt(0), Constant::Int(0)),
        (Token::ConstInt(i32::MAX as u128), Constant::Int(i32::MAX)),
        (
            Token::ConstInt(i32::MAX as u128 + 1),
            Constant::Long(i32::MAX as i64 + 1),
        ),
        (Token::ConstLong(i64::MAX as u128), Constant::Long(i64::MAX)),
        (Token::ConstUInt(u32::MAX as u128), Constant::UInt(u32::MAX)),
        (
            Token::ConstUInt(u32::MAX as u128 + 1),
            Constant::ULong(u32::MAX as u64 + 1),
        ),
        (
            Token::ConstULong(u64::MAX as u128),
            Constant::ULong(u64::MAX),
        ),
        (Token::ConstDouble(1.25), Constant::Double(1.25)),
        (Token::ConstChar("\\n".into()), Constant::Int('\n' as i32)),
    ];

    for (token, expected) in cases {
        let (constant, rest) =
            parseConst(TokStream::ofList(vec![token, Token::Semicolon])).unwrap();
        assert_eq!(constant, expected);
        assert_eq!(rest.peek(), Some(&Token::Semicolon));
    }

    let overflow_cases = [
        (
            Token::ConstInt(i64::MAX as u128 + 1),
            "Constant is too large to represent as an int or long",
        ),
        (
            Token::ConstLong(i64::MAX as u128 + 1),
            "Constant is too large to represent as an int or long",
        ),
        (
            Token::ConstUInt(u64::MAX as u128 + 1),
            "Constant is too large to represent as an unsigned int or unsigned long",
        ),
        (
            Token::ConstULong(u64::MAX as u128 + 1),
            "Constant is too large to represent as an unsigned int or unsigned long",
        ),
        (
            Token::ConstChar("ab".into()),
            "multi-character constant tokens not supported",
        ),
    ];
    for (token, expected) in overflow_cases {
        assert_eq!(
            parseConst(TokStream::ofList(vec![token])).unwrap_err(),
            expected
        );
    }
}

#[test]
fn reports_malformed_abstract_and_concrete_declarators_at_the_first_error() {
    let cases = [
        (
            "int values[2);",
            "Expected CloseBracket but found CloseParen",
        ),
        (
            "int (*callback)(void);",
            "can't apply additional type derivations to a function declarator",
        ),
        (
            "int apply(int callback(void));",
            "Function pointers in parameters are not supported",
        ),
        (
            "struct s { int member(void); };",
            "Found function declarator in struct member list",
        ),
    ];
    for (source, expected) in cases {
        assert_eq!(
            parse(lex(source).unwrap()),
            Err(expected.into()),
            "{source}"
        );
    }

    assert_eq!(
        parseExp(0, TokStream::ofList(lex("sizeof(int [2); ").unwrap())).unwrap_err(),
        "Expected CloseBracket but found CloseParen"
    );
}

#[test]
fn parses_forward_structs_prototypes_and_definitions_in_order() {
    let program = parse(
        lex("struct node; static int helper(int value); int helper(int value) { return value; }")
            .unwrap(),
    )
    .unwrap();

    assert!(matches!(
        program.0.as_slice(),
        [
            Declaration::StructDecl(StructDeclaration { members, .. }),
            Declaration::FunDecl(prototype),
            Declaration::FunDecl(definition),
        ] if members.is_empty()
            && prototype.body.is_none()
            && prototype.storageClass == Some(StorageClass::Static)
            && definition.body.is_some()
            && definition.params == ["value"]
    ));
}

#[test]
fn parses_every_statement_branch_and_preserves_the_suffix() {
    let cases = [
        "return; trailing",
        "if (1) break; else continue; trailing",
        "while (1) ; trailing",
        "do ; while (1); trailing",
        "for (int i = 0; i < 2; i = i + 1) ; trailing",
        "{ int value; value = 1; } trailing",
        "; trailing",
        "value = 1; trailing",
    ];

    for source in cases {
        let (statement, rest) = parseStatement(TokStream::ofList(lex(source).unwrap())).unwrap();
        assert_eq!(
            rest.peek(),
            Some(&Token::Identifier("trailing".into())),
            "{source}"
        );
        match source {
            s if s.starts_with("return") => assert!(matches!(statement, Statement::Return(None))),
            s if s.starts_with("if") => assert!(matches!(statement, Statement::If(..))),
            s if s.starts_with("while") => assert!(matches!(statement, Statement::While(..))),
            s if s.starts_with("do") => assert!(matches!(statement, Statement::DoWhile(..))),
            s if s.starts_with("for") => assert!(matches!(
                statement,
                Statement::For(ForInit::InitDecl(_), Some(_), Some(_), _, _)
            )),
            s if s.starts_with('{') => assert!(matches!(statement, Statement::Compound(_))),
            s if s.starts_with(';') => assert_eq!(statement, Statement::Null),
            _ => assert!(matches!(statement, Statement::Expression(_))),
        }
    }
}

#[test]
fn reports_each_missing_statement_delimiter_without_panicking() {
    let cases = [
        ("if 1) ;", "Expected OpenParen but found (ConstInt 1)"),
        ("if (1 ;", "Expected CloseParen but found Semicolon"),
        (
            "do ; while 1);",
            "Expected OpenParen but found (ConstInt 1)",
        ),
        ("do ; while (1;", "Expected CloseParen but found Semicolon"),
        ("do ; while (1)", "Unexpected end of file"),
        ("for 1; ; ) ;", "Expected OpenParen but found (ConstInt 1)"),
        ("for (1 2; ) ;", "Expected Semicolon but found (ConstInt 2)"),
        ("for (; 1 2) ;", "Expected Semicolon but found (ConstInt 2)"),
        ("for (; ; 1 ;", "Expected CloseParen but found Semicolon"),
    ];
    for (source, expected) in cases {
        assert_eq!(
            parseStatement(TokStream::ofList(lex(source).unwrap())).unwrap_err(),
            expected,
            "{source}"
        );
    }
}
