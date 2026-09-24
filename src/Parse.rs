//! Recursive-descent parser, faithfully ported from the original `Parse.fs`.
#![allow(non_snake_case)]

use super::Ast::*;
use super::Const::Constant;
use super::TokStream::TokStream;
use super::Tokens::Token;
use super::Types::Type;

type Parsed<T> = Result<(T, TokStream), String>;

fn show(token: &Token) -> String {
    format!("{token:?}")
}
fn expected(name: &str, actual: &Token) -> String {
    format!("Expected {name} but found {}", show(actual))
}
fn expect(wanted: Token, tokens: TokStream) -> Result<TokStream, String> {
    let (actual, rest) = tokens.takeToken()?;
    if actual == wanted {
        Ok(rest)
    } else {
        Err(expected(&show(&wanted), &actual))
    }
}
fn isTypeSpecifier(t: &Token) -> bool {
    matches!(
        t,
        Token::Int
            | Token::Long
            | Token::Unsigned
            | Token::Signed
            | Token::Double
            | Token::Char
            | Token::Void
            | Token::Struct
    )
}
fn isSpecifier(t: &Token) -> bool {
    matches!(t, Token::Static | Token::Extern) || isTypeSpecifier(t)
}
fn precedence(t: &Token) -> Option<u8> {
    Some(match t {
        Token::Star | Token::Slash | Token::Percent => 50,
        Token::Plus | Token::Hyphen => 45,
        Token::LessThan | Token::LessOrEqual | Token::GreaterThan | Token::GreaterOrEqual => 35,
        Token::DoubleEqual | Token::NotEqual => 30,
        Token::LogicalAnd => 10,
        Token::LogicalOr => 5,
        Token::QuestionMark => 3,
        Token::EqualSign => 1,
        _ => return None,
    })
}

fn unescape(s: &str) -> String {
    let mut out = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            out.push(c);
            continue;
        }
        let Some(e) = chars.next() else {
            out.push('\\');
            break;
        };
        out.push(match e {
            '\'' => '\'',
            '"' => '"',
            '?' => '?',
            '\\' => '\\',
            'a' => '\x07',
            'b' => '\x08',
            'f' => '\x0c',
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => '\x0b',
            other => other,
        });
    }
    out
}

fn parseType(specs: &[Token]) -> Result<Type, String> {
    if let [Token::Identifier(tag)] = specs {
        return Ok(Type::Struct(tag.clone()));
    }
    if specs == [Token::Void] {
        return Ok(Type::Void);
    }
    if specs == [Token::Double] {
        return Ok(Type::Double);
    }
    if specs == [Token::Char] {
        return Ok(Type::Char);
    }
    let count = |needle: &Token| specs.iter().filter(|x| *x == needle).count();
    if count(&Token::Char) == 1 && count(&Token::Signed) == 1 && specs.len() == 2 {
        return Ok(Type::SChar);
    }
    if count(&Token::Char) == 1 && count(&Token::Unsigned) == 1 && specs.len() == 2 {
        return Ok(Type::UChar);
    }
    let duplicate = [
        Token::Int,
        Token::Long,
        Token::Unsigned,
        Token::Signed,
        Token::Double,
        Token::Char,
        Token::Void,
    ]
    .iter()
    .any(|t| count(t) > 1);
    if specs.is_empty()
        || duplicate
        || specs.iter().any(|t| {
            matches!(
                t,
                Token::Double | Token::Char | Token::Void | Token::Identifier(_)
            )
        })
        || (count(&Token::Signed) > 0 && count(&Token::Unsigned) > 0)
    {
        return Err("Invalid type specifier".into());
    }
    if count(&Token::Unsigned) > 0 && count(&Token::Long) > 0 {
        Ok(Type::ULong)
    } else if count(&Token::Unsigned) > 0 {
        Ok(Type::UInt)
    } else if count(&Token::Long) > 0 {
        Ok(Type::Long)
    } else {
        Ok(Type::Int)
    }
}

fn parseId(tokens: TokStream) -> Parsed<String> {
    let (t, rest) = tokens.takeToken()?;
    match t {
        Token::Identifier(s) => Ok((s, rest)),
        x => Err(expected("an identifier", &x)),
    }
}
fn parseTypeSpecifier(tokens: TokStream) -> Parsed<Token> {
    let (spec, rest) = tokens.takeToken()?;
    if spec == Token::Struct {
        let (tag, rest) = rest.takeToken()?;
        match tag {
            Token::Identifier(_) => Ok((tag, rest)),
            x => Err(expected("a structure tag", &x)),
        }
    } else if isTypeSpecifier(&spec) {
        Ok((spec, rest))
    } else {
        Err(expected("a type specifier", &spec))
    }
}
fn parseTypeSpecifierList(mut tokens: TokStream) -> Parsed<Vec<Token>> {
    let mut result = Vec::new();
    let (s, r) = parseTypeSpecifier(tokens)?;
    result.push(s);
    tokens = r;
    while tokens.peek().is_some_and(isTypeSpecifier) {
        let (s, r) = parseTypeSpecifier(tokens)?;
        result.push(s);
        tokens = r;
    }
    Ok((result, tokens))
}
fn parseSpecifierList(mut tokens: TokStream) -> Parsed<Vec<Token>> {
    let mut result = Vec::new();
    loop {
        let Some(t) = tokens.peek() else {
            return Err("Unexpected end of file".into());
        };
        if !isSpecifier(t) {
            if result.is_empty() {
                return Err(expected("a type or storage-class specifier", t));
            }
            break;
        }
        if isTypeSpecifier(t) {
            let (s, r) = parseTypeSpecifier(tokens)?;
            result.push(s);
            tokens = r;
        } else {
            let (s, r) = tokens.takeToken()?;
            result.push(s);
            tokens = r;
        }
        if !tokens.peek().is_some_and(isSpecifier) {
            break;
        }
    }
    Ok((result, tokens))
}
fn parseTypeAndStorageClass(specs: Vec<Token>) -> Result<(Type, Option<StorageClass>), String> {
    let (types, storage): (Vec<_>, Vec<_>) = specs
        .into_iter()
        .partition(|t| isTypeSpecifier(t) || matches!(t, Token::Identifier(_)));
    let typ = parseType(&types)?;
    let storage = match storage.as_slice() {
        [] => None,
        [Token::Extern] => Some(StorageClass::Extern),
        [Token::Static] => Some(StorageClass::Static),
        [_] => return Err("Expected a storage class specifier".into()),
        _ => return Err("Internal error - not a storage class".into()),
    };
    Ok((typ, storage))
}

pub fn parseConst(tokens: TokStream) -> Parsed<Constant> {
    let (tok, rest) = tokens.takeToken()?;
    let c = match tok {
        Token::ConstInt(v) | Token::ConstLong(v) => {
            if v > i64::MAX as u128 {
                return Err("Constant is too large to represent as an int or long".into());
            }
            if matches!(tok, Token::ConstInt(_)) && v <= i32::MAX as u128 {
                Constant::Int(v as i32)
            } else {
                Constant::Long(v as i64)
            }
        }
        Token::ConstUInt(v) | Token::ConstULong(v) => {
            if v > u64::MAX as u128 {
                return Err(
                    "Constant is too large to represent as an unsigned int or unsigned long".into(),
                );
            }
            if matches!(tok, Token::ConstUInt(_)) && v <= u32::MAX as u128 {
                Constant::UInt(v as u32)
            } else {
                Constant::ULong(v as u64)
            }
        }
        Token::ConstDouble(v) => Constant::Double(v),
        Token::ConstChar(s) => {
            let s = unescape(&s);
            let mut cs = s.chars();
            let c = cs
                .next()
                .ok_or("multi-character constant tokens not supported")?;
            if cs.next().is_some() {
                return Err("multi-character constant tokens not supported".into());
            }
            Constant::Int(c as i32)
        }
        x => return Err(expected("a constant token", &x)),
    };
    Ok((c, rest))
}
fn parseDim(tokens: TokStream) -> Parsed<usize> {
    let tokens = expect(Token::OpenBracket, tokens)?;
    let (c, tokens) = parseConst(tokens)?;
    let dim = match c {
        Constant::Double(_) => return Err("Floating-point array dimensions not allowed".into()),
        _ => usize::try_from(c.as_i128())
            .map_err(|_| "Array dimension is out of range".to_owned())?,
    };
    Ok((dim, expect(Token::CloseBracket, tokens)?))
}

#[derive(Clone)]
enum AbstractDeclarator {
    Pointer(Box<AbstractDeclarator>),
    Array(Box<AbstractDeclarator>, usize),
    Base,
}
fn parseAbstractDeclarator(tokens: TokStream) -> Parsed<AbstractDeclarator> {
    if tokens.peek() == Some(&Token::Star) {
        let (_, rest) = tokens.takeToken()?;
        let (inner, rest) = if matches!(
            rest.peek(),
            Some(Token::Star | Token::OpenParen | Token::OpenBracket)
        ) {
            parseAbstractDeclarator(rest)?
        } else {
            (AbstractDeclarator::Base, rest)
        };
        Ok((AbstractDeclarator::Pointer(Box::new(inner)), rest))
    } else {
        parseDirectAbstractDeclarator(tokens)
    }
}
fn parseAbstractArraySuffix(
    mut d: AbstractDeclarator,
    mut tokens: TokStream,
) -> Parsed<AbstractDeclarator> {
    loop {
        let (n, r) = parseDim(tokens)?;
        d = AbstractDeclarator::Array(Box::new(d), n);
        tokens = r;
        if tokens.peek() != Some(&Token::OpenBracket) {
            return Ok((d, tokens));
        }
    }
}
fn parseDirectAbstractDeclarator(tokens: TokStream) -> Parsed<AbstractDeclarator> {
    if tokens.peek() == Some(&Token::OpenParen) {
        let (_, r) = tokens.takeToken()?;
        let (d, r) = parseAbstractDeclarator(r)?;
        let r = expect(Token::CloseParen, r)?;
        if r.peek() == Some(&Token::OpenBracket) {
            parseAbstractArraySuffix(d, r)
        } else {
            Ok((d, r))
        }
    } else {
        parseAbstractArraySuffix(AbstractDeclarator::Base, tokens)
    }
}
fn processAbstract(d: AbstractDeclarator, base: Type) -> Type {
    match d {
        AbstractDeclarator::Base => base,
        AbstractDeclarator::Pointer(i) => processAbstract(*i, Type::Pointer(Box::new(base))),
        AbstractDeclarator::Array(i, n) => processAbstract(*i, Type::Array(Box::new(base), n)),
    }
}
fn parseTypeName(tokens: TokStream) -> Parsed<Type> {
    let (specs, r) = parseTypeSpecifierList(tokens)?;
    let base = parseType(&specs)?;
    if r.peek() == Some(&Token::CloseParen) {
        Ok((base, r))
    } else {
        let (d, r) = parseAbstractDeclarator(r)?;
        Ok((processAbstract(d, base), r))
    }
}

fn parsePrimaryExp(tokens: TokStream) -> Parsed<Exp> {
    match tokens.peek() {
        Some(
            Token::ConstInt(_)
            | Token::ConstLong(_)
            | Token::ConstUInt(_)
            | Token::ConstULong(_)
            | Token::ConstDouble(_)
            | Token::ConstChar(_),
        ) => {
            let (c, r) = parseConst(tokens)?;
            Ok((Exp::Constant(c), r))
        }
        Some(Token::Identifier(_)) => {
            let (id, mut r) = parseId(tokens)?;
            if r.peek() == Some(&Token::OpenParen) {
                (_, r) = r.takeToken()?;
                let (args, n) = if r.peek() == Some(&Token::CloseParen) {
                    (vec![], r)
                } else {
                    parseArgumentList(r)?
                };
                Ok((Exp::FunCall(id, args), expect(Token::CloseParen, n)?))
            } else {
                Ok((Exp::Var(id), r))
            }
        }
        Some(Token::OpenParen) => {
            let (_, r) = tokens.takeToken()?;
            let (e, r) = parseExp(0, r)?;
            Ok((e, expect(Token::CloseParen, r)?))
        }
        Some(Token::StringLiteral(_)) => {
            let mut s = String::new();
            let mut r = tokens;
            while let Some(Token::StringLiteral(raw)) = r.peek() {
                s.push_str(&unescape(raw));
                (_, r) = r.takeToken()?;
            }
            Ok((Exp::String(s), r))
        }
        Some(t) => Err(expected("a primary expression", t)),
        None => Err("Unexpected end of file".into()),
    }
}
fn parseArgumentList(tokens: TokStream) -> Parsed<Vec<Exp>> {
    let (a, mut r) = parseExp(0, tokens)?;
    let mut args = vec![a];
    while r.peek() == Some(&Token::Comma) {
        (_, r) = r.takeToken()?;
        let (a, n) = parseExp(0, r)?;
        args.push(a);
        r = n
    }
    Ok((args, r))
}
fn parsePostfixExp(tokens: TokStream) -> Parsed<Exp> {
    let (mut e, mut r) = parsePrimaryExp(tokens)?;
    loop {
        match r.peek() {
            Some(Token::OpenBracket) => {
                (_, r) = r.takeToken()?;
                let (i, n) = parseExp(0, r)?;
                r = expect(Token::CloseBracket, n)?;
                e = Exp::Subscript(Box::new(e), Box::new(i))
            }
            Some(Token::Dot) => {
                (_, r) = r.takeToken()?;
                let (m, n) = parseId(r)?;
                r = n;
                e = Exp::Dot(Box::new(e), m)
            }
            Some(Token::Arrow) => {
                (_, r) = r.takeToken()?;
                let (m, n) = parseId(r)?;
                r = n;
                e = Exp::Arrow(Box::new(e), m)
            }
            _ => return Ok((e, r)),
        }
    }
}
fn parseUnaryExp(tokens: TokStream) -> Parsed<Exp> {
    match tokens.npeek(3) {
        [Token::Star, ..] => {
            let (_, r) = tokens.takeToken()?;
            let (e, r) = parseCastExp(r)?;
            Ok((Exp::Dereference(Box::new(e)), r))
        }
        [Token::Ampersand, ..] => {
            let (_, r) = tokens.takeToken()?;
            let (e, r) = parseCastExp(r)?;
            Ok((Exp::AddrOf(Box::new(e)), r))
        }
        [op @ (Token::Hyphen | Token::Tilde | Token::Bang), ..] => {
            let op = match op {
                Token::Hyphen => UnaryOperator::Negate,
                Token::Tilde => UnaryOperator::Complement,
                _ => UnaryOperator::Not,
            };
            let (_, r) = tokens.takeToken()?;
            let (e, r) = parseCastExp(r)?;
            Ok((Exp::Unary(op, Box::new(e)), r))
        }
        [Token::SizeOf, Token::OpenParen, t] if isTypeSpecifier(t) => {
            let (_, r) = tokens.takeToken()?;
            let (_, r) = r.takeToken()?;
            let (t, r) = parseTypeName(r)?;
            Ok((Exp::SizeOfT(t), expect(Token::CloseParen, r)?))
        }
        [Token::SizeOf, ..] => {
            let (_, r) = tokens.takeToken()?;
            let (e, r) = parseUnaryExp(r)?;
            Ok((Exp::SizeOf(Box::new(e)), r))
        }
        _ => parsePostfixExp(tokens),
    }
}
fn parseCastExp(tokens: TokStream) -> Parsed<Exp> {
    if let [Token::OpenParen, t, ..] = tokens.npeek(2) {
        if isTypeSpecifier(t) {
            let (_, r) = tokens.takeToken()?;
            let (t, r) = parseTypeName(r)?;
            let r = expect(Token::CloseParen, r)?;
            let (e, r) = parseCastExp(r)?;
            return Ok((Exp::Cast(t, Box::new(e)), r));
        }
    }
    parseUnaryExp(tokens)
}
fn binary(t: &Token) -> Result<BinaryOperator, String> {
    Ok(match t {
        Token::Plus => BinaryOperator::Add,
        Token::Hyphen => BinaryOperator::Subtract,
        Token::Star => BinaryOperator::Multiply,
        Token::Slash => BinaryOperator::Divide,
        Token::Percent => BinaryOperator::Mod,
        Token::LogicalAnd => BinaryOperator::And,
        Token::LogicalOr => BinaryOperator::Or,
        Token::DoubleEqual => BinaryOperator::Equal,
        Token::NotEqual => BinaryOperator::NotEqual,
        Token::LessThan => BinaryOperator::LessThan,
        Token::LessOrEqual => BinaryOperator::LessOrEqual,
        Token::GreaterThan => BinaryOperator::GreaterThan,
        Token::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
        x => return Err(expected("a binary operator", x)),
    })
}
pub fn parseExp(minPrec: u8, tokens: TokStream) -> Parsed<Exp> {
    let (mut left, mut r) = parseCastExp(tokens)?;
    loop {
        let Some(next) = r.peek().cloned() else {
            return Ok((left, r));
        };
        let Some(p) = precedence(&next) else {
            return Ok((left, r));
        };
        if p < minPrec {
            return Ok((left, r));
        }
        if next == Token::EqualSign {
            (_, r) = r.takeToken()?;
            let (right, n) = parseExp(p, r)?;
            left = Exp::Assignment(Box::new(left), Box::new(right));
            r = n
        } else if next == Token::QuestionMark {
            (_, r) = r.takeToken()?;
            let (mid, n) = parseExp(0, r)?;
            r = expect(Token::Colon, n)?;
            let (right, n) = parseExp(p, r)?;
            left = Exp::Conditional(Box::new(left), Box::new(mid), Box::new(right));
            r = n
        } else {
            let op = binary(&next)?;
            (_, r) = r.takeToken()?;
            let (right, n) = parseExp(p + 1, r)?;
            left = Exp::Binary(op, Box::new(left), Box::new(right));
            r = n
        }
    }
}
fn parseOptionalExp(delim: Token, tokens: TokStream) -> Parsed<Option<Exp>> {
    if tokens.peek() == Some(&delim) {
        let (_, r) = tokens.takeToken()?;
        Ok((None, r))
    } else {
        let (e, r) = parseExp(0, tokens)?;
        Ok((Some(e), expect(delim, r)?))
    }
}

#[derive(Clone)]
enum Declarator {
    Ident(String),
    Pointer(Box<Declarator>),
    Array(Box<Declarator>, usize),
    Fun(Vec<(Type, Declarator)>, Box<Declarator>),
}
fn parseDeclarator(tokens: TokStream) -> Parsed<Declarator> {
    if tokens.peek() == Some(&Token::Star) {
        let (_, r) = tokens.takeToken()?;
        let (d, r) = parseDeclarator(r)?;
        Ok((Declarator::Pointer(Box::new(d)), r))
    } else {
        parseDirectDeclarator(tokens)
    }
}
fn parseSimpleDeclarator(tokens: TokStream) -> Parsed<Declarator> {
    let (t, r) = tokens.takeToken()?;
    match t {
        Token::OpenParen => {
            let (d, r) = parseDeclarator(r)?;
            Ok((d, expect(Token::CloseParen, r)?))
        }
        Token::Identifier(s) => Ok((Declarator::Ident(s), r)),
        x => Err(expected("a simple declarator", &x)),
    }
}
fn parseDirectDeclarator(tokens: TokStream) -> Parsed<Declarator> {
    let (mut d, r) = parseSimpleDeclarator(tokens)?;
    if r.peek() == Some(&Token::OpenParen) {
        let (ps, r) = parseParamList(r)?;
        Ok((Declarator::Fun(ps, Box::new(d)), r))
    } else if r.peek() == Some(&Token::OpenBracket) {
        let mut r = r;
        loop {
            let (n, x) = parseDim(r)?;
            d = Declarator::Array(Box::new(d), n);
            r = x;
            if r.peek() != Some(&Token::OpenBracket) {
                return Ok((d, r));
            }
        }
    } else {
        Ok((d, r))
    }
}
fn parseParamList(tokens: TokStream) -> Parsed<Vec<(Type, Declarator)>> {
    if tokens.npeek(2) == [Token::OpenParen, Token::CloseParen]
        || tokens.npeek(3) == [Token::OpenParen, Token::Void, Token::CloseParen]
    {
        let mut r = tokens;
        while r.peek() != Some(&Token::CloseParen) {
            (_, r) = r.takeToken()?
        }
        (_, r) = r.takeToken()?;
        return Ok((vec![], r));
    }
    let mut r = expect(Token::OpenParen, tokens)?;
    let mut ps = vec![];
    loop {
        let (specs, n) = parseTypeSpecifierList(r)?;
        let t = parseType(&specs)?;
        let (d, n) = parseDeclarator(n)?;
        ps.push((t, d));
        r = n;
        if r.peek() != Some(&Token::Comma) {
            break;
        }
        (_, r) = r.takeToken()?
    }
    Ok((ps, expect(Token::CloseParen, r)?))
}
fn processDeclarator(d: Declarator, base: Type) -> Result<(String, Type, Vec<String>), String> {
    match d {
        Declarator::Ident(s) => Ok((s, base, vec![])),
        Declarator::Pointer(i) => processDeclarator(*i, Type::Pointer(Box::new(base))),
        Declarator::Array(i, n) => processDeclarator(*i, Type::Array(Box::new(base), n)),
        Declarator::Fun(ps, i) => {
            let Declarator::Ident(name) = *i else {
                return Err(
                    "can't apply additional type derivations to a function declarator".into(),
                );
            };
            let mut names = vec![];
            let mut types = vec![];
            for (t, d) in ps {
                let (n, t, _) = processDeclarator(d, t)?;
                if matches!(t, Type::Function(..)) {
                    return Err("Function pointers in parameters are not supported".into());
                }
                names.push(n);
                types.push(t)
            }
            Ok((name, Type::Function(types, Box::new(base)), names))
        }
    }
}
fn parseInitializer(tokens: TokStream) -> Parsed<Initializer> {
    if tokens.peek() == Some(&Token::OpenBrace) {
        let (_, mut r) = tokens.takeToken()?;
        let mut xs = vec![];
        loop {
            let (x, n) = parseInitializer(r)?;
            xs.push(x);
            r = n;
            if r.peek() != Some(&Token::Comma) {
                break;
            }
            (_, r) = r.takeToken()?;
            if r.peek() == Some(&Token::CloseBrace) {
                break;
            }
        }
        Ok((Initializer::CompoundInit(xs), expect(Token::CloseBrace, r)?))
    } else {
        let (e, r) = parseExp(0, tokens)?;
        Ok((Initializer::SingleInit(e), r))
    }
}
fn parseMemberDeclaration(tokens: TokStream) -> Parsed<MemberDeclaration> {
    let (specs, r) = parseTypeSpecifierList(tokens)?;
    let base = parseType(&specs)?;
    let (d, r) = parseDeclarator(r)?;
    if matches!(d, Declarator::Fun(..)) {
        return Err("Found function declarator in struct member list".into());
    }
    let r = expect(Token::Semicolon, r)?;
    let (name, memberType, _) = processDeclarator(d, base)?;
    Ok((
        MemberDeclaration {
            memberName: name,
            memberType,
        },
        r,
    ))
}
fn parseStructDeclaration(tokens: TokStream) -> Parsed<StructDeclaration> {
    let r = expect(Token::Struct, tokens)?;
    let (tag, mut r) = parseId(r)?;
    let mut members = vec![];
    if r.peek() == Some(&Token::OpenBrace) {
        (_, r) = r.takeToken()?;
        while r.peek() != Some(&Token::CloseBrace) {
            let (m, n) = parseMemberDeclaration(r)?;
            members.push(m);
            r = n
        }
        r = expect(Token::CloseBrace, r)?
    }
    Ok((
        StructDeclaration { tag, members },
        expect(Token::Semicolon, r)?,
    ))
}
fn parseDeclaration(tokens: TokStream) -> Parsed<Declaration> {
    if matches!(
        tokens.npeek(3),
        [
            Token::Struct,
            Token::Identifier(_),
            Token::OpenBrace | Token::Semicolon
        ]
    ) {
        let (s, r) = parseStructDeclaration(tokens)?;
        return Ok((Declaration::StructDecl(s), r));
    }
    let (specs, r) = parseSpecifierList(tokens)?;
    let (base, storage) = parseTypeAndStorageClass(specs)?;
    let (d, mut r) = parseDeclarator(r)?;
    let (name, typ, params) = processDeclarator(d, base)?;
    if matches!(typ, Type::Function(..)) {
        let (body, n) = if r.peek() == Some(&Token::Semicolon) {
            let (_, n) = r.takeToken()?;
            (None, n)
        } else {
            let (b, n) = parseBlock(r)?;
            (Some(b), n)
        };
        Ok((
            Declaration::FunDecl(FunctionDeclaration {
                name,
                funType: typ,
                params,
                body,
                storageClass: storage,
            }),
            n,
        ))
    } else {
        let init;
        if r.peek() == Some(&Token::EqualSign) {
            (_, r) = r.takeToken()?;
            let (i, n) = parseInitializer(r)?;
            init = Some(i);
            r = n
        } else {
            init = None
        }
        Ok((
            Declaration::VarDecl(VariableDeclaration {
                name,
                varType: typ,
                init,
                storageClass: storage,
            }),
            expect(Token::Semicolon, r)?,
        ))
    }
}
fn parseForInit(tokens: TokStream) -> Parsed<ForInit> {
    if tokens.peek().is_some_and(isSpecifier) {
        let (d, r) = parseDeclaration(tokens)?;
        if let Declaration::VarDecl(v) = d {
            Ok((ForInit::InitDecl(v), r))
        } else {
            Err("Found a function declaration in a for loop header".into())
        }
    } else {
        let (e, r) = parseOptionalExp(Token::Semicolon, tokens)?;
        Ok((ForInit::InitExp(e), r))
    }
}
pub fn parseStatement(tokens: TokStream) -> Parsed<Statement> {
    match tokens.peek() {
        Some(Token::Return) => {
            let (_, r) = tokens.takeToken()?;
            let (e, r) = parseOptionalExp(Token::Semicolon, r)?;
            Ok((Statement::Return(e), r))
        }
        Some(Token::If) => {
            let (_, r) = tokens.takeToken()?;
            let r = expect(Token::OpenParen, r)?;
            let (c, r) = parseExp(0, r)?;
            let r = expect(Token::CloseParen, r)?;
            let (t, mut r) = parseStatement(r)?;
            let e = if r.peek() == Some(&Token::Else) {
                (_, r) = r.takeToken()?;
                let (e, n) = parseStatement(r)?;
                r = n;
                Some(Box::new(e))
            } else {
                None
            };
            Ok((Statement::If(c, Box::new(t), e), r))
        }
        Some(Token::OpenBrace) => {
            let (b, r) = parseBlock(tokens)?;
            Ok((Statement::Compound(b), r))
        }
        Some(Token::Break) => {
            let (_, r) = tokens.takeToken()?;
            Ok((
                Statement::Break(String::new()),
                expect(Token::Semicolon, r)?,
            ))
        }
        Some(Token::Continue) => {
            let (_, r) = tokens.takeToken()?;
            Ok((
                Statement::Continue(String::new()),
                expect(Token::Semicolon, r)?,
            ))
        }
        Some(Token::While) => {
            let (_, r) = tokens.takeToken()?;
            let r = expect(Token::OpenParen, r)?;
            let (c, r) = parseExp(0, r)?;
            let r = expect(Token::CloseParen, r)?;
            let (b, r) = parseStatement(r)?;
            Ok((Statement::While(c, Box::new(b), String::new()), r))
        }
        Some(Token::Do) => {
            let r = expect(Token::Do, tokens)?;
            let (b, r) = parseStatement(r)?;
            let r = expect(Token::While, r)?;
            let r = expect(Token::OpenParen, r)?;
            let (c, r) = parseExp(0, r)?;
            let r = expect(Token::CloseParen, r)?;
            Ok((
                Statement::DoWhile(Box::new(b), c, String::new()),
                expect(Token::Semicolon, r)?,
            ))
        }
        Some(Token::For) => {
            let r = expect(Token::For, tokens)?;
            let r = expect(Token::OpenParen, r)?;
            let (i, r) = parseForInit(r)?;
            let (c, r) = parseOptionalExp(Token::Semicolon, r)?;
            let (p, r) = parseOptionalExp(Token::CloseParen, r)?;
            let (b, r) = parseStatement(r)?;
            Ok((Statement::For(i, c, p, Box::new(b), String::new()), r))
        }
        _ => {
            let (e, r) = parseOptionalExp(Token::Semicolon, tokens)?;
            Ok((e.map_or(Statement::Null, Statement::Expression), r))
        }
    }
}
fn parseBlock(tokens: TokStream) -> Parsed<Block> {
    let mut r = expect(Token::OpenBrace, tokens)?;
    let mut items = vec![];
    while r.peek() != Some(&Token::CloseBrace) {
        if r.isEmpty() {
            return Err("Unexpected end of file".into());
        }
        if r.peek().is_some_and(isSpecifier) {
            let (d, n) = parseDeclaration(r)?;
            items.push(BlockItem::Decl(d));
            r = n
        } else {
            let (s, n) = parseStatement(r)?;
            items.push(BlockItem::Stmt(s));
            r = n
        }
    }
    Ok((Block(items), expect(Token::CloseBrace, r)?))
}
pub fn parse(tokens: Vec<Token>) -> Result<UntypedProgram, String> {
    let mut r = TokStream::ofList(tokens);
    let mut ds = vec![];
    while !r.isEmpty() {
        let (d, n) = parseDeclaration(r)?;
        ds.push(d);
        r = n
    }
    Ok(UntypedProgram(ds))
}

pub fn parse_const(tokens: TokStream) -> Parsed<Constant> {
    parseConst(tokens)
}
pub fn parse_exp(min_precedence: u8, tokens: TokStream) -> Parsed<Exp> {
    parseExp(min_precedence, tokens)
}
pub fn parse_statement(tokens: TokStream) -> Parsed<Statement> {
    parseStatement(tokens)
}
