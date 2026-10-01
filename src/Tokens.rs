//! Tokens produced by the C lexer.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String), StringLiteral(String), ConstChar(String), ConstInt(u128), ConstLong(u128), ConstUInt(u128), ConstULong(u128), ConstDouble(f64),
    Int, Long, Char, Signed, Unsigned, Double, Return, Void, If, Else, Do, While, For, Break, Continue, Static, Extern, SizeOf, Struct,
    OpenParen, CloseParen, OpenBrace, CloseBrace, Semicolon, Hyphen, DoubleHyphen, Tilde, Plus, Star, Slash, Percent, Bang, LogicalAnd, LogicalOr,
    DoubleEqual, NotEqual, LessThan, GreaterThan, LessOrEqual, GreaterOrEqual, EqualSign, QuestionMark, Colon, Comma, Ampersand, OpenBracket, CloseBracket, Dot, Arrow,
}

/// Render a token using the compiler's canonical diagnostic representation.
///
/// This intentionally does not use `Debug`: the original parser's errors are
/// part of its public behaviour and, in particular, payload-bearing tokens do
/// not contain Rust string quotes.
pub fn show(token: &Token) -> String {
    use Token::*;

    let name = match token {
        Identifier(value) => return format!("(Identifier {value})"),
        StringLiteral(value) => return format!("(StringLiteral {value})"),
        ConstChar(value) => return format!("(ConstChar {value})"),
        ConstInt(value) => return format!("(ConstInt {value})"),
        ConstLong(value) => return format!("(ConstLong {value})"),
        ConstUInt(value) => return format!("(ConstUInt {value})"),
        ConstULong(value) => return format!("(ConstULong {value})"),
        ConstDouble(value) => return format!("(ConstDouble {value})"),
        Int => "KWInt", Long => "KWLong", Char => "KWChar",
        Signed => "KWSigned", Unsigned => "KWUnsigned", Double => "KWDouble",
        Return => "KWReturn", Void => "KWVoid", If => "KWIf", Else => "KWElse",
        Do => "KWDo", While => "KWWhile", For => "KWFor", Break => "KWBreak",
        Continue => "KWContinue", Static => "KWStatic", Extern => "KWExtern",
        SizeOf => "KWSizeOf", Struct => "KWStruct", OpenParen => "OpenParen",
        CloseParen => "CloseParen", OpenBrace => "OpenBrace", CloseBrace => "CloseBrace",
        Semicolon => "Semicolon", Hyphen => "Hyphen", DoubleHyphen => "DoubleHyphen",
        Tilde => "Tilde", Plus => "Plus", Star => "Star", Slash => "Slash",
        Percent => "Percent", Bang => "Bang", LogicalAnd => "LogicalAnd",
        LogicalOr => "LogicalOr", DoubleEqual => "DoubleEqual", NotEqual => "NotEqual",
        LessThan => "LessThan", GreaterThan => "GreaterThan", LessOrEqual => "LessOrEqual",
        GreaterOrEqual => "GreaterOrEqual", EqualSign => "EqualSign",
        QuestionMark => "QuestionMark", Colon => "Colon", Comma => "Comma",
        Ampersand => "Ampersand", OpenBracket => "OpenBracket",
        CloseBracket => "CloseBracket", Dot => "Dot", Arrow => "Arrow",
    };
    name.to_owned()
}
