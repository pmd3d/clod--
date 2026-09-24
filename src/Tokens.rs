//! Tokens produced by the C lexer.
#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Identifier(String), StringLiteral(String), ConstChar(String), ConstInt(u128), ConstLong(u128), ConstUInt(u128), ConstULong(u128), ConstDouble(f64),
    Int, Long, Char, Signed, Unsigned, Double, Return, Void, If, Else, Do, While, For, Break, Continue, Static, Extern, SizeOf, Struct,
    OpenParen, CloseParen, OpenBrace, CloseBrace, Semicolon, Hyphen, DoubleHyphen, Tilde, Plus, Star, Slash, Percent, Bang, LogicalAnd, LogicalOr,
    DoubleEqual, NotEqual, LessThan, GreaterThan, LessOrEqual, GreaterOrEqual, EqualSign, QuestionMark, Colon, Comma, Ampersand, OpenBracket, CloseBracket, Dot, Arrow,
}
