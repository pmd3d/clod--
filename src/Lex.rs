//! Lexer for the supported C token set.
//!
//! This module intentionally follows the organization of the original
//! `Lex.fs`: token conversion functions are followed by the token definition
//! table, matching helpers, and the recursive lexer.

#![allow(non_snake_case, non_upper_case_globals)]

use super::Tokens::Token;

#[derive(Clone, Copy)]
pub enum Regex {
    Identifier,
    Int,
    Long,
    UInt,
    ULong,
    Double,
    Char,
    String,
    Literal(&'static str),
    Dot,
}

impl Regex {
    fn matchedSubstring(self, input: &str, group: usize) -> Option<&str> {
        let matched = match self {
            Self::Identifier => matchIdentifier(input),
            Self::Int => matchInteger(input, IntegerSuffix::None),
            Self::Long => matchInteger(input, IntegerSuffix::Long),
            Self::UInt => matchInteger(input, IntegerSuffix::Unsigned),
            Self::ULong => matchInteger(input, IntegerSuffix::UnsignedLong),
            Self::Double => matchDouble(input),
            Self::Char => matchQuoted(input, b'\'', false),
            Self::String => matchQuoted(input, b'"', true),
            Self::Literal(literal) => input
                .strip_prefix(literal)
                .map(|_| &input[..literal.len()]),
            Self::Dot => input
                .strip_prefix('.')
                .and_then(|rest| rest.chars().next())
                .filter(|next| !next.is_ascii_digit())
                .map(|_| &input[..1]),
        }?;
        // The original regular expressions use group 1 to exclude a required
        // look-ahead character. These matchers already return that group.
        (group <= 1).then_some(matched)
    }
}

pub struct TokenDef {
    pub re: Regex,
    pub group: usize,
    pub converter: Box<dyn Fn(&str) -> Token + Send + Sync>,
}

pub struct MatchDef<'a> {
    pub matchedSubstring: &'a str,
    pub matchingToken: &'static TokenDef,
}

// Functions to convert individual tokens from strings to Token values.

pub fn literal(tok: Token) -> impl Fn(&str) -> Token + Send + Sync {
    move |_s| tok.clone()
}

pub fn convertIdentifier(s: &str) -> Token {
    match s {
        "int" => Token::Int,
        "return" => Token::Return,
        "void" => Token::Void,
        "if" => Token::If,
        "else" => Token::Else,
        "do" => Token::Do,
        "while" => Token::While,
        "for" => Token::For,
        "break" => Token::Break,
        "continue" => Token::Continue,
        "static" => Token::Static,
        "extern" => Token::Extern,
        "long" => Token::Long,
        "unsigned" => Token::Unsigned,
        "signed" => Token::Signed,
        "double" => Token::Double,
        "char" => Token::Char,
        "sizeof" => Token::SizeOf,
        "struct" => Token::Struct,
        other => Token::Identifier(other.to_owned()),
    }
}

pub fn convertInt(s: &str) -> Token {
    Token::ConstInt(s.parse().expect("integer token must fit in u128"))
}

pub fn convertLong(s: &str) -> Token {
    Token::ConstLong(chopSuffix(s, 1).parse().expect("long token must fit in u128"))
}

pub fn convertUInt(s: &str) -> Token {
    Token::ConstUInt(
        chopSuffix(s, 1)
            .parse()
            .expect("unsigned integer token must fit in u128"),
    )
}

pub fn convertULong(s: &str) -> Token {
    Token::ConstULong(
        chopSuffix(s, 2)
            .parse()
            .expect("unsigned long token must fit in u128"),
    )
}

pub fn convertDouble(s: &str) -> Token {
    Token::ConstDouble(s.parse().expect("regex only matches valid double tokens"))
}

pub fn convertChar(s: &str) -> Token {
    Token::ConstChar(chopSuffix(&s[1..], 1).to_owned())
}

pub fn convertString(s: &str) -> Token {
    Token::StringLiteral(chopSuffix(&s[1..], 1).to_owned())
}

fn chopSuffix(s: &str, count: usize) -> &str {
    &s[..s.len() - count]
}

fn matchIdentifier(input: &str) -> Option<&str> {
    let mut chars = input.char_indices();
    let (_, first) = chars.next()?;
    if !(first.is_ascii_alphabetic() || first == '_') {
        return None;
    }
    let end = chars
        .take_while(|(_, ch)| ch.is_ascii_alphanumeric() || *ch == '_')
        .map(|(index, ch)| index + ch.len_utf8())
        .last()
        .unwrap_or(first.len_utf8());
    Some(&input[..end])
}

#[derive(Clone, Copy)]
enum IntegerSuffix {
    None,
    Long,
    Unsigned,
    UnsignedLong,
}

fn matchInteger(input: &str, suffix: IntegerSuffix) -> Option<&str> {
    let digitCount = input.bytes().take_while(u8::is_ascii_digit).count();
    if digitCount == 0 {
        return None;
    }
    let suffixLength = match suffix {
        IntegerSuffix::None => 0,
        IntegerSuffix::Long if matches!(input.as_bytes().get(digitCount), Some(b'l' | b'L')) => 1,
        IntegerSuffix::Unsigned
            if matches!(input.as_bytes().get(digitCount), Some(b'u' | b'U')) =>
        {
            1
        }
        IntegerSuffix::UnsignedLong => {
            let suffix = input.get(digitCount..digitCount + 2)?;
            matches!(suffix, "lu" | "lU" | "Lu" | "LU" | "ul" | "uL" | "Ul" | "UL")
                .then_some(2)?
        }
        _ => return None,
    };
    let end = digitCount + suffixLength;
    numericBoundary(input, end).then_some(&input[..end])
}

fn matchDouble(input: &str) -> Option<&str> {
    let bytes = input.as_bytes();
    let mut end = bytes.iter().take_while(|byte| byte.is_ascii_digit()).count();
    let leadingDigits = end;
    let hasDot = bytes.get(end) == Some(&b'.');
    if hasDot {
        end += 1;
        let fractionalDigits = bytes[end..]
            .iter()
            .take_while(|byte| byte.is_ascii_digit())
            .count();
        end += fractionalDigits;
        if leadingDigits == 0 && fractionalDigits == 0 {
            return None;
        }
    }

    let hasExponent = matches!(bytes.get(end), Some(b'e' | b'E'));
    if hasExponent {
        end += 1;
        if matches!(bytes.get(end), Some(b'+' | b'-')) {
            end += 1;
        }
        let exponentDigits = bytes[end..]
            .iter()
            .take_while(|byte| byte.is_ascii_digit())
            .count();
        if exponentDigits == 0 {
            return None;
        }
        end += exponentDigits;
    }

    if !hasDot && !hasExponent {
        return None;
    }
    numericBoundary(input, end).then_some(&input[..end])
}

fn numericBoundary(input: &str, end: usize) -> bool {
    input[end..]
        .chars()
        .next()
        .is_some_and(|ch| !(ch.is_alphanumeric() || ch == '_' || ch == '.'))
}

fn matchQuoted(input: &str, quote: u8, allowEmpty: bool) -> Option<&str> {
    let bytes = input.as_bytes();
    if bytes.first() != Some(&quote) {
        return None;
    }
    let mut index = 1;
    let mut characters = 0;
    while index < bytes.len() {
        match bytes[index] {
            byte if byte == quote => {
                return (allowEmpty || characters == 1).then_some(&input[..=index]);
            }
            b'\n' => return None,
            b'\\' => {
                let escaped = *bytes.get(index + 1)?;
                if !matches!(escaped, b'\'' | b'"' | b'?' | b'\\' | b'a' | b'b' | b'f' | b'n' | b'r' | b't' | b'v') {
                    return None;
                }
                index += 2;
            }
            _ => index += 1,
        }
        characters += 1;
    }
    None
}

pub static tokenDefs: std::sync::LazyLock<Vec<TokenDef>> = std::sync::LazyLock::new(|| {
    fn def(
        group: usize,
        re: Regex,
        converter: impl Fn(&str) -> Token + Send + Sync + 'static,
    ) -> TokenDef {
        TokenDef {
            re,
            group,
            converter: Box::new(converter),
        }
    }

    fn def0(
        re: Regex,
        converter: impl Fn(&str) -> Token + Send + Sync + 'static,
    ) -> TokenDef {
        def(0, re, converter)
    }

    vec![
        // All identifiers, including keywords.
        def0(Regex::Identifier, convertIdentifier),
        // Constants. The trailing character prevents accepting a numeric
        // prefix; group 1 leaves that character for the next lexing step.
        def(1, Regex::Int, convertInt),
        def(1, Regex::Long, convertLong),
        def(1, Regex::UInt, convertUInt),
        def(1, Regex::ULong, convertULong),
        def(1, Regex::Double, convertDouble),
        def0(Regex::Char, convertChar),
        // String literals.
        def0(Regex::String, convertString),
        // Punctuation.
        def0(Regex::Literal("("), literal(Token::OpenParen)),
        def0(Regex::Literal(")"), literal(Token::CloseParen)),
        def0(Regex::Literal("{"), literal(Token::OpenBrace)),
        def0(Regex::Literal("}"), literal(Token::CloseBrace)),
        def0(Regex::Literal(";"), literal(Token::Semicolon)),
        def0(Regex::Literal("-"), literal(Token::Hyphen)),
        def0(Regex::Literal("--"), literal(Token::DoubleHyphen)),
        def0(Regex::Literal("~"), literal(Token::Tilde)),
        def0(Regex::Literal("+"), literal(Token::Plus)),
        def0(Regex::Literal("*"), literal(Token::Star)),
        def0(Regex::Literal("/"), literal(Token::Slash)),
        def0(Regex::Literal("%"), literal(Token::Percent)),
        def0(Regex::Literal("!"), literal(Token::Bang)),
        def0(Regex::Literal("&&"), literal(Token::LogicalAnd)),
        def0(Regex::Literal("||"), literal(Token::LogicalOr)),
        def0(Regex::Literal("=="), literal(Token::DoubleEqual)),
        def0(Regex::Literal("!="), literal(Token::NotEqual)),
        def0(Regex::Literal("<"), literal(Token::LessThan)),
        def0(Regex::Literal(">"), literal(Token::GreaterThan)),
        def0(Regex::Literal("<="), literal(Token::LessOrEqual)),
        def0(Regex::Literal(">="), literal(Token::GreaterOrEqual)),
        def0(Regex::Literal("="), literal(Token::EqualSign)),
        def0(Regex::Literal("?"), literal(Token::QuestionMark)),
        def0(Regex::Literal(":"), literal(Token::Colon)),
        def0(Regex::Literal(","), literal(Token::Comma)),
        def0(Regex::Literal("&"), literal(Token::Ampersand)),
        def0(Regex::Literal("["), literal(Token::OpenBracket)),
        def0(Regex::Literal("]"), literal(Token::CloseBracket)),
        def0(Regex::Literal("->"), literal(Token::Arrow)),
        // The dot operator must be followed by a non-digit.
        def(1, Regex::Dot, literal(Token::Dot)),
    ]
});

pub fn findMatch<'a>(s: &'a str, tokDef: &'static TokenDef) -> Option<MatchDef<'a>> {
    Some(MatchDef {
        matchedSubstring: tokDef.re.matchedSubstring(s, tokDef.group)?,
        matchingToken: tokDef,
    })
}

pub fn countLeadingWs(s: &str) -> Option<usize> {
    let length = s
        .char_indices()
        .take_while(|(_, ch)| ch.is_whitespace())
        .map(|(index, ch)| index + ch.len_utf8())
        .last()?;
    Some(length)
}

pub fn lex(input: &str) -> Result<Vec<Token>, String> {
    if input.is_empty() {
        return Ok(Vec::new());
    }

    if let Some(wsCount) = countLeadingWs(input) {
        return lex(&input[wsCount..]);
    }

    let matches: Vec<MatchDef<'_>> = tokenDefs
        .iter()
        .filter_map(|tokDef| findMatch(input, tokDef))
        .collect();

    let longestMatch = matches
        .into_iter()
        .max_by_key(|matched| matched.matchedSubstring.len())
        .ok_or_else(|| input.to_owned())?;
    let matchingSubstring = longestMatch.matchedSubstring;
    let nextTok = (longestMatch.matchingToken.converter)(matchingSubstring);
    let remaining = &input[matchingSubstring.len()..];
    let mut rest = lex(remaining)?;
    rest.insert(0, nextTok);
    Ok(rest)
}
