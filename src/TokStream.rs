//! Persistent-style token stream used by the recursive-descent parser.

use super::Tokens::Token;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct TokStream {
    items: Arc<[Token]>,
    position: usize,
}

impl TokStream {
    pub fn ofList(items: Vec<Token>) -> Self {
        Self {
            items: items.into(),
            position: 0,
        }
    }
    pub fn peek(&self) -> Option<&Token> {
        self.items.get(self.position)
    }
    pub fn npeek(&self, n: usize) -> &[Token] {
        &self.items[self.position..(self.position + n).min(self.items.len())]
    }
    pub fn isEmpty(&self) -> bool {
        self.position == self.items.len()
    }
    pub fn takeToken(&self) -> Result<(Token, Self), String> {
        let token = self
            .peek()
            .cloned()
            .ok_or_else(|| "Unexpected end of file".to_owned())?;
        let mut rest = self.clone();
        rest.position += 1;
        Ok((token, rest))
    }
}

pub fn ofList(items: Vec<Token>) -> TokStream {
    TokStream::ofList(items)
}

pub fn takeToken(tokens: &TokStream) -> Result<(Token, TokStream), String> {
    tokens.takeToken()
}

pub fn peek(tokens: &TokStream) -> Option<&Token> {
    tokens.peek()
}

pub fn npeek(tokens: &TokStream, n: usize) -> &[Token] {
    tokens.npeek(n)
}

pub fn isEmpty(tokens: &TokStream) -> bool {
    tokens.isEmpty()
}
