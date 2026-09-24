//! Untyped syntax tree produced by the parser.

use super::{Const::Constant, Types::Type};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Mod,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    Constant(Constant),
    Var(String),
    String(String),
    Cast(Type, Box<Exp>),
    Unary(UnaryOperator, Box<Exp>),
    Binary(BinaryOperator, Box<Exp>, Box<Exp>),
    Assignment(Box<Exp>, Box<Exp>),
    Conditional(Box<Exp>, Box<Exp>, Box<Exp>),
    FunCall(String, Vec<Exp>),
    Dereference(Box<Exp>),
    AddrOf(Box<Exp>),
    Subscript(Box<Exp>, Box<Exp>),
    SizeOf(Box<Exp>),
    SizeOfT(Type),
    Dot(Box<Exp>, String),
    Arrow(Box<Exp>, String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Initializer {
    SingleInit(Exp),
    CompoundInit(Vec<Initializer>),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum StorageClass {
    Static,
    Extern,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MemberDeclaration {
    pub memberName: String,
    pub memberType: Type,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct StructDeclaration {
    pub tag: String,
    pub members: Vec<MemberDeclaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct VariableDeclaration {
    pub name: String,
    pub varType: Type,
    pub init: Option<Initializer>,
    pub storageClass: Option<StorageClass>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForInit {
    InitDecl(VariableDeclaration),
    InitExp(Option<Exp>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Return(Option<Exp>),
    Expression(Exp),
    If(Exp, Box<Statement>, Option<Box<Statement>>),
    Compound(Block),
    Break(String),
    Continue(String),
    While(Exp, Box<Statement>, String),
    DoWhile(Box<Statement>, Exp, String),
    For(ForInit, Option<Exp>, Option<Exp>, Box<Statement>, String),
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub enum BlockItem {
    Stmt(Statement),
    Decl(Declaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block(pub Vec<BlockItem>);

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclaration {
    pub name: String,
    pub funType: Type,
    pub params: Vec<String>,
    pub body: Option<Block>,
    pub storageClass: Option<StorageClass>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    FunDecl(FunctionDeclaration),
    VarDecl(VariableDeclaration),
    StructDecl(StructDeclaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct UntypedProgram(pub Vec<Declaration>);
