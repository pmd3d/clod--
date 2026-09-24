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

/// Expression annotated with the type assigned by semantic analysis.
#[derive(Clone, Debug, PartialEq)]
pub struct TypedExp {
    pub e: TypedInnerExp,
    pub t: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedInnerExp {
    Constant(Constant), Var(String), String(String),
    Cast(Type, Box<TypedExp>), Unary(UnaryOperator, Box<TypedExp>),
    Binary(BinaryOperator, Box<TypedExp>, Box<TypedExp>),
    Assignment(Box<TypedExp>, Box<TypedExp>),
    Conditional(Box<TypedExp>, Box<TypedExp>, Box<TypedExp>),
    FunCall(String, Vec<TypedExp>), Dereference(Box<TypedExp>),
    AddrOf(Box<TypedExp>), Subscript(Box<TypedExp>, Box<TypedExp>),
    SizeOf(Box<TypedExp>), SizeOfT(Type), Dot(Box<TypedExp>, String),
    Arrow(Box<TypedExp>, String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypedInitializer {
    SingleInit(TypedExp),
    CompoundInit(Type, Vec<TypedInitializer>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypedVariableDeclaration {
    pub name: String, pub varType: Type, pub init: Option<TypedInitializer>,
    pub storageClass: Option<StorageClass>,
}
#[derive(Clone, Debug, PartialEq)]
pub struct TypedStructDeclaration { pub tag: String, pub members: Vec<MemberDeclaration> }
#[derive(Clone, Debug, PartialEq)]
pub struct TypedFunctionDeclaration {
    pub name: String, pub funType: Type, pub params: Vec<String>,
    pub body: Option<TypedBlock>, pub storageClass: Option<StorageClass>,
}
#[derive(Clone, Debug, PartialEq)]
pub enum TypedForInit { InitDecl(TypedVariableDeclaration), InitExp(Option<TypedExp>) }
#[derive(Clone, Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum TypedStatement {
    Return(Option<TypedExp>), Expression(TypedExp),
    If(TypedExp, Box<TypedStatement>, Option<Box<TypedStatement>>),
    Compound(TypedBlock), Break(String), Continue(String),
    While(TypedExp, Box<TypedStatement>, String),
    DoWhile(Box<TypedStatement>, TypedExp, String),
    For(TypedForInit, Option<TypedExp>, Option<TypedExp>, Box<TypedStatement>, String), Null,
}
#[derive(Clone, Debug, PartialEq)]
pub enum TypedBlockItem { Stmt(TypedStatement), Decl(TypedDeclaration) }
#[derive(Clone, Debug, PartialEq)]
pub struct TypedBlock(pub Vec<TypedBlockItem>);
#[derive(Clone, Debug, PartialEq)]
pub enum TypedDeclaration {
    FunDecl(TypedFunctionDeclaration), VarDecl(TypedVariableDeclaration),
    StructDecl(TypedStructDeclaration),
}
#[derive(Clone, Debug, PartialEq)]
pub struct TypedProgram(pub Vec<TypedDeclaration>);
