module Ast

(* Unary and binary operators; used in exp AST nodes both with and without
   type information *)
module Ops =
    type UnaryOperator =
        | Complement
        | Negate
        | Not

    type BinaryOperator =
        | Add
        | Subtract
        | Multiply
        | Divide
        | Mod
        | And
        | Or
        | Equal
        | NotEqual
        | LessThan
        | LessOrEqual
        | GreaterThan
        | GreaterOrEqual

(* Exp and initializer AST definitions without type info *)
module UntypedExp =
    open Ops

    type Exp =
        | Constant of Const.t
        | Var of string
        | String of string
        | Cast of targetType: Types.t * e: Exp
        | Unary of UnaryOperator * Exp
        | Binary of BinaryOperator * Exp * Exp
        | Assignment of Exp * Exp
        | Conditional of condition: Exp * thenResult: Exp * elseResult: Exp
        | FunCall of f: string * args: Exp list
        | Dereference of Exp
        | AddrOf of Exp
        | Subscript of ptr: Exp * index: Exp
        | SizeOf of Exp
        | SizeOfT of Types.t
        | Dot of strct: Exp * ``member``: string
        | Arrow of strct: Exp * ``member``: string

    type Initializr =
        | SingleInit of Exp
        | CompoundInit of Initializr list

(* Exp and initializer AST definitions with type info *)
module TypedExp =
    open Ops

    type InnerExp =
        | Constant of Const.t
        | Var of string
        | String of string
        | Cast of targetType: Types.t * e: Exp
        | Unary of UnaryOperator * Exp
        | Binary of BinaryOperator * Exp * Exp
        | Assignment of Exp * Exp
        | Conditional of condition: Exp * thenResult: Exp * elseResult: Exp
        | FunCall of f: string * args: Exp list
        | Dereference of Exp
        | AddrOf of Exp
        | Subscript of ptr: Exp * index: Exp
        | SizeOf of Exp
        | SizeOfT of Types.t
        | Dot of strct: Exp * ``member``: string
        | Arrow of strct: Exp * ``member``: string

    and Exp = { e: InnerExp; t: Types.t }

    type Initializr =
        | SingleInit of Exp
        | CompoundInit of Types.t * Initializr list

module StorageClass =
    type StorageClass =
        | Static
        | Extern

(* The complete untyped AST *)
module Untyped =
    open Ops
    open StorageClass

    type UnaryOperator = Ops.UnaryOperator
    type BinaryOperator = Ops.BinaryOperator
    type StorageClass = StorageClass.StorageClass

    type Exp = UntypedExp.Exp
    type Initializr = UntypedExp.Initializr

    type MemberDeclaration =
        { memberName: string
          memberType: Types.t }

    type StructDeclaration =
        { tag: string
          members: MemberDeclaration list }

    type VariableDeclaration =
        { name: string
          varType: Types.t
          init: Initializr option
          storageClass: StorageClass option }

    type ForInit =
        | InitDecl of VariableDeclaration
        | InitExp of Exp option

    type Statement =
        | Return of Exp option
        | Expression of Exp
        | If of condition: Exp * thenClause: Statement * elseClause: Statement option
        | Compound of Block
        | Break of string
        | Continue of string
        | While of condition: Exp * body: Statement * id: string
        | DoWhile of body: Statement * condition: Exp * id: string
        | For of
            init: ForInit *
            condition: Exp option *
            post: Exp option *
            body: Statement *
            id: string
        | Null

    and BlockItem =
        | S of Statement
        | D of Declaration

    and Block = Block of BlockItem list

    and FunctionDeclaration =
        { name: string
          funType: Types.t
          ``params``: string list
          body: Block option
          storageClass: StorageClass option }

    and Declaration =
        | FunDecl of FunctionDeclaration
        | VarDecl of VariableDeclaration
        | StructDecl of StructDeclaration

    type T = Program of Declaration list

(* The complete typed AST *)
module Typed =
    open Ops
    open StorageClass

    type UnaryOperator = Ops.UnaryOperator
    type BinaryOperator = Ops.BinaryOperator
    type StorageClass = StorageClass.StorageClass

    type InnerExp = TypedExp.InnerExp
    type Exp = TypedExp.Exp
    type Initializr = TypedExp.Initializr

    type MemberDeclaration =
        { memberName: string
          memberType: Types.t }

    type StructDeclaration =
        { tag: string
          members: MemberDeclaration list }

    type VariableDeclaration =
        { name: string
          varType: Types.t
          init: Initializr option
          storageClass: StorageClass option }

    type ForInit =
        | InitDecl of VariableDeclaration
        | InitExp of Exp option

    type Statement =
        | Return of Exp option
        | Expression of Exp
        | If of condition: Exp * thenClause: Statement * elseClause: Statement option
        | Compound of Block
        | Break of string
        | Continue of string
        | While of condition: Exp * body: Statement * id: string
        | DoWhile of body: Statement * condition: Exp * id: string
        | For of
            init: ForInit *
            condition: Exp option *
            post: Exp option *
            body: Statement *
            id: string
        | Null

    and BlockItem =
        | S of Statement
        | D of Declaration

    and Block = Block of BlockItem list

    and FunctionDeclaration =
        { name: string
          funType: Types.t
          ``params``: string list
          body: Block option
          storageClass: StorageClass option }

    and Declaration =
        | FunDecl of FunctionDeclaration
        | VarDecl of VariableDeclaration
        | StructDecl of StructDeclaration

    type T = Program of Declaration list