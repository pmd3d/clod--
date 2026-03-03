module Parse

open System.Numerics
open System

module T = Tokens
module Ast = Ast.Untyped

exception ParseError of string

type private Expected =
    | Tok of T.Token
    | Name of string

let private ppExpected =
    function
    | Tok tk -> Tokens.show tk
    | Name s -> s

let private raiseError expected actual =
    let msg =
        sprintf "Expected %s but found %s"
            (ppExpected expected)
            (Tokens.show actual)
    raise (ParseError msg)

let private expect expected (tokens: TokStream.TokStream) =
    let actual, tokens = TokStream.takeToken tokens
    if actual <> expected then
        raiseError (Tok expected) actual
    tokens

let private unescape (s: string) =
    let escapes =
        [ ("\\'", char 39)
          ("\\\"", char 34)
          ("\\?", char 63)
          ("\\\\", char 92)
          ("\\a", char 7)
          ("\\b", char 8)
          ("\\f", char 12)
          ("\\n", char 10)
          ("\\r", char 13)
          ("\\t", char 9)
          ("\\v", char 11) ]

    let rec unescapeNext (remaining: string) =
        if remaining = "" then
            []
        else
            let findMatchingEscape (escapeSeq: string, _) =
                remaining.StartsWith(escapeSeq)
            match List.tryFind findMatchingEscape escapes with
            | Some(escapeSeq, unescaped) ->
                unescaped
                :: unescapeNext
                       (StringUtil.drop escapeSeq.Length remaining)
            | None ->
                remaining.[0]
                :: unescapeNext (StringUtil.drop 1 remaining)

    let unescapedList = unescapeNext s
    StringUtil.ofList unescapedList

let private isIdent =
    function
    | T.Identifier _ -> true
    | _ -> false

let private parseId (tokens: TokStream.TokStream) =
    let tok, tokens = TokStream.takeToken tokens
    match tok with
    | T.Identifier x -> (x, tokens)
    | other -> raiseError (Name "an identifier") other

let private isTypeSpecifier =
    function
    | T.KWInt
    | T.KWLong
    | T.KWUnsigned
    | T.KWSigned
    | T.KWDouble
    | T.KWChar
    | T.KWVoid
    | T.KWStruct -> true
    | _ -> false

let private parseTypeSpecifier (tokens: TokStream.TokStream) =
    let spec, tokens = TokStream.takeToken tokens
    match spec with
    | T.KWStruct ->
        let expectedTag, tokens = TokStream.takeToken tokens
        if isIdent expectedTag then
            (expectedTag, tokens)
        else
            raiseError (Name "a structure tag") expectedTag
    | _ ->
        if isTypeSpecifier spec then
            (spec, tokens)
        else
            raiseError (Name "a type specifier") spec

let rec private parseTypeSpecifierList (tokens: TokStream.TokStream) =
    let spec, tokens = parseTypeSpecifier tokens
    if isTypeSpecifier (TokStream.peek tokens) then
        let rest, tokens = parseTypeSpecifierList tokens
        (spec :: rest, tokens)
    else
        ([ spec ], tokens)

let private isSpecifier =
    function
    | T.KWStatic | T.KWExtern -> true
    | other -> isTypeSpecifier other

let private parseSpecifier (tokens: TokStream.TokStream) =
    let spec = TokStream.peek tokens
    if isTypeSpecifier spec then
        parseTypeSpecifier tokens
    else if isSpecifier spec then
        let spec, tokens = TokStream.takeToken tokens
        (spec, tokens)
    else
        raiseError (Name "a type or storage-class specifier") spec

let rec private parseSpecifierList (tokens: TokStream.TokStream) =
    let spec, tokens = parseSpecifier tokens
    if isSpecifier (TokStream.peek tokens) then
        let rest, tokens = parseSpecifierList tokens
        (spec :: rest, tokens)
    else
        ([ spec ], tokens)

let private parseStorageClass =
    function
    | T.KWExtern -> Ast.StorageClass.Extern
    | T.KWStatic -> Ast.StorageClass.Static
    | other -> raiseError (Name "a storage class specifier") other

let private parseType specifierList =
    let specifierList = List.sortWith Tokens.compare specifierList
    match specifierList with
    | [ T.Identifier tag ] -> Types.Structure tag
    | [ T.KWVoid ] -> Types.Void
    | [ T.KWDouble ] -> Types.Double
    | [ T.KWChar ] -> Types.Char
    | [ T.KWChar; T.KWSigned ] -> Types.SChar
    | [ T.KWChar; T.KWUnsigned ] -> Types.UChar
    | _ ->
        if
            specifierList = []
            || List.sortWith compare (List.distinct specifierList)
               <> List.sortWith compare specifierList
            || List.contains T.KWDouble specifierList
            || List.contains T.KWChar specifierList
            || List.contains T.KWVoid specifierList
            || List.exists isIdent specifierList
            || List.contains T.KWSigned specifierList
               && List.contains T.KWUnsigned specifierList
        then
            raise (ParseError "Invalid type specifier")
        else if
            List.contains T.KWUnsigned specifierList
            && List.contains T.KWLong specifierList
        then
            Types.ULong
        else if List.contains T.KWUnsigned specifierList then
            Types.UInt
        else if List.contains T.KWLong specifierList then
            Types.Long
        else
            Types.Int

let private parseTypeAndStorageClass specifierList =
    let types, storageClasses =
        List.partition
            (fun t -> isTypeSpecifier t || isIdent t)
            specifierList
    let typ = parseType types
    let storageClass =
        match storageClasses with
        | [] -> None
        | [ sc ] -> Some(parseStorageClass sc)
        | _ :: _ -> failwith "Internal error - not a storage class"
    (typ, storageClass)

let private parseSignedConstant token =
    let v, isInt =
        match token with
        | T.ConstInt i -> (i, true)
        | T.ConstLong l -> (l, false)
        | other ->
            raiseError (Name "a signed integer constant") other
    if v > (BigInteger.Pow(2I, 63) - 1I) then
        raise
            (ParseError
                "Constant is too large to represent as an int or long")
    else if isInt && v <= (BigInteger.Pow(2I, 31) - 1I) then
        Const.ConstInt(int32 v)
    else
        Const.ConstLong(int64 v)

let private parseUnsignedConstant token =
    let v, isUint =
        match token with
        | T.ConstUInt ui -> (ui, true)
        | T.ConstULong ul -> (ul, false)
        | other ->
            raiseError (Name "an unsigned integer  constant") other
    if v > (BigInteger.Pow(2I, 64) - 1I) then
        raise
            (ParseError
                "Constant is too large to represent as an unsigned int or unsigned \
                 long")
    else if isUint && v <= (BigInteger.Pow(2I, 32) - 1I) then
        Const.ConstUInt(uint32 v)
    else
        Const.ConstULong(uint64 v)

let private parseChar token =
    let unescaped = unescape token
    if String.length unescaped = 1 then
        let chCode = int unescaped.[0]
        Const.ConstInt(int32 chCode)
    else
        raise (ParseError "multi-character constant tokens not supported")

let parseConst (tokens: TokStream.TokStream) =
    let constTok, tokens = TokStream.takeToken tokens
    let result =
        match constTok with
        | T.ConstInt _ | T.ConstLong _ -> parseSignedConstant constTok
        | T.ConstUInt _ | T.ConstULong _ -> parseUnsignedConstant constTok
        | T.ConstDouble d -> Const.ConstDouble d
        | T.ConstChar c -> parseChar c
        | other -> raiseError (Name "a constant token") other
    (result, tokens)

let private parseDim (tokens: TokStream.TokStream) =
    let tokens = expect T.OpenBracket tokens
    let c, tokens = parseConst tokens
    let dim =
        match c with
        | Const.ConstDouble _ ->
            raise (ParseError "Floating-point array dimensions not allowed")
        | Const.ConstChar c -> int64 c
        | Const.ConstInt i -> int64 i
        | Const.ConstLong l -> int64 l
        | Const.ConstUChar uc -> int64 uc
        | Const.ConstUInt u -> int64 u
        | Const.ConstULong ul -> int64 ul
    let tokens = expect T.CloseBracket tokens
    (dim, tokens)

let private parseString (tokens: TokStream.TokStream) =
    let tok, tokens = TokStream.takeToken tokens
    match tok with
    | T.StringLiteral s -> (unescape s, tokens)
    | other -> raiseError (Name "a string literal") other

type private AbstractDeclarator =
    | AbstractPointer of AbstractDeclarator
    | AbstractArray of AbstractDeclarator * int64
    | AbstractBase

let rec private parseAbstractArrayDeclSuffix baseDecl (tokens: TokStream.TokStream) =
    let dim, tokens = parseDim tokens
    let newDecl = AbstractArray(baseDecl, dim)
    if TokStream.peek tokens = T.OpenBracket then
        parseAbstractArrayDeclSuffix newDecl tokens
    else
        (newDecl, tokens)

let rec private parseAbstractDeclarator (tokens: TokStream.TokStream) =
    match TokStream.peek tokens with
    | T.Star ->
        let _, tokens = TokStream.takeToken tokens
        let inner, tokens =
            match TokStream.peek tokens with
            | T.Star | T.OpenParen | T.OpenBracket ->
                parseAbstractDeclarator tokens
            | _ -> (AbstractBase, tokens)
        (AbstractPointer inner, tokens)
    | _ -> parseDirectAbstractDeclarator tokens

and private parseDirectAbstractDeclarator (tokens: TokStream.TokStream) =
    if TokStream.peek tokens = T.OpenParen then
        let _, tokens = TokStream.takeToken tokens
        let decl, tokens = parseAbstractDeclarator tokens
        let tokens = expect T.CloseParen tokens
        if TokStream.peek tokens = T.OpenBracket then
            parseAbstractArrayDeclSuffix decl tokens
        else
            (decl, tokens)
    else
        parseAbstractArrayDeclSuffix AbstractBase tokens

let rec private processAbstractDeclarator decl baseType =
    match decl with
    | AbstractBase -> baseType
    | AbstractPointer inner ->
        let derivedType = Types.Pointer baseType
        processAbstractDeclarator inner derivedType
    | AbstractArray(inner, size) ->
        let derivedType = Types.Array(baseType, size)
        processAbstractDeclarator inner derivedType

let private getPrecedence =
    function
    | T.Star | T.Slash | T.Percent -> Some 50
    | T.Plus | T.Hyphen -> Some 45
    | T.LessThan | T.LessOrEqual | T.GreaterThan | T.GreaterOrEqual ->
        Some 35
    | T.DoubleEqual | T.NotEqual -> Some 30
    | T.LogicalAnd -> Some 10
    | T.LogicalOr -> Some 5
    | T.QuestionMark -> Some 3
    | T.EqualSign -> Some 1
    | _ -> None

let private parseUnop (tokens: TokStream.TokStream) =
    let tok, tokens = TokStream.takeToken tokens
    let op =
        match tok with
        | T.Tilde -> Ast.UnaryOperator.Complement
        | T.Hyphen -> Ast.UnaryOperator.Negate
        | T.Bang -> Ast.UnaryOperator.Not
        | other -> raiseError (Name "a unary operator") other
    (op, tokens)

let private parseBinop (tokens: TokStream.TokStream) =
    let tok, tokens = TokStream.takeToken tokens
    let op =
        match tok with
        | T.Plus -> Ast.BinaryOperator.Add
        | T.Hyphen -> Ast.BinaryOperator.Subtract
        | T.Star -> Ast.BinaryOperator.Multiply
        | T.Slash -> Ast.BinaryOperator.Divide
        | T.Percent -> Ast.BinaryOperator.Mod
        | T.LogicalAnd -> Ast.BinaryOperator.And
        | T.LogicalOr -> Ast.BinaryOperator.Or
        | T.DoubleEqual -> Ast.BinaryOperator.Equal
        | T.NotEqual -> Ast.BinaryOperator.NotEqual
        | T.LessThan -> Ast.BinaryOperator.LessThan
        | T.LessOrEqual -> Ast.BinaryOperator.LessOrEqual
        | T.GreaterThan -> Ast.BinaryOperator.GreaterThan
        | T.GreaterOrEqual -> Ast.BinaryOperator.GreaterOrEqual
        | other -> raiseError (Name "a binary operator") other
    (op, tokens)

let private parseTypeName (tokens: TokStream.TokStream) =
    let typeSpecifiers, tokens = parseTypeSpecifierList tokens
    let baseType = parseType typeSpecifiers
    match TokStream.peek tokens with
    | T.CloseParen -> (baseType, tokens)
    | _ ->
        let abstractDecl, tokens = parseAbstractDeclarator tokens
        (processAbstractDeclarator abstractDecl baseType, tokens)

let rec private parsePrimaryExp (tokens: TokStream.TokStream) =
    let nextToken = TokStream.peek tokens
    match nextToken with
    | T.ConstInt _
    | T.ConstLong _
    | T.ConstUInt _
    | T.ConstULong _
    | T.ConstDouble _
    | T.ConstChar _ ->
        let c, tokens = parseConst tokens
        (Ast.Exp.Constant c, tokens)
    | T.Identifier _ ->
        let id, tokens = parseId tokens
        if TokStream.peek tokens = T.OpenParen then
            let _, tokens = TokStream.takeToken tokens
            let args, tokens =
                if TokStream.peek tokens = T.CloseParen then
                    ([], tokens)
                else
                    parseArgumentList tokens
            let tokens = expect T.CloseParen tokens
            (Ast.Exp.FunCall(id, args), tokens)
        else
            (Ast.Exp.Var id, tokens)
    | T.OpenParen ->
        let _, tokens = TokStream.takeToken tokens
        let e, tokens = parseExp 0 tokens
        let tokens = expect T.CloseParen tokens
        (e, tokens)
    | T.StringLiteral _ ->
        let rec parseStringLoop tokens =
            let s, tokens = parseString tokens
            match TokStream.peek tokens with
            | T.StringLiteral _ ->
                let rest, tokens = parseStringLoop tokens
                (s + rest, tokens)
            | _ -> (s, tokens)
        let s, tokens = parseStringLoop tokens
        (Ast.Exp.String s, tokens)
    | t -> raiseError (Name "a primary expression") t

and private parseArgumentList (tokens: TokStream.TokStream) =
    let arg, tokens = parseExp 0 tokens
    if TokStream.peek tokens = T.Comma then
        let _, tokens = TokStream.takeToken tokens
        let rest, tokens = parseArgumentList tokens
        (arg :: rest, tokens)
    else
        ([ arg ], tokens)

and private parsePostfixExp (tokens: TokStream.TokStream) =
    let primary, tokens = parsePrimaryExp tokens

    let rec postfixLoop e tokens =
        match TokStream.peek tokens with
        | T.OpenBracket ->
            let _, tokens = TokStream.takeToken tokens
            let subscript, tokens = parseExp 0 tokens
            let tokens = expect T.CloseBracket tokens
            let subscriptExp = Ast.Exp.Subscript(e, subscript)
            postfixLoop subscriptExp tokens
        | T.Dot ->
            let _, tokens = TokStream.takeToken tokens
            let ``member``, tokens = parseId tokens
            let dotExp = Ast.Exp.Dot(e, ``member``)
            postfixLoop dotExp tokens
        | T.Arrow ->
            let _, tokens = TokStream.takeToken tokens
            let ``member``, tokens = parseId tokens
            let arrowExp = Ast.Exp.Arrow(e, ``member``)
            postfixLoop arrowExp tokens
        | _ -> (e, tokens)

    postfixLoop primary tokens

and private parseUnaryExp (tokens: TokStream.TokStream) =
    match TokStream.npeek 3 tokens with
    | T.Star :: _ ->
        let _, tokens = TokStream.takeToken tokens
        let innerExp, tokens = parseCastExp tokens
        (Ast.Exp.Dereference innerExp, tokens)
    | T.Ampersand :: _ ->
        let _, tokens = TokStream.takeToken tokens
        let innerExp, tokens = parseCastExp tokens
        (Ast.Exp.AddrOf innerExp, tokens)
    | (T.Hyphen | T.Tilde | T.Bang) :: _ ->
        let operator, tokens = parseUnop tokens
        let innerExp, tokens = parseCastExp tokens
        (Ast.Exp.Unary(operator, innerExp), tokens)
    | [ T.KWSizeOf; T.OpenParen; t ] when isTypeSpecifier t ->
        let _, tokens = TokStream.takeToken tokens
        let _, tokens = TokStream.takeToken tokens
        let typ, tokens = parseTypeName tokens
        let tokens = expect T.CloseParen tokens
        (Ast.Exp.SizeOfT typ, tokens)
    | T.KWSizeOf :: _ ->
        let _, tokens = TokStream.takeToken tokens
        let innerExp, tokens = parseUnaryExp tokens
        (Ast.Exp.SizeOf innerExp, tokens)
    | _ -> parsePostfixExp tokens

and private parseCastExp (tokens: TokStream.TokStream) =
    match TokStream.npeek 2 tokens with
    | [ T.OpenParen; t ] when isTypeSpecifier t ->
        let _, tokens = TokStream.takeToken tokens
        let targetType, tokens = parseTypeName tokens
        let tokens = expect T.CloseParen tokens
        let innerExp, tokens = parseCastExp tokens
        (Ast.Exp.Cast(targetType, innerExp), tokens)
    | _ -> parseUnaryExp tokens

and private parseConditionalMiddle (tokens: TokStream.TokStream) =
    let tokens = expect T.QuestionMark tokens
    let e, tokens = parseExp 0 tokens
    let tokens = expect T.Colon tokens
    (e, tokens)

and parseExp minPrec (tokens: TokStream.TokStream) =
    let initialFactor, tokens = parseCastExp tokens
    let nextToken = TokStream.peek tokens

    let rec parseExpLoop left next tokens =
        match getPrecedence next with
        | Some prec when prec >= minPrec ->
            let left, tokens =
                if next = T.EqualSign then
                    let _, tokens = TokStream.takeToken tokens
                    let right, tokens = parseExp prec tokens
                    (Ast.Exp.Assignment(left, right), tokens)
                else if next = T.QuestionMark then
                    let middle, tokens = parseConditionalMiddle tokens
                    let right, tokens = parseExp prec tokens
                    (Ast.Exp.Conditional(left, middle, right), tokens)
                else
                    let operator, tokens = parseBinop tokens
                    let right, tokens = parseExp (prec + 1) tokens
                    (Ast.Exp.Binary(operator, left, right), tokens)
            parseExpLoop left (TokStream.peek tokens) tokens
        | _ -> (left, tokens)

    parseExpLoop initialFactor nextToken tokens

let private parseOptionalExp delim (tokens: TokStream.TokStream) =
    if TokStream.peek tokens = delim then
        let _, tokens = TokStream.takeToken tokens
        (None, tokens)
    else
        let e, tokens = parseExp 0 tokens
        let tokens = expect delim tokens
        (Some e, tokens)

type private Declarator =
    | Ident of string
    | PointerDeclarator of Declarator
    | ArrayDeclarator of Declarator * int64
    | FunDeclarator of ParamInfo list * Declarator

and private ParamInfo = Param of Types.CType * Declarator

let rec private parseArrayDeclSuffix baseDecl (tokens: TokStream.TokStream) =
    let dim, tokens = parseDim tokens
    let newDecl = ArrayDeclarator(baseDecl, dim)
    if TokStream.peek tokens = T.OpenBracket then
        parseArrayDeclSuffix newDecl tokens
    else
        (newDecl, tokens)

let rec private parseDeclarator (tokens: TokStream.TokStream) =
    match TokStream.peek tokens with
    | T.Star ->
        let _, tokens = TokStream.takeToken tokens
        let inner, tokens = parseDeclarator tokens
        (PointerDeclarator inner, tokens)
    | _ -> parseDirectDeclarator tokens

and private parseDirectDeclarator (tokens: TokStream.TokStream) =
    let simpleDec, tokens = parseSimpleDeclarator tokens
    match TokStream.peek tokens with
    | T.OpenParen ->
        let ``params``, tokens = parseParamList tokens
        (FunDeclarator(``params``, simpleDec), tokens)
    | T.OpenBracket -> parseArrayDeclSuffix simpleDec tokens
    | _ -> (simpleDec, tokens)

and private parseParamList (tokens: TokStream.TokStream) =
    if TokStream.npeek 2 tokens = [ T.OpenParen; T.CloseParen ] then
        let _, tokens = TokStream.takeToken tokens
        let _, tokens = TokStream.takeToken tokens
        ([], tokens)
    else if TokStream.npeek 3 tokens = [ T.OpenParen; T.KWVoid; T.CloseParen ] then
        let _, tokens = TokStream.takeToken tokens
        let _, tokens = TokStream.takeToken tokens
        let _, tokens = TokStream.takeToken tokens
        ([], tokens)
    else
        let tokens = expect T.OpenParen tokens
        let rec paramLoop tokens =
            let nextParam, tokens = parseParam tokens
            if TokStream.peek tokens = T.Comma then
                let _, tokens = TokStream.takeToken tokens
                let rest, tokens = paramLoop tokens
                (nextParam :: rest, tokens)
            else
                ([ nextParam ], tokens)
        let ``params``, tokens = paramLoop tokens
        let tokens = expect T.CloseParen tokens
        (``params``, tokens)

and private parseParam (tokens: TokStream.TokStream) =
    let specs, tokens = parseTypeSpecifierList tokens
    let paramT = parseType specs
    let paramDecl, tokens = parseDeclarator tokens
    (Param(paramT, paramDecl), tokens)

and private parseSimpleDeclarator (tokens: TokStream.TokStream) =
    let nextTok, tokens = TokStream.takeToken tokens
    match nextTok with
    | T.OpenParen ->
        let decl, tokens = parseDeclarator tokens
        let tokens = expect T.CloseParen tokens
        (decl, tokens)
    | T.Identifier id -> (Ident id, tokens)
    | other -> raiseError (Name "a simple declarator") other

let rec private processDeclarator decl baseType =
    match decl with
    | Ident s -> (s, baseType, [])
    | PointerDeclarator d ->
        let derivedType = Types.Pointer baseType
        processDeclarator d derivedType
    | ArrayDeclarator(inner, size) ->
        let derivedType = Types.Array(baseType, size)
        processDeclarator inner derivedType
    | FunDeclarator(``params``, Ident s) ->
        let processParam (Param(pBaseType, pDecl)) =
            let paramName, paramT, _ = processDeclarator pDecl pBaseType
            match paramT with
            | Types.FunType _ ->
                raise
                    (ParseError
                        "Function pointers in parameters are not supported")
            | _ -> ()
            (paramName, paramT)
        let paramNames, paramTypes =
            List.unzip (List.map processParam ``params``)
        let funType =
            Types.FunType(paramTypes, baseType)
        (s, funType, paramNames)
    | FunDeclarator _ ->
        raise
            (ParseError
                "can't apply additional type derivations to a function declarator")

let rec private parseInitializer (tokens: TokStream.TokStream) =
    if TokStream.peek tokens = T.OpenBrace then
        let _, tokens = TokStream.takeToken tokens
        let rec parseInitLoop tokens =
            let nextInit, tokens = parseInitializer tokens
            match TokStream.npeek 2 tokens with
            | [ T.Comma; T.CloseBrace ] ->
                let _, tokens = TokStream.takeToken tokens
                ([ nextInit ], tokens)
            | T.Comma :: _ ->
                let _, tokens = TokStream.takeToken tokens
                let rest, tokens = parseInitLoop tokens
                (nextInit :: rest, tokens)
            | _ -> ([ nextInit ], tokens)
        let initList, tokens = parseInitLoop tokens
        let tokens = expect T.CloseBrace tokens
        (Ast.Initializer.CompoundInit initList, tokens)
    else
        let e, tokens = parseExp 0 tokens
        (Ast.Initializer.SingleInit e, tokens)

let private parseMemberDeclaration (tokens: TokStream.TokStream) =
    let specifiers, tokens = parseTypeSpecifierList tokens
    let baseType = parseType specifiers
    let decl, tokens = parseDeclarator tokens
    match decl with
    | FunDeclarator _ ->
        raise (ParseError "Found function declarator in struct member list")
    | _ ->
        let tokens = expect T.Semicolon tokens
        let memberName, memberType, _params =
            processDeclarator decl baseType
        let result : Ast.MemberDeclaration =
            { memberName = memberName
              memberType = memberType }
        (result, tokens)

let private parseStructDeclaration (tokens: TokStream.TokStream) =
    let tokens = expect T.KWStruct tokens
    let tag, tokens = parseId tokens
    let members, tokens =
        match TokStream.peek tokens with
        | T.OpenBrace ->
            let _, tokens = TokStream.takeToken tokens
            let rec parseMemberLoop tokens =
                let nextMember, tokens = parseMemberDeclaration tokens
                if TokStream.peek tokens = T.CloseBrace then
                    ([ nextMember ], tokens)
                else
                    let rest, tokens = parseMemberLoop tokens
                    (nextMember :: rest, tokens)
            let members, tokens = parseMemberLoop tokens
            let tokens = expect T.CloseBrace tokens
            (members, tokens)
        | _ -> ([], tokens)
    let tokens = expect T.Semicolon tokens
    let result : Ast.StructDeclaration =
        { tag = tag
          members = members }
    (result, tokens)

let rec private parseFunctionOrVariableDeclaration (tokens: TokStream.TokStream) =
    let specifiers, tokens = parseSpecifierList tokens
    let baseType, storageClass = parseTypeAndStorageClass specifiers
    let decl, tokens = parseDeclarator tokens
    let name, typ, ``params`` = processDeclarator decl baseType
    match typ with
    | Types.FunType _ ->
        let body, tokens =
            match TokStream.peek tokens with
            | T.Semicolon ->
                let _, tokens = TokStream.takeToken tokens
                (None, tokens)
            | _ ->
                let block, tokens = parseBlock tokens
                (Some block, tokens)
        (Ast.FunDecl
            { name = name
              funType = typ
              storageClass = storageClass
              ``params`` = ``params``
              body = body }, tokens)
    | _ ->
        let init, tokens =
            if TokStream.peek tokens = T.EqualSign then
                let _, tokens = TokStream.takeToken tokens
                let init, tokens = parseInitializer tokens
                (Some init, tokens)
            else
                (None, tokens)
        let tokens = expect T.Semicolon tokens
        (Ast.VarDecl
            { name = name
              varType = typ
              storageClass = storageClass
              init = init }, tokens)

and private parseDeclaration (tokens: TokStream.TokStream) =
    match TokStream.npeek 3 tokens with
    | [ T.KWStruct; T.Identifier _; (T.OpenBrace | T.Semicolon) ] ->
        let sd, tokens = parseStructDeclaration tokens
        (Ast.StructDecl sd, tokens)
    | _ -> parseFunctionOrVariableDeclaration tokens

and private parseForInit (tokens: TokStream.TokStream) =
    if isSpecifier (TokStream.peek tokens) then
        let decl, tokens = parseDeclaration tokens
        match decl with
        | Ast.VarDecl vd -> (Ast.InitDecl vd, tokens)
        | _ ->
            raise
                (ParseError
                    "Found a function declaration in a for loop header")
    else
        let optE, tokens = parseOptionalExp T.Semicolon tokens
        (Ast.InitExp optE, tokens)

and parseStatement (tokens: TokStream.TokStream) =
    match TokStream.peek tokens with
    | T.KWReturn ->
        let _, tokens = TokStream.takeToken tokens
        let optExp, tokens = parseOptionalExp T.Semicolon tokens
        (Ast.Return optExp, tokens)
    | T.KWIf ->
        let _, tokens = TokStream.takeToken tokens
        let tokens = expect T.OpenParen tokens
        let condition, tokens = parseExp 0 tokens
        let tokens = expect T.CloseParen tokens
        let thenClause, tokens = parseStatement tokens
        let elseClause, tokens =
            if TokStream.peek tokens = T.KWElse then
                let _, tokens = TokStream.takeToken tokens
                let e, tokens = parseStatement tokens
                (Some e, tokens)
            else
                (None, tokens)
        (Ast.If(condition, thenClause, elseClause), tokens)
    | T.OpenBrace ->
        let block, tokens = parseBlock tokens
        (Ast.Compound block, tokens)
    | T.KWBreak ->
        let _, tokens = TokStream.takeToken tokens
        let tokens = expect T.Semicolon tokens
        (Ast.Break "", tokens)
    | T.KWContinue ->
        let _, tokens = TokStream.takeToken tokens
        let tokens = expect T.Semicolon tokens
        (Ast.Continue "", tokens)
    | T.KWWhile ->
        let _, tokens = TokStream.takeToken tokens
        let tokens = expect T.OpenParen tokens
        let condition, tokens = parseExp 0 tokens
        let tokens = expect T.CloseParen tokens
        let body, tokens = parseStatement tokens
        (Ast.While(condition, body, ""), tokens)
    | T.KWDo ->
        let tokens = expect T.KWDo tokens
        let body, tokens = parseStatement tokens
        let tokens = expect T.KWWhile tokens
        let tokens = expect T.OpenParen tokens
        let condition, tokens = parseExp 0 tokens
        let tokens = expect T.CloseParen tokens
        let tokens = expect T.Semicolon tokens
        (Ast.DoWhile(body, condition, ""), tokens)
    | T.KWFor ->
        let tokens = expect T.KWFor tokens
        let tokens = expect T.OpenParen tokens
        let init, tokens = parseForInit tokens
        let condition, tokens = parseOptionalExp T.Semicolon tokens
        let post, tokens = parseOptionalExp T.CloseParen tokens
        let body, tokens = parseStatement tokens
        (Ast.For(init, condition, post, body, ""), tokens)
    | _ ->
        let optExp, tokens = parseOptionalExp T.Semicolon tokens
        match optExp with
        | Some e -> (Ast.Expression e, tokens)
        | None -> (Ast.Null, tokens)

and private parseBlockItem (tokens: TokStream.TokStream) =
    if isSpecifier (TokStream.peek tokens) then
        let decl, tokens = parseDeclaration tokens
        (Ast.Decl decl, tokens)
    else
        let stmt, tokens = parseStatement tokens
        (Ast.Stmt stmt, tokens)

and private parseBlock (tokens: TokStream.TokStream) =
    let tokens = expect T.OpenBrace tokens
    let rec parseBlockItemLoop tokens =
        if TokStream.peek tokens = T.CloseBrace then
            ([], tokens)
        else
            let nextBlockItem, tokens = parseBlockItem tokens
            let rest, tokens = parseBlockItemLoop tokens
            (nextBlockItem :: rest, tokens)
    let block, tokens = parseBlockItemLoop tokens
    let tokens = expect T.CloseBrace tokens
    (Ast.Block block, tokens)

let private parseProgram (tokens: TokStream.TokStream) =
    let rec parseDeclLoop tokens =
        if TokStream.isEmpty tokens then
            ([], tokens)
        else
            let nextDecl, tokens = parseDeclaration tokens
            let rest, tokens = parseDeclLoop tokens
            (nextDecl :: rest, tokens)
    let funDecls, _tokens = parseDeclLoop tokens
    Ast.UntypedProgram.Program funDecls

let parse tokens =
    try
        let tokenStream = TokStream.ofList tokens
        parseProgram tokenStream
    with
    | TokStream.End_of_stream ->
        raise (ParseError "Unexpected end of file")
