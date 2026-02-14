module Parse

open System.Numerics
open System

module T = Tokens
module Ast = Ast.Untyped

exception ParseError of string

module Private =

    type Expected =
        | Tok of T.t
        | Name of string

    let ppExpected =
        function
        | Tok tk -> Tokens.show tk
        | Name s -> s

    let raiseError expected actual =
        let msg =
            sprintf "Expected %s but found %s"
                (ppExpected expected)
                (Tokens.show actual)
        raise (ParseError msg)

    let expect expected (tokens: TokStream.t) =
        let actual = TokStream.takeToken tokens
        if actual <> expected then
            raiseError (Tok expected) actual

    let unescape (s: string) =
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

    let isIdent =
        function
        | T.Identifier _ -> true
        | _ -> false

    let parseId (tokens: TokStream.t) =
        match TokStream.takeToken tokens with
        | T.Identifier x -> x
        | other -> raiseError (Name "an identifier") other

    let isTypeSpecifier =
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

    let parseTypeSpecifier (tokens: TokStream.t) =
        let spec = TokStream.takeToken tokens
        match spec with
        | T.KWStruct ->
            let expectedTag = TokStream.takeToken tokens
            if isIdent expectedTag then
                expectedTag
            else
                raiseError (Name "a structure tag") expectedTag
        | _ ->
            if isTypeSpecifier spec then
                spec
            else
                raiseError (Name "a type specifier") spec

    let rec parseTypeSpecifierList (tokens: TokStream.t) =
        let spec = parseTypeSpecifier tokens
        if isTypeSpecifier (TokStream.peek tokens) then
            spec :: parseTypeSpecifierList tokens
        else
            [ spec ]

    let isSpecifier =
        function
        | T.KWStatic | T.KWExtern -> true
        | other -> isTypeSpecifier other

    let parseSpecifier (tokens: TokStream.t) =
        let spec = TokStream.peek tokens
        if isTypeSpecifier spec then
            parseTypeSpecifier tokens
        else if isSpecifier spec then
            let _ = TokStream.takeToken tokens
            spec
        else
            raiseError (Name "a type or storage-class specifier") spec

    let rec parseSpecifierList (tokens: TokStream.t) =
        let spec = parseSpecifier tokens
        if isSpecifier (TokStream.peek tokens) then
            spec :: parseSpecifierList tokens
        else
            [ spec ]

    let parseStorageClass =
        function
        | T.KWExtern -> Ast.StorageClass.Extern
        | T.KWStatic -> Ast.StorageClass.Static
        | other -> raiseError (Name "a storage class specifier") other

    let parseType specifierList =
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

    let parseTypeAndStorageClass specifierList =
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

    let parseSignedConstant token =
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

    let parseUnsignedConstant token =
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

    let parseChar token =
        let unescaped = unescape token
        if String.length unescaped = 1 then
            let chCode = int unescaped.[0]
            Const.ConstInt(int32 chCode)
        else
            raise (ParseError "multi-character constant tokens not supported")

    let parseConst (tokens: TokStream.t) =
        let constTok = TokStream.takeToken tokens
        match constTok with
        | T.ConstInt _ | T.ConstLong _ -> parseSignedConstant constTok
        | T.ConstUInt _ | T.ConstULong _ -> parseUnsignedConstant constTok
        | T.ConstDouble d -> Const.ConstDouble d
        | T.ConstChar c -> parseChar c
        | other -> raiseError (Name "a constant token") other

    let parseDim (tokens: TokStream.t) =
        expect T.OpenBracket tokens
        let dim =
            match parseConst tokens with
            | Const.ConstDouble _ ->
                raise (ParseError "Floating-point array dimensions not allowed")
            | Const.ConstChar c -> int c
            | Const.ConstInt i -> int i
            | Const.ConstLong l -> int l
            | Const.ConstUChar uc -> int uc
            | Const.ConstUInt u -> int u
            | Const.ConstULong ul -> int ul
        expect T.CloseBracket tokens
        dim

    let parseString (tokens: TokStream.t) =
        match TokStream.takeToken tokens with
        | T.StringLiteral s -> unescape s
        | other -> raiseError (Name "a string literal") other

    type AbstractDeclarator =
        | AbstractPointer of AbstractDeclarator
        | AbstractArray of AbstractDeclarator * int
        | AbstractBase

    let rec parseAbstractArrayDeclSuffix baseDecl (tokens: TokStream.t) =
        let dim = parseDim tokens
        let newDecl = AbstractArray(baseDecl, dim)
        if TokStream.peek tokens = T.OpenBracket then
            parseAbstractArrayDeclSuffix newDecl tokens
        else
            newDecl

    let rec parseAbstractDeclarator (tokens: TokStream.t) =
        match TokStream.peek tokens with
        | T.Star ->
            let _ = TokStream.takeToken tokens
            let inner =
                match TokStream.peek tokens with
                | T.Star | T.OpenParen | T.OpenBracket ->
                    parseAbstractDeclarator tokens
                | _ -> AbstractBase
            AbstractPointer inner
        | _ -> parseDirectAbstractDeclarator tokens

    and parseDirectAbstractDeclarator (tokens: TokStream.t) =
        if TokStream.peek tokens = T.OpenParen then
            let _ = TokStream.takeToken tokens
            let decl = parseAbstractDeclarator tokens
            expect T.CloseParen tokens
            if TokStream.peek tokens = T.OpenBracket then
                parseAbstractArrayDeclSuffix decl tokens
            else
                decl
        else
            parseAbstractArrayDeclSuffix AbstractBase tokens

    let rec processAbstractDeclarator decl baseType =
        match decl with
        | AbstractBase -> baseType
        | AbstractPointer inner ->
            let derivedType = Types.Pointer baseType
            processAbstractDeclarator inner derivedType
        | AbstractArray(inner, size) ->
            let derivedType = Types.Array(baseType, size)
            processAbstractDeclarator inner derivedType

    let getPrecedence =
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

    let parseUnop (tokens: TokStream.t) =
        match TokStream.takeToken tokens with
        | T.Tilde -> Ast.UnaryOperator.Complement
        | T.Hyphen -> Ast.UnaryOperator.Negate
        | T.Bang -> Ast.UnaryOperator.Not
        | other -> raiseError (Name "a unary operator") other

    let parseBinop (tokens: TokStream.t) =
        match TokStream.takeToken tokens with
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

    let parseTypeName (tokens: TokStream.t) =
        let typeSpecifiers = parseTypeSpecifierList tokens
        let baseType = parseType typeSpecifiers
        match TokStream.peek tokens with
        | T.CloseParen -> baseType
        | _ ->
            let abstractDecl = parseAbstractDeclarator tokens
            processAbstractDeclarator abstractDecl baseType

    let rec parsePrimaryExp (tokens: TokStream.t) =
        let nextToken = TokStream.peek tokens
        match nextToken with
        | T.ConstInt _
        | T.ConstLong _
        | T.ConstUInt _
        | T.ConstULong _
        | T.ConstDouble _
        | T.ConstChar _ -> Ast.Exp.Constant(parseConst tokens)
        | T.Identifier _ ->
            let id = parseId tokens
            if TokStream.peek tokens = T.OpenParen then
                let _ = TokStream.takeToken tokens
                let args =
                    if TokStream.peek tokens = T.CloseParen then
                        []
                    else
                        parseArgumentList tokens
                let _ = expect T.CloseParen tokens
                Ast.Exp.FunCall(id, args)
            else
                Ast.Exp.Var id
        | T.OpenParen ->
            let _ = TokStream.takeToken tokens
            let e = parseExp 0 tokens
            expect T.CloseParen tokens
            e
        | T.StringLiteral _ ->
            let rec parseStringLoop () =
                let s = parseString tokens
                match TokStream.peek tokens with
                | T.StringLiteral _ -> s + parseStringLoop ()
                | _ -> s
            Ast.Exp.String(parseStringLoop ())
        | t -> raiseError (Name "a primary expression") t

    and parseArgumentList (tokens: TokStream.t) =
        let arg = parseExp 0 tokens
        if TokStream.peek tokens = T.Comma then
            let _ = TokStream.takeToken tokens
            arg :: parseArgumentList tokens
        else
            [ arg ]

    and parsePostfixExp (tokens: TokStream.t) =
        let primary = parsePrimaryExp tokens

        let rec postfixLoop e =
            match TokStream.peek tokens with
            | T.OpenBracket ->
                let _ = TokStream.takeToken tokens
                let subscript = parseExp 0 tokens
                let () = expect T.CloseBracket tokens
                let subscriptExp = Ast.Exp.Subscript(e, subscript)
                postfixLoop subscriptExp
            | T.Dot ->
                let _ = TokStream.takeToken tokens
                let ``member`` = parseId tokens
                let dotExp = Ast.Exp.Dot(e, ``member``)
                postfixLoop dotExp
            | T.Arrow ->
                let _ = TokStream.takeToken tokens
                let ``member`` = parseId tokens
                let arrowExp = Ast.Exp.Arrow(e, ``member``)
                postfixLoop arrowExp
            | _ -> e

        postfixLoop primary

    and parseUnaryExp (tokens: TokStream.t) =
        match TokStream.npeek 3 tokens with
        | T.Star :: _ ->
            let _ = TokStream.takeToken tokens
            let innerExp = parseCastExp tokens
            Ast.Exp.Dereference innerExp
        | T.Ampersand :: _ ->
            let _ = TokStream.takeToken tokens
            let innerExp = parseCastExp tokens
            Ast.Exp.AddrOf innerExp
        | (T.Hyphen | T.Tilde | T.Bang) :: _ ->
            let operator = parseUnop tokens
            let innerExp = parseCastExp tokens
            Ast.Exp.Unary(operator, innerExp)
        | [ T.KWSizeOf; T.OpenParen; t ] when isTypeSpecifier t ->
            let _ = TokStream.takeToken tokens
            let _ = TokStream.takeToken tokens
            let typ = parseTypeName tokens
            expect T.CloseParen tokens
            Ast.Exp.SizeOfT typ
        | T.KWSizeOf :: _ ->
            let _ = TokStream.takeToken tokens
            let innerExp = parseUnaryExp tokens
            Ast.Exp.SizeOf innerExp
        | _ -> parsePostfixExp tokens

    and parseCastExp (tokens: TokStream.t) =
        match TokStream.npeek 2 tokens with
        | [ T.OpenParen; t ] when isTypeSpecifier t ->
            let _ = TokStream.takeToken tokens
            let targetType = parseTypeName tokens
            let _ = expect T.CloseParen tokens
            let innerExp = parseCastExp tokens
            Ast.Exp.Cast(targetType, innerExp)
        | _ -> parseUnaryExp tokens

    and parseConditionalMiddle (tokens: TokStream.t) =
        expect T.QuestionMark tokens
        let e = parseExp 0 tokens
        expect T.Colon tokens
        e

    and parseExp minPrec (tokens: TokStream.t) =
        let initialFactor = parseCastExp tokens
        let nextToken = TokStream.peek tokens

        let rec parseExpLoop left next =
            match getPrecedence next with
            | Some prec when prec >= minPrec ->
                let left =
                    if next = T.EqualSign then
                        let _ = TokStream.takeToken tokens
                        let right = parseExp prec tokens
                        Ast.Exp.Assignment(left, right)
                    else if next = T.QuestionMark then
                        let middle = parseConditionalMiddle tokens
                        let right = parseExp prec tokens
                        Ast.Exp.Conditional(left, middle, right)
                    else
                        let operator = parseBinop tokens
                        let right = parseExp (prec + 1) tokens
                        Ast.Exp.Binary(operator, left, right)
                parseExpLoop left (TokStream.peek tokens)
            | _ -> left

        parseExpLoop initialFactor nextToken

    let parseOptionalExp delim (tokens: TokStream.t) =
        if TokStream.peek tokens = delim then
            let _ = TokStream.takeToken tokens
            None
        else
            let e = parseExp 0 tokens
            expect delim tokens
            Some e

    type Declarator =
        | Ident of string
        | PointerDeclarator of Declarator
        | ArrayDeclarator of Declarator * int
        | FunDeclarator of ParamInfo list * Declarator

    and ParamInfo = Param of Types.t * Declarator

    let rec parseArrayDeclSuffix baseDecl (tokens: TokStream.t) =
        let dim = parseDim tokens
        let newDecl = ArrayDeclarator(baseDecl, dim)
        if TokStream.peek tokens = T.OpenBracket then
            parseArrayDeclSuffix newDecl tokens
        else
            newDecl

    let rec parseDeclarator (tokens: TokStream.t) =
        match TokStream.peek tokens with
        | T.Star ->
            let _ = TokStream.takeToken tokens
            let inner = parseDeclarator tokens
            PointerDeclarator inner
        | _ -> parseDirectDeclarator tokens

    and parseDirectDeclarator (tokens: TokStream.t) =
        let simpleDec = parseSimpleDeclarator tokens
        match TokStream.peek tokens with
        | T.OpenParen ->
            let ``params`` = parseParamList tokens
            FunDeclarator(``params``, simpleDec)
        | T.OpenBracket -> parseArrayDeclSuffix simpleDec tokens
        | _ -> simpleDec

    and parseParamList (tokens: TokStream.t) =
        if TokStream.npeek 3 tokens = [ T.OpenParen; T.KWVoid; T.CloseParen ] then
            let _ = TokStream.takeToken tokens
            let _ = TokStream.takeToken tokens
            let _ = TokStream.takeToken tokens
            []
        else
            let _ = expect T.OpenParen tokens
            let rec paramLoop () =
                let nextParam = parseParam tokens
                if TokStream.peek tokens = T.Comma then
                    let _ = TokStream.takeToken tokens
                    nextParam :: paramLoop ()
                else
                    [ nextParam ]
            let ``params`` = paramLoop ()
            let _ = expect T.CloseParen tokens
            ``params``

    and parseParam (tokens: TokStream.t) =
        let paramT = parseType (parseTypeSpecifierList tokens)
        let paramDecl = parseDeclarator tokens
        Param(paramT, paramDecl)

    and parseSimpleDeclarator (tokens: TokStream.t) =
        let nextTok = TokStream.takeToken tokens
        match nextTok with
        | T.OpenParen ->
            let decl = parseDeclarator tokens
            expect T.CloseParen tokens
            decl
        | T.Identifier id -> Ident id
        | other -> raiseError (Name "a simple declarator") other

    let rec processDeclarator decl baseType =
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

    let rec parseInitializer (tokens: TokStream.t) =
        if TokStream.peek tokens = T.OpenBrace then
            let _ = TokStream.takeToken tokens
            let rec parseInitLoop () =
                let nextInit = parseInitializer tokens
                match TokStream.npeek 2 tokens with
                | [ T.Comma; T.CloseBrace ] ->
                    let _ = TokStream.takeToken tokens
                    [ nextInit ]
                | T.Comma :: _ ->
                    let _ = TokStream.takeToken tokens
                    nextInit :: parseInitLoop ()
                | _ -> [ nextInit ]
            let initList = parseInitLoop ()
            let () = expect T.CloseBrace tokens
            Ast.Initializr.CompoundInit initList
        else
            Ast.Initializr.SingleInit(parseExp 0 tokens)

    let parseMemberDeclaration (tokens: TokStream.t) =
        let specifiers = parseTypeSpecifierList tokens
        let baseType = parseType specifiers
        let decl = parseDeclarator tokens
        match decl with
        | FunDeclarator _ ->
            raise (ParseError "Found function declarator in struct member list")
        | _ ->
            expect T.Semicolon tokens
            let memberName, memberType, _params =
                processDeclarator decl baseType
            { memberName = memberName
              memberType = memberType } : Ast.MemberDeclaration

    let parseStructDeclaration (tokens: TokStream.t) =
        expect T.KWStruct tokens
        let tag = parseId tokens
        let members =
            match TokStream.peek tokens with
            | T.OpenBrace ->
                let _ = TokStream.takeToken tokens
                let rec parseMemberLoop () =
                    let nextMember = parseMemberDeclaration tokens
                    if TokStream.peek tokens = T.CloseBrace then
                        [ nextMember ]
                    else
                        nextMember :: parseMemberLoop ()
                let members = parseMemberLoop ()
                expect T.CloseBrace tokens
                members
            | _ -> []
        expect T.Semicolon tokens
        { tag = tag
          members = members } : Ast.StructDeclaration

    let rec parseFunctionOrVariableDeclaration (tokens: TokStream.t) =
        let specifiers = parseSpecifierList tokens
        let baseType, storageClass = parseTypeAndStorageClass specifiers
        let decl = parseDeclarator tokens
        let name, typ, ``params`` = processDeclarator decl baseType
        match typ with
        | Types.FunType _ ->
            let body =
                match TokStream.peek tokens with
                | T.Semicolon ->
                    let _ = TokStream.takeToken tokens
                    None
                | _ -> Some(parseBlock tokens)
            Ast.FunDecl
                { name = name
                  funType = typ
                  storageClass = storageClass
                  ``params`` = ``params``
                  body = body }
        | _ ->
            let init =
                if TokStream.peek tokens = T.EqualSign then
                    let _ = TokStream.takeToken tokens
                    Some(parseInitializer tokens)
                else
                    None
            expect T.Semicolon tokens
            Ast.VarDecl
                { name = name
                  varType = typ
                  storageClass = storageClass
                  init = init }

    and parseDeclaration (tokens: TokStream.t) =
        match TokStream.npeek 3 tokens with
        | [ T.KWStruct; T.Identifier _; (T.OpenBrace | T.Semicolon) ] ->
            Ast.StructDecl(parseStructDeclaration tokens)
        | _ -> parseFunctionOrVariableDeclaration tokens

    and parseForInit (tokens: TokStream.t) =
        if isSpecifier (TokStream.peek tokens) then
            match parseDeclaration tokens with
            | Ast.VarDecl vd -> Ast.InitDecl vd
            | _ ->
                raise
                    (ParseError
                        "Found a function declaration in a for loop header")
        else
            let optE = parseOptionalExp T.Semicolon tokens
            Ast.InitExp optE

    and parseStatement (tokens: TokStream.t) =
        match TokStream.peek tokens with
        | T.KWReturn ->
            let _ = TokStream.takeToken tokens
            let optExp = parseOptionalExp T.Semicolon tokens
            Ast.Return optExp
        | T.KWIf ->
            let _ = TokStream.takeToken tokens
            expect T.OpenParen tokens
            let condition = parseExp 0 tokens
            expect T.CloseParen tokens
            let thenClause = parseStatement tokens
            let elseClause =
                if TokStream.peek tokens = T.KWElse then
                    let _ = TokStream.takeToken tokens
                    Some(parseStatement tokens)
                else
                    None
            Ast.If(condition, thenClause, elseClause)
        | T.OpenBrace -> Ast.Compound(parseBlock tokens)
        | T.KWBreak ->
            let _ = TokStream.takeToken tokens
            expect T.Semicolon tokens
            Ast.Break ""
        | T.KWContinue ->
            let _ = TokStream.takeToken tokens
            expect T.Semicolon tokens
            Ast.Continue ""
        | T.KWWhile ->
            let _ = TokStream.takeToken tokens
            expect T.OpenParen tokens
            let condition = parseExp 0 tokens
            expect T.CloseParen tokens
            let body = parseStatement tokens
            Ast.While(condition, body, "")
        | T.KWDo ->
            expect T.KWDo tokens
            let body = parseStatement tokens
            expect T.KWWhile tokens
            expect T.OpenParen tokens
            let condition = parseExp 0 tokens
            expect T.CloseParen tokens
            expect T.Semicolon tokens
            Ast.DoWhile(body, condition, "")
        | T.KWFor ->
            expect T.KWFor tokens
            expect T.OpenParen tokens
            let init = parseForInit tokens
            let condition = parseOptionalExp T.Semicolon tokens
            let post = parseOptionalExp T.CloseParen tokens
            let body = parseStatement tokens
            Ast.For(init, condition, post, body, "")
        | _ ->
            let optExp = parseOptionalExp T.Semicolon tokens
            match optExp with
            | Some e -> Ast.Expression e
            | None -> Ast.Null

    and parseBlockItem (tokens: TokStream.t) =
        if isSpecifier (TokStream.peek tokens) then
            Ast.D(parseDeclaration tokens)
        else
            Ast.S(parseStatement tokens)

    and parseBlock (tokens: TokStream.t) =
        expect T.OpenBrace tokens
        let rec parseBlockItemLoop () =
            if TokStream.peek tokens = T.CloseBrace then
                []
            else
                let nextBlockItem = parseBlockItem tokens
                nextBlockItem :: parseBlockItemLoop ()
        let block = parseBlockItemLoop ()
        expect T.CloseBrace tokens
        Ast.Block block

    let parseProgram (tokens: TokStream.t) =
        let rec parseDeclLoop () =
            if TokStream.isEmpty tokens then
                []
            else
                let nextDecl = parseDeclaration tokens
                nextDecl :: parseDeclLoop ()
        let funDecls = parseDeclLoop ()
        Ast.T.Program funDecls

let parse tokens =
    try
        let tokenStream = TokStream.ofList tokens
        Private.parseProgram tokenStream
    with
    | TokStream.End_of_stream ->
        raise (ParseError "Unexpected end of file")