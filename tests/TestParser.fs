module TestParser

open System
open Xunit
open Parse
open Ast
open TokStream

let printConst c = sprintf "%A" c

[<Fact>]
let ``signed long constant`` () =
    let resultString =
        [ Tokens.ConstLong 4611686018427387904I ]
        |> TokStream.ofList
        |> Parse.parseConst
        |> fst
        |> printConst

    Assert.Equal("ConstLong 4611686018427387904L", resultString)

[<Fact>]
let ``unsigned int constant`` () =
    let resultString =
        [ Tokens.ConstUInt 4294967291I ]
        |> TokStream.ofList
        |> Parse.parseConst
        |> fst
        |> printConst

    Assert.Equal("ConstUInt 4294967291u", resultString)

[<Fact>]
let ``unsigned long constant`` () =
    let resultString =
        [ Tokens.ConstULong 18446744073709551611I ]
        |> TokStream.ofList
        |> Parse.parseConst
        |> fst
        |> printConst

    Assert.Equal("ConstULong 18446744073709551611UL", resultString)

[<Fact>]
let ``expression`` () =
    let result =
        [ Tokens.ConstInt 100; Tokens.Semicolon ]
        |> TokStream.ofList
        |> Parse.parseExp 40
        |> fst

    Assert.Equal(Ast.UntypedExp.Constant (Const.ConstInt 100), result)

[<Fact>]
let ``statement`` () =
    let result =
        [ Tokens.KWReturn; Tokens.ConstInt 4; Tokens.Semicolon ]
        |> TokStream.ofList
        |> Parse.parseStatement
        |> fst
    Assert.Equal(Ast.Untyped.Return (Some (Ast.UntypedExp.Constant (Const.ConstInt 4))), result)

[<Fact>]
let ``error`` () =
    Assert.Throws<Parse.ParseError>(fun () ->
        [ Tokens.KWInt ] |> Parse.parse |> ignore
    )
