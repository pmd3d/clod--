module TestLexer

open System
open Xunit
open Lexer

[<Fact>]
let ``leading whitespace`` () =
    Assert.True(Lexer.lex "   return" = [ Tokens.KWReturn ])

[<Fact>]
let ``trailing whitespace``() =
  Assert.True(Lexer.lex "0;\t\n" = [ Tokens.ConstInt 0I; Tokens.Semicolon ])

[<Fact>]
let ``a full program``() =
  Assert.True(Lexer.lex "int main(void){return 0;}" = [
      Tokens.KWInt;
      Tokens.Identifier "main";
      Tokens.OpenParen;
      Tokens.KWVoid;
      Tokens.CloseParen;
      Tokens.OpenBrace;
      Tokens.KWReturn;
      Tokens.ConstInt 0I;
      Tokens.Semicolon;
      Tokens.CloseBrace;
    ])

[<Fact>]
let ``two hyphens``() = 
    Assert.True(Lexer.lex "- -" = [ Tokens.Hyphen; Tokens.Hyphen ])

[<Fact>]
let ``double hyphen``() = 
    Assert.True(Lexer.lex "a--" = [ Tokens.Identifier "a"; Tokens.DoubleHyphen ])

[<Fact>]
let ``two tildes``() = 
    Assert.True(Lexer.lex "~~" = [ Tokens.Tilde; Tokens.Tilde ])

