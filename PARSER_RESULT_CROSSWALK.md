# Phase 11 parser declaration crosswalk

## Frozen baseline

This crosswalk freezes the parser migration comparison at F# commit
`2888aa6a3b66b98c6885194a0bd536c75eedccfb` (`Phase 11: Parser → Result`).
The parent (`2888aa6^`) is used only to identify what that commit changed; it is
not a source for grammar behavior.

| Reviewed file | Git object | Role in the comparison |
| --- | --- | --- |
| `src/Parse.fs` | `2888aa6:src/Parse.fs` | Parser structure, token consumption, returned AST, and first error |
| `src/Compile.fs` | `2888aa6:src/Compile.fs` | Translation of the parser `Error` into `CompilerError.ParseError` |
| `tests/TestParser.fs` | `2888aa6:tests/TestParser.fs` | Phase tests and the change from exception assertions to `Result` assertions |

The baseline can be reproduced without a worktree checkout with:

```sh
git show 2888aa6:src/Parse.fs
git show 2888aa6:src/Compile.fs
git show 2888aa6:tests/TestParser.fs
git diff 2888aa6^ 2888aa6 -- src/Parse.fs src/Compile.fs tests/TestParser.fs
```

## Result and stream conventions

F# parser helpers returning `Result<'a * TokStream, string>` map to the Rust
alias `Parsed<T> = Result<(T, TokStream), String>`. On success, both return the
parsed value and the suffix beginning at the first unconsumed token. On error,
both stop at the first failing bind; neither exposes a partially consumed
stream. F# helpers returning `Result<'a, string>` map directly to
`Result<T, String>`.

The table uses these compact outcome labels:

- **value + suffix**: `Parsed<T>` with the same AST/helper value and unconsumed
  stream;
- **value**: a result without a token stream;
- **whole program**: a program result after consuming declarations to EOF;
- **predicate/display**: a pure helper with no stream or error behavior.

## Declaration-by-declaration mapping

| Reviewed F# declaration | Rust analogue | Success and remaining stream | First-error contract / parity note | Status |
| --- | --- | --- | --- | --- |
| `peekOpt : TokStream -> Token option` | `peekOpt(&TokStream) -> Option<&Token>` | Next token, without consumption | EOF is `None`, never an error | Direct |
| `peekEq : Token -> TokStream -> bool` | `peekEq(&Token, &TokStream) -> bool` | Predicate, without consumption | EOF is `false` | Direct |
| `peekIs : (Token -> bool) -> TokStream -> bool` | `peekIs(predicate, &TokStream) -> bool` | Predicate, without consumption | EOF is `false` | Direct |
| `Expected = Tok \| Name` | `Expected::Tok \| Expected::Name` | Diagnostic category | No failure | Direct |
| `ppExpected` | `ppExpected` | Canonical token display or descriptive name | No failure | Direct |
| `formatError` | `formatError` | `Expected … but found …` using canonical token display | No failure | Direct |
| `takeToken` | `takeToken` / `TokStream::takeToken` | Token + suffix | `Unexpected end of file` | Direct |
| `expect` | `expect` | Suffix after one matching token | EOF comes from `takeToken`; mismatch uses `formatError (Tok …)` | Direct |
| `unescape` (including local recursion) | `unescape` | Decoded literal contents | Infallible; unmatched trailing slash is retained | Equivalent loop |
| `isIdent` | Identifier pattern matches in `parsePrimaryExp` and declarator parsing | Predicate/display | No failure; no standalone Rust item | Inlined |
| `isTypeSpecifier` | `isTypeSpecifier` | Predicate/display | No failure | Direct |
| `isSpecifier` | `isSpecifier` | Predicate/display | No failure | Direct |
| `getPrecedence` | `precedence` | Operator precedence option | Non-operator is `None` | Direct |
| `parseStorageClass` | `parseStorageClass` | `StorageClass` value | Expected `a storage class specifier` | Direct |
| `parseType` | `parseType` | `Type` value | `Invalid type specifier` | Direct; Rust counts rather than sorts |
| `parseSignedConstant` | `parseSignedConstant` | Promoted `Int`/`Long` value | Canonical expected-name error; overflow message preserved | Direct |
| `parseUnsignedConstant` | `parseUnsignedConstant` | Promoted `UInt`/`ULong` value | Canonical expected-name error (including baseline's doubled space); overflow message preserved | Direct |
| `parseChar` | `parseChar` | Integer character constant | `multi-character constant tokens not supported` | Direct |
| `parseTypeAndStorageClass` | `parseTypeAndStorageClass` | `(Type, StorageClass option)` value | More than one storage class: `Internal error - not a storage class`; type errors propagate | Direct |
| `parseId` | `parseId` | Identifier string + suffix | Expected `an identifier` | Direct |
| `parseTypeSpecifier` | `parseTypeSpecifier` | Normalized type token + suffix | Structure tag, type-specifier, and EOF errors propagate in bind order | Direct |
| `parseTypeSpecifierList` | `parseTypeSpecifierList` | Nonempty specifier list + suffix | First `parseTypeSpecifier` error | Equivalent loop; multi-specifier declarations and truncated parameter fixtures cover value/error order |
| `parseSpecifier` | Branch in `parseSpecifierList` | One type/storage token + suffix | Expected `a type or storage-class specifier`; EOF is explicit | Inlined |
| `parseSpecifierList` | `parseSpecifierList` | Nonempty specifier list + suffix | First specifier/type/storage error | Equivalent loop; mixed storage/type and duplicate/conflicting specifier fixtures cover value/error order |
| `parseConst` | `parseConst` | `Constant` + suffix | Expected `a constant token`; delegated constant/character errors propagate | Direct, public |
| `parseDim` | `parseDim` | Array length + suffix | Floating dimensions rejected; other constant errors propagate | Representation boundary: F# `int64`, Rust `usize` after checked signed-width conversion |
| `parseString` | `parseString` | Unescaped string + suffix | Expected `a string literal` | Direct |
| `AbstractDeclarator` | `AbstractDeclarator` | Internal pointer/array tree | No direct failure | Direct shape |
| `parseAbstractArrayDeclSuffix` | `parseAbstractArraySuffix` | Abstract declarator + suffix | Dimension or closing-bracket error | Equivalent iterative suffix collector; nested-array `sizeof` fixture covers type nesting and suffix |
| `parseAbstractDeclarator` | `parseAbstractDeclarator` plus `parseDirectAbstractDeclarator` | Abstract declarator + suffix | Delimiter/type-name errors propagate at the same grammar sites | Decomposed Rust equivalent; nested-array and malformed-delimiter fixtures cover value, suffix, and first error |
| `processAbstractDeclarator` | `processAbstract` | Derived `Type` value | Infallible | Direct |
| `parseUnop` | `parseUnop` | Unary operator + suffix | Expected `a unary operator` | Direct |
| `parseBinop` | `parseBinop` | Binary operator + suffix | Expected `a binary operator` | Direct |
| `parseTypeName` | `parseTypeName` | Derived `Type` + suffix | Specifier/declarator errors propagate | Direct |
| `parsePrimaryExp` (including string loop) | `parsePrimaryExp` | Primary expression + suffix | Expected `a primary expression`; EOF is explicit; nested parse errors propagate | Concatenated-string fixture covers combined value and exact unconsumed suffix |
| `parsePostfixExp` (including postfix loop) | `parsePostfixExp` plus `parseArgumentList` | Postfix expression + suffix | First argument/member/delimiter error | Chained subscript/member fixture covers AST order and suffix; malformed arguments cover first error |
| `parseUnaryExp` | `parseUnaryExp` | Unary expression + suffix | Operator/operand errors propagate | Direct |
| `parseCastExp` | `parseCastExp` | Cast or unary expression + suffix | Type-name, delimiter, or operand error propagates | Direct |
| `parseConditionalMiddle` | `parseConditionalMiddle` | Middle expression + suffix | Requires `:` after expression | Direct |
| `parseExp` (including expression loop) | `parseExp` | Expression + suffix | First operand/operator/conditional error; precedence and associativity preserved | Precedence/assignment fixtures cover AST order and a malformed conditional covers first error |
| `parseOptionalExp` | `parseOptionalExp` | Optional expression + suffix after delimiter | Expression or expected-delimiter error | Direct |
| `Declarator` | `Declarator` | Internal identifier/pointer/array/function tree | No direct failure | Direct shape |
| `parseArrayDeclSuffix` | Declarator suffix handling in `parseDirectDeclarator` | Declarator + suffix | Dimension and delimiter errors propagate | Multidimensional declaration fixture covers derived-type order; truncation covers delimiter error |
| `parseDeclarator` | `parseDeclarator`, `parseSimpleDeclarator`, and `parseDirectDeclarator` | Declarator + suffix | Expected `a simple declarator` and nested delimiter errors propagate | Decomposed equivalent; multidimensional, function, and malformed-declarator fixtures cover derived-type order and first error |
| local `paramLoop` | `parseParamList` | Parameter list + suffix | First parameter/comma/closing-paren error | Malformed trailing-parameter fixture locks down first error |
| parameter body in `paramLoop` | `parseParam` | `(Type, Declarator)` + suffix | Specifier/declarator error propagates | Named Rust boundary |
| `processDeclarator` (including local `processParam`) | `processDeclarator` | `(name, Type, parameter names)` value | Unsupported/nested function declarator errors propagate unchanged | Direct recursion |
| `parseInitializer` (including initializer loop) | `parseInitializer` | Initializer tree + suffix | First initializer or delimiter error; optional trailing comma retained | Nested compound fixture covers tree order/trailing comma; truncation covers first error |
| `parseMemberDeclaration` | `parseMemberDeclaration` | Member declaration + suffix | `Found function declarator in struct member list` or delegated error | Direct |
| `parseStructDeclaration` (including member loop) | `parseStructDeclaration` | Structure declaration + suffix | Parses one member before testing `}`, so an empty definition fails like the baseline | Multi-member, empty-body, and truncated-body fixtures cover order and errors |
| `parseFunctionOrVariableDeclaration` | `parseFunctionOrVariableDeclaration` | Declaration + suffix | First specifier/declarator/initializer/body/semicolon error | Direct named boundary |
| `parseDeclaration` | `parseDeclaration` | Declaration + suffix | Three-token structure dispatch; otherwise delegated declaration error | Direct mutual-recursion equivalent |
| `parseForInit` | `parseForInit` | For initializer + suffix | Function declaration gets the baseline-specific error; delegated errors propagate | Direct |
| `parseStatement` | `parseStatement` | Statement + suffix | Branch delimiters and child errors propagate in source order | Direct, public |
| `parseBlockItem` | `parseBlockItem` | Block item + suffix | Declaration/statement error propagates | Direct named boundary |
| `parseBlock` (including block-item loop) | `parseBlock` | Block + suffix | EOF while awaiting `}` is `Unexpected end of file`; first item error propagates | Mixed declaration/statement fixture covers order; truncated block covers EOF error |
| `parseProgram` (including declaration loop) | `parseProgram` | Whole program | First declaration error; stops successfully only at EOF | Multi-declaration fixture covers order; collection truncations cover first error |
| `parse` | `parse` | Whole program after list-to-stream conversion | `parseProgram` error unchanged | Direct, public |

## Compiler and phase-test mapping

| Reviewed F# site | Rust site | Preserved behavior |
| --- | --- | --- |
| `Compile.compileInner` unwraps `Parse.parse` and raises private `ParseError` | `Compile::compileInner` uses `map_err(CompileFailure::Parse)` | A parser error crosses the internal compiler boundary without becoming a panic |
| `Compile.compile` maps private `ParseError` to `CompilerError.ParseError` | `Compile::compile` maps `CompileFailure::Parse` to `CompilerError::ParseError` | Public error classification is preserved |
| `TestParser.unwrap` accepts `Ok` and fails tests on `Error` | Rust tests use `.unwrap()` for successful fixtures | Phase success fixtures consume the new result boundary |
| F# `error` test matches `Error` for `[KWInt]` | Rust `rejects_incomplete_declaration` asserts `is_err()` | Incomplete declarations are returned errors rather than parser exceptions |

## Audit conclusion and next evidence

Every declaration in the reviewed `Parse.fs` now maps to a Rust item, a named
Rust decomposition, or an explicitly identified inlining. No declaration is
unaccounted for. The iterative collection routines now have parity fixtures
derived from the frozen F# implementation. Those fixtures compare AST value
and remaining suffix where the public parser exposes a suffix, and compare the
exact first error for malformed argument, conditional, parameter, array,
initializer, structure, and block collections.
