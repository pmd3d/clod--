# Phase 11 parser-to-`Result` fidelity plan

## Scope and source of truth

This plan reviews F# commit
`2888aa6a3b66b98c6885194a0bd536c75eedccfb` (`Phase 11: Parser → Result`)
against the Rust parser on the `beforecomputationexpressions` line of work.  The
commit, rather than the current shape of `src/Parse.rs`, is the behavioral and
structural source of truth for this phase.

The phase has a deliberately narrow boundary:

- parser failures become `Result<_, String>` values;
- lookahead becomes optional and must not turn end-of-input into an error until
  a token is actually required;
- parser helpers propagate the first error instead of panicking;
- `Compile` translates the parser's final `Err` into `CompilerError::ParseError`;
- AST shapes, grammar decisions, precedence, and diagnostics otherwise remain
  unchanged.

This is **not** a request to redesign the parser into an imperative Rust parser.
Where Rust cannot reproduce an F# computation expression literally, `?` and
small result-returning helpers should preserve the same bind order and error.

## Review of the current Rust rewrite

The Rust parser already returns `Result` in many places, but that alone is not a
faithful port of the reviewed commit.  The following differences must be
resolved (or explicitly proven equivalent) during the rewrite:

1. **Diagnostic rendering is not equivalent.** `Parse.fs` distinguishes an
   expected token from an expected descriptive name and formats actual tokens
   with `Tokens.show`.  Rust's `expected` accepts a string and renders tokens
   with derived `Debug`.  Port `Expected::{Tok, Name}`, `ppExpected`, and
   `formatError`, and add the corresponding canonical token renderer before
   relying on exact diagnostic tests.
2. **The helper decomposition was collapsed.** The reviewed F# code has
   `peekOpt`, `peekEq`, `peekIs`, `takeToken`, `parseStorageClass`,
   `parseSignedConstant`, `parseUnsignedConstant`, `parseChar`, `parseString`,
   `parseUnop`, `parseBinop`, `parseConditionalMiddle`, `parseParam`,
   `parseBlockItem`, `parseFunctionOrVariableDeclaration`, and
   `parseProgram`.  Several are absent or folded into callers in Rust.  Restore
   these boundaries so each F# bind/error site has an auditable Rust analogue.
3. **Recursive grammar routines were broadly replaced with mutable loops.**
   Iteration can be retained later as a separately justified optimization, but
   this fidelity pass should first mirror the recursive construction and token
   consumption of specifier lists, arguments, postfix expressions, array
   suffixes, initializers, members, blocks, and top-level declarations.
4. **There is at least one observable grammar difference.** The F#
   `parseStructDeclaration` enters `parseMemberLoop` after `{` and therefore
   rejects an empty structure body; Rust checks `}` before parsing the first
   member and accepts it.  Match the F# behavior and lock it down with a
   regression test.
5. **Type-width choices obscure parity.** F# array dimensions and declarators
   carry `int64`; Rust currently uses `usize`.  Decide this at the AST/type port
   boundary, not opportunistically in the parser.  For this phase, parsing and
   conversion failures must match the F# path for the same token values.
6. **Compatibility aliases add an API not present in F#.** The snake-case
   wrappers at the bottom of `Parse.rs` should either be removed or documented
   outside the faithful module.  The canonical exported surface for this phase
   is `parse`, `parseConst`, `parseExp`, and `parseStatement`.
7. **The compiler bridge should preserve intent, not implementation accident.**
   The F# commit temporarily raises a private `ParseError` because the compiler
   is still exception-oriented.  Rust's compiler already has a result pipeline,
   so it should use `map_err(CompileFailure::Parse)` and ultimately produce
   `CompilerError::ParseError`; it should not introduce a panic merely to mimic
   the temporary F# exception.
8. **Tests cover outcomes but not phase fidelity.** Existing Rust tests exercise
   successful constants, expressions, statements, one complete program, and
   generic rejection.  They do not verify exact error text, EOF lookahead,
   storage-class errors, invalid types, casts/abstract declarators, compound
   initializer token consumption, struct-member behavior, or compiler error
   classification.

## Ordered implementation plan

### 1. Freeze the comparison baseline ✅

- Record the parent and reviewed versions of `src/Parse.fs`, `src/Compile.fs`,
  and `tests/TestParser.fs` from `2888aa6`.
- Build a function-by-function crosswalk containing the F# signature, Rust
  signature, success value, remaining token stream, and every error string.
- Treat unrelated behavior from later F# commits as out of scope, so this port
  does not accidentally combine migration phases.

**Exit criterion:** every declaration in the reviewed `Parse.fs` is mapped to a
Rust item or to a written, tested equivalence justification.

**Progress:** Complete. `PARSER_RESULT_CROSSWALK.md` freezes the three reviewed
Git objects, records the result/stream conventions, and maps every top-level and
nested parser declaration. Iterative or decomposed implementations are labeled
for differential testing rather than being assumed equivalent.

### 2. Port token lookahead and diagnostics first

- Add `peekOpt`, `peekEq`, and `peekIs`; none may fail on an empty stream.
- Keep `takeToken` as the only primitive that reports
  `"Unexpected end of file"` when consumption is required.
- Port the `Expected` representation and token display behavior.
- Port `expect` using `?`, preserving the order: consume, compare, then return
  either the remaining stream or the formatted error.

**Exit criterion:** table-driven tests cover token/name expectations, a wrong
token, and empty input with exact messages.

### 3. Port pure fallible helpers without grammar changes

- Port storage-class, type-specifier, signed constant, unsigned constant, and
  character parsing as separate `Result` helpers.
- Preserve duplicate-specifier detection, signed/unsigned conflict handling,
  integer range promotion, and error strings byte-for-byte.
- Resolve the array-dimension representation explicitly and test values at the
  signed and unsigned conversion boundaries.

**Exit criterion:** helper-level tests cover every success variant and every
error branch represented in the reviewed commit.

### 4. Port leaf parsers and declarators in F# order

- Rewrite identifier, specifier-list, constant, dimension, and string parsers.
- Then port abstract declarators/type names, unary and binary operator parsers,
  primary/postfix/unary/cast/conditional expressions, and precedence climbing.
- Port concrete declarators, parameter parsing, declarator processing, and
  initializers.  Preserve the F# recursion and the exact point at which each
  token is consumed until parity tests pass.

**Exit criterion:** paired fixtures produce the same AST and same unconsumed
token suffix, or the same first error, in F# and Rust.

### 5. Port declarations, statements, blocks, and programs

- Keep `parseFunctionOrVariableDeclaration` separate from the struct dispatch
  in `parseDeclaration`.
- Restore `parseBlockItem` and `parseProgram` as explicit parity points.
- Match declaration-vs-statement lookahead and EOF behavior exactly.
- Match non-empty structure-body behavior from the reviewed commit rather than
  silently accepting a broader grammar.

**Exit criterion:** differential fixtures cover forward struct declarations,
struct definitions, variables, functions, all statement forms, empty blocks,
truncated input at each delimiter, and trailing top-level input.

### 6. Wire the compiler boundary

- Propagate `Parse::parse(tokens)` with `map_err(CompileFailure::Parse)`.
- Confirm a parser failure returned by `compile` is
  `CompilerError::ParseError`, while lexer errors remain `LexError` and later
  validation failures retain their existing classification.
- Do not use `catch_unwind` or panic as parser control flow.

**Exit criterion:** integration tests distinguish lex, parse, and downstream
compiler errors.

### 7. Prove fidelity before cleanup

- Port all tests from the reviewed `TestParser.fs`, including the new
  result-unwrapping behavior and the explicit parse-error assertion.
- Add regression tests for every discrepancy listed above.
- Run formatting, unit/integration tests, and Clippy with warnings denied.
- Only after differential tests pass, consider loop-based refactors or public
  naming aliases in a separate commit whose behavior is demonstrably neutral.

**Exit criterion:** the test matrix passes, no parser path panics for malformed
  token input, and the crosswalk has no unresolved rows.

## Acceptance matrix

| Area | Required parity evidence |
| --- | --- |
| Lookahead | EOF is benign for `peek*`, erroneous only when consumption is required |
| Errors | Exact message and first failing token match the F# commit |
| Success | AST and remaining token stream match |
| Constants | Promotion limits and overflows match for all four integer token kinds |
| Expressions | Precedence, right-associative assignment/conditional, casts, and postfix chains match |
| Declarators | Pointer/array/function nesting and unsupported-function errors match |
| Initializers | Nested lists and optional trailing comma consume identical tokens |
| Declarations | Storage class, struct dispatch, members, prototypes, and definitions match |
| Statements | All branches, optional expressions, blocks, and `for` initializers match |
| Compiler | Parser `Err` becomes `CompilerError::ParseError` without panic control flow |

The phase is complete only when this behavioral evidence exists.  Merely
having Rust functions return `Result` is necessary, but is not sufficient to
claim a faithful conversion.

## Completion log

### 2026-10-01

The implementation completed the Phase 11 `Result` boundary and fidelity pass:

- `peekOpt`, `peekEq`, and `peekIs` provide non-failing lookahead, while
  `takeToken` remains the primitive that reports end-of-file during required
  consumption.
- `Expected::{Tok, Name}`, `Tokens::show`, and exact-message parser tests cover
  canonical expected-token and expected-name diagnostics.
- Signed and unsigned constants, storage classes, character constants, type
  parsing, and the signed-width array-dimension boundary return errors rather
  than panic.
- The F# helper boundaries for strings, unary and binary operators,
  conditional middles, parameters, function-or-variable declarations, and
  block items have Rust counterparts.  This makes their token-consumption and
  error sites independently auditable even where surrounding collection code
  is still iterative.
- Empty structure definitions match the reviewed Phase 11 behavior and are
  covered by a regression test.
- `Compile` maps parser failures to `CompilerError::ParseError` without using a
  panic as parser control flow; lexer and parser classification are covered by
  compiler tests.
- Differential fixtures derived from the frozen F# routines now exercise every
  iterative collection boundary: specifiers, adjacent strings, arguments and
  postfixes, expressions, array suffixes, parameters, nested initializers,
  structure members, blocks, and whole programs. Successful fixtures assert
  AST construction order and, where exposed, the unconsumed suffix; malformed
  fixtures assert the exact first error.
- Statement-delimiter and trailing-top-level fixtures now assert the first
  parser error for truncated `if`, `while`, `do`, `for`, and compound
  statements, and for a non-declaration after a valid file-scope declaration.
- Compiler integration tests now assert the exact `CompilerError` variant and
  message for lexer failures, parser failures, name-resolution failures, and
  typechecking failures.

The declaration-by-declaration crosswalk against `2888aa6` is now complete in
`PARSER_RESULT_CROSSWALK.md`. It records success shapes, remaining-stream
contracts, first-error behavior, and every inlined or decomposed Rust analogue.

The Phase 11 parser-to-`Result` migration is **complete**. Boundary
fixtures cover every constant token variant and its promotion/overflow edges,
malformed abstract and concrete declarators, structure-member restrictions,
forward declarations, function prototypes and definitions, every statement
branch, missing statement delimiters, and trailing top-level input. These join
the earlier collection, lookahead, diagnostic, and compiler-classification
fixtures to satisfy the acceptance matrix.

The completed implementation passes the full formatter, unit/integration test,
and warnings-denied Clippy matrix. Future parser changes can therefore be made
as behavior-preserving refactors against this Phase 11 baseline rather than as
part of the migration itself.
