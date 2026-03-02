module Symbols

type InitialValue =
    | Tentative
    | Initial of Initializers.StaticInit list
    | NoInitializer

type FunAttr = { defined: bool; ``global``: bool }
type StaticAttr = { init: InitialValue; ``global``: bool }

type IdentifierAttrs =
    | FunAttr of FunAttr
    | StaticAttr of StaticAttr
    | ConstAttr of Initializers.StaticInit
    | LocalAttr

type SymbolEntry = { symType: Types.CType; attrs: IdentifierAttrs }

let symbolTable = System.Collections.Generic.Dictionary<string, SymbolEntry>()

// always use replace instead of add; we want to remove old binding when we add a new one

let addAutomaticVar name (t: Types.CType) =
    symbolTable.[name] <- { symType = t; attrs = LocalAttr }

let addStaticVar name (t: Types.CType) (``global``: bool) (init: InitialValue) =
    symbolTable.[name] <- { symType = t; attrs = StaticAttr { init = init; ``global`` = ``global`` } }

let addFun name (t: Types.CType) (``global``: bool) (defined: bool) =
    symbolTable.[name] <- { symType = t; attrs = FunAttr { ``global`` = ``global``; defined = defined } }

let get name = symbolTable.[name]

let getOpt name =
    match symbolTable.TryGetValue(name) with
    | true, v -> Some v
    | false, _ -> None

let addString (s: string) =
    let str_id = UniqueIds.makeNamedTemporary "string"
    let t = Types.Array (Types.Char, int64 (String.length s + 1))
    symbolTable.[str_id] <-
        { symType = t; attrs = ConstAttr (Initializers.StringInit (s, true)) }
    str_id

let isGlobal name =
    match (get name).attrs with
    | LocalAttr | ConstAttr _ -> false
    | StaticAttr { ``global`` = g } -> g
    | FunAttr { ``global`` = g } -> g

let bindings () =
    symbolTable |> Seq.map (fun kv -> (kv.Key, kv.Value)) |> Seq.toList

let iter f =
    symbolTable |> Seq.iter (fun kv -> f kv.Key kv.Value)