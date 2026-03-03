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

let mutable private _symbolTable: Map<string, SymbolEntry> = Map.empty

// always use replace instead of add; we want to remove old binding when we add a new one

let addAutomaticVar name (t: Types.CType) =
    _symbolTable <- Map.add name { symType = t; attrs = LocalAttr } _symbolTable

let addStaticVar name (t: Types.CType) (``global``: bool) (init: InitialValue) =
    _symbolTable <- Map.add name { symType = t; attrs = StaticAttr { init = init; ``global`` = ``global`` } } _symbolTable

let addFun name (t: Types.CType) (``global``: bool) (defined: bool) =
    _symbolTable <- Map.add name { symType = t; attrs = FunAttr { ``global`` = ``global``; defined = defined } } _symbolTable

let get name = Map.find name _symbolTable

let getOpt name = Map.tryFind name _symbolTable

let addStringWithCounter (counter: UniqueIds.Counter) (s: string) =
    let counter', str_id = UniqueIds.makeNamedTemporary "string" counter
    let t = Types.Array (Types.Char, int64 (String.length s + 1))
    _symbolTable <- Map.add str_id { symType = t; attrs = ConstAttr (Initializers.StringInit (s, true)) } _symbolTable
    (counter', str_id)

let addString (s: string) =
    let str_id = UniqueIds.makeNamedTemporaryShared "string"
    let t = Types.Array (Types.Char, int64 (String.length s + 1))
    _symbolTable <- Map.add str_id { symType = t; attrs = ConstAttr (Initializers.StringInit (s, true)) } _symbolTable
    str_id

let isGlobal name =
    match (get name).attrs with
    | LocalAttr | ConstAttr _ -> false
    | StaticAttr { ``global`` = g } -> g
    | FunAttr { ``global`` = g } -> g

let bindings () = Map.toList _symbolTable

let iter f =
    _symbolTable |> Map.iter f

// Snapshot/restore for pipeline threading
let getTable () = _symbolTable
let setTable m = _symbolTable <- m