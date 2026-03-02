module Symbols

type InitialValue =
    | Tentative
    | Initial of Initializers.static_init list
    | NoInitializer

type FunAttr = { defined: bool; ``global``: bool }
type StaticAttr = { init: InitialValue; ``global``: bool }

type IdentifierAttrs =
    | FunAttr of FunAttr
    | StaticAttr of StaticAttr
    | ConstAttr of Initializers.static_init
    | LocalAttr

type SymbolEntry = { symType: Types.t; attrs: IdentifierAttrs }

let symbol_table = System.Collections.Generic.Dictionary<string, SymbolEntry>()

// always use replace instead of add; we want to remove old binding when we add a new one

let add_automatic_var name (t: Types.t) =
    symbol_table.[name] <- { symType = t; attrs = LocalAttr }

let add_static_var name (t: Types.t) (``global``: bool) (init: InitialValue) =
    symbol_table.[name] <- { symType = t; attrs = StaticAttr { init = init; ``global`` = ``global`` } }

let add_fun name (t: Types.t) (``global``: bool) (defined: bool) =
    symbol_table.[name] <- { symType = t; attrs = FunAttr { ``global`` = ``global``; defined = defined } }

let get name = symbol_table.[name]

let get_opt name =
    match symbol_table.TryGetValue(name) with
    | true, v -> Some v
    | false, _ -> None

let add_string (s: string) =
    let str_id = UniqueIds.makeNamedTemporary "string"
    let t = Types.Array (Types.Char, int64 (String.length s + 1))
    symbol_table.[str_id] <-
        { symType = t; attrs = ConstAttr (Initializers.StringInit (s, true)) }
    str_id

let is_global name =
    match (get name).attrs with
    | LocalAttr | ConstAttr _ -> false
    | StaticAttr { ``global`` = g } -> g
    | FunAttr { ``global`` = g } -> g

let bindings () =
    symbol_table |> Seq.map (fun kv -> (kv.Key, kv.Value)) |> Seq.toList

let iter f =
    symbol_table |> Seq.iter (fun kv -> f kv.Key kv.Value)