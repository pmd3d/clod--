module Symbols

type initial_value =
    | Tentative
    | Initial of Initializers.static_init list
    | NoInitializer

type fun_attr = { defined: bool; ``global``: bool }
type static_attr = { init: initial_value; ``global``: bool }

type identifier_attrs =
    | FunAttr of fun_attr
    | StaticAttr of static_attr
    | ConstAttr of Initializers.static_init
    | LocalAttr

type entry = { t: Types.t; attrs: identifier_attrs }

let symbol_table = System.Collections.Generic.Dictionary<string, entry>()

// always use replace instead of add; we want to remove old binding when we add a new one

let add_automatic_var name (t: Types.t) =
    symbol_table.[name] <- { t = t; attrs = LocalAttr }

let add_static_var name (t: Types.t) (``global``: bool) (init: initial_value) =
    symbol_table.[name] <- { t = t; attrs = StaticAttr { init = init; ``global`` = ``global`` } }

let add_fun name (t: Types.t) (``global``: bool) (defined: bool) =
    symbol_table.[name] <- { t = t; attrs = FunAttr { ``global`` = ``global``; defined = defined } }

let get name = symbol_table.[name]

let get_opt name =
    match symbol_table.TryGetValue(name) with
    | true, v -> Some v
    | false, _ -> None

let add_string (s: string) =
    let str_id = UniqueIds.makeNamedTemporary "string"
    let t = Types.Array (Types.Char, int64 (String.length s + 1))
    symbol_table.[str_id] <-
        { t = t; attrs = ConstAttr (Initializers.StringInit (s, true)) }
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