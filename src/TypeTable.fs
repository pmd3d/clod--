module TypeTable

(* structure type definitions *)

type MemberDef = { member_type: Types.CType; offset: int }

type StructDef = {
    alignment: int
    size: int
    members: Map<string, MemberDef>
}

let mutable private _typeTable: Map<string, StructDef> = Map.empty

let addStructDefinition tag structDef =
    _typeTable <- Map.add tag structDef _typeTable

let mem tag = Map.containsKey tag _typeTable
let find tag = Map.find tag _typeTable

let getMembers tag =
    let structDef = find tag
    let compareOffset m1 m2 = compare m1.offset m2.offset
    structDef.members
    |> Map.toList
    |> List.map snd
    |> List.sortWith compareOffset

let getMemberTypes tag = List.map (fun m -> m.member_type) (getMembers tag)

// Snapshot/restore for pipeline threading
let getTable () = _typeTable
let setTable m = _typeTable <- m