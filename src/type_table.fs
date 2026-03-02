module TypeTable

open System.Collections.Generic

(* structure type definitions *)

type MemberDef = { member_type: Types.CType; offset: int }

type StructDef = {
    alignment: int
    size: int
    members: Map<string, MemberDef>
}

let type_table: Dictionary<string, StructDef> = Dictionary<string, StructDef>(20)

let add_struct_definition tag struct_def =
    type_table.[tag] <- struct_def

let mem tag = type_table.ContainsKey(tag)
let find tag = type_table.[tag]

let get_members tag =
    let struct_def = find tag
    let compare_offset m1 m2 = compare m1.offset m2.offset
    struct_def.members
    |> Map.toList
    |> List.map snd
    |> List.sortWith compare_offset

let get_member_types tag = List.map (fun m -> m.member_type) (get_members tag)