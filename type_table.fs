module TypeTable

open System.Collections.Generic

(* structure type definitions *)

type member_entry = { member_type: Types.t; offset: int }

type struct_entry = {
    alignment: int
    size: int
    members: Map<string, member_entry>
}

let type_table: Dictionary<string, struct_entry> = Dictionary<string, struct_entry>(20)

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