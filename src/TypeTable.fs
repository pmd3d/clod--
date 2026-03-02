module TypeTable

open System.Collections.Generic

(* structure type definitions *)

type MemberDef = { member_type: Types.CType; offset: int }

type StructDef = {
    alignment: int
    size: int
    members: Map<string, MemberDef>
}

let typeTable: Dictionary<string, StructDef> = Dictionary<string, StructDef>(20)

let addStructDefinition tag structDef =
    typeTable.[tag] <- structDef

let mem tag = typeTable.ContainsKey(tag)
let find tag = typeTable.[tag]

let getMembers tag =
    let structDef = find tag
    let compareOffset m1 m2 = compare m1.offset m2.offset
    structDef.members
    |> Map.toList
    |> List.map snd
    |> List.sortWith compareOffset

let getMemberTypes tag = List.map (fun m -> m.member_type) (getMembers tag)