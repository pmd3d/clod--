module Cfg

type simple_instr =
    | Label of string
    | ConditionalJump of string
    | UnconditionalJump of string
    | Return
    | Other

[<CustomEquality; CustomComparison>]
type node_id =
    | Entry
    | Block of int
    | Exit

    override this.Equals(obj) =
        match obj with
        | :? node_id as other ->
            match (this, other) with
            | Entry, Entry -> true
            | Block a, Block b -> a = b
            | Exit, Exit -> true
            | _ -> false
        | _ -> false

    override this.GetHashCode() =
        match this with
        | Entry -> 0
        | Block n -> hash (1, n)
        | Exit -> 2

    interface System.IComparable with
        member this.CompareTo(obj) =
            let tag =
                function
                | Entry -> 0
                | Block _ -> 1
                | Exit -> 2
            match obj with
            | :? node_id as other ->
                match (this, other) with
                | Entry, Entry -> 0
                | Exit, Exit -> 0
                | Block a, Block b -> compare a b
                | _ -> compare (tag this) (tag other)
            | _ ->
                invalidArg "obj" "Cannot compare values of different types"

type basic_block<'v, 'instr> = {
    id: node_id
    instructions: ('v * 'instr) list
    mutable preds: node_id list
    mutable succs: node_id list
    value: 'v
}

type t<'v, 'instr> = {
    (* store basic blocks in association list, indexed by block # *)
    basic_blocks: (int * basic_block<'v, 'instr>) list
    mutable entry_succs: node_id list
    mutable exit_preds: node_id list
    debug_label: string
}

let get_succs nd_id cfg =
    match nd_id with
    | Entry -> cfg.entry_succs
    | Block n ->
        let nd = List.find (fun (k, _) -> k = n) cfg.basic_blocks |> snd
        nd.succs
    | Exit -> []

let get_block_value blocknum cfg =
    let nd =
        List.find (fun (k, _) -> k = blocknum) cfg.basic_blocks |> snd
    nd.value

let private update_successors f nd_id g =
    match nd_id with
    | Entry -> g.entry_succs <- f g.entry_succs
    | Block n ->
        let blk =
            List.find (fun (k, _) -> k = n) g.basic_blocks |> snd
        blk.succs <- f blk.succs
    | Exit -> failwith "Internal error: malformed CFG"

let private update_predecessors f nd_id g =
    match nd_id with
    | Entry -> failwith "Internal error: malformed CFG"
    | Block n ->
        let blk =
            List.find (fun (k, _) -> k = n) g.basic_blocks |> snd
        blk.preds <- f blk.preds
    | Exit -> g.exit_preds <- f g.exit_preds

let add_edge pred succ g =
    let add_id nd_id id_list =
        if List.contains nd_id id_list then id_list
        else nd_id :: id_list
    update_successors (add_id succ) pred g
    update_predecessors (add_id pred) succ g

let remove_edge pred succ g =
    let remove_id nd_id id_list =
        List.filter (fun i -> i <> nd_id) id_list
    update_successors (remove_id succ) pred g
    update_predecessors (remove_id pred) succ g

(* replace block with given block ID *)
let update_basic_block block_idx new_block g =
    let new_blocks =
        List.map
            (fun ((i, _) as blk) ->
                if i = block_idx then (i, new_block) else blk)
            g.basic_blocks
    { g with basic_blocks = new_blocks }

(* constructing the CFG *)
let private partition_into_basic_blocks simplify instructions =
    let f (finished_blocks, current_block) i =
        match simplify i with
        | Label _ ->
            let finished_blocks' =
                if current_block = [] then finished_blocks
                else List.rev current_block :: finished_blocks
            (finished_blocks', [ i ])
        | ConditionalJump _ | UnconditionalJump _ | Return ->
            let finished = List.rev (i :: current_block)
            (finished :: finished_blocks, [])
        | Other -> (finished_blocks, i :: current_block)
    let finished, last = List.fold f ([], []) instructions
    let all_blocks =
        if last = [] then finished else List.rev last :: finished
    List.rev all_blocks

let private add_all_edges simplify g =
    (* build map from labels to the IDs of the blocks that they start with *)
    let label_map =
        List.fold
            (fun lbl_map (_, blk) ->
                match simplify (snd (List.head blk.instructions)) with
                | Label lbl -> Map.add lbl blk.id lbl_map
                | _ -> lbl_map)
            Map.empty g.basic_blocks

    (* add outgoing edges from a single basic block *)
    let process_node (id_num, block) =
        let next_block =
            if id_num = fst (ListUtil.last g.basic_blocks) then Exit
            else Block(id_num + 1)
        let _, last_instr = ListUtil.last block.instructions

        match simplify last_instr with
        | Return -> add_edge block.id Exit g
        | UnconditionalJump target ->
            let target_id = Map.find target label_map
            add_edge block.id target_id g
        | ConditionalJump target ->
            let target_id = Map.find target label_map
            add_edge block.id next_block g
            add_edge block.id target_id g
        | _ -> add_edge block.id next_block g

    add_edge Entry (Block 0) g
    List.iter process_node g.basic_blocks

let instructions_to_cfg simplify debug_label instructions =
    let to_node idx instructions =
        let ann x = ((), x)
        (idx,
         { id = Block idx
           instructions = List.map ann instructions
           preds = []
           succs = []
           value = () })
    let cfg =
        { basic_blocks =
              List.mapi to_node
                  (partition_into_basic_blocks simplify instructions)
          entry_succs = []
          exit_preds = []
          debug_label = debug_label }

    add_all_edges simplify cfg
    cfg

(* converting back to instructions *)
let cfg_to_instructions g =
    let blk_to_instrs (_, { instructions = instructions }) =
        List.map snd instructions
    List.collect blk_to_instrs g.basic_blocks

(* working with annotations *)
(* NOTE: Cannot use { x with ... } here because F# doesn't allow changing
   the type parameter in a record update expression (unlike OCaml).
   We construct new records explicitly to allow 'v to change. *)
let initialize_annotation cfg dummy_val =
    let initialize_instruction (_, i) = (dummy_val, i)
    let initialize_block (idx, b) =
        (idx,
         { id = b.id
           instructions = List.map initialize_instruction b.instructions
           preds = b.preds
           succs = b.succs
           value = dummy_val })
    { basic_blocks = List.map initialize_block cfg.basic_blocks
      entry_succs = cfg.entry_succs
      exit_preds = cfg.exit_preds
      debug_label = cfg.debug_label }

let strip_annotations cfg = initialize_annotation cfg ()

(* debugging *)
let print_graphviz (pp_instr: System.IO.TextWriter -> 'instr -> unit)
                   (pp_val: System.IO.TextWriter -> 'v -> unit)
                   (cfg: t<'v, 'instr>) =
    let filename =
        UniqueIds.makeLabel cfg.debug_label + ".dot"
    let path =
        if System.IO.Path.IsPathRooted(filename) then filename
        else System.IO.Path.Combine(System.IO.Directory.GetCurrentDirectory(), filename)
    use writer = new System.IO.StreamWriter(path)
    let pp_node_id (out: System.IO.TextWriter) = function
        | Exit -> out.Write("exit")
        | Entry -> out.Write("entry")
        | Block n -> out.Write(sprintf "block%d" n)
    let pp_annotated_instruction (out: System.IO.TextWriter) (v, i) =
        out.Write("<tr>")
        out.Write("<td align=\"left\">")
        pp_instr out i
        out.Write("</td>")
        out.Write("<td align=\"left\">")
        pp_val out v
        out.Write("</td>")
        out.Write("</tr>")
        out.WriteLine()
    let pp_block_instructions (out: System.IO.TextWriter) blk =
        out.Write("<table>")
        out.Write("<tr><td colspan=\"2\"><b>")
        pp_node_id out blk.id
        out.Write("</b></td></tr>")
        out.WriteLine()
        List.iter (pp_annotated_instruction out) blk.instructions
        out.Write("<tr><td colspan=\"2\">")
        pp_val out blk.value
        out.Write("</td></tr>")
        out.Write("</table>")
    let pp_block (out: System.IO.TextWriter) (lbl, b) =
        out.Write(sprintf "block%d[label=<" lbl)
        pp_block_instructions out b
        out.Write(">]")
    let pp_entry_edge (out: System.IO.TextWriter) lbl =
        out.Write("entry -> ")
        pp_node_id out lbl
    let pp_edge i (out: System.IO.TextWriter) succ =
        out.Write(sprintf "block%d -> " i)
        pp_node_id out succ
    let pp_edges (out: System.IO.TextWriter) ((lbl: int), (blk: basic_block<'a, _>)) =
        List.iter (pp_edge lbl out) blk.succs
    writer.WriteLine("digraph {")
    writer.WriteLine("  labeljust=l")
    writer.WriteLine("  node[shape=\"box\"]")
    writer.WriteLine("  entry[label=\"ENTRY\"]")
    writer.WriteLine("  exit[label=\"EXIT\"]")
    List.iter (fun b -> pp_block writer b; writer.WriteLine()) cfg.basic_blocks
    List.iter (fun e -> pp_entry_edge writer e; writer.WriteLine()) cfg.entry_succs
    List.iter (fun b -> pp_edges writer b; writer.WriteLine()) cfg.basic_blocks
    writer.WriteLine("}")
    writer.Flush()
    writer.Close()
    let cmd =
        sprintf "dot -Tpng %s -o %s" filename
            (System.IO.Path.ChangeExtension(filename, ".png"))
    let proc = System.Diagnostics.Process.Start("bash", "-c \"" + cmd + "\"")
    let finished = proc.WaitForExit(30000)
    if not finished then failwith ("graphviz fail: " + cmd)


module TackyCfg =
    let simplify = function
        | Tacky.Label l -> Label l
        | Tacky.Jump target -> UnconditionalJump target
        | Tacky.JumpIfZero(_, target) -> ConditionalJump target
        | Tacky.JumpIfNotZero(_, target) -> ConditionalJump target
        | Tacky.Return _ -> Return
        | _ -> Other

    let instructions_to_cfg debug_label instructions =
        instructions_to_cfg simplify debug_label instructions

    let cfg_to_instructions g = cfg_to_instructions g
    let get_succs nd_id cfg = get_succs nd_id cfg
    let get_block_value blocknum cfg = get_block_value blocknum cfg
    let add_edge pred succ g = add_edge pred succ g
    let remove_edge pred succ g = remove_edge pred succ g
    let update_basic_block idx blk g = update_basic_block idx blk g
    let initialize_annotation cfg v = initialize_annotation cfg v
    let strip_annotations cfg = strip_annotations cfg

module AsmCfg =
    let simplify = function
        | Assembly.Label l -> Label l
        | Assembly.Jmp target -> UnconditionalJump target
        | Assembly.JmpCC(_, target) -> ConditionalJump target
        | Assembly.Ret -> Return
        | _ -> Other

    let instructions_to_cfg debug_label instructions =
        instructions_to_cfg simplify debug_label instructions

    let cfg_to_instructions g = cfg_to_instructions g
    let get_succs nd_id cfg = get_succs nd_id cfg
    let get_block_value blocknum cfg = get_block_value blocknum cfg
    let add_edge pred succ g = add_edge pred succ g
    let remove_edge pred succ g = remove_edge pred succ g
    let update_basic_block idx blk g = update_basic_block idx blk g
    let initialize_annotation cfg v = initialize_annotation cfg v
    let strip_annotations cfg = strip_annotations cfg