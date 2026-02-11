module Regalloc

open Assembly
open Utils

// Bind AsmCfg
module AsmCfg = Cfg.AsmCfg

// Operand module mostly for type definition compat
module Operand =
    type t = operand
    let compare = compare_operand

// F# Sets and Maps are generic, but we define aliases to match OCaml naming
type OperandSet = Set<Operand.t>
type StringSet = Set<string>
type StringMap<'v> = Map<string, 'v>
type IntMap<'v> = Map<int, 'v>

// Assuming Disjoint_sets is available externally, similar to OCaml
module Disjoint = Disjoint_sets.Make(Operand)

let debug_print fmt =
    Printf.ksprintf
        (fun msg -> if !Settings.debug then Printf.printf "%s" msg)
        fmt

// extract all operands from an instruction.
let get_operands = function
    | Mov (_, src, dst) -> [ src; dst ]
    | Movsx i -> [ i.src; i.dst ]
    | MovZeroExtend zx -> [ zx.src; zx.dst ]
    | Lea (src, dst) -> [ src; dst ]
    | Cvttsd2si (_, src, dst) -> [ src; dst ]
    | Cvtsi2sd (_, src, dst) -> [ src; dst ]
    | Unary (_, _, op) -> [ op ]
    | Binary b -> [ b.src; b.dst ]
    | Cmp (_, v1, v2) -> [ v1; v2 ]
    | Idiv (_, op) -> [ op ]
    | Div (_, op) -> [ op ]
    | SetCC (_, op) -> [ op ]
    | Push op -> [ op ]
    | Label _ | Call _ | Ret | Cdq _ | JmpCC _ | Jmp _ -> []
    | Pop _ -> failwith "Internal error"

// map function f over all the operands in an instruction
let replace_ops f i =
    match i with
    | Mov (t, src, dst) -> Mov (t, f src, f dst)
    | Movsx sx -> Movsx { sx with dst = f sx.dst; src = f sx.src }
    | MovZeroExtend zx -> MovZeroExtend { zx with dst = f zx.dst; src = f zx.src }
    | Lea (src, dst) -> Lea (f src, f dst)
    | Cvttsd2si (t, src, dst) -> Cvttsd2si (t, f src, f dst)
    | Cvtsi2sd (t, src, dst) -> Cvtsi2sd (t, f src, f dst)
    | Unary (operator, t, operand) -> Unary (operator, t, f operand)
    | Binary b -> Binary { b with dst = f b.dst; src = f b.src }
    | Cmp (code, v1, v2) -> Cmp (code, f v1, f v2)
    | Idiv (t, v) -> Idiv (t, f v)
    | Div (t, v) -> Div (t, f v)
    | SetCC (code, dst) -> SetCC (code, f dst)
    | Push v -> Push (f v)
    | Label _ | Call _ | Ret | Cdq _ | Jmp _ | JmpCC _ -> i
    | Pop _ -> failwith "Shouldn't use this yet"

let cleanup_movs instructions =
    let is_redundant_mov = function
        | Mov (_, src, dst) when src = dst -> true
        | _ -> false
    in
    List.filter (fun i -> not (is_redundant_mov i)) instructions

// Configuration type to replace the OCaml functor argument module
type RegTypeOps = {
    suffix : string
    all_hardregs : Assembly.reg list
    caller_saved_regs : Assembly.reg list
    pseudo_is_current_type : string -> bool
}

// Helper to mimic OCaml's List.fold_left_map
// OCaml: ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
// F# mapFold: ('State -> 'T -> 'Result * 'State) -> ...
let fold_left_map f acc xs =
    let f_swapped state item =
        let (newState, result) = f state item
        (result, newState)
    let (results, finalState) = List.mapFold f_swapped acc xs
    (finalState, results)

// The Allocator Functor converted to a Class
type Allocator(R : RegTypeOps) =
    
    // convenience function : convert set of regs to set of operands
    let regs_to_operands regs = List.map (fun r -> Reg r) regs

    // values derived from R
    let all_hardregs = R.all_hardregs |> regs_to_operands |> Set.ofList

    let caller_saved_regs =
        R.caller_saved_regs |> regs_to_operands |> Set.ofList

    let regs_used_and_written i =
        let ops_used, ops_written =
            match i with
            | Mov (_, src, dst) -> ([ src ], [ dst ])
            | MovZeroExtend zx -> ([ zx.src ], [ zx.dst ])
            | Movsx sx -> ([ sx.src ], [ sx.dst ])
            | Cvtsi2sd (_, src, dst) -> ([ src ], [ dst ])
            | Cvttsd2si (_, src, dst) -> ([ src ], [ dst ])
            (* dst of binary or unary instruction is both read and written *)
            | Binary b -> ([ b.src; b.dst ], [ b.dst ])
            | Unary (_, _, op) -> ([ op ], [ op ])
            | Cmp (_, v1, v2) -> ([ v1; v2 ], [])
            | SetCC (_, op) -> ([], [ op ])
            | Push v -> ([ v ], [])
            | Idiv (_, op) -> ([ op; Reg AX; Reg DX ], [ Reg AX; Reg DX ])
            | Div (_, op) -> ([ op; Reg AX; Reg DX ], [ Reg AX; Reg DX ])
            | Cdq _ -> ([ Reg AX ], [ Reg DX ])
            | Call f ->
                (* function call updates caller-saved regs, uses param-passing
                     registers *)
                let used =
                    Assembly_symbols.param_regs_used f
                    |> List.filter (fun r -> List.contains r R.all_hardregs)
                    |> List.map (fun r -> Reg r)
                in
                (used, Set.toList caller_saved_regs)
            (* if src is a pseudo, lea won't actually generate it,
             * but we've excluded it from the graph anyway
             * if it's a memory address or indexed operand, we _do_ want to generate
             * hardregs used in address calculations
             *)
            | Lea (src, dst) -> ([ src ], [ dst ])
            | Jmp _ | JmpCC _ | Label _ | Ret -> ([], [])
            | Pop _ -> failwith "Internal error"
        in
        (* convert list of operands read into list of hard/pseudoregs read *)
        let regs_used_to_read opr =
            match opr with
            | Pseudo _ | Reg _ -> [ opr ]
            | Memory (r, _) -> [ Reg r ]
            | Indexed x -> [ Reg x.base; Reg x.index ]
            | Imm _ | Data _ | PseudoMem _ -> []
        in
        let regs_read1 = List.collect regs_used_to_read ops_used in
        (* now convert list of operands written into lists of hard/pseudoregs
         * read _or_ written, accounting for the fact that writing to a memory address
         * may require reading a pointer *)
        let regs_used_to_update opr =
            match opr with
            | Pseudo _ | Reg _ -> ([], [ opr ])
            | Memory (r, _) -> ([ Reg r ], [])
            | Indexed x -> ([ Reg x.base; Reg x.index ], [])
            | Imm _ | Data _ | PseudoMem _ -> ([], [])
        in
        let concat_pair (a, b) = (List.concat a, List.concat b) in
        let regs_read2, regs_written =
            List.map regs_used_to_update ops_written |> List.unzip |> concat_pair
        in
        ( Set.ofList (regs_read1 @ regs_read2),
          Set.ofList regs_written )

    // Types defined inside the allocator
    // Note: In F# types must be defined before use in the class or outside. 
    // Since they depend on generic concepts but not strictly on R values for definition, 
    // we define the structure here.
    
    // type node_id = Assembly.operand // Alias

    // type node = {
    //    id : Assembly.operand;
    //    mutable neighbors : OperandSet;
    //    spill_cost : float;
    //    color : int option;
    //    pruned : bool;
    // }

    // type NodeMap<'T> = Map<Operand.t, 'T>
    // type graph = NodeMap<node>

    let show_node_id nd =
        let s =
            match nd with
            | Reg r -> show_reg r
            | Pseudo p -> p
            | _ ->
                failwith "Internal error: malformed interference graph"
        in
        String.map (function '.' -> '_' | c -> c) s

    // Since types need to be concrete for the methods
    // We will use local record definitions or assume they are mapped to the structure below
    
    // Helper function for Liveness
    let meet fn_name cfg block =
        let live_at_exit =
            let all_return_regs =
                AssemblySymbols.return_regs_used fn_name
                |> regs_to_operands
                |> Set.ofList
            in
            let return_regs = Set.intersect all_hardregs all_return_regs in
            return_regs
        in

        let update_live live = function
            | Entry ->
                failwith "Internal error: malformed interference graph"
            | Exit -> Set.union live live_at_exit
            | Block n -> Set.union live (AsmCfg.get_block_value n cfg)
        in
        List.fold update_live Set.empty block.succs

    let transfer block end_live_regs =
        let process_instr current_live_regs (_, i) =
            let annotated_instr = (current_live_regs, i) in
            let new_live_regs =
                let regs_used, regs_written = regs_used_and_written i in
                let without_killed = Set.difference current_live_regs regs_written in
                Set.union without_killed regs_used
            in
            (new_live_regs, annotated_instr)
        in
        let incoming_live_regs, annotated_reversed_instrutions =
            block.instructions
            |> List.rev
            |> fold_left_map process_instr end_live_regs
        in
        {
            block with
            instructions = List.rev annotated_reversed_instrutions;
            value = incoming_live_regs;
        }

    let analyze_liveness fn_name = 
        // Emulating: module Iterative = Backward_dataflow.Dataflow (AsmCfg) (OperandSet)
        // Assuming Backward_dataflow.Dataflow.analyze exists as a static function
        Backward_dataflow.analyze pp_operand (meet fn_name) transfer

    let k = Set.count all_hardregs

    member this.allocate fn_name aliased_pseudos instructions =
        
        // Define node and graph locally to closing over R if needed, 
        // though strictly they could be outside.
        
        let mk_base_graph () =
            let add_node g r =
                Map.add r
                    {
                        AllocTypes.id = r;
                        AllocTypes.neighbors = Set.remove r all_hardregs;
                        AllocTypes.spill_cost = Double.PositiveInfinity;
                        AllocTypes.color = None;
                        AllocTypes.pruned = false;
                    }
                    g
            in
            List.fold add_node Map.empty (Set.toList all_hardregs)

        let get_pseudo_nodes aliased_pseudos instructions =
            let operands_to_pseudos = function
                | Assembly.Pseudo r ->
                    if
                        R.pseudo_is_current_type r
                        && not
                                (AssemblySymbols.is_static r
                                || StringSet.contains r aliased_pseudos)
                    then Some r
                    else None
                | _ -> None
            in
            let get_pseudos i = get_operands i |> List.choose operands_to_pseudos in
            let initialize_node pseudo =
                {
                    AllocTypes.id = Pseudo pseudo;
                    AllocTypes.neighbors = Set.empty;
                    AllocTypes.spill_cost = 0.0;
                    AllocTypes.color = None;
                    AllocTypes.pruned = false;
                }
            in
            List.collect get_pseudos instructions
            |> List.distinct
            |> List.sort
            |> List.map initialize_node

        let add_pseudo_nodes aliased_pseudos graph instructions =
            let nds = get_pseudo_nodes aliased_pseudos instructions in
            let add_node g (nd : AllocTypes.node) = Map.add nd.id nd g in
            List.fold add_node graph nds

        let get_node_by_id graph node_id = Map.find node_id graph

        let add_edge g nd_id1 nd_id2 =
            let nd1 = Map.find nd_id1 g
            let nd2 = Map.find nd_id2 g
            nd1.neighbors <- Set.add nd_id2 nd1.neighbors
            nd2.neighbors <- Set.add nd_id1 nd2.neighbors

        let remove_edge g nd_id1 nd_id2 =
            let nd1, nd2 = (get_node_by_id g nd_id1, get_node_by_id g nd_id2)
            nd1.neighbors <- Set.remove nd_id2 nd1.neighbors
            nd2.neighbors <- Set.remove nd_id1 nd2.neighbors

        let degree graph nd_id =
            let nd = get_node_by_id graph nd_id
            Set.count nd.neighbors

        let are_neighbors g nd_id1 nd_id2 =
            let nd1 = Map.find nd_id1 g
            Set.contains nd_id2 nd1.neighbors

        let add_edges liveness_cfg interference_graph =
            let handle_instr (live_after_instr, i) =
                let _, updated_regs = regs_used_and_written i in

                let handle_livereg l =
                    match i with
                    | Mov (_, src, _) when src = l -> ()
                    | _ ->
                        let handle_update u =
                            if
                                u <> l
                                && Map.containsKey l interference_graph
                                && Map.containsKey u interference_graph
                            then add_edge interference_graph l u
                            else ()
                        in
                        Set.iter handle_update updated_regs
                in
                Set.iter handle_livereg live_after_instr
            in

            let all_instructions =
                let open AsmCfg in
                List.collect
                    (fun (_, blk) -> blk.instructions)
                    liveness_cfg.basic_blocks
            in
            List.iter handle_instr all_instructions

        let build_interference_graph fn_name aliased_pseudos instructions =
            let base_graph = mk_base_graph () in
            let graph = add_pseudo_nodes aliased_pseudos base_graph instructions in
            let cfg = AsmCfg.instructions_to_cfg fn_name instructions in
            let liveness_cfg = analyze_liveness fn_name cfg in
            add_edges liveness_cfg graph;
            graph

        let add_spill_costs graph instructions =
            let incr_count (counts : Map<string, int>) pseudo =
                let updater = function None -> Some 1 | Some i -> Some (i + 1) in
                // F# Map.change is equivalent to OCaml Map.update
                Map.change pseudo updater counts
            in
            let operands = List.collect get_operands instructions in
            let get_pseudo = function Assembly.Pseudo r -> Some r | _ -> None in
            let pseudos = List.choose get_pseudo operands in
            let count_map = List.fold incr_count Map.empty pseudos in
            let set_spill_cost (nd : AllocTypes.node) =
                match nd.id with
                | Pseudo r ->
                    { nd with spill_cost = float (Map.find r count_map) }
                | _ -> nd
            in
            Map.map (fun _ v -> set_spill_cost v) graph

        let george_test graph hardreg pseudo =
            let pseudoreg_neighbors = (get_node_by_id graph pseudo).neighbors in
            let neighbor_is_ok neighbor_id =
                are_neighbors graph neighbor_id hardreg || degree graph neighbor_id < k
            in
            Set.forall neighbor_is_ok pseudoreg_neighbors

        let briggs_test graph x y =
            let x_nd = get_node_by_id graph x in
            let y_nd = get_node_by_id graph y in
            let neighbors = Set.union x_nd.neighbors y_nd.neighbors in
            let has_significant_degree neighbor_id =
                let deg = degree graph neighbor_id in
                let adjusted_deg =
                    if
                        are_neighbors graph x neighbor_id && are_neighbors graph y neighbor_id
                    then deg - 1
                    else deg
                in
                adjusted_deg >= k
            in
            let count_significant neighbor cnt =
                if has_significant_degree neighbor then cnt + 1 else cnt
            in
            let significant_neighbor_count =
                Set.fold count_significant 0 neighbors
            in
            significant_neighbor_count < k

        let conservative_coalescable graph src dst =
            if briggs_test graph src dst then true
            else
                match (src, dst) with
                | Reg _, _ -> george_test graph src dst
                | _, Reg _ -> george_test graph dst src
                | _ -> false

        let update_graph g to_merge to_keep =
            let update_neighbor neighbor_id =
                add_edge g neighbor_id to_keep
                remove_edge g neighbor_id to_merge
            in
            Set.iter update_neighbor (get_node_by_id g to_merge).neighbors
            Map.remove to_merge g

        let coalesce graph instructions =
            let process_instr (g, reg_map) = function
                | Mov (_, src, dst) ->
                    let src' = DisjointSets.find src reg_map
                    let dst' = DisjointSets.find dst reg_map
                    if
                        Map.containsKey src' g
                        && Map.containsKey dst' g
                        && src' <> dst'
                        && (not (are_neighbors g src' dst'))
                        && conservative_coalescable g src' dst'
                    then
                        match src' with
                        | Reg _ ->
                            ( update_graph g dst' src',
                                DisjointSets.union dst' src' reg_map )
                        | _ ->
                            ( update_graph g src' dst',
                                DisjointSets.union src' dst' reg_map )
                    else (g, reg_map)
                | _ -> (g, reg_map)
            in
            let _updated_graph, new_instructions =
                List.fold process_instr (graph, DisjointSets.init) instructions
            in
            new_instructions

        let rewrite_coalesced instructions coalesced_regs =
            let f r = DisjointSets.find r coalesced_regs in
            let rewrite_instruction = function
                | Mov (t, src, dst) ->
                    let new_src = f src
                    let new_dst = f dst
                    if new_src = new_dst then None else Some (Mov (t, new_src, new_dst))
                | i -> Some (replace_ops f i)
            in
            List.choose rewrite_instruction instructions

        let rec color_graph graph =
            let remaining =
                graph
                |> Map.toList
                |> List.map snd
                |> List.filter (fun (nd: AllocTypes.node) -> not nd.pruned)
            in
            match remaining with
            | [] -> graph
            | _ ->
                let not_pruned nd_id = not (Map.find nd_id graph).pruned in
                let degree (nd: AllocTypes.node) =
                    let unpruned_neighbors = Set.filter not_pruned nd.neighbors in
                    Set.count unpruned_neighbors
                in
                let is_low_degree nd = degree nd < k in
                let next_node =
                    try List.find is_low_degree remaining
                    with :? System.Collections.Generic.KeyNotFoundException ->
                        let spill_metric nd = nd.spill_cost / float (degree nd) in
                        let cmp nd1 nd2 =
                            compare (spill_metric nd1) (spill_metric nd2)
                        in
                        let print_spill_info nd =
                            debug_print "Node %s has degree %d, spill cost %f and metric %f\n"
                                (show_node_id nd.id) (degree nd) nd.spill_cost (spill_metric nd)
                        in
                        debug_print "================================\n"
                        List.iter print_spill_info remaining
                        let spilled = ListUtil.min cmp remaining
                        debug_print "Spill candidate: %s\n" (show_node_id spilled.id)
                        spilled
                in
                let pruned_graph =
                    Map.change next_node.id
                        (function
                            | Some nd -> Some { nd with pruned = true }
                            | None -> failwith "what??")
                        graph
                in
                let partly_colored = color_graph pruned_graph in
                let all_colors = List.init k id in
                let remove_neighbor_color neighbor_id remaining_colors =
                    let neighbor_nd = Map.find neighbor_id partly_colored in
                    match neighbor_nd.color with
                    | Some c -> List.filter (fun col -> col <> c) remaining_colors
                    | None -> remaining_colors
                in
                let available_colors =
                    Set.fold (fun acc elem -> remove_neighbor_color elem acc) all_colors next_node.neighbors
                in
                match available_colors with
                | [] -> partly_colored
                | _ :: _ ->
                    let c =
                        match next_node.id with
                        | Reg r when not (List.contains r R.caller_saved_regs) ->
                            ListUtil.max compare available_colors
                        | _ -> ListUtil.min compare available_colors
                    in
                    Map.change next_node.id
                        (function
                            | Some nd -> Some { nd with pruned = false; color = Some c }
                            | None -> failwith "NOPE")
                        partly_colored

        let make_register_map fn_name graph =
            let add_color nd_id (nd: AllocTypes.node) color_map =
                match nd_id with
                | Reg r -> Map.add (Option.get nd.color) r color_map
                | _ -> color_map
            in
            let colors_to_regs = Map.fold add_color Map.empty graph in

            let add_mapping _k (nd: AllocTypes.node) (used_callee_saved, reg_map) =
                match nd with
                | { id = Pseudo p; color = Some c; _ } ->
                    let hardreg = Map.find c colors_to_regs in
                    let used_callee_saved =
                        if List.contains hardreg R.caller_saved_regs then used_callee_saved
                        else Reg_set.add hardreg used_callee_saved
                    in
                    (used_callee_saved, Map.add p hardreg reg_map)
                | _ -> (used_callee_saved, reg_map)
            in
            let callee_saved_regs_used, reg_map =
                Map.fold add_mapping (Reg_set.empty, Map.empty) graph
            in
            Assembly_symbols.add_callee_saved_regs_used fn_name callee_saved_regs_used
            reg_map

        let replace_pseudoregs instructions reg_map =
            let f = function
                | Assembly.Pseudo p as op -> 
                    (try Reg (Map.find p reg_map) with :? System.Collections.Generic.KeyNotFoundException -> op)
                | op -> op
            in
            cleanup_movs (List.map (replace_ops f) instructions)

        let rec coalesce_loop current_instructions =
            let graph = build_interference_graph fn_name aliased_pseudos current_instructions
            let coalesced_regs = coalesce graph current_instructions in
            if DisjointSets.isEmpty coalesced_regs then (graph, current_instructions)
            else
                let new_instructions =
                    rewrite_coalesced current_instructions coalesced_regs
                in
                coalesce_loop new_instructions

        let coalesced_graph, coalesced_instructions = coalesce_loop instructions
        let graph_with_spill_costs =
            add_spill_costs coalesced_graph coalesced_instructions
        in
        let colored_graph = color_graph graph_with_spill_costs in
        let register_map = make_register_map fn_name colored_graph in
        replace_pseudoregs coalesced_instructions register_map

// Auxiliary types for the Allocator to avoid recursive type definitions inside the class
and AllocTypes = 
    struct 
         type node = {
            id : Assembly.operand;
            mutable neighbors : OperandSet;
            spill_cost : float;
            color : int option;
            pruned : bool;
         }
    end

let GP = new Allocator ({
    suffix = "gp"
    all_hardregs = [ AX; BX; CX; DX; DI; SI; R8; R9; R12; R13; R14; R15 ]
    caller_saved_regs = [ AX; CX; DX; DI; SI; R8; R9 ]
    pseudo_is_current_type = fun p -> Assembly_symbols.get_type p <> Double
})

let XMM = new Allocator ({
    suffix = "xmm"
    all_hardregs =
        [
            XMM0; XMM1; XMM2; XMM3; XMM4; XMM5; XMM6;
            XMM7; XMM8; XMM9; XMM10; XMM11; XMM12; XMM13;
        ]
    caller_saved_regs = 
        [
            XMM0; XMM1; XMM2; XMM3; XMM4; XMM5; XMM6;
            XMM7; XMM8; XMM9; XMM10; XMM11; XMM12; XMM13;
        ]
    pseudo_is_current_type = fun p -> Assembly_symbols.get_type p = Double
})

let allocate_registers aliased_pseudos (Program tls) =
    let allocate_regs_for_fun fn_name instructions =
        instructions
        |> GP.allocate fn_name aliased_pseudos
        |> XMM.allocate fn_name aliased_pseudos
    in
    let alloc_in_tl = function
        | Function f ->
            Function
                { f with instructions = allocate_regs_for_fun f.name f.instructions }
        | tl -> tl
    in
    Program (List.map alloc_in_tl tls)