module ReplacePseudos

open Assembly

(* Structure to keep track of what stack slots we've assigned so far *)
type ReplacementState = {
    current_offset : int (* last used stack slot *)
    offset_map : Map<string, int> (* map from pseudoregister to stack slots *)
}

let calculate_offset state name =
    let size = AssemblySymbols.get_size name
    let alignment = AssemblySymbols.get_alignment name
    let new_offset =
        Rounding.roundAwayFromZero alignment (state.current_offset - size)
    
    let new_state = {
        current_offset = new_offset
        offset_map = Map.add name new_offset state.offset_map
    }
    (new_state, new_offset)

let replace_operand state = function
    (* if it's a pseudoregister, replace it with a stack slot *)
    | Pseudo s ->
        if AssemblySymbols.is_static s then (state, Data (s, 0))
        else
            match Map.tryFind s state.offset_map with
            (* We've already assigned this operand a stack slot *)
            | Some offset -> (state, Memory (BP, offset))
            (* We haven't already assigned it a stack slot;
             * assign it and update state *)
            | None ->
                let new_state, new_offset = calculate_offset state s
                (new_state, Memory (BP, new_offset))
    | PseudoMem (s, offset) when AssemblySymbols.is_static s ->
        (state, Data (s, offset))
    | PseudoMem (s, offset) ->
        match Map.tryFind s state.offset_map with
        (* We've already assigned this operand a stack slot *)
        | Some var_offset -> (state, Memory (BP, offset + var_offset))
        | None ->
            (* assign s a stack slot, and add its offset to the offset w/in s to
               get new operand *)
            let new_state, new_var_offset = calculate_offset state s
            (new_state, Memory (BP, offset + new_var_offset))
    (* not a pseudo, so nothing to do *)
    | other -> (state, other)

let replace_pseudos_in_instruction state = function
    (* Replace src and dst of mov instruction *)
    | Mov (t, src, dst) ->
        let state1, new_src = replace_operand state src
        let state2, new_dst = replace_operand state1 dst
        let new_mov = Mov (t, new_src, new_dst)
        (state2, new_mov)
    | Movsx fields ->
        let state1, new_src = replace_operand state fields.src
        let state2, new_dst = replace_operand state1 fields.dst
        let new_movsx = Movsx { fields with src = new_src; dst = new_dst }
        (state2, new_movsx)
    | MovZeroExtend fields ->
        let state1, new_src = replace_operand state fields.src
        let state2, new_dst = replace_operand state1 fields.dst
        let new_movzx =
            MovZeroExtend { fields with src = new_src; dst = new_dst }
        (state2, new_movzx)
    | Lea (src, dst) ->
        let state1, new_src = replace_operand state src
        let state2, new_dst = replace_operand state1 dst
        let new_lea = Lea (new_src, new_dst)
        (state2, new_lea)
        (* Replace dst of unary instruction *)
    | Unary (t, op, dst) ->
        let state1, new_dst = replace_operand state dst
        let new_unary = Unary (t, op, new_dst)
        (state1, new_unary)
    | Binary { op = op; t = t; src = src; dst = dst } ->
        let state1, new_src = replace_operand state src
        let state2, new_dst = replace_operand state1 dst
        let new_binary = Binary { op = op; t = t; src = new_src; dst = new_dst }
        (state2, new_binary)
    | Cmp (t, op1, op2) ->
        let state1, new_op1 = replace_operand state op1
        let state2, new_op2 = replace_operand state1 op2
        let new_cmp = Cmp (t, new_op1, new_op2)
        (state2, new_cmp)
    | Idiv (t, op) ->
        let state1, new_op = replace_operand state op
        (state1, Idiv (t, new_op))
    | Div (t, op) ->
        let state1, new_op = replace_operand state op
        (state1, Div (t, new_op))
    (* Ret instruction has no operands, doesn't need to be rewritten *)
    | SetCC (code, op) ->
        let state1, new_op = replace_operand state op
        (state1, SetCC (code, new_op))
    | Push op ->
        let state1, new_op = replace_operand state op
        (state1, Push new_op)
    | Cvttsd2si (t, src, dst) ->
        let state1, new_src = replace_operand state src
        let state2, new_dst = replace_operand state1 dst
        let new_cvt = Cvttsd2si (t, new_src, new_dst)
        (state2, new_cvt)
    | Cvtsi2sd (t, src, dst) ->
        let state1, new_src = replace_operand state src
        let state2, new_dst = replace_operand state1 dst
        let new_cvt = Cvtsi2sd (t, new_src, new_dst)
        (state2, new_cvt)
    | (Ret | Cdq _ | Label _ | JmpCC _ | Jmp _ | Call _) as other -> (state, other)
    | Pop _ -> failwith "Internal error"

let replace_pseudos_in_tl = function
    | Function { name = name; global = ``global``; instructions = instructions } ->
        (* should we stick returns_on_stack in the AST or symbol table? *)
        let starting_offset =
            if AssemblySymbols.returns_on_stack name then -8 else 0
        
        let init_state =
            { current_offset = starting_offset; offset_map = Map.empty }
        
        (* rewrite each instruction, tracking current offset/stack slot map as
           we go *)
        (* F# mapFold expects (state -> item -> item * state), whereas OCaml fold_left_map expects (state -> item -> state * item).
           We must adapt the function and swap the result tuple. *)
        let map_folder s i =
            let s', i' = replace_pseudos_in_instruction s i
            (i', s')

        let fixed_instructions, final_state =
            List.mapFold map_folder init_state instructions
        
        AssemblySymbols.set_bytes_required name final_state.current_offset
        Function { name = name; global = ``global``; instructions = fixed_instructions }
    | static_var -> static_var

let replace_pseudos (Program tls) =
    let fixed_defs = List.map replace_pseudos_in_tl tls
    Program fixed_defs
