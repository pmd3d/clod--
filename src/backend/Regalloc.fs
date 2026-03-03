module Regalloc

open Assembly

// Operand module mostly for type definition compat
module Operand =
    type OperandType = AsmOperand
    let compare (a: AsmOperand) (b: AsmOperand) = Operators.compare a b

let showReg (r: AsmReg) = sprintf "%A" r
let ppOperand (out: System.IO.TextWriter) (op: AsmOperand) = out.Write(sprintf "%A" op)

// F# Sets and Maps are generic, but we define aliases to match OCaml naming
type OperandSet = Set<Operand.OperandType>
type StringSet = Set<string>
type StringMap<'v> = Map<string, 'v>
type IntMap<'v> = Map<int, 'v>

// DisjointSets is used directly (F# has no functors)

let debugPrint fmt =
    Printf.kprintf
        (fun msg -> if !Settings.Debug then printf "%s" msg)
        fmt

// extract all operands from an instruction.
let getOperands = function
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
let replaceOps f i =
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

let cleanupMovs instructions =
    let isRedundantMov = function
        | Mov (_, src, dst) when src = dst -> true
        | _ -> false
    in
    List.filter (fun i -> not (isRedundantMov i)) instructions

// Configuration type to replace the OCaml functor argument module
type RegTypeOps = {
    suffix : string
    all_hardregs : Assembly.AsmReg list
    caller_saved_regs : Assembly.AsmReg list
    pseudo_is_current_type : string -> bool
}

// Helper to mimic OCaml's List.foldLeftMap
// OCaml: ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
// F# mapFold: ('State -> 'T -> 'Result * 'State) -> ...
let foldLeftMap f acc xs =
    let fSwapped state item =
        let (newState, result) = f state item
        (result, newState)
    let (results, finalState) = List.mapFold fSwapped acc xs
    (finalState, results)

// Node type for the interference graph
type AllocNode = {
    id : Assembly.AsmOperand
    mutable neighbors : OperandSet
    spillCost : float
    color : int option
    pruned : bool
}

// The Allocator Functor converted to a Class
type Allocator(R : RegTypeOps) =
    
    // convenience function : convert set of regs to set of operands
    let regsToOperands regs = List.map (fun r -> Reg r) regs

    // values derived from R
    let allHardregs = R.all_hardregs |> regsToOperands |> Set.ofList

    let callerSavedRegs =
        R.caller_saved_regs |> regsToOperands |> Set.ofList

    let regsUsedAndWritten i =
        let opsUsed, opsWritten =
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
                    AssemblySymbols.paramRegsUsed f
                    |> List.filter (fun r -> List.contains r R.all_hardregs)
                    |> List.map (fun r -> Reg r)
                in
                (used, Set.toList callerSavedRegs)
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
        let regsUsedToRead opr =
            match opr with
            | Pseudo _ | Reg _ -> [ opr ]
            | Memory (r, _) -> [ Reg r ]
            | Indexed x -> [ Reg x.``base``; Reg x.index ]
            | Imm _ | Data _ | PseudoMem _ -> []
        in
        let regsRead1 = List.collect regsUsedToRead opsUsed in
        (* now convert list of operands written into lists of hard/pseudoregs
         * read _or_ written, accounting for the fact that writing to a memory address
         * may require reading a pointer *)
        let regsUsedToUpdate opr =
            match opr with
            | Pseudo _ | Reg _ -> ([], [ opr ])
            | Memory (r, _) -> ([ Reg r ], [])
            | Indexed x -> ([ Reg x.``base``; Reg x.index ], [])
            | Imm _ | Data _ | PseudoMem _ -> ([], [])
        in
        let concatPair (a, b) = (List.concat a, List.concat b) in
        let regsRead2, regsWritten =
            List.map regsUsedToUpdate opsWritten |> List.unzip |> concatPair
        in
        ( Set.ofList (regsRead1 @ regsRead2),
          Set.ofList regsWritten )

    // Types defined inside the allocator
    // Note: In F# types must be defined before use in the class or outside. 
    // Since they depend on generic concepts but not strictly on R values for definition, 
    // we define the structure here.
    
    // type nodeId = Assembly.AsmOperand // Alias

    // type node = {
    //    id : Assembly.AsmOperand;
    //    mutable neighbors : OperandSet;
    //    spillCost : float;
    //    color : int option;
    //    pruned : bool;
    // }

    // type NodeMap<'T> = Map<Operand.OperandType, 'T>
    // type graph = NodeMap<node>

    let showNodeId nd =
        let s =
            match nd with
            | Reg r -> showReg r
            | Pseudo p -> p
            | _ ->
                failwith "Internal error: malformed interference graph"
        in
        String.map (function '.' -> '_' | c -> c) s

    // Since types need to be concrete for the methods
    // We will use local record definitions or assume they are mapped to the structure below
    
    // Helper function for Liveness
    // OCaml foldLeftMap: ('acc -> 'a -> 'acc * 'b) -> 'acc -> 'a list -> 'acc * 'b list
    let foldLeftMap f acc lst =
        let rec go acc result = function
            | [] -> (acc, List.rev result)
            | x :: xs ->
                let acc', y = f acc x
                go acc' (y :: result) xs
        go acc [] lst

    let meet fn_name (cfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) (block: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>) =
        let liveAtExit =
            let allReturnRegs =
                AssemblySymbols.returnRegsUsed fn_name
                |> regsToOperands
                |> Set.ofList
            in
            let returnRegs = Set.intersect allHardregs allReturnRegs in
            returnRegs
        in

        let updateLive live = function
            | Cfg.Entry ->
                failwith "Internal error: malformed interference graph"
            | Cfg.Exit -> Set.union live liveAtExit
            | Cfg.Block n -> Set.union live (AsmCfg.getBlockValue n cfg)
        in
        List.fold updateLive Set.empty block.succs

    let transfer (block: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>) (endLiveRegs: Set<AsmOperand>) =
        let processInstr currentLiveRegs ((_: Set<AsmOperand>), (i: AsmInstruction)) =
            let annotatedInstr = (currentLiveRegs, i) in
            let newLiveRegs =
                let regsUsed, regsWritten = regsUsedAndWritten i in
                let withoutKilled = Set.difference currentLiveRegs regsWritten in
                Set.union withoutKilled regsUsed
            in
            (newLiveRegs, annotatedInstr)
        in
        let incomingLiveRegs, annotatedReversedInstructions =
            block.instructions
            |> List.rev
            |> foldLeftMap processInstr endLiveRegs
        in
        { block with
            instructions = List.rev annotatedReversedInstructions
            value = incomingLiveRegs }

    let analyzeLiveness fn_name cfg =
        Backward_dataflow.analyze
            ppOperand
            (Set.empty : Set<AsmOperand>)
            (=)
            Set.toList
            (meet fn_name)
            transfer
            Cfg.initializeAnnotation
            Cfg.updateBasicBlock
            (fun (blk: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>) -> blk.value)
            (fun (blk: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>) -> blk.preds)
            (fun (cfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) -> cfg.BasicBlocks)
            (fun (cfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) -> cfg.debugLabel)
            (fun lbl (cfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) -> { cfg with debugLabel = lbl })
            (Cfg.printGraphviz (fun (out: System.IO.TextWriter) (i: AsmInstruction) -> out.Write(sprintf "%A" i)))
            cfg

    let k = Set.count allHardregs

    member this.allocate fn_name aliased_pseudos instructions =
        
        // Define node and graph locally to closing over R if needed, 
        // though strictly they could be outside.
        
        let mkBaseGraph () =
            let addNode g r =
                Map.add r
                    {
                        id = r;
                        neighbors = Set.remove r allHardregs;
                        spillCost = infinity;
                        color = None;
                        pruned = false;
                    }
                    g
            in
            List.fold addNode Map.empty (Set.toList allHardregs)

        let getPseudoNodes aliased_pseudos instructions =
            let operandsToPseudos = function
                | Assembly.Pseudo r ->
                    if
                        R.pseudo_is_current_type r
                        && not
                                (AssemblySymbols.isStatic r
                                || Set.contains r aliased_pseudos)
                    then Some r
                    else None
                | _ -> None
            in
            let getPseudos i = getOperands i |> List.choose operandsToPseudos in
            let initializeNode pseudo =
                {
                    id = Pseudo pseudo;
                    neighbors = Set.empty;
                    spillCost = 0.0;
                    color = None;
                    pruned = false;
                }
            in
            List.collect getPseudos instructions
            |> List.distinct
            |> List.sort
            |> List.map initializeNode

        let addPseudoNodes aliased_pseudos graph instructions =
            let nds = getPseudoNodes aliased_pseudos instructions in
            let addNode g (nd : AllocNode) = Map.add nd.id nd g in
            List.fold addNode graph nds

        let getNodeById graph nodeId = Map.find nodeId graph

        let addEdge g nd_id1 nd_id2 =
            let nd1 = Map.find nd_id1 g
            let nd2 = Map.find nd_id2 g
            nd1.neighbors <- Set.add nd_id2 nd1.neighbors
            nd2.neighbors <- Set.add nd_id1 nd2.neighbors

        let removeEdge g nd_id1 nd_id2 =
            let nd1, nd2 = (getNodeById g nd_id1, getNodeById g nd_id2)
            nd1.neighbors <- Set.remove nd_id2 nd1.neighbors
            nd2.neighbors <- Set.remove nd_id1 nd2.neighbors

        let degree graph nd_id =
            let nd = getNodeById graph nd_id
            Set.count nd.neighbors

        let areNeighbors g nd_id1 nd_id2 =
            let nd1 = Map.find nd_id1 g
            Set.contains nd_id2 nd1.neighbors

        let addEdges (livenessCfg: Cfg.ControlFlowGraph<Set<AsmOperand>, AsmInstruction>) interference_graph =
            let handleInstr (liveAfterInstr, i) =
                let _, updatedRegs = regsUsedAndWritten i in

                let handleLivereg l =
                    match i with
                    | Mov (_, src, _) when src = l -> ()
                    | _ ->
                        let handleUpdate u =
                            if
                                u <> l
                                && Map.containsKey l interference_graph
                                && Map.containsKey u interference_graph
                            then addEdge interference_graph l u
                            else ()
                        in
                        Set.iter handleUpdate updatedRegs
                in
                Set.iter handleLivereg liveAfterInstr
            in

            let allInstructions =
                List.collect
                    (fun (_, (blk: Cfg.BasicBlock<Set<AsmOperand>, AsmInstruction>)) -> blk.instructions)
                    livenessCfg.BasicBlocks
            in
            List.iter handleInstr allInstructions

        let buildInterferenceGraph fn_name aliased_pseudos instructions =
            let baseGraph = mkBaseGraph () in
            let graph = addPseudoNodes aliased_pseudos baseGraph instructions in
            let cfg = AsmCfg.instructionsToCfg fn_name instructions in
            let livenessCfg = analyzeLiveness fn_name cfg in
            addEdges livenessCfg graph;
            graph

        let addSpillCosts graph instructions =
            let incrCount (counts : Map<string, int>) pseudo =
                let updater = function None -> Some 1 | Some i -> Some (i + 1) in
                // F# Map.change is equivalent to OCaml Map.update
                Map.change pseudo updater counts
            in
            let operands = List.collect getOperands instructions in
            let getPseudo = function Assembly.Pseudo r -> Some r | _ -> None in
            let pseudos = List.choose getPseudo operands in
            let countMap = List.fold incrCount Map.empty pseudos in
            let setSpillCost (nd : AllocNode) =
                match nd.id with
                | Pseudo r ->
                    { nd with spillCost = float (Map.find r countMap) }
                | _ -> nd
            in
            Map.map (fun _ v -> setSpillCost v) graph

        let georgeTest graph hardreg pseudo =
            let pseudoregNeighbors = (getNodeById graph pseudo).neighbors in
            let neighborIsOk neighborId =
                areNeighbors graph neighborId hardreg || degree graph neighborId < k
            in
            Set.forall neighborIsOk pseudoregNeighbors

        let briggsTest graph x y =
            let xNd = getNodeById graph x in
            let yNd = getNodeById graph y in
            let neighbors = Set.union xNd.neighbors yNd.neighbors in
            let hasSignificantDegree neighborId =
                let deg = degree graph neighborId in
                let adjustedDeg =
                    if
                        areNeighbors graph x neighborId && areNeighbors graph y neighborId
                    then deg - 1
                    else deg
                in
                adjustedDeg >= k
            in
            let countSignificant cnt neighbor =
                if hasSignificantDegree neighbor then cnt + 1 else cnt
            in
            let significantNeighborCount =
                Set.fold countSignificant 0 neighbors
            in
            significantNeighborCount < k

        let conservativeCoalescable graph src dst =
            if briggsTest graph src dst then true
            else
                match (src, dst) with
                | Reg _, _ -> georgeTest graph src dst
                | _, Reg _ -> georgeTest graph dst src
                | _ -> false

        let updateGraph g to_merge to_keep =
            let updateNeighbor neighborId =
                addEdge g neighborId to_keep
                removeEdge g neighborId to_merge
            in
            Set.iter updateNeighbor (getNodeById g to_merge).neighbors
            Map.remove to_merge g

        let coalesce graph instructions =
            let processInstr (g, regMap) = function
                | Mov (_, src, dst) ->
                    let src' = DisjointSets.find src regMap
                    let dst' = DisjointSets.find dst regMap
                    if
                        Map.containsKey src' g
                        && Map.containsKey dst' g
                        && src' <> dst'
                        && (not (areNeighbors g src' dst'))
                        && conservativeCoalescable g src' dst'
                    then
                        match src' with
                        | Reg _ ->
                            ( updateGraph g dst' src',
                                DisjointSets.union dst' src' regMap )
                        | _ ->
                            ( updateGraph g src' dst',
                                DisjointSets.union src' dst' regMap )
                    else (g, regMap)
                | _ -> (g, regMap)
            in
            let _updated_graph, newInstructions =
                List.fold processInstr (graph, DisjointSets.init) instructions
            in
            newInstructions

        let rewriteCoalesced instructions coalescedRegs =
            let f r = DisjointSets.find r coalescedRegs in
            let rewriteInstruction = function
                | Mov (t, src, dst) ->
                    let newSrc = f src
                    let newDst = f dst
                    if newSrc = newDst then None else Some (Mov (t, newSrc, newDst))
                | i -> Some (replaceOps f i)
            in
            List.choose rewriteInstruction instructions

        let rec colorGraph graph =
            let remaining =
                graph
                |> Map.toList
                |> List.map snd
                |> List.filter (fun (nd: AllocNode) -> not nd.pruned)
            in
            match remaining with
            | [] -> graph
            | _ ->
                let notPruned nd_id = not (Map.find nd_id graph).pruned in
                let degree (nd: AllocNode) =
                    let unprunedNeighbors = Set.filter notPruned nd.neighbors in
                    Set.count unprunedNeighbors
                in
                let isLowDegree nd = degree nd < k in
                let nextNode =
                    try List.find isLowDegree remaining
                    with :? System.Collections.Generic.KeyNotFoundException ->
                        let spillMetric nd = nd.spillCost / float (degree nd) in
                        let cmp nd1 nd2 =
                            compare (spillMetric nd1) (spillMetric nd2)
                        in
                        let printSpillInfo nd =
                            debugPrint "Node %s has degree %d, spill cost %f and metric %f\n"
                                (showNodeId nd.id) (degree nd) nd.spillCost (spillMetric nd)
                        in
                        debugPrint "================================\n"
                        List.iter printSpillInfo remaining
                        let spilled = ListUtil.min cmp remaining
                        debugPrint "Spill candidate: %s\n" (showNodeId spilled.id)
                        spilled
                in
                let prunedGraph =
                    Map.change nextNode.id
                        (function
                            | Some nd -> Some { nd with pruned = true }
                            | None -> failwith "what??")
                        graph
                in
                let partlyColored = colorGraph prunedGraph in
                let allColors = List.init k id in
                let removeNeighborColor neighborId remainingColors =
                    let neighborNd = Map.find neighborId partlyColored in
                    match neighborNd.color with
                    | Some c -> List.filter (fun col -> col <> c) remainingColors
                    | None -> remainingColors
                in
                let availableColors =
                    Set.fold (fun acc elem -> removeNeighborColor elem acc) allColors nextNode.neighbors
                in
                match availableColors with
                | [] -> partlyColored
                | _ :: _ ->
                    let c =
                        match nextNode.id with
                        | Reg r when not (List.contains r R.caller_saved_regs) ->
                            ListUtil.max compare availableColors
                        | _ -> ListUtil.min compare availableColors
                    in
                    Map.change nextNode.id
                        (function
                            | Some nd -> Some { nd with pruned = false; color = Some c }
                            | None -> failwith "NOPE")
                        partlyColored

        let makeRegisterMap fn_name graph =
            let addColor colorMap nd_id (nd: AllocNode) =
                match nd_id with
                | Reg r -> Map.add (Option.get nd.color) r colorMap
                | _ -> colorMap
            in
            let colorsToRegs = Map.fold addColor Map.empty graph in

            let addMapping (usedCalleeSaved, regMap) _k (nd: AllocNode) =
                match nd with
                | { id = Pseudo p; color = Some c } ->
                    let hardreg = Map.find c colorsToRegs in
                    let usedCalleeSaved =
                        if List.contains hardreg R.caller_saved_regs then usedCalleeSaved
                        else Reg_set.add hardreg usedCalleeSaved
                    in
                    (usedCalleeSaved, Map.add p hardreg regMap)
                | _ -> (usedCalleeSaved, regMap)
            in
            let calleeSavedRegsUsed, regMap =
                Map.fold addMapping (Reg_set.empty, Map.empty) graph
            in
            AssemblySymbols.addCalleeSavedRegsUsed fn_name calleeSavedRegsUsed
            regMap

        let replacePseudoregs instructions regMap =
            let f = function
                | Assembly.Pseudo p as op -> 
                    (try Reg (Map.find p regMap) with :? System.Collections.Generic.KeyNotFoundException -> op)
                | op -> op
            in
            cleanupMovs (List.map (replaceOps f) instructions)

        let rec coalesceLoop currentInstructions =
            let graph = buildInterferenceGraph fn_name aliased_pseudos currentInstructions
            let coalescedRegs = coalesce graph currentInstructions in
            if DisjointSets.isEmpty coalescedRegs then (graph, currentInstructions)
            else
                let newInstructions =
                    rewriteCoalesced currentInstructions coalescedRegs
                in
                coalesceLoop newInstructions

        let coalescedGraph, coalescedInstructions = coalesceLoop instructions
        let graphWithSpillCosts =
            addSpillCosts coalescedGraph coalescedInstructions
        in
        let coloredGraph = colorGraph graphWithSpillCosts in
        let registerMap = makeRegisterMap fn_name coloredGraph in
        replacePseudoregs coalescedInstructions registerMap


let GP = new Allocator ({
    suffix = "gp"
    all_hardregs = [ AX; BX; CX; DX; DI; SI; R8; R9; R12; R13; R14; R15 ]
    caller_saved_regs = [ AX; CX; DX; DI; SI; R8; R9 ]
    pseudo_is_current_type = fun p -> AssemblySymbols.getType p <> Double
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
    pseudo_is_current_type = fun p -> AssemblySymbols.getType p = Double
})

let allocateRegisters aliased_pseudos (Program tls) =
    let allocateRegsForFun fnName instructions =
        instructions
        |> GP.allocate fnName aliased_pseudos
        |> XMM.allocate fnName aliased_pseudos
    in
    let allocInTl = function
        | Function f ->
            Function
                { f with instructions = allocateRegsForFun f.name f.instructions }
        | tl -> tl
    in
    Program (List.map allocInTl tls)