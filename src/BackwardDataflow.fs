module Backward_dataflow

type Annotation<'varSet> = 'varSet
type AnnotatedBlock<'a, 'block> = Cfg.BasicBlock<'a, 'block>
type AnnotatedGraph<'a, 'block> = Cfg.ControlFlowGraph<'a, 'block>

let debugPrint (debug: bool) (extra_tag: string) (pp_var: System.IO.TextWriter -> 'var -> unit)
                (elements: 'varSet -> 'var list)
                (print_graphviz: (System.IO.TextWriter -> 'varSet -> unit) -> 'cfg -> unit)
                (debugLabel: string)
                (setDebugLabel: string -> 'cfg -> 'cfg)
                (cfg: 'cfg) =
    if debug then
        let livevarPrinter (fmt: System.IO.TextWriter) (liveVars: 'varSet) =
            elements liveVars
            |> List.iteri (fun i v ->
                if i > 0 then fmt.Write(", ")
                pp_var fmt v)
        let lbl = debugLabel + "_dse" + extra_tag
        print_graphviz livevarPrinter (setDebugLabel lbl cfg)

let analyze (debug: bool) (pp_var: System.IO.TextWriter -> 'var -> unit)
            (empty: 'varSet)
            (equal: 'varSet -> 'varSet -> bool)
            (elements: 'varSet -> 'var list)
            (meet_fn: 'cfg -> 'block -> 'varSet)
            (transfer_fn: 'block -> 'varSet -> 'block)
            (initialize_annotation: 'cfg0 -> 'varSet -> 'cfg)
            (update_basic_block: int -> 'block -> 'cfg -> 'cfg)
            (get_value: 'block -> 'varSet)
            (get_preds: 'block -> Cfg.NodeId list)
            (get_basic_blocks: 'cfg -> (int * 'block) list)
            (getDebugLabel: 'cfg -> string)
            (setDebugLabel: string -> 'cfg -> 'cfg)
            (print_graphviz: (System.IO.TextWriter -> 'varSet -> unit) -> 'cfg -> unit)
            (cfg: 'cfg0) =
    let startingCfg = initialize_annotation cfg empty
    let rec processWorklist (currentCfg: 'cfg)
                             (worklist: (int * 'block) list) =
        debugPrint debug "_in_progress_" pp_var elements print_graphviz
            (getDebugLabel currentCfg) setDebugLabel currentCfg
        match worklist with
        | [] -> currentCfg // we're done
        | (blockIdx, blk) :: rest ->
            let oldAnnotation = get_value blk
            let liveVarsAtExit = meet_fn currentCfg blk
            let block' = transfer_fn blk liveVarsAtExit
            let updatedCfg = update_basic_block blockIdx block' currentCfg
            let newWorklist =
                if equal oldAnnotation (get_value block') then rest
                else
                    List.fold
                        (fun wklist pred ->
                            match pred with
                            | Cfg.Entry -> wklist
                            | Cfg.Exit ->
                                failwith "Internal error: malformed CFG"
                            | Cfg.Block n ->
                                if List.exists (fun (k, _) -> k = n) wklist then wklist
                                else
                                    let blocks = get_basic_blocks updatedCfg
                                    let blk =
                                        match List.tryFind (fun (k, _) -> k = n) blocks with
                                        | Some (_, b) -> b
                                        | None -> failwith "Internal error: block not found in CFG"
                                    (n, blk) :: wklist)
                        rest (get_preds block')
            processWorklist updatedCfg newWorklist
    processWorklist startingCfg (List.rev (get_basic_blocks startingCfg))