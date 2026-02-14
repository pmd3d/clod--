module Backward_dataflow

type annotation<'varSet> = 'varSet
type annotated_block<'a, 'block> = Cfg.basic_block<'a, 'block>
type annotated_graph<'a, 'block> = Cfg.t<'a, 'block>

let debug_print (extra_tag: string) (pp_var: System.IO.TextWriter -> 'var -> unit)
                (elements: 'varSet -> 'var list)
                (print_graphviz: (System.IO.TextWriter -> 'varSet -> unit) -> 'cfg -> unit)
                (debug_label: string)
                (set_debug_label: string -> 'cfg -> 'cfg)
                (cfg: 'cfg) =
    if Settings.debug.Value then
        let livevar_printer (fmt: System.IO.TextWriter) (live_vars: 'varSet) =
            elements live_vars
            |> List.iteri (fun i v ->
                if i > 0 then fmt.Write(", ")
                pp_var fmt v)
        let lbl = debug_label + "_dse" + extra_tag
        print_graphviz livevar_printer (set_debug_label lbl cfg)

let analyze (pp_var: System.IO.TextWriter -> 'var -> unit)
            (empty: 'varSet)
            (equal: 'varSet -> 'varSet -> bool)
            (elements: 'varSet -> 'var list)
            (meet_fn: 'cfg -> 'block -> 'varSet)
            (transfer_fn: 'block -> 'varSet -> 'block)
            (initialize_annotation: 'cfg0 -> 'varSet -> 'cfg)
            (update_basic_block: int -> 'block -> 'cfg -> 'cfg)
            (get_value: 'block -> 'varSet)
            (get_preds: 'block -> Cfg.node_id list)
            (get_basic_blocks: 'cfg -> (int * 'block) list)
            (get_debug_label: 'cfg -> string)
            (set_debug_label: string -> 'cfg -> 'cfg)
            (print_graphviz: (System.IO.TextWriter -> 'varSet -> unit) -> 'cfg -> unit)
            (cfg: 'cfg0) =
    let starting_cfg = initialize_annotation cfg empty
    let rec process_worklist (current_cfg: 'cfg)
                             (worklist: (int * 'block) list) =
        debug_print "_in_progress_" pp_var elements print_graphviz
            (get_debug_label current_cfg) set_debug_label current_cfg
        match worklist with
        | [] -> current_cfg // we're done
        | (block_idx, blk) :: rest ->
            let old_annotation = get_value blk
            let live_vars_at_exit = meet_fn current_cfg blk
            let block' = transfer_fn blk live_vars_at_exit
            let updated_cfg = update_basic_block block_idx block' current_cfg
            let new_worklist =
                if equal old_annotation (get_value block') then rest
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
                                    let blocks = get_basic_blocks updated_cfg
                                    let blk = List.find (fun (k, _) -> k = n) blocks |> snd
                                    (n, blk) :: wklist)
                        rest (get_preds block')
            process_worklist updated_cfg new_worklist
    process_worklist starting_cfg (List.rev (get_basic_blocks starting_cfg))