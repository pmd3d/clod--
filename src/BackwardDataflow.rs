//! Generic work-list solver for backwards data-flow analyses.
#![allow(non_snake_case)]

use std::io::{self, Write};

use super::Cfg::{BasicBlock, ControlFlowGraph, NodeId};

pub type Annotation<VarSet> = VarSet;
pub type AnnotatedBlock<VarSet, Instruction> = BasicBlock<VarSet, Instruction>;
pub type AnnotatedGraph<VarSet, Instruction> = ControlFlowGraph<VarSet, Instruction>;

#[allow(clippy::too_many_arguments)]
pub fn debugPrint<Var, VarSet, Cfg, PP, Elements, Print, Label, SetLabel>(
    debug: bool,
    extra_tag: &str,
    pp_var: &PP,
    elements: &Elements,
    print_graphviz: &Print,
    debug_label: &Label,
    set_debug_label: &SetLabel,
    cfg: &Cfg,
) -> io::Result<()>
where
    PP: Fn(&mut dyn Write, &Var) -> io::Result<()>,
    Elements: Fn(&VarSet) -> Vec<Var>,
    Print: Fn(&mut dyn FnMut(&mut dyn Write, &VarSet) -> io::Result<()>, &Cfg) -> io::Result<()>,
    Label: Fn(&Cfg) -> &str,
    SetLabel: Fn(String, &Cfg) -> Cfg,
{
    if debug {
        let mut livevar_printer = |out: &mut dyn Write, live_vars: &VarSet| {
            for (index, variable) in elements(live_vars).iter().enumerate() {
                if index > 0 { write!(out, ", ")?; }
                pp_var(out, variable)?;
            }
            Ok(())
        };
        let label = format!("{}_dse{}", debug_label(cfg), extra_tag);
        print_graphviz(&mut livevar_printer, &set_debug_label(label, cfg))?;
    }
    Ok(())
}

/// Run a backwards analysis to a fixed point.
///
/// As in the F# implementation, blocks initially enter the work list in
/// reverse CFG order and a changed block schedules each unscheduled predecessor
/// at the front of the list.
#[allow(clippy::too_many_arguments)]
pub fn analyze<Var, VarSet, Block, Cfg0, Cfg, PP, Elements, Meet, Transfer, Initialize,
    Update, Value, Preds, Blocks, Label, SetLabel, Print>(
    debug: bool, pp_var: PP, empty: VarSet, equal: impl Fn(&VarSet, &VarSet) -> bool,
    elements: Elements, meet_fn: Meet, transfer_fn: Transfer,
    initialize_annotation: Initialize, update_basic_block: Update, get_value: Value,
    get_preds: Preds, get_basic_blocks: Blocks, get_debug_label: Label,
    set_debug_label: SetLabel, print_graphviz: Print, cfg: Cfg0,
) -> io::Result<Cfg>
where
    VarSet: Clone,
    Block: Clone,
    PP: Fn(&mut dyn Write, &Var) -> io::Result<()>,
    Elements: Fn(&VarSet) -> Vec<Var>,
    Meet: Fn(&Cfg, &Block) -> VarSet,
    Transfer: Fn(Block, VarSet) -> Block,
    Initialize: Fn(Cfg0, VarSet) -> Cfg,
    Update: Fn(usize, Block, &mut Cfg),
    Value: Fn(&Block) -> &VarSet,
    Preds: Fn(&Block) -> &[NodeId],
    Blocks: Fn(&Cfg) -> Vec<(usize, Block)>,
    Label: Fn(&Cfg) -> &str,
    SetLabel: Fn(String, &Cfg) -> Cfg,
    Print: Fn(&mut dyn FnMut(&mut dyn Write, &VarSet) -> io::Result<()>, &Cfg) -> io::Result<()>,
{
    let mut current_cfg = initialize_annotation(cfg, empty);
    let mut worklist = get_basic_blocks(&current_cfg);
    worklist.reverse();
    loop {
        debugPrint(debug, "_in_progress_", &pp_var, &elements, &print_graphviz,
                   &get_debug_label, &set_debug_label, &current_cfg)?;
        let Some((block_index, block)) = worklist.first().cloned() else { return Ok(current_cfg); };
        worklist.remove(0);
        let old_annotation = get_value(&block).clone();
        let live_vars_at_exit = meet_fn(&current_cfg, &block);
        let updated_block = transfer_fn(block, live_vars_at_exit);
        let changed = !equal(&old_annotation, get_value(&updated_block));
        let predecessors = get_preds(&updated_block).to_vec();
        update_basic_block(block_index, updated_block, &mut current_cfg);
        if changed {
            for predecessor in predecessors {
                match predecessor {
                    NodeId::Entry => {}
                    NodeId::Exit => panic!("Internal error: malformed CFG"),
                    NodeId::Block(number) if !worklist.iter().any(|(index, _)| *index == number) => {
                        let predecessor_block = get_basic_blocks(&current_cfg).into_iter()
                            .find(|(index, _)| *index == number).map(|(_, block)| block)
                            .expect("Internal error: block not found in CFG");
                        worklist.insert(0, (number, predecessor_block));
                    }
                    NodeId::Block(_) => {}
                }
            }
        }
    }
}
