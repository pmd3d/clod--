//! Control-flow graph helpers specialized for assembly instructions.
//!
//! This module is the Rust counterpart of the original `AsmCfg.fs`: it maps
//! assembly branch instructions to the generic CFG representation and exposes
//! the generic CFG operations under the assembly-specific module name.
#![allow(non_snake_case)]

use super::Assembly::AsmInstruction;

use super::Cfg::{self, SimpleInstr};
pub use super::Cfg::{BasicBlock, ControlFlowGraph, NodeId};

/// Reduce an assembly instruction to the control-flow effect understood by
/// the generic CFG builder.
pub fn simplify(instruction: &AsmInstruction) -> SimpleInstr {
    match instruction {
        AsmInstruction::Label(label) => SimpleInstr::Label(label.clone()),
        AsmInstruction::Jmp(target) => SimpleInstr::UnconditionalJump(target.clone()),
        AsmInstruction::JmpCC(_, target) => SimpleInstr::ConditionalJump(target.clone()),
        AsmInstruction::Ret => SimpleInstr::Return,
        _ => SimpleInstr::Other,
    }
}

pub fn instructionsToCfg(
    debugLabel: impl Into<String>,
    instructions: Vec<AsmInstruction>,
) -> ControlFlowGraph<(), AsmInstruction> {
    Cfg::instructionsToCfg(simplify, debugLabel, instructions)
}

pub fn cfgToInstructions<V>(g: &ControlFlowGraph<V, AsmInstruction>) -> Vec<AsmInstruction> {
    Cfg::cfgToInstructions(g)
}

pub fn getSuccs<V>(ndId: NodeId, cfg: &ControlFlowGraph<V, AsmInstruction>) -> &[NodeId] {
    Cfg::getSuccs(ndId, cfg)
}

pub fn getBlockValue<V>(blocknum: usize, cfg: &ControlFlowGraph<V, AsmInstruction>) -> &V {
    Cfg::getBlockValue(blocknum, cfg)
}

pub fn addEdge<V>(pred: NodeId, succ: NodeId, g: &mut ControlFlowGraph<V, AsmInstruction>) {
    Cfg::addEdge(pred, succ, g)
}

pub fn removeEdge<V>(pred: NodeId, succ: NodeId, g: &mut ControlFlowGraph<V, AsmInstruction>) {
    Cfg::removeEdge(pred, succ, g)
}

pub fn updateBasicBlock<V>(
    idx: usize,
    block: BasicBlock<V, AsmInstruction>,
    g: &mut ControlFlowGraph<V, AsmInstruction>,
) {
    Cfg::updateBasicBlock(idx, block, g)
}

pub fn initializeAnnotation<V, W: Clone>(
    cfg: ControlFlowGraph<V, AsmInstruction>,
    value: W,
) -> ControlFlowGraph<W, AsmInstruction> {
    Cfg::initializeAnnotation(cfg, value)
}

pub fn stripAnnotations<V>(
    cfg: ControlFlowGraph<V, AsmInstruction>,
) -> ControlFlowGraph<(), AsmInstruction> {
    Cfg::stripAnnotations(cfg)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ports::Assembly::{AsmCondCode, AsmInstruction::*};

    #[test]
    fn classifies_control_flow_instructions() {
        assert_eq!(
            simplify(&Label("again".into())),
            SimpleInstr::Label("again".into())
        );
        assert_eq!(
            simplify(&Jmp("done".into())),
            SimpleInstr::UnconditionalJump("done".into())
        );
        assert_eq!(
            simplify(&JmpCC(AsmCondCode::E, "yes".into())),
            SimpleInstr::ConditionalJump("yes".into())
        );
        assert_eq!(simplify(&Ret), SimpleInstr::Return);
        assert_eq!(simplify(&Call("function".into())), SimpleInstr::Other);
    }

    #[test]
    fn builds_and_round_trips_an_assembly_cfg() {
        let instructions = vec![
            Label("start".into()),
            JmpCC(AsmCondCode::NE, "done".into()),
            Jmp("start".into()),
            Label("done".into()),
            Ret,
        ];
        let cfg = instructionsToCfg("assembly", instructions.clone());

        assert_eq!(cfg.BasicBlocks.len(), 3);
        assert_eq!(getSuccs(NodeId::Entry, &cfg), [NodeId::Block(0)]);
        assert_eq!(
            getSuccs(NodeId::Block(0), &cfg),
            [NodeId::Block(2), NodeId::Block(1)]
        );
        assert_eq!(cfgToInstructions(&cfg), instructions);
    }

    #[test]
    fn forwards_annotation_and_graph_mutation_operations() {
        let mut cfg = instructionsToCfg("assembly", vec![Ret]);
        removeEdge(NodeId::Entry, NodeId::Block(0), &mut cfg);
        assert!(getSuccs(NodeId::Entry, &cfg).is_empty());
        addEdge(NodeId::Entry, NodeId::Block(0), &mut cfg);

        let annotated = initializeAnnotation(cfg, 42_u8);
        assert_eq!(*getBlockValue(0, &annotated), 42);
        assert_eq!(stripAnnotations(annotated).BasicBlocks[0].1.value, ());
    }
}
