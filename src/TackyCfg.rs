//! Control-flow graph helpers specialized for TACKY instructions.
#![allow(non_snake_case)]

use super::Cfg::{self, SimpleInstr};
pub use super::Cfg::{BasicBlock, ControlFlowGraph, NodeId};
use super::Tacky::TackyInstruction;

pub fn simplify(instruction: &TackyInstruction) -> SimpleInstr {
    match instruction {
        TackyInstruction::Label(label) => SimpleInstr::Label(label.clone()),
        TackyInstruction::Jump(target) => SimpleInstr::UnconditionalJump(target.clone()),
        TackyInstruction::JumpIfZero(_, target) | TackyInstruction::JumpIfNotZero(_, target) => {
            SimpleInstr::ConditionalJump(target.clone())
        }
        TackyInstruction::Return(_) => SimpleInstr::Return,
        _ => SimpleInstr::Other,
    }
}

pub fn instructionsToCfg(
    debugLabel: impl Into<String>,
    instructions: Vec<TackyInstruction>,
) -> ControlFlowGraph<(), TackyInstruction> {
    Cfg::instructionsToCfg(simplify, debugLabel, instructions)
}
pub fn cfgToInstructions<V>(g: &ControlFlowGraph<V, TackyInstruction>) -> Vec<TackyInstruction> {
    Cfg::cfgToInstructions(g)
}
pub fn getSuccs<V>(id: NodeId, cfg: &ControlFlowGraph<V, TackyInstruction>) -> &[NodeId] {
    Cfg::getSuccs(id, cfg)
}
pub fn getBlockValue<V>(n: usize, cfg: &ControlFlowGraph<V, TackyInstruction>) -> &V {
    Cfg::getBlockValue(n, cfg)
}
pub fn addEdge<V>(pred: NodeId, succ: NodeId, g: &mut ControlFlowGraph<V, TackyInstruction>) {
    Cfg::addEdge(pred, succ, g)
}
pub fn removeEdge<V>(pred: NodeId, succ: NodeId, g: &mut ControlFlowGraph<V, TackyInstruction>) {
    Cfg::removeEdge(pred, succ, g)
}
pub fn updateBasicBlock<V>(
    idx: usize,
    block: BasicBlock<V, TackyInstruction>,
    g: &mut ControlFlowGraph<V, TackyInstruction>,
) {
    Cfg::updateBasicBlock(idx, block, g)
}
pub fn initializeAnnotation<V, W: Clone>(
    cfg: ControlFlowGraph<V, TackyInstruction>,
    value: W,
) -> ControlFlowGraph<W, TackyInstruction> {
    Cfg::initializeAnnotation(cfg, value)
}
pub fn stripAnnotations<V>(
    cfg: ControlFlowGraph<V, TackyInstruction>,
) -> ControlFlowGraph<(), TackyInstruction> {
    Cfg::stripAnnotations(cfg)
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn builds_tacky_cfg() {
        let code = vec![
            TackyInstruction::Label("loop".into()),
            TackyInstruction::JumpIfZero(
                super::super::Tacky::TackyVal::Constant(super::super::Const::INT_ZERO),
                "done".into(),
            ),
            TackyInstruction::Jump("loop".into()),
            TackyInstruction::Label("done".into()),
            TackyInstruction::Return(None),
        ];
        let cfg = instructionsToCfg("tacky", code.clone());
        assert_eq!(cfgToInstructions(&cfg), code);
        assert_eq!(
            getSuccs(NodeId::Block(0), &cfg),
            [NodeId::Block(2), NodeId::Block(1)]
        );
    }
}
