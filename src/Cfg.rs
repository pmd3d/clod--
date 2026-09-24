//! Generic control-flow graphs, faithfully ported from the original `Cfg.fs`.
#![allow(non_snake_case)]

use std::collections::BTreeMap;
use std::fs::File;
use std::io::{self, Write};
use std::process::Command;
use std::sync::atomic::{AtomicUsize, Ordering};

/// The control-flow effect of an instruction.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SimpleInstr {
    Label(String),
    ConditionalJump(String),
    UnconditionalJump(String),
    Return,
    Other,
}

/// An entry node, numbered basic block, or exit node.
///
/// Variant order deliberately matches the custom comparison in the F# type.
#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum NodeId {
    Entry,
    Block(usize),
    Exit,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct BasicBlock<V, I> {
    pub id: NodeId,
    pub instructions: Vec<(V, I)>,
    pub preds: Vec<NodeId>,
    pub succs: Vec<NodeId>,
    pub value: V,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ControlFlowGraph<V, I> {
    /// Basic blocks are kept in an association list indexed by block number.
    pub BasicBlocks: Vec<(usize, BasicBlock<V, I>)>,
    pub entrySuccs: Vec<NodeId>,
    pub exitPreds: Vec<NodeId>,
    pub debugLabel: String,
}

fn findBlock<V, I>(n: usize, blocks: &[(usize, BasicBlock<V, I>)]) -> &BasicBlock<V, I> {
    blocks
        .iter()
        .find(|(key, _)| *key == n)
        .map(|(_, block)| block)
        .expect("Internal error: block not found in CFG")
}

pub fn getSuccs<V, I>(ndId: NodeId, cfg: &ControlFlowGraph<V, I>) -> &[NodeId] {
    match ndId {
        NodeId::Entry => &cfg.entrySuccs,
        NodeId::Block(n) => &findBlock(n, &cfg.BasicBlocks).succs,
        NodeId::Exit => &[],
    }
}

pub fn getBlockValue<V, I>(blocknum: usize, cfg: &ControlFlowGraph<V, I>) -> &V {
    &findBlock(blocknum, &cfg.BasicBlocks).value
}

fn updateBlock<V, I, F>(f: F, blockNum: usize, g: &mut ControlFlowGraph<V, I>)
where
    F: FnOnce(&mut BasicBlock<V, I>),
{
    if let Some((_, block)) = g.BasicBlocks.iter_mut().find(|(i, _)| *i == blockNum) {
        f(block);
    }
}

fn addId(ndId: NodeId, ids: &mut Vec<NodeId>) {
    if !ids.contains(&ndId) {
        // F#'s list cons makes newly discovered edges appear first.
        ids.insert(0, ndId);
    }
}

fn removeId(ndId: NodeId, ids: &mut Vec<NodeId>) {
    ids.retain(|id| *id != ndId);
}

fn updateSuccessors<V, I, F>(f: F, ndId: NodeId, g: &mut ControlFlowGraph<V, I>)
where
    F: FnOnce(&mut Vec<NodeId>),
{
    match ndId {
        NodeId::Entry => f(&mut g.entrySuccs),
        NodeId::Block(n) => updateBlock(|block| f(&mut block.succs), n, g),
        NodeId::Exit => panic!("Internal error: malformed CFG"),
    }
}

fn updatePredecessors<V, I, F>(f: F, ndId: NodeId, g: &mut ControlFlowGraph<V, I>)
where
    F: FnOnce(&mut Vec<NodeId>),
{
    match ndId {
        NodeId::Entry => panic!("Internal error: malformed CFG"),
        NodeId::Block(n) => updateBlock(|block| f(&mut block.preds), n, g),
        NodeId::Exit => f(&mut g.exitPreds),
    }
}

/// Add an edge, without duplicating an existing predecessor or successor.
pub fn addEdge<V, I>(pred: NodeId, succ: NodeId, g: &mut ControlFlowGraph<V, I>) {
    updateSuccessors(|ids| addId(succ, ids), pred, g);
    updatePredecessors(|ids| addId(pred, ids), succ, g);
}

pub fn removeEdge<V, I>(pred: NodeId, succ: NodeId, g: &mut ControlFlowGraph<V, I>) {
    updateSuccessors(|ids| removeId(succ, ids), pred, g);
    updatePredecessors(|ids| removeId(pred, ids), succ, g);
}

/// Replace the block with the specified association-list key.
pub fn updateBasicBlock<V, I>(
    blockIdx: usize,
    newBlock: BasicBlock<V, I>,
    g: &mut ControlFlowGraph<V, I>,
) {
    if let Some(entry) = g.BasicBlocks.iter_mut().find(|(i, _)| *i == blockIdx) {
        entry.1 = newBlock;
    }
}

fn partitionIntoBasicBlocks<I, F>(simplify: &F, instructions: Vec<I>) -> Vec<Vec<I>>
where
    F: Fn(&I) -> SimpleInstr,
{
    let mut finished = Vec::new();
    let mut current = Vec::new();
    for instruction in instructions {
        match simplify(&instruction) {
            SimpleInstr::Label(_) => {
                if !current.is_empty() {
                    finished.push(current);
                    current = Vec::new();
                }
                current.push(instruction);
            }
            SimpleInstr::ConditionalJump(_)
            | SimpleInstr::UnconditionalJump(_)
            | SimpleInstr::Return => {
                current.push(instruction);
                finished.push(current);
                current = Vec::new();
            }
            SimpleInstr::Other => current.push(instruction),
        }
    }
    if !current.is_empty() {
        finished.push(current);
    }
    finished
}

fn addAllEdges<I, F>(simplify: &F, g: &mut ControlFlowGraph<(), I>)
where
    F: Fn(&I) -> SimpleInstr,
{
    let mut labelMap = BTreeMap::new();
    for (_, block) in &g.BasicBlocks {
        if let Some((_, first)) = block.instructions.first() {
            if let SimpleInstr::Label(label) = simplify(first) {
                labelMap.insert(label, block.id);
            }
        }
    }

    // Preserve the F# implementation's entry edge even for an empty input.
    addEdge(NodeId::Entry, NodeId::Block(0), g);
    let block_count = g.BasicBlocks.len();
    for index in 0..block_count {
        let (id_num, id, effect) = {
            let (id_num, block) = &g.BasicBlocks[index];
            let (_, last) = block
                .instructions
                .last()
                .expect("Internal error: empty basic block");
            (*id_num, block.id, simplify(last))
        };
        let next = if index + 1 == block_count {
            NodeId::Exit
        } else {
            NodeId::Block(id_num + 1)
        };
        match effect {
            SimpleInstr::Return => addEdge(id, NodeId::Exit, g),
            SimpleInstr::UnconditionalJump(target) => {
                let target_id = *labelMap.get(&target).expect("label not found in CFG");
                addEdge(id, target_id, g);
            }
            SimpleInstr::ConditionalJump(target) => {
                let target_id = *labelMap.get(&target).expect("label not found in CFG");
                addEdge(id, next, g);
                addEdge(id, target_id, g);
            }
            _ => addEdge(id, next, g),
        }
    }
}

pub fn instructionsToCfg<I, F>(
    simplify: F,
    debugLabel: impl Into<String>,
    instructions: Vec<I>,
) -> ControlFlowGraph<(), I>
where
    F: Fn(&I) -> SimpleInstr,
{
    let BasicBlocks = partitionIntoBasicBlocks(&simplify, instructions)
        .into_iter()
        .enumerate()
        .map(|(idx, instructions)| {
            (
                idx,
                BasicBlock {
                    id: NodeId::Block(idx),
                    instructions: instructions.into_iter().map(|i| ((), i)).collect(),
                    preds: Vec::new(),
                    succs: Vec::new(),
                    value: (),
                },
            )
        })
        .collect();
    let mut cfg = ControlFlowGraph {
        BasicBlocks,
        entrySuccs: Vec::new(),
        exitPreds: Vec::new(),
        debugLabel: debugLabel.into(),
    };
    addAllEdges(&simplify, &mut cfg);
    cfg
}

pub fn cfgToInstructions<V, I: Clone>(g: &ControlFlowGraph<V, I>) -> Vec<I> {
    g.BasicBlocks
        .iter()
        .flat_map(|(_, block)| block.instructions.iter().map(|(_, i)| i.clone()))
        .collect()
}

pub fn initializeAnnotation<V, I, W: Clone>(
    cfg: ControlFlowGraph<V, I>,
    dummyVal: W,
) -> ControlFlowGraph<W, I> {
    ControlFlowGraph {
        BasicBlocks: cfg
            .BasicBlocks
            .into_iter()
            .map(|(idx, block)| {
                (
                    idx,
                    BasicBlock {
                        id: block.id,
                        instructions: block
                            .instructions
                            .into_iter()
                            .map(|(_, i)| (dummyVal.clone(), i))
                            .collect(),
                        preds: block.preds,
                        succs: block.succs,
                        value: dummyVal.clone(),
                    },
                )
            })
            .collect(),
        entrySuccs: cfg.entrySuccs,
        exitPreds: cfg.exitPreds,
        debugLabel: cfg.debugLabel,
    }
}

pub fn stripAnnotations<V, I>(cfg: ControlFlowGraph<V, I>) -> ControlFlowGraph<(), I> {
    initializeAnnotation(cfg, ())
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn setCounter(counter: usize) {
    COUNTER.store(counter, Ordering::SeqCst);
}

pub fn getCounter() -> usize {
    COUNTER.load(Ordering::SeqCst)
}

fn writeNodeId(out: &mut dyn Write, id: NodeId) -> io::Result<()> {
    match id {
        NodeId::Exit => write!(out, "exit"),
        NodeId::Entry => write!(out, "entry"),
        NodeId::Block(n) => write!(out, "block{n}"),
    }
}

/// Write a Graphviz file and render it with `dot`, as the F# debug helper does.
pub fn printGraphviz<V, I, PI, PV>(
    ppInstr: PI,
    ppVal: PV,
    cfg: &ControlFlowGraph<V, I>,
) -> io::Result<()>
where
    PI: Fn(&mut dyn Write, &I) -> io::Result<()>,
    PV: Fn(&mut dyn Write, &V) -> io::Result<()>,
{
    let number = COUNTER.fetch_add(1, Ordering::SeqCst);
    let filename = format!("{}.{}.dot", cfg.debugLabel, number);
    let mut writer = File::create(&filename)?;
    writeln!(writer, "digraph {{")?;
    writeln!(writer, "  labeljust=l")?;
    writeln!(writer, "  node[shape=\"box\"]")?;
    writeln!(writer, "  entry[label=\"ENTRY\"]")?;
    writeln!(writer, "  exit[label=\"EXIT\"]")?;
    for (label, block) in &cfg.BasicBlocks {
        write!(writer, "block{label}[label=<<table><tr><td colspan=\"2\"><b>")?;
        writeNodeId(&mut writer, block.id)?;
        writeln!(writer, "</b></td></tr>")?;
        for (value, instruction) in &block.instructions {
            write!(writer, "<tr><td align=\"left\">")?;
            ppInstr(&mut writer, instruction)?;
            write!(writer, "</td><td align=\"left\">")?;
            ppVal(&mut writer, value)?;
            writeln!(writer, "</td></tr>")?;
        }
        write!(writer, "<tr><td colspan=\"2\">")?;
        ppVal(&mut writer, &block.value)?;
        writeln!(writer, "</td></tr></table>>]")?;
    }
    for successor in &cfg.entrySuccs {
        write!(writer, "entry -> ")?;
        writeNodeId(&mut writer, *successor)?;
        writeln!(writer)?;
    }
    for (label, block) in &cfg.BasicBlocks {
        for successor in &block.succs {
            write!(writer, "block{label} -> ")?;
            writeNodeId(&mut writer, *successor)?;
        }
        // The original emits one newline after all of a block's edges.
        writeln!(writer)?;
    }
    writeln!(writer, "}}")?;
    writer.flush()?;
    drop(writer);

    let png = format!("{}.{}.png", cfg.debugLabel, number);
    let status = Command::new("dot")
        .args(["-Tpng", &filename, "-o", &png])
        .status()?;
    if status.success() {
        Ok(())
    } else {
        Err(io::Error::other(format!(
            "graphviz fail: dot -Tpng {filename} -o {png}"
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Clone, Debug, Eq, PartialEq)]
    enum Instruction {
        Label(&'static str),
        Jump(&'static str),
        JumpIf(&'static str),
        Return,
        Other(u8),
    }

    fn simplify(i: &Instruction) -> SimpleInstr {
        match i {
            Instruction::Label(label) => SimpleInstr::Label((*label).into()),
            Instruction::Jump(label) => SimpleInstr::UnconditionalJump((*label).into()),
            Instruction::JumpIf(label) => SimpleInstr::ConditionalJump((*label).into()),
            Instruction::Return => SimpleInstr::Return,
            Instruction::Other(_) => SimpleInstr::Other,
        }
    }

    #[test]
    fn partitions_instructions_and_builds_edges() {
        let instructions = vec![
            Instruction::Label("start"),
            Instruction::Other(1),
            Instruction::JumpIf("done"),
            Instruction::Other(2),
            Instruction::Jump("start"),
            Instruction::Label("done"),
            Instruction::Return,
        ];
        let cfg = instructionsToCfg(simplify, "test", instructions.clone());

        assert_eq!(cfg.BasicBlocks.len(), 3);
        assert_eq!(cfg.entrySuccs, [NodeId::Block(0)]);
        assert_eq!(cfg.BasicBlocks[0].1.succs, [NodeId::Block(2), NodeId::Block(1)]);
        assert_eq!(cfg.BasicBlocks[1].1.succs, [NodeId::Block(0)]);
        assert_eq!(cfg.BasicBlocks[2].1.succs, [NodeId::Exit]);
        assert_eq!(cfg.exitPreds, [NodeId::Block(2)]);
        assert_eq!(cfgToInstructions(&cfg), instructions);
    }

    #[test]
    fn edge_updates_are_symmetric_and_do_not_duplicate_edges() {
        let mut cfg = instructionsToCfg(simplify, "test", vec![Instruction::Return]);
        addEdge(NodeId::Entry, NodeId::Block(0), &mut cfg);
        assert_eq!(cfg.entrySuccs, [NodeId::Block(0)]);
        assert_eq!(cfg.BasicBlocks[0].1.preds, [NodeId::Entry]);

        removeEdge(NodeId::Entry, NodeId::Block(0), &mut cfg);
        assert!(cfg.entrySuccs.is_empty());
        assert!(cfg.BasicBlocks[0].1.preds.is_empty());
    }

    #[test]
    fn annotations_can_change_type_and_be_stripped() {
        let cfg = instructionsToCfg(simplify, "test", vec![Instruction::Other(1)]);
        let annotated = initializeAnnotation(cfg, "unknown".to_owned());
        assert_eq!(annotated.BasicBlocks[0].1.value, "unknown");
        assert_eq!(annotated.BasicBlocks[0].1.instructions[0].0, "unknown");
        let stripped = stripAnnotations(annotated);
        assert_eq!(stripped.BasicBlocks[0].1.value, ());
    }
}
