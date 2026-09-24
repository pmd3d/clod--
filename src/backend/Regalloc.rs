//! Graph-coloring register allocator for integer and SSE pseudo-registers.
#![allow(non_snake_case)]
use super::Assembly::*;
use super::{AsmCfg, AssemblySymbols, Cfg, DisjointSets};
use std::collections::{BTreeMap, BTreeSet};
use std::sync::atomic::{AtomicBool, Ordering};

type OperandSet = BTreeSet<AsmOperand>;
type Graph = BTreeMap<AsmOperand, AllocNode>;
static DEBUG: AtomicBool = AtomicBool::new(false);
#[derive(Clone, Debug)]
pub struct AllocNode {
    pub id: AsmOperand,
    pub neighbors: OperandSet,
    pub spillCost: f64,
    pub color: Option<usize>,
    pub pruned: bool,
}
#[derive(Clone)]
pub struct RegTypeOps {
    pub suffix: &'static str,
    pub all_hardregs: Vec<AsmReg>,
    pub caller_saved_regs: Vec<AsmReg>,
    pub pseudo_is_current_type: fn(&str) -> bool,
}
pub struct Allocator {
    ops: RegTypeOps,
}

pub fn getOperands(i: &AsmInstruction) -> Vec<AsmOperand> {
    use AsmInstruction::*;
    match i {
        Mov(_, a, b) | Lea(a, b) | Cvttsd2si(_, a, b) | Cvtsi2sd(_, a, b) | Cmp(_, a, b) => {
            vec![a.clone(), b.clone()]
        }
        Movsx(x) => vec![x.src.clone(), x.dst.clone()],
        MovZeroExtend(x) => vec![x.src.clone(), x.dst.clone()],
        Unary(_, _, a) | Idiv(_, a) | Div(_, a) | SetCC(_, a) | Push(a) => vec![a.clone()],
        Binary(x) => vec![x.src.clone(), x.dst.clone()],
        Pop(_) => panic!("Internal error"),
        _ => vec![],
    }
}
pub fn replaceOps<F: Fn(&AsmOperand) -> AsmOperand>(f: &F, i: AsmInstruction) -> AsmInstruction {
    use AsmInstruction::*;
    match i {
        Mov(t, a, b) => Mov(t, f(&a), f(&b)),
        Movsx(mut x) => {
            x.src = f(&x.src);
            x.dst = f(&x.dst);
            Movsx(x)
        }
        MovZeroExtend(mut x) => {
            x.src = f(&x.src);
            x.dst = f(&x.dst);
            MovZeroExtend(x)
        }
        Lea(a, b) => Lea(f(&a), f(&b)),
        Cvttsd2si(t, a, b) => Cvttsd2si(t, f(&a), f(&b)),
        Cvtsi2sd(t, a, b) => Cvtsi2sd(t, f(&a), f(&b)),
        Unary(o, t, a) => Unary(o, t, f(&a)),
        Binary(mut x) => {
            x.src = f(&x.src);
            x.dst = f(&x.dst);
            Binary(x)
        }
        Cmp(t, a, b) => Cmp(t, f(&a), f(&b)),
        Idiv(t, a) => Idiv(t, f(&a)),
        Div(t, a) => Div(t, f(&a)),
        SetCC(c, a) => SetCC(c, f(&a)),
        Push(a) => Push(f(&a)),
        Pop(_) => panic!("Shouldn't use this yet"),
        x => x,
    }
}
pub fn cleanupMovs(xs: Vec<AsmInstruction>) -> Vec<AsmInstruction> {
    xs.into_iter()
        .filter(|i| !matches!(i,AsmInstruction::Mov(_,a,b) if a==b))
        .collect()
}
fn reg(r: AsmReg) -> AsmOperand {
    AsmOperand::Reg(r)
}

impl Allocator {
    pub fn new(ops: RegTypeOps) -> Self {
        Self { ops }
    }
    fn hardregs(&self) -> OperandSet {
        self.ops.all_hardregs.iter().copied().map(reg).collect()
    }
    fn regsUsedAndWritten(&self, i: &AsmInstruction) -> (OperandSet, OperandSet) {
        use AsmInstruction::*;
        let callers: Vec<_> = self
            .ops
            .caller_saved_regs
            .iter()
            .copied()
            .map(reg)
            .collect();
        let (used, written): (Vec<AsmOperand>, Vec<AsmOperand>) = match i {
            Mov(_, a, b) => (vec![a.clone()], vec![b.clone()]),
            Movsx(x) => (vec![x.src.clone()], vec![x.dst.clone()]),
            MovZeroExtend(x) => (vec![x.src.clone()], vec![x.dst.clone()]),
            Cvtsi2sd(_, a, b) | Cvttsd2si(_, a, b) => (vec![a.clone()], vec![b.clone()]),
            Binary(x) => (vec![x.src.clone(), x.dst.clone()], vec![x.dst.clone()]),
            Unary(_, _, a) => (vec![a.clone()], vec![a.clone()]),
            Cmp(_, a, b) => (vec![a.clone(), b.clone()], vec![]),
            SetCC(_, a) => (vec![], vec![a.clone()]),
            Push(a) => (vec![a.clone()], vec![]),
            Idiv(_, a) | Div(_, a) => (
                vec![a.clone(), reg(AsmReg::AX), reg(AsmReg::DX)],
                vec![reg(AsmReg::AX), reg(AsmReg::DX)],
            ),
            Cdq(_) => (vec![reg(AsmReg::AX)], vec![reg(AsmReg::DX)]),
            Call(name) => (
                AssemblySymbols::paramRegsUsed(name)
                    .into_iter()
                    .filter(|r| self.ops.all_hardregs.contains(r))
                    .map(reg)
                    .collect(),
                callers,
            ),
            Lea(a, b) => (vec![a.clone()], vec![b.clone()]),
            Pop(_) => panic!("Internal error"),
            _ => (vec![], vec![]),
        };
        fn read(o: &AsmOperand) -> Vec<AsmOperand> {
            match o {
                AsmOperand::Pseudo(_) | AsmOperand::Reg(_) => vec![o.clone()],
                AsmOperand::Memory(r, _) => vec![reg(*r)],
                AsmOperand::Indexed(x) => vec![reg(x.base), reg(x.index)],
                _ => vec![],
            }
        }
        fn update(o: &AsmOperand) -> (Vec<AsmOperand>, Vec<AsmOperand>) {
            match o {
                AsmOperand::Pseudo(_) | AsmOperand::Reg(_) => (vec![], vec![o.clone()]),
                AsmOperand::Memory(r, _) => (vec![reg(*r)], vec![]),
                AsmOperand::Indexed(x) => (vec![reg(x.base), reg(x.index)], vec![]),
                _ => (vec![], vec![]),
            }
        }
        let mut reads: OperandSet = used.iter().flat_map(read).collect();
        let mut writes = OperandSet::new();
        for o in &written {
            let (r, w) = update(o);
            reads.extend(r);
            writes.extend(w)
        }
        (reads, writes)
    }
    fn liveness(
        &self,
        name: &str,
        instructions: Vec<AsmInstruction>,
    ) -> Cfg::ControlFlowGraph<OperandSet, AsmInstruction> {
        let raw = AsmCfg::instructionsToCfg(name, instructions);
        let mut cfg = Cfg::initializeAnnotation(raw, OperandSet::new());
        let returns: OperandSet = AssemblySymbols::returnRegsUsed(name)
            .into_iter()
            .filter(|r| self.ops.all_hardregs.contains(r))
            .map(reg)
            .collect();
        loop {
            let old: Vec<_> = cfg
                .BasicBlocks
                .iter()
                .map(|(_, b)| b.value.clone())
                .collect();
            for idx in (0..cfg.BasicBlocks.len()).rev() {
                let end =
                    cfg.BasicBlocks[idx]
                        .1
                        .succs
                        .iter()
                        .fold(OperandSet::new(), |mut s, n| {
                            match n {
                                Cfg::NodeId::Exit => s.extend(returns.clone()),
                                Cfg::NodeId::Block(j) => {
                                    s.extend(cfg.BasicBlocks[*j].1.value.clone())
                                }
                                Cfg::NodeId::Entry => panic!("Internal error"),
                            };
                            s
                        });
                let mut live = end;
                let mut annotated = Vec::new();
                for (_, ins) in cfg.BasicBlocks[idx].1.instructions.iter().rev() {
                    annotated.push((live.clone(), ins.clone()));
                    let (u, w) = self.regsUsedAndWritten(ins);
                    live.retain(|x| !w.contains(x));
                    live.extend(u)
                }
                annotated.reverse();
                cfg.BasicBlocks[idx].1.instructions = annotated;
                cfg.BasicBlocks[idx].1.value = live
            }
            if old
                .iter()
                .enumerate()
                .all(|(i, x)| x == &cfg.BasicBlocks[i].1.value)
            {
                break;
            }
        }
        cfg
    }
    fn add_edge(g: &mut Graph, a: &AsmOperand, b: &AsmOperand) {
        if a == b {
            return;
        }
        if g.contains_key(a) && g.contains_key(b) {
            g.get_mut(a).unwrap().neighbors.insert(b.clone());
            g.get_mut(b).unwrap().neighbors.insert(a.clone());
        }
    }
    fn buildGraph(
        &self,
        name: &str,
        aliases: &BTreeSet<String>,
        instructions: &[AsmInstruction],
    ) -> Graph {
        let hard = self.hardregs();
        let mut g = Graph::new();
        for r in &hard {
            g.insert(
                r.clone(),
                AllocNode {
                    id: r.clone(),
                    neighbors: hard.iter().filter(|x| *x != r).cloned().collect(),
                    spillCost: f64::INFINITY,
                    color: None,
                    pruned: false,
                },
            );
        }
        for p in instructions.iter().flat_map(getOperands).filter_map(|o| {
            if let AsmOperand::Pseudo(p) = o {
                Some(p)
            } else {
                None
            }
        }) {
            if (self.ops.pseudo_is_current_type)(&p)
                && !AssemblySymbols::isStatic(&p)
                && !aliases.contains(&p)
            {
                g.entry(AsmOperand::Pseudo(p.clone())).or_insert(AllocNode {
                    id: AsmOperand::Pseudo(p),
                    neighbors: BTreeSet::new(),
                    spillCost: 0.0,
                    color: None,
                    pruned: false,
                });
            }
        }
        let live = self.liveness(name, instructions.to_vec());
        for (_, b) in live.BasicBlocks {
            for (after, i) in b.instructions {
                let (_, writes) = self.regsUsedAndWritten(&i);
                for l in after {
                    if matches!(&i,AsmInstruction::Mov(_,src,_) if src==&l) {
                        continue;
                    }
                    for w in &writes {
                        Self::add_edge(&mut g, &l, w)
                    }
                }
            }
        }
        g
    }
    fn degree(g: &Graph, n: &AsmOperand) -> usize {
        g[n].neighbors.len()
    }
    fn coalescable(&self, g: &Graph, a: &AsmOperand, b: &AsmOperand) -> bool {
        let k = self.ops.all_hardregs.len();
        let union: OperandSet = g[a].neighbors.union(&g[b].neighbors).cloned().collect();
        let briggs = union
            .iter()
            .filter(|n| {
                let mut d = Self::degree(g, n);
                if g[a].neighbors.contains(n) && g[b].neighbors.contains(n) {
                    d -= 1
                }
                d >= k
            })
            .count()
            < k;
        if briggs {
            return true;
        }
        let (h, p) = match (a, b) {
            (AsmOperand::Reg(_), _) => (a, b),
            (_, AsmOperand::Reg(_)) => (b, a),
            _ => return false,
        };
        g[p].neighbors
            .iter()
            .all(|n| g[n].neighbors.contains(h) || Self::degree(g, n) < k)
    }
    fn coalesce(
        &self,
        mut g: Graph,
        instructions: &[AsmInstruction],
    ) -> (Graph, DisjointSets::DisjointSet<AsmOperand>) {
        let mut sets = DisjointSets::init();
        for i in instructions {
            if let AsmInstruction::Mov(_, a, b) = i {
                let a = DisjointSets::find(a, &sets);
                let b = DisjointSets::find(b, &sets);
                if a != b
                    && g.contains_key(&a)
                    && g.contains_key(&b)
                    && !g[&a].neighbors.contains(&b)
                    && self.coalescable(&g, &a, &b)
                {
                    let (merge, keep) = if matches!(a, AsmOperand::Reg(_)) {
                        (b, a)
                    } else {
                        (a, b)
                    };
                    let neighbors = g[&merge].neighbors.clone();
                    for n in neighbors {
                        Self::add_edge(&mut g, &n, &keep);
                        if let Some(x) = g.get_mut(&n) {
                            x.neighbors.remove(&merge);
                        }
                    }
                    g.remove(&merge);
                    DisjointSets::union(merge, keep, &mut sets)
                }
            }
        }
        (g, sets)
    }
    fn color(&self, mut g: Graph) -> Graph {
        let k = self.ops.all_hardregs.len();
        let mut stack = Vec::new();
        while stack.len() < g.len() {
            let left: Vec<_> = g
                .values()
                .filter(|n| !n.pruned)
                .map(|n| n.id.clone())
                .collect();
            let low = left
                .iter()
                .find(|n| g[n].neighbors.iter().filter(|x| !g[*x].pruned).count() < k)
                .cloned();
            let n = low.unwrap_or_else(|| {
                left.into_iter()
                    .min_by(|a, b| {
                        let da = g[a].neighbors.iter().filter(|x| !g[*x].pruned).count();
                        let db = g[b].neighbors.iter().filter(|x| !g[*x].pruned).count();
                        (g[a].spillCost / da as f64).total_cmp(&(g[b].spillCost / db as f64))
                    })
                    .unwrap()
            });
            g.get_mut(&n).unwrap().pruned = true;
            stack.push(n)
        }
        while let Some(n) = stack.pop() {
            let used: BTreeSet<_> = g[&n].neighbors.iter().filter_map(|x| g[x].color).collect();
            let mut avail: Vec<_> = (0..k).filter(|c| !used.contains(c)).collect();
            if let Some(c) = if matches!(&n,AsmOperand::Reg(r) if !self.ops.caller_saved_regs.contains(r))
            {
                avail.pop()
            } else {
                avail.first().copied()
            } {
                let x = g.get_mut(&n).unwrap();
                x.color = Some(c);
                x.pruned = false
            }
        }
        g
    }
    pub fn allocate(
        &self,
        name: &str,
        aliases: &BTreeSet<String>,
        mut instructions: Vec<AsmInstruction>,
    ) -> Vec<AsmInstruction> {
        let graph = loop {
            let g = self.buildGraph(name, aliases, &instructions);
            let (g, sets) = self.coalesce(g, &instructions);
            if DisjointSets::is_empty(&sets) {
                break g;
            }
            instructions = instructions
                .into_iter()
                .filter_map(|i| {
                    if let AsmInstruction::Mov(t, a, b) = i {
                        let a = DisjointSets::find(&a, &sets);
                        let b = DisjointSets::find(&b, &sets);
                        if a == b {
                            None
                        } else {
                            Some(AsmInstruction::Mov(t, a, b))
                        }
                    } else {
                        Some(replaceOps(&|o| DisjointSets::find(o, &sets), i))
                    }
                })
                .collect()
        };
        let mut graph = graph;
        let mut counts: BTreeMap<String, usize> = BTreeMap::new();
        for p in instructions.iter().flat_map(getOperands) {
            if let AsmOperand::Pseudo(p) = p {
                *counts.entry(p).or_default() += 1
            }
        }
        for n in graph.values_mut() {
            if let AsmOperand::Pseudo(p) = &n.id {
                n.spillCost = *counts.get(p).unwrap_or(&0) as f64
            }
        }
        let graph = self.color(graph);
        let mut colors = BTreeMap::new();
        for n in graph.values() {
            if let (AsmOperand::Reg(r), Some(c)) = (&n.id, n.color) {
                colors.insert(c, *r);
            }
        }
        let mut map = BTreeMap::new();
        let mut saved = BTreeSet::new();
        for n in graph.values() {
            if let (AsmOperand::Pseudo(p), Some(c)) = (&n.id, n.color) {
                let r = colors[&c];
                map.insert(p.clone(), r);
                if !self.ops.caller_saved_regs.contains(&r) {
                    saved.insert(r);
                }
            }
        }
        AssemblySymbols::addCalleeSavedRegsUsed(name, &saved);
        cleanupMovs(
            instructions
                .into_iter()
                .map(|i| {
                    replaceOps(
                        &|o| match o {
                            AsmOperand::Pseudo(p) => {
                                map.get(p).copied().map(reg).unwrap_or_else(|| o.clone())
                            }
                            _ => o.clone(),
                        },
                        i,
                    )
                })
                .collect(),
        )
    }
}
fn gp_type(p: &str) -> bool {
    AssemblySymbols::getType(p) != AsmType::Double
}
fn xmm_type(p: &str) -> bool {
    AssemblySymbols::getType(p) == AsmType::Double
}
pub fn allocateRegisters(debug: bool, aliases: &BTreeSet<String>, p: AsmProgram) -> AsmProgram {
    DEBUG.store(debug, Ordering::Relaxed);
    use AsmReg::*;
    let gp = Allocator::new(RegTypeOps {
        suffix: "gp",
        all_hardregs: vec![AX, BX, CX, DX, DI, SI, R8, R9, R12, R13, R14, R15],
        caller_saved_regs: vec![AX, CX, DX, DI, SI, R8, R9],
        pseudo_is_current_type: gp_type,
    });
    let xmm = Allocator::new(RegTypeOps {
        suffix: "xmm",
        all_hardregs: vec![
            XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13,
        ],
        caller_saved_regs: vec![
            XMM0, XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7, XMM8, XMM9, XMM10, XMM11, XMM12, XMM13,
        ],
        pseudo_is_current_type: xmm_type,
    });
    let AsmProgram::Program(t) = p;
    AsmProgram::Program(
        t.into_iter()
            .map(|x| match x {
                AsmTopLevel::Function(mut f) => {
                    f.instructions = gp.allocate(&f.name, aliases, f.instructions);
                    f.instructions = xmm.allocate(&f.name, aliases, f.instructions);
                    AsmTopLevel::Function(f)
                }
                x => x,
            })
            .collect(),
    )
}
