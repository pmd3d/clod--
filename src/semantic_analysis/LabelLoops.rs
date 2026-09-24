//! Assign unique labels to loops and bind `break`/`continue` statements.
#![allow(non_snake_case)]

use super::{Ast::*, UniqueIds};

fn label_statement(counter: usize, current: Option<&str>, statement: Statement) -> (usize, Statement) {
    match statement {
        Statement::Break(_) => (counter, Statement::Break(current.expect("Break outside of loop").into())),
        Statement::Continue(_) => (counter, Statement::Continue(current.expect("Continue outside of loop").into())),
        Statement::While(condition, body, _) => {
            let (counter, id) = UniqueIds::make_label("while", counter);
            let (counter, body) = label_statement(counter, Some(&id), *body);
            (counter, Statement::While(condition, Box::new(body), id))
        }
        Statement::DoWhile(body, condition, _) => {
            let (counter, id) = UniqueIds::make_label("do_while", counter);
            let (counter, body) = label_statement(counter, Some(&id), *body);
            (counter, Statement::DoWhile(Box::new(body), condition, id))
        }
        Statement::For(init, condition, post, body, _) => {
            let (counter, id) = UniqueIds::make_label("for", counter);
            let (counter, body) = label_statement(counter, Some(&id), *body);
            (counter, Statement::For(init, condition, post, Box::new(body), id))
        }
        Statement::Compound(block) => { let (c, b) = label_block(counter, current, block); (c, Statement::Compound(b)) }
        Statement::If(condition, then_clause, else_clause) => {
            let (counter, then_clause) = label_statement(counter, current, *then_clause);
            let (counter, else_clause) = match else_clause {
                Some(e) => { let (c, e) = label_statement(counter, current, *e); (c, Some(Box::new(e))) }
                None => (counter, None),
            };
            (counter, Statement::If(condition, Box::new(then_clause), else_clause))
        }
        other => (counter, other),
    }
}

fn label_block(counter: usize, current: Option<&str>, block: Block) -> (usize, Block) {
    block.0.into_iter().fold((counter, Block(Vec::new())), |(counter, mut out), item| {
        let (counter, item) = match item {
            BlockItem::Stmt(s) => { let (c, s) = label_statement(counter, current, s); (c, BlockItem::Stmt(s)) }
            declaration => (counter, declaration),
        };
        out.0.push(item); (counter, out)
    })
}

/// Port of `labelLoops`: returns the updated unique-id counter and program.
pub fn labelLoops(counter: usize, program: UntypedProgram) -> (usize, UntypedProgram) {
    program.0.into_iter().fold((counter, UntypedProgram(Vec::new())), |(counter, mut out), declaration| {
        let (counter, declaration) = match declaration {
            Declaration::FunDecl(mut function) => if let Some(body) = function.body.take() {
                let (c, body) = label_block(counter, None, body); function.body = Some(body);
                (c, Declaration::FunDecl(function))
            } else { (counter, Declaration::FunDecl(function)) },
            declaration => (counter, declaration),
        };
        out.0.push(declaration); (counter, out)
    })
}

pub fn label_loops(counter: usize, program: UntypedProgram) -> (usize, UntypedProgram) { labelLoops(counter, program) }
