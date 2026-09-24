//! Find TACKY variables whose addresses are taken.

use std::collections::BTreeSet;

use super::Tacky::{TackyInstruction, TackyProgram, TackyTopLevel, TackyVal};

/// Return every variable used as the source of a `GetAddress` instruction.
pub fn analyze(instructions: &[TackyInstruction]) -> BTreeSet<String> {
    instructions
        .iter()
        .filter_map(|instruction| match instruction {
            TackyInstruction::GetAddress(info) => match &info.src {
                TackyVal::Var(variable) => Some(variable.clone()),
                TackyVal::Constant(_) => None,
            },
            _ => None,
        })
        .collect()
}

/// Find address-taken variables in all function bodies in a program.
///
/// Static variables and constants have no instruction bodies and are ignored.
#[allow(non_snake_case)]
pub fn analyzeProgram(program: &TackyProgram) -> BTreeSet<String> {
    program
        .0
        .iter()
        .filter_map(|top_level| match top_level {
            TackyTopLevel::Function(function) => Some(analyze(&function.body)),
            TackyTopLevel::StaticVariable(_) | TackyTopLevel::StaticConstant(_) => None,
        })
        .fold(BTreeSet::new(), |mut variables, function_variables| {
            variables.extend(function_variables);
            variables
        })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ports::{
        Const::ConstValue,
        Tacky::{TackyFunctionDef, TackySrcDst},
    };

    #[test]
    fn finds_only_variables_whose_addresses_are_taken() {
        let instructions = vec![
            TackyInstruction::GetAddress(TackySrcDst {
                src: TackyVal::Var("addressed".into()),
                dst: TackyVal::Var("pointer".into()),
            }),
            TackyInstruction::GetAddress(TackySrcDst {
                src: TackyVal::Constant(ConstValue::Int(1)),
                dst: TackyVal::Var("other_pointer".into()),
            }),
            TackyInstruction::Copy(TackySrcDst {
                src: TackyVal::Var("copied".into()),
                dst: TackyVal::Var("destination".into()),
            }),
        ];

        assert_eq!(analyze(&instructions), BTreeSet::from(["addressed".into()]));
    }

    #[test]
    fn combines_results_from_every_function() {
        let function = |name: &str, variable: &str| {
            TackyTopLevel::Function(TackyFunctionDef {
                name: name.into(),
                global: false,
                params: Vec::new(),
                body: vec![TackyInstruction::GetAddress(TackySrcDst {
                    src: TackyVal::Var(variable.into()),
                    dst: TackyVal::Var(format!("{variable}_pointer")),
                })],
            })
        };
        let program = TackyProgram(vec![function("first", "a"), function("second", "b")]);

        assert_eq!(analyzeProgram(&program), BTreeSet::from(["a".into(), "b".into()]));
    }
}
