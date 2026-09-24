//! Top-level TACKY optimization pipeline.
//!
//! The original pipeline does not yet perform any transformations.  It accepts
//! the selected optimization options and source filename for the eventual
//! passes, then returns the program unchanged.

use std::path::Path;

use super::{Settings::Optimizations, Tacky::TackyProgram};

pub fn optimize(
    _options: &Optimizations,
    _source_file: &Path,
    tacky_program: TackyProgram,
) -> TackyProgram {
    tacky_program
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn currently_leaves_the_program_unchanged() {
        let program = TackyProgram(Vec::new());

        assert_eq!(
            optimize(&Optimizations::default(), Path::new("example.c"), program.clone()),
            program
        );
    }
}
