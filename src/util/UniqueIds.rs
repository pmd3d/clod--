//! Deterministic unique-name allocation.
#[derive(Default, Debug, Clone)] pub struct UniqueIds(u64);
impl UniqueIds { pub fn next(&mut self, prefix: &str) -> String { let id=self.0; self.0+=1; format!("{prefix}.{id}") } }
