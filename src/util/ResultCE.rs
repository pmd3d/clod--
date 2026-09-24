//! A direct Rust analogue of the F# result computation-expression builder.

#[derive(Clone, Copy, Debug, Default)]
pub struct ResultBuilder;

impl ResultBuilder {
    pub fn return_<T, E>(&self, value: T) -> Result<T, E> { Ok(value) }
    pub fn return_from<T, E>(&self, result: Result<T, E>) -> Result<T, E> { result }
    pub fn bind<T, U, E>(&self, result: Result<T, E>, f: impl FnOnce(T) -> Result<U, E>) -> Result<U, E> {
        result.and_then(f)
    }
    pub fn zero<E>(&self) -> Result<(), E> { Ok(()) }
    pub fn combine<T, E>(&self, first: Result<(), E>, second: impl FnOnce() -> Result<T, E>) -> Result<T, E> {
        first.and_then(|()| second())
    }
    pub fn delay<T, E, F: FnOnce() -> Result<T, E>>(&self, f: F) -> F { f }
    pub fn run<T, E>(&self, f: impl FnOnce() -> Result<T, E>) -> Result<T, E> { f() }
}

pub const RESULT: ResultBuilder = ResultBuilder;
