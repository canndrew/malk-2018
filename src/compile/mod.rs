use super::*;
use crate::parser::Expr;

use crate::lsp::Diagnostic;

static SYMBOL_TABLE: &[&str] = &[];

pub fn check(code: &str) -> Result<Result<Expr, Vec<Diagnostic>>, Error> {
    let expr = match parser::parse(code) {
        Ok(expr) => expr,
        Err(e) => {
            let err: Result<_, _> = e.into();
            return err.map(Err);
        },
    };

    Ok(Ok(expr))
}

pub fn run(code: &str) -> Result<(), Error> {
    let _expr = parser::parse(code)?;
    //let module = wasm::build_module(&expr);
    //wasm::write_to_file(&module, "/home/shum/malk-out.wasm")?;

    Ok(())
}

