use super::*;
use crate::parser::Expr;

use lsp_types::Diagnostic;

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

pub fn interpret_value(bytes: &[u8], type_: &Expr) -> Expr {
    match type_ {
        Expr::Var(ident) if ident.name() == "String" => {
            debug!("data in mem: {:?}", &bytes[..20]);
            let len = u32::from_le_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]) as usize;
            debug!("got len: {}", len);
            let bytes = &bytes[4..];
            let bytes = &bytes[..len];
            let s = unwrap!(str::from_utf8(bytes));
            debug!("got a string yo!: {:?}", s);
            Expr::String(crate::parser::Ident::new(s, crate::lexer::Span {
                start: crate::lexer::TextPos {
                    line: 0,
                    col: 0,
                    byte: 0,
                },
                end: crate::lexer::TextPos {
                    line: 0,
                    col: 0,
                    byte: 0,
                },
            }))
        },
        _ => panic!("can't interpret that :("),
    }
}

