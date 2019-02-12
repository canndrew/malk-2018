use super::*;

use crate::lsp::{Range, Diagnostic};

#[derive(Debug, Clone, PartialEq, Fail)]
pub enum LexError {
    #[fail(display = "invalid symbol table")]
    InvalidSymbolTable(InvalidSymbolTableError),
    #[fail(display = "invalid closing bracket")]
    InvalidClosingBracket {
        open_pos: TextPos,
        close_pos: TextPos,
    },
    #[fail(display = "unclosed bracket")]
    UnclosedBracket {
        open_pos: TextPos,
    },
    #[fail(display = "unexpected char")]
    UnexpectedChar {
        pos: TextPos,
        c: char,
    },
    #[fail(display = "unexpected closing bracket")]
    UnexpectedClosingBracket {
        pos: TextPos,
        c: char,
    },
    #[fail(display = "unclosed string")]
    UnclosedString {
        start_pos: TextPos,
    },
    #[fail(display = "invalid escape digit")]
    InvalidEscapeDigit {
        c: char,
        pos: TextPos,
    },
    #[fail(display = "invalid escape code")]
    InvalidEscapeCode {
        code: u32,
        pos: TextPos,
    },
    #[fail(display = "invalid escape char")]
    InvalidEscapeChar {
        c: char,
        pos: TextPos,
    },
    #[fail(display = "invalid unicode escape")]
    InvalidUnicodeEscape {
        pos: TextPos,
    },
    #[fail(display = "invalid unicode escape syntax")]
    InvalidUnicodeEscapeSyntax {
        pos: TextPos,
    },
}

impl From<LexError> for Result<Vec<Diagnostic>, Error> {
    fn from(lex_error: LexError) -> Result<Vec<Diagnostic>, Error> {
        match lex_error {
            LexError::InvalidSymbolTable(e) => Err(e.into()),
            LexError::InvalidClosingBracket { open_pos, close_pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(close_pos),
                        msg: String::from("invalid closing bracket"),
                    },
                    Diagnostic {
                        range: Range::from(open_pos),
                        msg: String::from("unmatched opening bracket"),
                    },
                ])
            },
            LexError::UnclosedBracket { open_pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(open_pos),
                        msg: String::from("unclosed bracket"),
                    },
                ])
            },
            LexError::UnexpectedChar { pos, c } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: format!("unexpected char {:?}", c),
                    },
                ])
            },
            LexError::UnexpectedClosingBracket { pos, c } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: format!("unexpected closing bracket {:?}", c),
                    },
                ])
            },
            LexError::UnclosedString { start_pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(start_pos),
                        msg: String::from("unclosed string"),
                    }
                ])
            },
            LexError::InvalidEscapeDigit { c, pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: format!("invalid escape digit {:?}", c),
                    }
                ])
            },
            LexError::InvalidEscapeCode { pos, .. } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: String::from("invalid unicode escape code"),
                    }
                ])
            },
            LexError::InvalidEscapeChar { c, pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: format!("invalid escape character {:?}", c),
                    }
                ])
            },
            LexError::InvalidUnicodeEscape { pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: String::from("invalid unicode escape"),
                    }
                ])
            },
            LexError::InvalidUnicodeEscapeSyntax { pos } => {
                Ok(vec![
                    Diagnostic {
                        range: Range::from(pos),
                        msg: String::from("invalid unicode escape syntax"),
                    }
                ])
            },
        }
    }
}

/// The result of a succesful call to sub_lex
struct SubLex<'s> {
    /// The tokens succesfully lexed.
    tokens: TokensBuf<'s>,
    /// If lexing ended by hitting a closing bracket, the bracket and the position after it.
    terminator: Option<(char, TextPos)>,
}

fn sub_lex<'s>(start: TextPos, src: &'s str, symbols: &[&'s str]) -> Result<SubLex<'s>, LexError> {
    let mut tokens = Vec::new();
    let mut pos = start;
    'main: loop {
        let (c, p) = match pos.next(src) {
            Some(x) => x,
            None => return Ok(SubLex {
                tokens: TokensBuf {
                    tokens: tokens,
                    end: pos,
                },
                terminator: None,
            }),
        };
        if c.is_whitespace() {
            let mut end = p;
            loop {
                let (c, p) = match end.next(src) {
                    Some(x) => x,
                    None => break,
                };
                if !c.is_whitespace() {
                    break;
                }
                end = p;
            }

            let token = Token {
                kind: TokenKind::Whitespace(&src[pos.byte..end.byte]),
                start: pos,
            };

            tokens.push(token);
            pos = end;
            continue;
        }
        if c.is_numeric() {
            let mut end = p;
            loop {
                let (c, p) = match end.next(src) {
                    Some(x) => x,
                    None => break,
                };
                if !UnicodeXID::is_xid_continue(c) {
                    break;
                }
                end = p;
            }

            let token = Token {
                kind: TokenKind::Numeric(&src[pos.byte..end.byte]),
                start: pos,
            };

            tokens.push(token);
            pos = end;
            continue;
        }
        if c == '/' {
            match p.next(src) {
                None => (),
                Some((c, p)) => {
                    if c == '/' {
                        let mut end = p;
                        loop {
                            match end.next(src) {
                                Some((c, p)) => {
                                    end = p;
                                    if c == '\n' {
                                        break;
                                    }
                                }
                                None => break,
                            }
                        }
                        /*
                        let token = Token {
                            kind: TokenKind::Comment(&src[p.byte..end.byte]),
                            start: pos,
                        };
                        tokens.push(token);
                        */
                        pos = end;
                        continue;
                    }
                }
            }
        }
        if UnicodeXID::is_xid_start(c) || c == '_' {
            let mut end = p;
            loop {
                let (c, p) = match end.next(src) {
                    Some(x) => x,
                    None => break,
                };
                if !(UnicodeXID::is_xid_continue(c) || c == '_') {
                    break;
                }
                end = p;
            }

            let token = Token {
                kind: TokenKind::Ident(&src[pos.byte..end.byte]),
                start: pos,
            };

            tokens.push(token);
            pos = end;
            continue;
        }
        if c.is_open_bracket() {
            let sub = sub_lex(p, src, symbols)?;
            match sub.terminator {
                Some((term_char, new_end)) => {
                    if term_char == c.to_close_bracket() {
                        let kind = TokenKind::Bracket(c, sub.tokens);
                        let token = Token {
                            kind: kind,
                            start: pos,
                        };
                        tokens.push(token);
                        pos = new_end;
                        continue;
                    }
                    else {
                        return Err(LexError::InvalidClosingBracket {
                            open_pos: pos,
                            close_pos: sub.tokens.end,
                        });
                    }
                },
                None => {
                    return Err(LexError::UnclosedBracket {
                        open_pos: pos,
                    });
                },
            }
        }
        if c.is_close_bracket() {
            return Ok(SubLex {
                tokens: TokensBuf {
                    tokens: tokens,
                    end: pos,
                },
                terminator: Some((c, p)),
            });
        }
        if c == '\'' || c == '"' {
            let token_start = pos;
            let next = |some_pos: TextPos| match some_pos.next(src) {
                Some(x) => Ok(x),
                None => Err(LexError::UnclosedString {
                    start_pos: token_start,
                }),
            };
            let from_hex = |some_char, its_pos| match some_char {
                '0'...'9' => Ok(some_char as u32 - '0' as u32),
                'a'...'f' => Ok(some_char as u32 - 'a' as u32 + 10),
                _ => Err(LexError::InvalidEscapeDigit {
                    c: some_char,
                    pos: its_pos,
                }),
            };
            let from_u32 = |some_u32, esc_pos| match char::from_u32(some_u32) {
                Some(c) => Ok(c),
                None => Err(LexError::InvalidEscapeCode {
                    code: some_u32,
                    pos: esc_pos,
                }),
            };
            let mut owned = None;
            let string_start = p;
            let mut p = p;
            loop {
                let (new_c, new_p) = next(p)?;
                if new_c == c {
                    let cow = match owned {
                        Some(s) => Cow::Owned(s),
                        None => Cow::Borrowed(&src[string_start.byte..p.byte]),
                    };
                    let kind = TokenKind::String(cow);
                    let token = Token {
                        kind: kind,
                        start: pos,
                    };
                    tokens.push(token);
                    pos = new_p;
                    continue 'main;
                }
                if new_c == '\\' {
                    let (esc_c, esc_p) = next(new_p)?;
                    let (unescaped, unescaped_end) = match esc_c {
                        '\'' => ('\'', esc_p),
                        '"'  => ('"',  esc_p),
                        '0'  => ('\0', esc_p),
                        't'  => ('\t', esc_p),
                        'n'  => ('\n', esc_p),
                        'r'  => ('\r', esc_p),
                        '\\' => ('\\', esc_p),
                        'x' => {
                            let (nib0, nib0_end) = next(esc_p)?;
                            let (nib1, nib1_end) = next(nib0_end)?;
                            let nib0 = from_hex(nib0, esc_p)?;
                            let nib1 = from_hex(nib1, nib0_end)?;
                            (from_u32((nib0 << 4) | nib1, p)?, nib1_end)
                        },
                        'u' => {
                            let (open_c, open_p) = next(esc_p)?;
                            if open_c != '{' {
                                return Err(LexError::InvalidUnicodeEscapeSyntax {
                                    pos: esc_p,
                                });
                            }

                            let mut code = 0u32;
                            let mut end = open_p;
                            let mut found_end = false;
                            for _ in 0..6 {
                                let (nib, nib_end) = next(end)?;
                                if nib == '}' {
                                    end = nib_end;
                                    found_end = true;
                                    break;
                                }
                                let nib = from_hex(nib, end)?;
                                code = (code << 4) | nib;
                                end = nib_end;
                            }
                            if !found_end {
                                let (close_c, close_p) = next(end)?;
                                if close_c != '}' {
                                    return Err(LexError::InvalidUnicodeEscape {
                                        pos: p,
                                    });
                                }
                                end = close_p;
                            }
                            (from_u32(code, p)?, end)
                        },
                        _   => {
                            return Err(LexError::InvalidEscapeChar {
                                c: esc_c,
                                pos: new_p,
                            });
                        },
                    };
                    let took = owned.take();
                    let mut s = match took {
                        Some(s) => s,
                        None => String::from(&src[string_start.byte..p.byte]),
                    };
                    s.push(unescaped);
                    owned = Some(s);
                    p = unescaped_end;
                    continue;
                }
                p = new_p;
            }
        }

        let mut sym_end = p;
        loop {
            /*
            let sym_prefix = &src[pos.byte..sym_end.byte];
            let mut seen = false;
            let mut is_symbol = false;
            for this_symbol in symbols {
                if sym_prefix.is_prefix_of(this_symbol) {
                    match seen {
                        true => {
                            is_symbol = false;
                            break;
                        },
                        false => {
                            if this_symbol.len() == sym_prefix.len() {
                                is_symbol = true;
                            }
                            seen = true;
                        },
                    }
                }
            }

            if is_symbol {
                let token = Token {
                    kind: TokenKind::Symbol(sym_prefix),
                    start: pos,
                };
                tokens.push(token);
                pos = sym_end;
                continue 'main;
            };
            */
            let new_p = match sym_end.next(src) {
                Some((c, new_p)) => {
                    if c.is_whitespace() ||
                       UnicodeXID::is_xid_start(c) ||
                       c.is_open_bracket() ||
                       c.is_close_bracket() ||
                       c == '\'' || c == '"' {
                        break;
                    }
                    new_p
                },
                _ => break,
            };
            sym_end = new_p;
        }
        let symbol = &src[pos.byte..sym_end.byte];
        for this_symbol in symbols {
            if this_symbol == &symbol {
                let token = Token {
                    kind: TokenKind::Symbol(symbol),
                    start: pos,
                };
                tokens.push(token);
                pos = sym_end;
                continue 'main;
            }
        }
        return Err(LexError::UnexpectedChar {
            c: c,
            pos: pos,
        });
    }
}

pub fn lex<'s>(src: &'s str, symbols: &[&'s str]) -> Result<TokensBuf<'s>, LexError> {
    match validate_symbol_table(symbols) {
        Ok(()) => (),
        Err(e) => return Err(LexError::InvalidSymbolTable(e)),
    };

    let pos = TextPos::start();
    let sub = sub_lex(pos, src, symbols)?;
    match sub.terminator {
        None => return Ok(sub.tokens),
        Some((c, _)) => return Err(LexError::UnexpectedClosingBracket {
            pos: sub.tokens.end,
            c: c,
        }),
    };
}

