use super::*;

#[derive(Debug, Clone, PartialEq)]
pub struct TokensBuf<'s> {
    pub tokens: Vec<Token<'s>>,
    pub end: TextPos,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TokensRef<'t, 's: 't> {
    pub tokens: &'t [Token<'s>],
    pub end: TextPos,
}

impl<'s> TokensBuf<'s> {
    /// Produce a `TokensRef` from this `TokenBuf`
    pub fn borrow<'t>(&'t self) -> TokensRef<'s, 't> {
        TokensRef {
            tokens: &self.tokens[..],
            end: self.end,
        }
    }
}

impl<'t, 's: 't> TokensRef<'t, 's> {
    /// Slice a range of tokens between `start_index` (inclusive) and `end_index` (exclusive).
    pub fn slice(&self, start_index: usize, end_index: usize) -> TokensRef<'t, 's> {
        TokensRef {
            tokens: &self.tokens[start_index..end_index],
            end: self.pos_of(end_index),
        }
    }

    /// Slice a range of tokens between `start_index` and the end.
    pub fn slice_from(&self, start_index: usize) -> TokensRef<'t, 's> {
        TokensRef {
            tokens: &self.tokens[start_index..],
            end: self.end,
        }
    }

    pub fn slice_to(&self, end_index: usize) -> TokensRef<'t, 's> {
        TokensRef {
            tokens: &self.tokens[..end_index],
            end: self.pos_of(end_index),
        }
    }

    /// Split around the token at `index` returning all the tokens before it and all the tokens
    /// after it.
    pub fn split_around(&self, index: usize) -> (TokensRef<'t, 's>, TokensRef<'t, 's>) {
        let l = TokensRef {
            tokens: &self.tokens[..index],
            end: self.tokens[index].start,
        };
        let r = TokensRef {
            tokens: &self.tokens[(index + 1)..],
            end: self.end,
        };
        (l, r)
    }

    pub fn split_symbol<'b>(&self, symbol: &'b str) -> SplitSymbol<'t, 's, 'b> {
        SplitSymbol {
            tokens: Some(self.clone()),
            symbol,
        }
    }

    pub fn split_symbols<'b>(&self, symbols: &'b [&'b str]) -> (TokensRef<'t, 's>, SplitSymbols<'t, 's, 'b>) {
        for (i, token) in self.tokens.iter().enumerate() {
            if let TokenKind::Symbol(s) = token.kind {
                if symbols.contains(&s) {
                    let split_symbols = SplitSymbols {
                        tokens: Some((self.slice_from(i + 1), s)),
                        symbols,
                    };
                    let tokens = self.slice_to(i);
                    return (tokens, split_symbols);
                }
            }
        }
        let split_symbols = SplitSymbols {
            tokens: None,
            symbols,
        };
        (self.clone(), split_symbols)
    }

    pub fn split_symbols_rev<'b>(&self, symbols: &'b [&'b str]) -> (SplitSymbolsRev<'t, 's, 'b>, TokensRef<'t, 's>) {
        for (i, token) in self.tokens.iter().enumerate().rev() {
            if let TokenKind::Symbol(s) = token.kind {
                if symbols.contains(&s) {
                    let split_symbols = SplitSymbolsRev {
                        tokens: Some((self.slice_to(i), s)),
                        symbols,
                    };
                    let tokens = self.slice_from(i + 1);
                    return (split_symbols, tokens);
                }
            }
        }
        let split_symbols = SplitSymbolsRev {
            tokens: None,
            symbols,
        };
        (split_symbols, self.clone())
    }

    pub fn split_next_symbol(&self, symbol: &str) -> Option<(TokensRef<'t, 's>, TokensRef<'t, 's>)> {
        let index_opt = {
            self
            .tokens
            .iter()
            .position(|token| token.kind == TokenKind::Symbol(symbol))
        };
        index_opt.map(|index| self.split_around(index))
    }

    pub fn split_next_symbols(&self, symbols: &[&str])
        -> Option<(TokensRef<'t, 's>, &'s str, TokensRef<'t, 's>)>
    {
        for (i, token) in self.tokens.iter().enumerate() {
            if let TokenKind::Symbol(s) = token.kind {
                if symbols.contains(&s) {
                    return Some((self.slice_to(i), s, self.slice_from(i + 1)));
                }
            }
        }

        None
    }

    /// Trim whitespace tokens from both sides.
    pub fn trim_whitespace(&self) -> TokensRef<'t, 's> {
        let mut start_index = None;
        for (index, token) in self.tokens.iter().enumerate() {
            if !token.is_whitespace() {
                start_index = Some(index);
                break;
            }
        }
        let start_index = match start_index {
            Some(start_index) => start_index,
            None => return TokensRef {
                tokens: &[],
                end: self.end,
            },
        };

        let mut end_index = None;
        for (index, token) in self.tokens.iter().enumerate().rev() {
            if !token.is_whitespace() {
                end_index = Some(index);
                break;
            }
        }
        let end_index = end_index.unwrap() + 1;
        let end_pos = match end_index == self.tokens.len() {
            true  => self.end,
            false => self.tokens[end_index].start,
        };
        TokensRef {
            tokens: &self.tokens[start_index..end_index],
            end: end_pos,
        }
    }

    /// Get the span of this `TokensRef`
    pub fn span(&self) -> Span {
        Span {
            start: match self.tokens.first() {
                None => self.end,
                Some(t) => t.start,
            },
            end: self.end,
        }
    }

    pub fn iter(&self) -> slice::Iter<'t, Token<'s>> {
        self.tokens.iter()
    }

    pub fn pos_of(&self, index: usize) -> TextPos {
        if index == self.tokens.len() {
            return self.end;
        }

        self.tokens[index].start
    }

    pub fn span_of(&self, index: usize) -> Span {
        Span {
            start: self.pos_of(index),
            end: self.pos_of(index + 1),
        }
    }
}

pub struct SplitSymbol<'t, 's: 't, 'b> {
    tokens: Option<TokensRef<'t, 's>>,
    symbol: &'b str,
}

impl<'t, 's: 't, 'b> Iterator for SplitSymbol<'t, 's, 'b> {
    type Item = TokensRef<'t, 's>;

    fn next(&mut self) -> Option<TokensRef<'t, 's>> {
        let tokens = match self.tokens {
            Some(tokens) => tokens,
            None => return None,
        };

        for (i, token) in tokens.iter().enumerate() {
            if token.kind == TokenKind::Symbol(self.symbol) {
                self.tokens = Some(tokens.slice_from(i + 1));
                return Some(tokens.slice_to(i));
            }
        }

        self.tokens = None;
        Some(tokens)
    }
}

impl<'t, 's: 't, 'b> DoubleEndedIterator for SplitSymbol<'t, 's, 'b> {
    fn next_back(&mut self) -> Option<TokensRef<'t, 's>> {
        let tokens = match self.tokens {
            Some(tokens) => tokens,
            None => return None,
        };

        for (i, token) in tokens.iter().enumerate().rev() {
            if token.kind == TokenKind::Symbol(self.symbol) {
                self.tokens = Some(tokens.slice_to(i));
                return Some(tokens.slice_from(i + 1));
            }
        }

        self.tokens = None;
        Some(tokens)
    }
}

pub struct SplitSymbolsRev<'t, 's: 't, 'b> {
    tokens: Option<(TokensRef<'t, 's>, &'s str)>,
    symbols: &'b [&'b str],
}

impl<'t, 's: 't, 'b> Iterator for SplitSymbolsRev<'t, 's, 'b> {
    type Item = (&'s str, TokensRef<'t, 's>);

    fn next(&mut self) -> Option<(&'s str, TokensRef<'t, 's>)> {
        let (tokens, prev_symbol) = match self.tokens {
            Some(tokens) => tokens,
            None => return None,
        };

        for (i, token) in tokens.iter().enumerate().rev() {
            if let TokenKind::Symbol(s) = token.kind {
                if self.symbols.contains(&s) {
                    self.tokens = Some((tokens.slice_to(i), s));
                    return Some((prev_symbol, tokens.slice_from(i + 1)));
                }
            }
        }

        self.tokens = None;
        Some((prev_symbol, tokens))
    }
}

pub struct SplitSymbols<'t, 's: 't, 'b> {
    tokens: Option<(TokensRef<'t, 's>, &'s str)>,
    symbols: &'b [&'b str],
}

impl<'t, 's: 't, 'b> Iterator for SplitSymbols<'t, 's, 'b> {
    type Item = (&'s str, TokensRef<'t, 's>);

    fn next(&mut self) -> Option<(&'s str, TokensRef<'t, 's>)> {
        let (tokens, prev_symbol) = match self.tokens {
            Some(tokens) => tokens,
            None => return None,
        };

        for (i, token) in tokens.iter().enumerate() {
            if let TokenKind::Symbol(s) = token.kind {
                if self.symbols.contains(&s) {
                    self.tokens = Some((tokens.slice_from(i + 1), s));
                    return Some((prev_symbol, tokens.slice_to(i)));
                }
            }
        }

        self.tokens = None;
        Some((prev_symbol, tokens))
    }
}

