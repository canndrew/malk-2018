use super::*;
use lsp_types::{Range, Position};

pub trait RangeExt {
    fn contains(&self, position: Position) -> bool;
    fn adjust_replace(
        &self,
        remove: Range,
        insert: &str,
    ) -> Range;
}

impl RangeExt for Range {
    fn contains(&self, position: Position) -> bool {
        !position.before(self.start) && position.before(self.end)
    }

    fn adjust_replace(
        &self,
        remove: Range,
        insert: &str,
    ) -> Range {
        Range {
            start: self.start.adjust_replace(remove, insert),
            end: self.end.adjust_replace(remove, insert),
        }
    }
}

pub trait PositionExt {
    fn before(&self, other: Position) -> bool;
    fn diff_from(&self, other: Position) -> Position;
    fn adjust_replace(
        &self,
        remove: Range,
        insert: &str,
    ) -> Position;
}

impl PositionExt for Position {
    fn before(&self, other: Position) -> bool {
        self.line < other.line || (self.line == other.line && self.character < other.character)
    }

    fn diff_from(&self, other: Position) -> Position {
        trace!("diff_from({:?}, {:?})", self, other);
        Position {
            line: self.line - other.line,
            character: {
                if other.line == self.line {
                    self.character - other.character
                } else {
                    other.character
                }
            },
        }
    }

    fn adjust_replace(
        &self,
        remove: Range,
        insert: &str,
    ) -> Position {
        trace!("adjust_replace(
            self == {:?},
            remove == {:?},
            insert == {:?},
        )", self, remove, insert);
        if !remove.start.before(*self) {
            return *self;
        }
        debug_assert!(!self.before(remove.end));

        let mut ret = *self;

        let end_pos = insert.lsp_pos_of_end();

        ret.line += end_pos.line;
        ret.line -= remove.end.line - remove.start.line;

        if self.line == remove.end.line {
            ret.character += end_pos.character;
            ret.character -= remove.end.character;
        }

        ret
    }
}

pub trait StrExt {
    fn lsp_character_pos_to_byte_pos(&self, n: u64) -> usize;
    fn split_from_lsp_pos(&self, pos: Position) -> &str;
    fn split_to_lsp_pos(&self, pos: Position) -> &str;
    fn split_from_lsp_character_pos(&self, n: u64) -> &str;
    fn split_to_lsp_character_pos(&self, n: u64) -> &str;
    fn lsp_pos_to_byte_pos(&self, position: Position) -> usize;
    fn lsp_pos_of_end(&self) -> Position;
    fn lsp_range(&self, range: Range) -> &str;
}

impl StrExt for str {
    fn lsp_character_pos_to_byte_pos(&self, n: u64) -> usize {
        self.lsp_pos_to_byte_pos(Position { line: 0, character: n })
    }

    fn split_from_lsp_pos(&self, pos: Position) -> &str {
        let i = self.lsp_pos_to_byte_pos(pos);
        &self[i..]
    }

    fn split_to_lsp_pos(&self, pos: Position) -> &str {
        let i = self.lsp_pos_to_byte_pos(pos);
        &self[..i]
    }

    fn split_from_lsp_character_pos(&self, n: u64) -> &str {
        let i = self.lsp_character_pos_to_byte_pos(n);
        &self[i..]
    }

    fn split_to_lsp_character_pos(&self, n: u64) -> &str {
        let i = self.lsp_character_pos_to_byte_pos(n);
        &self[..i]
    }

    fn lsp_pos_to_byte_pos(&self, position: Position) -> usize {
        trace!("lsp_pos_to_byte_pos(\n{:#?},\n{:?},\n)", self, position);
        let mut char_indices = self.char_indices().peekable();
        let mut line = 0;
        let mut character = 0;
        loop {
            if line == position.line && character == position.character {
                let ret = char_indices.peek().map(|(i, _)| *i).unwrap_or(self.len());
                return ret;
            }

            if line > position.line || (line == position.line && character > position.character) {
                panic!("invalid position");
            }

            let (_, c) = unwrap!(char_indices.next());
            if c == '\n' {
                line += 1;
                character = 0;
                continue;
            }
            if c == '\r' {
                if let Some((_, '\n')) = char_indices.peek() {
                    continue;
                }
                line += 1;
                character = 0;
                continue;
            }
            character += c.lsp_width();
        }
    }

    fn lsp_pos_of_end(&self) -> Position {
        let mut line = 0;
        let mut character = 0;
        let mut chars = self.chars().peekable();
        while let Some(c) = chars.next() {
            if c == '\n' {
                line += 1;
                character = 0;
                continue;
            }
            if c == '\r' {
                if let Some('\n') = chars.peek() {
                    continue;
                }
                line += 1;
                character = 0;
                continue;
            }
            character += c.lsp_width();
        }
        Position { line, character }
    }

    fn lsp_range(&self, range: Range) -> &str {
        trace!("lsp_range(\n{:#?},\n{:?},\n)", self, range);
        let start = self.lsp_pos_to_byte_pos(range.start);
        let end = start + self[start..].lsp_pos_to_byte_pos(range.end.diff_from(range.start));
        &self[start..end]
    }
}

pub trait CharExt {
    fn lsp_width(&self) -> u64;
}

impl CharExt for char {
    fn lsp_width(&self) -> u64 {
        let mut units = [0; 2];
        self.encode_utf16(&mut units).len() as u64
    }
}


