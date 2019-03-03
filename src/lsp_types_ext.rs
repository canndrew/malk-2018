use super::*;
use lsp_types::{Range, Position};

pub trait RangeExt {
    fn contains(&self, position: Position) -> bool;
}

impl RangeExt for Range {
    fn contains(&self, position: Position) -> bool {
        !position.before(self.start) && position.before(self.end)
    }
}

trait PositionExt {
    fn before(&self, other: Position) -> bool;
}

impl PositionExt for Position {
    fn before(&self, other: Position) -> bool {
        self.line < other.line || (self.line == other.line && self.character < other.character)
    }
}

pub trait StrExt {
    fn lsp_character_pos_to_byte_pos(&self, n: usize) -> usize;
    fn split_from_lsp_character_pos(&self, n: usize) -> &str;
    fn split_to_lsp_character_pos(&self, n: usize) -> &str;
}

impl StrExt for str {
    fn lsp_character_pos_to_byte_pos(&self, n: usize) -> usize {
        let mut char_indices = self.char_indices();
        let mut width = 0;
        loop {
            if width == n {
                let i = char_indices.next().map(|(i, _)| i).unwrap_or(self.len());
                return i;
            }
            assert!(width < n);
            let (_, c) = unwrap!(char_indices.next());
            let mut units = [0; 2];
            width += c.encode_utf16(&mut units).len();
        }
    }

    fn split_from_lsp_character_pos(&self, n: usize) -> &str {
        let i = self.lsp_character_pos_to_byte_pos(n);
        &self[i..]
    }

    fn split_to_lsp_character_pos(&self, n: usize) -> &str {
        let i = self.lsp_character_pos_to_byte_pos(n);
        &self[..i]
    }
}



