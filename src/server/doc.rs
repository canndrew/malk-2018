use super::*;
use std::sync::Arc;
use crate::parser::{self, Ast};
use crate::core::Term;
use lsp_types::{Diagnostic, Url, TextDocumentContentChangeEvent, Position, Range};

pub struct Doc {
    uri: Arc<Url>,
    text: String,
    //parsed: Option<Ast<Term>>,
    parsed: Ast<Term>,
    pending_update: Option<Update>,
    diagnostics: Vec<Diagnostic>,
}

#[derive(Debug)]
struct Update {
    range: Range,
    text: String,
}

impl Doc {
    pub fn new(uri: Url) -> Doc {
        let uri = Arc::new(uri);
        Doc {
            uri: uri.clone(),
            text: String::new(),
            parsed: Ast {
                node: Box::new(Term::Unit),
                origin: Origin::Document {
                    uri: uri,
                    range: Range {
                        start: Position {
                            line: 0,
                            character: 0,
                        },
                        end: Position {
                            line: 0,
                            character: 0,
                        },
                    },
                },
            },
            pending_update: None,
            diagnostics: Vec::new(),
        }
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    pub fn diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.clone()
    }

    pub fn change_content(&mut self, change: TextDocumentContentChangeEvent) {
        /*
        let parsed_range = match self.parsed.origin {
            Origin::Document { range, .. } => range,
            _ => panic!("wtf"),
        };

        let new_range = match change.range {
            Some(range) => range,
            None => match &self.pending_update {
                None => parsed_range,
                Some(old_update) => {
                    trace!("parsed_range.end == {:?}", parsed_range.end);
                    trace!("old_update.range == {:?}", old_update.range);
                    trace!("old_update.text == {:?}", old_update.text);
                    let new_end = parsed_range.end.adjust_replace(old_update.range, &old_update.text);
                    trace!("new_end == {:?}", new_end);
                    Range {
                        start: parsed_range.start,
                        end: new_end,
                    }
                },
            },
        };
        trace!("new_range == {:?}", new_range);
        let new_update = Update {
            range: new_range,
            text: change.text,
        };







        match &mut self.parsed.origin {
            Origin::Document { range, .. } => {
                range.end = Position { line: 0, character: 8 };
            }
            _ => panic!("wtf"),
        }
        self.text = String::from("aabbccdd");

        let tests = &[
            (5, 7, "CCDD", 1, 3, "AABB", 1, 7, "AABBbcCCDD"),
            (5, 7, "CCDD", 4, 7, "~", 4, 7, "~DD"),
            (5, 7, "CCDD", 4, 10, "~", 4, 8, "~"),
            (5, 7, "CCDD", 6, 8, "EEEE", 5, 7, "CEEEED"),
            (5, 7, "CCDD", 7, 10, "FF", 5, 8, "CCFF"),
            (5, 7, "CCDD", 9, 10, "WOW", 5, 8, "CCDDWOW"),
        ];

        for (i, &(s0, e0, t0, s1, e1, t1, s2, e2, t2)) in tests.into_iter().enumerate() {
            trace!("\n\n\t\tIN TEST {}\n\n", i);

            self.pending_update = Some(Update {
                range: Range {
                    start: Position { line: 0, character: s0 },
                    end: Position { line: 0, character: e0 },
                },
                text: String::from(t0),
            });
            let new_update = Update {
                range: Range {
                    start: Position { line: 0, character: s1 },
                    end: Position { line: 0, character: e1 },
                },
                text: String::from(t1),
            };

            let merged_update = match self.pending_update.take() {
                None => new_update,
                Some(old_update) => {
                    let old_range = old_update.range;
                    let new_range = {
                        trace!("self.text == {:?}", self.text);
                        let orig_text = self.text.lsp_range(old_update.range);
                        trace!("orig_text == {:?}", orig_text);
                        trace!("old_range == {:?}", old_update.range);
                        let old_range_adjust = Range {
                            start: old_update.range.start,
                            end: old_update.range.end.adjust_replace(old_update.range, &old_update.text),
                        };
                        trace!("old_range_adjust == {:?}", old_range_adjust);
                        new_update.range.adjust_replace(old_range_adjust, orig_text)
                    };
                    trace!("new_range (unadjusted) == {:?}", new_range);

                    if new_range.start.before(old_range.start) {
                        trace!("\n\n\t\tWAY COOL!!\n\n");
                        if new_range.end.before(old_range.start) {
                            let inner_text = self.text.lsp_range(Range {
                                start: new_range.end,
                                end: old_range.start,
                            });
                            Update {
                                range: Range {
                                    start: new_range.start,
                                    end: old_range.end,
                                },
                                text: format!("{}{}{}", new_update.text, inner_text, old_update.text),
                            }
                        } else if new_range.end.before(old_range.end) {
                            let old_text_tail = old_update.text.split_from_lsp_pos(new_update.range.end.diff_from(old_range.start));
                            Update {
                                range: Range {
                                    start: new_range.start,
                                    end: old_range.end,
                                },
                                text: format!("{}{}", new_update.text, old_text_tail),
                            }
                        } else {
                            Update {
                                range: Range {
                                    start: new_range.start,
                                    end: new_range.end,
                                },
                                text: new_update.text,
                            }
                        }
                    } else if new_range.start.before(old_range.end) {
                        trace!("\n\n\t\tWOW!\n\n");
                        if new_range.end.before(old_range.end) {
                            let old_text_head = old_update.text.split_to_lsp_pos(new_update.range.start.diff_from(old_range.start));
                            let old_text_tail = old_update.text.split_from_lsp_pos(new_update.range.end.diff_from(old_range.start));
                            trace!("old_text_head == {:?}", old_text_head);
                            trace!("old_text_tail == {:?}", old_text_tail);
                            Update {
                                range: old_range,
                                text: format!("{}{}{}", old_text_head, new_update.text, old_text_tail),
                            }
                        } else {
                            let old_text_head = old_update.text.split_to_lsp_pos(new_update.range.start.diff_from(old_range.start));
                            Update {
                                range: Range {
                                    start: old_range.start,
                                    end: new_range.end,
                                },
                                text: format!("{}{}", old_text_head, new_update.text),
                            }
                        }
                    } else {
                        trace!("\n\n\t\tBANANAS!!\n\n");
                        let inner_text = self.text.lsp_range(Range {
                            start: old_range.end,
                            end: new_range.start,
                        });
                        Update {
                            range: Range {
                                start: old_range.start,
                                end: new_range.end,
                            },
                            text: format!("{}{}{}", old_update.text, inner_text, new_update.text)
                        }
                    }
                },
            };

            trace!("merged_update == {:?}", merged_update);

            assert!(merged_update.text == t2);
            assert!(merged_update.range == Range {
                start: Position { line: 0, character: s2 },
                end: Position { line: 0, character: e2 },
            });
        };

        //trace!("merged_update.text == \n{:#?}\n", merged_update.text);


            //self.pending_update = Some(merged_update);
        /*
        if self.parsed.apply_update(&merged_update, &self.text) {
            self.text = format!(
                "{}{}{}",
                self.text.split_to_lsp_pos(merged_update.range.start),
                merged_update.text,
                self.text.split_from_lsp_pos(merged_update.range.end),
            );
        } else {
            self.pending_update = Some(merged_update);
        }
        */
        */

        match parser::parse_doc(&self.uri, &change.text) {
            Ok(ast) => {
                self.text = change.text;
                self.parsed = ast;
                self.diagnostics = Vec::new();
            },
            Err(e) => {
                self.diagnostics = vec![e];
            },
        }
    }

    pub fn parsed(&self) -> Option<&Ast<Term>> {
        if self.diagnostics.is_empty() {
            Some(&self.parsed)
        } else {
            None
        }
    }

    pub fn highlight(&mut self, position: Position) -> Vec<DocumentHighlight> {
        if self.pending_update.is_some() {
            return Vec::new();
        }

        /*
        if let Some(range) = self.parsed.redex_at_position(position) {
            return vec![DocumentHighlight {
                range,
                kind: Some(DocumentHighlightKind::Write),
            }];
        };
        */

        Vec::new()
    }
}

