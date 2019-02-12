use crate::parser::*;

/*
struct Renderer {
    s: String,
    line_length: usize,
    unclosed_brackets: Vec<char>,
    indent: usize,
    line_pos: usize,
    line_unclosed_brackets: usize,
}

#[derive(PartialOrd, Ord, PartialEq, Eq)]
enum Precedence {
    Parens,
    Let,
    FuncApp,
}

impl Renderer {
    fn new_line(&mut self) {
        if self.line_unclosed_brackets > self.unclosed_brackets.len() {
            self.indent += 4;
            self.line_unclosed_brackets = self.unclosed_brackets.len();
        }

        self.s.push('\n');
        for _ in 0..self.indent {
            self.s.push(' ');
        }
        self.line_pos = self.indent;
    }

    fn open_bracket(&mut self, bracket_char: char) {
        self.unclosed_brackets.push(bracket_char);
        self.s.push(bracket_char);
        self.line_pos += 1;
    }

    fn close_bracket(&mut self, bracket_char: char) {
        let expected = unwrap!(self.unclosed_brackets.pop());
        assert_eq!(bracket_char, expected.to_close_bracket());
        self.s.push(bracket_char);
        self.line_pos += 1;
    }

    fn push(&mut self, text: &str) {
        if self.line_pos + text.len() > self.line_length && self.indent + text.len() <= self.line_length  {
            self.new_line();
        }
        self.s.push_str(text);
        self.line_pos += text.len();
    }

    fn render_expr(
        &mut self,
        expr: &Expr,
        precedence: Precedence,
    ) {
        match expr {
            Expr::Let { pat, expr, body, .. } => {
                let needs_new_line = self.line_length < self.line_pos + 5;
                let needs_bracket = needs_new_line || precedence >= Precedence::Let;
                if needs_bracket {
                    self.open_bracket('(');
                }
                if needs_new_line {
                    self.new_line();
                }

                self.push("let ");
                self.render_pat(pat, Precedence::Let);
                self.push(" = ");
                self.render_expr(expr, Precedence::Let);
                self.push(";");

                if needs_new_line {
                    self.new_line();
                }
                if needs_bracket {
                    self.close_bracket(')');
                }
            },
            Expr::Parens(_, expr) => {
                self.open_bracket('(');
                self.render_expr(expr, Precedence::Parens);
                self.close_bracket(')');
            },
            Expr::Var(ident) => {
                self.push(ident.name())
            },
            Expr::UnitTerm(_) => {
                self.push("{}");
            },
            Expr::UnitType(_) => {
                self.push("#{}");
            },
            Expr::PairTerm { head, tail, .. } => {
                self.open_bracket('{');
                let initial_indent = self.indent;
                let initial_s_len = self.s.len();
                'all_on_one_line {
                    let mut tail = tail;
                    self.render_composite_term_elem(head);
                    loop {
                        if self.indent > initial_indent {
                            self.s.truncate(initial_s_len);
                            break 'all_on_one_line;
                        }
                        match tail {
                            Expr::UnitTerm(_) => {
                                self.close_bracket('}');
                                return;
                            },
                            Expr::PairTerm { head, tail } => {
                                self.push(", ");
                                self.render_composite_term_elem(head);
                                tail = tail;
                            },
                            tail => {
                                self.push(", .. ");

                            },
                        }
                    }
                }
            },
            Expr::PairType {
                head: CompositeTypeElem,
                tail: Box<Expr>,
                span: Span,
            },
            Expr::NeverType(Span),
            Expr::EnumType {
                head: CompositeTypeElem,
                tail: Box<Expr>,
                span: Span,
            },
            Expr::NegFuncTerm {
                pat: Box<Pat>,
                body: Box<Expr>,
                span: Span,
            },
            Expr::NegFuncType {
                pat: Box<Pat>,
                body: Box<Expr>,
                span: Span,
            },
            Expr::Number(Ident),
            Expr::String(Ident),
            Expr::EnumLeft {
                elem: CompositeTermElem,
                span: Span,
            },
            Expr::EnumRight {
                expr: Box<Expr>,
                span: Span,
            },
            Expr::EnumFuncTerm {
                pat: Box<CompositePatElem>,
                body: Box<Expr>,
                tail: Box<Expr>,
                span: Span,
            },
            Expr::EnumFuncType {
                pat: Box<CompositePatElem>,
                body: Box<Expr>,
                tail: Box<Expr>,
                span: Span,
            },
            Expr::NeverFunc(Span),
            Expr::App {
                func: Box<Expr>,
                arg: Box<Expr>,
                span: Span,
            },
        }
    }

    fn render_pat(&mut self, pat: &Pat, precedence: Precedence) {
        match pat {
        }
    }
}
*/

