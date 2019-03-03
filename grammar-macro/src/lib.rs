#![recursion_limit = "256"]

extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2, Span};
use quote::quote;
use std::mem;
use std::fs::File;
use std::io::Read;
use unicode_segmentation::UnicodeSegmentation;

#[derive(Debug)]
struct Rule<'s> {
    name: &'s str,
    ty: syn::Type,
    expr: Expr<'s>,
}

#[derive(Debug)]
enum Expr<'s> {
    Var(&'s str),
    Enum(Vec<Expr<'s>>),
    Map(Box<Expr<'s>>, syn::Expr),
    Seq(Vec<Expr<'s>>),
    Lit(String),
    Bind(syn::Pat, Box<Expr<'s>>),
    Regex(String),
}

#[derive(Debug)]
struct Ruleset<'s> {
    whitespace: Rule<'s>,
    other_rules: Vec<Rule<'s>>,
    imports: Vec<syn::ItemUse>,
}

fn parse_ruleset(mut s: &str) -> Ruleset<'_> {
    let mut imports = Vec::new();
    let mut whitespace_opt = None;
    let mut other_rules = Vec::new();

    loop {
        s = s.trim();
        match s.split_word_bounds().next() {
            Some("use") => {
                let (new_s, import) = get_rust_syntax(s, ';');
                let import = &s[..(import.len() + 1)];
                let import = syn::parse_str(import).unwrap_or_else(|e| panic!("failed to parse `use` statement: {}", e));
                imports.push(import);
                s = new_s;
            },
            _ => break,
        }
    }

    while !s.trim().is_empty() {
        let (new_s, rule) = parse_rule(s);
        s = new_s;
        if rule.name == "$whitespace" {
            if whitespace_opt.is_some() {
                panic!("two $whitespace rules specified");
            }
            whitespace_opt = Some(rule);
        } else {
            other_rules.push(rule);
        }
    }

    let whitespace = whitespace_opt.expect("no $whitespace rule specified");
    Ruleset { whitespace, other_rules, imports }
}

fn get_rust_syntax(mut s: &str, stop: char) -> (&str, &str) {
    let orig = s;
    loop {
        let (c, new_s) = match split_first(s) {
            Some(x) => x,
            None => panic!("expected {:?}", stop),
        };
        s = new_s;
        if c == stop {
            let rust_syntax = &orig[..(orig.len() - s.len() - 1)];
            return (s, rust_syntax);
        }
        if c == '(' {
            let (new_s, _) = get_rust_syntax(s, ')');
            s = new_s;
        }
        if c == '[' {
            let (new_s, _) = get_rust_syntax(s, ']');
            s = new_s;
        }
        if c == '{' {
            let (new_s, _) = get_rust_syntax(s, '}');
            s = new_s;
        }
        if c == '"' {
            let (new_s, _) = parse_stringish(s, '"');
            s = new_s;
        }
        if c == '\'' {
            let (new_s, _) = parse_stringish(s, '\'');
            s = new_s;
        }
        if c == '/' {
            let (c, new_s) = match split_first(s) {
                Some(x) => x,
                None => panic!("expected {:?}", stop),
            };
            if c == '*' {
                s = skip_rust_multiline_comment(new_s);
            }
            if c == '/' {
                s = skip_rest_of_line(new_s);
            }
        }
    }
}

fn skip_rust_multiline_comment(mut s: &str) -> &str {
    loop {
        let (c, new_s) = match split_first(s) {
            Some(x) => x,
            None => panic!("unclosed multi-line rust comment"),
        };
        s = new_s;
        if c == '*' {
            let (c, new_s) = match split_first(s) {
                Some(x) => x,
                None => panic!("unclosed multi-line rust comment"),
            };
            if c == '/' {
                return new_s;
            }
        }
        if c == '/' {
            let (c, new_s) = match split_first(s) {
                Some(x) => x,
                None => panic!("unclosed multi-line rust comment"),
            };
            if c == '*' {
                s = skip_rust_multiline_comment(new_s);
            }
        }
    }
}

fn skip_rest_of_line(mut s: &str) -> &str {
    loop {
        let (c, new_s) = match split_first(s) {
            Some(x) => x,
            None => return "",
        };
        s = new_s;
        if c == '\n' {
            return s;
        }
    }
}

fn parse_rust_syntax<T: syn::parse::Parse>(s: &str, stop: char) -> (&str, T) {
    let (new_s, rust_code) = get_rust_syntax(s, stop);
    let t = syn::parse_str(rust_code).unwrap_or_else(|e| panic!("error parsing rust syntax: {}", e));
    (new_s, t)
}

fn split_first(s: &str) -> Option<(char, &str)> {
    let mut char_indices = s.char_indices();
    let c = match char_indices.next() {
        None => return None,
        Some((_, c)) => c,
    };
    match char_indices.next() {
        None => Some((c, "")),
        Some((i, _)) => Some((c, &s[i..])),
    }
}

fn parse_rule(mut s: &str) -> (&str, Rule<'_>) {
    //println!("parsing rule from {:?}", s);
    let (new_s, name) = parse_ident(s);
    s = match split_first(new_s.trim()) {
        Some((':', new_s)) => new_s,
        _ => panic!("expected a colon after rule name '{}'", name),
    };

    /*
    let mut matches = s.match_indices('=');
    let (new_s, ty) = loop {
        let (eq_pos, _) = matches.next().expect("expected an equals sign");
        let t = &s[..eq_pos];
        match syn::parse_str::<syn::Type>(t) {
            Err(..) => continue,
            Ok(ty) => break (&s[(eq_pos + 1)..], ty),
        }
    };
    */
    let (new_s, ty) = parse_rust_syntax::<syn::Type>(s, '=');
    s = new_s;
    
    let (new_s, expr) = parse_expr(s, ';');
    let rule = Rule { name, ty, expr };
    (new_s, rule)
}

fn parse_stringish(s: &str, stop: char) -> (&str, String) {
    let mut ret = String::new();
    let mut char_indices = s.char_indices();
    let end = loop {
        let (i, c) = char_indices.next().expect("unexpected end of string or regex");
        if c == '\\' {
            let (_, c) = char_indices.next().expect("missing escape code");
            if c == stop {
                ret.push(c);
                continue;
            }
            ret.push('\\');
            ret.push(c);
            continue;
        }
        if c == stop {
            break i;
        }
        ret.push(c);
    };
    let s = &s[(end + 1)..];
    (s, ret)
}

fn parse_expr(mut s: &str, stop: char) -> (&str, Expr<'_>) {
    let mut all_exprs: Vec<Expr<'_>> = Vec::new();
    loop {
        s = s.trim();
        let (c, new_s) = split_first(s).expect("expected an expression");
        let (new_s, expr) = match c {
            c if c == stop => {
                let expr = if all_exprs.len() <= 1 {
                    all_exprs.remove(0)
                } else {
                    Expr::Seq(all_exprs)
                };
                break (new_s, expr);
            }
            '/' => {
                let (new_s, regex) = parse_stringish(new_s, '/');
                let expr = Expr::Regex(regex);
                (new_s, expr)
            },
            '"' => {
                let (new_s, inner) = parse_stringish(new_s, '"');
                let expr = Expr::Lit(inner);
                (new_s, expr)
            },
            '[' => {
                let mut variants = Vec::new();
                let mut brackets_s = new_s.trim();
                loop {
                    let (c, new_s) = split_first(brackets_s).expect("unexpected end of list");
                    if c == ']' {
                        let expr = Expr::Enum(variants);
                        break (new_s, expr);
                    }
                    let (new_s, expr) = parse_expr(brackets_s, ',');
                    brackets_s = new_s.trim();
                    variants.push(expr);
                }
            },
            '(' => {
                let (new_s, pat) = parse_rust_syntax(new_s, ':');
                let (new_s, expr) = parse_expr(new_s, ')');
                let expr = Expr::Bind(pat, Box::new(expr));
                (new_s, expr)
            },
            '=' => {
                let new_s = match split_first(new_s.trim()) {
                    Some(('>', new_s)) => new_s,
                    _ => panic!("unexpected equals sign"),
                };
                let (new_s, rust_expr) = parse_rust_syntax(new_s, stop);
                let mut arg_exprs = mem::replace(&mut all_exprs, Vec::new());
                let expr = if arg_exprs.len() <= 1 {
                    arg_exprs.remove(0)
                } else {
                    Expr::Seq(arg_exprs)
                };
                let expr = Expr::Map(Box::new(expr), rust_expr);
                return (new_s, expr);
            },
            _ => {
                let (new_s, ident) = parse_ident(s);
                let expr = Expr::Var(ident);
                (new_s, expr)
            }
        };
        all_exprs.push(expr);
        s = new_s;
    }
}

fn parse_ident(s: &str) -> (&str, &str) {
    let s = s.trim();
    let mut char_indices = s.char_indices();
    loop {
        let (i, c) = match char_indices.next() {
            Some(x) => x,
            None => break ("", s),
        };
        if c.is_alphanumeric() || c == '_' || c == '$' {
            continue;
        }
        if i == 0 {
            panic!("expected an ident");
        }
        break (&s[i..], &s[..i]);
    }
}

#[proc_macro]
pub fn grammar(code: TokenStream) -> TokenStream {
    let item_mod: syn::ItemMod = match syn::parse(code) {
        Ok(item_mod) => item_mod,
        Err(e) => panic!("expected `mod blah`: {}", e),
    };
    if item_mod.content.is_some() {
        panic!("wrong wrong wrong");
    }
    let mod_name = item_mod.ident;
    let mut name = mod_name.to_string();
    name.push_str(".grm");

    let mut file = File::open(&name).unwrap_or_else(|e| panic!("error opening file {:?}: {}", name, e));
    let mut code = Vec::new();
    file.read_to_end(&mut code).unwrap_or_else(|e| panic!("io error reading file: {}", e));
    let s = String::from_utf8(code).expect("grammar file contains invalid utf8");

    let ruleset = parse_ruleset(&s);

    let mut quoted_rules = Vec::with_capacity(1 + ruleset.other_rules.len());
    for rule in ruleset.other_rules {
        let name = syn::Ident::new(&format!("parse_{}", rule.name), Span::call_site());
        let ty = rule.ty;
        let (expr, _pat, _ty) = quote_expr(rule.expr);
        let quoted_rule = quote! {
            pub fn #name<'u, 'p>(uri: &'u Arc<Url>, p: Parser<'p>) -> Result<(Ast<#ty>, Parser<'p>), Diagnostic> {
                log::trace!("parse_{}({:#?})", stringify!(#name), p.rest());
                #expr
            }
        };
        quoted_rules.push(quoted_rule);
    }
    let (whitespace_expr, _pat, _ty) = quote_expr(ruleset.whitespace.expr);
    let whitespace_rule = quote! {
        pub fn skip_whitespace<'u, 'p>(uri: &'u Arc<Url>, p: Parser<'p>) -> Parser<'p> {
            let res: Result<(Ast<()>, Parser<'p>), Diagnostic> = #whitespace_expr;
            match res {
                Ok((_, p)) => p,
                _ => p,
            }
        }
    };
    quoted_rules.push(whitespace_rule);
    let imports = ruleset.imports;

    let rules = quote! {
        mod #mod_name {
            use std::sync::Arc;
            use super::{Ast, Parser, Origin};
            use lsp_types::{Diagnostic, DiagnosticSeverity, Range, Url};

            #(
                #imports
            )*
            #(
                #quoted_rules
            )*
        }
    };

    //println!("rules == \n{}", rules.to_string());

    rules.into()
}

fn quote_expr(expr: Expr<'_>) -> (TokenStream2, TokenStream2, TokenStream2) {
    match expr {
        Expr::Var(name) => {
            let name = syn::Ident::new(&format!("parse_{}", name), Span::call_site());
            let expr = quote! { #name(uri, p) };
            let pat = quote! { _ };
            let ty = quote! { _ };
            (expr, pat, ty)
        },
        Expr::Enum(variants) => {
            let mut exprs = Vec::new();
            let mut ty_opt = None;
            for variant in variants {
                let (expr, _pat, ty) = quote_expr(variant);
                exprs.push(expr);
                if ty_opt.is_none() {
                    ty_opt = Some(ty);
                }
            }
            let expr = quote! {
                loop {
                    let mut message = String::from("no rule matched: [");
                    let mut need_comma = false;
                    #(
                        match #exprs {
                            Ok(ret) => break Ok(ret),
                            Err(e) => {
                                if need_comma {
                                    message.push_str(", ");
                                }
                                message.push_str(&e.message[..]);
                                need_comma = true;
                            },
                        }
                    )*
                    let diagnostic = Diagnostic {
                        code: None,
                        related_information: None,
                        source: None,
                        range: Range {
                            start: p.position(),
                            end: p.position(),
                        },
                        severity: Some(DiagnosticSeverity::Error),
                        message: message,
                    };
                    break Err(diagnostic);
                }
            };
            let pat = quote! { _ };
            let ty = ty_opt.unwrap_or_else(|| quote! { _ });

            (expr, pat, ty)
        },
        Expr::Map(expr, rust_expr) => {
            let (expr, pat, ty) = quote_expr(*expr);
            let expr = quote! {
                {
                    let start = p.position();
                    (#expr).map(|(#pat, p): (#ty, _)| {
                        let end = p.position();
                        let range = Range { start, end };
                        let uri = uri.clone();
                        let origin = Origin::Document { uri, range };
                        let expr = (#rust_expr, origin).into();
                        (expr, p)
                    })
                }
            };
            let pat = quote! { _ };
            let ty = quote! { _ };
            (expr, pat, ty)
        },
        Expr::Seq(fields) => {
            let mut exprs = Vec::with_capacity(fields.len());
            let mut pats = Vec::with_capacity(fields.len());
            let mut names = Vec::with_capacity(fields.len());
            let mut tys = Vec::with_capacity(fields.len());
            for (i, field) in fields.into_iter().enumerate() {
                let (expr, pat, ty) = quote_expr(field);
                exprs.push(expr);
                pats.push(pat);
                names.push(syn::Ident::new(&format!("e{}", i), Span::call_site()));
                tys.push(ty);
            }
            let names = &names[..];
            let tys= &tys[..];
            let expr = quote! {
                loop {
                    #(
                        let (#names, p): (#tys, _) = match #exprs {
                            Err(e) => break Err(e),
                            Ok(ret) => ret,
                        };
                    )*
                    break Ok(((#(#names,)*), p));
                }
            };
            let pat = quote! {
                (#(#pats,)*)
            };
            let ty = quote! {
                (#(#tys,)*)
            };
            (expr, pat, ty)
        },
        Expr::Lit(lit) => {
            let expr = quote! {
                {
                    use unicode_segmentation::UnicodeSegmentation;
                    let p = skip_whitespace(uri, p);
                    let mut iter = p.rest().split_word_bounds();
                    let start = p.position();
                    let mut seek = #lit;
                    let opt = loop {
                        if seek == "" {
                            let p = p.advance(#lit.len());
                            let end = p.position();
                            let range = Range { start, end };
                            let uri = uri.clone();
                            let origin = Origin::Document { uri, range };
                            break Some((Ast::new((), origin), p));
                        }
                        match iter.next() {
                            None => break None,
                            Some(word) => {
                                if seek.starts_with(word) {
                                    seek = &seek[word.len()..];
                                } else {
                                    break None;
                                }
                            },
                        }
                    };
                    opt.ok_or_else(|| Diagnostic {
                        code: None,
                        related_information: None,
                        source: None,
                        range: Range {
                            start: start,
                            end: start,
                        },
                        severity: Some(DiagnosticSeverity::Error),
                        message: format!("expected \"{}\"", #lit),
                    })
                }
            };
            let pat = quote! { _ };
            let ty = quote! { Ast<()> };
            (expr, pat, ty)
        },
        Expr::Bind(pat, expr) => {
            let (expr, _pat, ty) = quote_expr(*expr);
            let pat = quote! { #pat };
            (expr, pat, ty)
        },
        Expr::Regex(regex) => {
            let expr = quote! {
                {
                    let start = p.position();
                    let regex = regex::Regex::new(#regex).unwrap();
                    let mut iter = regex.splitn(&p.rest(), 2).fuse();
                    let first = iter.next().unwrap_or("");

                    let opt = if first == "" {
                        match iter.next() {
                            None => None,
                            Some(rest) => {
                                let matched_len = p.rest().len() - rest.len();
                                let matched = &p.rest()[..matched_len];
                                let matched = String::from(matched);
                                let p = p.advance(matched_len);
                                let end = p.position();
                                let range = Range { start, end };
                                let uri = uri.clone();
                                let origin = Origin::Document { uri, range };
                                Some((Ast::new(matched, origin), p))
                            },
                        }
                    } else {
                        None
                    };
                    opt.ok_or_else(|| Diagnostic {
                        code: None,
                        related_information: None,
                        source: None,
                        range: Range {
                            start: start,
                            end: start,
                        },
                        severity: Some(DiagnosticSeverity::Error),
                        message: format!("expected /{}/", #regex),
                    })
                }
            };
            let pat = quote! { _ };
            let ty = quote! { Ast<String> };
            (expr, pat, ty)
        },
    }
}

