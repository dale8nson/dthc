use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
    token_stream::IntoIter,
};
use std::{
    collections::{BTreeMap, BTreeSet, HashSet},
    error::Error,
    fmt::{self, Display, Error as FmtError, Formatter},
    iter::Peekable,
    str::{Chars, FromStr},
    vec::Vec,
};

struct StringLexer<'a> {
    iter: Chars<'a>,
    pk: Option<Option<char>>,
}

impl<'a> StringLexer<'a> {
    pub fn new(string: &'a str) -> Self {
        let iter = string.chars();
        Self { iter, pk: None }
    }

    pub fn next(&mut self) -> Option<char> {
        match self.pk.take() {
            Some(v) => v,
            None => self.iter.next(),
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        let iter = &mut self.iter;
        self.pk.get_or_insert_with(|| iter.next()).as_ref()
    }

    pub fn skip_ws(&mut self) {
        while let Some(ch) = self.peek().cloned()
            && ch.is_ascii_whitespace()
        {
            self.iter.next();
        }
    }

    pub fn take_while<P>(&mut self, mut predicate: P) -> String
    where
        Self: Sized,
        P: FnMut(&char) -> bool,
    {
        let mut string = String::new();
        match self.pk {
            Some(v) => {
                if predicate(&v.unwrap()) {
                    string.push(v.unwrap())
                }
            }
            None => {}
        }
        let chars = &mut self.iter;
        string.push_str(chars.take_while(predicate).collect::<String>().as_str());
        string
    }
}

fn tokenize_str(string: &str) -> TokenStream {
    let mut ts = TokenStream::new();
    let mut delimiter_stack = Vec::<Delimiter>::new();
    let mut lexer = StringLexer::new(string);
    // lexer.skip_ws();
    let mut tt_stack = Vec::<Vec<TokenTree>>::from([Vec::<TokenTree>::new()]);

    while let Some(ch) = lexer.peek().cloned() {
        // println!("ch: {ch}");
        match ch {
            '(' | '[' | '{' => {
                // let mut group_str = lexer.take_while(|c| *c != closing_delim);
                // group_str.push(closing_delim);
                // println!("group_str: {group_str}");
                // println!("next char after {ch}: {:?}", lexer.peek().clone());
                let delim = match ch {
                    '(' => Delimiter::Parenthesis,
                    '[' => Delimiter::Bracket,
                    '{' => Delimiter::Brace,
                    _ => Delimiter::None,
                };
                delimiter_stack.push(delim);
                tt_stack.push(Vec::<TokenTree>::new());
                lexer.next();
                // ts.extend([TokenTree::Group(Group::new(
                //     delim,
                //     tokenize_str(group_str.as_str()),
                // ))]);
            }
            ')' | ']' | '}' => {
                let closing_delim = match ch {
                    ')' => Delimiter::Parenthesis,
                    ']' => Delimiter::Bracket,
                    '}' => Delimiter::Brace,
                    _ => Delimiter::None,
                };
                if let Some(delim) = delimiter_stack.pop() {
                    if delim == closing_delim {
                        let ts_ = TokenStream::from_iter(
                            tt_stack.pop().unwrap_or_else(|| panic!("missing tokens")),
                        );
                        tt_stack
                            .last_mut()
                            .unwrap()
                            .extend([TokenTree::Group(Group::new(delim, ts_))]);
                        println!("ts: {}", ts.to_string());
                        lexer.next();
                    } else {
                        panic!("delimiter mismatch: {ch}");
                    }
                }
            }
            // 'b' | 'r' => {
            //     todo!()
            // }
            'A'..='Z' | 'a'..='z' | '_' => {
                let mut string = String::new();
                string.push(lexer.next().unwrap());

                while let Some(pk) = lexer.peek().cloned() {
                    if pk.is_alphanumeric() || pk == '_' {
                        string.push(lexer.next().unwrap());
                    } else {
                        break;
                    }
                }
                // println!("string: {string}");
                match string.as_str() {
                    _ => {
                        tt_stack
                            .last_mut()
                            .unwrap()
                            .push(TokenTree::Ident(Ident::new(
                                string.as_str(),
                                Span::call_site(),
                            )));
                    }
                }
            }
            '"' | '\'' => {
                let _quote = lexer.next().unwrap();
                // println!("_quote: {_quote}");
                let mut string_literal = String::new();

                while let Some(pk) = lexer.peek().cloned() {
                    // println!("pk: {pk}");
                    match pk {
                        _q if _q == _quote => {
                            // println!("_q: {_q}");
                            lexer.next();
                            break;
                        }
                        '\\' => {
                            string_literal.push(lexer.next().unwrap());

                            while let Some(esc) = lexer.peek().cloned() {
                                // println!("esc: {esc}");
                                match esc {
                                    'n' | 't' | 'v' | 'r' | 'b' | 'a' | 'f' | '0' | '\\' => {
                                        string_literal.push(lexer.next().unwrap());
                                        break;
                                    }
                                    'u' | 'U' => {
                                        string_literal.push(lexer.next().unwrap());

                                        while let Some(h) = lexer.peek() {
                                            if matches!(h, 'A'..'F' | 'a'..'f' | '0'..'9') {
                                                string_literal.push(lexer.next().unwrap());
                                            }
                                            break;
                                        }
                                    }
                                    _q if _q == _quote => {
                                        string_literal.push(lexer.next().unwrap());
                                        break;
                                    }
                                    _ => {
                                        panic!("Invalid escape sequence: \\{esc}");
                                    }
                                }
                            }
                        }
                        _ if pk.is_alphanumeric() => {
                            string_literal.push(lexer.next().unwrap());
                        }
                        _p if pk.is_ascii_punctuation() => {
                            string_literal.push(lexer.next().unwrap());
                        }
                        ' ' => {
                            string_literal.push(lexer.next().unwrap());
                        }
                        _ => {
                            println!("string_literal: {string_literal}");
                            panic!("Invalid character: {}", ch);
                        }
                    }
                }
                tt_stack
                    .last_mut()
                    .unwrap()
                    .push(TokenTree::Literal(Literal::string(string_literal.as_str())));
                // ts.extend([TokenTree::Literal(Literal::string(string_literal.as_str()))]);
            }
            '0'..='9' | '-' | '+' => {
                let mut number_literal = String::new();
                if matches!(ch, '-' | '+') {
                    // println!("ch: {ch}");
                    let sign = lexer.next().unwrap();
                    if let Some(nxt) = lexer.peek() {
                        // println!("nxt: {nxt}");
                        if !matches!(nxt, '0'..='9') {
                            // println!("next: {nxt}");
                            let spacing = if nxt.is_ascii_punctuation() {
                                Spacing::Joint
                            } else {
                                Spacing::Alone
                            };
                            tt_stack
                                .last_mut()
                                .unwrap()
                                .push(TokenTree::Punct(Punct::new(sign, spacing)));

                            // ts.extend([TokenTree::Punct(Punct::new(sign, spacing))]);
                            // println!("ts: {ts:?}");
                            continue;
                        } else {
                            number_literal.push(sign);
                            // println!("number_literal: {number_literal}");
                        }
                    }
                }
                number_literal.push(lexer.next().unwrap());
                // println!("number_literal: {number_literal}");
                let mut suffix: Option<String> = None;

                while let Some(pk) = lexer.peek().cloned()
                    && pk != ' '
                {
                    match pk {
                        '0'..='9' | '.' | 'e' => {
                            number_literal.push(lexer.next().unwrap());
                            if pk == '0' {
                                if let Some(next_peek) = lexer.peek().cloned() {
                                    match next_peek {
                                        'x' => {
                                            number_literal.push(lexer.next().unwrap());
                                            while let Some(next_peek) = lexer.peek().cloned() {
                                                if matches!(next_peek, '0'..'9' | 'A'..'F' | 'a'..'f' | '_')
                                                {
                                                    number_literal.push(lexer.next().unwrap());
                                                }
                                            }
                                        }
                                        'b' => {
                                            number_literal.push(lexer.next().unwrap());
                                            while let Some(next_peek) = lexer.peek().cloned() {
                                                if matches!(next_peek, '0' | '1' | '_') {
                                                    number_literal.push(lexer.next().unwrap());
                                                }
                                            }
                                        }
                                        'o' => {
                                            number_literal.push(lexer.next().unwrap());
                                            while let Some(next_peek) = lexer.peek().cloned() {
                                                if matches!(next_peek, '0'..'7' | '_') {
                                                    number_literal.push(lexer.next().unwrap());
                                                }
                                            }
                                        }
                                        _ => {
                                            number_literal.push(lexer.next().unwrap());
                                            panic!("invalid number literal: {number_literal}");
                                        }
                                    }
                                }
                            }
                            // println!("number_literal: {number_literal}");
                        }
                        'f' => {
                            let mut suffix_str = String::new();
                            suffix_str.push(lexer.next().unwrap());

                            while let Some(s) = lexer.peek().cloned()
                                && s.is_alphanumeric()
                            {
                                suffix_str.push(lexer.next().unwrap());
                            }
                            suffix = Some(suffix_str);
                        }
                        'i' | 'u' => {
                            // println!("number_literal 125: {number_literal}");
                            let mut suffix_str = String::new();
                            suffix_str.push(lexer.next().unwrap());

                            while let Some(s) = lexer.peek().cloned()
                                && s.is_alphanumeric()
                            {
                                suffix_str.push(s);
                                lexer.next();
                            }
                            // println!("suffix_str: {suffix_str}");
                            suffix = Some(suffix_str);
                        }

                        _ => break,
                    }
                }
                // println!("number_literal: {number_literal:?}");
                let tt = match suffix {
                    Some(suf) => match suf.as_str() {
                        "u8" => TokenTree::Literal(Literal::u8_suffixed(
                            u8::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "u16" => TokenTree::Literal(Literal::u16_suffixed(
                            u16::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "u32" => TokenTree::Literal(Literal::u32_suffixed(
                            u32::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "u64" => TokenTree::Literal(Literal::u64_suffixed(
                            u64::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "u128" => TokenTree::Literal(Literal::u128_suffixed(
                            u128::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "i8" => {
                            let lit = Literal::i8_suffixed(
                                i8::from_str(number_literal.as_str()).unwrap(),
                            );
                            // println!("lit: {:?}", &lit);

                            let tt = TokenTree::Literal(lit);
                            // println!("tt: {:?}", &tt);
                            tt
                        }
                        "i16" => TokenTree::Literal(Literal::i16_suffixed(
                            i16::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "i32" => TokenTree::Literal(Literal::i32_suffixed(
                            i32::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "i64" => TokenTree::Literal(Literal::i64_suffixed(
                            i64::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "i128" => TokenTree::Literal(Literal::i128_suffixed(
                            i128::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "f32" => TokenTree::Literal(Literal::f32_suffixed(
                            f32::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "f64" => TokenTree::Literal(Literal::f64_suffixed(
                            f64::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "usize" => TokenTree::Literal(Literal::usize_suffixed(
                            usize::from_str(number_literal.as_str()).unwrap(),
                        )),
                        "isize" => TokenTree::Literal(Literal::isize_suffixed(
                            isize::from_str(number_literal.as_str()).unwrap(),
                        )),
                        _ => panic!("unknown suffix: {}", suf),
                    },
                    None => TokenTree::Literal(Literal::from_str(number_literal.as_str()).unwrap()),
                };
                // println!("tt: {:?}", &tt);
                tt_stack.last_mut().unwrap().push(tt);
                // ts.extend([tt]);
                // println!("ts: {ts:?}");
            }
            _ if ch.is_ascii_punctuation() => {
                let mut punct = lexer.next().unwrap();

                while let Some(c) = lexer.peek().cloned()
                    && c.is_ascii_punctuation()
                {
                    tt_stack
                        .last_mut()
                        .unwrap()
                        .push(TokenTree::Punct(Punct::new(punct, Spacing::Joint)));
                    // ts.extend([TokenTree::Punct(Punct::new(punct, Spacing::Joint))]);
                    lexer.next();
                    punct = c;
                }
                tt_stack
                    .last_mut()
                    .unwrap()
                    .push(TokenTree::Punct(Punct::new(punct, Spacing::Alone)));
                // ts.extend([TokenTree::Punct(Punct::new(punct, Spacing::Alone))]);
            }
            // '\n' => {
            //     tt_stack
            //         .last_mut()
            //         .unwrap()
            //         .push(TokenTree::Punct(Punct::new('\n', Spacing::Alone)));
            // }
            ' ' => {
                lexer.next();
            }
            _ => break,
        }
    }
    ts.extend(tt_stack.pop().unwrap());
    ts
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct T(String);
#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct NT(String);

impl Display for NT {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtError> {
        write!(f, "{}", self.0)?;
        Ok(())
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
struct S(String);

#[derive(Debug)]
struct EOS;

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Symbol {
    T(String),
    NT(String),
    HalfOpenRange(Box<Symbol>, Box<Symbol>),
    OpenRange(Box<Symbol>, Box<Symbol>),
    // S(String),
    Optional(Vec<Vec<Symbol>>),
    Star(Vec<Vec<Symbol>>),
    Plus(Vec<Vec<Symbol>>),
    Branches(Vec<Vec<Symbol>>),
    None,
}

impl Default for Symbol {
    fn default() -> Self {
        Self::None
    }
}

impl ToString for Symbol {
    fn to_string(&self) -> String {
        match self {
            Self::NT(string) => format!("{} ", string.clone()),
            Self::T(string) => format!("{} ", string.clone()),
            Self::HalfOpenRange(min, max) => format!("{}..{}", min.to_string(), max.to_string()),
            Self::OpenRange(min, max) => format!("{}..={}", min.to_string(), max.to_string()),
            Self::Optional(v) => {
                let string = v
                    .iter()
                    .map(|vec| {
                        vec.iter()
                            .map(|s| format!("{} ", s.to_string()))
                            .collect::<Vec<String>>()
                            .join(" ")
                    })
                    .collect::<Vec<String>>()
                    .join(" | ");

                format!(" ({})?", string)
            }
            Self::Star(v) => {
                let string = v
                    .iter()
                    .map(|vec| {
                        vec.iter()
                            .map(|s| format!("{} ", s.to_string()))
                            .collect::<Vec<String>>()
                            .join(" ")
                    })
                    .collect::<Vec<String>>()
                    .join(" | ");

                format!(" ({})*", string)
            }
            Self::Plus(v) => {
                let string = v
                    .iter()
                    .map(|vec| {
                        vec.iter()
                            .map(|s| format!("{} ", s.to_string()))
                            .collect::<Vec<String>>()
                            .join(" ")
                    })
                    .collect::<Vec<String>>()
                    .join(" | ");

                format!(" ({})+", string)
            }
            Self::Branches(v) => {
                let string = v
                    .iter()
                    .map(|vec| {
                        vec.iter()
                            .map(|s| format!("{} ", s.to_string()))
                            .collect::<Vec<String>>()
                            .join(" ")
                    })
                    .collect::<Vec<String>>()
                    .join(" | ");

                format!(" ( {} )", string)
            }
            Self::None => String::new(),
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct P {
    lhs: Symbol,
    rhs: Vec<Vec<Symbol>>,
}

impl P {
    pub fn is_terminal(&self) -> bool {
        self.rhs.len() == 1 && self.rhs[0].len() == 1 && matches!(self.rhs[0][0], Symbol::T(_))
    }
}

impl Display for P {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtError> {
        write!(
            f,
            "{} -> {}",
            self.lhs.to_string(),
            format!(
                "{}\n",
                self.rhs
                    .iter()
                    .map(|rs| rs
                        .iter()
                        .map(|s| s.to_string())
                        .collect::<Vec<String>>()
                        .join(" "))
                    .collect::<Vec<String>>()
                    .join(" | ")
            )
        )?;

        Ok(())
    }
}

#[derive(Clone)]
struct TokenLexer {
    iter: Peekable<IntoIter>,
    peeked: Option<Option<TokenTree>>,
}

impl Iterator for TokenLexer {
    type Item = TokenTree;
    fn next(&mut self) -> Option<TokenTree> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.iter.next(),
        }
    }
}

impl Default for TokenLexer {
    fn default() -> Self {
        Self {
            iter: TokenStream::new().into_iter().peekable(),
            peeked: None,
        }
    }
}

impl TokenLexer {
    pub fn new(ts: TokenStream) -> Self {
        Self {
            iter: ts.into_iter().peekable(),
            peeked: None,
        }
    }

    pub fn peek(&mut self) -> Option<&TokenTree> {
        let iter = &mut self.iter;
        self.peeked.get_or_insert_with(|| iter.next()).as_ref()
    }

    pub fn take(&mut self, n: u64) -> Vec<TokenTree> {
        let mut v = Vec::<TokenTree>::new();
        let mut iter = self.iter.clone();
        for _ in 0..n {
            if let Some(_) = self.peek().cloned() {
                v.push(self.peeked.take().unwrap().unwrap());
            } else if let Some(_) = iter.peek().clone() {
                v.push(iter.next().unwrap());
            }
        }
        self.iter.clone_from(&iter);
        v
    }

    pub fn take_as_string(&mut self) -> Option<String> {
        let mut iter = self.iter.clone();

        let tt = if let Some(_) = self.peek().cloned() {
            self.peeked.take().unwrap()
        } else if let Some(_) = iter.peek() {
            iter.next()
        } else {
            None
        };
        if let Some(tt) = tt {
            let token_string = match tt {
                TokenTree::Ident(ident) => ident.to_string(),
                TokenTree::Punct(_) => {
                    let mut string = String::new();

                    while let Some(TokenTree::Punct(p)) = iter.peek().cloned() {
                        match p.spacing() {
                            Spacing::Joint => {
                                string.push(p.as_char());
                                iter.next();
                            }
                            Spacing::Alone => {
                                string.push(p.as_char());
                                iter.next();
                                break;
                            }
                        }
                    }
                    string
                }
                TokenTree::Literal(lit) => lit.to_string(),
                TokenTree::Group(grp) => {
                    let mut string = String::new();
                    let (delim, closing_delim) = match grp.delimiter() {
                        Delimiter::Parenthesis => ('(', ')'),
                        Delimiter::Bracket => ('[', ']'),
                        Delimiter::Brace => ('{', '}'),
                        Delimiter::None => ('\0', '\0'),
                    };
                    if delim != '\0' {
                        string.push(delim);
                    }
                    self.iter.clone_from(&iter);
                    if let Some(s) = self.take_as_string() {
                        string.push_str(s.as_str());
                    }
                    if closing_delim != '\0' {
                        string.push(closing_delim);
                    }
                    string
                }
            };
            self.iter.clone_from(&iter);
            Some(token_string)
        } else {
            None
        }
    }

    fn parse_op(&mut self) -> String {
        if let Some(TokenTree::Punct(punct)) = self.peek().cloned() {
            self.next();
            let mut s = String::from(punct.as_char().to_string());
            s.push_str(self.parse_op().as_str());
            s
        } else {
            String::new()
        }
    }

    fn parse_rule(&mut self) -> Option<(Vec<Symbol>, Option<EOS>)> {
        let mut rule = Vec::<Symbol>::new();
        while let Some(peek) = self.peek().cloned() {
            // println!("peek: {peek:?}");
            match peek.clone() {
                TokenTree::Ident(ident) => {
                    println!("ident: {ident}");
                    self.next();
                    if let Some(pk) = self.peek().cloned() {
                        if let TokenTree::Punct(punct) = pk
                            && punct.as_char() == '.'
                        {
                            let op = self.parse_op();
                            println!("op: {op}");
                            match op.as_str() {
                                ".." => {
                                    rule.push(Symbol::HalfOpenRange(
                                        Box::<Symbol>::new(Symbol::NT(ident.to_string())),
                                        {
                                            if let Some(pk2) = self.peek().cloned() {
                                                let sym = match pk2 {
                                                    TokenTree::Ident(id) => Box::<Symbol>::new(
                                                        Symbol::NT(id.to_string()),
                                                    ),
                                                    TokenTree::Literal(lit) => Box::<Symbol>::new(
                                                        Symbol::T(lit.to_string()),
                                                    ),
                                                    other => {
                                                        panic!("invalid range: {}..{other}", peek)
                                                    }
                                                };
                                                sym
                                            } else {
                                                Box::<Symbol>::new(Symbol::None)
                                            }
                                        },
                                    ));
                                    self.next();
                                }
                                "..=" => {
                                    rule.push(Symbol::OpenRange(
                                        Box::<Symbol>::new(Symbol::NT(ident.to_string())),
                                        {
                                            if let Some(pk2) = self.peek().cloned() {
                                                let sym = match pk2 {
                                                    TokenTree::Ident(id) => Box::<Symbol>::new(
                                                        Symbol::NT(id.to_string()),
                                                    ),
                                                    TokenTree::Literal(lit) => Box::<Symbol>::new(
                                                        Symbol::T(lit.to_string()),
                                                    ),
                                                    other => {
                                                        panic!("invalid range: {peek}..{other}")
                                                    }
                                                };
                                                sym
                                            } else {
                                                Box::<Symbol>::new(Symbol::None)
                                            }
                                        },
                                    ));
                                    self.next();
                                }
                                _ => {
                                    rule.push(Symbol::NT(ident.to_string()));
                                    return Some((rule, Some(EOS)));
                                }
                            }
                        } else {
                            rule.push(Symbol::NT(ident.to_string()));
                            continue;
                        }
                    }
                    rule.push(Symbol::NT(ident.to_string()));
                    continue;
                }
                TokenTree::Literal(lit) => {
                    self.next();
                    if let Some(pk) = self.peek().cloned() {
                        if let TokenTree::Punct(punct) = pk
                            && punct.as_char() == '.'
                        {
                            let op = self.parse_op();
                            println!("op: {op}");
                            let mut eos = false;
                            match op.as_str() {
                                ".." => {
                                    rule.push(Symbol::HalfOpenRange(
                                        Box::<Symbol>::new(Symbol::T(lit.to_string())),
                                        {
                                            if let Some(pk2) = self.peek().cloned() {
                                                let sym = match pk2 {
                                                    TokenTree::Ident(id) => Box::<Symbol>::new(
                                                        Symbol::NT(id.to_string()),
                                                    ),
                                                    TokenTree::Literal(l) => {
                                                        let lit_string = l.to_string();
                                                        let mut chars = lit_string.chars();
                                                        if chars.any(|c| c.is_numeric()) {
                                                            if let Some(ch) = chars.last() {
                                                                if ch == '.' {
                                                                    eos = true;
                                                                    Box::<Symbol>::new(Symbol::T(
                                                                        lit_string[..lit_string
                                                                            .len()
                                                                            - 1]
                                                                            .to_string(),
                                                                    ))
                                                                } else {
                                                                    Box::<Symbol>::new(Symbol::T(
                                                                        l.to_string(),
                                                                    ))
                                                                }
                                                            } else {
                                                                Box::<Symbol>::new(Symbol::T(
                                                                    l.to_string(),
                                                                ))
                                                            }
                                                        } else {
                                                            Box::<Symbol>::new(Symbol::T(
                                                                l.to_string(),
                                                            ))
                                                        }
                                                    }
                                                    other => {
                                                        panic!("invalid range: {}..{other}", peek)
                                                    }
                                                };
                                                sym
                                            } else {
                                                Box::<Symbol>::new(Symbol::None)
                                            }
                                        },
                                    ));
                                    if eos {
                                        self.next();
                                        return Some((rule, Some(EOS)));
                                    }
                                    self.next();
                                    continue;
                                }
                                "..=" => {
                                    rule.push(Symbol::OpenRange(
                                        Box::<Symbol>::new(Symbol::T(lit.to_string())),
                                        {
                                            if let Some(pk2) = self.peek().cloned() {
                                                let sym = match pk2 {
                                                    TokenTree::Ident(id) => Box::<Symbol>::new(
                                                        Symbol::NT(id.to_string()),
                                                    ),
                                                    TokenTree::Literal(l) => {
                                                        let lit_string = l.to_string();
                                                        let mut chars = lit_string.chars();
                                                        if chars.any(|c| c.is_numeric()) {
                                                            if let Some(ch) = chars.last() {
                                                                if ch == '.' {
                                                                    eos = true;
                                                                    Box::<Symbol>::new(Symbol::T(
                                                                        lit_string[..lit_string
                                                                            .len()
                                                                            - 1]
                                                                            .to_string(),
                                                                    ))
                                                                } else {
                                                                    Box::<Symbol>::new(Symbol::T(
                                                                        l.to_string(),
                                                                    ))
                                                                }
                                                            } else {
                                                                Box::<Symbol>::new(Symbol::T(
                                                                    l.to_string(),
                                                                ))
                                                            }
                                                        } else {
                                                            Box::<Symbol>::new(Symbol::T(
                                                                l.to_string(),
                                                            ))
                                                        }
                                                    }
                                                    other => {
                                                        panic!("invalid range: {}..{other}", peek)
                                                    }
                                                };
                                                sym
                                            } else {
                                                Box::<Symbol>::new(Symbol::None)
                                            }
                                        },
                                    ));
                                    if eos {
                                        self.next();
                                        return Some((rule, Some(EOS)));
                                    }
                                    self.next();
                                    continue;
                                }
                                _ => {
                                    rule.push(Symbol::T(lit.to_string()));
                                    return Some((rule, Some(EOS)));
                                }
                            }
                        } else {
                            rule.push(Symbol::T(lit.to_string()));
                            continue;
                        }
                    }
                    rule.push(Symbol::T(lit.to_string()));
                    continue;
                }
                TokenTree::Punct(punct) => match punct.as_char() {
                    '|' => {
                        self.next();
                        return Some((rule, None));
                    }
                    '.' => {
                        self.next();
                        return Some((rule, Some(EOS)));
                    }

                    other => panic!("630 unexpected token: {other}"),
                },
                TokenTree::Group(grp) => {
                    println!("grp: {grp:#?}");
                    match grp.delimiter() {
                        Delimiter::Parenthesis => {
                            let mut lexer = TokenLexer::new(grp.stream());
                            let rs = lexer.parse_rules();

                            self.next();
                            if let Some(pk) = self.peek().cloned() {
                                match pk {
                                    TokenTree::Punct(punct) => match punct.as_char() {
                                        '?' => rule.push(Symbol::Optional(rs)),
                                        '*' => rule.push(Symbol::Star(rs)),
                                        '+' => rule.push(Symbol::Plus(rs)),
                                        _ => rule.push(Symbol::Branches(rs)),
                                    },
                                    _ => {
                                        rule.push(Symbol::Branches(rs));
                                        continue;
                                    }
                                }
                            }
                        }
                        other => panic!("invalid delimiter: {:?}", other),
                    }
                }
            };
            // println!("rule: {rule:#?}");
            self.next();
        }
        if rule.is_empty() {
            None
        } else {
            Some((rule, None))
        }
    }

    pub fn parse_rules(&mut self) -> Vec<Vec<Symbol>> {
        let mut rules = Vec::<Vec<Symbol>>::new();
        while let Some(_) = self.peek().cloned() {
            // println!("664 {peek:#?}");

            let rule = self.parse_rule();
            println!("816 rule: {rule:#?}");

            if let Some(r) = rule {
                if rules.iter().any(|r_| *r_ == r.0) {
                    eprintln!("Warning: duplicate rule found: {:#?}", r.0);
                }
                rules.push(r.0);

                if let Some(_) = r.1 {
                    break;
                }
            }
        }
        println!("\n\nrules: \n{rules:#?}\n\n");
        rules
    }
}

struct AST {}

struct Enum {
    name: String,
    variants: Vec<Variant>,
}

struct Variant(Vec<String>);

struct Parser {
    grammar: Vec<P>,
    terminals: BTreeMap<String, usize>,
    keywords: Vec<String>,
}

impl Parser {
    pub fn new(grammar: Vec<P>) -> Self {
        Self {
            grammar,
            terminals: BTreeMap::<String, usize>::new(),
            keywords: Vec::<String>::new(),
        }
    }

    pub fn build(mut self) -> Self {
        let terminal_indices = self
            .grammar
            .iter()
            .enumerate()
            .filter(|(_, p)| p.is_terminal())
            .map(|(i, _)| i)
            .collect::<Vec<usize>>();

        let terminals = BTreeMap::<String, usize>::from_iter(
            terminal_indices
                .into_iter()
                .map(|i| (self.grammar[i].rhs[0][0].to_string(), i)),
        );

        self.terminals = terminals;

        let mut iter = self.grammar.iter();

        while let Some(ref p) = iter.next() {
            let rule_name = p.lhs.to_string();
            let alternatives = p.rhs.clone();
        }

        self
    }

    pub fn parse(&self, src: String) {
        let mut lexer = StringLexer::new(&src);
        while let Some(ch) = lexer.peek().cloned() {
            let mut s = String::from_iter([ch]);
            lexer.next();
            let max = self.terminals.keys().max().unwrap().len();
            while !self.terminals.contains_key(&s) {
                if s.len() >= max {
                    panic!("invalid token: {}", s);
                }
                if let Some(next) = lexer.next() {
                    s.push(next);
                }
            }
            let p = &self.grammar[self.terminals.get(&s).unwrap().clone()];
        }
    }
}

#[proc_macro]
pub fn cf(__ts: TokenStream) -> TokenStream {
    let mut __output = TokenStream::new();
    let mut __lexer = TokenLexer::new(__ts);
    let mut clone = __lexer.clone();
    let mut peek = clone.peek();
    let mut ts = BTreeSet::<T>::new();
    let mut nts = BTreeSet::<NT>::new();
    let mut ps = Vec::<P>::new();

    while let Some(__tt) = peek.cloned() {
        // println!("__tt: {:#?}", __tt.span().source_text());
        match __tt {
            TokenTree::Ident(ident) => {
                // println!("rule lhs: {}", ident.to_string());
                // nts.insert(NT(ident.to_string()));
                let lhs = Symbol::NT(ident.to_string());
                __lexer.next();
                if let Some(arrow) = __lexer.take_as_string() {
                    // println!("arrow: {arrow}");
                    if arrow.as_str() != "->" {
                        panic!("expected -> operator, found {}", arrow.as_str());
                    }
                }
                let rhs = __lexer.parse_rules();
                // println!("\n\nrhs: {:#?}\n\n", &rhs);
                let production_rule = P { lhs, rhs };
                if ps.iter().any(|p| *p == production_rule) {
                    eprintln!("Warning: duplicate production rule: {production_rule:?}");
                }
                println!("{production_rule}");
                ps.push(production_rule);
            }

            other => panic!("expected identifier, got {other:?}"),
        }

        clone = __lexer.clone();
        peek = clone.peek();
    }

    // println!("ps: {ps:#?}");
    // ps.iter().for_each(|p| println!("{p}"));
    //

    let string_vec = ps.iter().map(|p| format!("{}", p)).collect::<Vec<String>>();
    let mut tts = string_vec
        .iter()
        .map(|string| tokenize_str(format!("{}", string).as_str()).to_string())
        .collect::<Vec<String>>()
        .join(".\n");
    tts.push('.');
    // println!("tts: {tts}");
    __output.extend([TokenTree::Literal(Literal::string(tts.as_str()))]);

    __output
}
