use proc_macro::{
    Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree,
    token_stream::IntoIter,
};
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::{Display, Error as FmtError, Formatter},
    iter::Peekable,
    str::{Chars, FromStr},
    vec::Vec,
};

const OPS: [&str; 5] = ["..", "..=", "|", ".", "->"];

#[derive(Debug)]
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
    // eprintln!("tokenize_str string: {string:?}");
    let mut ts = TokenStream::new();
    let mut delimiter_stack = Vec::<Delimiter>::new();
    let mut lexer = StringLexer::new(string);
    // println!("lexer: {lexer:?}");
    // lexer.skip_ws();
    let mut tt_stack = Vec::<Vec<TokenTree>>::from([Vec::<TokenTree>::new()]);

    while let Some(ch) = lexer.peek().cloned() {
        // print!("{ch}");
        match ch {
            '(' | '[' | '{' => {
                // print!("{ch}");
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
                // print!("{ch}");
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
                        // println!("ts: {}", ts.to_string());
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
                // print!("{ch}");
                let mut string = String::new();
                string.push(lexer.next().unwrap());

                while let Some(pk) = lexer.peek().cloned() {
                    if pk.is_alphanumeric() || pk == '_' {
                        string.push(lexer.next().unwrap());
                    } else {
                        break;
                    }
                }
                // println!("ident: {string}");
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
                // print!("{ch}");
                let _quote = lexer.next().unwrap();
                // println!("_quote: {_quote}");
                let mut string_literal = String::new();

                while let Some(pk) = lexer.peek().cloned()
                // && pk != _quote
                {
                    // println!("\n156 _quote: {_quote}");
                    // print!("157 pk: {pk:#?}");
                    match pk {
                        _q if _q == _quote => {
                            // print!("159 {_q}");
                            break;
                        }
                        '\\' => {
                            string_literal.push(lexer.next().unwrap());

                            if let Some(esc) = lexer.peek().cloned() {
                                // print!("esc {esc}");
                                match esc {
                                    'n' | 't' | 'v' | 'r' | 'b' | 'a' | 'f' | '0' | '\\' | '\n' => {
                                        string_literal.push(lexer.next().unwrap());
                                        // break;
                                    }
                                    'u' | 'U' => {
                                        string_literal.push(lexer.next().unwrap());

                                        while let Some(h) = lexer.peek() {
                                            // print!("{h}");
                                            if matches!(h, 'A'..'F' | 'a'..'f' | '0'..'9') {
                                                string_literal.push(lexer.next().unwrap());
                                            }
                                            // break;
                                        }
                                    }
                                    _q if _q == _quote => {
                                        // print!("185 quote {ch}");
                                        string_literal.push(lexer.next().unwrap());
                                        // break;
                                    }
                                    _ => {
                                        panic!("Invalid escape sequence: \\{esc}");
                                    }
                                }
                            }
                        }
                        an if pk.is_alphanumeric() => {
                            // print!("{an}");
                            string_literal.push(lexer.next().unwrap());
                        }
                        _p if pk.is_ascii_punctuation() => {
                            // print!("{_p}");
                            string_literal.push(lexer.next().unwrap());
                        }
                        ' ' => {
                            // print!(" ");
                            string_literal.push(lexer.next().unwrap());
                            lexer.next();
                        }
                        _ => {
                            // println!("string_literal: {string_literal}");
                            panic!("207 Invalid character: {:#?}", pk);
                        }
                    }
                }
                tt_stack
                    .last_mut()
                    .unwrap()
                    .push(TokenTree::Literal(Literal::string(string_literal.as_str())));
                lexer.next();
                // print!("{}", string_literal.as_str());
            }
            '0'..='9' | '-' | '+' => {
                // println!("{ch}");
                let mut number_literal = String::new();
                if matches!(ch, '-' | '+') {
                    // println!("ch: {ch}");

                    let sign = lexer.next().unwrap();
                    if let Some(pk) = lexer.peek() {
                        // println!("nxt: {nxt}");
                        if !matches!(pk, '0'..='9') {
                            // println!("next: {nxt}");
                            let spacing = if pk.is_ascii_punctuation() {
                                // println!("{pk}");
                                Spacing::Joint
                            } else {
                                // println!("{pk}");
                                Spacing::Alone
                            };
                            // println!("{pk}");
                            tt_stack
                                .last_mut()
                                .unwrap()
                                .push(TokenTree::Punct(Punct::new(sign, spacing)));
                            continue;
                        } else {
                            number_literal.push(sign);
                            number_literal.push(lexer.next().unwrap());
                            // print!("{number_literal}");
                        }
                    }
                }

                // print!("{number_literal}");
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
            _ if ch.is_ascii_punctuation() && !matches!(ch, '"' | '\'') => {
                // print!("{}", ch.clone());
                let mut punct = lexer.next().unwrap();
                let mut op = String::new();
                while let Some(c) = lexer.peek().cloned()
                    && c.is_ascii_punctuation()
                    && !matches!(c, '"' | '\'')
                {
                    op.push(c.clone());
                    // println!("op: {op}");
                    tt_stack
                        .last_mut()
                        .unwrap()
                        .push(TokenTree::Punct(Punct::new(punct, Spacing::Joint)));
                    // print!("{:#?}", lexer.next());
                    
                    punct = lexer.next().unwrap();
                }
                
                tt_stack
                    .last_mut()
                    .unwrap()
                    .push(TokenTree::Punct(Punct::new(punct, Spacing::Alone)));

                
                // print!("{:#?}", lexer.next());
                // print!("{tt_stack:#?}");
            }
            '\n' => {
                // print!("\n");
                lexer.next();
                // tt_stack
                //     .last_mut()
                //     .unwrap()
                //     .push(TokenTree::Punct(Punct::new('\n', Spacing::Alone)));
            }
            ' ' => {
                // print!(" ");
                lexer.next();
            }
            _ => break,
        }
    }
    // println!("tt_stack: {tt_stack:#?}");
    ts.extend(tt_stack.pop().unwrap());
    // println!("token stream: {ts:?}");
    // print!("\n");
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
    Compound(Box<Symbol>, Box<Symbol>),
    HalfOpenRange(Box<Symbol>, Box<Symbol>),
    OpenRange(Box<Symbol>, Box<Symbol>),
    T(String),
    NT(String),
    Optional(Box<Symbol>),
    Star(Box<Symbol>),
    Plus(Box<Symbol>),
    Delimited(Box<Symbol>),
    Error(String),
    EOE,
    EOS,
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
            Self::Compound(lhs, rhs) => format!("{} {}", lhs.to_string(), rhs.to_string()),
            Self::OpenRange(lhs, rhs) => format!("{}..={}", lhs.to_string(), rhs.to_string()),
            Self::HalfOpenRange(lhs, rhs) => format!("{}..{}", lhs.to_string(), rhs.to_string()),
            Self::NT(ident) => format!("{}", ident.clone()),
            Self::T(lit) => format!("{}", lit.clone()),
            Self::Optional(sym) => format!(" ({})?", sym.to_string()),
            Self::Plus(sym) => format!("({})+", sym.to_string()),
            Self::Star(sym) => format!("({})*", sym.to_string()),
            Self::Delimited(sym) => format!(" ({})", sym.to_string()),
            Self::EOE => "| ".to_string(),
            Self::EOS => ".".to_string(),
            Self::None => "".to_string(),
            Self::Error(msg) => format!("\n\nERROR: {}\n\n", msg.clone()),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
enum Primary {
    LHS(LHS),
    Delimited(Symbol),
    None,
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum InfixOp {
    OpenRange,
    HalfOpenRange,
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Op {
    Infix(InfixOp),
    Pipe,
    Stop,
    Arrow,
    None,
}

impl Display for Op {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let string = match self {
            Self::Arrow => "->",
            Self::Pipe => "|",
            Self::Stop => ".",
            Self::Infix(infix) => {
                match infix {
                    InfixOp::OpenRange => "..=",
                    InfixOp::HalfOpenRange => "..",
                }
            }
            _ => "",
        };
        write!(f, "{}", string)?;
        Ok(())
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum Rep {
    Optional,
    Plus,
    Star,
    None,
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
enum LHS {
    Ident(String),
    Literal(String),
    None,
}

impl LHS {
    pub fn symbol(&self) -> Symbol {
        match self {
            Self::Ident(ident) => Symbol::NT(ident.clone()),
            Self::Literal(lit) => Symbol::T(lit.clone()),
            Self::None => Symbol::None,
        }
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
struct P {
    nt: String,
    rules: Vec<Symbol>,
}

impl P {
    pub fn is_terminal(&self) -> bool {
        if self.rules.len() == 1 {
            if let Symbol::T(_) = self.rules[0] {
                true
            } else {
                false
            }
        } else {
            false
        }
    }
}

impl Display for P {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), FmtError> {
        write!(
            f,
            "{} -> {}",
            self.nt,
            self.rules
                .iter()
                .map(|r| r.to_string())
                .collect::<Vec<String>>()
                .join("")
        )?;
        Ok(())
    }
}

#[derive(Clone)]
struct TokenLexer {
    iter: Peekable<IntoIter>,
    peeked: Option<Option<TokenTree>>,
    stack: Vec<Option<Symbol>>,
    eos: bool,
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
            stack: Vec::<Option<Symbol>>::new(),
            eos: false,
        }
    }
}

impl TokenLexer {
    pub fn new(ts: TokenStream) -> Self {
        Self {
            iter: ts.into_iter().peekable(),
            peeked: None,
            stack: Vec::<Option<Symbol>>::new(),
            eos: false,
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

    pub fn push(&mut self, sym: &Option<Symbol>) {
        self.stack.push(sym.clone());
    }

    pub fn pop(&mut self) -> Option<Option<Symbol>> {
        self.stack.pop()
    }

    pub fn eos(&mut self) -> bool {
        std::mem::take(&mut self.eos)
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

    fn encode_op(&mut self) -> Op {
        let mut s = String::new();

        while let Some(TokenTree::Punct(punct)) = self.peek().cloned() {
            if OPS.iter().any(|op| {
                // println!("op: {op:?}");
                let conc = String::from_iter([s.clone(), punct.as_char().to_string()]);
                // println!("conc: {conc:?}");
                let mut op_str = op.to_string();
                if conc.len() <= op.len() {
                    op_str = op[..conc.len()].to_string();
                    // println!("op_str: {op_str:?}");
                }
                conc.len() <= op.len() && conc == op_str
            }) {
                s.push(punct.as_char());
                self.next();
            } else {
                break;
            }
        }

        // println!("s: {s:?}");

        match s.as_str() {
            "->" => Op::Arrow,
            "..=" => Op::Infix(InfixOp::OpenRange),
            ".." => Op::Infix(InfixOp::HalfOpenRange),
            "|" => Op::Pipe,
            "." => Op::Stop,
            _ => Op::None,
        }
    }

    fn encode_lhs(&self, lhs: TokenTree) -> LHS {
        match lhs {
            TokenTree::Ident(ident) => LHS::Ident(ident.to_string()),
            TokenTree::Literal(lit) => LHS::Literal(lit.to_string()),
            _ => {
                // println!("lhs: {lhs:?}");
                LHS::None
            }
        }
    }

    fn encode_delimited(&mut self, grp: Group) -> Symbol {
        let mut lexer = TokenLexer::new(grp.stream());
        let rep = self.encode_rep();
        let sym = lexer.parse_rule();
        // println!("encode_delimited sym: {sym:?}");
        match rep {
            Rep::Optional => Symbol::Compound(Box::new(Symbol::Optional(Box::new(sym))), Box::new(self.parse_rule())),
            Rep::Plus => Symbol::Compound(Box::new(Symbol::Plus(Box::new(sym))), Box::new(self.parse_rule())),
            Rep::Star => Symbol::Compound(Box::new(Symbol::Star(Box::new(sym))), Box::new(self.parse_rule())),
            Rep::None => Symbol::Compound(Box::new(Symbol::Delimited(Box::new(sym))), Box::new(self.parse_rule())),
        }
    }

    fn encode_rep(&mut self) -> Rep {
        let rep = if let Some(TokenTree::Punct(punct)) = self.peek().cloned() {
            match punct.as_char() {
                '?' => Rep::Optional,
                '+' => Rep::Plus,
                '*' => Rep::Star,
                _ => Rep::None,
            }
        } else {
            Rep::None
        };
        if rep != Rep::None {
            self.next();
        }
        rep
    }

    fn encode_primary(&mut self, tt: TokenTree) -> Primary {
        match tt {
            TokenTree::Ident(_) | TokenTree::Literal(_) => {
                let lhs = self.encode_lhs(tt);
                // println!("encode_primary lhs: {lhs:?}");
                Primary::LHS(lhs)
            }
            TokenTree::Group(grp) => Primary::Delimited(self.encode_delimited(grp)),
            _ => Primary::None,
        }
    }

    fn parse_infix(&mut self, lhs: LHS, op: InfixOp) -> Symbol {
        // println!("parse_infix lhs: {lhs:?} op: {op:?}");
        let rhs = self.parse_rule();
        // println!("parse_infix rhs: {rhs:?}");
        let infix_expr = match op {
            InfixOp::OpenRange => Symbol::OpenRange(Box::new(lhs.symbol()), Box::new(rhs)),
            InfixOp::HalfOpenRange => Symbol::HalfOpenRange(Box::new(lhs.symbol()), Box::new(rhs)),
        };

        // println!("parse_infix infix_expr: {:?}", &infix_expr);
        // println!("parse_infix eos: {}", self.eos);
        infix_expr
    }

    fn parse_postfix(&mut self, lhs: LHS, op: Op) -> Symbol {
        // println!("parse_postfix lhs: {lhs:?} op: {op:?}");
        let postfix = match op {
            Op::Pipe => Symbol::Compound(Box::new(lhs.symbol()), Box::new(Symbol::EOE)),
            Op::Stop => {
                self.eos = true;
                Symbol::Compound(Box::new(lhs.symbol()), Box::new(Symbol::EOS))
            }
            Op::None => {
                Symbol::Compound(Box::new(lhs.symbol()), Box::new(self.parse_rule()))
            }
            _ => Symbol::Error(format!("unexpected expression: {} {op}\n Are you missing a full stop? 🤔", lhs.symbol().to_string())),
        };
        // println!("parse_postfix postfix: {postfix:?}");
        postfix
    }

    fn parse_expr(&mut self, lhs: LHS, op: Op) -> Symbol {
        // println!("parse_expr lhs: {lhs:?} op: {op:?}");
        
        let expr = match op {
            Op::Infix(infix_op) => {
                let infix = self.parse_infix(lhs, infix_op);
                // let next = self.parse_rule();
                // println!("parse_expr infix: {:?} next: {:?}", &infix, &next);

                // Symbol::Compound(Box::new(infix), Box::new(next))
                infix
            }
            Op::Pipe => Symbol::Compound(Box::new(lhs.symbol()), Box::new(Symbol::EOE)),
            Op::Stop => {
                self.eos = true;
                Symbol::Compound(Box::new(lhs.symbol()), Box::new(Symbol::EOS))
            }
            _ => self.parse_postfix(lhs, op),
        };

        // println!("parse_expr expr: {expr:?}");
        expr
    }

    fn parse_rule(&mut self) -> Symbol {
        if let Some(tt) = self.peek().cloned() {
            // println!("tt: {tt:?}");
            let token = self.next().unwrap();
            let prim = self.encode_primary(tt);
            match prim {
                Primary::LHS(lhs) => {
                    let op = self.encode_op();
                    // println!("lhs: {lhs:?} op: {op:?}");
                    let expr = self.parse_expr(lhs, op);
                    // println!("parse_rule expr: {:?}", expr);
                    // println!("parse_rule eos: {}", self.eos);
                    return expr;
                }
                Primary::Delimited(sym) => sym,
                Primary::None => Symbol::Error(format!("invalid primary token: {token:?}")),
            }
        } else {
            Symbol::None
        }
    }

    pub fn parse_rules(&mut self) -> Option<Vec<Symbol>> {
        let mut rules = Vec::<Symbol>::new();
        while self.peek().is_some() {
            // println!("parse_rules tt: {tt:?}");
            let rule = self.parse_rule();
            // println!("parse_rules rule: {rule:?}");
            
            rules.push(rule);
            // println!("parse_rules eos: {}", self.eos);
            if self.eos() {
                break;
            }
            
        }
        // println!("parse_rules rules: {rules:?}");
        Some(rules)
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
                .map(|i| (self.grammar[i].nt.clone(), i)),
        );

        self.terminals = terminals;

        let mut iter = self.grammar.iter();

        while let Some(ref p) = iter.next() {
            let rule_name = p.nt.clone();
            let alternatives = p.rules.clone();
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
        }
    }
}

#[proc_macro]
pub fn cf(__ts: TokenStream) -> TokenStream {
    let mut __output = TokenStream::new();
    let mut __lexer = TokenLexer::new(__ts);
    let mut clone = __lexer.clone();
    // let mut peek = clone.peek();
    let mut ts = BTreeSet::<T>::new();
    let mut nts = BTreeSet::<NT>::new();
    let mut ps = Vec::<P>::new();

    while let Some(__tt) = __lexer.peek().cloned() {
        // println!("__tt: {__tt:?}");
        // println!("__tt: {:#?}", __tt.span().source_text());
        match __tt {
            TokenTree::Ident(ident) => {
                let nt = ident.to_string();
                __lexer.next();
                if !matches!(__lexer.encode_op(), Op::Arrow) {
                    panic!("expected -> operator");
                }

                if let Some(rules) = __lexer.parse_rules() {
                    // println!("\n\nrhs: {:#?}\n\n", &rhs);
                    let production_rule = P { nt, rules };
                    if ps.iter().any(|p| *p == production_rule) {
                        eprintln!("Warning: duplicate production rule: {production_rule:?}");
                    }
                    // println!("P: {production_rule}");
                    // println!("P: {production_rule:?}");
                    ps.push(production_rule);
                }
            }
            other => panic!("expected identifier, got {other:?}"),
        }

        // clone = __lexer.clone();
        // peek = clone.peek();
    }

    let string_vec = ps.iter().map(|p| format!("{p}")).collect::<Vec<String>>();
    // println!("string_vec: {string_vec:#?}");

    // println!("{}", tokenize_str(string_vec[0].clone().as_str()));

    let mut tts = string_vec
        .iter()
        .map(|string| tokenize_str(format!("{}", string.as_str()).as_str()).to_string())
        // .into_iter()
        .collect::<Vec<String>>()
        .join("\n");

    tts.push_str("\n👍🏻");

    __output.extend([TokenTree::Literal(Literal::string(tts.as_str()))]);

    // __output.extend([TokenTree::Literal(Literal::string("\n👍🏻"))]);

    __output
}
