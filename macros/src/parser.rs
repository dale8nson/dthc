use crate::symbol::Symbol;
use std::{collections::BTreeMap, iter::Peekable, str::Chars};

pub struct Parser<'a> {
    input: Option<Peekable<Chars<'a>>>,
    tokens: BTreeMap<String, Symbol>,
}

impl<'a> Parser<'a> {
    pub fn new() -> Self {
        Self {
            input: None,
            tokens: BTreeMap::<String, Symbol>::new(),
        }
    }

    fn get_terminal(&mut self) -> Option<Symbol> {
        let mut string = String::new();
        let input = self.input.as_mut().expect("missing input");
        while let Some(ch) = input.peek().cloned() {
          if ch.is_whitespace() {  input.next(); continue; }
            if self.tokens.contains_key(&ch.to_string()) {
                string.push(input.next().unwrap());
                input.next();
            } else {
                break;
            }
        }

        if string.is_empty() {
            None
        } else {
            self.tokens.get(&string).cloned()
        }
    }

    pub fn parse(&mut self, source: &'a mut String) -> String {
        let chars = source.chars();
        let input = chars.peekable();
        self.input = Some(input);
        let mut output = String::new();

        while let Some(nt) = self.get_terminal() {

        }

        output
    }
}
