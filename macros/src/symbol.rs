use std::fmt::{Display, Formatter, Error as FmtError};

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Symbol {
    Compound(Box<Symbol>, Box<Symbol>),
    HalfOpenRange(Box<Symbol>, Box<Symbol>),
    OpenRange(Box<Symbol>, Box<Symbol>),
    List(Vec<Symbol>),
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
            Self::List(list) => list.iter().map(|sym| sym.to_string()).collect::<Vec<String>>().join(""),
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
pub enum Primary {
    LHS(LHS),
    Delimited(Symbol),
    None,
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum InfixOp {
    OpenRange,
    HalfOpenRange,
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Op {
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
            Self::Infix(infix) => match infix {
                InfixOp::OpenRange => "..=",
                InfixOp::HalfOpenRange => "..",
            },
            _ => "",
        };
        write!(f, "{}", string)?;
        Ok(())
    }
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Rep {
    Optional,
    Plus,
    Star,
    None,
}

#[derive(Debug, Hash, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum LHS {
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
pub struct P {
    pub nt: String,
    pub rules: Vec<Symbol>,
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