use std::fmt;

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    Lambda(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lambda(param, body) => write!(f, "(\\{} {})", param, body),
            Expr::App(l, r) => write!(f, "({} {})", l, r),
        }
    }
}
