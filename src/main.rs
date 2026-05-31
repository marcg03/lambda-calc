use core::str::Chars;
use std::fmt;
use std::iter::Peekable;
use std::{io, println, string::String};

#[derive(Debug, Clone)]
enum Expr {
    Var(String),
    Lambda(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lambda(param, body) => write!(f, "\\{} {}", param, body),
            Expr::App(l, r) => write!(f, "({} {})", l, r),
        }
    }
}

fn try_skip_whitespace(code: &mut Peekable<Chars>) {
    while let Some(char) = code.peek() {
        if char.is_ascii_whitespace() {
            code.next().unwrap();
        } else {
            break;
        }
    }
}

fn try_skip_whitespace_peek<'a>(code: &'a mut Peekable<Chars<'_>>) -> Option<&'a char> {
    try_skip_whitespace(code);
    code.peek()
}

fn parse_var_string(code: &mut Peekable<Chars>) -> Result<String, String> {
    try_skip_whitespace(code);

    let mut var = String::new();
    while let Some(char) = code.peek()
        && char.is_ascii_alphabetic()
    {
        var.push(*char);
        code.next().unwrap();
    }
    if !var.is_empty() {
        Ok(var)
    } else {
        Err("Expected variable".to_string())
    }
}

fn parse_lambda(code: &mut Peekable<Chars>) -> Result<Expr, String> {
    try_skip_whitespace(code);

    match code.next() {
        Some(char) => {
            if char != '\\' {
                return Err("Expected \\".to_string());
            }
        }
        None => return Err("Expected \\".to_string()),
    };
    let param = parse_var_string(code)?;
    let body = parse(code)?;
    Ok(Expr::Lambda(param, Box::new(body)))
}

fn parse(code: &mut Peekable<Chars>) -> Result<Expr, String> {
    let mut opt_prev_expr = None;
    while let Some(char) = try_skip_whitespace_peek(code) {
        let expr = if char.is_ascii_alphabetic() {
            parse_var_string(code).map(|str| Expr::Var(str))
        } else if *char == '\\' {
            parse_lambda(code)
        } else if *char == ')' {
            if let Some(prev_expr) = opt_prev_expr {
                return Ok(prev_expr);
            } else {
                return Err("Didn't expect expr to finish".to_string());
            }
        } else if *char == '(' {
            code.next().unwrap();
            let expr = parse(code);
            if code.next() != Some(')') {
                return Err("Expected )".to_string());
            }
            expr
        } else {
            Err("Unexpected symbol".to_string())
        }?;
        if let Some(prev_expr) = opt_prev_expr {
            let new_expr = Expr::App(Box::new(prev_expr), Box::new(expr));
            opt_prev_expr = Some(new_expr)
        } else {
            opt_prev_expr = Some(expr)
        }
    }
    if let Some(expr) = opt_prev_expr {
        Ok(expr)
    } else {
        Err("Expression not found".to_string())
    }
}

struct Substituer {
    counter: u32,
}

impl Substituer {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    fn fresh_name(&mut self, base: &str) -> String {
        self.counter += 1;
        format!("{}{}", base, self.counter)
    }

    fn is_free(var: &str, expr: &Expr) -> bool {
        match expr {
            Expr::Var(str) => var == str,
            Expr::Lambda(param, body) => var != param && Self::is_free(var, body),
            Expr::App(l, r) => Self::is_free(var, l) || Self::is_free(var, r),
        }
    }

    pub fn substitute(&mut self, var: &str, expr: &Expr, value: &Expr) -> Expr {
        match expr {
            Expr::Var(name) => {
                if name == var {
                    value.clone()
                } else {
                    expr.clone()
                }
            }
            Expr::Lambda(param, body) => {
                if param == var {
                    expr.clone()
                } else if Self::is_free(param, value) {
                    let fresh = self.fresh_name(param);
                    let fresh_body = self.substitute(param, body, &Expr::Var(fresh.clone()));
                    let fresh_lambda = Expr::Lambda(fresh, Box::new(fresh_body));
                    self.substitute(var, &fresh_lambda, value)
                } else {
                    Expr::Lambda(param.clone(), Box::new(self.substitute(var, body, value)))
                }
            }
            Expr::App(l, r) => Expr::App(
                Box::new(self.substitute(var, l, value)),
                Box::new(self.substitute(var, r, value)),
            ),
        }
    }
}

struct Reducer {
    max_steps: u32,
    substituer: Substituer,
}

impl Reducer {
    pub fn new(max_steps: u32) -> Self {
        Self {
            max_steps,
            substituer: Substituer::new(),
        }
    }

    fn reduce_inner(&mut self, start: &Expr) -> Expr {
        if self.max_steps > 0 {
            self.max_steps -= 1;
        } else {
            return start.clone();
        }

        match start {
            Expr::Var(_) => start.clone(),
            Expr::Lambda(param, body) => {
                Expr::Lambda(param.clone(), Box::new(self.reduce_inner(body)))
            }
            Expr::App(l, arg) => {
                let l_reduced = self.reduce_inner(l);
                match l_reduced {
                    Expr::Lambda(param, body) => {
                        let substituted = self.substituer.substitute(&param, &body, &arg);
                        self.reduce_inner(&substituted)
                    }
                    _ => Expr::App(Box::new(l_reduced), Box::new(self.reduce_inner(arg))),
                }
            }
        }
    }

    pub fn reduce(self, start: &Expr) -> Expr {
        let mut reducer = Self::new(self.max_steps);
        reducer.reduce_inner(start)
    }
}

fn main() {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .expect("Expected to read a line");
    let mut code = line.chars().peekable();
    let res = parse(&mut code);
    if code.peek().is_some() {
        eprintln!("Unexpected char");
    } else {
        let reducer = Reducer::new(1024);
        match res {
            Ok(expr) => println!("Reduced expression: {}", reducer.reduce(&expr)),
            Err(str) => eprintln!("Failed to parse stdin: {}", str),
        }
    }
}
