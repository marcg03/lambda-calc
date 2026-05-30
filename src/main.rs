use core::str::Chars;
use std::iter::Peekable;
use std::{io, println, string::String};

#[derive(Debug)]
enum Expr {
    Var(String),
    Lambda(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
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
        match res {
            Ok(expr) => println!("{:?}", expr),
            Err(str) => eprintln!("Failed to parse stdin: {}", str),
        }
    }
}
