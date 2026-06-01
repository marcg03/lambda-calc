use crate::Expr;
use std::iter::Peekable;
use std::str::Chars;

pub struct Parser;
impl Parser {
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
        Self::try_skip_whitespace(code);
        code.peek()
    }

    fn parse_var_string(code: &mut Peekable<Chars>) -> Result<String, String> {
        Self::try_skip_whitespace(code);

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
        Self::try_skip_whitespace(code);

        match code.next() {
            Some(char) => {
                if char != '\\' {
                    return Err("Expected \\".to_string());
                }
            }
            None => return Err("Expected \\".to_string()),
        };
        let param = Self::parse_var_string(code)?;
        let body = Self::parse_inner(code)?;
        Ok(Expr::Lambda(param, Box::new(body)))
    }

    fn parse_inner(code: &mut Peekable<Chars>) -> Result<Expr, String> {
        let mut opt_prev_expr = None;
        while let Some(char) = Self::try_skip_whitespace_peek(code) {
            let expr = if char.is_ascii_alphabetic() {
                Self::parse_var_string(code).map(|str| Expr::Var(str))
            } else if *char == '\\' {
                Self::parse_lambda(code)
            } else if *char == ')' {
                if let Some(prev_expr) = opt_prev_expr {
                    return Ok(prev_expr);
                } else {
                    return Err("Didn't expect expr to finish".to_string());
                }
            } else if *char == '(' {
                code.next().unwrap();
                let expr = Self::parse_inner(code);
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

    pub fn parse(line: &str) -> Result<Expr, String> {
        let mut code = line.chars().peekable();
        let res = Parser::parse_inner(&mut code)?;
        if code.peek().is_none() {
            Ok(res)
        } else {
            Err("Unexpected char".to_string())
        }
    }
}
