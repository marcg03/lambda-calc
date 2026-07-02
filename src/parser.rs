use crate::expr::{BoundVar, Expr, FreeVar, Lambda};
use std::cell::RefCell;
use std::collections::HashMap;
use std::iter::Peekable;
use std::rc::{Rc, Weak};
use std::str::Chars;

pub struct Parser {
    params: HashMap<String, (Weak<Lambda>, RefCell<Option<Weak<BoundVar>>>)>,
    free_vars: HashMap<String, Rc<FreeVar>>,
}

impl Parser {
    fn new() -> Self {
        Parser {
            params: HashMap::new(),
            free_vars: HashMap::new(),
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
        Self::try_skip_whitespace(code);
        code.peek()
    }

    fn parse_param_string(code: &mut Peekable<Chars>) -> Result<String, String> {
        Self::parse_string(code, char::is_ascii_alphabetic)
    }

    fn parse_var_string(code: &mut Peekable<Chars>) -> Result<String, String> {
        Self::parse_string(code, |char| char.is_alphabetic() || *char == '_')
    }

    fn parse_string<F>(code: &mut Peekable<Chars>, f: F) -> Result<String, String>
    where
        F: Fn(&char) -> bool,
    {
        Self::try_skip_whitespace(code);

        let mut var = String::new();
        while let Some(char) = code.peek()
            && f(char)
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

    fn parse_lambda(&mut self, code: &mut Peekable<Chars>) -> Result<Expr, String> {
        Self::try_skip_whitespace(code);

        match code.next() {
            Some('\\') => {}
            _ => return Err("Expected \\".to_string()),
        }

        let param = Self::parse_param_string(code)?;

        let mut parsed: Result<(), String> = Ok(());
        let mut shadowed: Option<(Weak<Lambda>, RefCell<Option<Weak<BoundVar>>>)> = None;
        let lambda = Rc::new_cyclic(|weak: &Weak<Lambda>| {
            shadowed = self
                .params
                .insert(param.clone(), (Weak::clone(weak), RefCell::new(None)));
            match self.parse_inner(code) {
                Ok(body) => Lambda::new(body),
                Err(e) => {
                    parsed = Err(e);
                    Lambda::new(Expr::BoundVar(Rc::new(BoundVar::new())))
                }
            }
        });
        if let Some(bound_var) = self.params.get(&param).unwrap().1.borrow().as_ref() {
            lambda.set_bound_var(Weak::clone(bound_var));
        }

        // leaving the lambda's scope: restore the previous meaning of `param`
        // (handles nested same-name binders), or drop the binding if it was new.
        match shadowed {
            Some(outer) => {
                self.params.insert(param, outer);
            }
            None => {
                self.params.remove(&param);
            }
        }
        parsed?;

        Ok(Expr::Lambda(lambda))
    }

    fn parse_var(&mut self, code: &mut Peekable<Chars>) -> Result<Expr, String> {
        Self::parse_var_string(code).map(|str| {
            let entry = self.params.get(&str).cloned();
            match entry {
                Some((lambda, bound_var)) => {
                    let bound_var = bound_var
                        .borrow()
                        .as_ref()
                        .map(|bv| bv.upgrade().expect("Expected lambda to have bound var"))
                        .unwrap_or_else(|| {
                            let bound_var = Rc::new(BoundVar::new());
                            self.params.insert(
                                str.clone(),
                                (
                                    Weak::clone(&lambda),
                                    RefCell::new(Some(Rc::downgrade(&bound_var))),
                                ),
                            );
                            bound_var
                        });
                    Expr::BoundVar(bound_var)
                }
                None => Expr::FreeVar(Rc::clone(
                    self.free_vars
                        .entry(str)
                        .or_insert_with_key(|str| Rc::new(FreeVar { name: str.clone() })),
                )),
            }
        })
    }

    fn parse_inner(&mut self, code: &mut Peekable<Chars>) -> Result<Expr, String> {
        let mut opt_prev_expr = None;
        while let Some(char) = Self::try_skip_whitespace_peek(code) {
            let expr = if char.is_ascii_alphabetic() {
                self.parse_var(code)
            } else if *char == '\\' {
                self.parse_lambda(code)
            } else if *char == ')' {
                if let Some(prev_expr) = opt_prev_expr {
                    return Ok(prev_expr);
                } else {
                    return Err("Didn't expect expr to finish".to_string());
                }
            } else if *char == '(' {
                code.next().unwrap();
                let expr = self.parse_inner(code)?;
                if code.next() != Some(')') {
                    return Err("Expected )".to_string());
                }
                Ok(expr)
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
        let mut parser = Parser::new();
        let mut code = line.chars().peekable();
        let res = parser.parse_inner(&mut code)?;
        if code.peek().is_none() {
            Ok(res)
        } else {
            Err("Unexpected char".to_string())
        }
    }
}
