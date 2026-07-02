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

#[cfg(test)]
mod test {
    use crate::expr::Expr;
    use crate::parser::Parser;
    use std::rc::Rc;

    fn parse(s: &str) -> Expr {
        Parser::parse(s).unwrap_or_else(|e| panic!("failed to parse {s:?}: {e}"))
    }

    // ---------- variables ----------

    #[test]
    fn single_free_var() {
        let e = parse("x");
        match e {
            Expr::FreeVar(fv) => assert_eq!(fv.name, "x"),
            other => panic!("expected FreeVar, got {other:?}"),
        }
    }

    #[test]
    fn multichar_free_var_name() {
        let e = parse("foo");
        match e {
            Expr::FreeVar(fv) => assert_eq!(fv.name, "foo"),
            other => panic!("expected FreeVar, got {other:?}"),
        }
    }

    #[test]
    fn free_var_name_may_contain_underscore_after_first_char() {
        let e = parse("x_y");
        match e {
            Expr::FreeVar(fv) => assert_eq!(fv.name, "x_y"),
            other => panic!("expected FreeVar, got {other:?}"),
        }
    }

    #[test]
    fn same_free_var_name_shares_allocation() {
        // The parser interns free variables per parse.
        let e = parse("x x");
        match e {
            Expr::App(l, r) => match (*l, *r) {
                (Expr::FreeVar(a), Expr::FreeVar(b)) => {
                    assert!(
                        Rc::ptr_eq(&a, &b),
                        "both `x` should be the same Rc<FreeVar>"
                    )
                }
                other => panic!("expected FreeVar FreeVar, got {other:?}"),
            },
            other => panic!("expected App, got {other:?}"),
        }
    }

    #[test]
    fn different_free_var_names_get_different_allocations() {
        let e = parse("x y");
        match e {
            Expr::App(l, r) => match (*l, *r) {
                (Expr::FreeVar(a), Expr::FreeVar(b)) => {
                    assert!(!Rc::ptr_eq(&a, &b));
                    assert_eq!(a.name, "x");
                    assert_eq!(b.name, "y");
                }
                other => panic!("expected FreeVar FreeVar, got {other:?}"),
            },
            other => panic!("expected App, got {other:?}"),
        }
    }

    // ---------- lambdas & binder identity ----------

    #[test]
    fn identity_lambda_links_body_var_to_binder() {
        let e = parse("\\x x");
        let Expr::Lambda(l) = e else {
            panic!("expected Lambda, got {e:?}")
        };
        let Expr::BoundVar(ref body_bv) = l.body else {
            panic!("expected BoundVar body, got {:?}", l.body)
        };
        let assoc = l
            .associated_bound_var()
            .expect("lambda with a referenced param should have a bound var")
            .upgrade()
            .expect("bound var should still be alive");
        assert!(Rc::ptr_eq(body_bv, &assoc));
    }

    #[test]
    fn unused_param_has_no_associated_bound_var() {
        let e = parse("\\x y");
        let Expr::Lambda(l) = e else {
            panic!("expected Lambda, got {e:?}")
        };
        assert!(
            l.associated_bound_var().is_none(),
            "a never-referenced parameter should not allocate a bound var"
        );
        match &l.body {
            Expr::FreeVar(fv) => assert_eq!(fv.name, "y"),
            other => panic!("expected FreeVar body, got {other:?}"),
        }
    }

    #[test]
    fn multiple_occurrences_of_bound_var_share_pointer() {
        let e = parse("\\x x x");
        let Expr::Lambda(l) = e else {
            panic!("expected Lambda, got {e:?}")
        };
        let Expr::App(ref lhs, ref rhs) = l.body else {
            panic!("expected App body, got {:?}", l.body)
        };
        match (lhs.as_ref(), rhs.as_ref()) {
            (Expr::BoundVar(a), Expr::BoundVar(b)) => {
                assert!(
                    Rc::ptr_eq(a, b),
                    "both occurrences must be the same BoundVar"
                )
            }
            other => panic!("expected BoundVar BoundVar, got {other:?}"),
        }
    }

    #[test]
    fn lambda_body_extends_maximally_right() {
        // `\x x y` is `\x (x y)`, not `(\x x) y`.
        let e = parse("\\x x y");
        let Expr::Lambda(l) = e else {
            panic!("expected Lambda, got {e:?}")
        };
        assert!(matches!(l.body, Expr::App(..)));
    }

    // ---------- application structure ----------

    #[test]
    fn application_is_left_associative() {
        // `a b c` is `(a b) c`.
        let e = parse("a b c");
        let Expr::App(l, r) = e else {
            panic!("expected App, got {e:?}")
        };
        assert!(matches!(*l, Expr::App(..)), "left side should be `a b`");
        match *r {
            Expr::FreeVar(fv) => assert_eq!(fv.name, "c"),
            other => panic!("expected FreeVar c, got {other:?}"),
        }
    }

    #[test]
    fn parentheses_override_associativity() {
        // `a (b c)` groups to the right.
        let e = parse("a (b c)");
        let Expr::App(l, r) = e else {
            panic!("expected App, got {e:?}")
        };
        match *l {
            Expr::FreeVar(ref fv) => assert_eq!(fv.name, "a"),
            ref other => panic!("expected FreeVar a, got {other:?}"),
        }
        assert!(matches!(*r, Expr::App(..)), "right side should be `b c`");
    }

    #[test]
    fn redundant_parens_are_transparent() {
        let e = parse("((x))");
        assert!(matches!(e, Expr::FreeVar(_)));
    }

    // ---------- scoping & shadowing ----------

    #[test]
    fn shadowing_binds_to_innermost_lambda() {
        // In `\x \x x`, the body var belongs to the *inner* binder.
        let e = parse("\\x \\x x");
        let Expr::Lambda(outer) = e else {
            panic!("expected Lambda, got {e:?}")
        };
        assert!(
            outer.associated_bound_var().is_none(),
            "outer x is fully shadowed, so it is never referenced"
        );
        let Expr::Lambda(ref inner) = outer.body else {
            panic!("expected inner Lambda, got {:?}", outer.body)
        };
        let inner_bv = inner
            .associated_bound_var()
            .expect("inner binder is referenced")
            .upgrade()
            .unwrap();
        let Expr::BoundVar(ref body_bv) = inner.body else {
            panic!("expected BoundVar body")
        };
        assert!(Rc::ptr_eq(body_bv, &inner_bv));
    }

    #[test]
    fn outer_binding_is_restored_after_shadowed_scope_ends() {
        // `\x x (\x x) x`: first and last x belong to the outer binder,
        // the middle x to the inner one.
        let e = parse("\\x x (\\x x) x");
        let Expr::Lambda(outer) = e else {
            panic!("expected Lambda, got {e:?}")
        };
        // body is App(App(x, \x x), x)
        let Expr::App(ref l, ref last) = outer.body else {
            panic!("expected App body")
        };
        let Expr::App(ref first, ref inner_lam) = **l else {
            panic!("expected nested App")
        };
        let (Expr::BoundVar(first_bv), Expr::BoundVar(last_bv)) = (first.as_ref(), last.as_ref())
        else {
            panic!("expected outer occurrences to be BoundVars")
        };
        assert!(
            Rc::ptr_eq(first_bv, last_bv),
            "occurrences before and after the inner lambda must share the outer binder"
        );

        let Expr::Lambda(inner) = inner_lam.as_ref() else {
            panic!("expected inner Lambda")
        };
        let Expr::BoundVar(ref mid_bv) = inner.body else {
            panic!("expected inner BoundVar")
        };
        assert!(
            !Rc::ptr_eq(first_bv, mid_bv),
            "the shadowed occurrence must be a different BoundVar"
        );
    }

    #[test]
    fn param_goes_out_of_scope_after_lambda() {
        // In `(\x x) x`, the trailing x is free.
        let e = parse("(\\x x) x");
        let Expr::App(_, r) = e else {
            panic!("expected App, got {e:?}")
        };
        match *r {
            Expr::FreeVar(fv) => assert_eq!(fv.name, "x"),
            other => panic!("trailing x should be free, got {other:?}"),
        }
    }

    // ---------- whitespace ----------

    #[test]
    fn whitespace_is_insignificant() {
        for s in ["  x  ", "\\x   x", "\\ x x", " ( \\x  x )  y ", "a\n b\tc"] {
            assert!(Parser::parse(s).is_ok(), "should parse {s:?}");
        }
    }

    // ---------- errors ----------

    #[test]
    fn error_cases() {
        let bad = [
            "",         // empty input
            "   ",      // only whitespace
            "(",        // unclosed paren, no expr
            "(a",       // unclosed paren
            "()",       // empty parens
            "a)",       // stray closing paren after a full expr
            ")a",       // leading closing paren
            "\\",       // lambda without param
            "\\x",      // lambda without body
            "\\ \\x x", // param must be a name, not another lambda
            "a $ b",    // illegal symbol
            "a1",       // digits are not part of names
            "_x",       // a term can't start with underscore
            "\\x_y x",  // params are ascii-alphabetic only, so `_y` is left over
        ];
        for s in bad {
            assert!(
                Parser::parse(s).is_err(),
                "expected parse error for {s:?}, got {:?}",
                Parser::parse(s)
            );
        }
    }

    #[test]
    fn error_messages_are_stable_for_key_cases() {
        assert_eq!(Parser::parse("").unwrap_err(), "Expression not found");
        assert_eq!(Parser::parse("(a").unwrap_err(), "Expected )");
        assert_eq!(Parser::parse("\\").unwrap_err(), "Expected variable");
    }
}
