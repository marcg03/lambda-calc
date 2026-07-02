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
mod tests {
    use super::*;
    use std::rc::Rc;

    #[test]
    fn self_application_shares_bound_var() {
        // "\x x x"  ==  λx. (x x)
        // Note the doubled backslash — that's a literal `\` in a Rust string.
        let expr = Parser::parse("\\x x x").expect("`\\x x x` should parse");

        // 1. Top level is a Lambda
        let lambda = match &expr {
            Expr::Lambda(l) => Rc::clone(l),
            other => panic!("expected top-level Lambda, got {}", variant_name(other)),
        };

        // 2. Its body is an application of two terms
        let (left, right) = match &lambda.body {
            Expr::App(l, r) => (l.as_ref(), r.as_ref()),
            other => panic!("expected App body, got {}", variant_name(other)),
        };

        // 3. Both sides are BoundVars
        let left_bv = match left {
            Expr::BoundVar(bv) => Rc::clone(bv),
            other => panic!("expected BoundVar on left, got {}", variant_name(other)),
        };
        let right_bv = match right {
            Expr::BoundVar(bv) => Rc::clone(bv),
            other => panic!("expected BoundVar on right, got {}", variant_name(other)),
        };

        // 4. KEY INVARIANT: both occurrences of `x` resolve to the SAME
        //    Rc<BoundVar> — not two independent allocations.
        assert!(
            Rc::ptr_eq(&left_bv, &right_bv),
            "both `x` occurrences must share one Rc<BoundVar>"
        );

        // 5. The bound var's weak points back to the enclosing lambda.
        let bv_lambda = left_bv
            .associated_lambda()
            .upgrade()
            .expect("bound var's lambda weak should be live");
        assert!(
            Rc::ptr_eq(&bv_lambda, &lambda),
            "bound var should reference the enclosing lambda"
        );

        // 6. The lambda's weak points back to that same bound var.
        let lambda_bv = lambda
            .associated_bound_var()
            .expect("lambda should have an associated bound var")
            .upgrade()
            .expect("lambda's bound var weak should be live");
        assert!(
            Rc::ptr_eq(&lambda_bv, &left_bv),
            "lambda's associated bound var should be the shared bound var"
        );
    }

    fn variant_name(e: &Expr) -> &'static str {
        match e {
            Expr::BoundVar(_) => "BoundVar",
            Expr::FreeVar(_) => "FreeVar",
            Expr::Lambda(_) => "Lambda",
            Expr::App(_, _) => "App",
            Expr::Thunk(_) => "Thunk",
        }
    }

    // ---- navigation helpers ----
    fn as_lambda(e: &Expr) -> Rc<Lambda> {
        match e {
            Expr::Lambda(l) => Rc::clone(l),
            o => panic!("expected Lambda, got {}", variant_name(o)),
        }
    }
    fn as_bound(e: &Expr) -> Rc<BoundVar> {
        match e {
            Expr::BoundVar(b) => Rc::clone(b),
            o => panic!("expected BoundVar, got {}", variant_name(o)),
        }
    }
    fn as_free(e: &Expr) -> Rc<FreeVar> {
        match e {
            Expr::FreeVar(f) => Rc::clone(f),
            o => panic!("expected FreeVar, got {}", variant_name(o)),
        }
    }
    fn as_app(e: &Expr) -> (&Expr, &Expr) {
        match e {
            Expr::App(l, r) => (l.as_ref(), r.as_ref()),
            o => panic!("expected App, got {}", variant_name(o)),
        }
    }
    fn binder_of(bv: &BoundVar) -> Rc<Lambda> {
        bv.associated_lambda()
            .upgrade()
            .expect("bound var must point to a live lambda")
    }

    #[test]
    fn inner_binder_shadows_outer() {
        // "\x \x x"  ==  λx. λx. x
        // The body `x` must bind to the INNER λx, not the outer one.
        let expr = Parser::parse("\\x \\x x").expect("`\\x \\x x` should parse");

        // 1. Outer lambda
        let outer = match &expr {
            Expr::Lambda(l) => Rc::clone(l),
            other => panic!("expected outer Lambda, got {}", variant_name(other)),
        };

        // 2. Its body is the inner lambda
        let inner = match &outer.body {
            Expr::Lambda(l) => Rc::clone(l),
            other => panic!("expected inner Lambda, got {}", variant_name(other)),
        };

        // sanity: they are genuinely different lambdas
        assert!(
            !Rc::ptr_eq(&outer, &inner),
            "outer and inner lambdas must be distinct allocations"
        );

        // 3. Inner body is the bound var occurrence
        let bv = match &inner.body {
            Expr::BoundVar(bv) => Rc::clone(bv),
            other => panic!("expected BoundVar, got {}", variant_name(other)),
        };

        // 4. KEY INVARIANT: the occurrence binds to the INNER lambda.
        let bound_to = bv
            .associated_lambda()
            .upgrade()
            .expect("bound var's lambda weak should be live");
        assert!(
            Rc::ptr_eq(&bound_to, &inner),
            "shadowed `x` must bind to the inner lambda"
        );
        assert!(
            !Rc::ptr_eq(&bound_to, &outer),
            "shadowed `x` must NOT bind to the outer lambda"
        );

        // 5. Inner lambda points back to that bound var.
        let inner_bv = inner
            .associated_bound_var()
            .expect("inner lambda should have an associated bound var")
            .upgrade()
            .expect("inner bound var weak should be live");
        assert!(
            Rc::ptr_eq(&inner_bv, &bv),
            "inner lambda's bound var should be the occurrence"
        );

        // 6. Outer binder is never used -> it has NO associated bound var.
        //    This is what proves the outer binding was correctly shadowed
        //    (and later restored/removed) rather than wrongly referenced.
        assert!(
            outer.associated_bound_var().is_none(),
            "unused outer binder must have no associated bound var"
        );
    }

    #[test]
    fn shadow_restore_and_free_var_sharing() {
        // "\x ((\x x) (x y)) y"  ==  λx. ( ((λx. x) (x y)) y )
        let expr = Parser::parse("\\x ((\\x x) (x y)) y").expect("expression should parse");

        // outer lambda
        let outer = match &expr {
            Expr::Lambda(l) => Rc::clone(l),
            other => panic!("expected outer Lambda, got {}", variant_name(other)),
        };

        // outer body:  App( App(inner, (x y)), y2 )
        let (app_left, y2) = match &outer.body {
            Expr::App(l, r) => (l.as_ref(), r.as_ref()),
            other => panic!("expected outer body App, got {}", variant_name(other)),
        };
        let y2 = match y2 {
            Expr::FreeVar(f) => Rc::clone(f),
            other => panic!("expected trailing FreeVar, got {}", variant_name(other)),
        };

        // app_left:  App( inner_lambda, (x y) )
        let (inner_node, xy_node) = match app_left {
            Expr::App(l, r) => (l.as_ref(), r.as_ref()),
            other => panic!("expected nested App, got {}", variant_name(other)),
        };

        // inner lambda + its occurrence
        let inner = match inner_node {
            Expr::Lambda(l) => Rc::clone(l),
            other => panic!("expected inner Lambda, got {}", variant_name(other)),
        };
        let bv_inner = match &inner.body {
            Expr::BoundVar(bv) => Rc::clone(bv),
            other => panic!("expected inner BoundVar, got {}", variant_name(other)),
        };

        // (x y):  App( bv_outer, y1 )
        let (x_outer_node, y1) = match xy_node {
            Expr::App(l, r) => (l.as_ref(), r.as_ref()),
            other => panic!("expected (x y) App, got {}", variant_name(other)),
        };
        let bv_outer = match x_outer_node {
            Expr::BoundVar(bv) => Rc::clone(bv),
            other => panic!("expected outer-bound BoundVar, got {}", variant_name(other)),
        };
        let y1 = match y1 {
            Expr::FreeVar(f) => Rc::clone(f),
            other => panic!("expected FreeVar y1, got {}", variant_name(other)),
        };

        // --- shadowing: inner `x` binds to the inner lambda ---
        let inner_target = bv_inner
            .associated_lambda()
            .upgrade()
            .expect("inner bound var lambda should be live");
        assert!(
            Rc::ptr_eq(&inner_target, &inner),
            "inner `x` must bind to the inner lambda"
        );

        // --- restore: post-scope `x` binds back to the outer lambda ---
        let outer_target = bv_outer
            .associated_lambda()
            .upgrade()
            .expect("outer bound var lambda should be live");
        assert!(
            Rc::ptr_eq(&outer_target, &outer),
            "`x` after the inner scope must bind to the OUTER lambda (restore)"
        );
        assert!(
            !Rc::ptr_eq(&outer_target, &inner),
            "`x` after the inner scope must NOT still bind to the inner lambda"
        );

        // --- the two bound occurrences are distinct bindings ---
        assert!(
            !Rc::ptr_eq(&bv_inner, &bv_outer),
            "inner and outer `x` are different bindings -> different BoundVars"
        );

        // --- free var sharing: both `y` are one Rc<FreeVar> ---
        assert!(
            Rc::ptr_eq(&y1, &y2),
            "both `y` occurrences must share one Rc<FreeVar>"
        );
        assert_eq!(y1.name, "y", "free var should be named `y`");
    }

    #[test]
    fn church_two_reaches_across_inner_scope() {
        // "\f \x f (f x)"  ==  λf. λx. (f (f x))
        // `f` is used inside the λx scope but must still bind to λf.
        let expr = Parser::parse("\\f \\x f (f x)").expect("should parse");

        let l_f = as_lambda(&expr);
        let l_x = as_lambda(&l_f.body);
        assert!(!Rc::ptr_eq(&l_f, &l_x), "the two lambdas are distinct");

        // body: App(f1, App(f2, x1))
        let (f1_node, inner_app) = as_app(&l_x.body);
        let (f2_node, x1_node) = as_app(inner_app);
        let f1 = as_bound(f1_node);
        let f2 = as_bound(f2_node);
        let x1 = as_bound(x1_node);

        // both `f` reach across λx to bind λf; `x` binds λx
        assert!(Rc::ptr_eq(&binder_of(&f1), &l_f), "f1 binds to λf");
        assert!(Rc::ptr_eq(&binder_of(&f2), &l_f), "f2 binds to λf");
        assert!(Rc::ptr_eq(&binder_of(&x1), &l_x), "x binds to λx");

        // both `f` occurrences share ONE BoundVar despite different depths
        assert!(
            Rc::ptr_eq(&f1, &f2),
            "both `f` occurrences share one BoundVar"
        );
        assert!(!Rc::ptr_eq(&f1, &x1), "`f` and `x` are different bindings");
    }

    #[test]
    fn same_name_bound_and_free_not_conflated() {
        // "(\x x) x"  — inner x is BOUND, trailing x is FREE, same name.
        let expr = Parser::parse("(\\x x) x").expect("should parse");

        let (lam_node, free_node) = as_app(&expr);
        let lam = as_lambda(lam_node);
        let bound_x = as_bound(&lam.body);
        assert!(
            Rc::ptr_eq(&binder_of(&bound_x), &lam),
            "inner x binds to its λ"
        );

        // the trailing x must be a FreeVar (as_free panics if it's a BoundVar)
        let free_x = as_free(free_node);
        assert_eq!(free_x.name, "x", "trailing x is the free var named x");
    }

    #[test]
    fn sibling_scopes_are_isolated() {
        // "(\x x) (\x x)"  — two independent λx; first scope must not leak.
        let expr = Parser::parse("(\\x x) (\\x x)").expect("should parse");

        let (left_node, right_node) = as_app(&expr);
        let l1 = as_lambda(left_node);
        let l2 = as_lambda(right_node);
        assert!(!Rc::ptr_eq(&l1, &l2), "the two lambdas are distinct");

        let bv1 = as_bound(&l1.body);
        let bv2 = as_bound(&l2.body);
        assert!(!Rc::ptr_eq(&bv1, &bv2), "each scope has its own BoundVar");

        assert!(Rc::ptr_eq(&binder_of(&bv1), &l1), "left x binds to left λ");
        assert!(
            Rc::ptr_eq(&binder_of(&bv2), &l2),
            "right x binds to right λ"
        );
        assert!(
            !Rc::ptr_eq(&binder_of(&bv2), &l1),
            "right x must NOT bind to the left λ (no scope leakage)"
        );
    }

    #[test]
    fn application_is_left_associative() {
        // "x y z"  parses as  ((x y) z)
        let expr = Parser::parse("x y z").expect("should parse");

        let (xy_node, z_node) = as_app(&expr);
        let (x_node, y_node) = as_app(xy_node);
        let x = as_free(x_node);
        let y = as_free(y_node);
        let z = as_free(z_node);
        assert_eq!(
            (x.name.as_str(), y.name.as_str(), z.name.as_str()),
            ("x", "y", "z")
        );

        assert!(
            !Rc::ptr_eq(&x, &y) && !Rc::ptr_eq(&y, &z) && !Rc::ptr_eq(&x, &z),
            "different free names must be different Rc<FreeVar>"
        );
    }

    #[test]
    fn parentheses_override_associativity() {
        // "x (y z)"  parses as  (x (y z)), right-nested
        let expr = Parser::parse("x (y z)").expect("should parse");

        let (x_node, yz_node) = as_app(&expr);
        assert_eq!(as_free(x_node).name, "x");
        let (y_node, z_node) = as_app(yz_node);
        assert_eq!(as_free(y_node).name, "y");
        assert_eq!(as_free(z_node).name, "z");
    }

    #[test]
    fn unused_binder_has_no_bound_var() {
        // "\x y"  ==  λx. y  — x never used; y is free.
        let expr = Parser::parse("\\x y").expect("should parse");

        let lam = as_lambda(&expr);
        assert_eq!(as_free(&lam.body).name, "y", "body is the free var y");
        assert!(
            lam.associated_bound_var().is_none(),
            "an unused binder must have no associated bound var"
        );
    }

    #[test]
    fn malformed_input_is_rejected() {
        // NOTE: tune this list to your grammar's exact rules.
        for bad in ["(\\x x", "\\", ")", "(", "\\x"] {
            assert!(
                Parser::parse(bad).is_err(),
                "expected `{bad}` to be an error"
            );
        }
    }

    #[test]
    fn dropping_expr_breaks_the_cycle() {
        let weak_lambda = {
            let expr = Parser::parse("\\x x").expect("parse");
            let lam = match &expr {
                Expr::Lambda(l) => Rc::clone(l),
                other => panic!("expected Lambda, got {}", variant_name(other)),
            };
            Rc::downgrade(&lam)
            // both `expr` and `lam` drop at the end of this block
        };
        assert!(
            weak_lambda.upgrade().is_none(),
            "lambda must be freed once the tree drops — the BoundVar<->Lambda \
             cycle must not keep it alive via strong refs"
        );
    }
}
