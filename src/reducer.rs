use crate::expr::{BoundVar, Expr, Lambda};
use std::ptr;
use std::rc::{Rc, Weak};

pub struct Reducer;
impl Reducer {
    fn substitute(expr: Expr, var: &BoundVar, val: Expr) -> Expr {
        match expr {
            Expr::BoundVar(ref bv) => {
                if ptr::eq(bv.as_ref(), var) {
                    val
                } else {
                    expr
                }
            }
            Expr::Lambda(lambda) => {
                let new_body = Self::substitute(lambda.body.clone(), var, val);
                match lambda.associated_bound_var() {
                    Some(bv) => {
                        if ptr::eq(Weak::as_ptr(&bv), var as *const BoundVar) {
                            new_body
                        } else {
                            let new_lambda = Rc::new(Lambda::new(new_body));
                            Self::move_bound_var(&lambda, new_lambda.clone());
                            Expr::Lambda(new_lambda)
                        }
                    }
                    None => Expr::Lambda(Rc::new(Lambda::new(new_body))),
                }
            }
            Expr::App(l, r) => Expr::App(
                Box::new(Self::substitute(*l, var, val.clone())),
                Box::new(Self::substitute(*r, var, val)),
            ),
            _ => expr,
        }
    }

    pub fn whnf(expr: Expr) -> Expr {
        let mut expr = expr;
        loop {
            match expr {
                Expr::BoundVar(..) => return expr,
                Expr::FreeVar(..) => return expr,
                Expr::Lambda(..) => return expr,
                Expr::App(ref l, ref r) => {
                    let whnf_l = Self::whnf(*l.clone());
                    match whnf_l {
                        Expr::Lambda(ref lambda) => match lambda.associated_bound_var() {
                            Some(bv) => {
                                let bv = bv.upgrade().expect("Expected bound var to exist");
                                expr = Self::substitute(whnf_l, &bv, *r.clone());
                            }
                            None => {
                                expr = lambda.body.clone();
                            }
                        },
                        _ => return expr,
                    }
                }
            }
        }
    }

    pub fn nf(expr: Expr) -> Expr {
        let expr = Self::whnf(expr);
        match expr {
            Expr::BoundVar(..) => expr,
            Expr::FreeVar(..) => expr,
            Expr::Lambda(lambda) => {
                let new_body = Self::nf(lambda.body.clone());
                let new_lambda = Rc::new(Lambda::new(new_body));
                Self::move_bound_var(&lambda, new_lambda.clone());
                Expr::Lambda(new_lambda)
            }
            Expr::App(l, r) => Expr::App(
                Box::new(Self::nf(*l.clone())),
                Box::new(Self::nf(*r.clone())),
            ),
        }
    }

    fn move_bound_var(lambda: &Lambda, new_lambda: Rc<Lambda>) {
        if let Some(bv) = lambda.associated_bound_var() {
            new_lambda.set_bound_var(Weak::clone(&bv));
            bv.upgrade().unwrap().set_lambda(Rc::downgrade(&new_lambda));
        } else {
            new_lambda.set_bound_var(Weak::new());
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::expr::{BoundVar, Expr, Lambda};
    use crate::parser::Parser;
    use crate::reducer::Reducer;
    use std::rc::Rc;
    // ---------- helpers ----------

    fn parse(s: &str) -> Expr {
        Parser::parse(s).unwrap_or_else(|e| panic!("parse `{s}` failed: {e}"))
    }

    fn variant_name(e: &Expr) -> &'static str {
        match e {
            Expr::BoundVar(_) => "BoundVar",
            Expr::FreeVar(_) => "FreeVar",
            Expr::Lambda(_) => "Lambda",
            Expr::App(_, _) => "App",
        }
    }

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
    fn as_app(e: &Expr) -> (&Expr, &Expr) {
        match e {
            Expr::App(l, r) => (l.as_ref(), r.as_ref()),
            o => panic!("expected App, got {}", variant_name(o)),
        }
    }

    /// Structural equality modulo renaming of bound variables.
    /// Two BoundVars correspond iff their binding lambdas were introduced at
    /// the same depth in a lockstep traversal — i.e. same De Bruijn level.
    /// Assumes closed terms (every BoundVar binds within the compared tree).
    fn alpha_eq(a: &Expr, b: &Expr) -> bool {
        fn go(a: &Expr, b: &Expr, scope: &mut Vec<(*const Lambda, *const Lambda)>) -> bool {
            match (a, b) {
                (Expr::FreeVar(x), Expr::FreeVar(y)) => {
                    fn strip(n: &str) -> &str {
                        n.strip_prefix("fv_").unwrap_or(n)
                    }
                    strip(&x.name) == strip(&y.name)
                }
                (Expr::App(a1, a2), Expr::App(b1, b2)) => go(a1, b1, scope) && go(a2, b2, scope),
                (Expr::Lambda(la), Expr::Lambda(lb)) => {
                    scope.push((Rc::as_ptr(la), Rc::as_ptr(lb)));
                    let r = go(&la.body, &lb.body, scope);
                    scope.pop();
                    r
                }
                (Expr::BoundVar(ba), Expr::BoundVar(bb)) => {
                    match (
                        ba.associated_lambda().upgrade(),
                        bb.associated_lambda().upgrade(),
                    ) {
                        (Some(la), Some(lb)) => {
                            let (pa, pb) = (Rc::as_ptr(&la), Rc::as_ptr(&lb));
                            let ia = scope.iter().rposition(|&(s, _)| s == pa);
                            let ib = scope.iter().rposition(|&(_, s)| s == pb);
                            ia.is_some() && ia == ib
                        }
                        _ => false,
                    }
                }
                _ => false,
            }
        }
        go(a, b, &mut Vec::new())
    }

    /// Every BoundVar must upgrade (no dangling weak) and bind to an enclosing
    /// lambda (well-nested scope). The core "memory layout is sane" check.
    fn assert_well_scoped(expr: &Expr) {
        fn go(e: &Expr, ancestors: &mut Vec<*const Lambda>) {
            match e {
                Expr::FreeVar(_) => {}
                Expr::App(l, r) => {
                    go(l, ancestors);
                    go(r, ancestors);
                }
                Expr::Lambda(l) => {
                    ancestors.push(Rc::as_ptr(l));
                    go(&l.body, ancestors);
                    ancestors.pop();
                }
                Expr::BoundVar(bv) => {
                    let lam = bv
                        .associated_lambda()
                        .upgrade()
                        .expect("bound var must point to a live lambda after reduction");
                    assert!(
                        ancestors.contains(&Rc::as_ptr(&lam)),
                        "bound var must bind to an enclosing lambda (well-scoped)"
                    );
                }
            }
        }
        go(expr, &mut Vec::new());
    }

    // ---------- correctness ----------

    #[test]
    fn identity_of_identity() {
        let src = "(\\x x) (\\y y)";
        assert!(alpha_eq(&Reducer::whnf(parse(src)), &parse("\\y y")));
        assert!(alpha_eq(&Reducer::nf(parse(src)), &parse("\\y y")));
    }

    #[test]
    fn whnf_and_nf_differ_under_binder() {
        // \x ((\y y) z)  — a redex sits under the \x binder; z is free.
        let src = "\\x ((\\y y) z)";
        // WHNF reaches the head lambda and stops — body redex untouched.
        assert!(
            alpha_eq(&Reducer::whnf(parse(src)), &parse("\\x ((\\y y) z)")),
            "WHNF must not reduce under the lambda"
        );
        // NF reduces the body: (\y y) z -> z.
        assert!(
            alpha_eq(&Reducer::nf(parse(src)), &parse("\\x z")),
            "NF must reduce the redex under the lambda"
        );
    }

    #[test]
    fn nf_computes_successor() {
        // succ 0 = 1
        let r = Reducer::nf(parse("(\\n \\f \\x f (n f x)) (\\f \\x x)"));
        assert!(alpha_eq(&r, &parse("\\f \\x f x")), "succ 0 should be 1");
    }

    #[test]
    fn nf_computes_addition() {
        // plus 1 2 = 3
        let r = Reducer::nf(parse(
            "(\\m \\n \\f \\x m f (n f x)) (\\f \\x f x) (\\f \\x f (f x))",
        ));
        assert!(
            alpha_eq(&r, &parse("\\f \\x f (f (f x))")),
            "1 + 2 should be 3"
        );
    }

    #[test]
    fn nf_computes_multiplication() {
        // mult 2 3 = 6
        let r = Reducer::nf(parse(
            "(\\m \\n \\f m (n f)) (\\f \\x f (f x)) (\\f \\x f (f (f x)))",
        ));
        assert!(
            alpha_eq(&r, &parse("\\f \\x f (f (f (f (f (f x)))))")),
            "2 * 3 should be 6"
        );
    }

    #[test]
    fn discarded_argument_is_never_forced() {
        // K I Ω — Ω diverges if forced, but K discards it.
        // If the reducer evaluated arguments eagerly, these calls would HANG.
        let src = "(\\x \\y x) (\\a a) ((\\x x x) (\\x x x))";
        assert!(
            alpha_eq(&Reducer::whnf(parse(src)), &parse("\\a a")),
            "WHNF of K I Ω should be I"
        );
        assert!(
            alpha_eq(&Reducer::nf(parse(src)), &parse("\\a a")),
            "NF of K I Ω should be I"
        );
    }

    // ---------- memory layout after reduction ----------

    #[test]
    fn nf_output_is_well_scoped_and_live() {
        // A non-trivial reduction result (6); every binding must be sound.
        let r = Reducer::nf(parse(
            "(\\m \\n \\f m (n f)) (\\f \\x f (f x)) (\\f \\x f (f (f x)))",
        ));
        assert_well_scoped(&r);
    }

    #[test]
    fn nf_preserves_bound_var_sharing() {
        // succ 1 = 2 = \f \x f (f x); the two `f` occurrences in the RESULT
        // must share one Rc<BoundVar>, not be reallocated independently.
        let two = Reducer::nf(parse("(\\n \\f \\x f (n f x)) (\\f \\x f x)"));
        let outer = as_lambda(&two);
        let inner = as_lambda(&outer.body);
        let (f1, fx) = as_app(&inner.body);
        let (f2, _x) = as_app(fx);
        assert!(
            Rc::ptr_eq(&as_bound(f1), &as_bound(f2)),
            "both `f` in the reduced numeral 2 must be one shared Rc<BoundVar>"
        );
    }

    #[test]
    fn dropping_reduced_expr_breaks_cycle() {
        let weak = {
            let e = Reducer::nf(parse("(\\x x) (\\y y)")); // \y y
            Rc::downgrade(&as_lambda(&e))
        };
        assert!(
            weak.upgrade().is_none(),
            "reduced term must free cleanly once dropped — no leaked cycle"
        );
    }
}
