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
    }
}
