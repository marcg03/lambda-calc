use crate::expr::Expr;
use crate::substituer::Substituer;

pub struct Reducer {
    substituer: Substituer,
}

impl Reducer {
    pub fn new() -> Self {
        Self {
            substituer: Substituer::new(),
        }
    }

    fn reduce_whnf_inner(&mut self, start: &Expr) -> Expr {
        match start {
            Expr::Var(..) => start.clone(),
            Expr::Lambda(..) => start.clone(),
            Expr::App(l, arg) => {
                let l_reduced = self.reduce_whnf_inner(l);
                match l_reduced {
                    Expr::Lambda(param, body) => {
                        let substituted = self.substituer.substitute(&param, &body, &arg);
                        self.reduce_whnf_inner(&substituted)
                    }
                    _ => start.clone(),
                }
            }
        }
    }

    fn reduce_nf_inner(&mut self, start: &Expr) -> Expr {
        let whnf = self.reduce_whnf_inner(start);
        match whnf {
            Expr::Lambda(param, body) => {
                Expr::Lambda(param.clone(), Box::new(self.reduce_nf_inner(&body)))
            }
            Expr::App(l, r) => Expr::App(
                Box::new(self.reduce_nf_inner(&l)),
                Box::new(self.reduce_nf_inner(&r)),
            ),
            Expr::Var(_) => whnf,
        }
    }

    pub fn reduce_whnf(&self, start: &Expr) -> Expr {
        let mut reducer = Self::new();
        reducer.reduce_whnf_inner(start)
    }

    pub fn reduce_nf(&self, start: &Expr) -> Expr {
        let mut reducer = Self::new();
        reducer.reduce_nf_inner(start)
    }
}
