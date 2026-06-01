use crate::expr::Expr;

pub struct Substituer {
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
