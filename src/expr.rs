use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::{Rc, Weak};

#[derive(Debug)]
pub struct BoundVar;

impl BoundVar {
    pub fn new() -> Self {
        Self {}
    }
}

#[derive(Debug)]
pub struct FreeVar {
    pub name: String,
}

#[derive(Debug)]
pub struct Lambda {
    pub body: Expr,
    bound_var: RefCell<Option<Weak<BoundVar>>>,
}

impl Lambda {
    pub fn new(body: Expr) -> Self {
        Self {
            body,
            bound_var: RefCell::new(None),
        }
    }

    pub fn associated_bound_var(&self) -> Option<Weak<BoundVar>> {
        self.bound_var.borrow().clone()
    }

    pub fn set_bound_var(&self, bound_var: Weak<BoundVar>) {
        *self.bound_var.borrow_mut() = Some(bound_var);
    }
}

type Env = HashMap<*const BoundVar, Expr>;

#[derive(Debug)]
enum ThunkState {
    Forced(Expr),
    Unforced(Expr),
}

#[derive(Debug)]
pub struct Thunk {
    state: RefCell<ThunkState>,
    env: RefCell<Env>,
}

impl Thunk {
    fn new(expr: Expr, env: Env) -> Self {
        Self {
            state: RefCell::new(ThunkState::Unforced(expr)),
            env: RefCell::new(env),
        }
    }

    fn find(bv: *const BoundVar, env: &Env) -> Option<Expr> {
        env.get(&bv).cloned()
    }

    fn reduce(expr: Expr, env: &mut Env) -> Expr {
        match expr {
            Expr::BoundVar(ref bv) => {
                if let Some(val) = Thunk::find(Rc::as_ptr(bv), env) {
                    Thunk::reduce(val, env)
                } else {
                    expr
                }
            }
            Expr::FreeVar(..) => expr,
            Expr::Lambda(..) => {
                if env.is_empty() {
                    expr
                } else {
                    Expr::Thunk(Rc::new(Thunk::new(expr, env.clone())))
                }
            }
            Expr::App(l, r) => {
                let l_reduced = Thunk::reduce(*l, env);
                let initial_env = env.clone();
                let l_reduced = if let Expr::Thunk(thunk) = &l_reduced {
                    thunk.force();
                    env.extend(thunk.env.borrow().clone());
                    thunk.expr()
                } else {
                    l_reduced
                };

                if let Expr::Lambda(l) = l_reduced {
                    let bv = if let Some(bv) = l.associated_bound_var() {
                        bv
                    } else {
                        let result = Thunk::reduce(l.body.clone(), env);
                        *env = initial_env;
                        return result;
                    };

                    // wrap `r` in a thunk (snapshot crt env)
                    let thunk = if env.is_empty() {
                        *r
                    } else {
                        Expr::Thunk(Rc::new(Thunk::new(*r, env.clone())))
                    };
                    // add that thunk to env
                    env.insert(Weak::as_ptr(&bv), thunk);
                    // reduce the body of the lambda with the new env
                    let expr = Thunk::reduce(l.body.clone(), env);
                    *env = initial_env;
                    expr
                } else {
                    let thunk = Expr::Thunk(Rc::new(Thunk::new(*r, env.clone())));
                    *env = initial_env;
                    Expr::App(Box::new(l_reduced), Box::new(thunk))
                }
            }
            Expr::Thunk(thunk) => {
                thunk.force();
                let initial_env = env.clone();
                env.extend(thunk.env.borrow().clone());
                let result = Thunk::reduce(thunk.expr(), env);
                *env = initial_env;
                result
            }
        }
    }

    pub fn force(&self) {
        let mut state = self.state.borrow_mut();
        let expr = match &*state {
            ThunkState::Forced(..) => return,
            ThunkState::Unforced(expr) => expr.clone(),
        };
        let mut env = self.env.borrow_mut();
        let mut expr = Thunk::reduce(expr, &mut *env);
        if let Expr::Thunk(nested) = expr {
            env.extend(nested.env.borrow().clone());
            expr = nested.expr();
        }
        *state = ThunkState::Forced(expr);
    }

    fn expr(&self) -> Expr {
        let state = self.state.borrow();
        match &*state {
            ThunkState::Forced(expr) => expr.clone(),
            ThunkState::Unforced(expr) => expr.clone(),
        }
    }
}

pub struct Reducer;
impl Reducer {
    pub fn whnf(expr: Expr) -> Expr {
        let mut env = Env::new();
        Thunk::reduce(expr, &mut env)
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    BoundVar(Rc<BoundVar>),
    FreeVar(Rc<FreeVar>),
    Lambda(Rc<Lambda>),
    App(Box<Expr>, Box<Expr>),
    Thunk(Rc<Thunk>),
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut names: HashMap<*const BoundVar, String> = HashMap::new();
        let mut next = String::from("a");
        self.pp(f, &mut names, &mut next, 0)
    }
}

impl Expr {
    fn prec(&self) -> u8 {
        match self {
            Expr::BoundVar(_) | Expr::FreeVar(_) => 2,
            Expr::App(..) => 1,
            Expr::Lambda(_) => 0,
            Expr::Thunk(_) => 2,
        }
    }

    fn pp(
        &self,
        f: &mut fmt::Formatter<'_>,
        names: &mut HashMap<*const BoundVar, String>,
        next: &mut String,
        min_prec: u8,
    ) -> fmt::Result {
        if let Expr::Thunk(..) = self {
            return write!(f, "<unevaluated-thunk>");
        }

        let needs_parens = self.prec() < min_prec;
        if needs_parens {
            write!(f, "(")?;
        }
        match self {
            Expr::BoundVar(bv) => match names.get(&Rc::as_ptr(bv)) {
                Some(name) => write!(f, "{name}")?,
                None => write!(f, "_")?,
            },
            Expr::FreeVar(fv) => {
                if fv.name.starts_with("fv_") {
                    write!(f, "{}", fv.name)?
                } else {
                    write!(f, "fv_{}", fv.name)?
                }
            }
            Expr::Lambda(l) => {
                let name = fresh(next);
                if let Some(w) = l.associated_bound_var() {
                    names.insert(Weak::as_ptr(&w), name.clone());
                }
                write!(f, "\\{name} ")?;
                l.body.pp(f, names, next, 0)?;
            }
            Expr::App(lhs, rhs) => {
                lhs.pp(f, names, next, 1)?;
                write!(f, " ")?;
                rhs.pp(f, names, next, 2)?;
            }
            Expr::Thunk(_) => unreachable!("handled above"),
        }
        if needs_parens {
            write!(f, ")")?;
        }
        Ok(())
    }
}

fn fresh(next: &mut String) -> String {
    let name = next.clone();
    if next.ends_with('z') {
        *next = "a".repeat(next.len() + 1);
    } else {
        let mut bytes = std::mem::take(next).into_bytes();
        let n = bytes.len();
        bytes[n - 1] += 1;
        *next = String::from_utf8(bytes).unwrap();
    }
    name
}
