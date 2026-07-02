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

#[cfg(test)]
mod tests {
    use super::*;

    // --- term builders -------------------------------------------------

    /// A fresh, distinct bound variable. Its back-edge to a Lambda is left
    /// dangling because `reduce` never reads BoundVar -> Lambda, only the
    /// Lambda -> BoundVar weak and the occurrence's Rc pointer.
    fn fresh_bv() -> Rc<BoundVar> {
        Rc::new(BoundVar::default())
    }

    /// λ binding `bv`, with the given body. Wires the binder so that
    /// `Weak::as_ptr(associated_bound_var)` == `Rc::as_ptr(var(&bv))`.
    fn lam(bv: &Rc<BoundVar>, body: Expr) -> Expr {
        let l = Rc::new(Lambda::new(body));
        l.set_bound_var(Rc::downgrade(bv));
        Expr::Lambda(l)
    }

    fn var(bv: &Rc<BoundVar>) -> Expr {
        Expr::BoundVar(bv.clone())
    }

    fn free(name: &str) -> Expr {
        Expr::FreeVar(Rc::new(FreeVar {
            name: name.to_string(),
        }))
    }

    fn app(l: Expr, r: Expr) -> Expr {
        Expr::App(Box::new(l), Box::new(r))
    }

    /// Peel any thunk wrapping and report a FreeVar's name, forcing as needed.
    fn free_name(e: &Expr) -> Option<String> {
        match e {
            Expr::FreeVar(fv) => Some(fv.name.clone()),
            Expr::Thunk(t) => {
                t.force();
                free_name(&t.expr())
            }
            _ => None,
        }
    }

    // --- tests ---------------------------------------------------------

    #[test]
    fn identity_applied_to_free_var() {
        // (\x. x) f  ==>  f
        let x = fresh_bv();
        let id = lam(&x, var(&x));
        let result = Reducer::whnf(app(id, free("f")));
        assert_eq!(free_name(&result).as_deref(), Some("f"));
    }

    #[test]
    fn k_combinator_selects_first_arg() {
        // (\x. \y. x) f g  ==>  f
        let x = fresh_bv();
        let y = fresh_bv();
        let k = lam(&x, lam(&y, var(&x)));
        let result = Reducer::whnf(app(app(k, free("f")), free("g")));
        assert_eq!(free_name(&result).as_deref(), Some("f"));
    }

    #[test]
    fn second_arg_selected() {
        // (\x. \y. y) f g  ==>  g
        let x = fresh_bv();
        let y = fresh_bv();
        let k2 = lam(&x, lam(&y, var(&y)));
        let result = Reducer::whnf(app(app(k2, free("f")), free("g")));
        assert_eq!(free_name(&result).as_deref(), Some("g"));
    }

    #[test]
    fn argument_is_reduced_when_used() {
        // (\x. x) ((\y. y) f)  ==>  f
        let x = fresh_bv();
        let y = fresh_bv();
        let id_x = lam(&x, var(&x));
        let id_y = lam(&y, var(&y));
        let result = Reducer::whnf(app(id_x, app(id_y, free("f"))));
        assert_eq!(free_name(&result).as_deref(), Some("f"));
    }

    #[test]
    fn unused_argument_is_not_evaluated() {
        // (\z. a) Ω  ==>  a   — must NOT diverge.
        // If the reducer were strict, this test would hang forever.
        let z = fresh_bv();
        let const_a = lam(&z, free("a"));

        // Ω = (\x. x x) (\x. x x)
        let x = fresh_bv();
        let omega = lam(&x, app(var(&x), var(&x)));
        let big = app(omega.clone(), omega);

        let result = Reducer::whnf(app(const_a, big));
        assert_eq!(free_name(&result).as_deref(), Some("a"));
    }

    #[test]
    fn neutral_application_is_stuck() {
        // f g  ==>  f g   (head is free, nothing to apply)
        let result = Reducer::whnf(app(free("f"), free("g")));
        match result {
            Expr::App(l, r) => {
                assert_eq!(free_name(&l).as_deref(), Some("f"));
                assert_eq!(free_name(&r).as_deref(), Some("g"));
            }
            other => panic!("expected a stuck application, got: {other}"),
        }
    }

    #[test]
    fn lambda_is_already_whnf() {
        // \x. x is already in WHNF; reducing returns a lambda unchanged.
        let x = fresh_bv();
        let id = lam(&x, var(&x));
        match Reducer::whnf(id) {
            Expr::Lambda(_) => {}
            other => panic!("expected lambda, got: {other}"),
        }
    }

    #[test]
    fn nested_redex_in_argument_position() {
        // (\x. \y. y) g ((\z. z) f)  ==>  ((\z. z) f) ... no:
        // head (\x.\y.y) g  ==>  \y. y, then applied to ((\z.z) f)  ==>  f
        let x = fresh_bv();
        let y = fresh_bv();
        let z = fresh_bv();
        let snd = lam(&x, lam(&y, var(&y)));
        let id_z = lam(&z, var(&z));
        let result = Reducer::whnf(app(app(snd, free("g")), app(id_z, free("f"))));
        assert_eq!(free_name(&result).as_deref(), Some("f"));
    }

    #[test]
    fn shared_argument_forced_once() {
        // (\x. x) applied to a thunk should force, but forcing is idempotent:
        // reducing the same result twice must agree. Guards the Forced/Unforced cache.
        let x = fresh_bv();
        let y = fresh_bv();
        let id = lam(&x, var(&x));
        let inner = app(lam(&y, var(&y)), free("f"));
        let result = Reducer::whnf(app(id, inner));
        assert_eq!(free_name(&result).as_deref(), Some("f"));
        // idempotent read
        assert_eq!(free_name(&result).as_deref(), Some("f"));
    }

    #[test]
    fn applied_under_neutral_head_keeps_both_args() {
        // f a b  ==>  f a b  (spine stays intact, left-assoc)
        let result = Reducer::whnf(app(app(free("f"), free("a")), free("b")));
        match result {
            Expr::App(l, r) => {
                assert_eq!(free_name(&r).as_deref(), Some("b"));
                match *l {
                    Expr::App(ref ll, ref lr) => {
                        assert_eq!(free_name(ll).as_deref(), Some("f"));
                        assert_eq!(free_name(lr).as_deref(), Some("a"));
                    }
                    ref other => panic!("expected nested app, got {other}"),
                }
            }
            other => panic!("expected spine, got {other}"),
        }
    }

    #[test]
    fn display_of_identity_is_stable() {
        let x = fresh_bv();
        let id = lam(&x, var(&x));
        assert_eq!(format!("{id}"), "\\a a");
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut names: HashMap<*const BoundVar, String> = HashMap::new();
        let mut next = String::from("a");
        self.pp(f, &mut names, &mut next, 0)
    }
}

impl Expr {
    /// Precedence of this node: atom = 2, application = 1, lambda = 0.
    fn prec(&self) -> u8 {
        match self {
            Expr::BoundVar(_) | Expr::FreeVar(_) => 2,
            Expr::App(..) => 1,
            Expr::Lambda(_) => 0,
            Expr::Thunk(_) => 2, // never used; thunks are transparent in `pp`
        }
    }

    /// Print, wrapping in parens if this node binds looser than `min_prec`.
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
                None => write!(f, "_")?, // bound var with no binder in scope
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
                l.body.pp(f, names, next, 0)?; // body never needs parens
            }
            Expr::App(lhs, rhs) => {
                lhs.pp(f, names, next, 1)?; // fn position: parenthesize lambdas
                write!(f, " ")?;
                rhs.pp(f, names, next, 2)?; // arg position: parenthesize app/lambda
            }
            Expr::Thunk(_) => unreachable!("handled above"),
        }
        if needs_parens {
            write!(f, ")")?;
        }
        Ok(())
    }
}

/// Fresh variable name: a, b, …, z, aa, ab, … (your `get_next` scheme).
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
