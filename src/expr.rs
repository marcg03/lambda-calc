use std::cell::RefCell;
use std::cmp::{Ord, Ordering};
use std::collections::{BTreeSet, HashMap};
use std::fmt;
use std::rc::{Rc, Weak};

#[derive(Default, Debug)]
pub struct BoundVar {
    lambda: Rc<RefCell<Weak<Lambda>>>,
}

impl BoundVar {
    pub fn new(lambda: Weak<Lambda>) -> Self {
        Self {
            lambda: Rc::new(RefCell::new(lambda)),
        }
    }

    pub fn associated_lambda(&self) -> Weak<Lambda> {
        self.lambda.borrow().clone()
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
    env: Env,
}

impl Thunk {
    fn new(expr: Expr, env: Env) -> Self {
        Self {
            state: RefCell::new(ThunkState::Unforced(expr)),
            env,
        }
    }

    fn find(bv: *const BoundVar, env: &Env) -> Option<Expr> {
        env.get(&bv).cloned()
    }

    fn reduce(expr: Expr, env: &mut Env) -> Expr {
        match expr {
            Expr::BoundVar(ref bv) => {
                if let Some(val) = Self::find(Rc::as_ptr(bv), env) {
                    Self::reduce(val, env)
                } else {
                    expr
                }
            }
            Expr::FreeVar(..) => expr,
            Expr::Lambda(..) => expr,
            Expr::App(l, r) => {
                let l_reduced = Self::reduce(*l, env);
                if let Expr::Lambda(l) = l_reduced {
                    let bv = if let Some(bv) = l.associated_bound_var() {
                        bv.upgrade().expect("expected boundvar to exist")
                    } else {
                        return Self::reduce(l.body.clone(), env);
                    };

                    // wrap `r` in a thunk (snapshot crt env)
                    let thunk = Expr::Thunk(Rc::new(Self::new(*r, env.clone())));
                    // add that thunk to env
                    env.insert(Rc::as_ptr(&bv), thunk);
                    // reduce the body of the lambda with the new env
                    let expr = Self::reduce(l.body.clone(), env);
                    env.remove(&Rc::as_ptr(&bv));
                    expr
                } else {
                    let thunk = Expr::Thunk(Rc::new(Self::new(*r, env.clone())));
                    Expr::App(Box::new(l_reduced), Box::new(thunk))
                }
            }
            Expr::Thunk(thunk) => Self::reduce(thunk.get(), env),
        }
    }

    fn get(&self) -> Expr {
        let mut state = self.state.borrow_mut();
        let expr = match &*state {
            ThunkState::Forced(expr) => return expr.clone(),
            ThunkState::Unforced(expr) => expr.clone(),
        };
        let mut env = self.env.clone(); // PERF: this could be really slow
        let expr = Self::reduce(expr, &mut env);
        *state = ThunkState::Forced(expr.clone());
        expr
    }

    fn get_unforced(&self) -> Expr {
        let state = self.state.borrow();
        match &*state {
            ThunkState::Forced(expr) => expr.clone(),
            ThunkState::Unforced(expr) => expr.clone(),
        }
    }

    fn whnf(expr: Expr) -> Expr {
        let mut env = Env::new();
        let expr = Self::reduce(expr, &mut env);
        expr
    }
}

pub struct Reducer;

impl Reducer {
    pub fn whnf(expr: Expr) -> Expr {
        Thunk::whnf(expr)
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

struct BvData {
    pub depth: u32,
    pub bv: Rc<BoundVar>,
}

impl PartialEq for BvData {
    fn eq(&self, other: &Self) -> bool {
        self.depth == other.depth && Rc::ptr_eq(&self.bv, &other.bv)
    }
}

impl Eq for BvData {}

impl PartialOrd for BvData {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for BvData {
    fn cmp(&self, other: &Self) -> Ordering {
        self.depth.cmp(&other.depth).then_with(|| {
            self.bv
                .associated_lambda()
                .as_ptr()
                .cmp(&other.bv.associated_lambda().as_ptr())
        })
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut bound_vars = BTreeSet::new();
        let mut parents = HashMap::new();
        let mut depths = HashMap::new();
        collect_parents(self, &mut bound_vars, &mut parents, &mut depths, 0);
        let mut names = HashMap::new();
        name_lambdas(self, &mut names, &parents);
        display_expr(self, &names, f)
    }
}

fn display_expr(
    expr: &Expr,
    names: &HashMap<*const Lambda, String>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    match expr {
        Expr::BoundVar(bv) => {
            let name = names
                .get(&Weak::as_ptr(&bv.associated_lambda()))
                .expect("Expected associated lambda");
            write!(f, "{}", name)
        }
        Expr::Lambda(lambda) => {
            let name = names
                .get(&Rc::as_ptr(lambda))
                .expect("Expected named lambda");
            write!(f, "(\\{} ", name)?;
            display_expr(&lambda.body, names, f)?;
            write!(f, ")")
        }
        Expr::FreeVar(fv) => {
            if fv.name.starts_with("fv_") {
                write!(f, "{}", fv.name)
            } else {
                write!(f, "fv_{}", fv.name)
            }
        }
        Expr::App(l, r) => {
            write!(f, "(")?;
            display_expr(l, names, f)?;
            write!(f, " ")?;
            display_expr(r, names, f)?;
            write!(f, ")")
        }
        Expr::Thunk(thunk) => {
            write!(f, "{}", thunk.get_unforced())
        }
    }
}

pub fn name_lambdas(
    expr: &Expr,
    names: &mut HashMap<*const Lambda, String>,
    parents: &HashMap<*const Lambda, Weak<Lambda>>,
) {
    match expr {
        Expr::BoundVar(..) => {}
        Expr::FreeVar(..) => {}
        Expr::Lambda(lambda) => {
            ensure_named(lambda, names, parents);
            name_lambdas(&lambda.body, names, parents);
        }
        Expr::App(l, r) => {
            name_lambdas(l, names, parents);
            name_lambdas(r, names, parents);
        }
        Expr::Thunk(thunk) => {
            name_lambdas(&thunk.get(), names, parents);
        }
    }
}

fn ensure_named(
    lambda: &Rc<Lambda>,
    names: &mut HashMap<*const Lambda, String>,
    parents: &HashMap<*const Lambda, Weak<Lambda>>,
) -> String {
    let key = Rc::as_ptr(lambda);
    if let Some(name) = names.get(&key) {
        return name.clone();
    }
    let name = match parents.get(&key) {
        Some(parent) => {
            let parent_rc = parent.upgrade().expect("parent lambda dropped");
            let parent_name = ensure_named(&parent_rc, names, parents);
            match parent_name.as_bytes().last().unwrap() {
                b'z' => format!("{parent_name}a"),
                &c => format!(
                    "{}{}",
                    &parent_name[..parent_name.len() - 1],
                    char::from(c + 1)
                ),
            }
        }
        None => "a".to_string(),
    };
    names.insert(key, name.clone());
    name
}

fn collect_parents(
    expr: &Expr,
    bound_vars: &mut BTreeSet<BvData>,
    parents: &mut HashMap<*const Lambda, Weak<Lambda>>,
    depths: &mut HashMap<*const Lambda, u32>,
    depth: u32,
) {
    match expr {
        Expr::BoundVar(bv) => {
            if let Some(&binder_depth) = depths.get(&Weak::as_ptr(&bv.associated_lambda())) {
                bound_vars.insert(BvData {
                    depth: binder_depth,
                    bv: Rc::clone(bv),
                });
            }
        }
        Expr::FreeVar(..) => {}
        Expr::Lambda(lambda) => {
            depths.insert(Rc::as_ptr(lambda), depth);
            collect_parents(&lambda.body, bound_vars, parents, depths, depth + 1);
            while bound_vars.last().is_some_and(|x| x.depth >= depth) {
                bound_vars.pop_last();
            }
            if let Some(nearest) = bound_vars.last() {
                parents.insert(
                    Rc::as_ptr(lambda),
                    Weak::clone(&nearest.bv.associated_lambda()),
                );
            }
        }
        Expr::App(l, r) => {
            collect_parents(l, bound_vars, parents, depths, depth);
            collect_parents(r, bound_vars, parents, depths, depth);
        }
        Expr::Thunk(thunk) => {
            collect_parents(&thunk.get(), bound_vars, parents, depths, depth);
        }
    }
}
