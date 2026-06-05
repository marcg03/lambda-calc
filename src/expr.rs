use std::cmp::{Ord, Ordering};
use std::collections::{BTreeSet, HashMap};
use std::fmt;
use std::rc::{Rc, Weak};

pub struct Lambda {
    pub body: Rc<Expr>,
}

pub struct FreeVar {
    pub name: String,
}

#[derive(Clone)]
pub enum Expr {
    BoundVar(Weak<Lambda>),
    FreeVar(Rc<FreeVar>),
    Lambda(Rc<Lambda>),
    App(Rc<Expr>, Rc<Expr>),
}

struct BvData {
    pub depth: u32,
    pub bv: Weak<Lambda>,
}

impl PartialEq for BvData {
    fn eq(&self, other: &Self) -> bool {
        self.depth == other.depth && Weak::ptr_eq(&self.bv, &other.bv)
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
        self.depth
            .cmp(&other.depth)
            .then_with(|| self.bv.as_ptr().cmp(&other.bv.as_ptr()))
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
        Expr::BoundVar(lambda) => {
            let name = names
                .get(&Weak::as_ptr(lambda))
                .expect("Expected named lambda");
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
    }
}

pub fn name_lambdas(
    expr: &Expr,
    names: &mut HashMap<*const Lambda, String>,
    parents: &HashMap<*const Lambda, Weak<Lambda>>,
) {
    match expr {
        Expr::Lambda(lambda) => {
            ensure_named(lambda, names, parents);
            name_lambdas(&lambda.body, names, parents);
        }
        Expr::App(l, r) => {
            name_lambdas(l, names, parents);
            name_lambdas(r, names, parents);
        }
        _ => {}
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
            if let Some(&binder_depth) = depths.get(&bv.as_ptr()) {
                bound_vars.insert(BvData {
                    depth: binder_depth,
                    bv: Weak::clone(bv),
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
                parents.insert(Rc::as_ptr(lambda), Weak::clone(&nearest.bv));
            }
        }
        Expr::App(l, r) => {
            collect_parents(l, bound_vars, parents, depths, depth);
            collect_parents(r, bound_vars, parents, depths, depth);
        }
    }
}
