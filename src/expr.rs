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
        if let Expr::Thunk(thunk) = self {
            return write!(f, "<thunk>({})", thunk.expr());
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

#[cfg(test)]
mod test {
    //! Tests for `expr.rs`: the pretty-printer (`Display`) and call-by-need
    //! WHNF reduction via `Reducer::whnf`.
    //!
    //! Most tests go through the parser (round-trip: parse -> whnf -> Display)
    //! because `BoundVar` identity is pointer-based and awkward to build by hand.

    use crate::expr::{Expr, Reducer};
    use crate::parser::Parser;

    fn parse(s: &str) -> Expr {
        Parser::parse(s).unwrap_or_else(|e| panic!("failed to parse {s:?}: {e}"))
    }

    /// parse -> whnf -> pretty-print
    fn whnf(s: &str) -> String {
        Reducer::whnf(parse(s)).to_string()
    }

    // ---------- pretty-printer ----------

    #[test]
    fn pp_free_vars_get_fv_prefix() {
        assert_eq!(parse("x").to_string(), "fv_x");
    }

    #[test]
    fn pp_already_prefixed_free_var_is_not_double_prefixed() {
        assert_eq!(parse("fv_x").to_string(), "fv_x");
    }

    #[test]
    fn pp_binders_are_renamed_alphabetically() {
        assert_eq!(parse("\\x \\y x y").to_string(), "\\a \\b a b");
    }

    #[test]
    fn pp_unused_binder_still_consumes_a_name() {
        // `x` is never referenced, so the body var can't be attributed to it,
        // but the binder itself still prints as `\a`.
        assert_eq!(parse("\\x y").to_string(), "\\a fv_y");
    }

    #[test]
    fn pp_application_is_left_assoc_without_parens() {
        assert_eq!(parse("a b c").to_string(), "fv_a fv_b fv_c");
    }

    #[test]
    fn pp_right_nested_application_keeps_parens() {
        assert_eq!(parse("a (b c)").to_string(), "fv_a (fv_b fv_c)");
    }

    #[test]
    fn pp_lambda_in_argument_position_is_parenthesized() {
        assert_eq!(parse("\\x x (\\y y)").to_string(), "\\a a (\\b b)");
    }

    #[test]
    fn pp_lambda_in_function_position_is_parenthesized() {
        assert_eq!(parse("(\\x x) y").to_string(), "(\\a a) fv_y");
    }

    #[test]
    fn pp_shadowed_binders_get_distinct_names() {
        assert_eq!(parse("\\x \\x x").to_string(), "\\a \\b b");
    }

    // ---------- basic reduction ----------

    #[test]
    fn whnf_of_free_var_is_itself() {
        assert_eq!(whnf("x"), "fv_x");
    }

    #[test]
    fn whnf_of_lambda_is_itself() {
        assert_eq!(whnf("\\x x"), "\\a a");
    }

    #[test]
    fn identity_application() {
        assert_eq!(whnf("(\\x x) y"), "fv_y");
    }

    #[test]
    fn k_combinator_selects_first_argument() {
        assert_eq!(whnf("(\\x \\y x) a b"), "fv_a");
    }

    #[test]
    fn ki_combinator_selects_second_argument() {
        assert_eq!(whnf("(\\x \\y y) a b"), "fv_b");
    }

    #[test]
    fn nested_redexes_in_function_position() {
        // ((\x x) (\y y)) z  =>  z
        assert_eq!(whnf("((\\x x) (\\y y)) z"), "fv_z");
    }

    #[test]
    fn church_true_selects_then_branch() {
        // (\p p x y) TRUE  =>  x
        assert_eq!(whnf("(\\p p x y) (\\t \\f t)"), "fv_x");
    }

    #[test]
    fn church_false_selects_else_branch() {
        assert_eq!(whnf("(\\p p x y) (\\t \\f f)"), "fv_y");
    }

    #[test]
    fn s_k_k_behaves_like_identity() {
        // S K K v  =>  v
        assert_eq!(
            whnf("(\\x \\y \\z x z (y z)) (\\a \\b a) (\\c \\d c) v"),
            "fv_v"
        );
    }

    #[test]
    fn argument_is_substituted_at_every_occurrence() {
        // (\f f (f x)) with a free head: (\f f (f x)) g  =>  g <thunk of (g x)>
        // The head must be g; the argument stays an unforced thunk under WHNF.
        let e = Reducer::whnf(parse("(\\f f (f x)) g"));
        let Expr::App(l, r) = e else {
            panic!("expected App, got {e:?}")
        };
        match *l {
            Expr::FreeVar(ref fv) => assert_eq!(fv.name, "g"),
            ref other => panic!("expected head g, got {other:?}"),
        }
        assert!(matches!(*r, Expr::Thunk(_)), "argument should stay lazy");
    }

    // ---------- WHNF-ness: no reduction under binders ----------

    #[test]
    fn whnf_does_not_reduce_under_lambda() {
        // The redex (\y y) z sits under a binder and must survive.
        assert_eq!(whnf("\\x (\\y y) z"), "\\a (\\b b) fv_z");
    }

    // ---------- laziness (call-by-need) ----------

    // omega = (\w w w) (\w w w) diverges if ever forced.
    const OMEGA: &str = "((\\w w w) (\\w w w))";

    #[test]
    fn unused_argument_is_never_forced() {
        // K v omega  =>  v, without touching omega.
        assert_eq!(whnf(&format!("(\\x \\y x) v {OMEGA}")), "fv_v");
    }

    #[test]
    fn argument_to_unreferenced_param_is_never_forced() {
        // (\x y) omega  =>  y; the binder has no bound var at all.
        assert_eq!(whnf(&format!("(\\x y) {OMEGA}")), "fv_y");
    }

    #[test]
    fn y_combinator_unfolds_lazily_to_whnf() {
        // Y g  =>  g (Y g); with free g this must terminate at head `g`.
        let e = Reducer::whnf(parse("(\\f (\\x f (x x)) (\\x f (x x))) g"));
        let Expr::App(l, _) = e else {
            panic!("expected App, got {e:?}")
        };
        match *l {
            Expr::FreeVar(ref fv) => assert_eq!(fv.name, "g"),
            ref other => panic!("expected head g, got {other:?}"),
        }
    }

    #[test]
    fn application_of_free_var_leaves_argument_lazy() {
        let e = Reducer::whnf(parse("f ((\\x x) y)"));
        let Expr::App(l, r) = e else {
            panic!("expected App, got {e:?}")
        };
        assert!(matches!(*l, Expr::FreeVar(_)));
        assert!(
            matches!(*r, Expr::Thunk(_)),
            "argument of a stuck application should not be evaluated"
        );
    }

    // ---------- shadowing through reduction ----------

    #[test]
    fn shadowed_binder_reduces_correctly() {
        // (\x \x x) a b: the body x belongs to the inner binder, so result is b.
        assert_eq!(whnf("(\\x \\x x) a b"), "fv_b");
    }

    #[test]
    fn no_variable_capture_via_pointer_identity() {
        // (\x \y y x) z w: even reusing source-level names elsewhere can't
        // capture, since binding is by BoundVar pointer. Head must be w.
        let e = Reducer::whnf(parse("(\\x \\y y x) z w"));
        let Expr::App(l, _) = e else {
            panic!("expected App, got {e:?}")
        };
        match *l {
            Expr::FreeVar(ref fv) => assert_eq!(fv.name, "w"),
            ref other => panic!("expected head w, got {other:?}"),
        }
    }

    // ---------- stability / idempotence ----------

    #[test]
    fn whnf_is_idempotent_on_its_output() {
        for src in ["(\\x x) y", "(\\x \\y x) a b", "\\x (\\y y) z", "a b c"] {
            let once = Reducer::whnf(parse(src));
            let rendered_once = once.to_string();
            let twice = Reducer::whnf(once);
            assert_eq!(twice.to_string(), rendered_once, "not stable for {src:?}");
        }
    }

    // ---------- documented current behavior (quirks worth knowing about) ----------

    #[test]
    fn partial_application_currently_returns_an_opaque_thunk() {
        // (\x \y x) a is a partial application whose WHNF is morally `\y a`,
        // but the current reducer returns it wrapped as Expr::Thunk (a closure:
        // lambda + captured env), which Display renders opaquely.
        // If you later teach whnf to unwrap top-level lambda thunks, update this.
        let e = Reducer::whnf(parse("(\\x \\y x) a"));
        assert!(
            matches!(e, Expr::Thunk(_)),
            "expected the closure-thunk quirk, got {e:?}"
        );
        assert_eq!(e.to_string(), "<unevaluated-thunk>");
    }

    #[test]
    fn lambda_result_of_any_beta_step_is_also_an_opaque_thunk() {
        // Even (\x x) (\y y): while reducing the body `x`, the env holds
        // {x -> \y y}, so the Lambda branch of `reduce` sees a non-empty env
        // and wraps the result in a Thunk — although this particular lambda
        // doesn't reference anything in the env. Morally the WHNF is `\a a`.
        // If you make `reduce` smarter (e.g. only close over the env when the
        // lambda actually captures from it, or substitute on the way out),
        // change this to: assert_eq!(whnf("(\\x x) (\\y y)"), "\\a a");
        let e = Reducer::whnf(parse("(\\x x) (\\y y)"));
        assert!(
            matches!(e, Expr::Thunk(_)),
            "expected the closure-thunk quirk, got {e:?}"
        );
        assert_eq!(e.to_string(), "<unevaluated-thunk>");
    }
}
