mod expr;

use expr::{Expr, FreeVar, Lambda};
use std::rc::{Rc, Weak};

fn app(l: Expr, r: Expr) -> Expr {
    Expr::App(Rc::new(l), Rc::new(r))
}

/// λx. x
fn identity() -> Expr {
    let lam = Rc::new_cyclic(|me: &Weak<Lambda>| Lambda {
        body: Rc::new(Expr::BoundVar(me.clone())),
    });
    Expr::Lambda(lam)
}

/// λx. λy. x  (K combinator / Church true)
fn k() -> Expr {
    let lam = Rc::new_cyclic(|x: &Weak<Lambda>| {
        let inner = Rc::new(Lambda {
            body: Rc::new(Expr::BoundVar(x.clone())),
        });
        Lambda {
            body: Rc::new(Expr::Lambda(inner)),
        }
    });
    Expr::Lambda(lam)
}

/// λx. λy. y  (K2 / Church false)
/// outer doesn't need new_cyclic — it never references itself
fn k2() -> Expr {
    let inner = Rc::new_cyclic(|y: &Weak<Lambda>| Lambda {
        body: Rc::new(Expr::BoundVar(y.clone())),
    });
    Expr::Lambda(Rc::new(Lambda {
        body: Rc::new(Expr::Lambda(inner)),
    }))
}

/// λx. (x x)  (self-application ω)
fn self_app() -> Expr {
    let lam = Rc::new_cyclic(|x: &Weak<Lambda>| Lambda {
        body: Rc::new(Expr::App(
            Rc::new(Expr::BoundVar(x.clone())),
            Rc::new(Expr::BoundVar(x.clone())),
        )),
    });
    Expr::Lambda(lam)
}

/// λx. λy. λz. ((x z) (y z))  (S combinator)
fn s() -> Expr {
    let lam = Rc::new_cyclic(|x: &Weak<Lambda>| {
        let y_lam = Rc::new_cyclic(|y: &Weak<Lambda>| {
            let z_lam = Rc::new_cyclic(|z: &Weak<Lambda>| Lambda {
                body: Rc::new(Expr::App(
                    Rc::new(Expr::App(
                        Rc::new(Expr::BoundVar(x.clone())),
                        Rc::new(Expr::BoundVar(z.clone())),
                    )),
                    Rc::new(Expr::App(
                        Rc::new(Expr::BoundVar(y.clone())),
                        Rc::new(Expr::BoundVar(z.clone())),
                    )),
                )),
            });
            Lambda {
                body: Rc::new(Expr::Lambda(z_lam)),
            }
        });
        Lambda {
            body: Rc::new(Expr::Lambda(y_lam)),
        }
    });
    Expr::Lambda(lam)
}

/// λf. λg. λx. (f (g x))  (B combinator / composition)
fn b() -> Expr {
    let lam = Rc::new_cyclic(|f: &Weak<Lambda>| {
        let g_lam = Rc::new_cyclic(|g: &Weak<Lambda>| {
            let x_lam = Rc::new_cyclic(|x: &Weak<Lambda>| Lambda {
                body: Rc::new(Expr::App(
                    Rc::new(Expr::BoundVar(f.clone())),
                    Rc::new(Expr::App(
                        Rc::new(Expr::BoundVar(g.clone())),
                        Rc::new(Expr::BoundVar(x.clone())),
                    )),
                )),
            });
            Lambda {
                body: Rc::new(Expr::Lambda(x_lam)),
            }
        });
        Lambda {
            body: Rc::new(Expr::Lambda(g_lam)),
        }
    });
    Expr::Lambda(lam)
}

/// λf. λx. (f x)  (Church numeral 1)
fn church_one() -> Expr {
    let lam = Rc::new_cyclic(|f: &Weak<Lambda>| {
        let x_lam = Rc::new_cyclic(|x: &Weak<Lambda>| Lambda {
            body: Rc::new(Expr::App(
                Rc::new(Expr::BoundVar(f.clone())),
                Rc::new(Expr::BoundVar(x.clone())),
            )),
        });
        Lambda {
            body: Rc::new(Expr::Lambda(x_lam)),
        }
    });
    Expr::Lambda(lam)
}

/// λf. λx. (f (f x))  (Church numeral 2)
fn church_two() -> Expr {
    let lam = Rc::new_cyclic(|f: &Weak<Lambda>| {
        let x_lam = Rc::new_cyclic(|x: &Weak<Lambda>| Lambda {
            body: Rc::new(Expr::App(
                Rc::new(Expr::BoundVar(f.clone())),
                Rc::new(Expr::App(
                    Rc::new(Expr::BoundVar(f.clone())),
                    Rc::new(Expr::BoundVar(x.clone())),
                )),
            )),
        });
        Lambda {
            body: Rc::new(Expr::Lambda(x_lam)),
        }
    });
    Expr::Lambda(lam)
}

/// λx. (x fv_y)  (lambda applied to a free variable)
fn with_free_var() -> Expr {
    let fv = Rc::new(FreeVar {
        name: "y".to_string(),
    });
    let lam = Rc::new_cyclic(|me: &Weak<Lambda>| Lambda {
        body: Rc::new(Expr::App(
            Rc::new(Expr::BoundVar(me.clone())),
            Rc::new(Expr::FreeVar(fv.clone())),
        )),
    });
    Expr::Lambda(lam)
}

fn main() {
    // -- individual expressions --
    println!("identity:        {}", identity()); // \a a
    println!("K:               {}", k()); // \a \aa a
    println!("K2 (false):      {}", k2()); // \a \a a
    println!("ω (self-app):    {}", self_app()); // \a (a a)
    println!("S:               {}", s()); // \a \aa \aaa ((a aaa) (aa aaa))
    println!("B (compose):     {}", b()); // \a \aa \aaa (a (aa aaa))
    println!("Church 1:        {}", church_one()); // \a \aa (a aa)
    println!("Church 2:        {}", church_two()); // \a \aa (a (a aa))
    println!("with free var:   {}", with_free_var()); // \a (a fv_y)
    println!();

    // -- combinations --
    println!("K id:            {}", app(k(), identity()));
    // (\a \aa a \a a)

    println!("church2 church1: {}", app(church_two(), church_one()));
    // (\a \aa (a (a aa)) \a \aa (a aa))

    println!("Ω  (ω ω):        {}", app(self_app(), self_app()));
    // (\a (a a) \a (a a))

    // S K K — should reduce to the identity under beta reduction
    println!("S K K:           {}", app(app(s(), k()), k()));
    // ((\a \aa \aaa ((a aaa) (aa aaa)) \a \aa a) \a \aa a)

    // B identity church1 — (B id) composes id with c1, so (B id c1) = λx. (id (c1 x))
    println!(
        "B id church1:    {}",
        app(app(b(), identity()), church_one())
    );
}
