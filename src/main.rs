mod expr;
mod parser;
mod reducer;
mod substituer;

use expr::Expr;
use parser::Parser;
use reducer::Reducer;
use std::{io, println};

fn main() -> Result<(), String> {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|_| "Expected to read a line")?;

    let expr = Parser::parse(&line)?;
    let reducer = Reducer::new();
    println!(
        "Reduced expression (NF) {} and (WHNF) {}",
        reducer.reduce_nf(&expr),
        reducer.reduce_whnf(&expr)
    );
    Ok(())
}
