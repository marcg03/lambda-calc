mod expr;
mod parser;

use expr::{Expr, FreeVar, Lambda};
use parser::Parser;
use std::io;

fn main() -> Result<(), String> {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|_| "Expected to read line")?;
    let expr = Parser::parse(&line)?;
    println!("{}", expr);
    Ok(())
}
