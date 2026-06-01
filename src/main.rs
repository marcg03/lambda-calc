mod expr;
mod parser;

use expr::Expr;
use parser::Parser;
use std::{io, println};

fn main() -> Result<(), String> {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|_| "Expected to read a line")?;

    let expr = Parser::parse(&line)?;
    println!("Parsed expression {}", expr);
    Ok(())
}
