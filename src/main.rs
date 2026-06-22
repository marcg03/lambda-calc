mod expr;
mod parser;

use expr::Reducer;
use parser::Parser;
use std::io;

fn main() -> Result<(), String> {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|_| "Expected to read line")?;
    let parsed_expr = Parser::parse(&line)?;

    println!("Parsed expression: {}", parsed_expr);
    // FIX: this is not really WHNF
    println!("WHNF   expression: {}", Reducer::whnf(parsed_expr));
    Ok(())
}
