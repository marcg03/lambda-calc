mod expr;
mod parser;
mod reducer;

use parser::Parser;
use reducer::Reducer;
use std::io;

fn main() -> Result<(), String> {
    let mut line = String::new();
    io::stdin()
        .read_line(&mut line)
        .map_err(|_| "Expected to read line")?;
    let parsed_expr = Parser::parse(&line)?;

    let whnf = Reducer::whnf(parsed_expr.clone());
    println!("WHNF: {}", whnf);

    let nf = Reducer::nf(parsed_expr);
    println!("NF:   {}", nf);
    Ok(())
}
