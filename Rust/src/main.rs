extern crate scheme_interpreter;

use std::io::{self, Write};

use scheme_interpreter::types::Token;
use scheme_interpreter::tokenizer::tokenize;

fn main() {

    loop {
        print!("scheme> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();

        let input_result = io::stdin().read_line(&mut line);

        match input_result {
            Ok(0) => {
                print!("\n");
                break;
            }
            Ok(_) => {
            }
            Err(s) => {
                panic!("Error: {}", s);
            }
        }
        

        tokenize(&line);

        println!("{}", line);
    }
}

/*
fn tokenize(line: &String) {
    for c in line.as_str().chars() {
        println!("{}", c);
    }
}
*/
