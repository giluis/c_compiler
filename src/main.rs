#![feature(let_chains)]
use std::{fs::read_to_string, process::Command};

use parser::parse_program;
use lexer::lex;

use codegen::GenAsm;

mod lexer;
mod parser;
mod codegen;
mod checker;


fn main() {
    let output_url = "./program.s";
    let source = std::env::args().nth(1).expect("Please introduce an argument");
    // let source = "tests/stage_4/invalid/missing_first_op.c";
    let source = read_to_string(source).expect("Could not read file");
    let mut token_iter = match lex(&source) {
        Ok(token_iter) => token_iter, 
        Err(_) => {println!("Lexer failure"); return},
    };
    let program = parse_program(&mut token_iter);
    // println!("{:?}", program);
    match program {
        Ok(program) if program.is_valid() => {
            // println!("{:#?}", program);
            let asm_result = program.compile(); 
            match asm_result {
                Ok(asm) => std::fs::write(output_url, asm.0).expect("Could not write to URL"),
                Err(_) => println!("Failed to compile program"),
            }
            ;
        },
        _ => println!("Failed?"), // TODO: Failed to compile the program
    }
}