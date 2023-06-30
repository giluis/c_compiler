use std::{fs::read_to_string, process::Command};

use parser::parse_program;
use lexer::lex;
use codegen::GenAsm;

mod lexer;
mod parser;
mod codegen;


fn main() {
    let output_url = "./program.s";
    let source_url = std::env::args().nth(1).expect("Please introduce an argument");
    let source = read_to_string(source_url).expect("Could not read file");
    let mut token_iter = lex(&source);
    let program = parse_program(&mut token_iter);
    let asm = program.unwrap().to_asm(); 
    std::fs::write(output_url, asm).expect("Could not write to URL");
}