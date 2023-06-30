use astray::{TokenIter, Parsable, ParseError};

use crate::lexer::Token;

use self::grammar::Program;

pub mod grammar;

pub fn parse_program(iter: &mut TokenIter<Token>) -> Result<Program, ParseError<Token>> {
    Program::parse(iter)
}
