use astray::{TokenIter, Parsable, ParseError};

use crate::lexer::Token;

use self::grammar::AST;

pub mod grammar;

pub fn parse_program(iter: &mut TokenIter<Token>) -> Result<AST, ParseError<Token>> {

    AST::parse(iter) 
    
}
