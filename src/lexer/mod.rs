
use astray::{Parsable, TokenIter, ParseError, ConsumableToken};
use logos::{Logos, Lexer};


#[derive(Logos, Debug, Clone, PartialEq, Hash, Eq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex(r"[a-zA-Z]+")]
    RawIdentifier,
    Identifier(String),
    #[regex("&&")]
    DoubleAmpersand,
    #[regex(r"\|\|")]
    DoublePipe,
    #[regex("=")]
    Equals,
    #[regex("==")]
    DoubleEquals,
    #[regex("!=")]
    BangAndEquals,
    #[regex("<=")]
    LessThanOrEqual,
    #[regex("<")]
    LessThan,
    #[regex(">=")]
    GreaterThanOrEqual,
    #[regex(">")]
    GreaterThan,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LCurly,
    #[token("}")]
    RCurly,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Asterisk,
    #[token("/")]
    ForwardSlash,
    #[token("int")]
    KwInt,
    #[token("float")]
    KwFloat,
    #[regex("return")]
    KwReturn,
    #[token(";")]
    Semicolon,
    #[token("~")]
    Tilde,
    #[token("!")]
    Bang,
    #[regex("([1-9][0-9]*)|0")]
    RawNumber,
    Number(u32),
}

pub enum LexError {
    UnexpectedToken(String)
}


pub fn lex(input: &str) -> Result<TokenIter<Token>, LexError> {
    let mut lex = Token::lexer(input);
    let mut result = vec![];
    while let Some(token) = lex.next() {
        let token = match token {
            Ok(Token::RawIdentifier) => {
                let contents = lex.slice();
                Token::Identifier(contents.to_owned())
            }
            Ok(Token::RawNumber) => {
                let contents = lex.slice();
                Token::Number(contents.parse().unwrap())
            }
            Ok(token) => token,
            err => return Err(LexError::UnexpectedToken("Could not match any token".to_owned())),
        };
        result.push(token)
    }
    Ok(TokenIter::new(result))
}

impl std::fmt::Display  for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Token::RawIdentifier => unimplemented!(),
            Token::Identifier(ident) => ident.clone(),
            Token::LParen => unimplemented!(),
            Token::RParen => unimplemented!(),
            Token::LCurly => unimplemented!(),
            Token::RCurly => unimplemented!(),
            Token::Plus => unimplemented!(),
            Token::Minus => unimplemented!(),
            Token::Asterisk => unimplemented!(),
            Token::ForwardSlash => unimplemented!(),
            Token::KwInt => unimplemented!(),
            Token::KwFloat => unimplemented!(),
            Token::KwReturn => unimplemented!(),
            Token::Semicolon => unimplemented!(),
            Token::RawNumber => unimplemented!(),
            Token::Number(number) => number.to_string(),
            Token::Tilde => unimplemented!(),
            Token::Bang => unimplemented!(),
            Token::DoubleAmpersand => unimplemented!(),
            Token::DoublePipe => unimplemented!(),
            Token::DoubleEquals => unimplemented!(),
            Token::BangAndEquals => unimplemented!(),
            Token::LessThanOrEqual => unimplemented!(),
            Token::LessThan => unimplemented!(),
            Token::GreaterThanOrEqual => unimplemented!(),
            Token::GreaterThan => unimplemented!(),
            Token::Equals => todo!(),
        };
        write!(f, "{str}")
    }
}

impl ConsumableToken for Token {}

impl Parsable<Token> for Token {
    fn parse(iter: &mut TokenIter<Token>) -> Result<Self, ParseError<Token>>
    where
        Self: Sized,
    {
        match iter.consume() {
            Some(token) => Ok(token),
            None => Err(ParseError::no_more_tokens(iter.current)),
        }
    }

    fn parse_if_match<F: Fn(&Token) -> bool>(
        iter: &mut TokenIter<Token>,
        matches: F,
    ) -> Result<Self, ParseError<Token>>
    where
        Self: Sized,
    {
        match iter.consume() {
            Some(ref found) if matches(found) => Ok(found.clone()),
            Some(ref found) => Err(ParseError::parsed_but_unmatching(
                iter.current,
                found
            )),
            _ => Err(ParseError::no_more_tokens(iter.current)),
        }
    }
}

