use astray::*;
use hatch_result::ResultHatchExt;
use logos::Logos;

#[derive(Logos, Debug, Clone)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token {
    #[regex("[a-zA-Z]+")]
    RawIdentifier,
    Identifier(String),
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
    #[regex("([1-9][0-9]*)|0")]
    RawNumber,
    #[token("int")]
    KwInt,
    #[token("return")]
    KwReturn,
    #[token(";")]
    Semicolon,
    Number(u32),
}

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
            Some(ref found) => Err(ParseError::unmatching_token(
                iter.current,
                "Failed to expected token not found".to_string(),
                found.clone(),
            )),
            _ => Err(ParseError::no_more_tokens(iter.current)),
        }
    }
}

#[derive(Clone, Debug)]
pub struct ArithExpr {
    left: Term,
    others: Vec<(Operator, Term)>,
}

impl Parsable<Token> for ArithExpr {
    fn parse(iter: &mut TokenIter<Token>) -> Result<Self, ParseError<Token>>
    where
        Self: Sized,
    {
        let left = iter.parse()?;
        let others =
            iter.parse_if_match(|tup| matches!(tup, (Operator::Add | Operator::Sub, _)))?;
        Ok(ArithExpr { left, others })
    }
}

#[derive(Debug, Clone)]
pub struct Term {
    left: Factor,
    others: Vec<(Operator, Factor)>,
}

impl Parsable<Token> for Term {
    fn parse(iter: &mut TokenIter<Token>) -> Result<Self, ParseError<Token>>
    where
        Self: Sized,
    {
        let left = iter.parse()?;
        let others =
            iter.parse_if_match(|tup| matches!(tup, (Operator::Mult | Operator::Div, _)))?;
        Ok(Term { left, others })
    }
}

fn lex(input: &str) -> Vec<Token> {
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
            err => panic!("Failed to read token {:?}", err),
        };
        result.push(token)
    }
    result
}

#[derive(Debug, Clone)]
pub enum Factor {
    Expression(Box<ParenthesizedExpression>),
    Number(Token),
    Identifier(String),
}

#[derive(Debug, Clone)]
pub struct ParenthesizedExpression {
    expr: ArithExpr,
}

impl Parsable<Token> for ParenthesizedExpression {
    type ApplyMatchTo = Self;

    fn parse(iter: &mut TokenIter<Token>) -> Result<Self, ParseError<Token>> {
        let r: Token = iter.parse_if_match(|tok| matches!(tok, Token::LParen))?;
        let expr = iter.parse()?;
        let l: Token = iter.parse_if_match(|tok| matches!(tok, Token::RParen))?;
        Ok(ParenthesizedExpression { expr })
    }
}


impl Parsable<Token> for Factor {
    fn parse(iter: &mut TokenIter<Token>) -> Result<Self, ParseError<Token>>
    where
        Self: Sized,
    {
        let expr_err = iter.parse().map(Factor::Expression).hatch()?;
        let num_err = iter
            .parse_if_match(|tok| matches!(tok, Token::Number(_)))
            .map(Factor::Number)
            .hatch()?;

        let ident_err = iter
            .parse_if_match(|tok| matches!(tok, Token::Identifier(_)))
            .map(|ident| match ident {
                Token::Identifier(str) => Factor::Identifier(str),
                _ => unreachable!(),
            })
            .hatch()?;
        Err(ParseError::from_disjunct_errors(
            "Not sure".to_owned(),
            iter.current,
            vec![expr_err, num_err, ident_err],
        ))
    }
}

#[derive(Debug, Clone)]
pub enum Operator {
    Add,
    Mult,
    Div,
    Sub,
}

impl Parsable<Token> for Operator {
    fn parse(iter: &mut TokenIter<Token>) -> Result<Self, ParseError<Token>>
    where
        Self: Sized,
    {
        let asterisk_error = iter
            .parse_if_match(|tok| matches!(tok, Token::Asterisk))
            .map(|_: Token| Operator::Mult)
            .hatch()?;
        let plus_error = iter
            .parse_if_match(|tok| matches!(tok, Token::Plus))
            .map(|_: Token| Operator::Add)
            .hatch()?;
        let minus_error = iter
            .parse_if_match(|tok| matches!(tok, Token::Minus))
            .map(|_: Token| Operator::Sub)
            .hatch()?;
        let slash_error = iter
            .parse_if_match(|tok| matches!(tok, Token::ForwardSlash))
            .map(|_: Token| Operator::Div)
            .hatch()?;
        let branches = vec![asterisk_error, plus_error, minus_error, slash_error];
        Err(ParseError::from_disjunct_errors(
            "Operator".to_owned(),
            iter.current,
            branches,
        ))
    }
}
fn main() {
    let result = lex("(1 + 3) * 2");
    let mut token_iter = TokenIter::new(result);
    let expr: ArithExpr = token_iter.parse().unwrap();
    println!("{:#?}", expr);
}
