use std::marker::PhantomData;

use astray::*;
use crate::lexer::Token;

set_token!(Token);


#[derive(SN, Debug, PartialEq)]
pub struct AST{
    pub items: Vec<Item>,
}

impl AST {
    pub fn is_valid(&self) -> bool {
        !self.items.is_empty()
    }
}


#[derive(SN, Debug, PartialEq)]
pub enum Item {
    Function(Function)
}

#[derive(Debug, PartialEq)]
pub struct Between<TToken,T, L, R=L> 
where T: Parsable<TToken>,
L: Parsable<TToken>,
R: Parsable<TToken>,
TToken: ConsumableToken
{
    left: L,
    pub content: T,
    right: R,
    _token: PhantomData<TToken>,
}


impl <TToken, T, L, R> Between<TToken, T, L, R>  
where 
T: Parsable<TToken, ApplyMatchTo = T>,
L: Parsable<TToken>,
R: Parsable<TToken>,
TToken: ConsumableToken,
{

}

impl <TToken, T, L, R> Parsable<TToken> for Between<TToken, T, L, R> 
where 
T: Parsable<TToken, ApplyMatchTo = T>,
L: Parsable<TToken>,
R: Parsable<TToken>,
TToken: ConsumableToken
{
    type ApplyMatchTo = (L, T, R);

    fn parse(iter: &mut TokenIter<TToken>) -> Result<Self, ParseError<TToken>> {
        let left = iter.parse()?;
        let content = iter.parse()?;
        let right = iter.parse()?;
        Ok(Between {left, content, right, _token: PhantomData})
    }

    fn parse_if_match<F: Fn(&Self::ApplyMatchTo) -> bool>(
            iter: &mut TokenIter<TToken>,
            matches: F,
        ) -> Result<Self, ParseError<TToken>>
        where
            Self: Sized {
                let (left, content, right ) = iter.parse_if_match(matches)?;
                Ok(Between {left, content, right, _token: PhantomData})
        
    }
}

#[derive(SN, Debug, PartialEq)]
pub struct Function {
    return_type: Type,
    #[pattern(Token::Identifier(_))]
    pub ident: Token,
    #[pattern((Token::LParen, Token::RParen))]
    args: (Token, Token),
    #[pattern((Token::LCurly, _ , Token::RCurly))]
    pub body: Between<Token,Body,Token>
}


#[derive(SN, Debug, PartialEq)]
pub enum Type {
    #[pattern(Token::KwInt)]
    Int,
    #[pattern(Token::KwInt)]
    Char, 
}

#[derive(SN, PartialEq, Debug)]
pub struct Body {
    pub statements: Vec<Statement>
}


#[derive(SN, PartialEq, Debug)]
pub enum Statement {
    Return(ReturnStatement),
    Declaration(DeclarationStatement),
}

#[derive(SN, PartialEq, Debug)]
pub struct DeclarationStatement {
    #[pattern(Token::KwInt)]
    ty_kw: Token,
    #[pattern(Token::Identifier(_))]
    pub ident: Token,
    #[pattern((Token::Equals, _))]
    pub initialization: Option<(Token, Expression)>,
    #[pattern(Token::Semicolon)]
    semi: Token,
}

#[derive(SN, PartialEq, Debug)]
pub struct ReturnStatement {
    #[pattern(Token::KwReturn)]
    return_kw: Token,
    pub expr: Expression,
    #[pattern(Token::Semicolon)]
    semi: Token,
}


#[derive(SN, PartialEq, Debug)]
pub enum Expression {
    AssignExpression(AssignExpression),
    OrExpression(AssignExpression),
}

#[derive(SN, PartialEq, Debug)]
pub struct AssignExpression {
    #[pattern(Token::Identifier(_))]
    pub variable: Token,
    #[pattern(Token::Equals)]
    equals: Token,
    pub expr: Box<Expression>
}


#[derive(SN, PartialEq, Debug)]
pub struct OrExpression {
    pub left: AndExpression,
    #[pattern((Operator::LogOr, _))]
    pub others: Vec<(Operator, AndExpression)>
}

#[derive(SN, PartialEq, Debug)]
pub struct AndExpression {
    pub left: EqualityExpression,
    #[pattern((Operator::LogAnd,_))]
    pub others: Vec<(Operator, EqualityExpression)>
}

#[derive(SN, PartialEq, Debug)]
pub struct EqualityExpression {
    pub left: RelationalExpression,
    #[pattern((Operator::LogEqual | Operator::NotEqual, _))]
    pub others: Vec<(Operator, RelationalExpression)>
}

#[derive(SN, PartialEq, Debug)]
pub struct RelationalExpression {
    pub left: ArithExpr,
    #[pattern((Operator::LessThan | Operator::LessThanOrEqual | Operator::GreaterThan | Operator::GreaterThanOrEqual, _))]
    pub others: Vec<(Operator, RelationalExpression)>
}


#[derive(SN, Debug, PartialEq)]
pub struct ArithExpr {
    pub left: Term,
    #[pattern((Operator::Add | Operator::Sub, _))]
    pub others: Vec<(Operator, Term)>,
}

#[derive(SN, Debug, PartialEq)]
pub struct Term {
    pub left: Factor,
    #[pattern((Operator::Mult | Operator::Div, _))]
    pub others: Vec<(Operator, Factor)>,
}

#[derive(SN, Debug, PartialEq)]
pub enum Factor {
    Expression(Box<ParenthesizedExpression>),
    Unary((UnaryOperator,Box<Factor>)),
    #[pattern(Token::Number(_))]
    Number(Token),
    #[pattern(Token::Identifier(_))]
    Identifier(Token),
}


#[derive(SN, Debug, PartialEq)]
pub enum UnaryOperator {
    #[pattern(Token::Minus)]
    ArithNegation,
    #[pattern(Token::Tilde)]
    BitwiseComplement,
    #[pattern(Token::Bang)]
    LogicalNegation,
}

#[derive(Debug, PartialEq)]
pub struct ParenthesizedExpression {
    pub expr: Expression,
}

impl Parsable<Token> for ParenthesizedExpression {
    type ApplyMatchTo = Self;

    fn parse(iter: &mut TokenIter<Token>) -> Result<Self, ParseError<Token>> {
        let _r: Token = iter.parse_if_match(|tok| matches!(tok, Token::LParen))?;
        let expr = iter.parse()?;
        let _l: Token = iter.parse_if_match(|tok| matches!(tok, Token::RParen))?;
        Ok(ParenthesizedExpression { expr })
    }
}

#[derive(SN, Debug, Clone, PartialEq)]
pub enum Operator {
    #[pattern(Token::Plus)]
    Add,
    #[pattern(Token::Asterisk)]
    Mult,
    #[pattern(Token::ForwardSlash)]
    Div,
    #[pattern(Token::Minus)]
    Sub,
    #[pattern(Token::DoubleAmpersand)]
    LogAnd,
    #[pattern(Token::DoublePipe)]
    LogOr,
    #[pattern(Token::DoubleEquals)]
    LogEqual,
    #[pattern(Token::BangAndEquals)]
    NotEqual,
    #[pattern(Token::LessThan)]
    LessThan,
    #[pattern(Token::LessThanOrEqual)]
    LessThanOrEqual,
    #[pattern(Token::GreaterThan)]
    GreaterThan,
    #[pattern(Token::GreaterThanOrEqual)]
    GreaterThanOrEqual,
}