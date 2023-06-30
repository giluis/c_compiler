use std::{
    fmt::Display,
    ops::Add,
    sync::{Mutex, MutexGuard, OnceLock},
};

use crate::{lexer::Token, parser::grammar::*};

use Instruction::*;
use Label::*;
use Reg::*;

struct Asm(String);

impl Asm {
    fn new() -> Asm {
        Asm(String::new())

    }

}

impl Add for Asm {
    type Output = Asm;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.0 = self.0 + "\n" + &rhs.0;
        self
    }
}

impl Add<Instruction> for Asm {
    type Output = Asm;

    fn add(mut self, instruction: Instruction) -> Self::Output {
        self.0 = self.0 + "\n\t" + &instruction.to_string();
        self
    }
}

impl Add<Label> for Asm {
    type Output = Asm;

    fn add(mut self, label: Label) -> Self::Output {
        self.0 = self.0 + "\n" + &label.to_string();
        self
    }
}

impl Add for Instruction {
    type Output = Asm;
    fn add(mut self, instruction: Instruction) -> Self::Output {
        Asm(format!("{instruction}\n{self}"))
    }
}

pub trait GenAsm {
    fn to_asm(&self) -> Asm;
}

impl GenAsm for Program {
    fn to_asm(&self) -> Asm {
        let globals = Asm(self
            .items
            .iter()
            .fold("".to_owned(), |accum, item| match item {
                Item::Function(function) => accum + ".globl " + &format!("{}\n", function.ident),
            }));
        let program = self
            .items
            .iter()
            .map(Item::to_asm)
            .fold(globals, |accum, cur| accum + cur);
        program
    }
}

impl GenAsm for Item {
    fn to_asm(&self) -> Asm {
        match self {
            Item::Function(function) => function.to_asm(),
        }
    }
}

impl GenAsm for Function {
    fn to_asm(&self) -> Asm {
        Asm(format!("{}:", self.ident)) + self.body.content.to_asm()
    }
}

impl GenAsm for Body {
    fn to_asm(&self) -> Asm {
        self.expressions
            .iter()
            .fold(Asm::empty(), |accum, expr| accum + &expr.to_asm())
    }
}

impl GenAsm for Statement {
    fn to_asm(&self) -> Asm {
        match self {
            Statement::Return(return_statement) => return_statement.to_asm(),
        }
    }
}

impl GenAsm for ReturnStatement {
    fn to_asm(&self) -> Asm {
        self.expr.to_asm() + &ret()
    }
}

impl GenAsm for Expression {
    fn to_asm(&self) -> Asm {
        match self {
            Expression::OrExpression(or_expr) => or_expr.to_asm(),
        }
    }
}

impl GenAsm for OrExpression {
    fn to_asm(&self) -> Asm {
        let continue_label = Label::create_continue();
        let end_label = Label::create_end();
        let mut result = self.left.to_asm();
        for (_or_operator, parcel) in self.others.iter() {
            result = result
                + &cmp(Value::Literal(0), RAX)
                + &je(continue_label)
                + &mov(Value::Literal(1), RAX)
                + &jmp(end_label)
                + &continue_label.to_string()
                + &parcel.to_asm()
                + &cmp(Value::Literal(0), RAX)
                + &mov(Value::Literal(0), RAX)
                + &setne(RAX.lower());
        }
        result + &end_label.to_string()
    }
}

impl GenAsm for AndExpression {
    fn to_asm(&self) -> Asm {
        // result in %eax
        let mut result = self.left.to_asm();
        for (and_operator, parcel) in self.others.iter() {
            result += &push(RAX);
            result += &parcel.to_asm();
            result += &pop(RBX);
        }
        result
    }
}

impl GenAsm for EqualityExpression {
    fn to_asm(&self) -> Asm {
        // result in %eax
        let mut result = self.left.to_asm();
        for (equality_operator, parcel) in self.others.iter() {
            result = result
                + &push(RAX)
                + &parcel.to_asm()
                + &pop(RCX)
                + &cmp(Value::Reg(RAX), RCX)
                + &match equality_operator {
                    Operator::LogEqual => sete(RAX),
                    Operator::NotEqual => setne(RAX),
                    _ => unreachable!(
                        "Equality expressions will only have equals and not equals operators"
                    ),
                };
            // code for equality
        }
        result
    }
}

impl GenAsm for RelationalExpression {
    fn to_asm(&self) -> Asm {
        // result in %eax
        let mut result = self.left.to_asm();
        for (relational_operator, parcel) in self.others.iter() {
            result = result
                + &push(RAX)
                + &parcel.to_asm()
                + &pop(RCX)
                + &cmp(Value::Reg(RAX), RCX)
                + &clear(RAX)
                + &match relational_operator {
                    Operator::GreaterThan => setg(RAX),
                    Operator::GreaterThanOrEqual => setge(RAX),
                    Operator::LessThan => setl(RAX),
                    Operator::LessThanOrEqual => setle(RAX),
                    _ => unreachable!(
                        "Relational expressions will only have the operators expressed above"
                    ),
                };
        }
        result
    }
}

impl GenAsm for ArithExpr {
    fn to_asm(&self) -> Asm {
        // result in %eax
        let mut result = self.left.to_asm();
        for (operator, term) in self.others.iter() {
            result += &push(RAX);
            result += &term.to_asm();
            result += &pop(RBX);
            result += &match operator {
                Operator::Add => add(RBX, RAX),
                Operator::Sub => xchg(RAX, RBX) + &sub(RBX, RAX),
                _ => unreachable!("ArithExpr will only have Operator::Add and Operator::Sub"),
            };
        }
        result
    }
}

impl GenAsm for Term {
    fn to_asm(&self) -> Asm {
        let mut result = self.left.to_asm();
        for (operator, term) in self.others.iter() {
            result += &push(RAX);
            result += &term.to_asm();
            result += &pop(RBX);
            result += &match operator {
                Operator::Mult => imul(RBX, RAX),
                Operator::Div => xchg(RAX, RBX) + &idiv(RBX),
                _ => unreachable!("ArithExpr will only have Operator::Add and Operator::Sub"),
            };
        }
        result
    }
}

impl GenAsm for Factor {
    fn to_asm(&self) -> Asm {
        match &self {
            Factor::Expression(other) => other.expr.to_asm(),
            Factor::Unary((unop, other_factor)) => other_factor.to_asm() + &unop.to_asm(),
            Factor::Number(literal) => match literal {
                Token::Number(literal) => mov(Value::Literal(*literal), RAX),
                _ => unreachable!("Factor::Number should only contain Token::Number"),
            },
            Factor::Identifier(_) => todo!("Identifiers not implemented yet"),
        }
    }
}

impl GenAsm for UnaryOperator {
    fn to_asm(&self) -> Asm {
        match &self {
            UnaryOperator::ArithNegation => neg(RAX),
            UnaryOperator::BitwiseComplement => not(RAX),
            UnaryOperator::LogicalNegation => {
                cmp(Value::Literal(0), RAX) + mov(Value::Literal(0), RAX) + &sete(RAX.lower())
            }
        }
    }
}

static CURRENT_LABEL: OnceLock<Mutex<usize>> = OnceLock::new();

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy)]
enum Label {
    CONTINUE(usize),
    END(usize),
}

impl Label {
    fn create_end() -> Self {
        let counter = Self::increment_current();
        Self::END(*counter)
    }

    fn create_continue() -> Self {
        let counter = Self::increment_current();
        Self::CONTINUE(*counter)
    }
    fn increment_current<'a>() -> MutexGuard<'a, usize> {
        let mut current = match CURRENT_LABEL.get_or_init(|| Mutex::new(0)).lock() {
            Ok(val) => val,
            Err(lock) => lock.into_inner(),
        };
        *current += 1;
        current
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let (str, id) = match self {
            Label::CONTINUE(id) => ("continue", id),
            Label::END(id) => ("end", id),
        };
        write!(f, "{str}_{id}:\n")
    }
}

#[allow(clippy::upper_case_acronyms)]
#[derive(Clone, Copy)]
enum Reg {
    RAX,
    AL,
    RBX,
    RCX,
    RDX,
    ESP,
}

impl std::fmt::Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            RAX => "%rax",
            RBX => "%rbx",
            RCX => "%rcx",
            RDX => "%rdx",
            ESP => "%esp",
            AL => "%al",
        };
        write!(f, "{str}")
    }
}

impl Reg {
    // calling lower on the lowest registers returns the argument
    // should the function panic?
    fn lower(&self) -> Self {
        match self {
            RAX => AL,
            AL => AL,
            RBX => todo!(),
            RCX => todo!(),
            RDX => todo!(),
            ESP => todo!(),
        }
    }
}

enum Value {
    Literal(u32),
    Reg(Reg),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Value::Literal(literal) => format!("${literal}"),
            Value::Reg(reg) => format!("{reg}"),
        };
        write!(f, "{str}")
    }
}

#[allow(non_camel_case_types)]
enum Instruction {
    mov(Value, Reg),
    cmp(Value, Reg),
    add(Reg, Reg),
    imul(Reg, Reg),
    sub(Reg, Reg),
    xchg(Reg, Reg),
    neg(Reg),
    not(Reg),
    sete(Reg),
    setne(Reg),
    setl(Reg),
    setle(Reg),
    setg(Reg),
    setge(Reg),
    push(Reg),
    pop(Reg),
    idiv(Reg),
    clear(Reg),
    jmp(Label),
    je(Label),
    cqo,
    ret,
}

// All instructions have been padded with spaces after the instruction
// So that they are aligned with larger instructions (up to 5 characters)
impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Instruction::mov(operand, reg) => format!("mov   {operand}, {reg}"),
            Instruction::cmp(operand, reg) => format!("cmp   {operand}, {reg}"),
            Instruction::add(operand, reg) => format!("add   {operand}, {reg}"),
            Instruction::imul(operand, reg) => format!("imul  {operand}, {reg}"),
            Instruction::sub(operand, reg) => format!("sub   {operand}, {reg}"),
            Instruction::xchg(operand, reg) => format!("xchg  {operand}, {reg}"),
            Instruction::neg(reg) => format!("neg   {reg}"),
            Instruction::not(reg) => format!("not   {reg}"),
            Instruction::sete(reg) => format!("sete  {reg}"),
            Instruction::setne(reg) => format!("setne {reg}"),
            Instruction::setl(reg) => format!("setl {reg}"),
            Instruction::setle(reg) => format!("setle {reg}"),
            Instruction::setg(reg) => format!("setg {reg}"),
            Instruction::setge(reg) => format!("setge {reg}"),
            Instruction::push(reg) => format!("push  {reg}"),
            Instruction::pop(reg) => format!("pop   {reg}"),
            Instruction::idiv(divisor) => format!("idiv  {divisor}"),
            Instruction::clear(reg) => format!("clear {reg}"),
            Instruction::jmp(label) => format!("jmp {label}"),
            Instruction::je(label) => format!("je {label}"),
            Instruction::cqo => format!("cqo    "),
            Instruction::ret => format!("ret    "),
        };
        write!(f, "{str}")
    }
}
