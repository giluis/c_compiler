use std::{
    collections::HashMap,
    fmt::Display,
    ops::{Add, AddAssign},
    sync::{Mutex, MutexGuard, OnceLock},
};

use crate::{lexer::Token, parser::grammar::*};

mod immutable_map;
use nonempty::NonEmpty;
use Instruction::*;
use Reg::*;

#[derive(Debug)]
pub struct Asm(pub String);

impl Asm {
    fn new() -> Asm {
        Asm(String::new())
    }

    fn with<P>(p: P) -> Asm
    where
        P: Add<Asm, Output = Asm>,
    {
        p + Asm(String::new())
    }

    fn append_new_line(mut self) -> Self {
        self.0 += "\n";
        self
    }
}

impl AddAssign for Asm {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += &rhs.0;
    }
}

impl Add for Asm {
    type Output = Asm;

    fn add(mut self, rhs: Self) -> Self::Output {
        self.0 = self.0 + &rhs.0;
        self
    }
}

impl Add<Instruction> for Asm {
    type Output = Asm;

    fn add(mut self, instruction: Instruction) -> Self::Output {
        self.0 = self.0 + &instruction.to_string();
        self
    }
}

impl Add<Label> for Asm {
    type Output = Asm;

    fn add(mut self, label: Label) -> Self::Output {
        self.0 = self.0 + &label.to_string();
        self
    }
}

impl Add<Asm> for Label {
    type Output = Asm;

    fn add(self, asm: Asm) -> Self::Output {
        let asm = asm.0;
        Asm(format!("{self}{asm}"))
    }
}

impl Add for Instruction {
    type Output = Asm;
    fn add(self, instruction: Instruction) -> Self::Output {
        Asm(format!("{self}{instruction}"))
    }
}

impl Add<Asm> for Instruction {
    type Output = Asm;
    fn add(self, asm: Asm) -> Self::Output {
        let asm = asm.0;
        Asm(format!("{self}{asm}"))
    }
}

trait MonadAppend {
    fn append(self, other: Self) -> Self;
}

impl MonadAppend for Result<Asm, CheckError> {
    fn append(self, other: Self) -> Self {
        let program = self?;
        let other_asm = other?;
        Ok(program + other_asm)
    }
}

pub enum CheckError {
    UsageOfUndefinedVariable,
    UnparsableTokens,
    EmptyBody,
}

struct LocalScope {
    // var_name -> ESP offset
    vars: HashMap<Token, Var>,
    current_local_var_offset: isize,
}

impl LocalScope {
    fn declare_local(&mut self, ident: Token) -> Var {
        let var = Var {
            esp_offset: self.current_local_var_offset,
        };
        self.vars.insert(ident, var);
        // TODO: Different types must have different increments
        self.current_local_var_offset += 4;
        var
    }
}

struct Var {
    esp_offset: isize,
}

struct GlobalScope;

pub struct Vars {
    global: (),
    scope: Vec<LocalScope>,
}

impl Vars {
    fn get(&mut self, ident: Token) {}

    fn global(&mut self, ident: Token) {
        todo!("Global variables not yet implemented");
    }

    fn local(&self, ident: Token) -> Location {
        let var = self.scope.last().expect("Last local scope should never be valid. This means a local variable is being declare outside of a function, which doesn't make sense").declare_local(ident);
        Location::LocalVar(var)
    }
}

impl Vars {
    fn new() -> Self {
        Self {
            scope: vec![],
            global: (),
        }
    }
}

pub trait GenAsm {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult;
}

type AsmResult = Result<Asm, CheckError>;

impl AST {
    pub fn compile(self) -> AsmResult {
        let mut vars = Vars::new();
        self.to_asm(&mut vars)
    }
}

impl GenAsm for AST {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        if self.items.is_empty() {
            return Err(CheckError::UnparsableTokens);
        }

        // globals
        let mut program = Asm(self
            .items
            .iter()
            .fold("".to_owned(), |accum, item| match item {
                Item::Function(function) => accum + ".globl " + &format!("{}\n", function.ident),
            }));

        for item in self.items.iter().map(|i| i.to_asm(vars)) {
            match item {
                Ok(item_asm) => program += item_asm,
                Err(err) => return Err(err),
            }
        }
        Ok(program.append_new_line())
    }
}

impl GenAsm for Item {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        match self {
            Item::Function(function) => function.to_asm(vars),
        }
    }
}

impl GenAsm for Function {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        let body = self.body.content.to_asm(vars)?;
        Ok(Asm(format!("{}:", self.ident)) + body)
    }
}

impl GenAsm for Body {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        self.statements
            .iter()
            .map(|s| s.to_asm(vars))
            .reduce(|first, second| first.append(second))
            .ok_or(CheckError::EmptyBody)?
    }
}

impl GenAsm for Statement {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        match self {
            Statement::Return(return_statement) => return_statement.to_asm(vars),
            Statement::Declaration(declaration_statement) => declaration_statement.to_asm(vars),
        }
    }
}

impl GenAsm for DeclarationStatement {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        // TODO: Add global variables
        let expr = match self.initialization {
            Some((_equals_sign, expr)) => expr.to_asm(vars)?,
            None => Asm::new(),
        };

        Ok(expr
            + mov(
                Operand::Reg(RAX),
                Location::LocalVar(vars.local(self.ident)),
            ))
    }
}

impl GenAsm for ReturnStatement {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        let expr = self.expr.to_asm(vars)?;
        Ok(expr + ret)
    }
}

impl GenAsm for Expression {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        match self {
            Expression::AssignExpression(assign_expr) => assign_expr.to_asm(vars),
            Expression::OrExpression(_) => todo!(),
        }
    }
}

impl GenAsm for AssignExpression {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        todo!()
    }
}

impl GenAsm for OrExpression {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        let mut result = self.left.to_asm(vars)?;
        if self.others.is_empty() {
            return Ok(result);
        }
        let end_label = Label::create_end();
        for (_or_operator, parcel) in &self.others {
            let continue_label = Label::create_continue();
            result += cmp(Operand::Literal(0), RAX)
                + je(continue_label)
                + mov(Operand::Literal(1), Location::Reg(RAX))
                + jmp(end_label)
                + continue_label
                + parcel.to_asm(vars)?
                + cmp(Operand::Zero, RAX)
                + mov(Operand::Zero, Location::Reg(RAX))
                + setne(RAX.lower());
        }
        Ok(result + end_label)
    }
}

// TODO: Add short circuiting
impl GenAsm for AndExpression {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        // result in %eax
        let mut result = self.left.to_asm(vars)?;
        if self.others.is_empty() {
            return Ok(result);
        }
        let end_label = Label::create_end();
        for (_and_operator, parcel) in self.others.iter() {
            result += cmp(Operand::Zero, RAX) + je(end_label) + parcel.to_asm(vars)?;
        }

        Ok(result + end_label + cmp(Operand::Zero, RAX) + clear(RAX) + setne(RAX.lower()))
    }
}

impl GenAsm for EqualityExpression {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        let mut result = self.left.to_asm(vars)?;
        for (equality_operator, parcel) in self.others.iter() {
            result += push(RAX)
                + parcel.to_asm(vars)?
                + pop(RCX)
                + cmp(Operand::Reg(RAX), RCX)
                + match equality_operator {
                    Operator::LogEqual => sete(RAX.lower()),
                    Operator::NotEqual => setne(RAX.lower()),
                    _ => unreachable!(
                        "Equality expressions will only have equals and not equals operators"
                    ),
                };
            // code for equality
        }
        Ok(result)
    }
}

impl GenAsm for RelationalExpression {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        // result in %eax
        let mut result = self.left.to_asm(vars)?;
        for (relational_operator, parcel) in self.others.iter() {
            result += push(RAX)
                + parcel.to_asm(vars)?
                + pop(RCX)
                + cmp(Operand::Reg(RAX), RCX)
                + clear(RAX)
                + match relational_operator {
                    Operator::GreaterThan => setg(RAX.lower()),
                    Operator::GreaterThanOrEqual => setge(RAX.lower()),
                    Operator::LessThan => setl(RAX.lower()),
                    Operator::LessThanOrEqual => setle(RAX.lower()),
                    _ => unreachable!(
                        "Relational expressions will only have the operators expressed above"
                    ),
                };
        }
        Ok(result)
    }
}

impl GenAsm for ArithExpr {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        let mut result = self.left.to_asm(vars)?;
        for (operator, term) in self.others.iter() {
            result = result
                + push(RAX)
                + term.to_asm(vars)?
                + pop(RBX)
                + match operator {
                    Operator::Add => Asm::with(add(RBX, RAX)),
                    Operator::Sub => xchg(RAX, RBX) + sub(RBX, RAX),
                    _ => unreachable!("ArithExpr will only have Operator::Add and Operator::Sub"),
                };
        }
        Ok(result)
    }
}

impl GenAsm for Term {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        let mut result = self.left.to_asm(vars)?;
        for (operator, term) in self.others.iter() {
            result += push(RAX)
                + term.to_asm(vars)?
                + pop(RBX)
                + match operator {
                    Operator::Mult => Asm::new() + imul(RBX, RAX),
                    Operator::Div => xchg(RAX, RBX) + cqo + idiv(RBX),
                    _ => unreachable!("Term exprs will only have Operator::Mult and Operator::Div"),
                };
        }
        Ok(result)
    }
}

impl GenAsm for Factor {
    fn to_asm(&self, vars: &mut Vars) -> AsmResult {
        let factor = match &self {
            Factor::Expression(other) => other.expr.to_asm(vars)?,
            Factor::Unary((unop, other_factor)) => {
                other_factor.to_asm(vars)? + unop.to_asm(vars)?
            }
            Factor::Number(literal) => match literal {
                Token::Number(literal) => Asm::new() + mov(Operand::Literal(*literal), RAX),
                _ => unreachable!("Factor::Number should only contain Token::Number"),
            },
            Factor::Identifier(_) => todo!("Identifiers not implemented yet"),
        };
        Ok(factor)
    }
}

impl GenAsm for UnaryOperator {
    fn to_asm(&self, _vars: &mut Vars) -> AsmResult {
        let unop = match &self {
            UnaryOperator::ArithNegation => Asm::new() + neg(RAX),
            UnaryOperator::BitwiseComplement => Asm::new() + not(RAX),
            UnaryOperator::LogicalNegation => {
                cmp(Operand::Literal(0), RAX) + mov(Operand::Literal(0), RAX) + sete(RAX.lower())
            }
        };
        Ok(unop)
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
        let counter = Self::increment_counter();
        Self::END(*counter)
    }

    fn create_continue() -> Self {
        let counter = Self::increment_counter();
        Self::CONTINUE(*counter)
    }

    fn increment_counter<'a>() -> MutexGuard<'a, usize> {
        let mut current = match CURRENT_LABEL.get_or_init(|| Mutex::new(0)).lock() {
            Ok(val) => val,
            Err(lock) => lock.into_inner(),
        };
        *current += 1;
        current
    }

    fn ident(&self) -> String {
        let (str, id) = match self {
            Label::CONTINUE(id) => ("continue", id),
            Label::END(id) => ("end", id),
        };
        format!("{str}_{id}")
    }
}

#[allow(clippy::write_with_newline)]
impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = self.ident();
        write!(f, "\n\n  {str}:")
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

enum Operand {
    Literal(u32),
    Reg(Reg),
    Zero,
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Operand::Literal(literal) => format!("${literal}"),
            Operand::Reg(reg) => format!("{reg}"),
            Operand::Zero => "$0".to_owned(),
        };
        write!(f, "{str}")
    }
}

enum Location {
    Reg(Reg),
    LocalVar(LocalVar),
}

#[allow(non_camel_case_types)]
enum Instruction {
    mov(Operand, Location),
    cmp(Operand, Reg),
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
            Instruction::sub(operand, reg) => format!("sub   {operand}, {reg}"),
            Instruction::imul(operand, reg) => format!("imul  {operand}, {reg}"),
            Instruction::xchg(operand, reg) => format!("xchg  {operand}, {reg}"),
            Instruction::pop(reg) => format!("pop   {reg}"),
            Instruction::je(label) => format!("je    {}", label.ident()),
            Instruction::jmp(label) => format!("jmp   {}", label.ident()),
            Instruction::neg(reg) => format!("neg   {reg}"),
            Instruction::not(reg) => format!("not   {reg}"),
            Instruction::push(reg) => format!("push  {reg}"),
            Instruction::sete(reg) => format!("sete  {reg}"),
            Instruction::setl(reg) => format!("setl  {reg}"),
            Instruction::setg(reg) => format!("setg  {reg}"),
            Instruction::idiv(divisor) => format!("idiv  {divisor}"),
            Instruction::setne(reg) => format!("setne {reg}"),
            Instruction::setle(reg) => format!("setle {reg}"),
            Instruction::setge(reg) => format!("setge {reg}"),
            Instruction::clear(reg) => format!("mov {}, {reg}", Operand::Zero),
            Instruction::cqo => "cqo    ".to_string(),
            Instruction::ret => "ret    ".to_string(),
        };
        write!(f, "\n    {str}")
    }
}
