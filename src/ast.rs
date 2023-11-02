use std::fmt::{Debug, Error, Formatter};

// TODO add Error?
// TODO make ASTStatements
pub enum ASTStatement {
    CodeBlock(Vec<Box<ASTStatement>>),
    Assign(String, Box<Expr>),
    If(Box<Expr>, Box<ASTStatement>),
}

pub enum Expr {
    Number(i32),
    Literal(String),
    BinOp(Box<Expr>, BinOpcode, Box<Expr>),
    PfxUnOp(PfxOpcode, Box<Expr>),
    SfxUnOp(Box<Expr>, SfxOpcode),
    Error,
}

#[derive(Copy, Clone)]
pub enum BinOpcode {
    Mul,
    Div,
    Add,
    Sub,
}

#[derive(Copy, Clone)]
pub enum PfxOpcode {
    Not,
}

#[derive(Copy, Clone)]
pub enum SfxOpcode {
    Fact,
}

// Control Flow

// Debug impls

impl Debug for ASTStatement {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::ASTStatement::*;
        match self {
            CodeBlock(stmts) => write!(fmt, "{{ {:?} }}", stmts),
            Assign(id, e) => write!(fmt, "{id} = {:?};", e),
            If(cond, work) => write!(fmt, "if {:?} {:?}", cond, work),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Expr::*;
        match *self {
            Number(n) => write!(fmt, "{:?}", n),
            Literal(ref s) => write!(fmt, "\"{s}\""),
            BinOp(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            PfxUnOp(op, ref e) => write!(fmt, "{:?}({:?})", op, e),
            SfxUnOp(ref e, op) => write!(fmt, "({:?}){:?}", e, op),
            Error => write!(fmt, "error"),
        }
    }
}

impl Debug for BinOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::BinOpcode::*;
        match *self {
            Mul => write!(fmt, "*"),
            Div => write!(fmt, "/"),
            Add => write!(fmt, "+"),
            Sub => write!(fmt, "-"),
        }
    }
}

impl Debug for PfxOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::PfxOpcode::*;
        match *self {
            Not => write!(fmt, "!"),
        }
    }
}

impl Debug for SfxOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::SfxOpcode::*;
        match *self {
            Fact => write!(fmt, "!"),
        }
    }
}
