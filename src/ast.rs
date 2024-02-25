use std::fmt::{Debug, Display, Error, Formatter};

pub struct CondBlock(pub Box<Expr>, pub Box<Statement>);

// TODO add Error?
// TODO turn (Box<Expr>, Box<Statement>) into a conditioned-block type or something
pub enum Statement {
    CodeBlock(Vec<Box<Statement>>),
    Assign(String, Box<Expr>),
    Decl(String, Box<Type>),
    Init(String, Box<Type>, Box<Expr>),
    If(CondBlock, Vec<CondBlock>, Option<Box<Statement>>),
    While(Box<Expr>, Box<Statement>),
    For(Box<Statement>, Box<Expr>, Box<Statement>, Box<Statement>),
    Return(Box<Expr>),
}

// Can add e.g. lambdas here?
// Is it good practice to take a vec of AST statements when only one of the enum variants is valid?
pub enum Func {
    Func(String, Args, Option<Box<Type>>, Box<Statement>),
}

pub struct Args(pub Vec<Box<Statement>>);

// TODO actually parse bool literals
pub enum Expr {
    Int(i32),
    Float(f32),
    Bool(bool),
    Literal(String),
    Id(String),
    Array(Vec<Box<Expr>>),
    Index(Box<Expr>, Box<Expr>),
    BinOp(Box<Expr>, BinOpcode, Box<Expr>),
    PfxUnOp(PfxOpcode, Box<Expr>),
    SfxUnOp(Box<Expr>, SfxOpcode),
    Error,
}

pub enum Type {
    Int,
    Float,
    Custom(String),
}

#[derive(Copy, Clone)]
pub enum BinOpcode {
    Mul,
    Div,
    Add,
    Sub,
    GreaterThan,
    LessThan,
    Equals,
}

#[derive(Copy, Clone)]
pub enum PfxOpcode {
    Not,
    Minus,
}

#[derive(Copy, Clone)]
pub enum SfxOpcode {
    Fact,
}

// Control Flow

// Debug impls

impl Debug for Func {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Func::*;
        match self {
            Func(ref name, args, ret_type, body) => {
                let r = match ret_type {
                    Some(tipe) => format!("{:?}", tipe),
                    None => "void".into(),
                };
                write!(fmt, "define f:{name}({:?}) gives {r} as {:?}", args, body)
            }
        }
    }
}

impl Debug for Args {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        let Args(bob) = self;
        let args: Vec<String> = bob
            .iter()
            .map(|s| format!("{:?}", s).trim_end_matches(';').to_string())
            .collect();
        let args_s = args.join(", ");
        write!(fmt, "{args_s}")
    }
}

impl Display for CondBlock {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match self {
            CondBlock(cond, work) => write!(fmt, "if {:?} {:?}", cond, work),
        }
    }
}

impl Debug for Statement {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Statement::*;
        match self {
            CodeBlock(stmts) => {
                write!(fmt, "{{ {:?} }}", stmts)
            } // TODO fix this to get rid of square brackets
            Assign(id, e) => write!(fmt, "{id} = {:?};", e),
            Decl(id, tipe) => write!(fmt, "{id} of {:?};", tipe),
            Init(id, tipe, e) => write!(fmt, "{id} of {:?} = {:?};", tipe, e),
            If(i, eli, el) => {
                let elifs: Vec<String> = eli.iter().map(|cb| format!("else {cb}")).collect();
                let mut elifss = elifs.join(" ");
                if !elifs.is_empty() {
                    elifss = format!(" {elifss}");
                }
                write!(fmt, "{i}{elifss} else {:?}", el)
            }
            While(cond, work) => write!(fmt, "while ({:?}) {:?}", cond, work),
            For(init, cond, it, work) => {
                write!(fmt, "for ( {:?} ; {:?} ; {:?} ) {:?}", init, cond, it, work)
            }
            Return(val) => write!(fmt, "return {:?};", val),
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Expr::*;
        match *self {
            Int(i) => write!(fmt, "{:?}", i),
            Float(f) => write!(fmt, "{:?}", f),
            Bool(b) => write!(fmt, "{:?}", b),
            Literal(ref s) => write!(fmt, "\"{s}\""),
            Id(ref s) => write!(fmt, "{s}"),
            Array(ref elements) => write!(fmt, "{:?}", elements),
            Index(ref indexable, ref index) => write!(fmt, "{:?}[{:?}]", indexable, index),
            BinOp(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            PfxUnOp(op, ref e) => write!(fmt, "{:?}({:?})", op, e),
            SfxUnOp(ref e, op) => write!(fmt, "({:?}){:?}", e, op),
            Error => write!(fmt, "error"),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::Type::*;
        match *self {
            Int => write!(fmt, "t:Int"),
            Float => write!(fmt, "t:Float"),
            Custom(ref s) => write!(fmt, "t:c({s})"),
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
            GreaterThan => write!(fmt, ">"),
            LessThan => write!(fmt, "<"),
            Equals => write!(fmt, "=="),
        }
    }
}

impl Debug for PfxOpcode {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::PfxOpcode::*;
        match *self {
            Not => write!(fmt, "!"),
            Minus => write!(fmt, "-"),
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
