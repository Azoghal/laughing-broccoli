use std::fmt::{Debug, Display, Error, Formatter};

pub struct CondBlock(pub Box<Expr>, pub Box<ASTStatement>);

// TODO add Error?
// TODO turn (Box<Expr>, Box<ASTStatement>) into a conditioned-block type or something
pub enum ASTStatement {
    CodeBlock(Vec<Box<ASTStatement>>),
    Assign(String, Box<Expr>),
    Decl(String, Box<ASTType>),
    Init(String, Box<ASTType>, Box<Expr>),
    If(CondBlock, Vec<CondBlock>, Option<Box<ASTStatement>>),
    While(Box<Expr>, Box<ASTStatement>),
    For(
        Box<ASTStatement>,
        Box<Expr>,
        Box<ASTStatement>,
        Box<ASTStatement>,
    ),
    Return(Box<Expr>),
}

// Can add e.g. lambdas here?
// Is it good practice to take a vec of AST statements when only one of the enum variants is valid?
pub enum ASTFunc {
    Func(String, ASTArgs, Option<Box<ASTType>>, Box<ASTStatement>),
}

pub struct ASTArgs(pub Vec<Box<ASTStatement>>);

pub enum Expr {
    Number(i32),
    Literal(String),
    Id(String),
    BinOp(Box<Expr>, BinOpcode, Box<Expr>),
    PfxUnOp(PfxOpcode, Box<Expr>),
    SfxUnOp(Box<Expr>, SfxOpcode),
    Error,
}

pub enum ASTType {
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

impl Debug for ASTFunc {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::ASTFunc::*;
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

impl Debug for ASTArgs {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        let ASTArgs(bob) = self;
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

impl Debug for ASTStatement {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::ASTStatement::*;
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
            Number(n) => write!(fmt, "{:?}", n),
            Literal(ref s) => write!(fmt, "\"{s}\""),
            Id(ref s) => write!(fmt, "{s}"),
            BinOp(ref l, op, ref r) => write!(fmt, "({:?} {:?} {:?})", l, op, r),
            PfxUnOp(op, ref e) => write!(fmt, "{:?}({:?})", op, e),
            SfxUnOp(ref e, op) => write!(fmt, "({:?}){:?}", e, op),
            Error => write!(fmt, "error"),
        }
    }
}

impl Debug for ASTType {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        use self::ASTType::*;
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
