use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::IntType;
use inkwell::values::{BasicValue, FunctionValue, IntValue};
use inkwell::OptimizationLevel;
use tracing::info;

use std::collections::HashMap;
use std::error::Error;
use std::fmt;

use crate::ast::{ASTStatement, BinOpcode, Expr};

#[derive(Debug)]
pub enum CodegenError {
    Temp(String),
    Builder(BuilderError),
    NotImplemented(String),
    Scope(String),
}

impl From<BuilderError> for CodegenError {
    fn from(value: BuilderError) -> Self {
        Self::Builder(value)
    }
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Temp(msg) => write!(f, "Temp {msg}"),
            Self::Builder(b_err) => write!(f, "A builder error {:?}", b_err),
            Self::NotImplemented(msg) => write!(f, "codegen not implemented for {msg}"),
            Self::Scope(msg) => write!(f, "scoping issue: {msg}"),
        }
    }
}

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct BassoonScope<'ctx> {
    scopes: Vec<HashMap<String, &'ctx dyn BasicValue<'ctx>>>,
    scope_sources: Vec<String>, // TODO extend to be more helpful, include e.g. line numbers or something
}

impl<'ctx> BassoonScope<'ctx> {
    fn start_scope(&self, source: String) {
        self.scope_sources.push(source);
        self.scopes.push(HashMap::new())
    }

    fn end_scope(&self, expected_source: String) -> Result<(), CodegenError> {
        let Some(actual_source) = self.scope_sources.pop() else {
            return Err(CodegenError::Scope(format!("no scopes left to pop")));
        };

        if actual_source != expected_source {
            return Err(CodegenError::Scope(format!(
                "current top scope source {actual_source} does not match expected scope {expected_source}"
            )));
        }
        Ok(())
    }
}

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    // types
    i32_type: IntType<'ctx>,

    // data structures
    scope: BassoonScope<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compile_sum(&self) -> Option<JitFunction<SumFunc>> {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[i64_type.into(), i64_type.into(), i64_type.into()], false);
        let function = self.module.add_function("sum", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        let x = function.get_nth_param(0)?.into_int_value();
        let y = function.get_nth_param(1)?.into_int_value();
        let z = function.get_nth_param(2)?.into_int_value();

        let sum = self.builder.build_int_add(x, y, "sum").unwrap();
        let sum = self.builder.build_int_add(sum, z, "sum").unwrap();

        self.builder.build_return(Some(&sum)).unwrap();

        unsafe { self.execution_engine.get_function("sum").ok() }
    }

    fn print_llvm_function(&self, function: FunctionValue) {
        // TODO improvements:
        //  1. log info about the function
        //  2. Make this work out what tracing log level we are running in and then respect that, not printing if not infoing.
        info!("The function you requested");
        function.print_to_stderr();
    }

    // Starts a main function that returns an i32
    fn main_build(&self, arith: Box<Expr>) {
        let fn_type = self.i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        // build an arithmetic expression
        if let Ok(val) = self.int_expr_build(arith) {
            self.builder.build_return(Some(&val)).unwrap();
        } else {
            self.builder.build_return(None).unwrap();
        }

        self.print_llvm_function(function)
    }

    // TODO sort out return type for success - what even is it?
    fn statement_build(&self, statement: Box<ASTStatement>) -> Result<(), CodegenError> {
        match *statement {
            ASTStatement::Assign(identifier, expr) => {
                // TODO lookup in existing scope of identifiers, else error
                Ok(())
            }
            ASTStatement::Decl(identifier, typ) => Ok(()),
            ASTStatement::Init(identifier, typ, expr) => Ok(()),
            _ => Err(CodegenError::NotImplemented(
                "Non assign, decl, init statement".to_string(),
            )),
        }
    }

    // TODO This only works for integer things
    // This is a small builder that will only build for arithmetic
    fn int_expr_build(&self, arith: Box<Expr>) -> Result<IntValue, CodegenError> {
        match *arith {
            Expr::Int(i) => Ok(self.i32_type.const_int(i as u64, false)),
            Expr::Id(identifier) => Ok(self.i32_type.const_int(67 as u64, false)), // TODO lookup identifier
            Expr::BinOp(l, op, r) => {
                let left = self.int_expr_build(l)?;
                let right = self.int_expr_build(r)?;
                match op {
                    BinOpcode::Add => {
                        info!("making a binop add");
                        self.builder
                            .build_int_add(left, right, "binop-add-temp")
                            .map_err(CodegenError::from)
                    }
                    BinOpcode::Sub => self
                        .builder
                        .build_int_sub(left, right, "binop-sub-temp")
                        .map_err(CodegenError::from),
                    BinOpcode::Mul => self
                        .builder
                        .build_int_mul(left, right, "binop-mul-temp")
                        .map_err(CodegenError::from),
                    BinOpcode::Div => self
                        .builder
                        .build_int_signed_div(left, right, "binop-div-temp")
                        .map_err(CodegenError::from),
                    _ => Err(CodegenError::Temp(
                        "Not an int operation so can't do it".to_string(),
                    )),
                }
            }
            _ => Err(CodegenError::NotImplemented(
                "Unhandled case of expression".to_string(),
            )),
        }
    }
}

pub fn main_build(arith_expr: Box<Expr>) -> Result<(), CodegenError> {
    let context: Context = Context::create();
    let module = context.create_module("sum");
    let Ok(execution_engine) = module.create_jit_execution_engine(OptimizationLevel::None) else {
        return Err(CodegenError::Temp(
            "failed to make execution engine".to_string(),
        ));
    };
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
        i32_type: context.i32_type(),
    };

    codegen.main_build(arith_expr);
    Ok(())
}

pub fn jit_sum() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
        i32_type: context.i32_type(),
    };

    let sum = codegen
        .jit_compile_sum()
        .ok_or("Unable to JIT compile `sum`")?;

    let x = 1u64;
    let y = 2u64;
    let z = 3u64;

    unsafe {
        println!("{} + {} + {} = {}", x, y, z, sum.call(x, y, z));
        assert_eq!(sum.call(x, y, z), x + y + z);
    }

    Ok(())
}
