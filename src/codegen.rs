use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::IntType;
use inkwell::values::IntValue;
use inkwell::OptimizationLevel;

use std::error::Error;
use std::fmt;

use crate::ast::{BinOpcode, Expr};

#[derive(Debug)]
pub enum CodegenError {
    Temp(String),
    Builder(BuilderError),
}

impl From<BuilderError> for CodegenError {
    fn from(value: BuilderError) -> Self {
        Self::Builder(value)
    }
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Temp(s) => write!(f, "Temp {s}"),
            Self::Builder(b) => write!(f, "A builder error {:?}", b),
        }
    }
}

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn(u64, u64, u64) -> u64;

struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    // types
    i32_type: IntType<'ctx>,
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

    // TODO This only works for integer things
    // This is a small builder that will only build for arithmetic
    fn arithmetic_build(&self, arith: Box<Expr>) -> Result<IntValue, CodegenError> {
        match *arith {
            Expr::Int(i) => Ok(self.i32_type.const_int(i as u64, false)),
            Expr::BinOp(l, op, r) => {
                let left = self.arithmetic_build(l)?;
                let right = self.arithmetic_build(r)?;
                match op {
                    BinOpcode::Add => self
                        .builder
                        .build_int_add(left, right, "binop-add-temp")
                        .map_err(CodegenError::from),
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
            _ => Err(CodegenError::Temp(
                "Unhandled case of expression".to_string(),
            )),
        }
    }
}

pub fn arith_build(arith_expr: Box<Expr>) -> Result<(), CodegenError> {
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

    codegen.arithmetic_build(arith_expr).map(|_| ())
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
