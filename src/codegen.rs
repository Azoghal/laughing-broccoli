use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::execution_engine::{ExecutionEngine, JitFunction};
use inkwell::module::Module;
use inkwell::support::LLVMString;
use inkwell::types::IntType;
use inkwell::values::{BasicValue, BasicValueEnum, FunctionValue, IntValue};
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
    top: usize,
}

impl<'ctx> BassoonScope<'ctx> {
    fn new() -> BassoonScope<'ctx> {
        let mut scope = BassoonScope {
            scopes: Vec::new(),
            scope_sources: Vec::new(),
            top: 0,
        };
        scope.start_scope("GLOBAL".to_string());
        scope
    }

    fn start_scope(&mut self, source: String) {
        self.scope_sources.push(source);
        self.scopes.push(HashMap::new());
        self.top += 1;
    }

    fn end_scope(&mut self, expected_source: String) -> Result<(), CodegenError> {
        let (Some(actual_source), Some(_)) = (self.scope_sources.pop(), self.scopes.pop()) else {
            return Err(CodegenError::Scope("no scopes left to pop".to_string()));
        };

        if actual_source != expected_source {
            return Err(CodegenError::Scope(format!(
                "current top scope source {actual_source} does not match expected scope {expected_source}"
            )));
        }
        self.top -= 1;
        Ok(())
    }

    fn add_to_scope(
        &mut self,
        identifier: String,
        val: &'ctx (dyn BasicValue<'ctx>),
    ) -> Result<(), CodegenError> {
        let current_scope = &mut self.scopes[self.top - 1];
        if current_scope.contains_key(&identifier) {
            return Err(CodegenError::Scope(format!("this would conflict with an existing reference to {identifier} in the current scope")));
        }
        let None = current_scope.insert(identifier, val) else {
            return Err(CodegenError::Scope(
                "somewhow managed to override an existing reference in this scope, aborting"
                    .to_string(),
            ));
        };
        Ok(())
    }

    fn reference_lookup(&self, identifier: String) -> Option<&'ctx dyn BasicValue<'ctx>> {
        for scope in self.scopes.iter().rev() {
            if let Some(&val) = scope.get(&identifier) {
                return Some(val);
            }
        }
        None
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
            Expr::Id(identifier) => {
                if let Some(val) = self.scope.reference_lookup("TODO temp".to_string()) {
                    match val.as_basic_value_enum() {
                        //TODO remove unwrap
                        BasicValueEnum::PointerValue(v) => Ok(self
                            .builder
                            .build_load(v, "load-int")
                            .unwrap()
                            .into_int_value()),
                        _ => {
                            return Err(CodegenError::NotImplemented(
                                "Not implemented loads for non-ints yet".to_string(),
                            ))
                        }
                    }
                } else {
                    return Err(CodegenError::Temp("Variable lookup failed".to_string()));
                }
            }
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
    let scope = BassoonScope::new();

    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
        i32_type: context.i32_type(),
        scope,
    };

    codegen.main_build(arith_expr);
    Ok(())
}

pub fn jit_sum() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("sum");
    let execution_engine = module.create_jit_execution_engine(OptimizationLevel::None)?;
    let scope = BassoonScope::new();

    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
        i32_type: context.i32_type(),
        scope,
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

#[test]
fn test_scope_create() {
    let mut scope = BassoonScope::new();
    scope.end_scope("GLOBAL".to_string()).unwrap()
}

#[test]
fn test_scope_push_and_pop() {
    let mut scope = BassoonScope::new();
    scope.start_scope("test1".to_string());
    scope.start_scope("test2".to_string());
    scope.end_scope("test2".to_string()).unwrap();
    scope.end_scope("test1".to_string()).unwrap();
}

#[test]
fn test_scope_add() {
    let context = Context::create();
    let builder = context.create_builder();
    let mut scope = BassoonScope::new();

    let value = context.i32_type().const_int(23, false);
    scope.add_to_scope("bobbis".to_string(), &value).unwrap();
}

#[test]
fn test_scope_add_get() {
    let context = Context::create();
    let mut scope = BassoonScope::new();

    let value = context.i32_type().const_int(23, false);
    scope.add_to_scope("bobbis".to_string(), &value).unwrap();

    let Some(_) = scope.reference_lookup("bobbis".to_string()) else {
        panic!()
    };
}

#[test]
fn test_scope_add_get_multi_scope() {
    // Populate 3 scope levels with a binding for the same name, check that lookup returns the correct one
    let context = Context::create();
    let mut scope = BassoonScope::new();

    let value_1 = context.i32_type().const_int(23, false);
    scope.add_to_scope("bobbis".to_string(), &value_1).unwrap();

    let Some(val_1) = scope.reference_lookup("bobbis".to_string()) else {
        panic!()
    };
    let val_1_enum = val_1.as_basic_value_enum();
    match val_1_enum {
        BasicValueEnum::IntValue(v) => {
            assert_eq!(v.get_zero_extended_constant().unwrap(), 23)
        }
        _ => panic!(),
    }

    scope.start_scope("test1".to_string());
    let value_2 = context.i32_type().const_int(800, false);
    scope.add_to_scope("bobbis".to_string(), &value_2).unwrap();

    let Some(val_2) = scope.reference_lookup("bobbis".to_string()) else {
        panic!()
    };
    let val_2_enum = val_2.as_basic_value_enum();
    match val_2_enum {
        BasicValueEnum::IntValue(v) => {
            assert_eq!(v.get_zero_extended_constant().unwrap(), 800)
        }
        _ => panic!(),
    }

    scope.start_scope("test2".to_string());
    let value_3 = context.i32_type().const_int(1111, false);
    scope.add_to_scope("bobbis".to_string(), &value_3).unwrap();

    let Some(val_3) = scope.reference_lookup("bobbis".to_string()) else {
        panic!()
    };
    let val_3_enum = val_3.as_basic_value_enum();
    match val_3_enum {
        BasicValueEnum::IntValue(v) => {
            assert_eq!(v.get_zero_extended_constant().unwrap(), 1111)
        }
        _ => panic!(),
    }

    scope.end_scope("test2".to_string()).unwrap();

    let Some(val_2) = scope.reference_lookup("bobbis".to_string()) else {
        panic!()
    };
    let val_2_enum = val_2.as_basic_value_enum();
    match val_2_enum {
        BasicValueEnum::IntValue(v) => {
            assert_eq!(v.get_zero_extended_constant().unwrap(), 800)
        }
        _ => panic!(),
    }

    scope.end_scope("test1".to_string()).unwrap();

    let Some(val_1) = scope.reference_lookup("bobbis".to_string()) else {
        panic!()
    };
    let val_1_enum = val_1.as_basic_value_enum();
    match val_1_enum {
        BasicValueEnum::IntValue(v) => {
            assert_eq!(v.get_zero_extended_constant().unwrap(), 23)
        }
        _ => panic!(),
    }
}
