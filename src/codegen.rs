use inkwell::basic_block::BasicBlock;
use inkwell::builder::{Builder, BuilderError};
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{FloatType, IntType};
use inkwell::values::{
    AnyValue, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue,
};
use tracing::{error, info};

use std::collections::HashMap;
use std::fmt;

use crate::ast::{ASTFunc, ASTStatement, ASTType, BinOpcode, Expr};

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

// For build time data structures
struct Program<'ctx> {
    // data structures
    scope: BassoonScope<'ctx>,
    current_function: String,
}

struct BassoonScope<'ctx> {
    scopes: Vec<HashMap<String, PointerValue<'ctx>>>,
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
        val: PointerValue<'ctx>,
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

    fn reference_lookup(&self, identifier: String) -> Option<PointerValue<'ctx>> {
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

    // types
    i32_type: IntType<'ctx>,
    bool_type: IntType<'ctx>,
    f32_type: FloatType<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn new(context: &'ctx Context) -> Self {
        CodeGen {
            context,
            module: context.create_module("main"),
            builder: context.create_builder(),
            // execution_engine,
            i32_type: context.i32_type(),
            bool_type: context.i8_type(),
            f32_type: context.f32_type(),
            // scope: BassoonScope::new(),
        }
    }

    fn print_llvm_function(&self, function: FunctionValue) -> String {
        // TODO improvements:
        //  1. log info about the function
        //  2. Make this work out what tracing log level we are running in and then respect that, not printing if not infoing.
        info!("The function you requested");
        function.print_to_stderr();
        function.print_to_string().to_string()
    }

    // fn fn_build(&self, f: Box< )

    // Starts a main function that returns an i32
    fn main_build(&self, arith: Box<Expr>) -> Result<(), CodegenError> {
        let fn_type = self.i32_type.fn_type(&[], false);
        let function = self.module.add_function("main", fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(basic_block);

        let mut program = Program {
            scope: BassoonScope::new(),
            current_function: function.get_name().to_str().unwrap().to_string(),
        };

        // build an arithmetic expression
        if let Ok(val) = self.int_expr_build(arith, &mut program) {
            self.builder.build_return(Some(&val)).unwrap();
        } else {
            self.builder.build_return(None).unwrap();
        }

        self.print_llvm_function(function);
        Ok(())
    }

    // TODO sort out return type for success - what even is it?
    fn statement_build(
        &self,
        statement: Box<ASTStatement>,
        program: &mut Program<'ctx>,
    ) -> Result<(), CodegenError> {
        match *statement {
            ASTStatement::Assign(identifier, expr) => {
                let Some(alloca) = program.scope.reference_lookup(identifier) else {
                    return Err(CodegenError::Scope(
                        "Trying to reference a variable that is not in scope".to_string(),
                    ));
                };
                // make value from expr
                let value = self.int_expr_build(expr, program)?;
                // make store
                self.builder
                    .build_store(alloca, value)
                    .map_err(CodegenError::from)?;
                Ok(())
            }
            ASTStatement::Decl(identifier, typ) => {
                // make alloca
                // TODO update alloca name?
                let alloca = self
                    .builder
                    .build_alloca(self.i32_type, &identifier)
                    .map_err(CodegenError::from)?;
                // try to add to current scope
                program.scope.add_to_scope(identifier, alloca)?;
                Ok(())
            }
            ASTStatement::Init(identifier, typ, expr) => {
                // make alloca
                let alloca = self
                    .builder
                    .build_alloca(self.i32_type, &identifier)
                    .map_err(CodegenError::from)?;
                // try to add to current scope
                program.scope.add_to_scope(identifier, alloca)?;
                // make value from expr
                let value = self.int_expr_build(expr, program)?;
                // make store
                self.builder
                    .build_store(alloca, value)
                    .map_err(CodegenError::from)?;
                Ok(())
            }
            ASTStatement::If(iff, elsif_conds, els) => {
                let cond_val = self.bool_expr_build(iff.0);

                let comp_res = self
                    .builder
                    .build_int_compare(
                        inkwell::IntPredicate::EQ,
                        cond_val.unwrap(),
                        self.bool_type.const_int(1, false),
                        "bool-comp",
                    )
                    .map_err(CodegenError::from)?;

                let Some(top_level_block) = self.builder.get_insert_block() else {
                    return Err(CodegenError::Temp(
                        "failed to remember current basic block".to_string(),
                    ));
                };

                // Now we need to append two new basic blocks to the end of the current function
                // First append a then block, and move to the start of it, then build the block
                let Some(current_function) = self.module.get_function(&program.current_function)
                else {
                    return Err(CodegenError::Temp(
                        "builder could not find current function".to_string(),
                    ));
                };
                let if_block = self.context.append_basic_block(current_function, "if");
                self.builder.position_at_end(if_block);
                self.statement_build(iff.1, program)?;

                // TODO don't forget to loop and do the same as above for any else ifs

                // Then append the else block, and move to the start of it, then build that block
                // by popping one off the vec, and building a new ASTStatement::If using it?

                let else_block = self.context.append_basic_block(current_function, "else");
                self.builder.position_at_end(else_block);
                // If we have an else body, build it, otherwise this is the empty basic block that failing all if condition jumps to
                if let Some(st) = els {
                    self.statement_build(st, program)?;
                }

                self.builder.position_at_end(top_level_block);
                self.builder
                    .build_conditional_branch(comp_res, if_block, else_block)
                    .map_err(CodegenError::from)?;

                Ok(())
            }
            _ => Err(CodegenError::NotImplemented(
                "Non assign, decl, init statement".to_string(),
            )),
        }
    }

    // TODO This only works for integer things
    // This is a small builder that will only build for arithmetic
    fn int_expr_build(
        &self,
        arith: Box<Expr>,
        program: &Program<'ctx>,
    ) -> Result<IntValue, CodegenError> {
        match *arith {
            Expr::Int(i) => Ok(self.i32_type.const_int(i as u64, false)),
            Expr::Id(identifier) => {
                // TODO include types in bindings in scope
                // TODO wrap this in a function that does this lookup and verifies against some type, returning a basic value
                // That we can then turn into the type we want with another helper function that will try to do that
                if let Some(val) = program.scope.reference_lookup(identifier) {
                    match val.as_basic_value_enum() {
                        //TODO remove unwrap
                        BasicValueEnum::PointerValue(v) => Ok(self
                            .builder
                            .build_load(
                                v,
                                format!("load-int-{}", v.get_name().to_str().unwrap()).as_str(),
                            )
                            .unwrap()
                            .into_int_value()),
                        _ => Err(CodegenError::NotImplemented(
                            "Not implemented loads for non-ints yet".to_string(),
                        )),
                    }
                } else {
                    Err(CodegenError::Temp("Variable lookup failed".to_string()))
                }
            }
            Expr::BinOp(l, op, r) => {
                let left = self.int_expr_build(l, program)?;
                let right = self.int_expr_build(r, program)?;
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

    fn bool_expr_build(&self, expr: Box<Expr>) -> Result<IntValue, CodegenError> {
        match *expr {
            Expr::Bool(true) => Ok(self.bool_type.const_int(1, false)),
            Expr::Bool(false) => Ok(self.bool_type.const_int(0, false)),
            Expr::Id(identifier) => Err(CodegenError::NotImplemented(
                "not implemented yet".to_string(),
            )),
            _ => Err(CodegenError::NotImplemented(
                "not implemented yet".to_string(),
            )),
        }
    }
}

pub fn emit(arith_expr: Box<Expr>) -> Result<(), CodegenError> {
    let context: Context = Context::create();
    let codegen = CodeGen::new(&context);
    codegen.main_build(arith_expr)
}

#[cfg(test)]
fn setup_scope_tests<'ctx>(context: &'ctx Context, module: &Module<'ctx>, builder: &Builder<'ctx>) {
    let fn_type = context.i32_type().fn_type(&[], false);
    let function = module.add_function("main", fn_type, None);
    let basic_block = context.append_basic_block(function, "entry");
    builder.position_at_end(basic_block);
}

#[cfg(test)]
fn setup_codegen_tests<'ctx>(codegen: &mut CodeGen) -> Program<'ctx> {
    let fn_type = codegen.i32_type.fn_type(&[], false);
    let function = codegen.module.add_function("main", fn_type, None);
    let basic_block = codegen.context.append_basic_block(function, "entry");
    codegen.builder.position_at_end(basic_block);

    let mut program: Program = Program {
        scope: BassoonScope::new(),
        current_function: "main".to_string(),
    };

    return program;
}

mod scope_tests {
    #[cfg(test)]
    use super::*;

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
    #[should_panic]
    fn test_scope_error_if_source_mismatch() {
        let mut scope = BassoonScope::new();
        scope.start_scope("test1".to_string());
        scope.end_scope("test2".to_string()).unwrap();
    }

    #[test]
    fn test_scope_add() {
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();
        setup_scope_tests(&context, &module, &builder);
        let mut scope = BassoonScope::new();

        let Ok(value) = builder.build_alloca(context.i32_type(), "test") else {
            panic!()
        };
        scope.add_to_scope("bobbis".to_string(), value).unwrap();
    }

    #[test]
    #[should_panic]
    fn test_scope_add_fails_for_shadow() {
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();
        setup_scope_tests(&context, &module, &builder);
        let mut scope = BassoonScope::new();

        let Ok(value) = builder.build_alloca(context.i32_type(), "test") else {
            panic!()
        };
        scope.add_to_scope("bobbis".to_string(), value).unwrap();
        scope.add_to_scope("bobbis".to_string(), value).unwrap();
    }

    #[test]
    fn test_scope_add_get() {
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();
        setup_scope_tests(&context, &module, &builder);
        let mut scope = BassoonScope::new();

        let Ok(value) = builder.build_alloca(context.i32_type(), "test") else {
            panic!()
        };
        scope.add_to_scope("bobbis".to_string(), value).unwrap();

        let Some(_) = scope.reference_lookup("bobbis".to_string()) else {
            panic!()
        };
    }

    #[test]
    fn test_scope_add_get_gives_none() {
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();
        setup_scope_tests(&context, &module, &builder);
        let scope = BassoonScope::new();

        let None = scope.reference_lookup("bobbis".to_string()) else {
            panic!()
        };
    }

    #[test]
    fn test_scope_add_get_multi_scope() {
        // Populate 3 scope levels with a binding for the same name, check that lookup returns the correct one
        let context = Context::create();
        let module = context.create_module("test");
        let builder = context.create_builder();
        setup_scope_tests(&context, &module, &builder);
        let mut scope = BassoonScope::new();

        let Ok(value_1) = builder.build_alloca(context.i32_type(), "test1") else {
            panic!()
        };
        scope.add_to_scope("bobbis".to_string(), value_1).unwrap();

        let Some(val_1) = scope.reference_lookup("bobbis".to_string()) else {
            panic!()
        };
        assert_eq!(val_1.get_name().to_str().unwrap(), "test1");

        scope.start_scope("test1".to_string());
        let Ok(value_2) = builder.build_alloca(context.i32_type(), "test2") else {
            panic!()
        };
        scope.add_to_scope("bobbis".to_string(), value_2).unwrap();

        let Some(val_2) = scope.reference_lookup("bobbis".to_string()) else {
            panic!()
        };
        assert_eq!(val_2.get_name().to_str().unwrap(), "test2");

        scope.start_scope("test2".to_string());
        let Ok(value_3) = builder.build_alloca(context.i32_type(), "test3") else {
            panic!()
        };
        scope.add_to_scope("bobbis".to_string(), value_3).unwrap();

        let Some(val_3) = scope.reference_lookup("bobbis".to_string()) else {
            panic!()
        };
        assert_eq!(val_3.get_name().to_str().unwrap(), "test3");

        scope.end_scope("test2".to_string()).unwrap();

        let Some(val_2) = scope.reference_lookup("bobbis".to_string()) else {
            panic!()
        };
        assert_eq!(val_2.get_name().to_str().unwrap(), "test2");

        scope.end_scope("test1".to_string()).unwrap();

        let Some(val_1) = scope.reference_lookup("bobbis".to_string()) else {
            panic!()
        };
        assert_eq!(val_1.get_name().to_str().unwrap(), "test1");
    }
}

mod codegen_tests {
    use crate::{ast::CondBlock, parser};

    #[cfg(test)]
    use super::*;

    #[test]
    fn test_init() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context);
        let mut program = setup_codegen_tests(&mut codegen);

        let Ok(statement) = parser::_parse_statement("a of int = 3;") else {
            // TODO fix with some way of creating ASTs properly rather than relying on parser
            panic!()
        };

        match codegen.statement_build(statement, &mut program) {
            Ok(_) => {}
            Err(e) => {
                println!("failed {:?}", e);
                panic!()
            }
        };

        let output = codegen.print_llvm_function(codegen.module.get_function("main").unwrap());

        assert_eq!("define i32 @main() {\nentry:\n  %a = alloca i32, align 4\n  store i32 3, ptr %a, align 4\n}\n", output)
    }

    #[test]
    fn test_decl_assign() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context);
        let mut program = setup_codegen_tests(&mut codegen);

        // Add a declaration, includes "a" in scope
        match codegen.statement_build(
            Box::new(ASTStatement::Decl("a".to_string(), Box::new(ASTType::Int))),
            &mut program,
        ) {
            Ok(_) => {}
            Err(e) => {
                println!("failed {:?}", e);
                panic!()
            }
        };

        // Do the assignment, for "a" that was found in scope
        match codegen.statement_build(
            Box::new(ASTStatement::Assign(
                "a".to_string(),
                Box::new(Expr::Int(3)),
            )),
            &mut program,
        ) {
            Ok(_) => {}
            Err(e) => {
                println!("failed {:?}", e);
                panic!()
            }
        };

        let output = codegen.print_llvm_function(codegen.module.get_function("main").unwrap());

        /*
        define i32 @main() {
            entry:
                %a = alloca i32, align 4
                store i32 3, ptr %a, align 4
        }
        */

        assert_eq!("define i32 @main() {\nentry:\n  %a = alloca i32, align 4\n  store i32 3, ptr %a, align 4\n}\n", output)
    }

    #[test]
    fn test_if_else() {
        let context = Context::create();
        let mut codegen = CodeGen::new(&context);
        let mut program = setup_codegen_tests(&mut codegen);

        // Add a declaration, includes "a" in scope
        match codegen.statement_build(
            Box::new(ASTStatement::If(
                CondBlock(
                    Box::new(Expr::Bool(true)),
                    Box::new(ASTStatement::Init(
                        "a".to_string(),
                        Box::new(ASTType::Int),
                        Box::new(Expr::Int(3)),
                    )),
                ),
                Vec::new(),
                Some(Box::new(ASTStatement::Init(
                    "b".to_string(),
                    Box::new(ASTType::Int),
                    Box::new(Expr::Int(3)),
                ))),
            )),
            &mut program,
        ) {
            Ok(_) => {}
            Err(e) => {
                println!("failed {:?}", e);
                panic!()
            }
        };

        let output = codegen.print_llvm_function(codegen.module.get_function("main").unwrap());

        /*
        define i32 @main() {
            entry:
                br i1 true, label %if, label %else

            if: ; preds = %entry
                %a = alloca i32, align 4
                store i32 3, ptr %a, align 4

            else: ; preds = %entry
                %b = alloca i32, align 4
                store i32 3, ptr %b, align 4
        }

        */
        assert_eq!("define i32 @main() {\nentry:\n  br i1 true, label %if, label %else\n\nif:                                               ; preds = %entry\n  %a = alloca i32, align 4\n  store i32 3, ptr %a, align 4\n\nelse:                                             ; preds = %entry\n  %b = alloca i32, align 4\n  store i32 3, ptr %b, align 4\n}\n", output)
    }
}
