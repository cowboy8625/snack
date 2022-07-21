#![allow(dead_code)]
use parser::{Builtin, Expr, Value};
type Span = std::ops::Range<usize>;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Type {
    Int,
    Bool,
    Char,
    String,
    Word,
    Null,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypedExpr {
    Value(Value, Span, Type),
    Builtin(Builtin, Span, Type),
    While(Vec<Self>, Span),
    If(Vec<Self>, Span, Type),
    IfElse(Vec<Self>, Vec<Self>, Span, Type),
    Word(String, Vec<String>, Vec<Self>, Span, Type),
    Error(String, Span),
}

impl TypedExpr {
    pub fn span(&self) -> Span {
        match self {
            Self::Value(_, span, _) => span.clone(),
            Self::Builtin(_, span, _) => span.clone(),
            Self::While(_, span) => span.clone(),
            Self::If(_, span, _) => span.clone(),
            Self::IfElse(_, _, span, _) => span.clone(),
            Self::Word(_, _, _, span, _) => span.clone(),
            Self::Error(_, span) => span.clone(),
        }
    }
    pub fn type_(&self) -> Option<Type> {
        match self {
            Self::Value(_, _, t) => Some(t.clone()),
            Self::Builtin(_, _, t) => Some(t.clone()),
            Self::While(_, _) => None,
            Self::If(_, _, t) => Some(t.clone()),
            Self::IfElse(_, _, _, t) => Some(t.clone()),
            Self::Word(_, _, _, _, t) => Some(t.clone()),
            Self::Error(_, _) => None,
        }
    }
}

fn type_of_value(value: &Value) -> Type {
    match value {
        Value::U64(_) => Type::Int,
        Value::Bool(_) => Type::Bool,
        Value::String(_, _) => Type::String,
        Value::Char(_) => Type::Char,
    }
}

fn type_of_builtin(builtin: &Builtin) -> Type {
    match builtin {
        Builtin::Add => Type::Int,
        Builtin::Sub => Type::Int,
        Builtin::Mul => Type::Int,
        Builtin::Div => Type::Int,
        Builtin::Grt => Type::Bool,
        Builtin::Les => Type::Bool,
        Builtin::Geq => Type::Bool,
        Builtin::Leq => Type::Bool,
        Builtin::Equ => Type::Bool,
        Builtin::Not => Type::Bool,
        Builtin::Dot => Type::Null,
        Builtin::And => Type::Int,
        Builtin::Or => Type::Int,
        //FIXME: figure out what data type they are operating on
        Builtin::Copy => Type::Int,
        Builtin::Over => Type::Int,
        Builtin::Rot => Type::Int,
        Builtin::Swap => Type::Int,
        Builtin::Drop => Type::Int,
        Builtin::Max => Type::Int,
        // ----
        Builtin::SysCall1 => Type::Null,
        Builtin::SysCall2 => Type::Null,
        Builtin::SysCall3 => Type::Null,
    }
}

fn type_of_if(_expr: &[Expr]) -> Type {
    Type::Int
}

fn check_stack(block: &[TypedExpr], _return_type: Type) {
    let _stack = create_stack(block);
}

fn create_stack(items: &[TypedExpr]) -> Vec<TypedExpr> {
    let mut stack = Vec::new();
    for op in items.iter() {
        match op {
            value @ TypedExpr::Value(_, _, _) => stack.push(value.clone()),
            TypedExpr::Builtin(builtin, _, _) => {
                builtin_on_stack(builtin, &mut stack);
            }
            TypedExpr::While(_, _) => {}
            TypedExpr::If(_, _, _) => {}
            TypedExpr::IfElse(_, _, _, _) => {}
            TypedExpr::Word(_, _, _, _, _) => {}
            TypedExpr::Error(_, _) => {}
        }
    }
    stack
}

fn builtin_on_stack(builtin: &Builtin, _stack: &mut Vec<TypedExpr>) {
    match builtin {
        Builtin::Add => {}
        Builtin::Sub => {}
        Builtin::Mul => {}
        Builtin::Div => {}
        Builtin::Grt => {}
        Builtin::Les => {}
        Builtin::Geq => {}
        Builtin::Leq => {}
        Builtin::Equ => {}
        Builtin::Not => {}
        Builtin::Dot => {}
        Builtin::Copy => {}
        Builtin::Over => {}
        Builtin::Rot => {}
        Builtin::Swap => {}
        Builtin::Drop => {}
        Builtin::Max => {}
        Builtin::And => {}
        Builtin::Or => {}
        Builtin::SysCall1 => {}
        Builtin::SysCall2 => {}
        Builtin::SysCall3 => {}
    }
}

fn type_check_block(block: &[TypedExpr]) {
    check_stack(block, Type::Null);
}

fn _type_of(expr: &Expr) -> Type {
    match expr {
        Expr::Value(value, _) => type_of_value(value),
        Expr::Builtin(builtin, _) => type_of_builtin(builtin),
        Expr::While(_, _) => unimplemented!(),
        Expr::If(_, _) => unimplemented!(),
        Expr::IfElse(_, _, _) => unimplemented!(),
        Expr::Word(_, _, _, _) => unimplemented!(),
        Expr::Error(_, _) => unimplemented!(),
    }
}

pub fn type_check_ast(ast: &[Expr]) -> Vec<TypedExpr> {
    ast.iter()
        .map(|expr| match &expr {
            Expr::Value(value, span) => {
                TypedExpr::Value(value.clone(), span.clone(), type_of_value(value))
            }
            Expr::Builtin(builtin, span) => {
                // FIXME: Wrong Type
                //      I think these need to check if the correct number of items are on the
                //      stack to do there operations.
                TypedExpr::Builtin(builtin.clone(), span.clone(), type_of_builtin(builtin))
            }
            Expr::While(body, span) => TypedExpr::While(type_check_ast(body), span.clone()),
            Expr::If(body, span) => {
                TypedExpr::If(type_check_ast(body), span.clone(), type_of_if(body))
            }
            Expr::IfElse(_, _, _) => unimplemented!(),
            Expr::Word(_, _, _, _) => unimplemented!(),
            Expr::Error(_, _) => unimplemented!(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
