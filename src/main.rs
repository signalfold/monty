use std::fmt::Debug;
use rustpython_parser::ast::{Stmt, StmtKind, Expr as AstExpr, ExprKind, Constant, Operator, Keyword};
use rustpython_parser::parse_program;

fn main() {
    monty();
}


#[derive(Debug, Clone)]
enum Value {
    Int(i64),
    Float(f64),
    Str(String),
    List(Vec<Value>),
    Range(i64),
    True,
    False,
    None,
}

#[derive(Debug, Clone)]
enum Expr {
    Assign {
        target: String,
        value: Box<Expr>,
    },
    Constant(Constant),
    Name(String),
    Call {
        func: String,
        args: Vec<Expr>,
        kwargs: Vec<(String, Expr)>,
    },
    Op {
        left: Box<Expr>,
        op: Operator,
        right: Box<Expr>,
    },
    List(Vec<Expr>),
}


#[derive(Debug, Clone)]
enum Node {
    Pass,
    Expression(Expr),
    For {
        target: Expr,
        iter: Expr,
        body: Vec<Node>,
        or_else: Vec<Node>,
    },
    If {
        test: Expr,
        body: Vec<Node>,
        or_else: Vec<Node>,
    },
}


fn monty() {
    let code = "for i in foo:\n x = '1'\n";
    let ast = parse_program(code, "example.py").unwrap();
    dbg!(&ast);
    let nodes = parse_statements(ast).unwrap();
    dbg!(nodes);
}

type ParseResult<T> = Result<T, String>;

fn parse_statements(statements: Vec<Stmt>) -> ParseResult<Vec<Node>> {
    statements.into_iter().map(|e| parse_statement(e)).collect()
}

fn parse_statement(statement: Stmt) -> ParseResult<Node> {
    match statement.node {
        StmtKind::FunctionDef {name, args, body, decorator_list, returns, type_comment} => todo!("FunctionDef"),
        StmtKind::AsyncFunctionDef {name, args, body, decorator_list, returns, type_comment} => todo!("AsyncFunctionDef"),
        StmtKind::ClassDef {name, bases, keywords, body, decorator_list} => todo!("ClassDef"),
        StmtKind::Return {value} => todo!("Return"),
        StmtKind::Delete {targets} => todo!("Delete"),
        StmtKind::Assign {targets, value, ..} => parse_assignment(first(targets)?, *value),
        StmtKind::AugAssign {target, op, value} => todo!("AugAssign"),
        StmtKind::AnnAssign {target, value, ..} => match value {
            Some(value) => parse_assignment(*target, *value),
            None => Ok(Node::Pass),
        },
        StmtKind::For {target, iter, body, orelse, .. } => {
            let target = parse_expression(*target)?;
            let iter = parse_expression(*iter)?;
            let body = parse_statements(body)?;
            let or_else = parse_statements(orelse)?;
            Ok(Node::For { target, iter, body, or_else })
        },
        StmtKind::AsyncFor {target, iter, body, orelse, type_comment} => todo!("AsyncFor"),
        StmtKind::While {test, body, orelse} => todo!("While"),
        StmtKind::If {test, body, orelse} => {
            let test = parse_expression(*test)?;
            let body = parse_statements(body)?;
            let or_else = parse_statements(orelse)?;
            Ok(Node::If {test, body, or_else})
        },
        StmtKind::With {items, body, type_comment} => todo!("With"),
        StmtKind::AsyncWith {items, body, type_comment} => todo!("AsyncWith"),
        StmtKind::Match {subject, cases} => todo!("Match"),
        StmtKind::Raise {exc, cause} => todo!("Raise"),
        StmtKind::Try {body, handlers, orelse, finalbody} => todo!("Try"),
        StmtKind::TryStar {body, handlers, orelse, finalbody} => todo!("TryStar"),
        StmtKind::Assert {test, msg} => todo!("Assert"),
        StmtKind::Import {names} => todo!("Import"),
        StmtKind::ImportFrom {module, names, level} => todo!("ImportFrom"),
        StmtKind::Global {names} => todo!("Global"),
        StmtKind::Nonlocal {names} => todo!("Nonlocal"),
        StmtKind::Expr {value} => todo!("Expr"),
        StmtKind::Pass => Ok(Node::Pass),
        StmtKind::Break => todo!("Break"),
        StmtKind::Continue => todo!("Continue"),
    }
}

/// `lhs = rhs` -> `lhs, rhs`
fn parse_assignment(lhs: AstExpr, rhs: AstExpr) -> ParseResult<Node> {
    let target = get_name(lhs)?;
    let value = Box::new(parse_expression(rhs)?);
    Ok(Node::Expression(Expr::Assign { target, value }))
}

fn parse_expression(expression: AstExpr) -> ParseResult<Expr> {
    match expression.node {
        ExprKind::BoolOp { op, values } => { todo!("BoolOp") },
        ExprKind::NamedExpr { target, value } => todo!("NamedExpr"),
        ExprKind::BinOp { left, op, right } => {
            let left = Box::new(parse_expression(*left)?);
            let right = Box::new(parse_expression(*right)?);
            Ok(Expr::Op { left, op, right })
        },
        ExprKind::UnaryOp { op, operand } => todo!("UnaryOp"),
        ExprKind::Lambda { args, body } => todo!("Lambda"),
        ExprKind::IfExp { test, body, orelse } => todo!("IfExp"),
        ExprKind::Dict { keys, values } => todo!("Dict"),
        ExprKind::Set { elts } => todo!("Set"),
        ExprKind::ListComp { elt, generators } => todo!("ListComp"),
        ExprKind::SetComp { elt, generators } => todo!("SetComp"),
        ExprKind::DictComp { key, value, generators } => todo!("DictComp"),
        ExprKind::GeneratorExp { elt, generators } => todo!("GeneratorExp"),
        ExprKind::Await { value } => todo!("Await"),
        ExprKind::Yield { value } => todo!("Yield"),
        ExprKind::YieldFrom { value } => todo!("YieldFrom"),
        ExprKind::Compare { left, ops, comparators } => todo!("Compare"),
        ExprKind::Call { func, args, keywords } => {
            let func = get_name(*func)?;
            let args = args.into_iter().map(parse_expression).collect::<ParseResult<Vec<_>>>()?;
            let kwargs = keywords.into_iter().map(parse_kwargs).collect::<ParseResult<Vec<_>>>()?;
            Ok(Expr::Call { func, args, kwargs })
        },
        ExprKind::FormattedValue { value, conversion, format_spec } => todo!("FormattedValue"),
        ExprKind::JoinedStr { values } => todo!("JoinedStr"),
        ExprKind::Constant { value, kind } => Ok(Expr::Constant(value)),
        ExprKind::Attribute { value, attr, ctx } => todo!("Attribute"),
        ExprKind::Subscript { value, slice, ctx } => todo!("Subscript"),
        ExprKind::Starred { value, ctx } => todo!("Starred"),
        ExprKind::Name { id, .. } => Ok(Expr::Name(id)),
        ExprKind::List { elts, ctx } => todo!("List"),
        ExprKind::Tuple { elts, ctx } => todo!("Tuple"),
        ExprKind::Slice { lower, upper, step } => todo!("Slice"),
    }
}

fn parse_kwargs(kwarg: Keyword) -> ParseResult<(String, Expr)> {
    let key = match kwarg.node.arg {
        Some(key) => key,
        None => return Err("kwargs with no key".to_string()),
    };
    let value = parse_expression(kwarg.node.value)?;
    Ok((key, value))
}

fn get_name(lhs: AstExpr) -> ParseResult<String> {
    match lhs.node {
        ExprKind::Name { id, .. } => Ok(id),
        _ => Err(format!("Expected name, got {:?}", lhs.node)),
    }
}

fn first<T: Debug>(v: Vec<T>) -> ParseResult<T> {
    if v.len() != 1 {
        return Err(format!("Expected 1 element, got {} (raw: {v:?})", v.len()));
    }
    v.into_iter().next().ok_or_else(|| "Expected 1 element, got 0".to_string())
}
