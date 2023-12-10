use enum_as_inner::EnumAsInner;
use pest::{iterators::Pair, Parser};
use pest_derive::Parser;
use std::{fmt, rc::Rc};

#[derive(Parser)]
#[grammar = "paig.pest"]
struct PaigParser;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Num(f64),
    Id(String),
    String(String),
    If {
        test: Box<Expr>,
        then: Box<Expr>,
        else_: Box<Expr>,
    },
    Blam {
        params: Vec<String>,
        body: Box<Expr>,
    },
    Apply {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
}

type Env = im::HashMap<String, Value>;

pub fn wrap_op(op: Box<dyn Fn(f64, f64) -> f64>) -> Value {
    Value::Standard(Rc::new(move |args: Vec<Value>| -> Value {
        let [lhs, rhs] = args.try_into().unwrap();
        Value::Num(op(lhs.into_num().unwrap(), rhs.into_num().unwrap()))
    }))
}

pub fn env_new() -> Env {
    let mut env = Env::new();
    env.insert("+".to_string(), wrap_op(Box::new(|a, b| a + b)));
    env.insert("-".to_string(), wrap_op(Box::new(|a, b| a - b)));
    env.insert("*".to_string(), wrap_op(Box::new(|a, b| a * b)));
    env.insert("/".to_string(), wrap_op(Box::new(|a, b| a / b)));
    env.insert(
        "<=".to_string(),
        Value::Standard(Rc::new(move |args: Vec<Value>| -> Value {
            let [lhs, rhs] = args.try_into().unwrap();
            Value::Boolean(lhs.into_num().unwrap() <= rhs.into_num().unwrap())
        })),
    );
    env
}

#[derive(Clone, EnumAsInner)]
pub enum Value {
    Num(f64),
    String(String),
    Boolean(bool),
    Closure {
        params: Vec<String>,
        body: Expr,
        env: Env,
    },
    Standard(Rc<dyn Fn(Vec<Value>) -> Value>),
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(num) => write!(f, "Num({})", num),
            Value::String(s) => write!(f, "String({})", s),
            Value::Boolean(b) => write!(f, "Boolean({})", b),
            Value::Closure { params, body, env } => {
                let mut debug_struct = f.debug_struct("Closure");
                debug_struct.field("params", params);
                debug_struct.field("body", body);
                debug_struct.field("env", env);
                debug_struct.finish()
            }
            Value::Standard(_) => write!(f, "Standard(Rc<dyn Fn(Vec<Value>) -> Value>)"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Num(num) => write!(f, "{}", num),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Closure { params: _, body: _, env: _ } => write!(f, "#<closure>"),
            Value::Standard(_) => write!(f, "#<primop>"),
        }
    }
}


impl Expr {
    pub fn parse(input: &str) -> Expr {
        let pair = PaigParser::parse(Rule::expr, input)
            .unwrap()
            .next()
            .unwrap();
        Self::parse_expr(pair)
    }

    pub fn parse_expr(pair: Pair<'_, Rule>) -> Expr {
        match pair.as_rule() {
            Rule::num => Expr::Num(pair.as_str().parse().unwrap()),
            Rule::id => Expr::Id(pair.as_str().to_string()),
            Rule::string => Expr::String(pair.as_str().to_string()),
            Rule::r#if => {
                let [test, then, else_] = pair
                    .into_inner()
                    .map(Self::parse_expr)
                    .map(Box::new)
                    .collect::<Vec<_>>()
                    .try_into()
                    .unwrap();
                Expr::If { test, then, else_ }
            }
            Rule::blam => {
                let [params, body] = pair.into_inner().collect::<Vec<_>>().try_into().unwrap();
                let params: Vec<_> = params
                    .into_inner()
                    .map(|p| p.as_str().to_string())
                    .collect();
                let body = Box::new(Self::parse_expr(body));
                Expr::Blam { params, body }
            }
            Rule::apply => {
                let mut parts = pair.into_inner().map(Self::parse_expr);
                let func = Box::new(parts.next().unwrap());
                let args: Vec<_> = parts.collect();
                Expr::Apply { func, args }
            }
            _ => unreachable!(),
        }
    }
}

struct Interpreter;

impl Interpreter {
    pub fn interp(ast: Expr, env: &mut Env) -> Value {
        match ast {
            Expr::Num(n) => Value::Num(n),
            Expr::String(s) => Value::String(s),
            Expr::Id(id) => env
                .get(&id)
                .expect(&format!("unknown variable: {}", id))
                .clone(),
            Expr::If { test, then, else_ } => {
                if Self::interp(*test, env).into_boolean().unwrap() {
                    Self::interp(*then, env)
                } else {
                    Self::interp(*else_, env)
                }
            }
            Expr::Blam { params, body } => Value::Closure {
                params,
                body: *body,
                env: env.clone(),
            },
            Expr::Apply { func, args } => match Self::interp(*func, env) {
                Value::Closure {
                    params,
                    body,
                    env: clo_env,
                } => {
                    let mut clo_env = clo_env.clone();
                    for (param, arg) in params.iter().zip(args.into_iter()) {
                        let arg = Self::interp(arg, env);
                        clo_env.insert(param.clone(), arg);
                    }
                    Self::interp(body, &mut clo_env)
                }
                Value::Standard(func) => func(args.into_iter().map(|e| Self::interp(e, env)).collect()),
                _ => panic!("Value not callable"),
            },
        }
    }
}

fn main() {
    let input =
        "{{blam (fac) {fac fac 8}} {blam (self n) {{<= n 0} ? 1 else: {* n {self self {- n 1}}}}}}";

    let pair = PaigParser::parse(Rule::expr, input)
        .unwrap()
        .next()
        .unwrap();
    let ast = Expr::parse_expr(pair);
    println!("{}", Interpreter::interp(ast, &mut env_new()));
}
