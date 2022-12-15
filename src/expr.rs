use std::ops::Deref;

use crate::error::{Error, Result};
use crate::parse_error;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Quantity {
    pub value: f64,
    pub unit: Option<String>,
}

impl Quantity {
    pub fn parse(input: &str) -> Result<Self> {
        if let Ok((_, result)) = parse::quantity(input) {
            Ok(result)
        } else {
            Err(parse_error!("Could not parse quantity '{}'", input))
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Expr {
    // Simple Types
    F64(f64),
    Var(String),
    // Binary Expressions
    Add(Vec<Expr>),
    Mul(Vec<Expr>),
    Pow(Vec<Expr>),
    // Builtin Functions
    Exp(Box<Expr>),
    Log(Box<Expr>),
    Sqrt(Box<Expr>),
    ProximalDistanceFromRegion(String),
    DistanceFromRoot(),
    // Unknown, but possibly builtin functions
    Fun(String, Box<Expr>),
}

impl Expr {
    pub fn map(&self, f: &impl Fn(&Expr) -> Expr) -> Expr {
        match self {
            Expr::Add(vs) => Expr::Add(vs.iter().map(|v| v.map(f)).collect()),
            Expr::Mul(vs) => Expr::Mul(vs.iter().map(|v| v.map(f)).collect()),
            Expr::Pow(vs) => Expr::Pow(vs.iter().map(|v| v.map(f)).collect()),
            Expr::Exp(b) => Expr::Exp(Box::new(b.map(f))),
            Expr::Log(b) => Expr::Log(Box::new(b.map(f))),
            Expr::Sqrt(b) => Expr::Sqrt(Box::new(b.map(f))),
            Expr::Fun(a, b) => Expr::Fun(a.clone(), Box::new(b.map(f))),
            e => f(e),
        }
    }

    pub fn rename(&self, f: &impl Fn(&String) -> String) -> Self {
        let fun = |v: &Expr| -> Expr {
            if let Expr::Var(s) = v {
                Expr::Var(f(s))
            } else {
                v.clone()
            }
        };
        self.map(&fun)
    }

    pub fn fold<T>(&self, acc: &mut T, f: &impl Fn(&Expr, &mut T)) {
        f(self, acc);
        match self {
            Expr::Add(vs) => vs.iter().for_each(|v| v.fold(acc, f)),
            Expr::Mul(vs) => vs.iter().for_each(|v| v.fold(acc, f)),
            Expr::Pow(vs) => vs.iter().for_each(|v| v.fold(acc, f)),
            Expr::Exp(b) => b.fold(acc, f),
            Expr::Log(b) => b.fold(acc, f),
            Expr::Sqrt(b) => b.fold(acc, f),
            Expr::Fun(_, b) => b.fold(acc, f),
            _ => {}
        }
    }

    pub fn parse(input: &str) -> Result<Self> {
        match parse::expr(input) {
            Ok(("", result)) => Ok(result.simplify()),
            Ok((rs, _)) => Err(parse_error!(
                "Could not parse expression '{}': remainder '{}' at end",
                input,
                rs
            )),
            Err(x) => Err(parse_error!(
                "Could not parse expression '{}': {:?}",
                input,
                x
            )),
        }
    }

    pub fn print_to_string(&self) -> String {
        match &self {
            Expr::F64(x) => format!("{x}"),
            Expr::Var(x) => x.to_string(),
            Expr::Fun(f, x) => format!("{f}({})", x.print_to_string()),
            Expr::Exp(x) => format!("exp({})", x.print_to_string()),
            Expr::Log(x) => format!("log({})", x.print_to_string()),
            Expr::Sqrt(x) => format!("({})^0.5", x.print_to_string()), // NB. () needed since we want to call sqrt(...)
            Expr::Add(xs) => xs
                .iter()
                .map(|x| x.print_to_string())
                .collect::<Vec<_>>()
                .join(" + "),
            Expr::Mul(xs) => xs
                .iter()
                .map(|x| {
                    if let Expr::Add(_) = x {
                        format!("({})", x.print_to_string())
                    } else {
                        x.print_to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join(" * "),
            Expr::Pow(xs) => match &xs[..] {
                [Expr::Var(x), Expr::F64(k)] if *k >= 0.0 && k.fract() < f64::EPSILON => {
                    vec![x.clone(); *k as usize].join(" * ")
                }
                _ => xs
                    .iter()
                    .map(|x| match x {
                        Expr::Add(_) | Expr::Mul(_) => format!("({})", x.print_to_string()),
                        _ => x.print_to_string(),
                    })
                    .collect::<Vec<_>>()
                    .join("^"),
            },
            Expr::ProximalDistanceFromRegion(_) => {
                panic!("ProximalDistanceFromRegion can not be constructed in xml")
            }
            Expr::DistanceFromRoot() => {
                panic!("DistanceFromRoot can not be constructed in xml")
            }
        }
    }

    pub fn simplify(&self) -> Self {
        let mut done = false;
        let mut old = self.clone();
        while !done {
            let new = match &old {
                Expr::Pow(vs) => simplify_pow(vs),
                Expr::Add(vs) => simplify_add(vs),
                Expr::Mul(vs) => simplify_mul(vs),
                Expr::Exp(vs) => simplify_exp(vs),
                Expr::Log(vs) => simplify_log(vs),
                Expr::Sqrt(vs) => simplify_sqrt(vs),
                Expr::Fun(n, x) => Expr::Fun(n.into(), Box::new(x.simplify())),
                e => e.clone(),
            };
            done = old == new;
            old = new
        }
        old
    }

    pub fn is_var_with_name(&self, name: &str) -> bool {
        matches!(self, Expr::Var(x) if name == x)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmnt {
    Ift(Boolean, Box<Stmnt>, Box<Option<Stmnt>>),
    Ass(String, Expr),
}

impl Stmnt {
    pub fn simplify(&self) -> Self {
        match self {
            Stmnt::Ass(s, e) => Stmnt::Ass(s.to_string(), e.simplify()),
            Stmnt::Ift(c, t, e) => {
                let c = c.simplify();
                let t = t.simplify();
                let e = e.deref().as_ref().map(|i| i.simplify());
                match c {
                    Boolean::Lit(true) => t,
                    Boolean::Lit(false) => e.unwrap(),
                    _ => Stmnt::Ift(c, Box::new(t), Box::new(e)),
                }
            }
        }
    }

    pub fn set_name(&self, from: &str, to: &str) -> Self {
        match self {
            Stmnt::Ass(s, e) if s == from => Stmnt::Ass(to.to_string(), e.clone()),
            Stmnt::Ift(c, t, e) => {
                let c = c.clone();
                let t = t.set_name(from, to);
                let e = e.deref().as_ref().map(|i| i.set_name(from, to));
                Stmnt::Ift(c, Box::new(t), Box::new(e))
            }
            _ => self.clone(),
        }
    }

    pub fn print_to_string(&self, ind: usize) -> String {
        match self {
            Stmnt::Ass(n, e) => {
                format!("{:ind$}{n} = {}", "", e.print_to_string())
            }
            Stmnt::Ift(c, t, f) => {
                if let Some(f) = f.deref() {
                    format!(
                        "{:width$}if ({}) {{
{}
{:width$}}} else {{
{}
{:width$}}}",
                        " ",
                        c.print_to_string(),
                        t.print_to_string(ind + 2),
                        " ",
                        f.print_to_string(ind + 2),
                        " ",
                        width = ind
                    )
                } else {
                    format!(
                        "{:width$}if ({}) {{
{}
{:width$}}}",
                        " ",
                        c.print_to_string(),
                        t.print_to_string(ind + 2),
                        " ",
                        width = ind
                    )
                }
            }
        }
    }

    pub fn map(&self, f: &impl Fn(&Expr) -> Expr) -> Stmnt {
        match self {
            Stmnt::Ass(s, e) => Stmnt::Ass(s.to_string(), e.map(f)),
            Stmnt::Ift(c, t, e) => {
                if let Some(e) = e.as_ref() {
                    Stmnt::Ift(c.map(f), Box::new(t.map(f)), Box::new(Some(e.map(f))))
                } else {
                    Stmnt::Ift(c.map(f), Box::new(t.map(f)), Box::new(None))
                }
            }
        }
    }

    pub fn fold<T>(&self, acc: &mut T, f: &impl Fn(&Expr, &mut T)) {
        match self {
            Stmnt::Ass(_, e) => e.fold(acc, f),
            Stmnt::Ift(c, t, e) => {
                c.fold(acc, f);
                t.fold(acc, f);
                if let Some(i) = e.as_ref() {
                    i.fold(acc, f);
                }
            }
        }
    }

    pub fn rename(&self, f: &impl Fn(&String) -> String) -> Stmnt {
        match self {
            Stmnt::Ass(s, e) => Stmnt::Ass(f(s), e.rename(f)),
            Stmnt::Ift(c, t, e) => {
                if let Some(e) = e.as_ref() {
                    Stmnt::Ift(
                        c.rename(f),
                        Box::new(t.rename(f)),
                        Box::new(Some(e.rename(f))),
                    )
                } else {
                    Stmnt::Ift(c.rename(f), Box::new(t.rename(f)), Box::new(None))
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum Op {
    And,
    Or,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub enum Cmp {
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Boolean {
    Op(Op, Box<Boolean>, Box<Boolean>),
    Cmp(Cmp, Box<Expr>, Box<Expr>),
    Lit(bool),
}

impl Boolean {
    pub fn print_to_string(&self) -> String {
        match &self {
            Boolean::Lit(b) => format!("{b}"),
            Boolean::Cmp(o, l, r) => {
                let op = match o {
                    Cmp::Eq => "==",
                    Cmp::Ne => "!=",
                    Cmp::Ge => ">=",
                    Cmp::Le => "<=",
                    Cmp::Gt => ">",
                    Cmp::Lt => "<",
                };
                format!("{} {op} {}", l.print_to_string(), r.print_to_string())
            }
            Boolean::Op(o, l, r) => {
                let op = match o {
                    Op::And => "&&",
                    Op::Or => "||",
                };
                format!("{} {op} {}", l.print_to_string(), r.print_to_string())
            }
        }
    }

    pub fn parse(input: &str) -> Result<Self> {
        if let Ok((_, result)) = parse::boolean(input) {
            Ok(result.simplify())
        } else {
            Err(parse_error!("Could not parse {}", input))
        }
    }

    pub fn simplify(&self) -> Self {
        match self {
            Boolean::Cmp(o, l, r) => {
                let l = l.simplify();
                let r = r.simplify();
                if l == r {
                    match o {
                        Cmp::Ge | Cmp::Le | Cmp::Eq => Boolean::Lit(true),
                        Cmp::Gt | Cmp::Lt | Cmp::Ne => Boolean::Lit(false),
                    }
                } else {
                    match (&l, &r) {
                        (Expr::F64(x), Expr::F64(y)) => {
                            let r = match o {
                                Cmp::Eq => x == y,
                                Cmp::Ne => x != y,
                                Cmp::Lt => x < y,
                                Cmp::Le => x <= y,
                                Cmp::Gt => x > y,
                                Cmp::Ge => x >= y,
                            };
                            Boolean::Lit(r)
                        }
                        _ => Boolean::Cmp(*o, Box::new(l), Box::new(r)),
                    }
                }
            }
            Boolean::Op(o, l, r) => {
                let l = l.simplify();
                let r = r.simplify();
                // x && x == x || x
                if l == r {
                    return l;
                }
                match o {
                    Op::And => match (&l, &r) {
                        // eliminate literals
                        (Boolean::Lit(true), r) => return r.clone(),
                        (l, Boolean::Lit(true)) => return l.clone(),
                        (Boolean::Lit(false), _) |
                        (_, Boolean::Lit(false)) => return Boolean::Lit(false),
                        // peek one level into comparisons
                        (Boolean::Cmp(el, xl, yl), Boolean::Cmp(er, xr, yr))
                            if xl == xr && yl == yr => {
                                match (&el, &er) {
                                    // ...
                                    (u, v) if u == v =>  return Boolean::Cmp(**u, xl.clone(), yl.clone()),
                                    // contradiction
                                    (Cmp::Ne, Cmp::Eq) |
                                    (Cmp::Eq, Cmp::Ne) |
                                    (Cmp::Gt, Cmp::Lt) |
                                    (Cmp::Lt, Cmp::Gt) |
                                    (Cmp::Ge, Cmp::Lt) |
                                    (Cmp::Lt, Cmp::Ge) |
                                    (Cmp::Gt, Cmp::Le) |
                                    (Cmp::Le, Cmp::Gt) => return Boolean::Lit(false),
                                    // redundant
                                    (Cmp::Le, Cmp::Eq) |
                                    (Cmp::Eq, Cmp::Le) |
                                    (Cmp::Le, Cmp::Lt) |
                                    (Cmp::Lt, Cmp::Le) => return Boolean::Cmp(Cmp::Le, xl.clone(), yl.clone()),
                                    (Cmp::Ge, Cmp::Eq) |
                                    (Cmp::Eq, Cmp::Ge) |
                                    (Cmp::Ge, Cmp::Gt) |
                                    (Cmp::Gt, Cmp::Ge) => return Boolean::Cmp(Cmp::Ge, xl.clone(), yl.clone()),
                                    // intersection
                                    (Cmp::Ge, Cmp::Le) |
                                    (Cmp::Le, Cmp::Ge) => return Boolean::Cmp(Cmp::Eq, xl.clone(), yl.clone()),
                                    _ => unreachable!(),
                                }
                            }
                        (l@Boolean::Cmp(_, xl, yl), Boolean::Cmp(er, xr, yr))
                            if xl == yr && yl == xr => {
                                // want to swap the second comparison
                                let er = match er {
                                    Cmp::Eq => Cmp::Eq,
                                    Cmp::Ne => Cmp::Ne,
                                    Cmp::Ge => Cmp::Le,
                                    Cmp::Le => Cmp::Ge,
                                    Cmp::Gt => Cmp::Lt,
                                    Cmp::Lt => Cmp::Gt,
                                };
                                return Boolean::Op(Op::And,
                                                   Box::new(l.clone()),
                                                   Box::new(Boolean::Cmp(er,
                                                                         yr.clone(),
                                                                         xr.clone())))
                            }
                        _ => {}
                    }
                    Op::Or => match (&l, &r) {
                        // eliminate literals
                        (Boolean::Lit(false), r) => return r.clone(),
                        (l, Boolean::Lit(false)) => return l.clone(),
                        (Boolean::Lit(true), _) |
                        (_, Boolean::Lit(true)) => return Boolean::Lit(true),
                        // peek one level into comparisons
                        (Boolean::Cmp(el, xl, yl), Boolean::Cmp(er, xr, yr))
                            if xl == xr && yl == yr => {
                                match (&el, &er) {
                                    // ...
                                    (u, v) if u == v =>  return Boolean::Cmp(**u, xl.clone(), xr.clone()),
                                    // x > y || x < y => x /= y
                                    (Cmp::Lt, Cmp::Gt) | (Cmp::Gt, Cmp::Lt) => return Boolean::Cmp(Cmp::Ne, xl.clone(), yl.clone()),
                                    // tautology
                                    (Cmp::Ne, Cmp::Eq) | (Cmp::Eq, Cmp::Ne) |
                                    (Cmp::Ge, Cmp::Le) | (Cmp::Le, Cmp::Ge) |
                                    (Cmp::Gt, Cmp::Le) | (Cmp::Le, Cmp::Gt) |
                                    (Cmp::Ge, Cmp::Lt) | (Cmp::Lt, Cmp::Ge) => return Boolean::Lit(true),
                                    // redundant
                                    (Cmp::Ge, Cmp::Eq) |
                                    (Cmp::Eq, Cmp::Ge) => return Boolean::Cmp(Cmp::Ge, xl.clone(), yl.clone()),
                                    (Cmp::Le, Cmp::Eq) |
                                    (Cmp::Eq, Cmp::Le) => return Boolean::Cmp(Cmp::Le, xl.clone(), yl.clone()),
                                    _ => unreachable!(),
                                }
                            }
                        (l@Boolean::Cmp(_, xl, yl), Boolean::Cmp(er, xr, yr))
                            if xl == yr && yl == xr => {
                                // want to swap the second comparison
                                let er = match er {
                                    Cmp::Eq => Cmp::Eq,
                                    Cmp::Ne => Cmp::Ne,
                                    Cmp::Ge => Cmp::Le,
                                    Cmp::Le => Cmp::Ge,
                                    Cmp::Gt => Cmp::Lt,
                                    Cmp::Lt => Cmp::Gt,
                                };
                                return Boolean::Op(Op::Or,
                                                   Box::new(l.clone()),
                                                   Box::new(Boolean::Cmp(er,
                                                                         yr.clone(),
                                                                         xr.clone())))
                            }
                        _ => {}
                    }
                }
                Boolean::Op(*o, Box::new(l), Box::new(r))
            }
            _ => self.clone(),
        }
    }

    pub fn rename(&self, f: &impl Fn(&String) -> String) -> Self {
        let fun = |v: &Expr| -> Expr {
            if let Expr::Var(s) = v {
                Expr::Var(f(s))
            } else {
                v.clone()
            }
        };
        self.map(&fun)
    }

    pub fn map(&self, f: &impl Fn(&Expr) -> Expr) -> Boolean {
        match self {
            Boolean::Cmp(o, l, r) => Boolean::Cmp(*o, Box::new(l.map(f)), Box::new(r.map(f))),
            Boolean::Op(o, l, r) => Boolean::Op(*o, Box::new(l.map(f)), Box::new(r.map(f))),
            b => b.clone(),
        }
    }

    pub fn fold<T>(&self, acc: &mut T, f: &impl Fn(&Expr, &mut T)) {
        match self {
            Boolean::Cmp(_, l, r) => {
                l.fold(acc, f);
                r.fold(acc, f);
            }
            Boolean::Op(_, l, r) => {
                l.fold(acc, f);
                r.fold(acc, f);
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum Select {
    All,
    Index(usize),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub enum Path {
    Up,
    Fixed(String),
    When(String, Select),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd)]
pub struct Match(pub Vec<Path>);

impl Match {
    pub fn parse(input: &str) -> Result<Self> {
        if let Ok((_, result)) = parse::path(input) {
            Ok(Match(result))
        } else {
            Err(parse_error!("Could not parse {}", input))
        }
    }

    pub fn add_prefix(&self, pfx: &[String]) -> Self {
        Self(
            pfx.iter()
                .map(|p| Path::Fixed(p.to_string()))
                .chain(self.0.iter().cloned())
                .collect(),
        )
    }

    pub fn on_path(&self, exposures: &[String]) -> Vec<String> {
        let mut ms = Vec::new();
        for ex in exposures {
            let mut es = ex.split('/');
            let mut ok = true;
            for p in &self.0 {
                match p {
                    Path::Fixed(f) => {
                        ok &= if let Some(r) = es.next() {
                            r == f
                        } else {
                            false
                        }
                    }
                    Path::When(f, Select::All) => {
                        ok &= if let Some(r) = es.next() {
                            r == f
                        } else {
                            false
                        };
                        ok &= es.next().is_some();
                    }
                    Path::Up => panic!("Going up not allowed here"),
                    Path::When(_, _) => panic!("Indexing not allowed here"),
                }
            }
            if ok {
                ms.push(ex.to_string());
            }
        }
        ms
    }
}

mod parse {
    use nom::{
        branch::alt,
        bytes::complete::{tag, take_while, take_while1},
        character::complete::{alpha1, one_of, space0},
        character::is_alphanumeric,
        combinator::{fail, opt},
        multi::{fold_many0, separated_list1},
        number::complete::float,
        sequence::{delimited, pair, preceded, tuple},
        IResult,
    };

    use super::{Boolean, Cmp, Expr, Op, Path, Quantity, Select};

    fn up(input: &str) -> IResult<&str, Path> {
        let (input, _) = tag("..")(input)?;
        Ok((input, Path::Up))
    }

    fn fixed(input: &str) -> IResult<&str, Path> {
        let (input, v) = take_while(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        Ok((input, Path::Fixed(v.to_string())))
    }

    fn lpar(input: &str) -> IResult<&str, &str> {
        delimited(space0, tag("("), space0)(input)
    }
    fn rpar(input: &str) -> IResult<&str, &str> {
        delimited(space0, tag(")"), space0)(input)
    }

    fn when(input: &str) -> IResult<&str, Path> {
        let (input, v) = take_while(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        let (input, c) = delimited(tag("["), take_while1(|c| c != ']'), tag("]"))(input)?;
        let c = if c == "*" {
            Select::All
        } else {
            Select::Index(nom::character::complete::u64(c)?.1 as usize)
        };
        Ok((input, Path::When(v.to_string(), c)))
    }

    pub fn path(input: &str) -> IResult<&str, Vec<Path>> {
        separated_list1(tag("/"), alt((up, when, fixed)))(input)
    }

    pub fn quantity(input: &str) -> IResult<&str, Quantity> {
        let (input, f) = float(input)?;
        let (input, _) = space0(input)?;
        let (input, u) = take_while(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        let unit = if u.is_empty() {
            None
        } else {
            Some(u.to_string())
        };
        Ok((
            input,
            Quantity {
                value: f as f64,
                unit,
            },
        ))
    }

    pub fn expr(input: &str) -> IResult<&str, Expr> {
        add(input)
    }

    fn lit(input: &str) -> IResult<&str, Expr> {
        // Exclude some values that are not allowed in NML
        if input.starts_with("inf") || input.starts_with("nan") || input.starts_with('+') {
            fail::<_, &str, _>(input)?;
        }
        let (input, f) = float(input)?;
        Ok((input, Expr::F64(f as f64)))
    }

    fn var(input: &str) -> IResult<&str, Expr> {
        let (input, v) = take_while1(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        Ok((input, Expr::Var(v.to_string())))
    }

    fn parenthised(input: &str) -> IResult<&str, Expr> {
        delimited(lpar, expr, rpar)(input)
    }

    fn exp(input: &str) -> IResult<&str, Expr> {
        let (input, e) = preceded(tag("exp"), parenthised)(input)?;
        Ok((input, Expr::Exp(Box::new(e))))
    }

    fn log(input: &str) -> IResult<&str, Expr> {
        let (input, e) = preceded(tag("log"), parenthised)(input)?;
        Ok((input, Expr::Log(Box::new(e))))
    }

    fn sqrt(input: &str) -> IResult<&str, Expr> {
        let (input, e) = preceded(tag("sqrt"), parenthised)(input)?;
        Ok((input, Expr::Sqrt(Box::new(e))))
    }

    fn fun(input: &str) -> IResult<&str, Expr> {
        let (input, f) = take_while1(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        let (input, e) = parenthised(input)?;
        Ok((input, Expr::Fun(f.to_string(), Box::new(e))))
    }

    fn atom(input: &str) -> IResult<&str, Expr> {
        let (input, sign) = opt(delimited(space0, tag("-"), space0))(input)?;
        let (input, result) = delimited(
            space0,
            alt((parenthised, exp, log, sqrt, fun, lit, var)),
            space0,
        )(input)?;
        if sign.is_some() {
            Ok((input, Expr::Mul(vec![Expr::F64(-1.0), result])))
        } else {
            Ok((input, result))
        }
    }

    fn pow(input: &str) -> IResult<&str, Expr> {
        let (input, sum) = separated_list1(delimited(space0, tag("^"), space0), atom)(input)?;
        if sum.len() == 1 {
            Ok((input, sum.last().unwrap().clone()))
        } else {
            Ok((input, Expr::Pow(sum)))
        }
    }

    fn mul(input: &str) -> IResult<&str, Expr> {
        let (input, init) = pow(input)?;
        let (input, sum) = fold_many0(
            pair(delimited(space0, one_of("*/"), space0), pow),
            || vec![init.clone()],
            |mut acc: Vec<_>, (o, ref mut x)| {
                if '/' == o {
                    acc.push(Expr::Pow(vec![x.clone(), Expr::F64(-1.0)]));
                } else {
                    acc.push(x.clone());
                }
                acc
            },
        )(input)?;
        if sum.len() == 1 {
            Ok((input, sum.last().unwrap().clone()))
        } else {
            Ok((input, Expr::Mul(sum)))
        }
    }

    fn add(input: &str) -> IResult<&str, Expr> {
        let (input, init) = mul(input)?;
        let (input, sum) = fold_many0(
            pair(delimited(space0, one_of("+-"), space0), mul),
            || vec![init.clone()],
            |mut acc: Vec<_>, (o, ref mut x)| {
                if '-' == o {
                    acc.push(Expr::Mul(vec![Expr::F64(-1.0), x.clone()]));
                } else {
                    acc.push(x.clone());
                }
                acc
            },
        )(input)?;
        if sum.len() == 1 {
            Ok((input, sum.last().unwrap().clone()))
        } else {
            Ok((input, Expr::Add(sum)))
        }
    }

    fn op(input: &str) -> IResult<&str, Boolean> {
        let (input, (_, l, _, _, o, _, _, r, _)) = tuple((
            lpar,
            boolean,
            rpar,
            tag("."),
            alpha1,
            tag("."),
            lpar,
            boolean,
            rpar,
        ))(input)?;
        let op = match o {
            "and" => Op::And,
            "or" => Op::Or,
            x => panic!("Unknown boolean op: {x}"),
        };
        Ok((input, Boolean::Op(op, Box::new(l), Box::new(r))))
    }

    fn cmp(input: &str) -> IResult<&str, Boolean> {
        let (input, (l, _, _, o, _, _, r)) =
            tuple((expr, space0, tag("."), alpha1, tag("."), space0, expr))(input)?;
        let op = match o {
            "neq" => Cmp::Ne,
            "eq" => Cmp::Eq,
            "lt" => Cmp::Lt,
            "gt" => Cmp::Gt,
            "leq" => Cmp::Le,
            "geq" => Cmp::Ge,
            x => panic!("Unknown compare operator: {x}"),
        };
        Ok((input, Boolean::Cmp(op, Box::new(l), Box::new(r))))
    }

    pub fn boolean(input: &str) -> IResult<&str, Boolean> {
        alt((op, cmp))(input)
    }
}

fn simplify_pow(es: &[Expr]) -> Expr {
    let mut result = es.iter().map(|e| e.simplify()).collect::<Vec<_>>();
    // If there is a zero in the chain of pow's, we can simplify:
    let z = result.iter().position(|e| {
        if let Expr::F64(x) = *e {
            x.abs() < f64::EPSILON
        } else {
            false
        }
    });
    if let Some(ix) = z {
        if ix >= 1 {
            // a) It's somewhere in the middle a^..^b^c^0^d^..
            //    so we can replace
            //      0^d^..          => 0
            //      c^0             => 1
            //      a^..^b^c^0^d^.. => a^..^b.
            //    If we remove all terms (a^0), it's going to caught below.
            result.truncate(ix - 1);
        } else {
            // b) It's the first term, we can just drop all other terms.
            result = vec![Expr::F64(0.0)];
        }
    }
    // If there is a one in the chain of pow's, we can simplify:
    let o = result.iter().position(|e| {
        if let Expr::F64(x) = *e {
            (x - 1.0).abs() < f64::EPSILON
        } else {
            false
        }
    });
    if let Some(ix) = o {
        // a^..^b^1^c^.. => a^..^b
        result.truncate(ix);
    }
    // Otherwise try to fold up constants
    match &result[..] {
        [] => Expr::F64(1.0),
        [e] => e.clone(),
        // [x, Expr::F64(v)] if *v < 0.0 && v.fract() == 0.0 => Expr::Pow(vec![Expr::Mul(vec![x.clone(); (-*v) as usize]), Expr::F64(-1.0)]),
        [xs @ .., Expr::F64(x), Expr::F64(y)] => {
            let mut res = xs.to_vec();
            res.push(Expr::F64(x.powf(*y)));
            Expr::Pow(res)
        }
        _ => Expr::Pow(result),
    }
}

fn simplify_mul(es: &[Expr]) -> Expr {
    let mut result = Vec::new();
    let mut lit = 1.0;
    let mut todo = es.to_vec();
    while let Some(e) = todo.pop() {
        match e.simplify() {
            Expr::F64(z) => lit *= z,
            Expr::Mul(zs) => todo.extend(zs),
            e @ Expr::Pow(_) => result.push(e),
            e => result.push(Expr::Pow(vec![e, Expr::F64(1.0)])),
        }
    }
    if lit == 0.0 {
        return Expr::F64(0.0);
    }
    result.sort_by(|a, b| a.partial_cmp(b).unwrap());
    // Now we have these terms left
    // * at most one literal
    // * ex^k
    // * ex
    while result.len() > 1 {
        let old = result.clone();
        for ix in 0..result.len() - 1 {
            let (ab, ae) = match &result[ix] {
                Expr::Pow(ms) => {
                    if ms.len() != 2 {
                        continue;
                    }
                    (ms[0].clone(), ms[1].clone())
                }
                a => (a.clone(), Expr::F64(1.0)),
            };
            let (bb, be) = match &result[ix + 1] {
                Expr::Pow(ms) => {
                    if ms.len() != 2 {
                        continue;
                    }
                    (ms[0].clone(), ms[1].clone())
                }
                a => (a.clone(), Expr::F64(1.0)),
            };
            if ab != bb {
                continue;
            }
            // Here, ab == bb, thus we can contract ab^ae * ab^be = ab^(ae + be)
            let res = Expr::Pow(vec![ab, Expr::Add(vec![ae, be]).simplify()]);
            // Now, we can remove the two contracted terms and replace them
            result.remove(ix);
            result.remove(ix);
            result.insert(ix, res);
            break;
        }
        if result == old {
            break;
        }
    }
    for ex in result.iter_mut() {
        if let Expr::Pow(es) = ex {
            if es.len() == 2 && es[1] == Expr::F64(1.0) {
                *ex = es[0].clone();
            }
        }
    }
    result.sort_by(|a, b| a.partial_cmp(b).unwrap());
    if (lit - 1.0).abs() > f64::EPSILON {
        result.insert(0, Expr::F64(lit));
    }
    match &result[..] {
        [] => Expr::F64(1.0),
        [x] => x.clone(),
        _ => Expr::Mul(result),
    }
}

fn simplify_add(es: &[Expr]) -> Expr {
    let mut todo = es.to_vec();
    let mut result = Vec::new();
    let mut lit = 0.0;
    while let Some(e) = todo.pop() {
        match e.simplify() {
            Expr::F64(z) => lit += z,
            Expr::Add(zs) => todo.extend(zs),
            k => result.push(k),
        }
    }
    if lit != 0.0 {
        result.push(Expr::F64(lit));
    }
    result.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let mut ix = 0;
    while ix < result.len() {
        let (mut f, mut ps) = match &result[ix] {
            v @ Expr::Var(_) => (1.0, vec![v.clone()]),
            Expr::Mul(ps) => {
                if let Expr::F64(f) = ps[0] {
                    (f, ps[1..].to_vec())
                } else {
                    (1.0, ps.clone())
                }
            }
            _ => {
                ix += 1;
                continue;
            }
        };
        let mut rem = Vec::new();
        for (iy, item) in result.iter().enumerate().skip(ix + 1) {
            let (g, qs) = match item {
                v @ Expr::Var(_) => (1.0, vec![v.clone()]),
                Expr::Mul(ps) => {
                    if let Expr::F64(f) = ps[0] {
                        (f, ps[1..].to_vec())
                    } else {
                        (1.0, ps.clone())
                    }
                }
                _ => continue,
            };
            if ps == qs {
                f += g;
                rem.push(iy);
            }
        }
        for iy in &rem {
            result.remove(*iy);
        }
        ps.insert(0, Expr::F64(f));
        result[ix] = simplify_mul(&ps);
        ix += 1;
    }
    match &result[..] {
        [] => Expr::F64(0.0),
        [x] => x.clone(),
        _ => Expr::Add(result),
    }
}

fn simplify_exp(es: &Expr) -> Expr {
    let xs = es.simplify();
    if let Expr::F64(x) = xs {
        Expr::F64(x.exp())
    } else {
        Expr::Exp(Box::new(xs))
    }
}

fn simplify_log(es: &Expr) -> Expr {
    let xs = es.simplify();
    if let Expr::F64(x) = xs {
        Expr::F64(x.ln())
    } else {
        Expr::Log(Box::new(xs))
    }
}

fn simplify_sqrt(es: &Expr) -> Expr {
    let xs = es.simplify();
    if let Expr::F64(x) = xs {
        Expr::F64(x.sqrt())
    } else {
        Expr::Sqrt(Box::new(xs))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_parse() {
        assert_eq!(
            Quantity::parse("10 mV").unwrap(),
            Quantity {
                value: 10.0,
                unit: Some(String::from("mV"))
            }
        );
        assert_eq!(Expr::parse(" 1.0  ").unwrap(), Expr::F64(1.0));
        assert_eq!(Expr::parse("  1+2 + 3 +3  *4 ").unwrap(), Expr::F64(18.0));
        assert_eq!(
            Expr::parse("log(42)").unwrap(),
            Expr::F64(3.7376696182833684)
        );
        assert_eq!(Expr::parse("sqrt(16.0)").unwrap(), Expr::F64(4.00));
    }

    #[test]
    fn test_parse_fail() {
        assert!(matches!(Quantity::parse("mV 10"), Err(Error::Parse { .. })));
        assert!(matches!(
            Expr::parse(" 10*").unwrap_err(),
            Error::Parse { .. }
        ));
    }

    #[test]
    fn test_add() {
        assert_eq!(Expr::parse("5*x - (2+3)*x").unwrap(), Expr::F64(0.0));
        assert_eq!(Expr::parse("2*x -x").unwrap(), Expr::Var(String::from("x")));
        assert_eq!(
            Expr::parse("3*x -2*x").unwrap(),
            Expr::Var(String::from("x"))
        );
        assert_eq!(Expr::parse("x -x").unwrap(), Expr::F64(0.0));
        assert_eq!(
            Expr::parse("x + y -x").unwrap(),
            Expr::Var(String::from("y"))
        );
    }

    #[test]
    fn test_mul() {
        assert_eq!(Expr::parse("1 * -1").unwrap(), Expr::F64(-1.0));
        assert_eq!(Expr::parse("1 * -  1").unwrap(), Expr::F64(-1.0));
        assert_eq!(Expr::parse("- 1 * -  1").unwrap(), Expr::F64(1.0));
        assert_eq!(
            Expr::parse("2.0 * x").unwrap(),
            Expr::Mul(vec![Expr::F64(2.0), Expr::Var("x".to_string())])
        );
        assert_eq!(Expr::parse("1.0 * x").unwrap(), Expr::Var("x".to_string()));
        assert_eq!(
            Expr::parse("z*y").unwrap(),
            Expr::Mul(vec![
                Expr::Var(String::from("y")),
                Expr::Var(String::from("z")),
            ])
        );
        assert_eq!(Expr::parse("a / a").unwrap(), Expr::F64(1.0));
    }

    #[test]
    fn test_pow_contraction() {
        assert_eq!(Expr::parse("a^-2 * a ^2").unwrap(), Expr::F64(1.0));
        assert_eq!(
            Expr::parse("a^-2 * k * a ^2").unwrap(),
            Expr::Var("k".to_string())
        );
        assert_eq!(
            Expr::parse("a * a^-2 * k * a ").unwrap(),
            Expr::Var("k".to_string())
        );
    }

    #[test]
    fn test_pow() {
        assert_eq!(
            Expr::parse("2^3^4").unwrap(),
            Expr::F64(2417851639229258349412352.0)
        );
        assert_eq!(Expr::parse("(2^3)^4").unwrap(), Expr::F64(4096.0));
        assert_eq!(Expr::parse("1^2^3").unwrap(), Expr::F64(1.0));
        assert_eq!(Expr::parse("1^2^3").unwrap(), Expr::F64(1.0));
        assert_eq!(Expr::parse("42^2^0^23").unwrap(), Expr::F64(42.0));
        assert_eq!(Expr::parse("42^2^0^23").unwrap(), Expr::F64(42.0));
        assert_eq!(Expr::parse("4^2^1^23").unwrap(), Expr::F64(16.0));
        assert_eq!(
            Expr::parse("x^2.5").unwrap(),
            Expr::Pow(vec![Expr::Var("x".to_string()), Expr::F64(2.5)])
        );
        assert_eq!(
            Expr::parse("x^2^5").unwrap(),
            Expr::Pow(vec![Expr::Var("x".to_string()), Expr::F64(32.0)])
        );
        assert_eq!(
            Expr::parse("1/x").unwrap(),
            Expr::Pow(vec![Expr::Var("x".to_string()), Expr::F64(-1.0)])
        );
    }

    #[test]
    fn test_exp() {
        assert_eq!(Expr::parse("exp (0   )").unwrap(), Expr::F64(1.0));
        assert_eq!(Expr::parse("  exp (  0   )  ").unwrap(), Expr::F64(1.0));
        assert_eq!(Expr::parse("exp(0)").unwrap(), Expr::F64(1.0));
        assert_eq!(Expr::parse("exp(0*x)").unwrap(), Expr::F64(1.0));
        assert_eq!(Expr::parse("exp(0 + 0*x)").unwrap(), Expr::F64(1.0));
        assert_eq!(Expr::parse("exp(1 - 1)").unwrap(), Expr::F64(1.0));
    }

    #[test]
    fn test_path() {
        use Path::*;
        let m = Match::parse("a_b/c[*]").unwrap();
        assert_eq!(
            m,
            Match(vec![
                Fixed(String::from("a_b")),
                When(String::from("c"), Select::All)
            ])
        );

        let ex = vec![
            String::from("a_b/c/foo"),
            String::from("a_b/c/foo_bar"),
            String::from(""),
            String::from("foo"),
        ];

        assert_eq!(
            m.on_path(&ex),
            vec![String::from("a_b/c/foo"), String::from("a_b/c/foo_bar"),]
        );
    }

    #[test]
    fn test_bool() {
        assert_eq!(Boolean::parse("23 .gt. 42").unwrap(), Boolean::Lit(false));
        assert_eq!(Boolean::parse("23 .lt. 42").unwrap(), Boolean::Lit(true));
        assert_eq!(Boolean::parse("x .eq. x").unwrap(), Boolean::Lit(true));
        assert_eq!(Boolean::parse("(x .gt. y) .and. (x .lt. z)").unwrap(),
                   Boolean::Op(Op::And,
                               Box::new(Boolean::parse("x .gt. y").unwrap()),
                               Box::new(Boolean::parse("x .lt. z").unwrap())));
        assert_eq!(Boolean::parse("(x .gt. y) .and. (x .lt. y)").unwrap(),
                   Boolean::Lit(false));
        assert_eq!(Boolean::parse("(x .gt. y) .or. (x .lt. y)").unwrap(),
                   Boolean::parse("x .neq. y").unwrap());
        assert_eq!(Boolean::parse("(x .neq. y) .or. (x .eq. y)").unwrap(),
                   Boolean::Lit(true));
        assert_eq!(Boolean::parse("(x .neq. y) .and. (x .eq. y)").unwrap(),
                   Boolean::Lit(false));
    }

}
