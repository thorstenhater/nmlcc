use std::collections::HashMap as Map;

type Result<T> = std::result::Result<T, String>;

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
            Err(format!("Could not parse {}", input))
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum BoolOp { AD, OR, EQ, NE, GE, LE, LT, GT, }

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
    // Boolean
    Bol(BoolOp, Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn parse(input: &str) -> Result<Self> {
        if let Ok((_, result)) = parse::add(input) {
            Ok(result.simplify())
        } else {
            Err(format!("Could not parse {}", input))
        }
    }

    pub fn parse_bool(input: &str) -> Result<Expr> {
        if let Ok((_, result)) = parse::boolean(input) {
            Ok(result.simplify())
        } else {
            Err(format!("Could not parse {}", input))
        }
    }

    pub fn print_to_string(&self) -> String {
        match self {
            Expr::F64(x) => format!("{}", x),
            Expr::Var(x) => x.to_string(),
            Expr::Bol(o, l, r) => {
                let op = match o {
                    BoolOp::AD => "&&",
                    BoolOp::OR => "||",
                    BoolOp::EQ => "==",
                    BoolOp::NE => "!=",
                    BoolOp::GE => ">=",
                    BoolOp::LE => "<=",
                    BoolOp::GT => ">",
                    BoolOp::LT => "<",
                };
                format!("{} {} {}", l.print_to_string(), op, r.print_to_string())
            }
            Expr::Exp(x)  => format!("exp({})", x.print_to_string()),
            Expr::Add(xs) => xs.iter().map(|x| x.print_to_string()).collect::<Vec<_>>().join(" + "),
            Expr::Mul(xs) => xs.iter()
                               .map(|x|
                                    if let Expr::Add(_) = x {
                                        format!("({})", x.print_to_string())
                                    } else {
                                        x.print_to_string()
                                    }).collect::<Vec<_>>().join(" * "),
            Expr::Pow(xs) => xs.iter().map(|x|
                                           match x {
                                               Expr::Add(_) | Expr::Mul(_) => format!("({})", x.print_to_string()),
                                               _ => x.print_to_string(),
                                    }).collect::<Vec<_>>().join("^"),
        }
    }

    pub fn simplify(&self) -> Self {
        match self {
            Expr::Pow(vs)      => simplify_pow(vs),
            Expr::Add(vs)      => simplify_add(vs),
            Expr::Mul(vs)      => simplify_mul(vs),
            Expr::Exp(vs)      => simplify_exp(vs),
            Expr::Bol(o, a, b) => Expr::Bol(o.clone(),
                                            Box::new(a.simplify()),
                                            Box::new(b.simplify())),
            e => e.clone(),
        }
    }

}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Path {
    Fixed(String),
    When(String, String),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Match(Vec<Path>);

impl Match {
    fn new() -> Self { Match(Vec::new()) }

    pub fn parse(input: &str) -> Result<Self> {
        if let Ok((_, result)) = parse::path(input) {
            Ok(Match(result))
        } else {
            Err(format!("Could not parse {}", input))
        }
    }

    pub fn add_prefix(&mut self, pfx: &[String]) {
        self.0 = pfx.iter()
                    .map(|p| Path::Fixed(p.to_string()))
                    .chain(self.0.iter().cloned())
                    .collect();
    }

    pub fn on_path(&self, exposures: &Map<String, String>) -> Vec<String> {
        let mut ms = Vec::new();
        for ex in exposures.keys() {
            let mut es = ex.split('_');
            let mut ok = true;
            for p in &self.0 {
                match p {
                    Path::Fixed(f) => {
                        ok &= if let Some(r) = es.next() {
                            r == f
                        } else {
                            false
                        };
                    }
                    Path::When(f, c) => {
                        assert!("*" == c);
                        ok &= if let Some(r) = es.next() {
                            r == f
                        } else {
                            false
                        };
                        ok &= es.next().is_some(); // TODO(TH): More elaborate matching
                    }
                }
            }
            if ok {
                ms.push(ex.to_string());
            }
        }
        ms
    }

}

pub fn map_leaves(expr: &Expr, f: &impl Fn(&Expr) -> Expr) -> Expr {
    match expr {
        Expr::Add(vs)      => Expr::Add(vs.iter().map(|v| map_leaves(v, f)).collect()),
        Expr::Mul(vs)      => Expr::Mul(vs.iter().map(|v| map_leaves(v, f)).collect()),
        Expr::Pow(vs)      => Expr::Pow(vs.iter().map(|v| map_leaves(v, f)).collect()),
        Expr::Exp(b)       => Expr::Exp(Box::new(map_leaves(&*b, f))),
        Expr::Bol(o, a, b) => Expr::Bol(o.clone(),
                                        Box::new(map_leaves(&*a, f)),
                                        Box::new(map_leaves(&*b, f))),
        e => f(e),
    }
}

pub fn fold_leaves<T>(expr: &Expr, acc: &mut T, f: &impl Fn(&Expr, &mut T)) {
    match expr {
        Expr::Add(vs)      => vs.iter().for_each(|v| fold_leaves(v, acc, f)),
        Expr::Mul(vs)      => vs.iter().for_each(|v| fold_leaves(v, acc, f)),
        Expr::Pow(vs)      => vs.iter().for_each(|v| fold_leaves(v, acc, f)),
        Expr::Exp(b)       => fold_leaves(&*b, acc, f),
        Expr::Bol(_, a, b) => {
            fold_leaves(&*a, acc, f);
            fold_leaves(&*b, acc, f);
        }
        e => f(e, acc),
    }
}

mod parse {
    use nom::{IResult,
              bytes:: complete::{tag, take_while, take_while1},
              character::is_alphanumeric,
              character::complete::{one_of, space0, alpha1},
              combinator::{fail, opt},
              sequence::{pair, delimited, preceded, tuple},
              branch::alt,
              multi::{fold_many0, separated_list1},
              number::complete::{float},};

    use super::{Quantity, Path, Expr, BoolOp};

    fn fixed(input: &str) -> IResult<&str, Path> {
        let (input, v) = take_while1(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        Ok((input, Path::Fixed(v.to_string())))
    }

    fn when(input: &str) -> IResult<&str, Path> {
        let (input, v) = take_while1(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        let (input, c) = delimited(tag("["),
                                   take_while1(|c| c != ']'),
                                   tag("]"))(input)?;
        Ok((input, Path::When(v.to_string(), c.to_string())))
    }

    pub fn path(input: &str) -> IResult<&str, Vec<Path>> {
        separated_list1(tag("/"),
                        alt((when, fixed)))(input)
    }


    pub fn quantity(input: &str) -> IResult<&str, Quantity> {
        let (input, f) = float(input)?;
        let (input, _) = space0(input)?;
        let (input, u) = take_while(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;

        let value = f as f64;
        let unit  = if u.is_empty() {
            None
        } else {
            Some(u.to_string())
        };

        Ok((input, Quantity { value, unit }))
    }

    pub fn expr(input: &str) -> IResult<&str, Expr> { add(input) }

    pub fn lit(input: &str) -> IResult<&str, Expr> {
        // TODO(TH): ugly AF
        if input.starts_with("inf") || input.starts_with("nan") || input.starts_with('+') {
            fail::<_,&str,_>(input)?;
        }
        let (input, f) = float(input)?;
        Ok((input, Expr::F64(f as f64)))
    }

    fn var(input: &str) -> IResult<&str, Expr> {
        let (input, v) = take_while1(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        Ok((input, Expr::Var(v.to_string())))
    }

    fn exp(input: &str) -> IResult<&str, Expr> {
        let (input, e) = preceded(tag("exp"),
                                  delimited(delimited(space0,
                                                      tag("("),
                                                      space0),
                                            expr,
                                            delimited(space0,
                                                      tag(")"),
                                                      space0)))(input)?;
        Ok((input, Expr::Exp(Box::new(e))))
    }

    fn atom(input: &str) -> IResult<&str, Expr> {
        let (input, sign) = opt(delimited(space0,
                                          tag("-"),
                                          space0))(input)?;
        let (input, result) = alt((delimited(delimited(space0,
                                                       tag("("),
                                                       space0),
                                             expr,
                                             delimited(space0,
                                                       tag(")"),
                                                       space0)),
                                   exp,
                                   lit,
                                   var,))(input)?;
        if sign.is_some() {
            Ok((input, Expr::Mul(vec![Expr::F64(-1.0), result])))
        } else {
            Ok((input, result))
        }
    }

    pub fn pow(input: &str) -> IResult<&str, Expr> {
        let (input, sum) = separated_list1(delimited(space0,
                                                     tag("^"),
                                                     space0),
                                           atom)(input)?;
        if sum.len() == 1 {
            Ok((input, sum.last().unwrap().clone()))
        } else {
            Ok((input, Expr::Pow(sum)))
        }
    }

    pub fn mul(input: &str) -> IResult<&str, Expr> {
        let (input, init) = pow(input)?;
        let (input, sum) = fold_many0(pair(delimited(space0,
                                                     one_of("*/"),
                                                     space0),
                                           pow),
                                      || vec![init.clone()],
                                      |mut acc: Vec<_>, (o, ref mut x)| {
                                          if '-' == o {
                                              acc.push(Expr::Pow(vec![x.clone(), Expr::F64(-1.0)]));
                                          } else {
                                              acc.push(x.clone());
                                          }
                                          acc
                                      })(input)?;
        if sum.len() == 1 {
            Ok((input, sum.last().unwrap().clone()))
        } else {
            Ok((input, Expr::Mul(sum)))
        }
    }

    pub fn add(input: &str) -> IResult<&str, Expr> {
        let (input, init) = mul(input)?;
        let (input, sum) = fold_many0(pair(delimited(space0,
                                                     one_of("+-"),
                                                     space0),
                                           mul),
                                      || vec![init.clone()],
                                      |mut acc: Vec<_>, (o, ref mut x)| {
                                          if '-' == o {
                                              acc.push(Expr::Mul(vec![Expr::F64(-1.0), x.clone()]));
                                          } else {
                                              acc.push(x.clone());
                                          }
                                          acc
                                      })(input)?;
        if sum.len() == 1 {
            Ok((input, sum.last().unwrap().clone()))
        } else {
            Ok((input, Expr::Add(sum)))
        }
    }

    pub fn boolean(input: &str) -> IResult<&str, Expr> {
        let (input, (l, o, r)) = tuple((alt((delimited(delimited(space0,
                                                                 tag("("),
                                                                 space0),
                                                       boolean,
                                                       delimited(space0,
                                                                 tag(")"),
                                                                 space0)),
                                             add)),
                                        delimited(space0,
                                                  delimited(tag("."),
                                                            alpha1,
                                                            tag(".")),
                                                  space0),
                                        alt((delimited(delimited(space0,
                                                                 tag("("),
                                                                 space0),
                                                       boolean,
                                                       delimited(space0,
                                                                 tag(")"),
                                                                 space0)),
                                             add))))(input)?;
        let op = match o {
            "neq" => BoolOp::NE,
            "eq"  => BoolOp::EQ,
            "lt"  => BoolOp::LT,
            "gt"  => BoolOp::GT,
            "leq" => BoolOp::LE,
            "geq" => BoolOp::GE,
            "and" => BoolOp::AD,
            "or"  => BoolOp::OR,
            x     => panic!("Unknown boolean op: {}", x),
        };
        Ok((input, Expr::Bol(op, Box::new(l), Box::new(r))))
    }
}

fn simplify_pow(es: &[Expr]) -> Expr {
    // TODO(TH) This could be better, actually simplify stuff here?
    let result = es.iter().map(|e| e.simplify()).collect::<Vec<_>>();
    match &result[..] {
        []  => Expr::F64(1.0),
        [e] => e.clone(),
        [_, Expr::F64(v)] if *v == 0.0 => Expr::F64(1.0),
        [x, Expr::F64(v)] if *v == 1.0 => x.clone(),
        [Expr::F64(x), Expr::F64(y)] => Expr::F64(x.powf(*y)),
        es => Expr::Pow(es.to_vec()),
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
            k => result.push(k),
        }
    }
    if lit == 0.0 { return Expr::F64(0.0); }
    if lit != 1.0 { result.push(Expr::F64(lit)); }
    result.sort_by(|a, b| a.partial_cmp(b).unwrap());
    match result.len() {
        0 => Expr::F64(1.0),
        1 => result.last().unwrap().clone(),
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
    if lit != 0.0 { result.push(Expr::F64(lit)); }
    result.sort_by(|a, b| a.partial_cmp(b).unwrap());
    match result.len() {
        0 => Expr::F64(0.0),
        1 => result.last().unwrap().clone(),
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
