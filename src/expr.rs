use crate::{error::{Result, Error,},};

fn parse_error<T: Into<String>>(what: T) -> Error { Error::Parse { what: what.into() } }

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
            Err(parse_error(format!("Could not parse {}", input)))
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
    pub fn map(&self, f: &impl Fn(&Expr) -> Expr) -> Expr {
        match self {
            Expr::Add(vs)      => Expr::Add(vs.iter().map(|v| v.map(f)).collect()),
            Expr::Mul(vs)      => Expr::Mul(vs.iter().map(|v| v.map(f)).collect()),
            Expr::Pow(vs)      => Expr::Pow(vs.iter().map(|v| v.map(f)).collect()),
            Expr::Exp(b)       => Expr::Exp(Box::new(b.map(f))),
            Expr::Bol(o, a, b) => Expr::Bol(o.clone(), Box::new(a.map(f)), Box::new(b.map(f))),
            e => f(e),
        }
    }

    pub fn fold<T>(&self, acc: &mut T, f: &impl Fn(&Expr, &mut T)) {
        match self {
            Expr::Add(vs)      => vs.iter().for_each(|v| v.fold(acc, f)),
            Expr::Mul(vs)      => vs.iter().for_each(|v| v.fold(acc, f)),
            Expr::Pow(vs)      => vs.iter().for_each(|v| v.fold(acc, f)),
            Expr::Exp(b)       => b.fold(acc, f),
            Expr::Bol(_, a, b) => { a.fold(acc, f); b.fold(acc, f); }
            e => f(e, acc),
        }
    }

    pub fn parse(input: &str) -> Result<Self> {
        if let Ok((_, result)) = parse::add(input) {
            Ok(result.simplify())
        } else {
            Err(parse_error(format!("Could not parse {}", input)))
        }
    }

    pub fn parse_bool(input: &str) -> Result<Expr> {
        if let Ok((_, result)) = parse::boolean(input) {
            Ok(result.simplify())
        } else {
            Err(parse_error(format!("Could not parse {}", input)))
        }
    }

    pub fn print_to_string(&self) -> String {
        match &self {
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
            Expr::Mul(xs) => xs.iter().map(|x|
                                           if let Expr::Add(_) = x {
                                               format!("({})", x.print_to_string())
                                           } else {
                                               x.print_to_string()
                                           }).collect::<Vec<_>>().join(" * "),
            Expr::Pow(xs) => xs.iter().map(|x|
                                           match x {
                                               Expr::Add(_) | Expr::Mul(_) => format!("({})", x.print_to_string()),
                                               _ => x.print_to_string(),
                                           }).collect::<Vec<_>>().join("^")
        }
    }

    pub fn simplify(&self) -> Self {
        let mut done = false;
        let mut old = self.clone();
        while !done {
            let new = match &old {
                Expr::Pow(vs)      => simplify_pow(vs),
                Expr::Add(vs)      => simplify_add(vs),
                Expr::Mul(vs)      => simplify_mul(vs),
                Expr::Exp(vs)      => simplify_exp(vs),
                Expr::Bol(o, a, b) => Expr::Bol(o.clone(), Box::new(a.simplify()), Box::new(b.simplify())),
                e => e.clone(),
            };
            done = old == new;
            old = new
        }
        old
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum Path {
    Fixed(String),
    When(String, String),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Match(pub Vec<Path>);

impl Match {
    pub fn parse(input: &str) -> Result<Self> {
        if let Ok((_, result)) = parse::path(input) {
            Ok(Match(result))
        } else {
            Err(parse_error(format!("Could not parse {}", input)))
        }
    }

    pub fn add_prefix(&self, pfx: &[String]) -> Self {
        Self(pfx.iter()
                .map(|p| Path::Fixed(p.to_string()))
                .chain(self.0.iter().cloned())
                .collect())
    }

    pub fn on_path(&self, exposures: &[String]) -> Vec<String> {
        let mut ms = Vec::new();
        for ex in exposures {
            let mut es = ex.split('_');
            let mut ok = true;
            for p in &self.0 {
                match p {
                    Path::Fixed(f) => ok &= if let Some(r) = es.next() { r == f } else { false },
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
            if ok { ms.push(ex.to_string()); }
        }
        ms
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
        let (input, v) = take_while(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        Ok((input, Path::Fixed(v.to_string())))
    }

    fn when(input: &str) -> IResult<&str, Path> {
        let (input, v) = take_while(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        let (input, c) = delimited(tag("["),
                                   take_while1(|c| c != ']'),
                                   tag("]"))(input)?;
        Ok((input, Path::When(v.to_string(), c.to_string())))
    }

    pub fn path(input: &str) -> IResult<&str, Vec<Path>> { separated_list1(tag("/"), alt((when, fixed)))(input) }

    pub fn quantity(input: &str) -> IResult<&str, Quantity> {
        let (input, f) = float(input)?;
        let (input, _) = space0(input)?;
        let (input, u) = take_while(|c| is_alphanumeric(c as u8) || '_' == c)(input)?;
        let unit  = if u.is_empty() { None } else { Some(u.to_string()) };
        Ok((input, Quantity { value: f as f64, unit }))
    }

    pub fn expr(input: &str) -> IResult<&str, Expr> { add(input) }

    pub fn lit(input: &str) -> IResult<&str, Expr> {
        // Exclude some values that are not allowed in NML
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

    fn parenthised(input: &str) -> IResult<&str, Expr> {
        delimited(delimited(space0, tag("("), space0),
                  expr,
                  delimited(space0, tag(")"), space0))(input)
    }

    fn exp(input: &str) -> IResult<&str, Expr> {
        let (input, e) = preceded(tag("exp"), parenthised)(input)?;
        Ok((input, Expr::Exp(Box::new(e))))
    }

    fn atom(input: &str) -> IResult<&str, Expr> {
        let (input, sign) = opt(delimited(space0, tag("-"), space0))(input)?;
        let (input, result) = delimited(space0,
                                        alt((parenthised,
                                             exp,
                                             lit,
                                             var,)), space0)(input)?;
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
                                          if '/' == o {
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
    let mut result = es.iter().map(|e| e.simplify()).collect::<Vec<_>>();
    // If there is a zero in the chain of pow's, we can simplify:
    let z = result.iter().position(|e| if let Expr::F64(x) = *e { x.abs() < f64::EPSILON } else { false });
    if let Some(ix) = z {
        if ix >= 1 {
            // a) It's somewhere in the middle a^..^b^c^0^d^..
            //    so we can replace
            //      0^d^..          => 0
            //      c^0             => 1
            //      a^..^b^c^0^d^.. => a^..^b.
            //    If we remove all terms (a^0), it's going to caught below.
            result.truncate(ix-1);
        } else {
            // b) It's the first term, we can just drop all other terms.
            result = vec![Expr::F64(0.0)];
        }
    }
    // If there is a one in the chain of pow's, we can simplify:
    let o = result.iter().position(|e| if let Expr::F64(x) = *e { (x-1.0).abs() < f64::EPSILON } else { false });
    if let Some(ix) = o {
        // a^..^b^1^c^.. => a^..^b
        result.truncate(ix);
    }
    // Otherwise try to fold up constants
    match &result[..] {
        []  => Expr::F64(1.0),
        [e] => e.clone(),
        // TODO These are only a win if `v` is a simple variable. We should however enable this optmisation
        // since GCC/Clang only do it for `e = 1, 2`` OR if --fast-math is engaged. If done, it seems to be
        // a win up to at least `e=1000`
        // [x, Expr::F64(v)] if *v > 0.0 && v.fract() == 0.0 => Expr::Mul(vec![x.clone(); *v as usize]),
        // [x, Expr::F64(v)] if *v < 0.0 && v.fract() == 0.0 => Expr::Pow(vec![Expr::Mul(vec![x.clone(); (-*v) as usize]), Expr::F64(-1.0)]),
        [xs @ .., Expr::F64(x), Expr::F64(y)] => {
            let mut res = xs.to_vec();
            res.push(Expr::F64(x.powf(*y)));
            Expr::Pow(res)
        }
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
    if (lit - 1.0).abs() > f64::EPSILON { result.push(Expr::F64(lit)); }
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
    'a: loop {
        for ix in 0..result.len() {
            if let Expr::Mul(xs) = &result[ix] {
                let (x, rs) = if let Some(Expr::F64(x)) = xs.first() {
                    (*x, &xs[1..])
                } else {
                    (1.0, &xs[..])
                };
                for iy in ix+1..result.len() {
                    if let Expr::Mul(ys) = &result[iy] {
                        let (y, ts) = if let Some(Expr::F64(y)) = ys.first() {
                            (*y, &ys[1..])
                        } else {
                            (1.0, &ys[..])
                        };
                        if rs == ts {
                            let mut ks = ts.to_vec();
                            ks.insert(0, Expr::F64(x + y));
                            result.remove(iy);
                            result.remove(ix);
                            result.insert(0, Expr::Mul(ks));
                            continue 'a;
                        }
                    }
                }
            }
        }
        break;
    }
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(Expr::parse(" 1.0  "), Ok(Expr::F64(1.0)));
        assert_eq!(Expr::parse("  1+2 + 3 +3  *4 "), Ok(Expr::F64(18.0)));
    }

    #[test]
    fn test_add() {
        assert_eq!(Expr::parse("5*x - (2+3)*x"), Ok(Expr::F64(0.0)));
        assert_eq!(Expr::parse("2*x -x"), Ok(Expr::Var(String::from("x"))));
        assert_eq!(Expr::parse("x -2*x"), Ok(Expr::Mul(vec![Expr::F64(-1.0), Expr::Var(String::from("x"))])));
        assert_eq!(Expr::parse("x -x"), Ok(Expr::F64(0.0)));
        assert_eq!(Expr::parse("x + y -x"), Ok(Expr::Var(String::from("y"))));
    }

    #[test]
    fn test_mul() {
        assert_eq!(Expr::parse("1 * -1"), Ok(Expr::F64(-1.0)));
        assert_eq!(Expr::parse("1 * -  1"), Ok(Expr::F64(-1.0)));
        assert_eq!(Expr::parse("- 1 * -  1"), Ok(Expr::F64(1.0)));
        assert_eq!(Expr::parse("2.0 * x"), Ok(Expr::Mul(vec![Expr::F64(2.0), Expr::Var("x".to_string())])));
        assert_eq!(Expr::parse("1.0 * x"), Ok(Expr::Var("x".to_string())));
        assert_eq!(Expr::parse("z*y"), Ok(Expr::Mul(vec![Expr::Var(String::from("y")),
                                                         Expr::Var(String::from("z")),])));
    }

    #[test]
    fn test_pow() {
        assert_eq!(Expr::parse("2^3^4"), Ok(Expr::F64(2417851639229258349412352.0)));
        assert_eq!(Expr::parse("(2^3)^4"), Ok(Expr::F64(4096.0)));
        assert_eq!(Expr::parse("1^2^3"), Ok(Expr::F64(1.0)));
        assert_eq!(Expr::parse("1^2^3"), Ok(Expr::F64(1.0)));
        assert_eq!(Expr::parse("42^2^0^23"), Ok(Expr::F64(42.0)));
        assert_eq!(Expr::parse("42^2^0^23"), Ok(Expr::F64(42.0)));
        assert_eq!(Expr::parse("4^2^1^23"), Ok(Expr::F64(16.0)));
        assert_eq!(Expr::parse("x^2.5"), Ok(Expr::Pow(vec![Expr::Var("x".to_string()), Expr::F64(2.5)])));
        assert_eq!(Expr::parse("x^2^5"), Ok(Expr::Pow(vec![Expr::Var("x".to_string()), Expr::F64(32.0)])));
        assert_eq!(Expr::parse("1/x"), Ok(Expr::Pow(vec![Expr::Var("x".to_string()), Expr::F64(-1.0)])));
        // TODO See above, we would like to do this, but need the basis to be a simple expression
        // assert_eq!(Expr::parse("x^2.0"), Ok(Expr::Mul(vec![Expr::Var("x".to_string()), Expr::Var("x".to_string())])));
    }

    #[test]
    fn test_exp() {
        assert_eq!(Expr::parse("exp (0   )"), Ok(Expr::F64(1.0)));
        assert_eq!(Expr::parse("  exp (  0   )  "), Ok(Expr::F64(1.0)));
        assert_eq!(Expr::parse("exp(0)"), Ok(Expr::F64(1.0)));
        assert_eq!(Expr::parse("exp(0*x)"), Ok(Expr::F64(1.0)));
        assert_eq!(Expr::parse("exp(0 + 0*x)"), Ok(Expr::F64(1.0)));
        assert_eq!(Expr::parse("exp(1 - 1)"), Ok(Expr::F64(1.0)));
    }

    #[test]
    fn test_path() {
        use Path::*;
        let m = Match::parse("a/b/c[*]");
        assert_eq!(m,
                   Ok(Match(vec![Fixed(String::from("a")), Fixed(String::from("b")), When(String::from("c"), String::from("*"))])));

        let ex = vec![String::from("a_b_c_foo"),
                      String::from("a_b_c_bar"),
                      String::from(""),
                      String::from("a__b"),
                      String::from("foo"), ];
        let m = m.unwrap();
        assert_eq!(m.on_path(&ex), vec![String::from("a_b_c_foo"),
                                        String::from("a_b_c_bar"),]);
    }
}
