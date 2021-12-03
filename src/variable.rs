use crate::expr::{Expr, Match};

#[derive(Clone, Debug, PartialEq)]
pub enum SelectBy { Get, Sum, Product, }

#[derive(Clone, Debug, PartialEq)]
pub enum VarKind {
    /// State variable defined by 1st order ODE X(t=0), X'(t)
    State(Option<Expr>, Option<Expr>),
    /// cases=[condition => expression] default=expression
    Derived(Vec<(Expr, Expr)>, Option<Expr>),
    /// Select one or more fields from children. NOTE will be gone after collapsing
    Select(SelectBy, Match),
}

#[derive(Clone, Debug, PartialEq)]
/// Variable
pub struct Variable {
    pub name: String,
    /// if linked to an exposure. NOTE According to PG must be same as `name`
    pub exposure: Option<String>,
    /// link to a known dimension
    pub dimension: String,
    pub kind: VarKind,
}

impl Variable {
    pub fn new(n: &str, e: &Option<String>, d: &str, k: &VarKind) -> Self {
        Variable { name: n.to_string(), exposure: e.clone(), dimension: d.to_string(), kind: k.clone() }
    }
}
