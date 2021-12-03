use tracing::info;

/// LEMS ComponentType ('class') preprocessed
#[derive(Clone, Debug, Default)]
pub struct ComponentType {
    /// type name
    pub name: String,
    /// Base component name, if any
    pub base: Option<String>,
    /// One per entry with a type derived from base
    /// Given a -> A, we expect to find
    /// <a type="A">
    /// and 'a' is a single value of type 'A'
    pub child: Map<String, String>,
    /// Potentially many per entry with a type derived from base
    /// Given a -> A, we expect to find
    /// <A id="a0">
    /// <A id="a1">
    /// thus 'a' is a collection ['a0', 'a1']
    pub children: Map<String, String>,
    /// unique list of externally visible variables, must be linked to a type of variable
    /// NOTE: exposures cannot change name of variables, see answer here https://github.com/NeuroML/NeuroML2/issues/178
    pub exposures: Map<String, String>,
    /// variables, taken from the <Dynamics> block
    pub variables: Vec<Variable>,
    /// parameters
    pub parameters: Vec<String>,
    /// attributes (non-numerical parameters?)
    pub attributes: Vec<String>,
    /// constants
    pub constants: Map<String, Quantity>,
    /// events: on event assign `variable` <- `Expr`
    pub events: Vec<(String, Expr)>,
}

impl ComponentType {
    fn from_lems(ct: &lems::raw::ComponentType) ->  Result<Self> {
        let name = ct.name.to_string();
        let base = ct.extends.clone();
        let mut child = Map::new();
        let mut children = Map::new();
        let mut exposures = Map::new();
        let mut variables = Vec::new();
        let mut parameters = Vec::new();
        let mut attributes = Vec::new();
        let mut constants = Map::new();
        let mut events = Vec::new();

        for ix in &ct.body {
            use lems::raw::ComponentTypeBody::*;
            match ix {
                Child(c)     => { child.insert(c.name.to_string(), c.r#type.to_string()); }
                Children(c)  => { children.insert(c.name.to_string(), c.r#type.as_ref().unwrap().to_string()); }
                Parameter(p) => { parameters.push(p.name.to_string()); }
                Constant(c)  => { constants.insert(c.name.to_string(), Quantity::parse(&c.value)?); }
                Exposure(e)  => { exposures.insert(e.name.to_string(), e.dimension.to_string());  }
                Text(t)      => { attributes.push(t.name.to_string()); }
                Dynamics(d)  => { lems_dynamics(d, &mut variables, &mut events)?; },
                _ => {}
            }
        }
        Ok(Self { name, base, child, children, exposures, variables, constants, parameters, attributes, events })
    }
}

/// Helper: Process Dynamics part of ComponentType
fn lems_dynamics(dynamics: &lems::raw::Dynamics,
                 variables: &mut Vec<Variable>,
                 events: &mut Vec<(String, Expr)> ) -> Result<()> {
    use lems::raw::DynamicsBody::*;
    use lems::raw::ConditionalDerivedVariableBody::*;
    for b in &dynamics.body {
        match b {
            DerivedVariable(v) => {
                let kind = if let Some(s) = v.select.as_ref() {
                    let by = match v.reduce.as_deref() {
                        Some("add")      => SelectBy::Sum,
                        Some("multiply") => SelectBy::Product,
                        None             => SelectBy::Get,
                        Some(x)          => return Err(format!("Unknown reduction {}", x)),
                    };
                    VarKind::Select(by, Match::parse(s)?)
                } else if let Some(e) = v.value.as_ref() {
                    VarKind::Derived(Vec::new(), Some(Expr::parse(e)?))
                } else {
                    return Err("Illegal DerivedVariable".to_string());
                };
                variables.push(Variable::new(&v.name, &v.exposure, &v.dimension, &kind));
            }
            StateVariable(v) =>
                variables.push(Variable::new(&v.name, &v.exposure, &v.dimension, &VarKind::State(None, None))),
            ConditionalDerivedVariable(v) => {
                let mut cs = Vec::new();
                let mut df = None;
                for Case(c) in &v.body {
                    let e = Expr::parse(&c.value)?;
                    if let Some(b) = c.condition.as_ref() {
                        cs.push((Expr::parse_bool(b)?, e));
                    } else {
                        df = Some(e);
                    }
                }
                variables.push(Variable::new(&v.name, &v.exposure, &v.dimension, &VarKind::Derived(cs, df)));
            }
            OnStart(v) => {
                use lems::raw::OnStartBody::*;
                for StateAssignment(a) in &v.body {
                    let it = variables.iter_mut().find(|x| x.name == a.variable);
                    if let Some(Variable { kind: VarKind::State(ref mut i, _), ..}) = it {
                        *i = Some(Expr::parse(&a.value)?);
                    } else {
                        return Err(format!("Must be a StateVar: {}", a.variable));
                    }
                }
            }
            OnEvent(v) => {
                use lems::raw::OnEventBody::*;
                for b in &v.body {
                    match b {
                        StateAssignment(a) => {
                            let it = variables.iter().find(|x| x.name == a.variable);
                            if let Some(Variable { kind: VarKind::State(_, _), ..}) = it {
                                events.push((a.variable.to_string(), Expr::parse(&a.value)?));
                            } else {
                                return Err(format!("Must be a StateVar: {}", a.variable));
                            }
                        }
                        b => info!("Ignoring {:?}", b),
                    }
                }
            }
            TimeDerivative(v) => {
                let it = variables.iter_mut().find(|x| x.name == v.variable);
                if let Some(Variable { kind: VarKind::State(_, ref mut d), ..}) = it {
                    *d = Some(Expr::parse(&v.value)?);
                } else {
                    return Err(format!("Must be a StateVar: {}", v.variable));
                }
            }
            _ => {}
        }
    }
    Ok(())
}
