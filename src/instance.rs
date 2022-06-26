use roxmltree::Node;
use tracing::{info, trace, warn};

use crate::{
    error::{nml2_error, Result},
    expr::{Boolean, Expr, Match, Quantity, Select},
    lems,
    variable::{SelectBy, VarKind, Variable},
    Map, Set,
};

/// Kinetic scheme from components
/// This does not hold any real data, just links and prefixes. The surrounding
/// component needs to held a set of components with prefix `node` each exposing
/// a state variable with name `state`. These are states (NB. overloaded term)
/// of the scheme. Similarly, we need to find components prefixed with `edge`
/// each exposing `forward` and `backward` transition rates.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Kinetic {
    /// Identifier
    pub name: String,
    /// State node prefix in surrounding ComponentType
    pub node: Match,
    /// Transition edge prefix in surrounding ComponentType
    pub edge: Match,
    /// State variable name exposed by all nodes
    pub state: String,
    /// Backward rates exposed by all edges
    pub rfwd: String,
    /// Backward rates exposed by all edges
    pub rbwd: String,
}

impl Kinetic {
    pub fn new(ks: &lems::raw::KineticScheme) -> Result<Self> {
        Ok(Kinetic {
            name: ks.name.clone(),
            node: Match::parse(&format!("{}[*]", ks.nodes))?,
            edge: Match::parse(&format!("{}[*]", ks.edges))?,
            state: ks.stateVariable.to_string(),
            rfwd: ks.forwardRate.to_string(),
            rbwd: ks.reverseRate.to_string(),
        })
    }

    fn add_prefix(&self, ctx: &Context) -> Self {
        let pfx = ctx.keys();
        let name = ctx.add_prefix(&self.name);
        let node = self.node.add_prefix(&pfx);
        let edge = self.edge.add_prefix(&pfx);
        let state = self.state.to_string();
        let rfwd = self.rfwd.to_string();
        let rbwd = self.rbwd.to_string();
        Kinetic {
            name,
            node,
            edge,
            state,
            rfwd,
            rbwd,
        }
    }
}

/// Instantiated NML2 ComponentType
#[derive(Debug, Clone)]
pub struct Instance {
    pub component_type: ComponentType,
    pub child: Map<String, Instance>,
    pub children: Map<String, Vec<Instance>>,
    pub id: Option<String>,
    pub parameters: Map<String, Quantity>,
    pub attributes: Map<String, String>,
}

impl Instance {
    pub fn new(lems: &lems::file::LemsFile, xml: &Node) -> Result<Self> {
        let node = xml
            .attribute("type")
            .unwrap_or_else(|| xml.tag_name().name());
        let component_type = lems.compose_component_type(node)?;

        let mut attributes = Map::new();
        let mut parameters = Map::new();

        for attr in xml.attributes() {
            let key = attr.name().to_string();
            let val = attr.value();
            if component_type.parameters.contains(&key) {
                parameters.insert(key, lems.normalise_quantity(&Quantity::parse(val)?)?);
            } else if component_type.attributes.contains(&key)
                || component_type.links.contains_key(&key)
            {
                attributes.insert(key, val.to_string());
            } else if "id" == key || "type" == key {
            } else {
                return Err(nml2_error(format!(
                    "Unknown key/value pair in Instance: {:?} => {:?} in node: {:?}",
                    key, val, node
                )));
            }
        }
        let id = xml.attribute("id").map(|s| s.to_string());
        let mut children = Map::new();
        let mut child = Map::new();
        'a: for node in xml.children() {
            let nm = node.tag_name().name();
            if nm.is_empty() {
                continue 'a;
            }
            let ty = node.attribute("type").unwrap_or(nm);
            if component_type.child.contains_key(nm) {
                child.insert(nm.to_string(), Instance::new(lems, &node)?);
                continue 'a;
            }
            for (n, t) in &component_type.children {
                if lems.derived_from(ty, t) {
                    children
                        .entry(n.to_string())
                        .or_insert_with(Vec::new)
                        .push(Instance::new(lems, &node)?);
                    continue 'a;
                }
            }
            warn!(
                "No idea where to put item {} of type {} in {:?}",
                nm, ty, id
            );
        }
        Ok(Instance {
            component_type,
            child,
            children,
            id,
            parameters,
            attributes,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Collapsed {
    pub name: Option<String>,
    pub exposures: Map<String, String>,
    pub variables: Vec<Variable>,
    pub constants: Map<String, Quantity>,
    pub parameters: Map<String, Option<Quantity>>,
    pub attributes: Map<String, Option<String>>,
    pub conditions: Vec<(String, Boolean, Expr)>,
    pub events: Vec<(String, Expr)>,
    pub kinetic: Vec<Kinetic>,
    pub transitions: Vec<(String, String, String, String)>,
    pub states: Vec<Set<String>>,
}

impl Collapsed {
    pub fn new(nm: &Option<String>) -> Self {
        Collapsed {
            name: nm.clone(),
            exposures: Map::new(),
            variables: Vec::new(),
            constants: Map::new(),
            parameters: Map::new(),
            conditions: Vec::new(),
            attributes: Map::new(),
            events: Vec::new(),
            kinetic: Vec::new(),
            transitions: Vec::new(),
            states: Vec::new(),
        }
    }

    pub fn from_instance(inst: &Instance) -> Result<Self> {
        Self::from_instance_with_name(inst, false)
    }

    pub fn from_instance_with_name(inst: &Instance, use_name: bool) -> Result<Self> {
        use crate::expr::Path;
        let mut coll = Self::from_instance_(inst, &Context::default(), None, use_name)?;
        // Massage kinetic schemes
        for ks in &coll.kinetic {
            let Match(ps) = &ks.edge;
            let mut ix = 0;
            let mut nodes = vec![(vec![], inst.clone())];
            'a: loop {
                if ix >= ps.len() {
                    break;
                }
                let p = &ps[ix];
                ix += 1;
                match &p {
                    Path::Fixed(s) => {
                        for (pfx, node) in &nodes {
                            if let Some(x) = node.child.get(s) {
                                let mut pfx = pfx.clone();
                                pfx.push(s.clone());
                                nodes = vec![(pfx, x.clone())];
                                continue 'a;
                            }
                        }
                        if ix < ps.len() {
                            if let Path::Fixed(q) = &ps[ix] {
                                ix += 1;
                                for (pfx, node) in &nodes {
                                    if let Some(xs) = node.children.get(s) {
                                        let mut pfx = pfx.clone();
                                        pfx.push(s.clone());
                                        nodes = xs
                                            .iter()
                                            .filter(|x| x.id == Some(q.to_string()))
                                            .cloned()
                                            .map(|x| {
                                                let mut pfx = pfx.clone();
                                                pfx.push(x.id.as_deref().unwrap().to_string());
                                                (pfx, x)
                                            })
                                            .collect();
                                        continue 'a;
                                    }
                                }
                            }
                        }
                        panic!("Impossible path {:?}", ks.edge);
                    }
                    Path::When(s, Select::All) => {
                        for (pfx, node) in &nodes {
                            if let Some(xs) = node.children.get(s) {
                                nodes = xs
                                    .iter()
                                    .map(|x| {
                                        let mut pfx = pfx.clone();
                                        pfx.push(s.to_string());
                                        pfx.push(x.id.as_deref().unwrap().to_string());
                                        (pfx, x.clone())
                                    })
                                    .collect();
                                continue 'a;
                            }
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            let mut states = Set::new();
            for (pfx, node) in &nodes {
                // TODO This is a horrible hack and must be replaced by proper lookup
                let mut qfx = Vec::new();
                let Match(ref qs) = &ks.node;
                for q in qs {
                    match q {
                        Path::Up => {
                            qfx.pop().ok_or_else(|| nml2_error("Invalid path"))?;
                        }
                        Path::Fixed(s) => qfx.push(s.clone()),
                        Path::When(s, _) => qfx.push(s.clone()),
                    }
                }
                let qfx = qfx.join("_");
                // TODO End of horrible hack
                let pfx = pfx.join("_");
                let from = format!(
                    "{}_{}_{}",
                    qfx,
                    node.attributes.get("from").unwrap(),
                    ks.state
                );
                let to = format!(
                    "{}_{}_{}",
                    qfx,
                    node.attributes.get("to").unwrap(),
                    ks.state
                );
                coll.transitions.push((
                    from.clone(),
                    to.clone(),
                    format!("{}_{}", pfx, ks.rfwd),
                    format!("{}_{}", pfx, ks.rbwd),
                ));
                states.insert(from);
                states.insert(to);
            }
            coll.states.push(states);
        }
        // Eliminate fixed parameters
        coll.parameters
            .retain(|k, _| !coll.constants.contains_key(k));
        Ok(coll)
    }

    fn from_instance_(
        inst: &Instance,
        ctx: &Context,
        name: Option<String>,
        add_name: bool,
    ) -> Result<Self> {
        let mut result = Collapsed::new(&inst.id);
        let ct = &inst.component_type;
        let mut ctx = ctx.clone();
        let nm = if add_name {
            if let Some(n) = inst.id.as_deref().or(name.as_deref()) {
                n
            } else {
                info!("Found node without id, setting to 'Unknown'");
                "Unknown"
            }
        } else {
            ""
        };
        ctx.enter(
            nm,
            &ct.exposures
                .keys()
                .chain(ct.parameters.iter())
                .chain(ct.constants.keys())
                .chain(ct.variables.iter().map(|v| &v.name))
                .cloned()
                .collect::<Vec<_>>()[..],
        );

        result.exposures = ct
            .exposures
            .iter()
            .map(|(k, v)| (ctx.add_prefix(k), v.clone()))
            .collect();
        result.events = ct
            .events
            .iter()
            .map(|(k, v)| (ctx.add_prefix(k), v.clone()))
            .collect();
        result.constants = ct
            .constants
            .iter()
            .map(|(k, v)| (ctx.add_prefix(k), v.clone()))
            .collect();
        result.parameters = ct
            .parameters
            .iter()
            .map(|k| (ctx.add_prefix(k), inst.parameters.get(k).cloned()))
            .collect();
        result.attributes = ct
            .attributes
            .iter()
            .map(|k| (ctx.add_prefix(k), inst.attributes.get(k).cloned()))
            .collect();
        result.kinetic = ct.kinetic.iter().map(|k| k.add_prefix(&ctx)).collect();

        for v in &ct.variables {
            let name = ctx.add_prefix(&v.name);
            let exposure = v.exposure.as_ref().map(|s| ctx.add_prefix(s));
            let kind = ctx.rename_kind(&v.kind);
            let dimension = v.dimension.clone();
            result.variables.push(Variable {
                name,
                exposure,
                kind,
                dimension,
            });
        }

        for (name, test, val) in &ct.conditions {
            let name = ctx.add_prefix(name);
            let test = ctx.rename_bool(test);
            let val = ctx.rename_expr(val);
            result.conditions.push((name, test, val));
        }

        // Merge children by prefixing w/ children and id
        for (n, cs) in &inst.children {
            ctx.enter(n, &Vec::new());
            for inst in cs {
                result.add(inst, &ctx, None)?;
            }
            ctx.exit();
        }
        // Merge child by prefixing w{} child
        for (n, inst) in &inst.child {
            result.add(inst, &ctx, Some(n.to_string()))?;
        }

        // concretise reductions/selects by converting Select/Product/Sum into DerivedVariables
        for v in result.variables.iter_mut() {
            if let VarKind::Select(by, ps) = &v.kind {
                let ks = result.exposures.keys().cloned().collect::<Vec<_>>();
                let ms = ps
                    .on_path(&ks)
                    .iter()
                    .map(|m| Expr::Var(m.to_string()))
                    .collect::<Vec<Expr>>();
                let kind = match &by {
                    SelectBy::Get => {
                        if let [ref n] = ms[..] {
                            n.clone()
                        } else {
                            return Err(nml2_error(format!(
                                "Required field is not found for {:?} in {:?}",
                                ps, result.exposures
                            )));
                        }
                    }
                    SelectBy::Product => {
                        if ms.is_empty() {
                            trace!(
                                "No instances found for name={} select={:?} via {:?} among {:?}",
                                v.name,
                                ps,
                                by,
                                ks
                            );
                            Expr::F64(1.0)
                        } else {
                            Expr::Mul(ms)
                        }
                    }
                    SelectBy::Sum => {
                        if ms.is_empty() {
                            Expr::F64(0.0)
                        } else {
                            Expr::Add(ms)
                        }
                    }
                };
                v.kind = VarKind::Derived(Vec::new(), Some(kind));
            }
        }
        ctx.exit();
        Ok(result)
    }

    pub fn add(&mut self, inst: &Instance, ctx: &Context, name: Option<String>) -> Result<()> {
        let other = Self::from_instance_(inst, ctx, name, true)?;
        self.parameters
            .extend(other.parameters.iter().map(|(a, b)| (a.clone(), b.clone())));
        self.attributes
            .extend(other.attributes.iter().map(|(a, b)| (a.clone(), b.clone())));
        self.constants
            .extend(other.constants.iter().map(|(a, b)| (a.clone(), b.clone())));
        self.exposures
            .extend(other.exposures.iter().map(|(a, b)| (a.clone(), b.clone())));
        self.variables.extend(other.variables.iter().cloned());
        self.kinetic.extend(other.kinetic.iter().cloned());
        self.events.extend(other.events.iter().cloned());
        Ok(())
    }
}

/// Stacked contexts of local symbols
#[derive(Debug, Clone, Default)]
pub struct Context(Vec<(String, Vec<String>)>);

impl Context {
    fn enter(&mut self, name: &str, vars: &[String]) {
        self.0.push((name.to_string(), vars.to_vec()));
    }
    fn exit(&mut self) {
        self.0.pop();
    }
    fn keys(&self) -> Vec<String> {
        self.0
            .iter()
            .filter(|k| !k.0.is_empty())
            .map(|t| t.0.clone())
            .collect()
    }

    fn add_prefix(&self, name: &str) -> String {
        let mut ks = self.keys();
        ks.push(name.to_string());
        ks.join("_")
    }

    fn rename(&self, name: &str) -> String {
        let name = name.to_string();
        let mut it = false;
        let mut pfx: Vec<&str> = vec![&name];
        for (p, vs) in self.0.iter().rev() {
            it |= vs.contains(&name);
            if it {
                pfx.push(p);
            }
        }
        if !it {
            trace!("Could not find {} in context.", name);
        }
        pfx = pfx
            .iter()
            .filter(|s| !s.is_empty())
            .rev()
            .cloned()
            .collect();
        pfx.join("_")
    }

    fn rename_expr(&self, v: &Expr) -> Expr {
        v.map(&|e| {
            if let Expr::Var(s) = e {
                Expr::Var(self.rename(s))
            } else {
                e.clone()
            }
        })
    }

    fn rename_bool(&self, v: &Boolean) -> Boolean {
        v.map(&|e| {
            if let Expr::Var(s) = e {
                Expr::Var(self.rename(s))
            } else {
                e.clone()
            }
        })
    }

    fn rename_kind(&self, kind: &VarKind) -> VarKind {
        let pfx = self.keys();
        match kind {
            VarKind::Select(b, ps) => VarKind::Select(b.clone(), ps.add_prefix(&pfx)),
            VarKind::Derived(cs, df) => VarKind::Derived(
                cs.iter()
                    .map(|(c, e)| (self.rename_bool(c), self.rename_expr(e)))
                    .collect(),
                df.as_ref().map(|x| self.rename_expr(x)),
            ),
            VarKind::State(i, d) => VarKind::State(
                i.as_ref().map(|x| self.rename_expr(x)),
                d.as_ref().map(|x| self.rename_expr(x)),
            ),
        }
    }
}

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
    /// conditional state assignments
    pub conditions: Vec<(String, Boolean, Expr)>,
    /// parameters
    pub parameters: Vec<String>,
    /// attributes (non-numerical parameters?)
    pub attributes: Vec<String>,
    /// constants
    pub constants: Map<String, Quantity>,
    /// events: on event assign `variable` <- `Expr`
    pub events: Vec<(String, Expr)>,
    /// Linked components
    pub links: Map<String, String>,
    /// Linked components
    pub kinetic: Vec<Kinetic>,
}

impl ComponentType {
    pub fn from_lems(ct: &lems::raw::ComponentType) -> Result<Self> {
        let name = ct.name.to_string();
        let base = ct.extends.clone();
        let mut child = Map::new();
        let mut children = Map::new();
        let mut exposures = Map::new();
        let mut variables = Vec::new();
        let mut conditions = Vec::new();
        let mut parameters = Vec::new();
        let mut attributes = Vec::new();
        let mut constants = Map::new();
        let mut events = Vec::new();
        let mut kinetic = Vec::new();
        let mut links = Map::new();

        for ix in &ct.body {
            use lems::raw::ComponentTypeBody::*;
            match ix {
                ComponentReference(c) => {
                    attributes.push(c.name.to_string());
                }
                Path(c) => {
                    attributes.push(c.name.to_string());
                }
                Child(c) => {
                    child.insert(c.name.to_string(), c.r#type.to_string());
                }
                Children(c) => {
                    children.insert(c.name.to_string(), c.r#type.as_ref().unwrap().to_string());
                }
                Parameter(p) => {
                    parameters.push(p.name.to_string());
                }
                DerivedParameter(p) => {
                    let expr = Expr::parse(&p.value)?;
                    let kind = VarKind::Derived(Vec::new(), Some(expr));
                    let name = p.name.to_string();
                    let dimension = p.dimension.to_string();
                    variables.push(Variable {
                        name,
                        exposure: None,
                        dimension,
                        kind,
                    });
                }
                Constant(c) => {
                    constants.insert(c.name.to_string(), Quantity::parse(&c.value)?);
                }
                Exposure(e) => {
                    exposures.insert(e.name.to_string(), e.dimension.to_string());
                }
                Text(t) => {
                    attributes.push(t.name.to_string());
                }
                Dynamics(d) => {
                    lems_dynamics(
                        d,
                        &mut variables,
                        &mut conditions,
                        &mut events,
                        &mut kinetic,
                    )?;
                }
                Link(t) => {
                    links.insert(t.name.to_string(), t.r#type.to_string());
                }
                Fixed(t) => {
                    constants.insert(t.parameter.to_string(), Quantity::parse(&t.value)?);
                }
                b => trace!("Ignoring {:?}", b),
            }
        }

        trace!("Type {} children={:?} child={:?}", name, children, child);

        Ok(Self {
            name,
            base,
            child,
            children,
            exposures,
            variables,
            conditions,
            constants,
            parameters,
            attributes,
            events,
            links,
            kinetic,
        })
    }
}

/// Helper: Process Dynamics part of ComponentType
fn lems_dynamics(
    dynamics: &lems::raw::Dynamics,
    variables: &mut Vec<Variable>,
    conditions: &mut Vec<(String, Boolean, Expr)>,
    events: &mut Vec<(String, Expr)>,
    kinetic: &mut Vec<Kinetic>,
) -> Result<()> {
    use lems::raw::ConditionalDerivedVariableBody::*;
    use lems::raw::DynamicsBody::*;
    for b in &dynamics.body {
        match b {
            DerivedVariable(v) => {
                let kind = if let Some(s) = v.select.as_ref() {
                    let by = match v.reduce.as_deref() {
                        Some("add") => SelectBy::Sum,
                        Some("multiply") => SelectBy::Product,
                        None => SelectBy::Get,
                        Some(x) => return Err(nml2_error(format!("Unknown reduction {}", x))),
                    };
                    VarKind::Select(by, Match::parse(s)?)
                } else if let Some(e) = v.value.as_ref() {
                    VarKind::Derived(Vec::new(), Some(Expr::parse(e)?))
                } else {
                    return Err(nml2_error(format!("Illegal DerivedVar: {}", v.name)));
                };
                variables.push(Variable::new(&v.name, &v.exposure, &v.dimension, &kind));
            }
            StateVariable(v) => variables.push(Variable::new(
                &v.name,
                &v.exposure,
                &v.dimension,
                &VarKind::State(None, None),
            )),
            ConditionalDerivedVariable(v) => {
                let mut cs = Vec::new();
                let mut df = None;
                for Case(c) in &v.body {
                    let e = Expr::parse(&c.value)?;
                    if let Some(b) = c.condition.as_ref() {
                        cs.push((Boolean::parse(b)?, e));
                    } else {
                        df = Some(e);
                    }
                }
                variables.push(Variable::new(
                    &v.name,
                    &v.exposure,
                    &v.dimension,
                    &VarKind::Derived(cs, df),
                ));
            }
            OnStart(v) => {
                use lems::raw::OnStartBody::*;
                for StateAssignment(a) in &v.body {
                    let it = variables.iter_mut().find(|x| x.name == a.variable);
                    if let Some(Variable {
                        kind: VarKind::State(ref mut i, _),
                        ..
                    }) = it
                    {
                        *i = Some(Expr::parse(&a.value)?);
                    } else {
                        return Err(nml2_error(format!("Must be a StateVar: {}", a.variable)));
                    }
                }
            }
            OnEvent(v) => {
                use lems::raw::OnEventBody::*;
                for b in &v.body {
                    match b {
                        StateAssignment(a) => {
                            let it = variables.iter().find(|x| x.name == a.variable);
                            if let Some(Variable {
                                kind: VarKind::State(_, _),
                                ..
                            }) = it
                            {
                                events.push((a.variable.to_string(), Expr::parse(&a.value)?));
                            } else {
                                return Err(nml2_error(format!(
                                    "Must be a StateVar: {}",
                                    a.variable
                                )));
                            }
                        }
                        b => trace!("Ignoring {:?}", b),
                    }
                }
            }
            OnCondition(v) => {
                use lems::raw::OnConditionBody::*;
                for b in &v.body {
                    match b {
                        StateAssignment(a) => {
                            let it = variables.iter().find(|x| x.name == a.variable);
                            if let Some(Variable {
                                kind: VarKind::State(_, _),
                                ..
                            }) = it
                            {
                                conditions.push((
                                    a.variable.to_string(),
                                    Boolean::parse(&v.test)?,
                                    Expr::parse(&a.value)?,
                                ));
                            } else {
                                return Err(nml2_error(format!(
                                    "Must be a StateVar: {}",
                                    a.variable
                                )));
                            }
                        }
                        b => trace!("Ignoring {:?}", b),
                    }
                }
            }
            TimeDerivative(v) => {
                let it = variables.iter_mut().find(|x| x.name == v.variable);
                if let Some(Variable {
                    kind: VarKind::State(_, ref mut d),
                    ..
                }) = it
                {
                    *d = Some(Expr::parse(&v.value)?);
                } else {
                    return Err(nml2_error(format!("Must be a StateVar: {}", v.variable)));
                }
            }
            KineticScheme(k) => kinetic.push(Kinetic::new(k)?),
            b => trace!("Ignoring {:?}", b),
        }
    }
    Ok(())
}
