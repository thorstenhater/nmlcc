use std::collections::HashMap as Map;

use tracing::info;
use roxmltree::Node;

use crate::{Result,
            expr::{Quantity, Expr, Match},
            variable::{Variable, VarKind, SelectBy},
            lems};

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
        let node = xml.attribute("type").unwrap_or_else(|| xml.tag_name().name());
        let component_type = lems.compose_component_type(node)?;

        let mut attributes = Map::new();
        let mut parameters = Map::new();

        for attr in xml.attributes() {
            let key = attr.name().to_string();
            let val = attr.value();
            if component_type.parameters.contains(&key) {
                parameters.insert(key, lems.normalise_quantity(&Quantity::parse(val)?)?);
            } else if component_type.attributes.contains(&key) {
                attributes.insert(key, val.to_string());
            }
        }

        let id = xml.attribute("id").map(|s| s.to_string());

        let mut children = Map::new();
        let mut child = Map::new();
        for node in xml.children() {
            let nm = node.tag_name().name();
            if component_type.child.contains_key(nm) {
                child.insert(nm.to_string(), Instance::new(lems, &node)?);
            } else {
                for (n, t) in &component_type.children {
                    if lems.derived_from(nm, t) {
                        children.entry(n.to_string()).or_insert_with(Vec::new).push(Instance::new(lems, &node)?);
                    }
                }
            }
        }
        Ok(Instance { component_type, child, children, id, parameters, attributes })
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
    pub events: Vec<(String, Expr)>,
}

impl Collapsed {
    pub fn new(nm: &Option<String>) -> Self {
        Collapsed { name: nm.clone(), exposures: Map::new(), variables: Vec::new(), constants: Map::new(), parameters: Map::new(), attributes: Map::new(), events: Vec::new() }
    }

    pub fn from_instance(inst: &Instance) -> Result<Self> { Self::from_instance_(inst, &Context::new(), None, false) }

    fn from_instance_(inst: &Instance, ctx: &Context, name: Option<String>, add_name: bool) -> Result<Self> {
        let mut result = Collapsed::new(&inst.id);
        let ct = &inst.component_type;
        let mut ctx = ctx.clone();
        let nm = if add_name {
            inst.id.as_deref().or_else(|| name.as_deref()).unwrap()
        } else {
            ""
        };
        ctx.enter(nm,
                  &ct.exposures.keys()
                    .chain(ct.parameters.iter())
                    .chain(ct.constants.keys())
                    .chain(ct.variables.iter().map(|v| &v.name))
                    .cloned()
                    .collect::<Vec<_>>()[..]);

        result.exposures  = ct.exposures.iter().map(|(k, v)| (ctx.add_prefix(k), v.clone())).collect();
        result.events     = ct.events.iter().map(|(k, v)| (ctx.add_prefix(k), v.clone())).collect();
        result.constants  = ct.constants.iter().map(|(k, v)| (ctx.add_prefix(k), v.clone())).collect();
        result.parameters = ct.parameters.iter().map(|k| (ctx.add_prefix(k), inst.parameters.get(k).cloned())).collect();
        result.attributes = ct.attributes.iter().map(|k| (ctx.add_prefix(k), inst.attributes.get(k).cloned())).collect();

        for v in &ct.variables {
            let name      = ctx.add_prefix(&v.name);
            let exposure  = v.exposure.as_ref().map(|s| ctx.add_prefix(&s));
            let kind      = ctx.rename_kind(&v.kind);
            let dimension = v.dimension.clone();
            result.variables.push(Variable { name, exposure, kind, dimension});
        }

        // Merge children by prefixing w/ children and id
        for (n, cs) in &inst.children {
            ctx.enter(n, &Vec::new());
            for inst in cs { result.add(inst, &ctx, None)?; }
            ctx.exit();
        }
        // Merge child by prefixing w{} child
        for (n, inst) in &inst.child { result.add(inst, &ctx, Some(n.to_string()))?; }

        // concretise reductions/selects by converting Select/Product/Sum into DerivedVariables
        for v in result.variables.iter_mut() {
            if let VarKind::Select(by, ps) = &v.kind {
                let ms = ps.on_path(&result.exposures).iter()
                                                      .map(|m| Expr::Var(m.to_string()))
                                                      .collect::<Vec<Expr>>();
                let kind = match &by {
                    SelectBy::Get => if let [ref n] = ms[..] {
                        n.clone()
                    } else {
                        return Err(format!("Required field is not found for {:?} in {:?}", ps, result.exposures));
                    },
                     // TODO _technically_ we don't not need to guard ./. empty here, it will be squashed by simplify
                    SelectBy::Product => if ms.is_empty() { Expr::F64(1.0) } else { Expr::Mul(ms) },
                    SelectBy::Sum     => if ms.is_empty() { Expr::F64(0.0) } else { Expr::Add(ms) },
                };
                v.kind = VarKind::Derived(Vec::new(), Some(kind));
            }
        }
        ctx.exit();
        Ok(result)
    }

    fn add(&mut self, inst: &Instance, ctx: &Context, name: Option<String>) -> Result<()> {
        let other = Self::from_instance_(inst, ctx, name, true)?;
        self.parameters.extend(other.parameters.iter().map(|(a, b)| (a.clone(), b.clone())));
        self.attributes.extend(other.attributes.iter().map(|(a, b)| (a.clone(), b.clone())));
        self.constants.extend(other.constants.iter().map(|(a, b)| (a.clone(), b.clone())));
        self.exposures.extend(other.exposures.iter().map(|(a, b)| (a.clone(), b.clone())));
        self.variables.extend(other.variables.iter().cloned());
        Ok(())
    }

    pub fn pprint(&self) {
        println!(" * {}", self.name.as_deref().unwrap_or("???"));
        if !self.constants.is_empty() {
            println!("   * Constants");
            for v in &self.constants {
                println!("     * {} = {:?}", v.0, v.1);
            }
        }
        if !self.exposures.is_empty() {
            println!("   * Exposures");
            for (e, d) in &self.exposures {
                println!("     * {} ({})", e, d);
            }
        }
        if !self.parameters.is_empty() {
            println!("   * Parameters");
            for (k, v) in &self.parameters {
                if let Some(Quantity {value: x, unit: u}) = v {
                    println!("     * {} = {} ({:?})", k, x, u);
                } else {
                    println!("     * {} = UNSET", k);
                }
            }
        }
        if !self.attributes.is_empty() {
            println!("   * Attributes");
            for (k, v) in &self.attributes {
                println!("     * {} = {:?}", k, v);
            }
        }
        if !self.variables.is_empty() {
            println!("   * Variables");
            for v in &self.variables {
                if let Some(ref e) = v.exposure {
                    print!("     * {} -> {} ({}) = ", e, v.name, v.dimension);
                } else {
                    print!("     * {} ({}) = ", v.name, v.dimension);
                }
                match &v.kind {
                    VarKind::State(i, d)  => {
                        println!();
                        println!("       | {}(t=0) = {}", v.name, i.as_ref().unwrap().print_to_string());
                        println!("       | {}'(t)  = {}", v.name, d.as_ref().unwrap().print_to_string());
                    },
                    VarKind::Derived(cs, df) => {
                        if cs.is_empty() {
                            println!("{}", df.as_ref().unwrap().print_to_string());
                        } else {
                            println!();
                            for (b, c) in cs {
                                println!("       | {} => {}", b.print_to_string(), c.print_to_string());
                            }
                            if let Some(df) = df {
                                println!("       | otherwise => {}", df.print_to_string());
                            }
                        }
                    }
                    _ => panic!("Impossible"),
                }
            }
        }
    }

    pub fn simplify(&self) -> Self {
        let mut prv = self.clone();
        for _ in 0..5 {
            let mut table: Map<String, Expr> = Map::new();

            // NOTE(TH): this seems overly aggressive
            // for (p, v) in &self.parameters {
                // if let Some(Quantity{ value, ..}) = v {
                    // table.insert(p, value);
                // }
            // }

            for v in &prv.variables {
                if let VarKind::Derived(cs, Some(k)) = &v.kind {
                    if cs.is_empty() && matches!(k, Expr::F64(_) | Expr::Var(_)) {
                            table.insert(v.name.to_string(), k.clone());
                    }
                }
            }

            let mut cur = prv.clone();
            let splat = |v: &Expr| {
                if let Expr::Var(n) = v {
                    if let Some(x) = table.get(n) { return x.clone(); }
                }
                v.clone()
            };

            for v in cur.variables.iter_mut() {
                match &v.kind {
                    VarKind::State(i, d) => {
                        let i = i.as_ref().map(|e| e.map(&splat).simplify());
                        let d = d.as_ref().map(|e| e.map(&splat).simplify());
                        v.kind = VarKind::State(i, d);
                    }
                    VarKind::Derived(cs, df) => {
                        let cs = cs.iter()
                                   .map(|(c, e)| (c.map(&splat).simplify(), e.map(&splat).simplify()))
                                   .collect::<Vec<_>>();
                        let df = df.as_ref().map(|e| e.map(&splat).simplify());
                        v.kind = VarKind::Derived(cs, df);
                    }
                    _ => {}
                }
            }
            if cur == prv { break; }
            prv = cur;
        }
        prv
    }

}

/// Stacked contexts of local symbols
#[derive(Debug, Clone)]
struct Context(Vec<(String, Vec<String>)>);

impl Context {
    fn new() -> Self { Context(Vec::new()) }
    fn enter(&mut self, name: &str, vars: &[String]) { self.0.push((name.to_string(), vars.to_vec())); }
    fn exit(&mut self) { self.0.pop(); }
    fn keys(&self) -> Vec<String> { self.0.iter().filter(|k| !k.0.is_empty()).map(|t| t.0.clone()).collect() }

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
            if it { pfx.push(p); }
        }
        if !it { info!("Could not find {} in context.", name); }
        pfx = pfx.iter().filter(|s| !s.is_empty()).rev().cloned().collect();
        pfx.join("_")
    }

    fn rename_expr(&self, v: &Expr) -> Expr {
        v.map(&|e| if let Expr::Var(s) = e {
            Expr::Var(self.rename(s))
        } else {
            e.clone()
        })
    }

    fn rename_kind(&self, kind: &VarKind) -> VarKind {
        let pfx = self.keys();
        match kind {
            VarKind::Select(b, ps) => VarKind::Select(b.clone(), ps.add_prefix(&pfx)),
            VarKind::Derived(cs, df) =>
                VarKind::Derived(cs.iter()
                                   .map(|(c, e)| (self.rename_expr(c), self.rename_expr(e)))
                                   .collect(),
                                 df.as_ref().map(|x| self.rename_expr(x))),
            VarKind::State(i, d) => VarKind::State(i.as_ref().map(|x| self.rename_expr(x)),
                                                   d.as_ref().map(|x| self.rename_expr(x))),
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
    pub fn from_lems(ct: &lems::raw::ComponentType) ->  Result<Self> {
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
