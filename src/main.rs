use std::collections::HashMap as Map;
use std::collections::HashSet as Set;
use std::fs::write;

use roxmltree::Node;

use clap::Parser;

mod xml;
mod lems;
mod expr;

use expr::{Quantity, Expr, Match};

type Result<T> = std::result::Result<T, String>;

/// Dimension = mass^m length^l time^t current^i ???^k concentration^n
#[derive(Debug, Clone)]
pub struct Dimension { pub m: i64, pub l: i64, pub t: i64, pub i: i64, pub k: i64, pub n: i64, }

/// Unit = Dimension 10^power scale + offset
#[derive(Debug, Clone)]
pub struct Unit {
  pub dimension: String,
  pub power: i64,
  pub scale: f64,
  pub offset: f64,
}

#[derive(Clone, Debug, PartialEq)]
enum SelectBy { Get, Sum, Product, }

#[derive(Clone, Debug, PartialEq)]
enum VarKind {
    /// State variable defined by 1st order ODE X(t=0), X'(t)
    State(Option<Expr>, Option<Expr>),
    /// cases=[condition => expression] default=expression
    Derived(Vec<(Expr, Expr)>, Option<Expr>),
    /// Select one or more fields from children. NOTE will be gone after collapsing
    Select(SelectBy, Match),
}

#[derive(Clone, Debug, PartialEq)]
/// Variable
struct Variable {
    name: String,
    /// if linked to an exposure. NOTE According to PG must be same as `name`
    exposure: Option<String>,
    /// link to a known dimension
    dimension: String,
    kind: VarKind,
}

impl Variable {
    fn new(n: &str, e: &Option<String>, d: &str, k: &VarKind) -> Self {
        Variable { name: n.to_string(), exposure: e.clone(), dimension: d.to_string(), kind: k.clone() }
    }
}

/// LEMS ComponentType ('class') preprocessed
#[derive(Clone, Debug, Default)]
struct ComponentType {
    /// type name
    name: String,
    /// Base component name, if any
    base: Option<String>,
    /// One per entry with a type derived from base
    /// Given a -> A, we expect to find
    /// <a type="A">
    /// and 'a' is a single value of type 'A'
    child: Map<String, String>,
    /// Potentially many per entry with a type derived from base
    /// Given a -> A, we expect to find
    /// <A id="a0">
    /// <A id="a1">
    /// thus 'a' is a collection ['a0', 'a1']
    children: Map<String, String>,
    /// unique list of externally visible variables, must be linked to a type of variable
    /// NOTE: exposures cannot change name of variables, see answer here https://github.com/NeuroML/NeuroML2/issues/178
    exposures: Map<String, String>,
    /// variables, taken from the <Dynamics> block
    variables: Vec<Variable>,
    /// parameters
    parameters: Vec<String>,
    /// attributes (non-numerical parameters?)
    attributes: Vec<String>,
    /// constants
    constants: Map<String, Quantity>,
    /// events: on event assign `variable` <- `Expr`
    events: Vec<(String, Expr)>,
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
                        b => println!("NOTE: Ignoring {:?} in {:?}", b, v),
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

/// Processed LEMS data
#[derive(Clone, Debug, Default)]
struct Lems {
    /// Inheritance hierarchy derived -> base
    base_of: Map<String, String>,
    /// Name -> Type
    types: Map<String, ComponentType>,
    /// symbol -> unit
    units: Map<String, Unit>,
    /// name -> dimension
    dimensions: Map<String, Dimension>,
}

impl Lems {
    /// Pull LEMS from file
    fn from_file(dn: &str, file: &str) -> Result<Self> { Self::from_raw(&lems::Lems::from_file(dn, file)?) }

    /// Ingest raw LEMS on munge into a digestible form
    fn from_raw(raw: &lems::Lems) -> Result<Self> {
        let mut result = Self::default();
        for ct in &raw.component_types {
            if let Some(base) = &ct.extends {
                result.base_of.insert(ct.name.to_string(), base.to_string());
            }
            result.types.insert(ct.name.to_string(), ComponentType::from_lems(ct)?);
        }
        Ok(result)
    }

    /// Check if type `d` derives from type `b`.
    fn derived_from(&self, d: &str, b: &str) -> bool {
        let mut d = d;
        loop {
            if d == b { return true; }
            if let Some(k) = self.base_of.get(d) {
                d = k;
                continue;
            }
            break;
        }
        false
    }

    /// Flatten inheritance hierarchy. This will take a component-type name
    /// (must be present) and compose a final type from it by walking the
    /// inheritance chain. The result will be built by appending all members
    /// while later (='more derived') items take precedence.
    fn compose_component_type(&self, id: &str) -> Result<ComponentType> {
        let mut result = self.types.get(id).ok_or(format!("No such type: {}.", id))?.clone();
        let mut base = result.base.as_ref();
        while let Some(id) = base {
            let ty = self.types.get(id).ok_or(format!("No such type: {}.", id))?;
            for (k,v) in &ty.child {
                result.child.entry(k.to_string()).or_insert_with(|| v.to_string());
            }
            for (k,v) in &ty.children {
                result.children.entry(k.to_string()).or_insert_with(|| v.to_string());
            }
            for (e, d) in &ty.exposures {
                if !result.exposures.contains_key(e) { result.exposures.insert(e.clone(), d.clone()); }
            }
            for p in &ty.parameters {
                if !result.parameters.contains(p) { result.parameters.push(p.clone()); }
            }
            for p in &ty.variables {
                if !result.variables.contains(p) { result.variables.push(p.clone()); }
            }
            base = ty.base.as_ref();
        }
        Ok(result)
    }
}


#[derive(Debug, Clone, PartialEq)]
struct Collapsed {
    name: Option<String>,
    exposures: Map<String, String>,
    variables: Vec<Variable>,
    constants: Map<String, Quantity>,
    parameters: Map<String, Option<Quantity>>,
    attributes: Map<String, Option<String>>,
    events: Vec<(String, Expr)>,
}

/// Map variables to dependencies
fn find_dependencies(variables: &[Variable]) -> Map<String, Set<String>> {
    let mut deps = Map::new();
    let add_var = |e: &Expr, acc: &mut Set<String>| if let Expr::Var(v) = e { acc.insert(v.to_string()); };
    for v in variables {
        let u = deps.entry(v.name.to_string()).or_insert_with(Set::new);
        match &v.kind {
            VarKind::State(it, dv) => {
                if let Some(i) = it.as_ref() { i.fold(u, &add_var) }
                if let Some(d) = dv.as_ref() { d.fold(u, &add_var) }
            }
            VarKind::Derived(cs, df) => {
                if let Some(d) = df.as_ref() { d.fold(u, &add_var) }
                for (c, x) in cs { c.fold(u, &add_var); x.fold(u, &add_var); }
            }
            _ => {}
        }
    }
    deps
}

/// Dependency-sorted list of variable names, ignoring pre-defined names
/// Given constants {c0=23, c1=42}, roots=[r] and
///    a = b + c0; r = a*b; b = c1^2
/// will produce
///    b : only constants
///    a : b and constants
///    r : a and b
fn sorted_dependencies_of(start: &[String], deps: &Map<String, Set<String>>, known: &Set<String>) -> Result<Vec<String>> {
    // Build transitive dependencies
    let mut todo = Vec::new();
    let mut current = start.to_vec();
    while let Some(next) = current.pop() {
        let mut ds = deps.get(&next).unwrap_or(&Set::new()).clone();
        ds.remove(&next);
        for k in known.iter() { ds.remove(k); }
        for v in ds {
            current.push(v.to_string());
            todo.push(v.to_string());
        }
    }

    let mut result = Vec::new();
    let mut seen = known.clone();
    'a: while !todo.is_empty() {
        for ix in 0..todo.len() {
            let v = &todo[ix];
            let mut ds = deps.get(v).unwrap_or(&Set::new()).clone();
            ds.remove(v);
            if ds.is_subset(&seen) {
                if !result.contains(v) { result.push(v.to_string()); }
                seen.insert(v.to_string());
                todo.remove(ix);
                continue 'a;
            }
        }
        return Err(format!("Could not resolve {:?}: todo={:?} done={:?} table={:?}", start, todo, result, deps));
    }

    Ok(result)
}

fn print_dependencies(roots: &[String], vars: &[Variable], known: &Set<String>) -> Result<String> {
    let mut result = Vec::new();
    let dependencies = find_dependencies(vars);
    let mut deps = sorted_dependencies_of(roots, &dependencies, known)?;
    if !deps.is_empty() {
        result.push(format!("  LOCAL {}\n", deps.join(", ")));
    }
    deps.extend(roots.iter().cloned());
    for d in deps {
        match vars.iter().find(|k| k.name == d) {
            Some(Variable{ kind: VarKind::Derived(cs, df), .. }) => {
                let mut depth = 0;
                for (b, e) in cs {
                    if depth != 0 { result.push(String::from("  else {")); }
                    result.push(format!("  if ({}) {{ {} = {} }}", b.print_to_string(), d, e.print_to_string()));
                    depth += 1;
                }
                if let Some(e) = df {
                    if depth != 0 { result.push(String::from("  else {")); }
                    result.push(format!("  {} = {}", d, e.print_to_string()));
                    if depth != 0 { result.push(String::from("  }")); }
                }
                for _ in 0..depth-1 { result.push(String::from("  }")); }
            }
            Some(Variable{ kind: VarKind::State(Some(x), None), .. }) => { result.push(format!("  {} = {}",  d, x.print_to_string())); }
            Some(Variable{ kind: VarKind::State(None, Some(x)), .. }) => { result.push(format!("  {}' = {}", d, x.print_to_string())); }
            Some(e) => { return Err(format!("Don't know what to do with variable: {:?}", e)); }
            None => { return Err(format!("No such variable: {}", d)); }
        }
    }
    Ok(result.join("\n"))
}

impl Collapsed {
    fn new(nm: &Option<String>) -> Self {
        Collapsed { name: nm.clone(), exposures: Map::new(), variables: Vec::new(), constants: Map::new(), parameters: Map::new(), attributes: Map::new(), events: Vec::new() }
    }

    fn from_instance(inst: &Instance) -> Result<Self> { Self::from_instance_(inst, &Context::new(), None, false) }

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

    fn pprint(&self) {
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


    fn simplify(&self) -> Self {
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

    fn automatic_variables(&self) -> Vec<String> {
        vec!["v".to_string(),
             "v_peer".to_string(),
             format!("e{}", self.ion_species()),
             format!("i{}", self.ion_species()),
             format!("{}i", self.ion_species()),
             format!("{}o", self.ion_species()),]
    }

    fn nmodl_init_block(&self) -> Result<String> {
        let mut state = Vec::new();
        let mut deriv = Vec::new();

        for var in &self.variables {
            match &var.kind {
                VarKind::State(i, _) => {
                    if i.is_some() {
                        let mut v = var.clone();
                        v.kind = VarKind::State(None, i.clone());
                        state.push(v.clone());
                        deriv.push(v);
                    }
                }
                VarKind::Derived(_, _) => deriv.push(var.clone()),
                VarKind::Select(_, _) => return Err(format!("Select variable in {} post flattening stage.", var.name)),
            }
        }

        if state.is_empty() { return Ok(String::new()); }

        // Variables we can access everywhere: parameters, constants, but not state (might not be defined yet)
        let known = self.parameters.iter().map(|p| p.0.to_string())
                        .chain(self.constants.iter().map(|p|p.0.to_string()))
                        .chain(self.automatic_variables().iter().cloned())
                        .collect::<Set<_>>();

        let init = state.iter().map(|v| v.name.to_string()).collect::<Vec<_>>();
        let deps = deriv.iter().chain(state.iter()).cloned().collect::<Vec<_>>();
        let result = vec![String::from("INITIAL {"),
                         print_dependencies(&init, &deps, &known)?,
                         String::from("}\n\n"),];
        Ok(result.join("\n"))
    }

    fn nmodl_deriv_block(&self) -> Result<String> {
        let mut state = Vec::new();
        let mut deriv = Vec::new();

        for var in &self.variables {
            match &var.kind {
                VarKind::State(_, d) => {
                    if d.is_some() {
                        let mut v = var.clone();
                        v.kind = VarKind::State(None, d.clone());
                        state.push(v.clone());
                        deriv.push(v);
                    }
                }
                VarKind::Derived(_, _) => deriv.push(var.clone()),
                VarKind::Select(_, _) => return Err(format!("Select variable in {} post flattening stage.", var.name)),
            }
        }


        if state.is_empty() { return Ok(String::new()); }

        // Variables we can access everywhere: parameters, constants, state and voltage
        let known = self.parameters.iter().map(|p| p.0.to_string())
                        .chain(self.constants.iter().map(|p|p.0.to_string()))
                        .chain(state.iter().map(|v| v.name.to_string()))
                        .chain(self.automatic_variables().iter().cloned())
                        .collect::<Set<_>>();

        let init = state.iter().map(|v| v.name.to_string()).collect::<Vec<_>>();
        let deps = deriv.iter().chain(state.iter()).cloned().collect::<Vec<_>>();
        let result = vec![String::from("DERIVATIVE dstate {"),
                          print_dependencies(&init, &deps, &known)?,
                          String::from("}\n\n"),];
        Ok(result.join("\n"))
    }

    fn nmodl_break_block(&self) -> Result<String> {
        let mut state  = Vec::new();
        let mut deriv  = Vec::new();

        for var in &self.variables {
            match &var.kind {
                VarKind::State(_, d) => {
                    if d.is_some() {
                        let mut v = var.clone();
                        v.kind = VarKind::State(None, d.clone());
                        state.push(v.clone());
                        deriv.push(v);
                    }
                }
                VarKind::Derived(_, _) => deriv.push(var.clone()),
                VarKind::Select(_, _) => return Err(format!("Select variable in {} post flattening stage.", var.name)),
            }
        }

        // Variables we can access everywhere: parameters, constants, state and voltage
        let known = self.parameters.iter().map(|p| p.0.to_string())
                        .chain(self.constants.iter().map(|p|p.0.to_string()))
                        .chain(state.iter().map(|v| v.name.to_string()))
                        .chain(self.automatic_variables().iter().cloned())
                        .collect::<Set<_>>();

        let mut result = vec![String::from("BREAKPOINT {")];
        if !state.is_empty() {
            result.push(String::from("  SOLVE dstate METHOD cnexp"));
        }
        result.push(print_dependencies(&[format!("i{}", self.ion_species())], &deriv, &known)?);
        result.push(String::from("}\n\n"));
        Ok(result.join("\n"))
    }

    fn nmodl_state_block(&self) -> Result<String> {
        let state = self.variables.iter()
                                  .filter(|v| matches!(v.kind, VarKind::State(_, _)))
                                  .map(|v| v.name.to_string())
                                  .collect::<Vec<_>>();
        if !state.is_empty() {
            Ok(format!("STATE {{ {} }}\n\n", state.join(" ")))
        } else {
            Ok(String::new())
        }
    }

    fn nmodl_param_block(&self) -> Result<String> {
        if self.parameters.is_empty() { return Ok(String::new()); }
        let mut result = vec![String::from("PARAMETER {")];
        for (k, v) in &self.parameters {
            if let Some(v) = v {
                result.push(format!("  {} = {}", k, v.value));
            } else {
                result.push(format!("  {}", k));
            }
        }
        result.push(String::from("}\n\n"));
        Ok(result.join("\n"))
    }

    fn nmodl_neuron_block(&self) -> Result<String> {
        let ion = self.ion_species();
        let current = if ion.is_empty() {
            String::from("NONSPECIFIC_CURRENT i")
        } else {
            format!("USEION {} READ e{} WRITE i{}", ion, ion, ion)
        };

        let mut result = vec![String::from("NEURON {"),
                              format!("", self.suffix()),
        ];

        Ok(format!("NEURON {{
  SUFFIX {}
  {}
}}
", self.suffix(), current))
    }

    fn suffix(&self) -> String { self.name.as_ref().unwrap().to_string() }
    fn ion_species(&self) -> String { self.attributes.get("species").cloned().flatten().unwrap_or_else(String::new) }

    fn to_nmodl(&self) -> Result<String> {
        let ion = self.ion_species();
        let mut result = vec![
                              self.nmodl_param_block()?,
                              self.nmodl_state_block()?,
                              self.nmodl_init_block()?,
                              self.nmodl_deriv_block()?,
                              self.nmodl_break_block()?,];
        if !self.events.is_empty() {
            result.push(String::from("NET_RECEIVE(weight) {\n"));
            for (k, v) in &self.events {
                result.push(format!("  {} = {}\n", k, v.print_to_string()));
            }
            result.push(String::from("}\n\n"));
        }
        Ok(result.join(""))
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
        if !it { eprintln!("NOTE: Could not find {} in context.", name); }
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
                // cs.iter_mut().for_each(
            VarKind::State(i, d) => VarKind::State(i.as_ref().map(|x| self.rename_expr(x)),
                                                   d.as_ref().map(|x| self.rename_expr(x))),
        }
    }
}

/// Instantiated NML2 ComponentType
#[derive(Debug, Clone)]
struct Instance {
    component_type: ComponentType,
    child: Map<String, Instance>,
    children: Map<String, Vec<Instance>>,
    id: Option<String>,
    parameters: Map<String, Quantity>,
    attributes: Map<String, String>,
}

impl Instance {
    fn new(lems: &Lems, xml: &Node) -> Result<Self> {
        let node = xml.attribute("type").unwrap_or_else(|| xml.tag_name().name());
        let component_type = lems.compose_component_type(node)?;

        let mut attributes = Map::new();
        let mut parameters = Map::new();

        for attr in xml.attributes() {
            let key = attr.name().to_string();
            let val = attr.value();
            if component_type.parameters.contains(&key) {
                parameters.insert(key, Quantity::parse(val)?);
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

#[derive(Parser)]
#[clap(version="0.0.1", author="t.hater@fz-juelich.de")]
struct Options {
    /// Path to NMLCoreTypes
    #[clap(short, long, default_value="ext/NeuroML2/NeuroML2CoreTypes")]
    include_dir: String,
    /// Toplevel CoreType definition file
    #[clap(short, long, default_value="NeuroML2CoreTypes.xml")]
    core: String,
    /// A NeuroML2 compliant XML file
    nml: String,
    /// Base class to extract
    #[clap(short, long, default_value="ionChannelHH")]
    r#type: String,
    /// Output file
    #[clap(short, long)]
    output: Option<String>,
    /// Parameters to be retained as settable NIY
    #[clap(short, long)]
    parameters: Vec<String>,
}

fn main() -> Result<()> {
    let opts = Options::parse();
    let lems = Lems::from_file(&opts.include_dir, &opts.core)?;
    let xml  = std::fs::read_to_string(&opts.nml).map_err(|_| "File not found")?;
    let tree = roxmltree::Document::parse(&xml).map_err(|_| "Could not parse")?;
    let node = tree.descendants()
                   .find(|n| n.tag_name().name() == opts.r#type)
                   .ok_or(format!("Doc does not contain instances of {}", &opts.r#type))?;
    let mut instance = Instance::new(&lems, &node)?;

    // do fixes for known types
    match opts.r#type.as_ref() {
        "gapJunction" => {
            // Gap Junctions need peer voltage, which is provided by Arbor
            instance.component_type.variables.retain(|v| v.name != "vpeer");
            instance.component_type.variables.push( Variable { name: String::from("vpeer"),
                                                               exposure: None,
                                                               dimension: String::from("voltage"),
                                                               kind: VarKind::Derived(Vec::new(),
                                                                                      Some(Expr::parse("v_peer")?))});
            instance.component_type.parameters.push(String::from("weight"));
            instance.parameters.insert(String::from("weight"), Quantity::parse("1")?);
        }
        "ionChannel" | "ionChannelHH" => {
            let ion = instance.attributes.get("species").cloned().unwrap_or_else(String::new);
            let current = format!("g*(v - e{})", ion);
            instance.component_type.variables.push( Variable { name: format!("i{}", ion),
                                                               exposure: None,
                                                               dimension: String::from("current"),
                                                               kind: VarKind::Derived(Vec::new(),
                                                                                      Some(Expr::parse(&current)?)),
            });
        }
        _ => {}
    }



    let nmodl = Collapsed::from_instance(&instance)?.simplify().to_nmodl()?;
    if let Some(file) = opts.output {
        write(&file, nmodl).map_err(|_| "Error writing output to NMODL.")?;
    } else {
        print!("{}", nmodl);
    };
    eprintln!("{:?}", opts.parameters);
    Ok(())
}
