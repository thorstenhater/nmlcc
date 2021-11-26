use std::collections::HashMap as Map;
use std::collections::HashSet as Set;
use roxmltree::Node;

mod xml;
mod lems;
mod expr;

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
    State(Option<expr::Expr>, Option<expr::Expr>),
    /// cases=[condition => expression] default=expression
    Derived(Vec<(expr::Expr, expr::Expr)>, Option<expr::Expr>),
    /// Select one or more fields from children. NOTE will be gone after collapsing
    Select(SelectBy, expr::Match),
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
    constants: Map<String, expr::Quantity>,
}

/// Helper: Process Dynamics part of ComponentType
fn lems_dynamics(dynamics: &lems::raw::Dynamics, variables: &mut Vec<Variable>) -> Result<()> {
    use lems::raw::DynamicsBody::*;
    use lems::raw::OnStartBody::*;
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
                    VarKind::Select(by, expr::Match::parse(s)?)
                } else if let Some(e) = v.value.as_ref() {
                    VarKind::Derived(Vec::new(), Some(expr::Expr::parse(e)?))
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
                    let e = expr::Expr::parse(&c.value)?;
                    if let Some(b) = c.condition.as_ref() {
                        cs.push((expr::Expr::parse_bool(b)?, e));
                    } else {
                        df = Some(e);
                    }
                }
                variables.push(Variable::new(&v.name, &v.exposure, &v.dimension, &VarKind::Derived(cs, df)));
            }
            OnStart(v) =>
                for StateAssignment(a) in &v.body {
                    let it = variables.iter_mut().find(|x| x.name == a.variable);
                    if let Some(Variable { kind: VarKind::State(ref mut i, _), ..}) = it {
                        *i = Some(expr::Expr::parse(&a.value)?);
                    } else {
                        return Err(format!("Must be a StateVar: {}", a.variable));
                    }
                }
            TimeDerivative(v) => {
                let it = variables.iter_mut().find(|x| x.name == v.variable);
                if let Some(Variable { kind: VarKind::State(_, ref mut d), ..}) = it {
                    *d = Some(expr::Expr::parse(&v.value)?);
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

        for ix in &ct.body {
            use lems::raw::ComponentTypeBody::*;
            match ix {
                Child(c)     => { child.insert(c.name.to_string(), c.r#type.to_string()); }
                Children(c)  => { children.insert(c.name.to_string(), c.r#type.as_ref().unwrap().to_string()); }
                Parameter(p) => { parameters.push(p.name.to_string()); }
                Constant(c)  => { constants.insert(c.name.to_string(), expr::Quantity::parse(&c.value)?); }
                Exposure(e)  => { exposures.insert(e.name.to_string(), e.dimension.to_string());  }
                Text(t)      => { attributes.push(t.name.to_string()); }
                Dynamics(d)  => { lems_dynamics(d, &mut variables)?; },
                _ => {}
            }
        }
        Ok(Self { name, base, child, children, exposures, variables, constants, parameters, attributes })
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
    constants: Map<String, expr::Quantity>,
    parameters: Map<String, Option<expr::Quantity>>,
    attributes: Map<String, Option<String>>,
}

/// Map variables to dependencies
fn find_dependencies(variables: &[Variable]) -> Map<String, Set<String>> {
    let mut deps = Map::new();
    let add_var = |e: &expr::Expr, acc: &mut Set<String>| if let expr::Expr::Var(v) = e { acc.insert(v.to_string()); };
    for v in variables {
        let u = deps.entry(v.name.to_string()).or_insert_with(Set::new);
        match &v.kind {
            VarKind::State(it, dv) => {
                if let Some(i) = it.as_ref() { expr::fold_leaves(i, u, &add_var) }
                if let Some(d) = dv.as_ref() { expr::fold_leaves(d, u, &add_var) }
            }
            VarKind::Derived(cs, df) => {
                if let Some(d) = df.as_ref() { expr::fold_leaves(d, u, &add_var) }
                for (c, x) in cs {
                    expr::fold_leaves(c, u, &add_var);
                    expr::fold_leaves(x, u, &add_var);
                }
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

fn print_dependencies(roots: &[String], vars: &[Variable], known: &Set<String>) -> Result<()> {
    let dependencies = find_dependencies(vars);
    let mut deps = sorted_dependencies_of(roots, &dependencies, known)?;
    println!("  LOCAL {}", deps.join(", "));
    println!();
    deps.extend(roots.iter().cloned());

    for d in deps {
        match vars.iter().find(|k| k.name == d) {
            Some(Variable{ kind: VarKind::Derived(cs, df), .. }) => {
                let mut depth = 0;
                for (b, e) in cs {
                    if depth != 0 { println!("  else {{"); }
                    println!("  if ({}) {{ {} = {} }}", b.print_to_string(), d, e.print_to_string());
                    depth += 1;
                }
                if let Some(e) = df {
                    if depth != 0 { println!("  else {{"); }
                    println!("  {} = {}", d, e.print_to_string());
                    if depth != 0 { println!("  }}"); }
                }
                for _ in 0..depth-1 { println!("  }}"); }
            }
            Some(Variable{ kind: VarKind::State(Some(x), None), .. }) => { println!("  {} = {}",  d, x.print_to_string()); }
            Some(Variable{ kind: VarKind::State(None, Some(x)), .. }) => { println!("  {}' = {}", d, x.print_to_string()); }
            Some(e) => { return Err(format!("Don't know what to do with variable: {:?}", e)); }
            None => { return Err(format!("No such variable: {}", d)); }
        }
    }
    Ok(())
}

impl Collapsed {
    fn new(nm: &Option<String>) -> Self {
        Collapsed { name: nm.clone(), exposures: Map::new(), variables: Vec::new(), constants: Map::new(), parameters: Map::new(), attributes: Map::new() }
    }

    fn from_instance(inst: &Instance) -> Result<Self> { Self::from_instance_(inst, &Context::new(), None) }

    fn from_instance_(inst: &Instance, ctx: &Context, name: Option<String>) -> Result<Self> {
        let mut result = Collapsed::new(&inst.id);

        let mut ctx = ctx.clone();
        ctx.enter(inst.id.as_deref().or_else(|| name.as_deref()).unwrap(),
                  &inst.component_type.exposures.keys()
                       .chain(inst.component_type.parameters.iter())
                       .chain(inst.component_type.constants.keys())
                       .chain(inst.component_type.variables.iter().map(|v| &v.name))
                       .cloned()
                       .collect::<Vec<_>>()[..]);
        result.exposures  = inst.component_type.exposures.iter()
                                                         .map(|(k, v)| (ctx.add_prefix(k), v.clone()))
                                                         .collect();
        result.constants  = inst.component_type.constants.iter()
                                                         .map(|(k, v)| (ctx.add_prefix(k), v.clone()))
                                                         .collect();
        result.parameters = inst.component_type.parameters.iter()
                                                          .map(|k| (ctx.add_prefix(k), inst.parameters.get(k).cloned()))
                                                          .collect();
        result.attributes = inst.component_type.attributes.iter()
                                                          .map(|k| (ctx.add_prefix(k), inst.attributes.get(k).cloned()))
                                                          .collect();

        for v in inst.component_type.variables.clone() {
            let name      = ctx.add_prefix(&v.name);
            let exposure  =  v.exposure.map(|s| ctx.add_prefix(&s));
            let mut kind  = v.kind.clone(); ctx.rename_kind(&mut kind);
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
                                                      .map(|m| expr::Expr::Var(m.to_string()))
                                                      .collect::<Vec<expr::Expr>>();
                let kind = match &by {
                    SelectBy::Get => if let [ref n] = ms[..] {
                        n.clone()
                    } else {
                        panic!("Required field is not found for {:?} in {:?}", ps, result.exposures);
                    },
                     // TODO _technically_ we don't not need to guard ./. empty here, it will be squashed by simplify
                    SelectBy::Product => if ms.is_empty() { expr::Expr::F64(1.0) } else { expr::Expr::Mul(ms) },
                    SelectBy::Sum     => if ms.is_empty() { expr::Expr::F64(0.0) } else { expr::Expr::Add(ms) },
                };
                v.kind = VarKind::Derived(Vec::new(), Some(kind));
            }
        }
        ctx.exit();
        Ok(result)
    }

    fn add(&mut self, inst: &Instance, ctx: &Context, name: Option<String>) -> Result<()> {
        let other = Self::from_instance_(inst, ctx, name)?;
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
                if let Some(expr::Quantity {value: x, unit: u}) = v {
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
            let mut table: Map<String, expr::Expr> = Map::new();

            // NOTE(TH): this seems overly aggressive
            // for (p, v) in &self.parameters {
                // if let Some(expr::Quantity{ value, ..}) = v {
                    // table.insert(p, value);
                // }
            // }

            for v in &prv.variables {
                if let VarKind::Derived(cs, Some(k)) = &v.kind {
                    if cs.is_empty() && matches!(k, expr::Expr::F64(_) | expr::Expr::Var(_)) {
                            table.insert(v.name.to_string(), k.clone());
                    }
                }
            }

            let mut cur = prv.clone();
            let splat = |v: &expr::Expr| {
                if let expr::Expr::Var(n) = v {
                    if let Some(x) = table.get(n) { return x.clone(); }
                }
                v.clone()
            };

            for v in cur.variables.iter_mut() {
                match &v.kind {
                    VarKind::State(i, d) => {
                        let i = i.as_ref().map(|e| expr::map_leaves(e, &splat).simplify());
                        let d = d.as_ref().map(|e| expr::map_leaves(e, &splat).simplify());
                        v.kind = VarKind::State(i, d);
                    }
                    VarKind::Derived(cs, df) => {
                        let cs = cs.iter()
                                   .map(|(c, e)| (expr::map_leaves(c, &splat).simplify(), expr::map_leaves(e, &splat).simplify()))
                                   .collect::<Vec<_>>();
                        let df = df.as_ref().map(|e| expr::map_leaves(e, &splat).simplify());
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

    fn to_nmodl(&self) -> Result<()> {
        let mut state   = Vec::new();        // State variable names
        let mut state_i = Vec::new();        // State initialisation
        let mut state_d = Vec::new();        // State derivatives
        let mut deriv   = Vec::new();        // Derived variables

        for var in &self.variables {
            match &var.kind {
                VarKind::State(i, d) => {
                    if !state.contains(&var.name) { state.push(var.name.to_string()); }
                    if d.is_some() {
                        let mut v = var.clone();
                        v.kind = VarKind::State(None, d.clone());
                        state_d.push(v);
                    }
                    if i.is_some() {
                        let mut v = var.clone();
                        v.kind = VarKind::State(i.clone(), None);
                        state_i.push(v);
                    }
                }
                VarKind::Derived(_, _) => deriv.push(var.clone()),
                VarKind::Select(_, _) => return Err(format!("Select variable in {} post flattening stage.", var.name)),
            }
        }

        // Variables we can access everywhere: parameters, constants, state
        let known = self.parameters.iter().map(|p| p.0.to_string())
                        .chain(self.constants.iter().map(|p|p.0.to_string()))
                        .chain(state.iter().cloned())
                        .chain(std::iter::once("v".to_string()))
                        .collect::<Set<_>>();

        let pfx = self.name.as_ref().unwrap().to_string();
        let ion = self.attributes.get(&format!("{}_species", pfx)).cloned().flatten().unwrap_or_else(|| "l".to_string());
        let cod = format!("{}_g", pfx);
        println!("NEURON {{");
        println!("  SUFFIX {}", self.name.as_ref().unwrap());
        println!("  USEION {} READ e{} WRITE i{}", ion, ion, ion);
        println!("}}\n");

        println!("STATE {{");
        for v in &state_d { println!("  {}", v.name); }
        println!("}}\n");


        println!("PARAMETER {{");
        for (k, v) in &self.parameters {
            if let Some(v) = v {
                println!("  {} = {}", k, v.value);
            } else {
                println!("  {}", k);
            }
        }
        println!("}}\n");

        println!("INITIAL {{");
        let init = state_i.iter().map(|v| v.name.to_string()).collect::<Vec<_>>();
        let deps = deriv.iter().chain(state_d.iter()).cloned().collect::<Vec<_>>();
        print_dependencies(&init, &deps, &known)?;
        println!("}}\n");

        println!("DERIVATIVE dstate {{");
        let init = state_d.iter().map(|v| v.name.to_string()).collect::<Vec<_>>();
        let deps = deriv.iter().chain(state_d.iter()).cloned().collect::<Vec<_>>();
        print_dependencies(&init, &deps, &known)?;
        println!("}}\n");

        println!("BREAKPOINT {{");
        println!("  SOLVE dstate METHOD cnexp");
        print_dependencies(&[cod.to_string()], &deriv, &known)?;
        println!("  i{} = {}*(v - e{})", ion, cod, ion);
        println!("}}\n");
        Ok(())
    }
}

/// Stacked contexts of local symbols
#[derive(Debug, Clone)]
struct Context(Vec<(String, Vec<String>)>);

impl Context {
    fn new() -> Self { Context(Vec::new()) }
    fn enter(&mut self, name: &str, vars: &[String]) { self.0.push((name.to_string(), vars.to_vec())); }
    fn exit(&mut self) { self.0.pop(); }
    fn keys(&self) -> Vec<String> { self.0.iter().map(|t| t.0.clone()).collect() }

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

    fn rename_kind(&self, kind: &mut VarKind) {
        let rename = |v: &mut expr::Expr| {
            *v = expr::map_leaves(v, &|e| if let expr::Expr::Var(s) = e {
                let n = self.rename(s);
                expr::Expr::Var(n)
            } else {
                e.clone()
            })
        };
        let pfx = self.0.iter().map(|p| p.0.to_string()).collect::<Vec<_>>();
        match kind {
            VarKind::Select(_, ref mut ps)  => { ps.add_prefix(&pfx); }
            VarKind::Derived(ref mut cs, ref mut df) => {
                if let Some(x) = df.as_mut() { rename(x) }
                cs.iter_mut().for_each(|(ref mut c, ref mut e)| { rename(c); rename(e); });
            }
            VarKind::State(ref mut i, ref mut d) => {
                if let Some(x) = d.as_mut() { rename(x) }
                if let Some(x) = i.as_mut() { rename(x) }
            }
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
    parameters: Map<String, expr::Quantity>,
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
                parameters.insert(key, expr::Quantity::parse(val)?);
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

struct Options {

}

fn main() -> Result<()> {
    let lems = Lems::from_file("ext/NeuroML2/NeuroML2CoreTypes", "NeuroML2CoreTypes.xml")?;
    let path = std::env::args().nth(1).ok_or("Usage: ./lems <file.nml>".to_string())?;
    let xml  = std::fs::read_to_string(path).map_err(|_| "File not found")?;
    let tree = roxmltree::Document::parse(&xml).map_err(|_| "Could not parse")?;
    let name = "ionChannelHH";
    let node = tree.descendants()
                   .find(|n| n.tag_name().name() == name)
                   .ok_or(format!("Doc does not contain {}", name))?;
    let instance = Instance::new(&lems, &node)?;
    Collapsed::from_instance(&instance)?.simplify().to_nmodl()
}
