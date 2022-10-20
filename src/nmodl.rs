use std::path::PathBuf;
use std::{fs::write, iter::once};
use tracing::{info, trace, warn};

use crate::{
    error::{Error, Result},
    expr::{Expr, Quantity, Stmnt},
    instance::{Collapsed, Instance},
    lems::file::LemsFile,
    neuroml::process_files,
    nml2_error,
    variable::VarKind,
    Map, Set,
};

fn nmodl_error<T: Into<String>>(what: T) -> Error {
    Error::Nmodl { what: what.into() }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    Density,
    Point,
    Junction,
}

#[derive(Debug, Clone)]
pub struct Nmodl {
    /// List of known ions, defaults to Na, K, Ca
    pub known_ions: Vec<String>,
    /// Kind
    pub kind: Kind,
    /// Suffix
    suffix: String,
    /// Declared (but undefined) variables
    symbols: Set<String>,
    /// Settable constants
    constants: Map<String, Quantity>,
    /// Settable parameters, with optional defaults
    parameters: Map<String, Option<Quantity>>,
    /// State variables
    state: Set<String>,
    /// State initialisation
    init: Map<String, Stmnt>,
    /// State derivatives
    deriv: Map<String, Stmnt>,
    /// Output for BREAK block
    pub outputs: Map<String, Stmnt>,
    /// Variables
    pub variables: Map<String, Stmnt>,
    /// Ionic species
    species: Vec<String>,
    /// Transition matrix
    transitions: Vec<(String, String, String, String)>,
    /// Event processing
    events: Map<String, Stmnt>,
    /// Kinetic system transition rates
    rates: Map<String, Stmnt>,
    states: Vec<Set<String>>,
    fixed: Map<String, Expr>,
    keep: Set<String>,
    conditions: Map<String, Vec<Stmnt>>,
}

fn apply_filter(filter: &str, keys: &Set<String>) -> Set<String> {
    let mut retain = Set::new();
    for f in filter.split(',') {
        if let Some(f) = f.strip_prefix('+') {
            if f.ends_with('*') {
                let keep = keys
                    .iter()
                    .filter(|p| p.starts_with(&f[..f.len() - 1]))
                    .map(|p| p.to_string())
                    .collect::<Set<_>>();
                retain = retain.union(&keep).cloned().collect();
            } else {
                retain.insert(f.to_string());
            }
        } else if let Some(f) = f.strip_prefix('-') {
            if f.ends_with('*') {
                let keep = keys
                    .iter()
                    .filter(|p| p.starts_with(&f[..f.len() - 1]))
                    .map(|p| p.to_string())
                    .collect::<Set<_>>();
                retain = retain.difference(&keep).cloned().collect();
            } else {
                retain.remove(&f.to_string());
            }
        } else {
            panic!("Unknown filter kind: {}", f);
        }
    }
    retain
}

impl Nmodl {
    pub fn from(coll: &Collapsed, known_ions: &[String], filter: &str) -> Result<Self> {
        let known_ions = known_ions.to_vec();
        let kind = Kind::Density;
        let suffix = coll
            .name
            .as_deref()
            .ok_or_else(|| nml2_error!("Unnamed channel."))?
            .to_string();
        let species = ion_species(coll);
        let mut outputs = Map::new();
        let mut events: Map<String, Stmnt> = coll
            .events
            .iter()
            .map(|(k, e)| (k.to_string(), Stmnt::Ass(k.to_string(), e.clone())))
            .collect();
        let mut state = Set::new();
        let mut init = Map::new();
        let mut deriv = Map::new();
        let mut variables = Map::new();
        let mut conditions = Map::new();
        let mut rates = Map::new();
        for var in &coll.variables {
            let nm = var.name.to_string();
            if let VarKind::State(i, d) = &var.kind {
                state.insert(var.name.clone());
                if let Some(i) = i {
                    init.insert(nm.to_string(), Stmnt::Ass(nm.clone(), i.clone()));
                }
                if let Some(d) = d {
                    let dnm = format!("{}'", nm);
                    deriv.insert(dnm.to_string(), Stmnt::Ass(dnm.clone(), d.clone()));
                }
            }
        }

        for var in &coll.variables {
            if let VarKind::Derived(cs, df) = &var.kind {
                let nm = var.name.to_string();
                let mut init = false;
                let d = if let Some(d) = df {
                    init = true;
                    d.clone()
                } else {
                    warn!("Variable '{}' default case undefined, assuming zero.", nm);
                    Expr::F64(0.0)
                };
                let mut res = Stmnt::Ass(nm.clone(), d);
                let mut cs = cs.clone();
                while let Some((c, e)) = cs.pop() {
                    init = true;
                    res = Stmnt::Ift(c, Box::new(Stmnt::Ass(nm.clone(), e)), Box::new(Some(res)));
                }
                if !init {
                    return Err(nml2_error!("Variable '{}' undefined.", nm));
                }
                variables.insert(nm, res);
            }
        }

        let mut fixed = Map::new();
        let mut constants = Map::new();
        let mut parameters = Map::new();
        let keep = apply_filter(filter, &coll.constants.keys().cloned().collect::<Set<_>>());
        for (k, v) in &coll.constants {
            if keep.contains(k) {
                constants.insert(k.to_string(), v.clone());
            } else {
                fixed.insert(k.to_string(), Expr::F64(v.value));
            }
        }
        let keep = apply_filter(filter, &coll.parameters.keys().cloned().collect::<Set<_>>());
        for (k, v) in &coll.parameters {
            if keep.contains(k) || v.is_none() {
                parameters.insert(k.to_string(), v.clone());
            } else {
                fixed.insert(k.to_string(), Expr::F64(v.as_ref().unwrap().value));
            }
        }

        let mut table = Map::new();
        for (from, to, fwd, bwd) in &coll.transitions {
            // Swap if needed, since rates are symmetric
            let (key, fwd, bwd) = if from > to {
                ((from, to), fwd, bwd)
            } else {
                ((to, from), bwd, fwd)
            };
            let fwd = Expr::parse(fwd)?;
            let bwd = Expr::parse(bwd)?;
            // Rates are additive
            table
                .entry(key)
                .and_modify(|(f, b): &mut (Expr, Expr)| {
                    *f = Expr::Add(vec![fwd.clone(), f.clone()]).simplify();
                    *b = Expr::Add(vec![bwd.clone(), b.clone()]).simplify();
                })
                .or_insert((fwd, bwd));
        }
        let mut transitions = Vec::new();
        for ((from, to), (fwd, bwd)) in table.into_iter() {
            let pfx = from
                .chars()
                .zip(to.chars())
                .take_while(|(a, b)| a == b)
                .map(|t| t.0)
                .collect::<String>();
            let f = from.strip_prefix(&pfx).unwrap_or(from);
            let t = to.strip_prefix(&pfx).unwrap_or(to);
            let fname = format!("{}{}_to_{}", pfx, f, t);
            let bname = format!("{}{}_to_{}", pfx, t, f);
            let fwd = Stmnt::Ass(fname.clone(), fwd);
            let bwd = Stmnt::Ass(bname.clone(), bwd);
            rates.insert(fname.clone(), bwd.clone());
            rates.insert(bname.clone(), fwd.clone());
            transitions.push((
                from.to_string(),
                to.to_string(),
                fname.to_string(),
                bname.to_string(),
            ));
        }

        variables.extend([assign("caConc", "cai")?, assign("vpeer", "v_peer")?].into_iter());

        let mut symbols: Set<_> = [
            String::from("v"),
            String::from("area"),
            String::from("diam"),
            String::from("v_peer"),
            String::from("cai"),
        ]
        .into_iter()
        .collect();
        for ion in &species {
            if !ion.is_empty() {
                symbols.insert(format!("e{}", ion));
                symbols.insert(format!("i{}", ion));
                symbols.insert(format!("{}i", ion));
                symbols.insert(format!("{}o", ion));
            }
        }
        symbols.extend(parameters.iter().map(|p: (&String, _)| p.0.to_string()));
        symbols.extend(constants.iter().map(|p: (&String, _)| p.0.to_string()));
        symbols.extend(state.iter().cloned());

        let keep = outputs
            .keys()
            .chain(outputs.keys())
            .chain(deriv.keys())
            .chain(init.keys())
            .chain(events.keys())
            .chain(rates.keys())
            .chain(symbols.iter())
            .cloned()
            .collect();

        simplify(&mut variables, &mut fixed, &keep);
        simplify(&mut outputs, &mut fixed, &keep);
        simplify(&mut init, &mut fixed, &keep);
        simplify(&mut deriv, &mut fixed, &keep);
        simplify(&mut events, &mut fixed, &keep);
        simplify(&mut rates, &mut fixed, &keep);

        for (nm, ts, ex) in &coll.conditions {
            let ass = Stmnt::Ift(
                ts.clone(),
                Box::new(Stmnt::Ass(nm.clone(), ex.clone())),
                Box::new(None),
            );
            conditions
                .entry(nm.clone())
                .or_insert_with(Vec::new)
                .push(ass);
        }

        let subs = |k: &String| -> String { k.replace('/', "_") };

        let symbols = symbols.iter().map(subs).collect();
        let constants = constants
            .iter()
            .map(|(k, v)| (subs(k), v.clone()))
            .collect();
        let parameters = parameters
            .iter()
            .map(|(k, v)| (subs(k), v.clone()))
            .collect();
        let state = state.iter().map(subs).collect();
        let init = init
            .iter()
            .map(|(k, v)| (subs(k), v.rename(&subs)))
            .collect();
        let deriv = deriv
            .iter()
            .map(|(k, v)| (subs(k), v.rename(&subs)))
            .collect();
        let outputs = outputs
            .iter()
            .map(|(k, v)| (subs(k), v.rename(&subs)))
            .collect();
        let variables = variables
            .iter()
            .map(|(k, v)| (subs(k), v.rename(&subs)))
            .collect();
        let species = species.iter().map(|k| k.replace('/', "_")).collect();
        let transitions = transitions
            .iter()
            .map(|(a, b, c, d)| (subs(a), subs(b), subs(c), subs(d)))
            .collect();
        let events = events
            .iter()
            .map(|(k, v)| (subs(k), v.rename(&subs)))
            .collect();
        let rates = rates
            .iter()
            .map(|(k, v)| (subs(k), v.rename(&subs)))
            .collect();
        let states = coll.states.to_vec();
        let fixed = fixed
            .iter()
            .map(|(k, v)| (subs(k), v.rename(&subs)))
            .collect();
        let keep = keep.iter().map(|k| k.replace('/', "_")).collect();
        let conditions = conditions
            .iter()
            .map(|(k, vs)| (subs(k), vs.iter().map(|v| v.rename(&subs)).collect()))
            .collect();

        Ok(Nmodl {
            known_ions,
            kind,
            suffix,
            symbols,
            constants,
            parameters,
            state,
            init,
            deriv,
            variables,
            species,
            outputs,
            events,
            rates,
            transitions,
            states,
            fixed,
            keep,
            conditions,
        })
    }

    pub fn add_variables(&mut self, rhs: &Map<String, Stmnt>) {
        for (k, v) in rhs {
            let k = k.replace('/', "_");
            let v = v.rename(&|k| k.replace('/', "_"));
            self.variables.insert(k.clone(), v);
            self.keep.insert(k);
        }
        simplify(&mut self.variables, &mut self.fixed, &self.keep);
    }

    pub fn add_outputs(&mut self, rhs: &Map<String, Stmnt>) {
        for (k, v) in rhs {
            let k = k.replace('/', "_");
            let v = v.rename(&|k| k.replace('/', "_"));
            self.outputs.insert(k.clone(), v);
            self.keep.insert(k);
        }
        simplify(&mut self.outputs, &mut self.fixed, &self.keep);
    }
    pub fn add_initials(&mut self, rhs: &Map<String, Stmnt>) {
        for (k, v) in rhs {
            let k = k.replace('/', "_");
            let v = v.rename(&|k| k.replace('/', "_"));
            self.init.insert(k.clone(), v);
            self.keep.insert(k);
        }
        simplify(&mut self.init, &mut self.fixed, &self.keep);
    }

    pub fn simplify(self) -> Self {
        // for efficiency, consume self
        Nmodl {
            known_ions: self.known_ions,
            kind: self.kind,
            suffix: self.suffix,
            symbols: self.symbols,
            constants: self.constants,
            parameters: self.parameters,
            state: self.state,
            init: self
                .init
                .iter()
                .map(|(k, v)| (k.into(), v.simplify()))
                .collect(),
            deriv: self
                .deriv
                .iter()
                .map(|(k, v)| (k.into(), v.simplify()))
                .collect(),
            outputs: self
                .outputs
                .iter()
                .map(|(k, v)| (k.into(), v.simplify()))
                .collect(),
            variables: self
                .variables
                .iter()
                .map(|(k, v)| (k.into(), v.simplify()))
                .collect(),
            species: self.species,
            transitions: self.transitions,
            events: self
                .events
                .iter()
                .map(|(k, v)| (k.into(), v.simplify()))
                .collect(),
            rates: self
                .rates
                .iter()
                .map(|(k, v)| (k.into(), v.simplify()))
                .collect(),
            states: self.states,
            fixed: self.fixed,
            keep: self.keep,
            conditions: self
                .conditions
                .iter()
                .map(|(k, vs)| (k.into(), vs.iter().map(|v| v.simplify()).collect()))
                .collect(),
        }
    }
}

pub fn mk_nmodl(n: Nmodl) -> Result<String> {
    let n = &n.simplify();
    Ok(vec![
        nmodl_neuron_block(n)?,
        nmodl_const_block(n)?,
        nmodl_param_block(n)?,
        nmodl_state_block(n)?,
        nmodl_init_block(n)?,
        nmodl_deriv_block(n)?,
        nmodl_break_block(n)?,
        nmodl_recv_block(n)?,
        nmodl_kinetic_block(n)?,
    ]
    .join(""))
}

fn nmodl_state_block(n: &Nmodl) -> Result<String> {
    if !n.state.is_empty() {
        Ok(format!(
            "STATE {{ {} }}\n\n",
            n.state.iter().cloned().collect::<Vec<_>>().join(" ")
        ))
    } else {
        Ok(String::new())
    }
}

fn nmodl_init_block(n: &Nmodl) -> Result<String> {
    if n.init.is_empty() && n.transitions.is_empty() {
        return Ok(String::new());
    }
    let vars = n
        .variables
        .iter()
        .chain(n.init.iter())
        .map(|(a, b)| (a.clone(), b.clone()))
        .collect();
    let roots = n.init.keys().cloned().collect::<Vec<String>>();
    let init = n
        .init
        .values()
        .map(|s| s.print_to_string(2))
        .collect::<Vec<String>>()
        .join("\n");
    let mut result = vec![
        String::from("INITIAL {"),
        print_dependency_chains(&roots, &vars, &n.symbols)?,
        init,
        if !n.transitions.is_empty() {
            String::from("  SOLVE scheme STEADYSTATE sparse")
        } else {
            String::new()
        },
        String::from("}\n\n"),
    ];
    result.retain(|s| !s.is_empty());
    Ok(result.join("\n"))
}

fn nmodl_recv_block(n: &Nmodl) -> Result<String> {
    if n.events.is_empty() {
        return Ok(String::new());
    }
    let evts = n
        .events
        .iter()
        .map(|v| v.1.print_to_string(2))
        .collect::<Vec<_>>()
        .join("\n");
    let vars = n
        .variables
        .iter()
        .chain(n.events.iter())
        .map(|(a, b)| (a.clone(), b.clone()))
        .collect();
    let roots = n.events.keys().cloned().collect::<Vec<_>>();
    let syms = n
        .symbols
        .iter()
        .cloned()
        .chain(once(String::from("weight")))
        .collect();
    let mut result = vec![
        String::from("NET_RECEIVE(weight) {"),
        print_dependency_chains(&roots, &vars, &syms)?,
        evts,
        String::from("}\n\n"),
    ];
    result.retain(|s| !s.is_empty());
    Ok(result.join("\n"))
}

fn nmodl_deriv_block(n: &Nmodl) -> Result<String> {
    if n.deriv.is_empty() {
        return Ok(String::new());
    }
    let roots = n.deriv.keys().cloned().collect::<Vec<String>>();
    let vars = n
        .variables
        .iter()
        .chain(n.deriv.iter())
        .map(|(a, b)| (a.clone(), b.simplify()))
        .collect::<Map<String, Stmnt>>();
    let mut result = String::from("DERIVATIVE dstate {\n");
    let ls = print_dependency_chains(&roots, &vars, &n.symbols)?;
    if !ls.is_empty() {
        result.push_str(&ls);
        result.push('\n');
    }
    for d in n.deriv.values() {
        result.push_str(&d.print_to_string(2));
        result.push('\n');
    }
    result.push_str("}\n\n");
    Ok(result)
}

fn nmodl_break_block(n: &Nmodl) -> Result<String> {
    if !n.deriv.is_empty() && !n.transitions.is_empty() {
        return Err(nmodl_error("Both KINETIC and ODEs given"));
    }
    let mut result = vec![String::from("BREAKPOINT {")];
    if !n.deriv.is_empty() {
        result.push(String::from("  SOLVE dstate METHOD cnexp"));
    }
    if !n.transitions.is_empty() {
        result.push(String::from("  SOLVE scheme METHOD sparse"));
    }

    let vars = n
        .variables
        .iter()
        .chain(n.outputs.iter())
        .map(|(a, b)| (a.clone(), b.clone()))
        .collect::<Map<String, Stmnt>>();
    let roots = n.outputs.keys().cloned().collect::<Vec<String>>();
    let currents = n
        .outputs
        .values()
        .map(|s| s.print_to_string(2))
        .collect::<Vec<String>>()
        .join("\n");
    let deps = print_dependency_chains(&roots, &vars, &n.symbols)?;
    if !deps.is_empty() {
        result.push(deps);
    }
    for vs in n.conditions.values() {
        for v in vs {
            result.push(v.print_to_string(2));
        }
    }
    result.push(currents);
    result.push(String::from("}\n\n"));
    Ok(result.join("\n"))
}

fn nmodl_param_block(n: &Nmodl) -> Result<String> {
    let mut ps = n.parameters.clone();
    let read = read_variable(n)?;
    for p in ["diam"] {
        if read.contains(p) {
            ps.insert(p.to_string(), None);
        }
    }
    if ps.is_empty() {
        return Ok(String::new());
    }
    let mut result = vec![String::from("PARAMETER {")];
    for (k, v) in ps.into_iter() {
        let mut ln = format!("  {}", k);
        if let Some(v) = v {
            ln.push_str(&format!(" = {}", v.value));
            if let Some(u) = v.unit.as_ref() {
                ln.push_str(&format!(" ({})", u));
            }
        }
        result.push(ln);
    }
    result.push(String::from("}\n\n"));
    Ok(result.join("\n"))
}

fn nmodl_const_block(n: &Nmodl) -> Result<String> {
    if n.constants.is_empty() {
        return Ok(String::new());
    }
    let mut result = vec![String::from("CONSTANT {")];
    for (k, v) in &n.constants {
        let mut ln = format!("  {} = {}", k, v.value);
        if let Some(u) = v.unit.as_ref() {
            ln.push_str(&format!(" ({})", u));
        }
        result.push(ln);
    }
    result.push(String::from("}\n\n"));
    Ok(result.join("\n"))
}

fn nmodl_kinetic_block(n: &Nmodl) -> Result<String> {
    if n.transitions.is_empty() {
        return Ok(String::new());
    }

    let vars = n
        .variables
        .iter()
        .chain(n.rates.iter())
        .map(|(a, b)| (a.clone(), b.clone()))
        .collect::<Map<String, Stmnt>>();

    let locals = n.rates.keys().cloned().collect::<Vec<_>>();
    let mut result = String::from("KINETIC scheme {\n");
    if !locals.is_empty() {
        result.push_str(&format!("LOCAL {}\n", locals.join(", ")));
    }
    let ch = print_dependency_chains(&locals, &vars, &n.symbols)?;
    if !ch.is_empty() {
        result.push_str(&ch);
        result.push('\n');
    }
    for rate in &n.rates {
        result.push_str(&rate.1.print_to_string(2));
        result.push('\n');
    }
    for (f, t, rf, rt) in &n.transitions {
        result.push_str(&format!("  ~ {} <-> {} ({}, {})\n", f, t, rf, rt));
    }
    for row in &n.states {
        let row = row.iter().cloned().collect::<Vec<_>>().join(" + ");
        result.push_str(&format!("  CONSERVE {} = 1\n", row));
    }
    result.push_str("}\n\n");
    Ok(result)
}

fn read_variable(n: &Nmodl) -> Result<Set<String>> {
    let variables = n
        .variables
        .iter()
        .chain(n.init.iter())
        .chain(n.rates.iter())
        .chain(n.deriv.iter())
        .chain(n.outputs.iter())
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect::<Map<_, _>>();
    let deps = find_dependencies(&variables);
    let mut todo = n
        .outputs
        .iter()
        .chain(n.init.iter())
        .chain(n.rates.iter())
        .chain(n.deriv.iter())
        .chain(n.outputs.iter())
        .map(|p| p.0.to_string())
        .collect::<Vec<_>>();
    let mut result = Set::new();
    while let Some(v) = todo.pop() {
        if let Some(us) = deps.get(&v) {
            for u in us {
                if !result.contains(u) {
                    todo.push(u.clone());
                }
                result.insert(u.clone());
            }
        }
    }
    Ok(result)
}

fn nmodl_neuron_block(n: &Nmodl) -> Result<String> {
    let read = read_variable(n)?;
    if read.contains("area") {
        return Err(nmodl_error(
            "'Area' is not supported in Arbor; check if the model can use 'diam' instead.",
        ));
    }
    let mut ions = n.species.iter().cloned().collect::<Set<_>>();
    ions.insert(String::from("ca"));

    let kind = match &n.kind {
        Kind::Density => "SUFFIX",
        Kind::Point => "POINT_PROCESS",
        Kind::Junction => "JUNCTION",
    };
    let mut result = vec![
        String::from("NEURON {\n"),
        format!("  {} {}\n", kind, n.suffix),
    ];
    let mut write = n.outputs.keys().cloned().collect::<Set<_>>();
    for k in n.deriv.keys() {
        let s = &k[..k.len() - 1];
        write.insert(s.to_string());
    }
    for ion in &ions {
        let xi = format!("{}i", ion);
        let ix = format!("i{}", ion);
        let xo = format!("{}o", ion);
        let ex = format!("e{}", ion);

        if n.known_ions.contains(ion) {
            let mut used = false;
            let mut rs = Vec::new();
            let mut ws = Vec::new();
            for q in [xi, xo, ex, ix] {
                let w = write.contains(&q);
                let r = read.contains(&q);
                used |= w || r;
                if w {
                    ws.push(q.clone());
                }
                if r && !w {
                    rs.push(q.clone());
                }
            }
            if used {
                let ws = if !ws.is_empty() {
                    format!(" WRITE {}", ws.join(", "))
                } else {
                    String::new()
                };
                let rs = if !rs.is_empty() {
                    format!(" READ {}", rs.join(", "))
                } else {
                    String::new()
                };
                let usage = format!("  USEION {}{}{}\n", ion, ws, rs);

                result.push(usage);
            }
        }
    }

    if n.kind == Kind::Density {
        for ion in &ions {
            let ix = format!("i{}", ion);
            if write.contains(&ix) && !n.known_ions.contains(ion) {
                result.push(format!("  NONSPECIFIC_CURRENT {}\n", ix));
            }
        }
    } else if n.kind == Kind::Junction || n.kind == Kind::Point {
        result.push(String::from("  NONSPECIFIC_CURRENT i\n"));
    }

    if !n.parameters.is_empty() {
        let rs = n.parameters.keys().cloned().collect::<Vec<_>>();
        result.push(format!("  RANGE {}\n", rs.join(", ")));
    }
    result.push(String::from("}\n\n"));
    Ok(result.join(""))
}

fn ion_species(coll: &Collapsed) -> Vec<String> {
    coll.attributes
        .iter()
        .filter_map(|(k, v)| {
            if k.ends_with("species") || k == "ion" {
                Some(v.as_deref().unwrap_or_default().to_string())
            } else {
                None
            }
        })
        .collect::<Set<_>>()
        .into_iter()
        .collect::<Vec<_>>()
}

/// Map variables to dependencies
fn find_dependencies(variables: &Map<String, Stmnt>) -> Map<String, Set<String>> {
    let mut deps = Map::new();
    let add_var = |e: &Expr, acc: &mut Set<String>| {
        if let Expr::Var(v) = e {
            acc.insert(v.to_string());
        }
    };

    for (k, v) in variables {
        let u = deps.entry(k.to_string()).or_insert_with(Set::new);
        v.fold(u, &add_var);
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
fn sorted_dependencies_of(
    start: &[String],
    deps: &Map<String, Set<String>>,
    known: &Set<String>,
) -> Result<Vec<String>> {
    // Build transitive dependencies
    let mut todo = Vec::new();
    let mut current = start.to_vec();
    while let Some(next) = current.pop() {
        let mut ds = deps.get(&next).unwrap_or(&Set::new()).clone();
        ds.remove(&next);
        for k in known.iter() {
            ds.remove(k);
        }
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
                if !result.contains(v) {
                    result.push(v.to_string());
                }
                seen.insert(v.to_string());
                todo.remove(ix);
                continue 'a;
            }
        }
        return Err(nmodl_error(format!(
            "Could not resolve variables {:?}",
            todo
        )));
    }
    Ok(result)
}

fn print_dependency_chains(
    roots: &[String],
    vars: &Map<String, Stmnt>,
    known: &Set<String>,
) -> Result<String> {
    let mut result = Vec::new();
    let dependencies = find_dependencies(vars);
    let deps = sorted_dependencies_of(roots, &dependencies, known)?;
    if !deps.is_empty() {
        result.push(format!("  LOCAL {}\n", deps.join(", ")));
    }
    for d in deps {
        if let Some(s) = vars.get(&d) {
            result.push(s.print_to_string(2));
        } else {
            let exprs = roots
                .iter()
                .map(|r| {
                    vars.get(r)
                        .map(|s| s.print_to_string(2))
                        .unwrap_or_default()
                })
                .collect::<Vec<_>>()
                .join("\n");
            warn!(
                "Unresolved variable {} resulting from {:?} which are\n{}",
                d, roots, exprs
            );
            result.push(format!("{} = ???", d));
        }
    }
    Ok(result.join("\n"))
}

pub fn to_nmodl(
    instance: &Instance,
    filter: &str,
    base: &str,
    known_ions: &[String],
) -> Result<String> {
    let ty: &str = instance.component_type.name.as_ref();
    match base {
        "baseSynapse" => {
            if ty == "gapJunction" {
                let mut filter = filter.to_string();
                let mut instance = instance.clone();
                if !filter.is_empty() {
                    filter.push(',');
                }
                filter.push_str("+weight,+conductance");
                // Gap Junctions need peer voltage, which is provided by Arbor
                instance
                    .component_type
                    .variables
                    .retain(|v| v.name != "vpeer");
                let mut coll = Collapsed::from_instance(&instance)?;
                coll.parameters
                    .insert(String::from("weight"), Some(Quantity::parse("1")?));
                let mut n = Nmodl::from(&coll, known_ions, &filter)?;
                // We know that we must write `i` and that it is in the variables
                if let Some((k, v)) = n.variables.remove_entry("i") {
                    n.outputs.insert(k, v);
                } else {
                    return Err(nmodl_error("Gap Junction without defined current 'i'"));
                }
                n.kind = Kind::Junction;
                mk_nmodl(n)
            } else {
                let filter = filter.to_string();
                let instance = instance.clone();
                let coll = Collapsed::from_instance(&instance)?;
                let mut n = Nmodl::from(&coll, known_ions, &filter)?;
                // We know that we must write `i` and that it is in the variables
                if let Some((k, v)) = n.variables.remove_entry("i") {
                    n.outputs.insert(k, v);
                } else {
                    return Err(nmodl_error("Synapse without defined current 'i'"));
                }
                n.kind = Kind::Point;
                mk_nmodl(n)
            }
        }
        "baseIonChannel" => {
            let mut filter = filter.to_string();
            let instance = instance.clone();
            if !filter.is_empty() {
                filter.push(',');
            }
            filter.push_str("+conductance");
            let coll = Collapsed::from_instance(&instance)?;
            let mut n = Nmodl::from(&coll, known_ions, &filter)?;
            for ion in &n.species {
                let ex = format!("e{}", ion);
                let gx = String::from("g");
                let ix = format!("i{}", ion);
                let xi = format!("{}i", ion);
                if known_ions.contains(ion) {
                    let (ki, xi) = assign(&format!("{}conc", ion), &xi)?;
                    n.variables.insert(ki, xi);
                } else {
                    filter.push_str(format!("+e{}", ion).as_str());
                    n.parameters
                        .insert(ex.clone(), Some(Quantity::parse("0 mV")?));
                    n.symbols.insert(ex.clone());
                }
                if !n.variables.contains_key(&gx) {
                    // to catch extensions of passiveChannels
                    let (g, v) = assign(&gx, "conductance")?;
                    n.variables.insert(g, v);
                }
                // NML ion channnels write out conductivities, but NMODL deals in currents.
                let (ik, ix) = assign(&ix, &format!("{}*(v - {})", gx, ex))?;
                n.outputs.insert(ik.clone(), ix);
                let (ki, xi) = assign(&format!("{}conc", ion), &xi)?;
                n.variables.insert(ki, xi);
            }
            n.kind = Kind::Density;
            mk_nmodl(n)
        }
        "concentrationModel" => {
            let mut filter = filter.to_string();
            let instance = instance.clone();
            let mut coll = Collapsed::from_instance(&instance)?;
            let ion = coll.attributes.get("ion").unwrap().as_deref().unwrap();
            if !filter.is_empty() {
                filter.push(',');
            }
            filter.push_str("+initialConcentration");
            coll.parameters.insert(
                String::from("initialConcentration"),
                Some(Quantity::parse("0 mM")?),
            );
            let mut n = Nmodl::from(&coll, known_ions, &filter)?;
            let xi = format!("{}i", ion);
            let xo = format!("{}o", ion);
            let dxi = format!("{}i'", ion);
            let dxo = format!("{}o'", ion);

            let ic = "concentration";
            let ec = "extConcentration";
            let dic = "concentration'";
            let dec = "extConcentration'";

            if !n.outputs.contains_key(ec) && !n.deriv.contains_key(dec) {
                n.init.remove(ec);
                n.state.remove(ec);
            }
            if !n.outputs.contains_key(ic) && !n.deriv.contains_key(dic) {
                n.init.remove(ic);
                n.state.remove(ic);
            }

            let ica = Expr::parse("-0.01*ica*surfaceArea")?;
            let fix = |ex: &Expr| -> Expr {
                match ex {
                    Expr::Var(x) if x == "iCa" => ica.clone(),
                    Expr::Var(x) if x == ic => Expr::Var(xi.clone()),
                    Expr::Var(x) if x == ec => Expr::Var(xo.clone()),
                    _ => ex.clone(),
                }
            };

            for stm in n.variables.values_mut() {
                *stm = stm.map(&fix);
            }
            for stm in n.deriv.values_mut() {
                *stm = stm.map(&fix);
            }
            for stm in n.init.values_mut() {
                *stm = stm.map(&fix);
            }
            for stm in n.outputs.values_mut() {
                *stm = stm.map(&fix);
            }
            for stms in n.conditions.values_mut() {
                for stm in stms {
                    *stm = stm.map(&fix);
                    *stm = stm.set_name(ic, &xi);
                }
            }
            if let Some(v) = n.outputs.remove(ic) {
                n.outputs.insert(xi.clone(), v.set_name(ic, &xi));
            }
            if let Some(v) = n.init.remove(ic) {
                n.init.insert(xi.clone(), v.set_name(ic, &xi));
            }
            if let Some(v) = n.deriv.remove(dic) {
                n.deriv.insert(dxi.clone(), v.set_name(dic, &dxi));
            }
            if let Some(vs) = n.conditions.remove(ic) {
                n.conditions.insert(xi.clone(), vs);
            }
            if let Some(v) = n.outputs.remove(ec) {
                n.outputs.insert(xo.clone(), v.set_name(ec, &xo));
            }
            if let Some(v) = n.init.remove(ec) {
                n.init.insert(xo.clone(), v.set_name(ec, &xo));
            }
            if let Some(v) = n.deriv.remove(dec) {
                n.deriv.insert(dxo.clone(), v.set_name(dec, &dxo));
            }
            if let Some(vs) = n.conditions.remove(ec) {
                n.conditions.insert(xo.clone(), vs);
            }
            if n.state.remove(ic) {
                n.state.insert(xi.clone());
            }
            if n.state.remove(ec) {
                n.state.insert(xo.clone());
            }
            mk_nmodl(n)
        }
        _ => Err(nmodl_error(format!(
            "Type {} deriving an expected base {}",
            ty, base
        ))),
    }
}

fn simplify(variables: &mut Map<String, Stmnt>, fixed: &mut Map<String, Expr>, keep: &Set<String>) {
    loop {
        let mut new = Map::new();
        for (k, v) in variables.iter() {
            match v {
                Stmnt::Ass(k, x @ Expr::F64(_)) if !keep.contains(k) => {
                    fixed.insert(k.clone(), x.clone());
                }
                Stmnt::Ass(k, x @ Expr::Var(_)) if !keep.contains(k) => {
                    fixed.insert(k.clone(), x.clone());
                }
                _ => {
                    new.insert(k.clone(), v.clone());
                }
            }
        }

        for v in new.values_mut() {
            let y = v
                .map(&|e| {
                    if let Expr::Var(n) = e {
                        if let Some(x) = fixed.get(n) {
                            x.clone()
                        } else {
                            e.clone()
                        }
                    } else {
                        e.clone()
                    }
                })
                .simplify();
            *v = y;
        }

        if new == *variables {
            break;
        }
        *variables = new;
    }
}

pub fn export(
    lems: &LemsFile,
    nml: &[String],
    filter: &str,
    cat: &str,
    known_ions: &[String],
) -> Result<()> {
    let tys = vec!["baseIonChannel", "baseSynapse", "concentrationModel"];

    process_files(nml, |_, node| {
        let tag = node.tag_name().name();
        for ty in &tys {
            if lems.derived_from(tag, ty) {
                let instance = Instance::new(lems, node)?;
                let mut path = PathBuf::from(&cat);
                if !path.exists() {
                    trace!("Creating path to {:?}", &path);
                    std::fs::create_dir_all(&path)?;
                }
                let file = instance.id.as_deref().ok_or(crate::error::Error::Nml {
                    what: String::from("Channel must have an id"),
                })?;
                path.push(file);
                path.set_extension("mod");
                info!(
                    "Writing NMODL for '{}' to {:?}",
                    instance.id.as_deref().unwrap(),
                    &path
                );
                write(&path, to_nmodl(&instance, filter, ty, known_ions)?)?;
            }
        }
        Ok(())
    })
}

fn assign(v: &str, e: &str) -> Result<(String, Stmnt)> {
    let e = Expr::parse(e)?;
    Ok((v.to_string(), Stmnt::Ass(v.to_string(), e)))
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_dependencies() {
        assert!(find_dependencies(&Map::new()).is_empty());
        let ds = [assign("x", "y * z").unwrap()]
            .into_iter()
            .collect::<Map<_, _>>();
        let vs = ["y".to_string(), "z".to_string()]
            .into_iter()
            .collect::<Set<_>>();
        assert_eq!(find_dependencies(&ds).get("x"), Some(&vs));
        let ds = [assign("x", "(1 + (0.5 * y^-1)^4.8)^-1").unwrap()]
            .into_iter()
            .collect::<Map<_, _>>();
        let vs = ["y".to_string()].into_iter().collect::<Set<_>>();
        assert_eq!(find_dependencies(&ds).get("x"), Some(&vs));
    }
}
