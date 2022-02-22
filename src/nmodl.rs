use std::fs::write;
use std::path::PathBuf;
use tracing::{info, trace, warn};

use crate::{
    error::{nml2_error, Error, Result},
    expr::{Expr, Quantity, Stmnt},
    instance::{Collapsed, Instance},
    lems::file::LemsFile,
    neuroml::process_files,
    variable::VarKind,
    Map, Set,
};

fn nmodl_error<T: Into<String>>(what: T) -> Error {
    Error::Nmodl { what: what.into() }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Kind {
    Density,
    Point,
    Junction,
}

#[derive(Debug, Clone)]
pub struct Nmodl {
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
    pub fn from(coll: &Collapsed, filter: &str) -> Result<Self> {
        let kind = Kind::Density;
        let suffix = coll
            .name
            .as_deref()
            .ok_or_else(|| nml2_error("Unnamed channel."))?
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
                    res = Stmnt::Ift(c, Box::new(Stmnt::Ass(nm.clone(), e)), Box::new(res));
                }
                if !init {
                    return Err(nml2_error(format!("Variable '{}' undefined.", nm)));
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

        let states = coll.states.clone();

        Ok(Nmodl {
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
        })
    }

    pub fn add_variables(&mut self, rhs: &Map<String, Stmnt>) {
        for (k, v) in rhs {
            self.variables.insert(k.clone(), v.clone());
            self.keep.insert(k.clone());
        }
        simplify(&mut self.variables, &mut self.fixed, &self.keep);
    }

    pub fn add_outputs(&mut self, rhs: &Map<String, Stmnt>) {
        for (k, v) in rhs {
            self.outputs.insert(k.clone(), v.clone());
            self.keep.insert(k.clone());
        }
        simplify(&mut self.outputs, &mut self.fixed, &self.keep);
    }
    pub fn add_initials(&mut self, rhs: &Map<String, Stmnt>) {
        for (k, v) in rhs {
            self.init.insert(k.clone(), v.clone());
            self.keep.insert(k.clone());
        }
        simplify(&mut self.init, &mut self.fixed, &self.keep);
    }
}

pub fn mk_nmodl(n: &Nmodl) -> Result<String> {
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
        .collect::<Map<String, Stmnt>>();
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

fn nmodl_deriv_block(n: &Nmodl) -> Result<String> {
    if n.deriv.is_empty() {
        return Ok(String::new());
    }
    let roots = n.deriv.keys().cloned().collect::<Vec<String>>();
    let vars = n
        .variables
        .iter()
        .chain(n.deriv.iter())
        .map(|(a, b)| (a.clone(), b.clone()))
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
    result.push(currents);
    result.push(String::from("}\n\n"));
    Ok(result.join("\n"))
}

fn nmodl_param_block(n: &Nmodl) -> Result<String> {
    if n.parameters.is_empty() {
        return Ok(String::new());
    }
    let mut result = vec![String::from("PARAMETER {")];
    for (k, v) in &n.parameters {
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
    let write = n.outputs.keys().collect::<Set<_>>();
    if write.contains(&String::from("i"))
        && (ions.contains(&String::new()) || n.kind == Kind::Junction || n.kind == Kind::Point)
    {
        result.push(String::from("  NONSPECIFIC_CURRENT i\n"));
    }

    for ion in ions {
        if !ion.is_empty() {
            let w = if write.contains(&format!("i{}", ion)) {
                format!(" WRITE i{}", ion)
            } else {
                String::new()
            };
            let mut r = vec![
                format!("{}i", ion),
                format!("i{}", ion),
                format!("{}o", ion),
                format!("e{}", ion),
            ];
            r.retain(|v| read.contains(v));
            let r = if !r.is_empty() {
                format!(" READ {}", r.join(", "))
            } else {
                String::new()
            };
            if !w.is_empty() || !r.is_empty() {
                result.push(format!("  USEION {}{}{}\n", ion, r, w));
            }
        };
    }
    if !n.parameters.is_empty() {
        let rs = n.parameters.keys().cloned().collect::<Vec<_>>();
        result.push(format!("  RANGE {}\n", rs.join(", ")));
    }
    result.push(String::from("}\n\n"));
    Ok(result.join(""))
}

fn nmodl_recv_block(n: &Nmodl) -> Result<String> {
    if !n.events.is_empty() {
        let evts = n
            .events
            .iter()
            .map(|v| v.1.print_to_string(2))
            .collect::<Vec<_>>()
            .join("\n");
        let result = vec![
            String::from("NET_RECEIVE(weight) {"),
            evts,
            String::from("}\n\n"),
        ];
        Ok(result.join("\n"))
    } else {
        Ok(String::new())
    }
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
            // return Err(nml2_error(format!("Could not resolve variable {}.", d)));
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

pub fn to_nmodl(instance: &Instance, filter: &str, base: &str) -> Result<String> {
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
                let mut n = Nmodl::from(&coll, &filter)?;
                // We know that we must write `i` and that it is in the variables
                if let Some((k, v)) = n.variables.remove_entry("i") {
                    n.outputs.insert(k, v);
                } else {
                    return Err(nmodl_error("Gap Junction without defined current 'i'"));
                }
                n.kind = Kind::Junction;
                mk_nmodl(&n)
            } else {
                let filter = filter.to_string();
                let instance = instance.clone();
                let coll = Collapsed::from_instance(&instance)?;
                let mut n = Nmodl::from(&coll, &filter)?;
                // We know that we must write `i` and that it is in the variables
                if let Some((k, v)) = n.variables.remove_entry("i") {
                    n.outputs.insert(k, v);
                } else {
                    return Err(nmodl_error("Synapse without defined current 'i'"));
                }
                n.kind = Kind::Point;
                mk_nmodl(&n)
            }
        }
        "baseIonChannel" => {
            let mut filter = filter.to_string();
            let mut instance = instance.clone();
            if !filter.is_empty() {
                filter.push(',');
            }
            filter.push_str("+conductance");
            if instance
                .attributes
                .keys()
                .filter(|s| s.ends_with("species"))
                .count()
                == 0
            {
                instance
                    .parameters
                    .insert(String::from("e"), Quantity::parse("0 mV")?);
                instance.component_type.parameters.push(String::from("e"));
                filter.push_str(",+e");
            }
            let coll = Collapsed::from_instance(&instance)?;
            let mut n = Nmodl::from(&coll, &filter)?;
            for ion in &n.species {
                let (ik, ix) = assign(&format!("i{}", ion), &format!("g*(v - e{})", ion))?;
                if ion.is_empty() {
                    let (k, v) = assign("g", "conductance")?;
                    n.variables.insert(k, v);
                }
                n.outputs.insert(ik.clone(), ix);
                let (ki, xi) = assign(&format!("{}conc", ion), &format!("{}i", ion))?;
                n.variables.insert(ki, xi);
            }
            n.kind = Kind::Density;
            mk_nmodl(&n)
        }
        "concentrationModel" => {
            let filter = filter.to_string();
            let instance = instance.clone();
            let coll = Collapsed::from_instance(&instance)?;
            let ion = coll.attributes.get("ion").unwrap().as_deref().unwrap();
            let mut n = Nmodl::from(&coll, &filter)?;
            let xi = format!("{}i", ion);
            let xo = format!("{}o", ion);
            let ix = format!("i{}", ion);
            let ic = String::from("concentration");
            let ec = String::from("extConcentration");
            n.add_outputs(&[assign(&xi, &ic)?, assign(&xo, &ec)?].into_iter().collect());
            n.add_initials(&[assign(&ic, &xi)?, assign(&ec, &xo)?].into_iter().collect());

            // Map variables iCa -> iX to compensate for NML2 mistakes
            let fix = |ex: &Expr| -> Expr {
                match ex {
                    Expr::Var(x) if x == "iCa" => Expr::Var(ix.clone()),
                    Expr::Var(x) if x == "surfaceArea" => Expr::Var("area".to_string()),
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
            mk_nmodl(&n)
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

pub fn export(lems: &LemsFile, nml: &[String], filter: &str, cat: &str) -> Result<()> {
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
                write(&path, to_nmodl(&instance, filter, ty)?)?;
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
