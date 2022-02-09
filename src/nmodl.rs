use std::collections::HashMap as Map;
use std::collections::HashSet as Set;
use std::fs::write;
use std::path::PathBuf;
use tracing::{info, trace, warn};

use crate::{
    error::{nml2_error, Error},
    expr::{Expr, Quantity, Stmnt},
    instance::{Collapsed, Instance},
    lems::file::LemsFile,
    neuroml::process_files,
    variable::{VarKind, Variable},
    Result,
};

fn nmodl_error<T: Into<String>>(what: T) -> Error {
    Error::Nmodl { what: what.into() }
}

#[derive(Debug, Clone)]
struct Nmodl {
    /// Suffix
    suffix: String,
    /// Declared (but undefined) variables
    symbols: Set<String>,
    /// Settable constants
    constants: Map<String, Quantity>,
    /// Settable parameters, with optional defaults
    parameters: Map<String, Option<Quantity>>,
    /// State variables
    state: Vec<String>,
    /// State initialisation
    init: Map<String, Stmnt>,
    /// State derivatives
    deriv: Map<String, Stmnt>,
    /// Variables
    variables: Map<String, Stmnt>,
    /// Ionic species we write currents for
    species: Vec<String>,
    /// Actual current assignments
    currents:  Map<String, Stmnt>,
    /// Transition matrix
    transitions: Vec<(String, String, String, String)>,
    /// Event processing
    events: Vec<(String, Expr)>,
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
    fn from(coll: &Collapsed, filter: &str) -> Result<Self> {
        let suffix = coll
            .name
            .as_deref()
            .ok_or_else(|| nml2_error("Unnamed channel."))?
            .to_string();
        let species = ion_species(coll);
        let events = coll.events.clone();
        let transitions = coll.transitions.clone();

        let mut state = Vec::new();
        let mut init = Map::new();
        let mut deriv = Map::new();
        let mut variables = Map::new();
        for var in &coll.variables {
            let nm = var.name.to_string();
            if let VarKind::State(i, d) = &var.kind {
                state.push(var.name.clone());
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
                    warn!(
                        "Variable '{}' default case undefined, assuming zero.",
                        var.name
                    );
                    Expr::F64(0.0)
                };
                let mut res = Stmnt::Ass(nm.clone(), d);
                let mut cs = cs.clone();
                while let Some((c, e)) = cs.pop() {
                    init = true;
                    res = Stmnt::Ift(c, Box::new(Stmnt::Ass(nm.clone(), e)), Box::new(res));
                }
                if !init {
                    return Err(nml2_error(format!("Variable '{}' undefined.", var.name)));
                }
                variables.insert(var.name.clone(), res);
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

        let mut currents = Map::new();
        for ion in &species {
            let ik = format!("i{}", ion);
            let ix = Expr::parse(&format!("g*(v - e{})", ion)).unwrap();
            currents.insert(ik.clone(), Stmnt::Ass(ik, ix));
            let ki = format!("{}conc", ion);
            let xi = Expr::Var(format!("{}i", ion));
            variables.insert(ki.clone(), Stmnt::Ass(ki, xi));
        }
        variables.insert(String::from("caConc"), Stmnt::Ass(String::from("caConc"), Expr::Var(String::from("cai"))));

        variables = simplify(&variables, &fixed);

        let mut symbols: Set<_> = [
            String::from("v"),
            String::from("v_peer"),
            String::from("cai"),
        ]
        .into_iter()
        .collect();
        for ion in &species {
            symbols.insert(format!("e{}", ion));
            symbols.insert(format!("i{}", ion));
            symbols.insert(format!("{}i", ion));
            symbols.insert(format!("{}o", ion));
        }
        symbols.extend(parameters.iter().map(|p: (&String, _)| p.0.to_string()));
        symbols.extend(constants.iter().map(|p: (&String, _)| p.0.to_string()));
        symbols.extend(state.iter().cloned());

        Ok(Nmodl {
            suffix,
            symbols,
            constants,
            parameters,
            state,
            init,
            deriv,
            variables,
            species,
            currents,
            events,
            transitions,
        })
    }
}

pub fn mk_nmodl(coll: &Collapsed, filter: &str) -> Result<String> {
    let n = Nmodl::from(coll, filter)?;
    let result = vec![
        nmodl_neuron_block(&n)?,
        nmodl_const_block(&n)?,
        nmodl_param_block(&n)?,
        nmodl_state_block(&n)?,
        nmodl_init_block(&n)?,
        nmodl_deriv_block(&n)?,
        nmodl_break_block(&n)?,
        nmodl_recv_block(&n)?,
        nmodl_kinetic_block(&n)?,
    ];
    Ok(result.join(""))
}

fn nmodl_state_block(n: &Nmodl) -> Result<String> {
    if !n.state.is_empty() {
        Ok(format!("STATE {{ {} }}\n\n", n.state.join(" ")))
    } else {
        Ok(String::new())
    }
}

fn nmodl_init_block(n: &Nmodl) -> Result<String> {
    if n.init.is_empty() {
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
    let result = vec![
        String::from("INITIAL {"),
        print_dependency_chains(&roots, &vars, &n.symbols)?,
        init,
        String::from("}\n\n"),
    ];
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
    let deriv = n
        .deriv
        .values()
        .map(|s| s.print_to_string(2))
        .collect::<Vec<String>>()
        .join("\n");
    let result = vec![
        String::from("DERIVATIVE dstate {"),
        print_dependency_chains(&roots, &vars, &n.symbols)?,
        deriv,
        String::from("}\n\n"),
    ];
    Ok(result.join("\n"))
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
        .chain(n.currents.iter())
        .map(|(a, b)| (a.clone(), b.clone()))
        .collect::<Map<String, Stmnt>>();
    let roots = n.currents.keys().cloned().collect::<Vec<String>>();
    let currents = n
        .currents
        .values()
        .map(|s| s.print_to_string(2))
        .collect::<Vec<String>>()
        .join("\n");


    result.push(print_dependency_chains(
        &roots,
        &vars,
        &n.symbols,
    )?);
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

    let mut transitions = Map::new();
    for (from, to, fwd, bwd) in &n.transitions {
        if from > to {
            transitions
                .entry((from, to))
                .and_modify(|t: &mut (Expr, Expr)| {
                    *t = (
                        Expr::Add(vec![Expr::parse(fwd).unwrap(), t.0.clone()]).simplify(),
                        Expr::Add(vec![Expr::parse(bwd).unwrap(), t.1.clone()]).simplify(),
                    )
                })
                .or_insert((Expr::parse(fwd).unwrap(), Expr::parse(bwd).unwrap()));
        } else {
            transitions
                .entry((to, from))
                .and_modify(|t: &mut (Expr, Expr)| {
                    *t = (
                        Expr::Add(vec![Expr::parse(bwd).unwrap(), t.0.clone()]).simplify(),
                        Expr::Add(vec![Expr::parse(fwd).unwrap(), t.1.clone()]).simplify(),
                    )
                })
                .or_insert((Expr::parse(bwd).unwrap(), Expr::parse(fwd).unwrap()));
        }
    }

    let mut vars = n.variables.clone();
    let mut table = Vec::new();
    let mut depds = Vec::new();
    let mut locals = Vec::new();
    for ((from, to), (fwd, bwd)) in transitions.into_iter() {
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


        eprintln!("{} {} {:?} {:?}", from, to, fwd, bwd);
        eprintln!("{} = {:?}", fname, fwd);

        let fwd = Stmnt::Ass(fname.clone(), fwd);
        let bwd = Stmnt::Ass(bname.clone(), bwd);
        locals.push(fname.clone());
        locals.push(bname.clone());
        vars.insert(fname.clone(), bwd.clone());
        vars.insert(bname.clone(), fwd.clone());
        table.push(format!("  ~ {} <-> {} ({}, {})", from, to, fname, bname));
        depds.push(fwd);
        depds.push(bwd);
    }

    eprintln!("Key {:?}", vars.keys());

    let result = format!(
        "KINETIC scheme {{
  LOCAL {}
{}

{}
}}

",
        locals.join(", "),
        print_dependency_chains(&locals, &vars, &n.symbols)?,
        table.join("\n")
    );
    Ok(result)
}

fn nmodl_neuron_block(n: &Nmodl) -> Result<String> {
    let mut result = vec![
        String::from("NEURON {\n"),
        format!("  SUFFIX {}\n", n.suffix),
    ];
    // TODO actually _scan_ the ion use here
    for ion in &n.species {
        let current = if ion.is_empty() {
            String::from("  NONSPECIFIC_CURRENT i\n")
        } else {
            format!(
                "  USEION {ion} READ e{ion}, {ion}i WRITE i{ion}\n",
                ion = ion
            )
        };
        result.push(current);
    }
    if !n.species.contains(&String::from("ca")) {
        result.push(String::from("  USEION ca READ cai\n"));
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
            .map(|v| v.1.print_to_string())
            .collect::<Vec<_>>()
            .join("\n");
        let result = vec![
            String::from("NET_RECEIVE(weight) {"),
            evts,
            String::from("}\n"),
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
            if k.ends_with("species") {
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
            result.push(format!("{} = ???", d));
        }
    }
    Ok(result.join("\n"))
}

pub fn to_nmodl(instance: &Instance, filter: &str) -> Result<String> {
    let mut filter = filter.to_string();
    let mut instance = instance.clone();
    // do fixes for known types
    match instance.component_type.name.as_ref() {
        "gapJunction" => {
            // Gap Junctions need peer voltage, which is provided by Arbor
            instance
                .component_type
                .variables
                .retain(|v| v.name != "vpeer");
            instance.component_type.variables.push(Variable {
                name: String::from("vpeer"),
                exposure: None,
                dimension: String::from("voltage"),
                kind: VarKind::Derived(Vec::new(), Some(Expr::parse("v_peer")?)),
            });
            if !filter.is_empty() {
                filter.push(',');
            }
            filter.push_str("+weight,+conductance");
            instance
                .component_type
                .parameters
                .push(String::from("weight"));
            instance
                .parameters
                .insert(String::from("weight"), Quantity::parse("1")?);
        }
        "ionChannel" | "ionChannelHH" | "ionChannelKS" | "ionChannelPassive" => {
            if !filter.is_empty() {
                filter.push(',');
            }
            filter.push_str("+conductance");
            // if instance..is_empty() {
                instance.parameters.insert(String::from("e"), Quantity::parse("0 mV")?);
                filter.push_str(",+e");
            // }
        }
        _ => {}
    }
    mk_nmodl(&Collapsed::from_instance(&instance)?, &filter)
}

fn simplify(variables: &Map<String, Stmnt>, fixed: &Map<String, Expr>) -> Map<String, Stmnt> {
    let mut result = variables.clone();
    let mut fixed = fixed.clone();
    loop {
        let mut new = Map::new();
        for (k, v) in &result {
            match v {
                Stmnt::Ass(_, x @ Expr::F64(_)) => {
                    fixed.insert(k.clone(), x.clone());
                }
                Stmnt::Ass(_, x @ Expr::Var(_)) => {
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

        if new == result {
            break;
        }
        result = new;
    }
    result
}

pub fn export(
    lems: &LemsFile,
    nml: &[String],
    ty: &Option<&str>,
    filter: &str,
    cat: &str,
) -> Result<()> {
    let tys = if let Some(ty) = ty {
        vec![*ty]
    } else {
        vec!["baseIonChannel", "baseSynapse"]
    };
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
                write(&path, to_nmodl(&instance, filter)?)?;
            }
        }
        Ok(())
    })
}
