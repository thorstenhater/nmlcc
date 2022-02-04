use std::collections::HashMap as Map;
use std::collections::HashSet as Set;
use std::fs::write;
use std::path::PathBuf;
use tracing::{info, trace};

use crate::{
    error::Error,
    expr::{Expr, Quantity},
    instance::{Collapsed, Instance},
    lems::file::LemsFile,
    neuroml::process_files,
    variable::{VarKind, Variable},
    Result,
};

fn illegal_select(v: &str) -> Error {
    nmodl_error(format!("Select variable in {} post flattening stage.", v))
}
fn nmodl_error<T: Into<String>>(what: T) -> Error {
    Error::Nmodl { what: what.into() }
}

fn automatic_variables(coll: &Collapsed) -> Vec<String> {
    let mut res = vec![String::from("v"), String::from("v_peer")];
    for ion in ion_species(coll) {
        res.push(format!("e{}", ion));
        res.push(format!("i{}", ion));
        res.push(format!("{}i", ion));
        res.push(format!("{}o", ion));
    }
    let cai = String::from("cai");
    if !res.contains(&cai) {
        res.push(cai);
    }
    res
}

fn nmodl_init_block(coll: &Collapsed) -> Result<String> {
    let mut result = vec![String::from("INITIAL {")];
    let mut state = Vec::new();
    let mut deriv = Vec::new();

    for var in &coll.variables {
        match &var.kind {
            VarKind::State(i, _) if i.is_some() => {
                let mut v = var.clone();
                v.kind = VarKind::State(i.clone(), None);
                state.push(v.clone());
                deriv.push(v);
            }
            VarKind::State(_, _) => {}
            VarKind::Derived(_, _) => deriv.push(var.clone()),
            VarKind::Select(_, _) => return Err(illegal_select(&var.name)),
        }
    }

    if state.is_empty() {
        return Ok(String::new());
    }

    // Variables we can access everywhere: parameters, constants, but not state (might not be defined yet)
    let known = coll
        .parameters
        .iter()
        .map(|p| p.0.to_string())
        .chain(coll.constants.iter().map(|p| p.0.to_string()))
        .chain(automatic_variables(coll).iter().cloned())
        .collect::<Set<_>>();

    let init = state.iter().map(|v| v.name.to_string()).collect::<Vec<_>>();
    let deps = deriv
        .iter()
        .chain(state.iter())
        .cloned()
        .collect::<Vec<_>>();
    result.push(print_dependencies(&init, &deps, &known)?);
    result.push(String::from("}\n\n"));
    Ok(result.join("\n"))
}

fn nmodl_deriv_block(coll: &Collapsed) -> Result<String> {
    let mut state = Vec::new();
    let mut deriv = Vec::new();

    for var in &coll.variables {
        match &var.kind {
            VarKind::State(_, d) if d.is_some() => {
                let mut v = var.clone();
                v.kind = VarKind::State(None, d.clone());
                state.push(v.clone());
                deriv.push(v);
            }
            VarKind::State(_, _) => {}
            VarKind::Derived(_, _) => deriv.push(var.clone()),
            VarKind::Select(_, _) => return Err(illegal_select(&var.name)),
        }
    }

    if state.is_empty() {
        return Ok(String::new());
    }

    // Variables we can access everywhere: parameters, constants, state and voltage
    let known = coll
        .parameters
        .iter()
        .map(|p| p.0.to_string())
        .chain(coll.constants.iter().map(|p| p.0.to_string()))
        .chain(state.iter().map(|v| v.name.to_string()))
        .chain(automatic_variables(coll).iter().cloned())
        .collect::<Set<_>>();

    let init: Vec<_> = state.iter().map(|v| v.name.to_string()).collect();
    let deps = deriv
        .iter()
        .chain(state.iter())
        .cloned()
        .collect::<Vec<_>>();
    let result = vec![
        String::from("DERIVATIVE dstate {"),
        print_dependencies(&init, &deps, &known)?,
        String::from("}\n\n"),
    ];
    Ok(result.join("\n"))
}

fn nmodl_break_block(coll: &Collapsed) -> Result<String> {
    let mut state = Vec::new();
    let mut dstate = Vec::new();
    let mut vars = Vec::new();

    for var in &coll.variables {
        match &var.kind {
            VarKind::State(_, d) => {
                let mut v = var.clone();
                v.kind = VarKind::State(None, None);
                state.push(v.clone());
                vars.push(v.clone());
                if d.is_some() {
                    v.kind = VarKind::State(None, d.clone());
                    dstate.push(v);
                }
            }
            VarKind::Derived(_, _) => vars.push(var.clone()),
            VarKind::Select(_, _) => return Err(illegal_select(&var.name)),
        }
    }

    // Variables we can access everywhere: parameters, constants, state and voltage
    let known = coll
        .parameters
        .iter()
        .map(|p| p.0.to_string())
        .chain(coll.constants.iter().map(|p| p.0.to_string()))
        .chain(state.iter().map(|v| v.name.to_string()))
        .chain(automatic_variables(coll).iter().cloned())
        .collect::<Set<_>>();

    let mut result = vec![String::from("BREAKPOINT {")];
    if !dstate.is_empty() && !coll.transitions.is_empty() {
        return Err(nmodl_error("Both KINETIC and ODEs given"));
    }
    if !dstate.is_empty() {
        result.push(String::from("  SOLVE dstate METHOD cnexp"));
    }
    if !coll.transitions.is_empty() {
        result.push(String::from("  SOLVE scheme METHOD sparse"));
    }
    let currents: Vec<_> = ion_species(coll)
        .iter()
        .map(|s| format!("i{}", s))
        .collect();
    result.push(print_dependencies(&currents, &vars, &known)?);
    result.push(String::from("}\n\n"));
    Ok(result.join("\n"))
}

fn nmodl_state_block(coll: &Collapsed) -> Result<String> {
    let state = coll
        .variables
        .iter()
        .filter(|v| matches!(v.kind, VarKind::State(_, _)))
        .map(|v| v.name.to_string())
        .collect::<Vec<_>>();
    if !state.is_empty() {
        Ok(format!("STATE {{ {} }}\n\n", state.join(" ")))
    } else {
        Ok(String::new())
    }
}

fn nmodl_param_block(coll: &Collapsed) -> Result<String> {
    if coll.parameters.is_empty() {
        return Ok(String::new());
    }
    let mut result = vec![String::from("PARAMETER {")];
    for (k, v) in &coll.parameters {
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

fn nmodl_const_block(coll: &Collapsed) -> Result<String> {
    if coll.constants.is_empty() {
        return Ok(String::new());
    }
    let mut result = vec![String::from("CONSTANT {")];
    for (k, v) in &coll.constants {
        let mut ln = format!("  {} = {}", k, v.value);
        if let Some(u) = v.unit.as_ref() {
            ln.push_str(&format!(" ({})", u));
        }
        result.push(ln);
    }
    result.push(String::from("}\n\n"));
    Ok(result.join("\n"))
}

fn nmodl_kinetic_block(coll: &Collapsed) -> Result<String> {
    if coll.transitions.is_empty() {
        return Ok(String::new());
    }

    let mut vars = Vec::new();

    for var in &coll.variables {
        match &var.kind {
            VarKind::State(_, _) => {}
            VarKind::Derived(_, _) => vars.push(var.clone()),
            VarKind::Select(_, _) => return Err(illegal_select(&var.name)),
        }
    }

    // Variables we can access everywhere: parameters, constants, state and voltage
    let known = coll
        .parameters
        .iter()
        .map(|p| p.0.to_string())
        .chain(coll.constants.iter().map(|p| p.0.to_string()))
        .chain(automatic_variables(coll).iter().cloned())
        .collect::<Set<_>>();

    // try to merge transitions
    let mut transitions = Map::new();
    for (from, to, fwd, bwd) in &coll.transitions {
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

    let mut table = Vec::new();
    let mut depds = Vec::new();
    for ((from, to), (fwd, bwd)) in &transitions {
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
        vars.push(Variable {
            name: fname.to_string(),
            kind: VarKind::Derived(Vec::new(), Some(fwd.clone())),
            dimension: String::new(),
            exposure: None,
        });
        vars.push(Variable {
            name: bname.to_string(),
            kind: VarKind::Derived(Vec::new(), Some(bwd.clone())),
            dimension: String::new(),
            exposure: None,
        });
        table.push(format!("  ~ {} <-> {} ({}, {})", from, to, fname, bname));
        depds.push(fname.to_string());
        depds.push(bname.to_string());
    }

    let result = format!(
        "KINETIC scheme {{
  LOCAL {}
{}

{}
}}

",
        depds.join(", "),
        print_dependencies(&depds, &vars, &known)?,
        table.join("\n")
    );
    Ok(result)
}

fn nmodl_neuron_block(coll: &Collapsed) -> Result<String> {
    let suffix = coll.name.as_ref().unwrap().to_string();
    let mut result = vec![String::from("NEURON {\n"), format!("  SUFFIX {}\n", suffix)];
    let ions = ion_species(coll);
    for ion in &ions {
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
    if !ions.contains(&String::from("ca")) {
        result.push(String::from("  USEION ca READ cai\n"));
    }
    if !coll.parameters.is_empty() {
        let rs = coll.parameters.keys().cloned().collect::<Vec<_>>();
        result.push(format!("  RANGE {}\n", rs.join(", ")));
    }
    result.push(String::from("}\n\n"));
    Ok(result.join(""))
}

fn nmodl_recv_block(coll: &Collapsed) -> Result<String> {
    if !coll.events.is_empty() {
        let evts = coll
            .events
            .iter()
            .map(|(k, v)| format!("  {} = {}", k, v.print_to_string()))
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
            Some(Variable {
                kind: VarKind::Derived(cs, df),
                ..
            }) => {
                let mut depth = 0;
                for (b, e) in cs {
                    if depth != 0 {
                        result.push(String::from("  else {"));
                    }
                    result.push(format!(
                        "  if ({}) {{ {} = {} }}",
                        b.print_to_string(),
                        d,
                        e.print_to_string()
                    ));
                    depth += 1;
                }
                if let Some(e) = df {
                    if depth != 0 {
                        result.push(String::from("  else {"));
                    }
                    result.push(format!("  {} = {}", d, e.print_to_string()));
                    if depth != 0 {
                        result.push(String::from("  }"));
                    }
                }
                for _ in 0..depth - 1 {
                    result.push(String::from("  }"));
                }
            }
            Some(Variable {
                kind: VarKind::State(Some(x), None),
                ..
            }) => {
                result.push(format!("  {} = {}", d, x.print_to_string()));
            }
            Some(Variable {
                kind: VarKind::State(None, Some(x)),
                ..
            }) => {
                result.push(format!("  {}' = {}", d, x.print_to_string()));
            }
            Some(e) => {
                return Err(nmodl_error(format!(
                    "Don't know what to do with variable: {:?}",
                    e
                )));
            }
            None => {
                return Err(nmodl_error(format!("No such variable: {}", d)));
            }
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
            let ion = instance
                .attributes
                .get("species")
                .cloned()
                .unwrap_or_default();

            if !filter.is_empty() {
                filter.push(',');
            }
            filter.push_str("+conductance");

            if !ion.is_empty() {
                instance.component_type.variables.push(Variable {
                    name: format!("{}Conc", ion),
                    exposure: None,
                    dimension: String::from("concentration"),
                    kind: VarKind::Derived(Vec::new(), Some(Expr::parse(&format!("{}i", ion))?)),
                });
            }

            instance.component_type.variables.push(Variable {
                name: String::from("caConc"),
                exposure: None,
                dimension: String::from("concentration"),
                kind: VarKind::Derived(Vec::new(), Some(Expr::parse("cai")?)),
            });

            if ion.is_empty() {
                instance
                    .parameters
                    .insert(String::from("e"), Quantity::parse("0 mV")?);
                instance.component_type.parameters.push(String::from("e"));
                filter.push_str(",+e");
            }

            let current = format!("g*(v - e{})", ion);
            instance.component_type.variables.push(Variable {
                name: format!("i{}", ion),
                exposure: None,
                dimension: String::from("current"),
                kind: VarKind::Derived(Vec::new(), Some(Expr::parse(&current)?)),
            });
        }
        _ => {}
    }
    mk_nmodl(&Collapsed::from_instance(&instance)?.simplify(&filter))
}

pub fn mk_nmodl(coll: &Collapsed) -> Result<String> {
    let result = vec![
        nmodl_neuron_block(coll)?,
        nmodl_const_block(coll)?,
        nmodl_param_block(coll)?,
        nmodl_state_block(coll)?,
        nmodl_init_block(coll)?,
        nmodl_deriv_block(coll)?,
        nmodl_break_block(coll)?,
        nmodl_recv_block(coll)?,
        nmodl_kinetic_block(coll)?,
    ];
    Ok(result.join(""))
}

/// Map variables to dependencies
fn find_dependencies(variables: &[Variable]) -> Map<String, Set<String>> {
    let mut deps = Map::new();
    let add_var = |e: &Expr, acc: &mut Set<String>| {
        if let Expr::Var(v) = e {
            acc.insert(v.to_string());
        }
    };
    for v in variables {
        let u = deps.entry(v.name.to_string()).or_insert_with(Set::new);
        match &v.kind {
            VarKind::State(it, dv) => {
                if let Some(i) = it.as_ref() {
                    i.fold(u, &add_var)
                }
                if let Some(d) = dv.as_ref() {
                    d.fold(u, &add_var)
                }
            }
            VarKind::Derived(cs, df) => {
                if let Some(d) = df.as_ref() {
                    d.fold(u, &add_var)
                }
                for (c, x) in cs {
                    c.fold(u, &add_var);
                    x.fold(u, &add_var);
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
