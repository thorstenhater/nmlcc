use crate::{
    error::{Error, Result},
    expr::Expr,
    expr::Quantity,
    lems::file::LemsFile,
    neuroml::{
        process_files,
        raw::{
            BiophysicalProperties, BiophysicalPropertiesBody, ChannelDensity, ChannelDensityNernst,
            ChannelDensityNonUniform, ChannelDensityNonUniformNernst, ExtracellularProperties,
            InhomogeneousParameter, InhomogeneousValue, InitMembPotential, IntracellularProperties,
            IntracellularPropertiesBody, MembraneProperties, MembranePropertiesBody, Resistivity,
            Species, SpecificCapacitance, VariableParameter,
        },
    },
    nml2_error,
    xml::XML,
    Map,
};

use std::fmt::Write as _;
use std::fs::write;
use std::path::PathBuf;
use tracing::{info, trace, warn};

pub fn to_decor(lems: &LemsFile, nml: &[String]) -> Result<Map<String, Vec<Decor>>> {
    let mut cells = Map::new();
    process_files(nml, |_, node| {
        if node.tag_name().name() == "cell" {
            if let Some(id) = node.attribute("id") {
                let mut result = Vec::new();
                let inhomogeneous_parameters = parse_inhomogeneous_parameters(node)?;
                for bpp in node.descendants() {
                    if bpp.tag_name().name() != "biophysicalProperties" {
                        continue;
                    }
                    let prop: BiophysicalProperties = XML::from_node(&bpp);
                    result.append(&mut biophys(&prop, lems, &inhomogeneous_parameters)?);
                }
                *cells.entry(id.to_string()).or_default() = result;
            }
        }
        Ok(())
    })?;
    Ok(cells)
}

#[allow(non_snake_case)] // xml..
fn parse_inhomogeneous_parameters(
    cell: &roxmltree::Node<'_, '_>,
) -> Result<Map<String, ParsedInhomogeneousParameter>> {
    let mut inhomogeneous_parameters = Map::new();
    for m in cell.descendants() {
        if m.tag_name().name() != "morphology" {
            continue;
        }
        for ihp in m
            .descendants()
            .filter(|n| n.has_tag_name("inhomogeneousParameter"))
        {
            if let Some(segmentGroup) = ihp.parent().and_then(|p| p.attribute("id")) {
                use crate::neuroml::raw::InhomogeneousParameterBody::*;
                use crate::neuroml::raw::{DistalDetails, ProximalDetails};
                let ihp: InhomogeneousParameter = XML::from_node(&ihp);
                if ihp.metric != "Path Length from root" {
                    return Err(nml2_error!(
                        "Only path length from root is supported as inhomogeneous parameter metric"
                    ));
                }
                let mut subtract_the_minimum = false;
                let mut normalize_end = false;
                for elem in ihp.body {
                    match elem {
                        proximal(ProximalDetails { translationStart }) => {
                            if translationStart == 0.0 {
                                subtract_the_minimum = true;
                            } else {
                                return Err(acc_unimplemented(
                                    "Proximal translationStart must be 0 in InhomogeneousParameter",
                                ));
                            }
                        }
                        distal(DistalDetails { normalizationEnd }) => {
                            if normalizationEnd == 1.0 {
                                normalize_end = true;
                            } else {
                                return Err(acc_unimplemented(
                                    "Distal normalizeEnd must be 1 in InhomogeneousParameter",
                                ));
                            }
                        }
                    }
                }
                if normalize_end {
                    return Err(acc_unimplemented(
                        "Endpoint normalization for inhomogeneous parameters is not yet supported",
                    ));
                }
                inhomogeneous_parameters.insert(
                    ihp.id,
                    ParsedInhomogeneousParameter {
                        variable: ihp.variable,
                        region: segmentGroup.to_string(),
                        subtract_the_minimum,
                        normalize_end,
                    },
                );
            } else {
                return Err(acc_unimplemented(
                    "inhomogeneousParameter definition must be inside segmentGroup group with id",
                ));
            }
        }
    }
    return Ok(inhomogeneous_parameters);
}

pub fn export(lems: &LemsFile, nml: &[String], pfx: &str, cat_prefix: &str) -> Result<()> {
    trace!("Creating path {}", pfx);
    std::fs::create_dir_all(&pfx)?;

    let cells = to_decor(lems, nml)?;
    for (cell, decor) in cells {
        let mut file = PathBuf::from(pfx);
        file.push(cell);
        file.set_extension("acc");
        info!("Writing ACC to {:?}", &file);
        write(
            &file,
            decor.to_sexp_with_config(&SexpConfig {
                cat_prefix: cat_prefix.to_string(),
            }),
        )?;
    }
    Ok(())
}

fn acc_unimplemented(f: &str) -> Error {
    Error::Acc {
        what: format!("Feature '{}' not implemented for ACC export.", f),
    }
}

#[derive(Clone, Debug, Default)]
pub struct SexpConfig {
    cat_prefix: String,
}

impl SexpConfig {
    fn add_prefix(&self, mech: &str) -> String {
        if mech == "nernst" {
            mech.to_string()
        } else {
            format!("{}{}", self.cat_prefix, mech)
        }
    }
}

pub trait Sexp {
    fn to_sexp(&self) -> String {
        self.to_sexp_with_config(&SexpConfig::default())
    }
    fn to_sexp_with_config(&self, config: &SexpConfig) -> String;
}

#[derive(Clone, Debug)]
pub struct ParsedInhomogeneousParameter {
    variable: String,           // p
    region: String,             // apicalDends
    subtract_the_minimum: bool, // proximal root or region minimum
    #[allow(dead_code)]
    normalize_end: bool, // most distal distance 1 or um (not supported yet)
}

#[derive(Clone, Debug)]
pub struct MechVariableParameter {
    param: ParsedInhomogeneousParameter,
    value: String, // 10 * p
}

#[derive(Clone, Debug)]
pub enum Paintable {
    Xi(String, String),
    Xo(String, String),
    Ra(String),
    Vm(String),
    Cm(String),
    Er(String, String),
    Em(String, String),
    Mech(String, Map<String, String>),
    NonUniformMech(
        String,
        Map<String, String>,
        Map<String, MechVariableParameter>,
    ),
}

impl Paintable {
    fn normalise(&self, lems: &LemsFile) -> Result<Self> {
        let norm = |v: &str| -> Result<String> {
            let q = Quantity::parse(v)?;
            let u = lems.normalise_quantity(&q)?;
            Ok(format!("{}", u.value))
        };
        let r = match self {
            Paintable::Xi(i, v) => Paintable::Xi(i.clone(), norm(v)?),
            Paintable::Xo(i, v) => Paintable::Xo(i.clone(), norm(v)?),
            Paintable::Ra(v) => Paintable::Ra(norm(v)?),
            Paintable::Vm(v) => Paintable::Vm(norm(v)?),
            Paintable::Cm(v) => Paintable::Cm(norm(v)?),
            Paintable::Er(i, v) => Paintable::Er(i.clone(), norm(v)?),
            Paintable::Em(i, m) if m == "nernst" => Paintable::Em(i.clone(), m.clone()),
            Paintable::Em(i, m) => Paintable::Em(i.clone(), norm(m)?),
            Paintable::Mech(m, ps) => {
                let mut ps = ps.clone();
                for v in ps.values_mut() {
                    *v = norm(v)?;
                }
                Paintable::Mech(m.clone(), ps)
            }
            Paintable::NonUniformMech(m, ps, ns) => {
                let mut ps = ps.clone();
                for v in ps.values_mut() {
                    *v = norm(v)?;
                }
                let mut ns = ns.clone();
                for v in ns.values_mut() {
                    let metric = if v.param.subtract_the_minimum {
                        Expr::ProximalDistanceFromRegion(v.param.region.to_string())
                    } else {
                        Expr::DistanceFromRoot()
                    };
                    let e = Expr::parse(&v.value)?;
                    let e = e.map(&|ex: &Expr| -> Expr {
                        match ex {
                            Expr::Var(x) if x == &v.param.variable => metric.clone(),
                            _ => ex.clone(),
                        }
                    });
                    let as_sexpr = e.to_sexp();
                    *v = MechVariableParameter {
                        param: v.param.clone(),
                        value: as_sexpr,
                    };
                }
                Paintable::NonUniformMech(m.clone(), ps, ns)
            }
        };
        Ok(r)
    }
}

impl Sexp for Expr {
    fn to_sexp_with_config(&self, _: &SexpConfig) -> String {
        fn op_to_sexp(op: &str, args: &Vec<Expr>) -> String {
            format!(
                "({op} {})",
                args.iter()
                    .map(|x| x.to_sexp())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        }
        match self {
            Expr::F64(x) => format!("{x}"),
            Expr::Var(x) => format!("{x}"),
            Expr::Add(x) => op_to_sexp("add", x),
            Expr::Mul(x) => op_to_sexp("mul", x),
            Expr::Pow(x) => op_to_sexp("pow", x),
            Expr::Exp(x) => format!("(exp {})", x.to_sexp()),
            Expr::Log(x) => format!("(log {})", x.to_sexp()),
            Expr::Sqrt(x) => format!("(sqrt {})", x.to_sexp()),
            Expr::H(x) => format!("(step {})", x.to_sexp()),
            Expr::ProximalDistanceFromRegion(region) => {
                format!("(proximal-distance (region \"{}\"))", region)
            }
            Expr::DistanceFromRoot() => format!("(distance (root))"),
        }
    }
}

impl Sexp for Paintable {
    fn to_sexp_with_config(&self, config: &SexpConfig) -> String {
        match self {
            Paintable::Xi(i, v) => format!("(ion-internal-concentration \"{}\" {})", i, v),
            Paintable::Xo(i, v) => format!("(ion-external-concentration \"{}\" {})", i, v),
            Paintable::Er(i, v) => format!("(ion-reversal-potential \"{}\" {})", i, v),
            Paintable::Em(i, v) => format!(
                "(ion-reversal-potential-method \"{}\" (mechanism \"{}/{}\"))",
                i, v, i
            ),
            Paintable::Ra(v) => format!("(axial-resistivity {})", v),
            Paintable::Vm(v) => format!("(membrane-potential {})", v),
            Paintable::Cm(v) => format!("(membrane-capacitance {})", v),
            Paintable::Mech(m, gs) => {
                let mut result = format!("(density (mechanism \"{}\"", config.add_prefix(m));
                for (k, v) in gs.iter() {
                    let x = format!(" (\"{}\" {})", k, v);
                    result.push_str(&x);
                }
                result.push(')');
                result.push(')');
                result
            }
            Paintable::NonUniformMech(m, gs, ns) => {
                let mut result = format!(
                    "(scaled-mechanism (density (mechanism \"{}\"",
                    config.add_prefix(m)
                );
                for (k, v) in gs.iter() {
                    let x = format!(" (\"{}\" {})", k, v);
                    result.push_str(&x);
                }
                result.push(')');
                result.push(')');
                for (k, v) in ns.iter() {
                    let x = format!(" (\"{}\" {})", k, v.value);
                    result.push_str(&x);
                }
                result.push(')');
                result
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Decor {
    Default(Paintable),
    Paint(String, Paintable),
}

impl Decor {
    fn new(g: &str, p: Paintable, f: bool) -> Self {
        if g.is_empty() || g == "all" {
            if f {
                Decor::Paint("all".to_string(), p)
            } else {
                Decor::Default(p)
            }
        } else {
            Decor::Paint(g.to_string(), p)
        }
    }

    fn normalise(&self, lems: &LemsFile) -> Result<Self> {
        match self {
            Decor::Default(p) => Ok(Decor::Default(p.normalise(lems)?)),
            Decor::Paint(r, p) => Ok(Decor::Paint(r.clone(), p.normalise(lems)?)),
        }
    }
}

impl Sexp for Decor {
    fn to_sexp_with_config(&self, config: &SexpConfig) -> String {
        match self {
            Decor::Default(i) => format!("(default {})", i.to_sexp_with_config(config)),
            Decor::Paint(r, i) => format!(
                "(paint (region \"{}\") {})",
                r,
                i.to_sexp_with_config(config)
            ),
        }
    }
}

impl Sexp for Vec<Decor> {
    fn to_sexp_with_config(&self, config: &SexpConfig) -> String {
        let mut result = String::from(
            "(arbor-component
  (meta-data (version \"0.1-dev\"))
  (decor
",
        );
        for it in self {
            writeln!(result, "    {}", it.to_sexp_with_config(config)).unwrap();
        }
        result.pop();
        result.push_str("))\n");
        result
    }
}

pub fn biophys(
    prop: &BiophysicalProperties,
    lems: &LemsFile,
    inhomogeneous_parameters: &Map<String, ParsedInhomogeneousParameter>,
) -> Result<Vec<Decor>> {
    use BiophysicalPropertiesBody::*;
    let mut decor = Vec::new();
    for item in &prop.body {
        match item {
            membraneProperties(m) => decor.append(&mut membrane(m, inhomogeneous_parameters)?),
            intracellularProperties(i) => decor.append(&mut intra(i)?),
            extracellularProperties(e) => decor.append(&mut extra(e)?),
            property(_) | notes(_) | annotation(_) => {}
        }
    }
    for it in decor.iter_mut() {
        *it = it.normalise(lems)?;
    }
    Ok(decor)
}

#[allow(non_snake_case)] // xml..
fn membrane(
    membrane: &MembraneProperties,
    inhomogeneous_parameters: &Map<String, ParsedInhomogeneousParameter>,
) -> Result<Vec<Decor>> {
    let known_ions = vec![String::from("ca"), String::from("k"), String::from("na")];

    use MembranePropertiesBody::*;
    let mut result = Vec::new();
    for item in &membrane.body {
        match item {
            channelDensity(ChannelDensity {
                ionChannel,
                condDensity,
                erev,
                segmentGroup,
                ion,
                body,
                ..
            }) => {
                if !body.is_empty() {
                    return Err(acc_unimplemented("Non-empty body in MembraneProperties"));
                }
                let mut gs = simple_ion(&known_ions, &mut result, ion, segmentGroup, erev)?;
                if let Some(g) = condDensity {
                    gs.insert(String::from("conductance"), g.clone());
                }
                result.push(Decor::new(
                    segmentGroup,
                    Paintable::Mech(ionChannel.to_string(), gs),
                    true,
                ));
            }
            channelDensityNernst(ChannelDensityNernst {
                ionChannel,
                condDensity,
                segmentGroup,
                ion,
                body,
                ..
            }) => {
                if !body.is_empty() {
                    return Err(acc_unimplemented("Non-empty body in ChannelDensityNernst"));
                }
                let mut gs = Map::new();
                if let Some(g) = condDensity {
                    gs.insert(String::from("conductance"), g.clone());
                }
                result.push(Decor::new(
                    "",
                    // TODO This is poison
                    Paintable::Em(ion.to_string(), "nernst".to_string()),
                    false,
                ));
                result.push(Decor::new(
                    segmentGroup,
                    Paintable::Mech(ionChannel.to_string(), gs),
                    true,
                ));
            }
            channelDensityNonUniform(ChannelDensityNonUniform {
                ionChannel,
                ion,
                body,
                erev,
                ..
            }) => {
                use crate::neuroml::raw::ChannelDensityNonUniformBody::variableParameter;
                use crate::neuroml::raw::VariableParameterBody::inhomogeneousValue;
                let (param, segmentGroup, ihb) = match &body[..] {
                    [variableParameter(VariableParameter {
                        parameter,
                        segmentGroup,
                        body: ihb,
                    })] => (parameter, segmentGroup, ihb),
                    _ => {
                        return Err(acc_unimplemented("ChannelDensityNonUniformNernst must contain a single VariableParameter"));
                    }
                };
                let (ihv, value) = match &ihb[..] {
                    [inhomogeneousValue(InhomogeneousValue {
                        inhomogeneousParameter,
                        value,
                    })] => (inhomogeneousParameter, value),
                    _ => {
                        return Err(acc_unimplemented(
                            "InhomogeneousValue must contain a single InhomogeneousParameter",
                        ));
                    }
                };
                let gs = simple_ion(&known_ions, &mut result, ion, segmentGroup, erev)?;
                let mut ns = Map::new();
                if let Some(ihv) = inhomogeneous_parameters.get(ihv) {
                    ns.insert(
                        if param == "condDensity" {
                            String::from("conductance")
                        } else {
                            param.to_string()
                        },
                        MechVariableParameter {
                            param: ihv.clone(),
                            value: value.to_string(),
                        },
                    );
                } else {
                    return Err(nml2_error!(
                        "Inhomogeneous parameter definition {ihv} not found"
                    ));
                }
                result.push(Decor::new(
                    segmentGroup,
                    Paintable::NonUniformMech(ionChannel.to_string(), gs, ns),
                    true,
                ));
            }
            channelDensityNonUniformNernst(ChannelDensityNonUniformNernst {
                ionChannel,
                ion,
                body,
                ..
            }) => {
                use crate::neuroml::raw::ChannelDensityNonUniformNernstBody::variableParameter;
                use crate::neuroml::raw::VariableParameterBody::inhomogeneousValue;
                let (param, segmentGroup, ihb) = match &body[..] {
                    [variableParameter(VariableParameter {
                        parameter,
                        segmentGroup,
                        body: ihb,
                    })] => (parameter, segmentGroup, ihb),
                    _ => {
                        return Err(acc_unimplemented("ChannelDensityNonUniformNernst must contain a single VariableParameter"));
                    }
                };
                let (ihv, value) = match &ihb[..] {
                    [inhomogeneousValue(InhomogeneousValue {
                        inhomogeneousParameter,
                        value,
                    })] => (inhomogeneousParameter, value),
                    _ => {
                        return Err(acc_unimplemented(
                            "InhomogeneousValue must contain a single InhomogeneousParameter",
                        ));
                    }
                };
                let mut ns = Map::new();
                if let Some(ihv) = inhomogeneous_parameters.get(ihv) {
                    ns.insert(
                        if param == "condDensity" {
                            String::from("conductance")
                        } else {
                            param.to_string()
                        },
                        MechVariableParameter {
                            param: ihv.clone(),
                            value: value.to_string(),
                        },
                    );
                } else {
                    return Err(nml2_error!(
                        "Inhomogeneous parameter definition {ihv} not found"
                    ));
                }
                result.push(Decor::new(
                    "",
                    // TODO This is poison
                    Paintable::Em(ion.to_string(), "nernst".to_string()),
                    false,
                ));
                result.push(Decor::new(
                    segmentGroup,
                    Paintable::NonUniformMech(ionChannel.to_string(), Map::new(), ns),
                    true,
                ));
            }
            spikeThresh(_) => {}
            specificCapacitance(SpecificCapacitance {
                value,
                segmentGroup,
            }) => result.push(Decor::new(
                segmentGroup,
                Paintable::Cm(value.to_string()),
                false,
            )),
            initMembPotential(InitMembPotential {
                value,
                segmentGroup,
            }) => result.push(Decor::new(
                segmentGroup,
                Paintable::Vm(value.to_string()),
                false,
            )),
            channelPopulation(_)
            | channelDensityVShift(_)
            | channelDensityGHK(_)
            | channelDensityGHK2(_)
            | channelDensityNonUniformGHK(_) => {
                return Err(acc_unimplemented("Complex channel type"))
            }
        }
    }
    Ok(result)
}

fn intra(intra: &IntracellularProperties) -> Result<Vec<Decor>> {
    use IntracellularPropertiesBody::*;
    let mut result = Vec::new();
    for item in &intra.body {
        match &item {
            species(Species {
                ion, // the neuroml standard suggests selecting on id instead of ion
                initialConcentration,
                initialExtConcentration,
                segmentGroup,
                concentrationModel,
                ..
            }) if ion.is_some() => {
                let ion = ion.as_deref().unwrap();
                result.push(Decor::new(
                    segmentGroup,
                    Paintable::Xi(ion.to_string(), initialConcentration.to_string()),
                    false,
                ));
                result.push(Decor::new(
                    segmentGroup,
                    Paintable::Xo(ion.to_string(), initialExtConcentration.to_string()),
                    false,
                ));
                result.push(Decor::new(
                    segmentGroup,
                    Paintable::Mech(
                        concentrationModel.to_string(),
                        Map::from([(
                            String::from("initialConcentration"),
                            initialConcentration.to_string(),
                        )]),
                    ),
                    false,
                ));
            }
            resistivity(Resistivity {
                value,
                segmentGroup,
            }) => {
                result.push(Decor::new(
                    segmentGroup,
                    Paintable::Ra(value.to_string()),
                    false,
                ));
            }
            _ => {}
        }
    }
    Ok(result)
}

fn simple_ion(
    known_ions: &[String],
    result: &mut Vec<Decor>,
    ion: &str,
    group: &str,
    erev: &str,
) -> Result<Map<String, String>> {
    let mut gs = Map::new();
    if known_ions.contains(&ion.to_string()) {
        match result
            .iter()
            .find(|x| matches!(x, Decor::Paint(r, Paintable::Er(i, _e)) if r == group && ion == i))
        {
            None => {
                result.push(Decor::new(
                    group,
                    Paintable::Er(ion.to_string(), erev.to_string()),
                    false,
                ));
            }
            Some(Decor::Paint(_, Paintable::Er(_, e))) if e == erev => {
                // do nothing
            }
            Some(Decor::Paint(_, Paintable::Er(_, e))) if e != erev => {
                return Err(nml2_error!(
                    "Overwriting different Er[{}] on {} erev {}.",
                    ion,
                    group,
                    erev
                ));
            }
            Some(_) => {
                unreachable!()
            }
        }
    } else {
        gs.insert(
            format!("e{}", if ion == "non_specific" { "" } else { ion }),
            erev.to_string(),
        );
    }
    Ok(gs)
}

fn extra(_: &ExtracellularProperties) -> Result<Vec<Decor>> {
    warn!("Not handling extracellular settings, if required please file an issue.");
    Ok(Vec::new())
}

#[test]
fn test_simple_ion() {
    let known_ions = ["k".to_string()];
    let mut result = Vec::new();
    assert!(simple_ion(&known_ions, &mut result, "k", "soma", "-80").is_ok());
    assert!(result.len() == 1);
    assert!(simple_ion(&known_ions, &mut result, "k", "soma", "-80").is_ok());
    assert!(result.len() == 1);
    assert!(simple_ion(&known_ions, &mut result, "k", "soma", "-90").is_err());
    assert!(result.len() == 1);
    assert!(simple_ion(&known_ions, &mut result, "k", "dend", "-90").is_ok());
    assert!(result.len() == 2);
    assert!(simple_ion(&known_ions, &mut result, "na", "soma", "-90")
        .and_then(|gs| if gs.len() == 1 {
            Ok(())
        } else {
            Err(nml2_error!("non_specific"))
        })
        .is_ok());
    assert!(result.len() == 2);
}
