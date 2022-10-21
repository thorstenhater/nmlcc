use crate::{
    error::{Error, Result},
    expr::Quantity,
    lems::file::LemsFile,
    neuroml::{
        process_files,
        raw::{
            BiophysicalProperties, BiophysicalPropertiesBody, ChannelDensity, ChannelDensityNernst,
            ExtracellularProperties, InitMembPotential, IntracellularProperties,
            IntracellularPropertiesBody, MembraneProperties, MembranePropertiesBody, Resistivity,
            Species, SpecificCapacitance,
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
                for bpp in node.descendants() {
                    if bpp.tag_name().name() != "biophysicalProperties" {
                        continue;
                    }
                    let prop: BiophysicalProperties = XML::from_node(&bpp);
                    result.append(&mut biophys(&prop, lems)?);
                }
                *cells.entry(id.to_string()).or_default() = result;
            }
        }
        Ok(())
    })?;
    Ok(cells)
}

pub fn export(lems: &LemsFile, nml: &[String], pfx: &str) -> Result<()> {
    trace!("Creating path {}", pfx);
    std::fs::create_dir_all(&pfx)?;

    let cells = to_decor(lems, nml)?;
    for (cell, decor) in cells {
        let mut file = PathBuf::from(pfx);
        file.push(cell);
        file.set_extension("acc");
        info!("Writing ACC to {:?}", &file);
        write(&file, decor.to_sexp())?;
    }
    Ok(())
}

fn acc_unimplemented(f: &str) -> Error {
    Error::Acc {
        what: format!("Feature '{}' not implemented for ACC export.", f),
    }
}

pub trait Sexp {
    fn to_sexp(&self) -> String;
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
        };
        Ok(r)
    }
}

impl Sexp for Paintable {
    fn to_sexp(&self) -> String {
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
                let mut result = format!("(density (mechanism \"{}\"", m);
                for (k, v) in gs.iter() {
                    let x = format!(" (\"{}\" {})", k, v);
                    result.push_str(&x);
                }
                result.push(')');
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
    fn to_sexp(&self) -> String {
        match self {
            Decor::Default(i) => format!("(default {})", i.to_sexp()),
            Decor::Paint(r, i) => format!("(paint (region \"{}\") {})", r, i.to_sexp()),
        }
    }
}

impl Sexp for Vec<Decor> {
    fn to_sexp(&self) -> String {
        let mut result = String::from(
            "(arbor-component
  (meta-data (version \"0.1-dev\"))
  (decor
",
        );
        for it in self {
            writeln!(result, "    {}", it.to_sexp()).unwrap();
        }
        result.pop();
        result.push_str("))\n");
        result
    }
}

pub fn biophys(prop: &BiophysicalProperties, lems: &LemsFile) -> Result<Vec<Decor>> {
    use BiophysicalPropertiesBody::*;
    let mut decor = Vec::new();
    for item in &prop.body {
        match item {
            membraneProperties(m) => decor.append(&mut membrane(m)?),
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

fn membrane(membrane: &MembraneProperties) -> Result<Vec<Decor>> {
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
            | channelDensityNonUniform(_)
            | channelDensityNonUniformNernst(_)
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
                ion,
                initialConcentration,
                initialExtConcentration,
                segmentGroup,
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
        for item in result.iter() {
            if let Decor::Paint(r, Paintable::Er(i, e)) = item {
                // We are trying to add the same key with a...
                if r == group && ion == i {
                    return if e == erev {
                        // ... matching value: SKIP
                        Ok(gs)
                    } else {
                        // ... mismatch: ERROR
                        Err(nml2_error!("Overwriting different Er[{ion}] on {group}."))
                    };
                }
            }
        }
        // New value: add it.
        result.push(Decor::new(
            group,
            Paintable::Er(ion.to_string(), erev.to_string()),
            false,
        ));
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
