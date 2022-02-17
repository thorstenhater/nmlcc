use crate::{
    error::{Error, Result},
    expr::Quantity,
    lems::file::LemsFile,
    network::Network,
    neuroml::{
        process_files,
        raw::{
            self, BiophysicalProperties, BiophysicalPropertiesBody, ChannelDensity,
            ChannelDensityNernst, ExtracellularProperties, InitMembPotential,
            IntracellularProperties, IntracellularPropertiesBody, MembraneProperties,
            MembranePropertiesBody, PulseGenerator, Resistivity, Species, SpecificCapacitance,
        },
    },
    xml::XML,
    Map,
};

use std::fs::write;
use std::path::PathBuf;
use tracing::info;

pub fn to_decor(lems: &LemsFile, nml: &[String]) -> Result<Map<String, Vec<Decor>>> {
    let mut cells = Map::new();
    let mut placings: Vec<Decor> = Vec::new();
    let mut iclamps = Map::new();

    let norm = |v: &str| -> Result<String> {
        let q = Quantity::parse(v)?;
        let u = lems.normalise_quantity(&q)?;
        Ok(format!("{}", u.value))
    };

    process_files(nml, |_, node| {
        match node.tag_name().name() {
            "network" => {
                let net: raw::Network = XML::from_node(node);
                let net = Network::new(lems, &net)?;
                for input in &net.inputs {
                    placings.push(Decor::Place(
                        Locset::Segment(input.segment, input.fraction),
                        Placeable::IClamp(input.source.clone(), Envelope::Unresolved),
                    ));
                }
            }
            "pulseGenerator" => {
                let ic: PulseGenerator = XML::from_node(node);
                iclamps.insert(
                    ic.id.to_string(),
                    (
                        norm(&ic.delay)?.parse::<f64>().unwrap(),
                        norm(&ic.duration)?.parse::<f64>().unwrap(),
                        norm(&ic.amplitude)?.parse::<f64>().unwrap(),
                    ),
                );
            }
            "cell" => {
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
            _ => {}
        }
        Ok(())
    })?;

    // Fix iclamp <> envelope assoc
    for placing in placings.iter_mut() {
        if let Decor::Place(_, Placeable::IClamp(n, ref mut e)) = placing {
            if let Some((d, l, a)) = iclamps.get(n) {
                *e = Envelope::Pulse(*d, *l, *a);
            }
        }
    }

    // TODO(TH, correctness): Need to match up cells and networks via populations here.
    let mut result = Map::new();
    for (cell, decor) in cells {
        result
            .entry(cell)
            .or_insert_with(Vec::new)
            .extend(decor.iter().chain(placings.iter()).cloned());
    }
    Ok(result)
}

pub fn export(lems: &LemsFile, nml: &[String], pfx: &str) -> Result<()> {
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
pub enum Envelope {
    Unresolved,
    Pulse(f64, f64, f64),
}

impl Sexp for Envelope {
    fn to_sexp(&self) -> String {
        match self {
            Envelope::Unresolved => panic!("Cannot serialize unresolved envelopes."),
            Envelope::Pulse(d, l, a) => format!("(envelope-pulse {} {} {})", d, l, a),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Placeable {
    IClamp(String, Envelope),
}

#[derive(Clone, Debug)]
pub enum Locset {
    Segment(i64, f64),
}

impl Sexp for Locset {
    fn to_sexp(&self) -> String {
        match self {
            Locset::Segment(i, f) => format!("(on-components {} (segment {}))", f, i),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Decor {
    Default(Paintable),
    Paint(String, Paintable),
    Place(Locset, Placeable),
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
            _ => Ok(self.clone()),
        }
    }
}

impl Sexp for Decor {
    fn to_sexp(&self) -> String {
        match self {
            Decor::Default(i) => format!("(default {})", i.to_sexp()),
            Decor::Paint(r, i) => format!("(paint (region \"{}\") {})", r, i.to_sexp()),
            Decor::Place(l, p) => {
                let (q, n) = match p {
                    Placeable::IClamp(n, e) => (
                        format!("(current-clamp {} 0 0)", e.to_sexp()),
                        n.to_string(),
                    ),
                };
                format!("(place {} {} \"{}\")", l.to_sexp(), q, n)
            }
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
            result += &format!("    {}\n", it.to_sexp());
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
                let mut gs = Map::new();
                if let Some(g) = condDensity {
                    gs.insert(String::from("conductance"), g.clone());
                }
                if ion != "non_specific" {
                    result.push(Decor::new(
                        segmentGroup,
                        Paintable::Er(ion.to_string(), erev.to_string()),
                        false,
                    ));
                } else {
                    gs.insert(String::from("e"), erev.to_string());
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

fn extra(_: &ExtracellularProperties) -> Result<Vec<Decor>> {
    info!("Not handling extracellular settings, if required please file an issue.");
    Ok(Vec::new())
}
