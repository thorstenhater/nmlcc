#![allow(dead_code, unused)]

use tracing::{trace, warn};

use crate::{
    error::{nml2_error, Result},
    expr::{Match, Path, Quantity, Select},
    instance::Instance,
    lems::file::LemsFile,
    neuroml::raw,
    Map,
};

#[derive(Clone, Debug)]
pub struct Network {
    pub temperature: f64,
    pub inputs: Vec<Input>,
    pub populations: Map<String, Population>,
    pub projections: Vec<Projection>,
}

#[derive(Clone, Debug)]
pub struct Input {
    /// id of source
    pub source: String,
    /// path to target component
    pub target: String,
    /// segment in target, default to 0
    pub segment: i64,
    /// fraction of segment to target, default 0.5
    pub fraction: f64,
}

#[derive(Clone, Debug)]
pub struct Loc {
    cell: i64,
    segment: i64,
    fraction: f64, // 0-1
}

#[derive(Clone, Debug)]
pub struct Connection {
    from: Loc,
    to: Loc,
    weight: f64,
}

#[derive(Clone, Debug)]
pub struct Projection {
    synapse: String,
    pre: String,
    post: String,
    connections: Vec<Connection>,
}

#[derive(Clone, Debug)]
pub struct Population {
    pub component: String,
    pub members: Vec<usize>,
}

impl Network {
    pub fn new(inst: &Instance) -> Result<Self> {
        let ty = inst.component_type.name.as_str();
        let id = inst
            .id
            .as_deref()
            .ok_or(nml2_error("Instance has no id attribute."))?
            .to_string();
        let temperature = if let Some(t) = inst.attributes.get("temperature") {
            t.parse::<f64>()
                .map_err(|_| nml2_error(format!("Could not parse T='{}'", t)))?
        } else {
            warn!("No temperature given, resorting to 0K!");
            0.0
        };

        let populations = if let Some(pops) = inst.children.get("populations") {
            get_populations(pops)?
        } else {
            Map::new()
        };
        let inputs = if let Some(inps) = inst.children.get("inputs") {
            get_inputs(inps)?
        } else {
            Vec::new()
        };
        let projections = if let Some(prjs) = inst.children.get("projections") {
            get_projections(prjs)?
        } else {
            Vec::new()
        };
        Ok(Network {
            temperature,
            inputs,
            populations,
            projections,
        })
    }
}

// TODO Do something with Spike inputs
fn get_inputs(inps: &[Instance]) -> Result<Vec<Input>> {
    let mut inputs = Vec::new();
    for inp in inps {
        let ty = inp.component_type.name.as_str();
        match ty {
            "inputList" => {
                let source = inp.attributes.get("component").unwrap().to_string();
                if let Some(xs) = inp.children.get("inputs") {
                    for x in xs {
                        let attr = &x.attributes;
                        let fraction = attr
                            .get("fractionAlong")
                            .map(|s| s.parse::<f64>().unwrap())
                            .unwrap_or(0.5);
                        let segment = attr
                            .get("segmentId")
                            .map(|s| s.parse::<i64>().unwrap())
                            .unwrap_or(0);
                        let target = attr
                            .get("target")
                            .ok_or(nml2_error("No target in input."))?
                            .to_string();
                        inputs.push(Input {
                            fraction,
                            segment,
                            target,
                            source: source.clone(),
                        });
                    }
                }
            }
            "explicitInput" => {
                warn!("Using 'ExplicitInput' is discouraged. Treated as targetting segment=0 fraction=0.5.");
                let fraction = 0.5;
                let segment = 0;
                let target = inp
                    .attributes
                    .get("target")
                    .ok_or(nml2_error("No target in explicitInput."))?
                    .to_string();
                let source = inp
                    .attributes
                    .get("input")
                    .ok_or(nml2_error("No input in explicitInput."))?
                    .to_string();
                inputs.push(Input {
                    fraction,
                    segment,
                    target,
                    source,
                });
            }
            t => return Err(nml2_error(format!("Unknown projections type '{}'.", t))),
        }
    }
    Ok(inputs)
}

fn get_populations(pops: &[Instance]) -> Result<Map<String, Population>> {
    let mut populations = Map::new();
    for pop in pops {
        let id = pop
            .id
            .as_deref()
            .ok_or(nml2_error("Population has no id."))?
            .to_string();
        let ty = pop.component_type.name.as_str();
        let members = match ty {
            "population" => {
                let size = pop
                    .parameters
                    .get("size")
                    .ok_or(nml2_error(format!("Population {} has no size.", id)))?
                    .value as usize;
                (0..size).collect()
            }
            "populationList" => {
                let mut ms = Vec::new();
                for i in pop.children.get("instances").unwrap_or(&vec![]) {
                    let ix =
                        i.id.as_deref()
                            .ok_or(nml2_error("Instance without id"))?
                            .parse()
                            .map_err(|_| nml2_error("Could not parse integral."))?;
                    ms.push(ix);
                }
                ms
            }
            t => {
                return Err(nml2_error(format!(
                    "Unknown population type '{}' for id '{}'",
                    t, id
                )))
            }
        };
        let component = pop
            .attributes
            .get("component")
            .ok_or(nml2_error(format!("Population {} without component", id)))?
            .to_string();
        populations.insert(id, Population { component, members });
    }
    Ok(populations)
}

pub fn get_cell_id(cell: &str) -> Result<(String, i64)> {
    let path = Match::parse(cell)?;
    match &path.0[..] {
        [.., Path::Fixed(p), Path::Fixed(ix), Path::Fixed(_)] => Ok((
            p.clone(),
            ix.parse::<i64>()
                .map_err(|_| nml2_error(format!("CellId '{}' is invalid", cell)))?,
        )),
        [.., Path::When(p, Select::Index(ix))] => Ok((p.clone(), *ix as i64)),
        _ => Err(nml2_error(format!("CellId '{}' is invalid", cell))),
    }
}

fn get_projections(prjs: &[Instance]) -> Result<Vec<Projection>> {
    let mut projections = Vec::new();
    for prj in prjs {
        let id = prj
            .id
            .as_deref()
            .ok_or(nml2_error("Projection has no id."))?
            .to_string();
        let ty = prj.component_type.name.as_str();
        if "projections" != ty {
            return Err(nml2_error("Projection has no id."));
        }
        if let Some(conns) = prj.children.get("connections") {
            for conn in conns {
                let attr = &conn.attributes;
                let pre = {
                    let fraction = attr
                        .get("preFractionAlong")
                        .map(|s| s.parse::<f64>().unwrap())
                        .unwrap_or(0.5);
                    let cell = attr
                        .get("preCellId")
                        .map(|s| s.parse::<i64>().unwrap())
                        .unwrap_or(0);
                    let segment = attr
                        .get("preSegmentId")
                        .map(|s| s.parse::<i64>().unwrap())
                        .unwrap_or(0);
                    Loc {
                        cell,
                        segment,
                        fraction,
                    }
                };
                let post = {
                    let fraction = attr
                        .get("postFractionAlong")
                        .map(|s| s.parse::<f64>().unwrap())
                        .unwrap_or(0.5);
                    let cell = attr
                        .get("postCellId")
                        .map(|s| s.parse::<i64>().unwrap())
                        .unwrap_or(0);
                    let segment = attr
                        .get("postSegmentId")
                        .map(|s| s.parse::<i64>().unwrap())
                        .unwrap_or(0);
                    Loc {
                        cell,
                        segment,
                        fraction,
                    }
                };
                let segment = attr
                    .get("weight")
                    .map(|s| s.parse::<f64>().unwrap())
                    .unwrap_or(0.0);
            }
        }
    }
    Ok(projections)
}
