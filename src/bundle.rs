use crate::{
    acc::{self, Decor, Paintable, ParsedInhomogeneousParameter, Sexp},
    error::{Error, Result},
    expr::{Expr, Quantity, Stmnt},
    instance::{Collapsed, Context, Instance},
    lems::file::LemsFile,
    network::{self, get_cell_id, Connection, Network, Projection},
    neuroml::raw::{
        BiophysicalProperties, BiophysicalPropertiesBody, ChannelDensity, MembranePropertiesBody,
        PoissonFiringSynapse, PulseGenerator,
    },
    neuroml::{
        process_files,
        raw::{ChannelDensityNernst, ChannelDensityNonUniform, ChannelDensityNonUniformNernst},
    },
    nml2_error,
    nmodl::{self, Nmodl},
    xml::XML,
    Map, Set,
};
use serde::Serialize;
use serde_json;
use std::fs::{create_dir_all, write};
use tracing::{info, trace};

static RUN_SH: &str = include_str!(r"../dat/run.sh");
static CMAKE: &str = include_str!(r"../dat/CMakeLists.txt");
static MAIN_CXX: &str = include_str!(r"../dat/main.cxx");
static MAIN_PY: &str = include_str!(r"../dat/main.py");

pub struct Bundle {
    pub dir: String,
    pub cxx: bool,
    pub py: bool,
    pub super_mechanisms: bool,
    pub cat_prefix: String,
}

pub fn export(lems: &LemsFile, nml: &[String], ions: &[String], cfg: Bundle) -> Result<()> {
    export_template(lems, nml, &cfg.dir)?;

    // We always export these to keep synapse etc alive
    nmodl::export(lems, nml, "-*", &format!("{}/cat", &cfg.dir), ions)?;

    if cfg.super_mechanisms {
        export_with_super_mechanisms(lems, nml, &cfg.dir, ions, &cfg.cat_prefix)?;
    } else {
        acc::export(
            lems,
            nml,
            &format!("{}/acc", &cfg.dir),
            ions,
            &cfg.cat_prefix,
        )?;
    }
    if cfg.py {
        write(format!("{}/main.py", &cfg.dir), MAIN_PY)?;
    }
    if cfg.cxx {
        write(format!("{}/run.sh", &cfg.dir), RUN_SH)?;
        write(format!("{}/CMakeLists.txt", &cfg.dir), CMAKE)?;
        write(format!("{}/main.cxx", &cfg.dir), MAIN_CXX)?;
    }
    Ok(())
}

type ConnectionData = (i64, String, String, String, f64, f64);

#[derive(Serialize)]
struct SimulationData {
    gid_to_cell: Vec<String>,
    cell_to_morph: Map<String, String>,
    gid_to_inputs: Map<i64, Vec<(i64, String, String)>>,
    gid_to_synapses: Map<i64, Vec<(i64, String, String)>>,
    gid_to_detectors: Map<i64, Vec<(i64, String, f64)>>,
    gid_to_connections: Map<i64, Vec<ConnectionData>>,
    // Inputs & Stimuli
    i_clamps: Map<String, (f64, f64, f64)>,
    regular_generators: Map<String, ()>,
    poisson_generators: Map<String, (String, f64, f64)>,
    // cells
    count: usize,
}

impl SimulationData {
    fn new(
        lems: &LemsFile,
        cell_to_morph: &Map<String, String>,
        cell_to_threshold: &Map<String, f64>,
        stimuli: &Map<String, Input>,
        net: &Network,
    ) -> Result<Self> {
        let mut gid = 0;
        let mut gid_to_cell = Vec::new();
        let mut pop_to_gid = Map::new();
        for (id, pop) in &net.populations {
            pop_to_gid.insert(id.clone(), gid);
            let cell = pop.component.to_string();
            for _ in &pop.members {
                gid_to_cell.push(cell.clone());
                gid += 1;
            }
        }
        let count = gid;

        let mut gid_to_inputs = Map::new();
        let mut gid_to_synapses: Map<_, Vec<_>> = Map::new();
        for network::Input {
            source,
            target,
            segment,
            fraction,
        } in &net.inputs
        {
            let (pop, id) = get_cell_id(target)?;
            let fst = pop_to_gid[&pop];
            let idx = if let Some(p) = net.populations.get(&pop) {
                p.members
                    .iter()
                    .position(|ix| id == *ix as i64)
                    .ok_or_else(|| nml2_error!("Bad index {id} in population {pop}."))?
            } else {
                return Err(nml2_error!("Indexing into an unknown population: {pop}."));
            };
            let gid: i64 = (fst + idx) as i64;
            let val = (*segment, fraction.to_string(), source.clone());
            gid_to_inputs.entry(gid).or_insert_with(Vec::new).push(val);
            match stimuli.get(source) {
                Some(Input::Poisson(synapse, _, _)) => {
                    gid_to_synapses.entry(gid).or_default().push((
                        *segment,
                        fraction.clone(),
                        synapse.clone(),
                    ));
                }
                None | Some(Input::Pulse(..)) => {}
            }
        }

        let mut gid_to_detectors: Map<_, Vec<_>> = Map::new();
        let mut gid_to_connections = Map::new();
        for Projection {
            synapse,
            pre,
            post,
            connections,
        } in &net.projections
        {
            let pre_gid = pop_to_gid[pre] as i64;
            let post_gid = pop_to_gid[post] as i64;
            for Connection {
                from,
                to,
                weight,
                delay,
            } in connections
            {
                let pre_cell_id = &net.populations[pre].component;
                let threshold = cell_to_threshold[pre_cell_id];
                let from_gid = pre_gid + from.cell;
                let to_gid = post_gid + to.cell;
                gid_to_detectors.entry(from_gid).or_default().push((
                    from.segment,
                    from.fraction.clone(),
                    threshold,
                ));
                gid_to_synapses.entry(to_gid).or_default().push((
                    to.segment,
                    to.fraction.clone(),
                    synapse.clone(),
                ));
                let weight = lems.normalise_quantity(weight)?.value;
                let delay = lems.normalise_quantity(delay)?.value;
                gid_to_connections
                    .entry(to_gid)
                    .or_insert_with(Vec::new)
                    .push((
                        from_gid,
                        from.to_label(),
                        synapse.to_string(),
                        to.to_label(),
                        weight,
                        delay,
                    ));
            }
        }

        let mut i_clamps = Map::new();
        let mut poisson_generators = Map::new();
        let regular_generators = Map::new();
        for (lbl, stimulus) in stimuli {
            match stimulus {
                Input::Pulse(delay, dt, stop) => {
                    i_clamps.insert(lbl.to_string(), (*delay, *dt, *stop));
                }
                Input::Poisson(syn, avg, wgt) => {
                    poisson_generators.insert(lbl.to_string(), (syn.to_string(), *avg, *wgt));
                }
            }
        }

        Ok(SimulationData {
            gid_to_cell,
            cell_to_morph: cell_to_morph.clone(),
            gid_to_inputs,
            gid_to_synapses,
            gid_to_detectors,
            gid_to_connections,
            i_clamps,
            regular_generators,
            poisson_generators,
            count,
        })
    }
}

fn mk_mrf(id: &str, mrf: &str) -> String {
    format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>

<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2  https://raw.githubusercontent.com/NeuroML/NeuroML2/master/Schemas/NeuroML2/NeuroML_v2beta3.xsd"
    id="{id}">
   {mrf}
</neuroml>
"#
    )
}

#[derive(Debug)]
#[non_exhaustive]
enum Input {
    Pulse(f64, f64, f64),
    Poisson(String, f64, f64),
}

fn export_template(lems: &LemsFile, nml: &[String], bundle: &str) -> Result<()> {
    trace!("Creating bundle {bundle}");
    create_dir_all(bundle)?;
    create_dir_all(format!("{bundle}/mrf"))?;
    create_dir_all(format!("{bundle}/acc"))?;
    create_dir_all(format!("{bundle}/cat"))?;
    create_dir_all(format!("{bundle}/dat"))?;

    let norm = |v: &str| -> Result<String> {
        let q = Quantity::parse(v)?;
        let u = lems.normalise_quantity(&q)?.value;
        Ok(format!("{u}"))
    };

    let mut inputs = Map::new();
    let mut cells = Map::new();
    let mut thresholds = Map::new();
    let mut nets = Vec::new();
    process_files(nml, |_, node| {
        let doc = node.document().input_text();
        match node.tag_name().name() {
            "morphology" => {
                let cell = node
                    .parent()
                    .ok_or(nml2_error!("Morphology has no parent"))?;
                if cell.tag_name().name() != "cell" {
                    return Err(nml2_error!("Morphology must have cell as parent"));
                }
                let morph_id = node.attribute("id").ok_or(nml2_error!("Morph has no id"))?;
                let cell_id = cell.attribute("id").ok_or(nml2_error!("Cell has no id"))?;
                let id = &format!("{cell_id}_{morph_id}");
                trace!("Writing morphology to {bundle}/mrf/{id}");
                write(
                    format!("{bundle}/mrf/{id}.nml"),
                    mk_mrf(id, &doc[node.range()]),
                )?;
            }
            "cell" => {
                let cell = node
                    .attribute("id")
                    .ok_or_else(|| nml2_error!("Cell has no id"))?;
                for mrf in node.children() {
                    if mrf.tag_name().name() == "morphology" {
                        let morph = mrf
                            .attribute("id")
                            .ok_or_else(|| nml2_error!("Morph has no id"))?;
                        cells.insert(cell.to_string(), morph.to_string());
                    }
                }
                for spk in node.descendants() {
                    if spk.tag_name().name() == "spikeThresh" {
                        let val = spk
                            .attribute("value")
                            .ok_or_else(|| nml2_error!("SpikeThresh has no value"))?;
                        let val = Quantity::parse(val)?;
                        thresholds.insert(cell.to_string(), val.value);
                    }
                }
            }
            "pulseGenerator" => {
                let ic: PulseGenerator = XML::from_node(node);
                inputs.insert(
                    ic.id.to_string(),
                    Input::Pulse(
                        norm(&ic.delay)?.parse::<f64>().unwrap(),
                        norm(&ic.duration)?.parse::<f64>().unwrap(),
                        norm(&ic.amplitude)?.parse::<f64>().unwrap(),
                    ),
                );
            }
            "poissonFiringSynapse" => {
                let ic: PoissonFiringSynapse = XML::from_node(node);
                let weight = 1.0;
                inputs.insert(
                    ic.id.to_string(),
                    Input::Poisson(
                        ic.synapse,
                        norm(&ic.averageRate)?.parse::<f64>().unwrap(),
                        weight,
                    ),
                );
            }
            "network" => {
                let inst = Instance::new(lems, node)?;
                let net = Network::new(&inst)?;
                nets.push(net);
            }
            _ => {}
        }
        Ok(())
    })?;

    for net in &nets {
        write(
            format!("{bundle}/dat/{}.json", net.name),
            serde_json::to_string_pretty(&SimulationData::new(
                lems,
                &cells,
                &thresholds,
                &inputs,
                net,
            )?)
            .unwrap(),
        )?;
    }
    Ok(())
}

#[derive(Clone, Debug)]
enum RevPot {
    Const(Quantity),
    Nernst,
}

#[derive(Clone, Debug)]
enum IonChannelConductanceParameter {
    DefaultConductance, // NonUniform doesn't give it
    FixedConductance(Quantity),
}

impl IonChannelConductanceParameter {
    fn parse(lems: &LemsFile, g: &Option<String>) -> Result<Self> {
        Ok(if let Some(g) = g {
            Self::FixedConductance(lems.normalise_quantity(&Quantity::parse(g)?)?)
        } else {
            Self::DefaultConductance
        })
    }
}

#[derive(Clone, Debug)]
pub struct IonChannel {
    name: String,
    reversal_potential: RevPot,
    conductance: IonChannelConductanceParameter,
}

pub fn export_with_super_mechanisms(
    lems: &LemsFile,
    nml: &[String],
    bundle: &str,
    ions: &[String],
    cat_prefix: &str,
) -> Result<()> {
    let cells = read_cell_data(nml, lems)?;
    let merge = build_super_mechanisms(&cells, lems, ions)?;

    for (id, cell) in merge {
        for (reg, chan) in cell.channels {
            let nmodl = nmodl::mk_nmodl(chan)?;
            let path = format!("{bundle}/cat/{id}_{reg}.mod");
            info!("Writing Super-Mechanism NMODL for cell '{id}' region '{reg}' to {path:?}",);
            write(&path, nmodl)?;
        }
        let path = format!("{bundle}/acc/{id}.acc");
        info!("Writing Super Mechanism ACC to {path:?}");
        let decor = cell
            .decor
            .iter()
            .map(|d| d.add_catalogue_prefix(cat_prefix))
            .collect::<Vec<_>>();
        write(&path, decor.to_sexp())?;
    }
    Ok(())
}

#[derive(Default)]
pub struct Cell {
    pub decor: Vec<Decor>,
    pub channels: Vec<(String, Nmodl)>,
}

pub fn build_super_mechanisms(
    cells: &CellData,
    lems: &LemsFile,
    ions: &[String],
) -> Result<Map<String, Cell>> {
    let sms = ion_channel_assignments(&cells.bio_phys, lems)?;
    let dec = split_decor(cells, lems, ions)?;
    let mrg = merge_ion_channels(&sms, cells, ions)?;

    Ok(dec
        .into_iter()
        .map(|(cell, decor)| {
            let channels = mrg.get(&cell).cloned().unwrap_or_default();
            (cell, Cell { decor, channels })
        })
        .collect())
}

#[derive(Default)]
pub struct CellData {
    pub bio_phys: Map<String, BiophysicalProperties>,
    pub density: Vec<Instance>,
    pub synapse: Vec<Instance>,
    pub c_model: Vec<Instance>,
    pub i_param: Map<String, Map<String, ParsedInhomogeneousParameter>>,
}

fn read_cell_data(nml: &[String], lems: &LemsFile) -> Result<CellData> {
    let mut result = CellData::default();
    process_files(nml, |_, node| {
        let tag = node.tag_name().name();
        if tag == "cell" {
            let id = node.attribute("id").ok_or(nml2_error!("Cell without id"))?;
            result
                .i_param
                .insert(id.to_string(), acc::parse_inhomogeneous_parameters(node)?);
            node.children()
                .find(|c| c.tag_name().name() == "biophysicalProperties")
                .into_iter()
                .for_each(|p| {
                    result.bio_phys.insert(id.to_string(), XML::from_node(&p));
                });
        } else if lems.derived_from(tag, "baseIonChannel") {
            result.density.push(Instance::new(lems, node)?);
        } else if lems.derived_from(tag, "concentrationModel") {
            result.c_model.push(Instance::new(lems, node)?);
        } else if lems.derived_from(tag, "baseSynapse") {
            result.synapse.push(Instance::new(lems, node)?);
        }
        Ok(())
    })?;
    Ok(result)
}

pub fn ion_channel_assignments(
    props: &Map<String, BiophysicalProperties>,
    lems: &LemsFile,
) -> Result<Map<(String, String), Vec<IonChannel>>> {
    use BiophysicalPropertiesBody::*;
    use MembranePropertiesBody::*;
    fn segment_group_or_all(x: &str) -> String {
        if x.is_empty() {
            String::from("all")
        } else {
            x.to_string()
        }
    }
    let mut result: Map<_, Vec<IonChannel>> = Map::new();
    for (id, prop) in props.iter() {
        for item in prop.body.iter() {
            if let membraneProperties(membrane) = item {
                for item in membrane.body.iter() {
                    match item {
                        channelDensity(ChannelDensity {
                            ionChannel,
                            condDensity: g,
                            erev,
                            segmentGroup,
                            ..
                        }) => {
                            let region = segment_group_or_all(segmentGroup);
                            let name = ionChannel.to_string();
                            let conductance = IonChannelConductanceParameter::parse(lems, g)?;
                            let reversal_potential =
                                RevPot::Const(lems.normalise_quantity(&Quantity::parse(erev)?)?);
                            result
                                .entry((id.to_string(), region))
                                .or_default()
                                .push(IonChannel {
                                    name,
                                    reversal_potential,
                                    conductance,
                                });
                        }
                        channelDensityNernst(ChannelDensityNernst {
                            ionChannel,
                            condDensity: g,
                            segmentGroup,
                            ..
                        }) => {
                            let region = segment_group_or_all(segmentGroup);
                            let name = ionChannel.to_string();
                            let conductance = IonChannelConductanceParameter::parse(lems, g)?;
                            let reversal_potential = RevPot::Nernst;
                            result
                                .entry((id.to_string(), region))
                                .or_default()
                                .push(IonChannel {
                                    name,
                                    reversal_potential,
                                    conductance,
                                });
                        }
                        channelDensityNonUniform(ChannelDensityNonUniform {
                            ionChannel,
                            body,
                            erev,
                            ..
                        }) => {
                            use crate::neuroml::raw::ChannelDensityNonUniformBody::variableParameter;
                            let variableParameter(vp) = &body.first().ok_or(nml2_error!(
                                "expected VariableParameter in ChannelDensityNonUniform"
                            ))?;
                            let name = ionChannel.to_string();
                            let region = segment_group_or_all(&vp.segmentGroup);
                            let conductance = IonChannelConductanceParameter::DefaultConductance;
                            let reversal_potential =
                                RevPot::Const(lems.normalise_quantity(&Quantity::parse(erev)?)?);
                            result
                                .entry((id.to_string(), region))
                                .or_default()
                                .push(IonChannel {
                                    name,
                                    reversal_potential,
                                    conductance,
                                });
                        }
                        channelDensityNonUniformNernst(ChannelDensityNonUniformNernst {
                            ionChannel,
                            body,
                            ..
                        }) => {
                            use crate::neuroml::raw::ChannelDensityNonUniformNernstBody::variableParameter;
                            let variableParameter(vp) = &body.first().ok_or(nml2_error!(
                                "expected VariableParameter in ChannelDensityNonUniform"
                            ))?;
                            let region = segment_group_or_all(&vp.segmentGroup);
                            let name = ionChannel.to_string();
                            let conductance = IonChannelConductanceParameter::DefaultConductance;
                            let reversal_potential = RevPot::Nernst;
                            result
                                .entry((id.to_string(), region))
                                .or_default()
                                .push(IonChannel {
                                    name,
                                    reversal_potential,
                                    conductance,
                                });
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    Ok(result)
}

fn split_decor(
    cells: &CellData,
    lems: &LemsFile,
    ions: &[String],
) -> Result<Map<String, Vec<Decor>>> {
    // Now splat in the remaining decor, but skip density mechs (these were merged)
    let densities = cells
        .density
        .iter()
        .map(|m| m.id.as_deref().unwrap().to_string())
        .collect::<Set<String>>();
    let ihp = &cells.i_param;
    let mut result: Map<String, Vec<Decor>> = Map::new();
    for (id, prop) in cells.bio_phys.iter() {
        let mut seen = Set::new();
        let mut sm = Vec::new();
        let biophys = acc::biophys(
            prop,
            lems,
            ions,
            ihp.get(id).ok_or(nml2_error!("should never happen"))?,
        )?;
        let mut non_uniform_args: Map<String, Map<String, acc::MechVariableParameter>> = Map::new();
        // collect non uniform args as they must be kept as PARAMETERS
        for d in biophys.decor.iter() {
            match d {
                Decor::Paint(ref r, Paintable::NonUniformMech { ref name, ns, .. })
                    if densities.contains(name) =>
                {
                    for (k, v) in ns.iter() {
                        non_uniform_args
                            .entry(r.to_owned())
                            .or_insert_with(Map::new)
                            .insert(format!("{name}_{k}"), v.to_owned());
                    }
                }
                _ => (),
            }
        }
        for d in biophys.decor {
            match d {
                Decor::Paint(r, Paintable::Mech(name, _)) if densities.contains(&name) => {
                    if !seen.contains(&r) {
                        if let Some(ns) = non_uniform_args.get(&r) {
                            sm.push(acc::Decor::non_uniform_mechanism(
                                &r,
                                &format!("{id}_{r}"),
                                &Map::new(),
                                ns,
                            ));
                        } else {
                            sm.push(acc::Decor::mechanism(&r, &format!("{id}_{r}"), &Map::new()));
                        }
                        seen.insert(r.to_string());
                    }
                }
                Decor::Paint(r, Paintable::NonUniformMech { name, .. })
                    if densities.contains(&name) =>
                {
                    if !seen.contains(&r) {
                        if let Some(ns) = non_uniform_args.get(&r) {
                            sm.push(acc::Decor::non_uniform_mechanism(
                                &r,
                                &format!("{id}_{r}"),
                                &Map::new(),
                                ns,
                            ));
                        } else {
                            panic!("no");
                        }
                        seen.insert(r.to_string());
                    }
                }
                _ => sm.push(d),
            }
        }
        result.insert(id.to_string(), sm);
    }
    Ok(result)
}

fn merge_ion_channels(
    channel_mappings: &Map<(String, String), Vec<IonChannel>>, // cell x region -> [channel]
    cell: &CellData,
    known_ions: &[String],
) -> Result<Map<String, Vec<(String, Nmodl)>>> {
    let mut result: Map<String, Vec<_>> = Map::new();
    let mut filter = String::from("-*");
    for ((id, reg), channels) in channel_mappings {
        let mut collapsed = Collapsed::new(&Some(format!("{id}_{reg}")));
        let mut ions: Map<_, Vec<_>> = Map::new();
        for channel in channels {
            for instance in cell.density.iter() {
                if instance.id == Some(channel.name.to_string()) {
                    let mut instance = instance.clone();
                    let ion = instance
                        .attributes
                        .get("species")
                        .cloned()
                        .unwrap_or_default();
                    // Set Parameters e, g
                    if let IonChannelConductanceParameter::FixedConductance(g) =
                        &channel.conductance
                    {
                        instance
                            .parameters
                            .insert(String::from("conductance"), g.clone());
                    } else {
                        filter.push_str(&format!(",+{}/conductance", channel.name));
                    }
                    if let RevPot::Const(q) = &channel.reversal_potential {
                        instance.parameters.insert(format!("e{ion}"), q.clone());
                        instance.component_type.parameters.push(format!("e{ion}"));
                    }
                    ions.entry(ion.clone())
                        .or_default()
                        .push(channel.name.clone());
                    collapsed.add(&instance, &Context::default(), None)?;
                }
            }
        }

        // Add iX
        let mut outputs = Map::new();
        let mut variables = Map::new();
        let mut ns = Vec::new();
        for (ion, mechs) in &ions {
            if !ion.is_empty() && known_ions.contains(ion) {
                // A non-empty, known ion species X maps to
                //   USEION X READ eX WRITE iX
                // and we can sum the conductivities M_g_X
                // where M is the mechanism
                // NOTE we can probably claim that an empty ion is never known...
                let ix = Stmnt::Ass(
                    format!("i{ion}"),
                    Expr::parse(&format!("g_{ion}*(v - e{ion})"))?,
                );
                outputs.insert(format!("i{ion}"), ix);
                let g = mechs
                    .iter()
                    .map(|m| format!("{m}_g"))
                    .collect::<Vec<_>>()
                    .join(" + ");
                let ig = Stmnt::Ass(format!("g_{ion}"), Expr::parse(&g)?);
                variables.insert(format!("g_{ion}"), ig);
            } else {
                // Anything else becomes
                //   NONSPECIFIC_CURRENT iX
                // and
                //   RANGE PARAMETER M_eX
                // where M is the mechanism's prefix
                // and we sum currents as sum(g_)
                let i = mechs
                    .iter()
                    .map(|m| format!("{m}_g*(v - {m}_e{ion})"))
                    .collect::<Vec<_>>()
                    .join(" + ");
                ns.push((format!("i{ion}"), i));
            }
        }

        match ns.as_slice() {
            [] => {}
            [(name, e)] => {
                let ix = Stmnt::Ass(name.to_owned(), Expr::parse(e)?);
                outputs.insert(name.to_owned(), ix);
            }
            ns => {
                // prevent outputting multiple NONSPECIFIC statements
                // collapse all into a single NONSPECIFIC_CURRENT i
                let i = ns
                    .iter()
                    .map(|(_, v)| v.to_owned())
                    .collect::<Vec<_>>()
                    .join(" + ");
                let ix = Stmnt::Ass(String::from("i"), Expr::parse(&i)?);
                outputs.insert(String::from("i"), ix);
            }
        }

        let mut n = nmodl::Nmodl::from(&collapsed, known_ions, &filter)?;
        n.add_outputs(&outputs);
        n.add_variables(&outputs);
        n.add_variables(&variables);
        result.entry(id.clone()).or_default().push((reg.clone(), n));
    }
    Ok(result)
}
