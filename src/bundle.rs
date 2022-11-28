use crate::{
    acc::{self, Decor, Paintable, ParsedInhomogeneousParameter, Sexp},
    error::{Error, Result},
    expr::{Expr, Quantity, Stmnt},
    instance::{Collapsed, Context, Instance},
    lems::file::LemsFile,
    network::{get_cell_id, Connection, Input, Network, Projection},
    neuroml::raw::{
        BiophysicalProperties, BiophysicalPropertiesBody, ChannelDensity, MembranePropertiesBody,
        PulseGenerator,
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
use std::fmt::Write as _;
use std::fs::{create_dir_all, write};
use tracing::{info, trace};

pub fn export(
    lems: &LemsFile,
    nml: &[String],
    bundle: &str,
    use_super_mechs: bool,
    ions: &[String],
    cat_prefix: &str,
) -> Result<()> {
    export_template(lems, nml, bundle)?;

    // We always export these to keep synapse etc alive
    nmodl::export(lems, nml, "-*", &format!("{bundle}/cat"), ions)?;

    if use_super_mechs {
        export_with_super_mechanisms(lems, nml, bundle, ions, cat_prefix)?;
    } else {
        acc::export(lems, nml, &format!("{bundle}/acc"), ions, cat_prefix)?;
    }
    Ok(())
}

fn mk_main_py(
    cells: &[(String, String)],
    iclamps: &Map<String, (f64, f64, f64)>,
    net: &Network,
) -> Result<String> {
    let mut cell_to_morph = String::from("{");
    for (c, m) in cells {
        write!(cell_to_morph, "'{c}': '{m}', ").unwrap();
    }
    cell_to_morph.push('}');

    let mut count = 0;
    let mut pop_to_gid = Map::new();
    let mut gid_to_cell = String::from("[");
    let mut gid_to_pop = String::from("[");
    let mut inputs = Map::new();
    for (id, pop) in &net.populations {
        pop_to_gid.insert(id.clone(), count);
        for _ in &pop.members {
            count += 1;
            gid_to_cell.push_str(&format!("'{}', ", pop.component));
            gid_to_pop.push_str(&format!("'{id}', "));
        }
    }

    let mut labels = Map::new();
    for Input {
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
        let key: i64 = (fst + idx) as i64;
        let val = (source.clone(), segment, fraction);
        inputs.entry(key).or_insert_with(Vec::new).push(val);
        labels
            .entry(key)
            .or_insert_with(Set::new)
            .insert((*segment, fraction.clone()));
    }
    gid_to_cell.push(']');
    gid_to_pop.push(']');

    let mut gid_to_inputs = String::from("{");
    for (key, vals) in inputs {
        gid_to_inputs.push_str(&format!("\n                                 {key}: ["));
        for (src, seg, frac) in vals {
            gid_to_inputs.push_str(&format!("(\"seg_{seg}_frac_{frac}\", \"{src}\"), "));
        }
        gid_to_inputs.push_str("], ");
    }
    gid_to_inputs.push('}');

    let mut i_clamps = String::from("{");
    for (lbl, iclamp) in iclamps {
        i_clamps.push_str(&format!("'{lbl}': {iclamp:?}, "))
    }
    i_clamps.push('}');

    // In arbor
    let mut detectors = Map::new();
    let mut synapses = Map::new();
    let mut conns = Map::new();
    for Projection {
        synapse,
        pre,
        post,
        connections,
    } in &net.projections
    {
        let pre = pop_to_gid[pre] as i64;
        let post = pop_to_gid[post] as i64;
        for Connection {
            from,
            to,
            weight,
            delay,
        } in connections
        {
            let from_gid = pre + from.cell;
            let to_gid = post + to.cell;
            labels
                .entry(from_gid)
                .or_insert_with(Set::new)
                .insert((from.segment, from.fraction.clone()));
            labels
                .entry(to_gid)
                .or_insert_with(Set::new)
                .insert((to.segment, to.fraction.clone()));
            detectors
                .entry(from_gid)
                .or_insert_with(Set::new)
                .insert(from.to_label());
            synapses
                .entry(to_gid)
                .or_insert_with(Set::new)
                .insert((to.to_label(), synapse.clone()));
            conns.entry(to_gid).or_insert_with(Vec::new).push((
                from_gid,
                from.to_label(),
                synapse.to_string(),
                to.to_label(),
                weight,
                delay,
            ));
        }
    }

    let mut gid_to_synapses = String::from("{\n");
    for (gid, vs) in &synapses {
        gid_to_synapses.push_str(&format!("                                  {gid}: ["));
        for (t, s) in vs {
            gid_to_synapses.push_str(&format!("(\"{t}\", \"{s}\"), "));
        }
        gid_to_synapses.push_str("],\n");
    }
    gid_to_synapses.push_str("                               }");

    let mut gid_to_detectors = String::from("{\n");
    for (gid, vs) in &detectors {
        gid_to_detectors.push_str(&format!("                                  {gid}: ["));
        for v in vs {
            gid_to_detectors.push_str(&format!("\"{v}\", "));
        }
        gid_to_detectors.push_str("],\n");
    }
    gid_to_detectors.push_str("                               }");

    let mut gid_to_connections = String::from("{\n");
    for (gid, vs) in &conns {
        gid_to_connections.push_str(&format!("                                {gid}: ["));
        for (fgid, floc, syn, tloc, weight, delay) in vs {
            gid_to_connections.push_str(&format!(
                "({fgid}, \"{floc}\", \"{syn}\", \"{tloc}\", {weight}, {delay}), "
            ));
        }
        gid_to_connections.push_str("],\n");
    }
    gid_to_connections.push_str("                               }");

    let mut gid_to_labels = String::from("{\n");
    for (gid, vs) in &labels {
        gid_to_labels.push_str(&format!("                                {gid}: ["));
        for (seg, frac) in vs {
            gid_to_labels.push_str(&format!("({seg}, {frac}), "));
        }
        gid_to_labels.push_str("],\n");
    }
    gid_to_labels.push_str("                               }");

    let cat_prefix = "local_";

    Ok(format!(
        "#!/usr/bin/env python3
import arbor as A

import subprocess as sp
from pathlib import Path
from time import perf_counter as pc

here = Path(__file__).parent

def compile(fn, cat):
    fn = fn.resolve()
    cat = cat.resolve()
    recompile = False
    if fn.exists():
        for src in cat.glob('*.mod'):
            src = Path(src).resolve()
            if src.stat().st_mtime > fn.stat().st_mtime:
                recompile = True
                break
    sp.run(f'arbor-build-catalogue local {{cat}}', shell=True, check=True)
    return A.load_catalogue(fn)

class recipe(A.recipe):
    def __init__(self):
        A.recipe.__init__(self)
        self.props = A.neuron_cable_properties()
        cat = compile(here / 'local-catalogue.so', here / 'cat')
        self.props.catalogue.extend(cat, '{cat_prefix}')
        self.cell_to_morph = {cell_to_morph}
        self.gid_to_cell = {gid_to_cell}
        self.i_clamps = {i_clamps}
        self.gid_to_inputs = {gid_to_inputs}
        self.gid_to_synapses = {gid_to_synapses}
        self.gid_to_detectors = {gid_to_detectors}
        self.gid_to_connections = {gid_to_connections}
        self.gid_to_labels = {gid_to_labels}

    def num_cells(self):
        return {count}

    def cell_kind(self, _):
        return A.cell_kind.cable

    def cell_description(self, gid):
        cid = self.gid_to_cell[gid]
        mrf = self.cell_to_morph[cid]
        nml = A.neuroml(f'{{here}}/mrf/{{mrf}}.nml').morphology(mrf, allow_spherical_root=True)
        lbl = A.label_dict()
        lbl.append(nml.segments())
        lbl.append(nml.named_segments())
        lbl.append(nml.groups())
        lbl['all'] = '(all)'
        if gid in self.gid_to_labels:
            for seg, frac in self.gid_to_labels[gid]:
                lbl[f'seg_{{seg}}_frac_{{frac}}'] = f'(on-components {{frac}} (segment {{seg}}))'
        dec = A.load_component(f'{{here}}/acc/{{cid}}.acc').component
        dec.discretization(A.cv_policy_every_segment())
        if gid in self.gid_to_inputs:
            for tag, inp in self.gid_to_inputs[gid]:
                lag, dur, amp = self.i_clamps[inp]
                dec.place(f'\"{{tag}}\"', A.iclamp(lag, dur, amp), f'ic_{{inp}}@{{tag}}')
        if gid in self.gid_to_synapses:
            for tag, syn in self.gid_to_synapses[gid]:
                dec.place(f'\"{{tag}}\"', A.synapse(syn), f'syn_{{syn}}@{{tag}}')
        if gid in self.gid_to_detectors:
            for tag in self.gid_to_detectors[gid]:
                dec.place(f'\"{{tag}}\"', A.spike_detector(-40), f'sd@{{tag}}') # -40 is a phony value!!!
        return A.cable_cell(nml.morphology, lbl, dec)

    def probes(self, _):
        # Example: probe center of the root (likely the soma)
        return [A.cable_probe_membrane_voltage('(location 0 0.5)')]

    def global_properties(self, kind):
        return self.props

    def connections_on(self, gid):
        res = []
        if gid in self.gid_to_connections:
            for src, dec, syn, loc, w, d in self.gid_to_connections[gid]:
                conn = A.connection((src, f'sd@{{dec}}'), f'syn_{{syn}}@{{loc}}', w, d)
                res.append(conn)
        return res

ctx = A.context()
mdl = recipe()
ddc = A.partition_load_balance(mdl, ctx)
sim = A.simulation(mdl, ctx, ddc)
hdl = sim.sample((0, 0), A.regular_schedule(0.1))

print('Running simulation for 1s...')
t0 = pc()
sim.run(1000, 0.0025)
t1 = pc()
print(f'Simulation done, took: {{t1-t0:.3f}}s')

print('Trying to plot...')
try:
  import pandas as pd
  import seaborn as sns

  for data, meta in sim.samples(hdl):
    df = pd.DataFrame({{'t/ms': data[:, 0], 'U/mV': data[:, 1]}})
    sns.relplot(data=df, kind='line', x='t/ms', y='U/mV', ci=None).savefig('result.pdf')
  print('Ok')
except:
  print('Failure, are seaborn and matplotlib installed?')
"))
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

fn export_template(lems: &LemsFile, nml: &[String], bundle: &str) -> Result<()> {
    trace!("Creating bundle {bundle}");
    create_dir_all(bundle)?;
    create_dir_all(format!("{bundle}/mrf"))?;
    create_dir_all(format!("{bundle}/acc"))?;
    create_dir_all(format!("{bundle}/cat"))?;

    let norm = |v: &str| -> Result<String> {
        let q = Quantity::parse(v)?;
        let u = lems.normalise_quantity(&q)?.value;
        Ok(format!("{u}"))
    };

    let mut iclamps = Map::new();
    let mut cells = Vec::new();
    let mut nets = Vec::new();
    process_files(nml, |_, node| {
        let doc = node.document().input_text();
        match node.tag_name().name() {
            "morphology" => {
                let id = node
                    .attribute("id")
                    .ok_or_else(|| nml2_error!("Morph has no id"))?;
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
                        cells.push((cell.to_string(), morph.to_string()));
                    }
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
            "network" => {
                let inst = Instance::new(lems, node)?;
                let net = Network::new(&inst)?;
                nets.push(net);
            }
            _ => {}
        }
        Ok(())
    })?;

    match &nets[..] {
        [] => Ok(()),
        [net] => {
            trace!("Writing main.py");
            write(
                format!("{bundle}/main.py"),
                mk_main_py(&cells, &iclamps, net)?,
            )?;
            Ok(())
        }
        _ => Err(nml2_error!(
            "Currently only one Network per bundle is supported.",
        )),
    }
}

#[derive(Clone, Debug)]
enum RevPot {
    Const(Quantity),
    Nernst,
}

#[derive(Clone, Debug)]
struct IonChannel {
    name: String,
    reversal_potential: RevPot,
    conductance: Option<Quantity>,
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

fn ion_channel_assignments(
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
                            condDensity: Some(g),
                            erev,
                            segmentGroup,
                            ..
                        }) => {
                            let region = segment_group_or_all(segmentGroup);
                            let name = ionChannel.to_string();
                            let conductance = Some(lems.normalise_quantity(&Quantity::parse(g)?)?);
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
                            condDensity: Some(g),
                            segmentGroup,
                            ..
                        }) => {
                            let region = segment_group_or_all(segmentGroup);
                            let name = ionChannel.to_string();
                            let conductance = Some(lems.normalise_quantity(&Quantity::parse(g)?)?);
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
                            let variableParameter(vp) = &body[0];
                            let name = ionChannel.to_string();
                            let region = segment_group_or_all(&vp.segmentGroup);
                            let conductance = None;
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
                            let variableParameter(vp) = &body[0];
                            let region = segment_group_or_all(&vp.segmentGroup);
                            let name = ionChannel.to_string();
                            let conductance = None;
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
        for d in biophys.iter() {
            match d {
                Decor::Paint(ref r, Paintable::NonUniformMech { ref name, ns, .. })
                    if densities.contains(name) =>
                {
                    if !seen.contains(r) {
                        for (k, v) in ns.iter() {
                            non_uniform_args
                                .entry(r.to_owned())
                                .or_insert_with(Map::new)
                                .insert(format!("{name}_{k}"), v.to_owned());
                        }
                    }
                }
                _ => (),
            }
        }
        for d in biophys {
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
                    if let Some(g) = &channel.conductance {
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
