use crate::{
    acc::{self, Decor, Paintable, Sexp},
    error::{Error, Result},
    expr::{Expr, Quantity, Stmnt},
    instance::{Collapsed, Context, Instance},
    lems::file::LemsFile,
    network::{get_cell_id, Connection, Input, Network, Projection},
    neuroml::process_files,
    neuroml::raw::{
        BiophysicalProperties, BiophysicalPropertiesBody, ChannelDensity, MembranePropertiesBody,
        PulseGenerator,
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
) -> Result<()> {
    export_template(lems, nml, bundle)?;

    // We always export these to keep synapse etc alive
    nmodl::export(lems, nml, "-*", &format!("{bundle}/cat"), ions)?;

    if use_super_mechs {
        export_with_super_mechanisms(lems, nml, bundle)?;
    } else {
        acc::export(lems, nml, &format!("{bundle}/acc"))?;
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
        self.props.catalogue.extend(cat, '')
        self.cell_to_morph = {}
        self.gid_to_cell = {}
        self.i_clamps = {}
        self.gid_to_inputs = {}
        self.gid_to_synapses = {}
        self.gid_to_detectors = {}
        self.gid_to_connections = {}
        self.gid_to_labels = {}

    def num_cells(self):
        return {}

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
", cell_to_morph, gid_to_cell, i_clamps, gid_to_inputs, gid_to_synapses, gid_to_detectors, gid_to_connections, gid_to_labels, count))
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
    create_dir_all(&bundle)?;
    create_dir_all(&format!("{bundle}/mrf"))?;
    create_dir_all(&format!("{bundle}/acc"))?;
    create_dir_all(&format!("{bundle}/cat"))?;

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
                &format!("{bundle}/main.py"),
                mk_main_py(&cells, &iclamps, net)?,
            )?;
            Ok(())
        }
        _ => Err(nml2_error!(
            "Currently only one Network per bundle is supported.",
        )),
    }
}

#[derive(Debug)]
struct Assign {
    m: String,
    g: Quantity,
    e: Quantity,
}

impl Assign {
    fn new(m: String, g: String, e: String, lems: &LemsFile) -> Result<Self> {
        let g = lems.normalise_quantity(&Quantity::parse(&g)?)?;
        let e = lems.normalise_quantity(&Quantity::parse(&e)?)?;
        Ok(Self { m, g, e })
    }
}

pub fn build_super_mechanisms(lems: &LemsFile, nml: &[String], bundle: &str) -> Result<()> {
    let sms = collect_ion_channel_assignments(lems, nml)?;
    let dec = collect_decor(lems, nml)?;
    let ins = collect_ion_channels(lems, nml)?;
    let mrg = merge_ion_channels(&sms, &ins)?;

    for (id, reg, n) in &mrg {
        let nmodl = nmodl::mk_nmodl(n)?;
        let path = format!("{bundle}/cat/{id}_{reg}.mod");
        info!("Writing Super-Mechanism NMODL for cell '{id}' region '{reg}' to {path:?}",);
        write(&path, nmodl)?;
    }

    for (id, ass) in dec {
        let path = format!("{bundle}/acc/{id}.acc");
        info!("Writing Super Mechanism ACC to {path:?}");
        write(&path, ass.to_sexp())?;
    }

    Ok(())
}

fn ion_channel_assigments(
    prop: BiophysicalProperties,
    lems: &LemsFile,
) -> Result<Vec<(String, Assign)>> {
    use BiophysicalPropertiesBody::*;
    use MembranePropertiesBody::*;

    let mut result = Vec::new();
    for item in prop.body.into_iter() {
        if let membraneProperties(membrane) = item {
            for item in membrane.body.into_iter() {
                if let channelDensity(ChannelDensity {
                    ionChannel,
                    condDensity: Some(g),
                    erev,
                    segmentGroup,
                    ..
                }) = item
                {
                    let region = if segmentGroup.is_empty() {
                        String::from("all")
                    } else {
                        segmentGroup
                    };
                    result.push((region, Assign::new(ionChannel, g, erev, lems)?));
                }
            }
        }
    }
    Ok(result)
}

fn collect_ion_channel_assignments(
    lems: &LemsFile,
    nml: &[String],
) -> Result<Map<(String, String), Vec<Assign>>> {
    let mut sms: Map<(String, String), Vec<Assign>> = Map::new();
    process_files(nml, |_, node| {
        if node.tag_name().name() != "cell" {
            return Ok(());
        }
        let id = node.attribute("id").ok_or(nml2_error!("Cell without id"))?;
        for bpp in node.children() {
            if bpp.tag_name().name() != "biophysicalProperties" {
                continue;
            }
            let res = ion_channel_assigments(XML::from_node(&bpp), lems)?;
            for (reg, ass) in res.into_iter() {
                sms.entry((id.to_string(), reg)).or_default().push(ass);
            }
        }
        Ok(())
    })?;
    Ok(sms)
}

fn collect_decor(lems: &LemsFile, nml: &[String]) -> Result<Map<String, Vec<Decor>>> {
    let mut result: Map<String, Vec<Decor>> = Map::new();
    for (id, ass) in acc::to_decor(lems, nml)?.into_iter() {
        let mut seen = Set::new();
        let mut sm = Vec::new();
        for d in ass.into_iter() {
            if let Decor::Paint(r, Paintable::Mech(_, _)) = d {
                if !seen.contains(&r) {
                    sm.push(acc::Decor::mechanism(&r, &format!("{id}_{r}"), &Map::new()));
                    seen.insert(r.to_string());
                }
            } else {
                sm.push(d);
            }
        }
        result.insert(id, sm);
    }
    Ok(result)
}

fn collect_ion_channels(lems: &LemsFile, nml: &[String]) -> Result<Vec<Instance>> {
    let mut instances = Vec::new();
    process_files(nml, |_, node| {
        let tag = node.tag_name().name();
        if lems.derived_from(tag, "baseIonChannel") {
            instances.push(Instance::new(lems, node)?);
        }
        Ok(())
    })?;
    Ok(instances)
}

fn merge_ion_channels(
    sms: &Map<(String, String), Vec<Assign>>,
    ins: &[Instance],
) -> Result<Vec<(String, String, Nmodl)>> {
    // TODO we might want to extend this when finding <species>.
    let known_ions = vec![String::from("ca"), String::from("k"), String::from("na")];

    let mut result = Vec::new();
    for ((id, reg), ms) in sms {
        let mut coll = Collapsed::new(&Some(format!("{id}_{reg}")));
        let mut ions: Map<_, Vec<_>> = Map::new();
        for Assign { m, g, e } in ms {
            for inst in ins {
                if inst.id == Some(m.to_string()) {
                    let mut inst = inst.clone();
                    let ion = inst.attributes.get("species").cloned().unwrap_or_default();
                    // Set Parameters e, g
                    inst.component_type
                        .parameters
                        .push(String::from("conductance"));
                    inst.parameters
                        .insert(String::from("conductance"), g.clone());
                    inst.parameters.insert(format!("e{ion}"), e.clone());
                    inst.component_type.parameters.push(format!("e{ion}"));
                    ions.entry(ion.clone()).or_default().push(m.clone());
                    coll.add(&inst, &Context::default(), None)?;
                }
            }
        }

        // Add iX
        let mut outputs = Map::new();
        let mut variables = Map::new();
        for (ion, mechs) in &ions {
            if !ion.is_empty() && known_ions.contains(ion) {
                // A non-empty, known ion species X maps to USEION X READ eX WRITE iX
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
                    .map(|m| format!("{m}_conductance*(v - {m}_e{ion})"))
                    .collect::<Vec<_>>()
                    .join(" + ");
                eprintln!("{i:?}");
                let ix = Stmnt::Ass(format!("i{ion}"), Expr::parse(&i)?);
                outputs.insert(format!("i{ion}"), ix);
            }
        }

        let mut n = nmodl::Nmodl::from(&coll, &known_ions, "-*")?;
        n.add_outputs(&outputs);
        n.add_variables(&outputs);
        n.add_variables(&variables);
        result.push((id.clone(), reg.clone(), n))
    }
    Ok(result)
}
