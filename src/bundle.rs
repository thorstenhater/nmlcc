use std::fs::{create_dir_all, write};
use tracing::info;

use crate::acc::Paintable;
use crate::expr::Stmnt;
use crate::{
    acc::{self, Sexp},
    error::{Error, Result},
    expr::Expr,
    expr::Quantity,
    instance::{Collapsed, Context, Instance},
    lems::file::LemsFile,
    neuroml::process_files,
    neuroml::raw::{
        BiophysicalProperties, BiophysicalPropertiesBody, ChannelDensity, MembranePropertiesBody,
    },
    nmodl,
    xml::XML,
    Map, Set,
};

pub fn export(lems: &LemsFile, nml: &[String], bundle: &str, use_super_mechs: bool) -> Result<()> {
    export_template(nml, bundle)?;

    // We always export these to keep synapse etc alive
    nmodl::export(lems, nml, "-*", &format!("{}/cat", bundle))?;

    if use_super_mechs {
        export_with_super_mechanisms(lems, nml, bundle)?;
    } else {
        acc::export(lems, nml, &format!("{}/acc", bundle))?;
    }
    Ok(())
}

fn mk_main_py(id: &str) -> Result<String> {
    Ok(format!(
        "#!/usr/bin/env python3
import arbor as A

import subprocess as sp
from pathlib import Path
from time import perf_counter as pc

# Auto-generated file, please copy to eg main.py

here = Path(__file__).parent

def nml_load_cell():
    nml = A.neuroml(here / 'mrf' / '{id}.nml').cell_morphology(\"{id}\", allow_spherical_root=True)
    lbl = A.label_dict()
    lbl.append(nml.segments())
    lbl.append(nml.named_segments())
    lbl.append(nml.groups())
    lbl['all'] = '(all)'
    dec = A.load_component(here / 'acc' / '{id}.acc').component
    return nml.morphology, lbl, dec

def mk_cat():
    sp.run('arbor-build-catalogue local cat', shell=True, check=True)
    res = A.default_catalogue()
    cat = A.load_catalogue(here / 'local-catalogue.so')
    res.extend(cat, '')
    return res

morph, labels, decor = nml_load_cell()

decor.discretization(A.cv_policy_every_segment())

cell = A.cable_cell(morph, labels, decor)
sim  = A.single_cell_model(cell)

sim.properties.catalogue = mk_cat()

# Add probes here (example below)
sim.probe('voltage', '(location 0 0.5)', frequency=10) # probe center of the root (likely the soma)

# Now run the simulation
print('Running simulation for 1s...')
t0 = pc()
sim.run(1000, 0.0025)
t1 = pc()
print(f'Simulation done, took: {{t1-t0:.3f}}s')

print('Trying to plot...')
try:
  import pandas as pd
  import seaborn as sns

  tr = sim.traces[0]
  df = pd.DataFrame({{'t/ms': tr.time, 'U/mV': tr.value}})

  sns.relplot(data=df, kind='line', x='t/ms', y='U/mV', ci=None).set_titles('Probe at (location 0 0.5)').savefig('{id}.pdf')
  print('Ok, generated {id}.pdf')
except:
  print('Failure, are seaborn and matplotlib installed?')
",
        id = id
    ))
}

fn mk_mrf(id: &str, mrf: &str) -> String {
    format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>

<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2  https://raw.githubusercontent.com/NeuroML/NeuroML2/master/Schemas/NeuroML2/NeuroML_v2beta3.xsd"
    id="{}">
    <cell id="{}">
        {}
    </cell>
</neuroml>
"#,
        id, id, mrf
    )
}

fn export_template(nml: &[String], bundle: &str) -> Result<()> {
    create_dir_all(&bundle)?;
    create_dir_all(&format!("{}/mrf", bundle))?;
    create_dir_all(&format!("{}/acc", bundle))?;
    create_dir_all(&format!("{}/cat", bundle))?;

    let mut ids = Vec::new();
    process_files(nml, |_, node| {
        let doc = node.document().input_text();
        for mrf in node.descendants() {
            match node.tag_name().name() {
                "cell" => {
                    let id = node.attribute("id").ok_or(Error::Nml {
                        what: String::from("Cell has no id"),
                    })?;
                    ids.push(id.to_string());
                    if mrf.tag_name().name() == "morphology" {
                        write(
                            format!("{}/mrf/{}.nml", bundle, id),
                            mk_mrf(id, &doc[mrf.range()]),
                        )?;
                    }
                }
                "network" => {}
                _ => {}
            }
        }
        Ok(())
    })?;

    for id in &ids {
        write(&format!("{}/main.{}.py", bundle, id), mk_main_py(id)?)?;
    }
    Ok(())
}

struct Assign {
    m: String,
    g: Quantity,
    e: Quantity,
}

impl Assign {
    fn new(m: &str, g: &str, e: &str) -> Result<Self> {
        let g = Quantity::parse(g)?;
        let e = Quantity::parse(e)?;
        Ok(Self {
            m: m.to_string(),
            g,
            e,
        })
    }
}

pub fn export_with_super_mechanisms(lems: &LemsFile, nml: &[String], bundle: &str) -> Result<()> {
    use BiophysicalPropertiesBody::*;
    use MembranePropertiesBody::*;
    let mut sms: Map<(String, String), Vec<Assign>> = Map::new();
    process_files(nml, |_, node| {
        if node.tag_name().name() == "cell" {
            let id = node.attribute("id").ok_or(Error::Nml {
                what: "Cell without id".to_string(),
            })?;
            for bpp in node.descendants() {
                match bpp.tag_name().name() {
                    "biophysicalProperties" => {
                        let prop: BiophysicalProperties = XML::from_node(&bpp);
                        for item in &prop.body {
                            if let membraneProperties(membrane) = item {
                                for item in &membrane.body {
                                    if let channelDensity(ChannelDensity {
                                        ionChannel,
                                        condDensity,
                                        erev,
                                        segmentGroup,
                                        ..
                                    }) = item
                                    {
                                        let a = Assign::new(
                                            ionChannel,
                                            condDensity.as_deref().unwrap(),
                                            erev.as_str(),
                                        )?;
                                        let region = if segmentGroup.is_empty() {
                                            "all"
                                        } else {
                                            segmentGroup
                                        }
                                        .to_string();
                                        sms.entry((id.to_string(), region)).or_default().push(a);
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
        Ok(())
    })?;

    let decor = acc::to_decor(lems, nml)?;
    for (id, ass) in decor {
        let path = format!("{}/acc/{}.acc", bundle, id);
        let mut seen = Set::new();
        let mut ass_sm = Vec::new();
        for d in ass.into_iter() {
            match d {
                acc::Decor::Paint(r, Paintable::Mech(_, _)) => {
                    if !seen.contains(&r) {
                        ass_sm.push(acc::Decor::Paint(
                            r.to_string(),
                            Paintable::Mech(format!("{}_{}", id, r), Map::new()),
                        ));
                        seen.insert(r.to_string());
                    }
                }
                _ => ass_sm.push(d),
            }
        }

        info!("Writing Super Mechanism ACC to {:?}", &path);
        write(&path, ass_sm.to_sexp())?;
    }

    let mut instances = Vec::new();
    process_files(nml, |_, node| {
        let tag = node.tag_name().name();
        if lems.derived_from(tag, "baseIonChannel") {
            let instance = Instance::new(lems, node)?;
            instances.push(instance);
        }
        Ok(())
    })?;

    for ((id, reg), ms) in &sms {
        let mut coll = Collapsed::new(&Some(format!("{}_{}", id, reg)));
        let mut ions: Map<_, Vec<_>> = Map::new();
        for Assign { m, g, e } in ms {
            for inst in &instances {
                if inst.id == Some(m.to_string()) {
                    let mut inst = inst.clone();
                    let ion = inst.attributes.get("species").cloned().unwrap_or_default();
                    // Set Parameters e, g
                    let g = lems.normalise_quantity(g)?;
                    inst.component_type.parameters.push(String::from("conductance"));
                    inst.parameters.insert(String::from("conductance"), g);
                    if ion.is_empty() {
                        let e = lems.normalise_quantity(e)?;
                        inst.parameters.insert(String::from("e"), e);
                        inst.component_type.parameters.push(String::from("e"));
                    }
                    ions.entry(ion.clone()).or_default().push(m.clone());
                    coll.add(&inst, &Context::default(), None)?;
                    eprintln!("{:?}", coll.parameters);
                }
            }
        }

        // Add iX
        let mut outputs = Map::new();
        let mut variables = Map::new();
        for (ion, mechs) in &ions {
            if !ion.is_empty() {
                let ik = format!("i{}", ion);
                let ix = Expr::parse(&format!("g_{}*(v - e{})", ion, ion))?;
                let ix = Stmnt::Ass(ik.clone(), ix);
                outputs.insert(ik, ix);

                let g = mechs
                    .iter()
                    .map(|m| format!("{}_g", m))
                    .collect::<Vec<_>>()
                    .join(" + ");
                let gk = format!("g_{}", ion);
                let ig = Expr::parse(&g)?;
                let ig = Stmnt::Ass(gk.clone(), ig);
                variables.insert(gk, ig);
            } else {
                // here we must sum currents directly
                let mut i = Vec::new();
                for mech in mechs {
                    i.push(format!("{}_conductance*(v - {}_e)", mech, mech));
                }
                let i = i.join(" + ");
                let i = Expr::parse(&i)?;
                outputs.insert(String::from("i"), Stmnt::Ass(String::from("i"), i));
            }
        }
        let mut n = nmodl::Nmodl::from(&coll, "+*")?;
        n.add_outputs(&outputs);
        n.add_variables(&outputs);
        n.add_variables(&variables);
        let nmodl = nmodl::mk_nmodl(&n)?;

        let path = format!("{}/cat/{}_{}.mod", bundle, id, reg);
        info!(
            "Writing Super-Mechanism NMODL for cell '{}' region '{}' to {:?}",
            id, reg, &path
        );
        write(&path, nmodl)?;
    }

    Ok(())
}
