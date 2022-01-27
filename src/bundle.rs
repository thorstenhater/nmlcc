use std::fs::{create_dir_all, write};
use std::collections::HashMap as Map;

use crate::{
    acc,
    error::{self, Result},
    lems::file::LemsFile,
    expr::Quantity,
    neuroml::process_files,
    neuroml::raw::PulseGenerator,
    nmodl, xml::XML,
};

pub fn export(lems: &LemsFile, nml: &str, bundle: &str) -> Result<()> {
    create_dir_all(&bundle)?;
    create_dir_all(&format!("{}/mrf", bundle))?;

    let mut ics = Vec::new();
    let mut ids = Vec::new();
    let mut map = Map::new();
    process_files(&[nml], |_, node| {
        // TODO This is clunky.
        if node.tag_name().name() == "pulseGenerator" {
            let ic: PulseGenerator = XML::from_node(&node);
            ics.push(ic);
        }

        let doc = node.document().input_text();
        for mrf in node.descendants() {
            if node.tag_name().name() == "cell" {
                let id = node.attribute("id").ok_or(error::Error::Nml {
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
        }
        Ok(())
    })?;

    nmodl::export(lems, nml, &None, "-*", &format!("{}/{}", bundle, "cat"))?;
    acc::export(lems, nml, &None, &format!("{}/{}", bundle, "acc"))?;

    for id in &ids {
        write(&format!("{}/main.{}.py", bundle, id), mk_main_py(lems, id, &ics)?)?;
    }
    Ok(())
}

fn mk_main_py(lems: &LemsFile, id: &str, stim: &[PulseGenerator]) -> Result<String> {
    let norm = |v: &str| -> Result<String> {
        let q = Quantity::parse(v)?;
        let u = lems.normalise_quantity(&q)?;
        Ok(format!("{}", u.value))
    };

    let ics = stim.iter()
                  .map(|p| Ok(format!("decor.place('<FIXME>', A.iclamp({}, {}, {}), '{}')",
                                   norm(&p.delay)?,
                                   norm(&p.duration)?,
                                   norm(&p.amplitude)?,
                                   p.id)))
                  .collect::<Result<Vec<_>>>()?
                  .join("\n");

    Ok(format!("#!/usr/bin/env python3
import arbor as A

import subprocess as sp
from pathlib import Path

# Auto-generated file, please copy to eg main.py

here = Path(__file__).parent

def nml_load_cell():
    nml = A.neuroml(str(here / 'mrf' / '{id}.nml')).cell_morphology(\"{id}\", allow_spherical_root=True)
    lbl = A.label_dict()
    lbl.append(nml.segments())
    lbl.append(nml.named_segments())
    lbl.append(nml.groups())
    lbl['all'] = '(all)'
    dec = A.load_component(str(here / 'acc' / '{id}.acc')).component
    return nml.morphology, lbl, dec

def mk_cat():
    sp.run('arbor-build-catalogue local cat', shell=True, check=True)
    res = A.default_catalogue()
    cat = A.load_catalogue(str(here / 'local-catalogue.so'))
    res.extend(cat, '')
    return res

morph, labels, decor = nml_load_cell()

# Place your stimuli here
{ics}

cell = A.cable_cell(morph, labels, decor)
sim  = A.single_cell_model(cell)

sim.catalogue = mk_cat()

# Add probes here

# Now run the simulation
sim.run(100, 0.0025)
", id=id, ics=ics))
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
