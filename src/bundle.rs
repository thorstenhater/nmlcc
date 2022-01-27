use std::fs::{create_dir_all, write};

use crate::{
    acc,
    error::{self, Result},
    lems::file::LemsFile,
    neuroml::process_files,
    nmodl,
};

pub fn export(lems: &LemsFile, nml: &str, bundle: &str) -> Result<()> {
    create_dir_all(&bundle)?;
    create_dir_all(&format!("{}/mrf", bundle))?;

    process_files(&[nml], |_, node| {
        if node.tag_name().name() != "cell" {
            return Ok(());
        }
        let id = node.attribute("id").ok_or(error::Error::Nml {
            what: String::from("Cell has no id"),
        })?;
        let doc = node.document().input_text();
        for mrf in node.descendants() {
            if mrf.tag_name().name() == "morphology" {
                write(
                    format!("{}/mrf/{}.nml", bundle, id),
                    mk_mrf(id, &doc[mrf.range()]),
                )?;
            }
        }
        Ok(())
    })?;

    nmodl::export(&lems, &nml, &None, "-*", &format!("{}/{}", bundle, "cat"))?;
    acc::export(&lems, &nml, &None, &format!("{}/{}", bundle, "acc"))?;

    write(&format!("{}/{}", bundle, "main.tmp.py"), mk_main_py())?;
    Ok(())
}

fn mk_main_py() -> String {
    String::from("#!/usr/bin/env python3
import arbor as A

import subprocess as sp
from pathlib import Path

# Auto-generated file, please copy to eg main.py

here = Path(__file__).parent

def nml_load_cell(cid):
    nml = A.neuroml(str(here / 'mrf' / (cid + '.nml'))).cell_morphology(cid, allow_spherical_root=True)
    lbl = A.label_dict()
    lbl.append(nml.segments())
    lbl.append(nml.named_segments())
    lbl.append(nml.groups())
    lbl['all'] = '(all)'
    dec = A.load_component(str(here / 'acc' / (cid + '.acc'))).component
    return nml.morphology, lbl, dec

def mk_cat():
    sp.run('arbor-build-catalogue local cat', shell=True, check=True)
    res = A.default_catalogue()
    cat = A.load_catalogue(str(here / 'local-catalogue.so'))
    res.extend(cat, '')
    return res

# Enter cell id here
cid = '<FIXME>'

morph, labels, decor = nml_load_cell(cid)

# Add things to the decor here

cell = A.cable_cell(morph, labels, decor)
sim  = A.single_cell_model(cell)

sim.catalogue = mk_cat()

# Add probes here

# Now run the simulation
sim.run(100, 0.0025)
")
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
