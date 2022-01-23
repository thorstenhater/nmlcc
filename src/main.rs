#![allow(soft_unstable)]

use clap::{Parser, Subcommand};
use lems::file::LemsFile;
use roxmltree::Node;
use std::collections::HashSet;
use std::fs::{copy, create_dir_all, write};
use std::path::PathBuf;
use tracing::{info, trace};

mod acc;
mod error;
mod expr;
mod instance;
mod lems;
mod neuroml;
mod nmodl;
mod variable;
mod xml;

use acc::Sexp;

use error::Result;

#[derive(Parser)]
#[clap(name = "nmlcc")]
#[clap(version = "0.0.1", author = "t.hater@fz-juelich.de")]
struct Cli {
    /// Path to NMLCoreTypes
    #[clap(short, long, default_value = "ext/NeuroML2/NeuroML2CoreTypes")]
    include_dir: Vec<String>,
    /// Toplevel CoreType definition file
    #[clap(short, long, default_value = "NeuroML2CoreTypes.xml")]
    core: Vec<String>,
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Export to NMODL
    Nmodl {
        /// NeuroML2 compliant XML file
        nml: String,
        /// Base class to extract, if not given, a list of known Dynamics base
        /// types will be tried, namely: baseSynapse, baseIonChannel
        #[clap(short, long)]
        r#type: Option<String>,
        /// Parameters to be retained/removed from NMODL; prefix with `-` to
        /// remove or `+` to retain, `*` matches all suffices, cannot be given
        /// as infix/prefix; eg --parameter='-*,+foo_*,-foo_bar_*' will retain
        /// only those starting with `foo_`, unless followed by `bar_`. NOTE:
        /// Must be given in order of specificity and follow our internal naming
        /// scheme (sorry, but this is for fine-tuning).
        #[clap(short, long, default_value = "+*")]
        parameter: String,
        /// Write mechanisms under this prefix
        #[clap(short, long, default_value = ".")]
        dir: String,
    },
    /// Export to Arbor Cable Cell format (.acc)
    Acc {
        /// NeuroML2 compliant XML file
        nml: String,
        /// Cell id to extract, if not given will visit _all_ cells.
        #[clap(short, long)]
        cell: Option<String>,
        /// Write ouput under this prefix
        #[clap(short, long, default_value = ".")]
        dir: String,
    },
    /// DWIM creation of an Arbor simulation template
    Bundle {
        /// NeuroML2 compliant XML file
        nml: String,
        /// Prefix to put bundle
        bundle: String,
    },
}

fn process_nml_files<F>(nmls: &[&str], mut f: F) -> Result<()>
where
    F: FnMut(&Node) -> Result<()>,
{
    let mut todo = Vec::new();
    for nml in nmls {
        todo.push(PathBuf::from(&nml).canonicalize()?);
    }
    let mut seen = HashSet::new();
    while let Some(nml) = todo.pop() {
        if seen.contains(&nml) {
            continue;
        }
        trace!("Reading NML2 file {:?}", nml);
        seen.insert(nml.clone());
        let xml = std::fs::read_to_string(&nml)?;
        let tree = roxmltree::Document::parse(&xml)?;
        for node in tree.descendants() {
            f(&node)?;
            if node.tag_name().name() == "include" {
                if let Some(fd) = node.attribute("href") {
                    let mut nml = nml.parent().unwrap().to_path_buf();
                    nml.push(fd);
                    nml = nml.canonicalize()?;
                    todo.push(nml);
                }
            }
        }
    }
    Ok(())
}

fn get_runtime_types(lems: &mut LemsFile, nml: &str) -> Result<()> {
    process_nml_files(&[nml], |node| {
        if node.tag_name().name() == "ComponentType" {
            let ct: lems::raw::ComponentType = xml::XML::from_node(node);
            lems.add_component_type(&ct)?;
        }
        Ok(())
    })
}

fn export_nmodl(
    lems: &LemsFile,
    nml: &str,
    ty: &Option<&str>,
    filter: &str,
    cat: &str,
) -> Result<()> {
    let tys = if let Some(ty) = ty {
        vec![*ty]
    } else {
        vec!["baseIonChannel", "baseSynapse"]
    };
    process_nml_files(&[nml], |node| {
        let tag = node.tag_name().name();
        for ty in &tys {
            if lems.derived_from(tag, ty) {
                let instance = instance::Instance::new(lems, node)?;
                let mut path = PathBuf::from(&cat);
                if !path.exists() {
                    trace!("Creating path to {:?}", &path);
                    std::fs::create_dir_all(&path)?;
                }
                let file = instance.id.as_deref().ok_or(crate::error::Error::Nml {
                    what: String::from("Channel must have an id"),
                })?;
                path.push(file);
                path.set_extension("mod");
                info!("Writing NMODL to {:?}", &path);
                write(&path, nmodl::to_nmodl(&instance, filter)?)?;
            }
        }
        Ok(())
    })
}

fn export_acc(lems: &LemsFile, nml: &str, cell: &Option<&str>, pfx: &str) -> Result<()> {
    std::fs::create_dir_all(&pfx)?;
    process_nml_files(&[nml], |node| {
        if node.tag_name().name() != "cell" {
            return Ok(());
        }
        if let Some(id) = node.attribute("id") {
            if let Some(cell) = cell {
                if id != *cell {
                    return Ok(());
                }
            }
            let mut result = Vec::new();
            for bpp in node.descendants() {
                if bpp.tag_name().name() != "biophysicalProperties" {
                    continue;
                }
                let prop: neuroml::raw::BiophysicalProperties = xml::XML::from_node(&bpp);
                result.append(&mut acc::acc(&prop, lems)?);
            }
            let mut file = PathBuf::from(pfx);
            file.push(id);
            file.set_extension("acc");
            info!("Writing ACC to {:?}", &file);
            write(&file, result.to_sexp())?;
        }
        Ok(())
    })
}

fn main() -> Result<()> {
    let collector = tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_target(false)
        .compact()
        .finish();
    let _guard = tracing::subscriber::set_global_default(collector);

    let opts = Cli::parse();
    let mut lems = lems::file::LemsFile::from(&opts.include_dir, &opts.core)?;
    match opts.cmd {
        Cmd::Nmodl {
            nml,
            r#type,
            parameter,
            dir,
        } => {
            get_runtime_types(&mut lems, &nml)?;
            export_nmodl(&lems, &nml, &r#type.as_deref(), &parameter, &dir)?;
        }
        Cmd::Acc { nml, cell, dir } => export_acc(&lems, &nml, &cell.as_deref(), &dir)?,
        Cmd::Bundle { nml, bundle } => {
            get_runtime_types(&mut lems, &nml)?;
            create_dir_all(&bundle)?;
            copy(&nml, &format!("{}/{}", bundle, "morph.nml"))?;
            export_nmodl(&lems, &nml, &None, "-*", &format!("{}/{}", bundle, "cat"))?;
            export_acc(&lems, &nml, &None, &format!("{}/{}", bundle, "acc"))?;
            write(
                &format!("{}/{}", bundle, "main.tmp.py"),
                "#!/usr/bin/env python3
import arbor as A

import subprocess as sp
from pathlib import Path

# Auto-generated file, please copy to eg main.py

here = Path(__file__).parent

def nml_load_cell(cid):
    nml = A.neuroml(str(here / 'morph.nml')).cell_morphology(cid, allow_spherical_root=True)
    lbl = A.label_dict()
    lbl.append(nml.segments())
    lbl.append(nml.named_segments())
    lbl.append(nml.groups())
    lbl['all'] = '(all)'
    dec = A.load_component(str(here / 'acc' / f'{cid}.acc')).component
    return nml.morphology, lbl, dec

def mk_cat():
    global cat
    global res
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
sim.run(1, 0) # FIXME
",
            )?;
        }
    }
    Ok(())
}
