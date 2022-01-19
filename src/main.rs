#![allow(soft_unstable)]

use clap::{Parser, Subcommand};
use roxmltree::Node;
use std::path::PathBuf;
use std::{collections::HashSet, fs::write};
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
        /// Base class to extract
        #[clap(short, long)]
        r#type: String,
        /// Parameters to be retained/removed from NMODL; prefix with `-` to
        /// remove or `+` to retain, `*` matches all suffices, cannot be given
        /// as infix/prefix; eg --parameter='-*,+foo_*,-foo_bar_*' will retain
        /// only those starting with `foo_`, unless followed by `bar_`. NOTE:
        /// Must be given in order of specificity and follow our internal naming
        /// scheme (sorry, but this is for fine-tuning).
        #[clap(short, long, default_value = "+*")]
        parameter: String,
        /// Write mechanisms under this prefix
        #[clap(short, long, default_value = "cat")]
        catalogue: String,
    },
    /// Export to Arbor Cable Cell format (.acc)
    Acc {
        /// NeuroML2 compliant XML file
        nml: String,
        /// Cell id to extract, if not given will visit _all_ cells.
        #[clap(short, long)]
        cell: Option<String>,
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
            catalogue,
        } => {
            // We need to process all potential extensions before...
            process_nml_files(&[&nml], |node| {
                if node.tag_name().name() == "ComponentType" {
                    let ct: lems::raw::ComponentType = xml::XML::from_node(node);
                    lems.add_component_type(&ct)?;
                }
                Ok(())
            })?;
            // ...we can instantiate LEMS component types.
            process_nml_files(&[&nml], |node| {
                if node.tag_name().name() == r#type {
                    let instance = instance::Instance::new(&lems, node)?;
                    let mut path = PathBuf::from(&catalogue);
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
                    write(&path, nmodl::to_nmodl(&instance, &parameter)?)?;
                }
                Ok(())
            })?;
        }
        Cmd::Acc { nml, cell } => process_nml_files(&[&nml], |node| {
            if node.tag_name().name() != "cell" {
                return Ok(());
            }
            if let Some(id) = node.attribute("id") {
                if let Some(cell) = &cell {
                    if id != cell {
                        return Ok(());
                    }
                }
                let mut result = Vec::new();
                for bpp in node.descendants() {
                    if bpp.tag_name().name() != "biophysicalProperties" {
                        continue;
                    }
                    let prop: neuroml::raw::BiophysicalProperties = xml::XML::from_node(&bpp);
                    result.append(&mut acc::acc(&prop, &lems)?);
                }
                let mut file = PathBuf::from(id);
                file.set_extension("acc");
                write(&file, result.to_sexp())?;
            }
            Ok(())
        })?,
    }
    Ok(())
}
