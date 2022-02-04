#![allow(soft_unstable)]

use clap::{Parser, Subcommand};
use lems::file::LemsFile;

mod acc;
mod bundle;
mod error;
mod expr;
mod instance;
mod lems;
mod network;
mod neuroml;
mod nmodl;
mod variable;
mod xml;

use error::Result;

#[derive(Parser)]
#[clap(name = "nmlcc")]
#[clap(version = "0.2.0", author = "t.hater@fz-juelich.de")]
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
        nml: Vec<String>,
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
        nml: Vec<String>,
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
        nml: Vec<String>,
        /// Try to combine channels per segment group
        #[clap(short, long)]
        super_mechanisms: bool,
        /// Prefix to put bundle
        dir: String,
    },
}

fn get_runtime_types(lems: &mut LemsFile, nml: &[String]) -> Result<()> {
    neuroml::process_files(nml, |_, node| {
        if node.tag_name().name() == "ComponentType" {
            let ct: lems::raw::ComponentType = xml::XML::from_node(node);
            lems.add_component_type(&ct)?;
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
            nmodl::export(&lems, &nml, &r#type.as_deref(), &parameter, &dir)?;
        }
        Cmd::Acc { nml, cell, dir } => acc::export(&lems, &nml, &cell.as_deref(), &dir)?,
        Cmd::Bundle {
            nml,
            dir,
            super_mechanisms,
        } => {
            get_runtime_types(&mut lems, &nml)?;
            bundle::export(&lems, &nml, &dir, super_mechanisms)?;
        }
    }
    Ok(())
}
