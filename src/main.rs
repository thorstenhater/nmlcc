use clap::{Parser, Subcommand};
use std::fs::write;

mod nmodl;
mod xml;
mod lems;
mod expr;
mod variable;
mod instance;

type Result<T> = std::result::Result<T, String>;

#[derive(Parser)]
#[clap(name = "nmlcc")]
#[clap(version="0.0.1", author="t.hater@fz-juelich.de")]
struct Cli {
    /// Path to NMLCoreTypes
    #[clap(short, long, default_value="ext/NeuroML2/NeuroML2CoreTypes")]
    include_dir: Vec<String>,
    /// Toplevel CoreType definition file
    #[clap(short, long, default_value="NeuroML2CoreTypes.xml")]
    core: Vec<String>,
    #[clap(subcommand)]
    cmd: Cmd
}

#[derive(Subcommand)]
enum Cmd {
    /// Export to NMODL
    Nmodl {
        /// A NeuroML2 compliant XML file
        nml: String,
        /// Base class to extract
        #[clap(short, long)]
        r#type: String,
        /// Parameters to be retained as settable NIY
        #[clap(short, long)]
        parameters: Vec<String>,
    }
}

fn main() -> Result<()> {
    let collector = tracing_subscriber::fmt()
        .with_max_level(tracing::Level::TRACE)
        .with_target(false)
        .compact()
        .finish();
    let _guard = tracing::subscriber::set_global_default(collector);

    let opts = Cli::parse();
    let mut lems = lems::file::LemsFile::from(&opts.include_dir, &opts.core)?;
    match opts.cmd {
        Cmd::Nmodl { nml, r#type, parameters } => {
            let xml  = std::fs::read_to_string(&nml).map_err(|_| format!("File not found: {}", &nml))?;
            let tree = roxmltree::Document::parse(&xml).map_err(|_| format!("Could not parse input : {}", &nml))?;
            for node in tree.descendants() {
                if node.tag_name().name() == "ComponentType" {
                    let ct: lems::raw::ComponentType = xml::XML::from_node(&node);
                    lems.add_component_type(&ct)?;
                }
            }
            for node in tree.descendants() {
                if node.tag_name().name() == r#type {
                    let instance = instance::Instance::new(&lems, &node)?;
                    let nmodl = nmodl::to_nmodl(&instance)?;
                    let file = format!("{}.mod", instance.id.as_deref().unwrap_or("out"));
                    write(&file, nmodl).map_err(|_| "Error writing output to NMODL.")?;
                }
            }
        }
    }
    Ok(())
}
