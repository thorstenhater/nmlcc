use std::fs::write;

use clap::Parser;

mod nmodl;
mod xml;
mod lems;
mod expr;
mod variable;
mod instance;

use instance::Instance;

type Result<T> = std::result::Result<T, String>;

#[derive(Parser)]
#[clap(version="0.0.1", author="t.hater@fz-juelich.de")]
struct Options {
    /// Path to NMLCoreTypes
    #[clap(short, long, default_value="ext/NeuroML2/NeuroML2CoreTypes")]
    include_dir: String,
    /// Toplevel CoreType definition file
    #[clap(short, long, default_value="NeuroML2CoreTypes.xml")]
    core: String,
    /// A NeuroML2 compliant XML file
    nml: String,
    /// Base class to extract
    #[clap(short, long, default_value="ionChannelHH")]
    r#type: String,
    /// Output file
    #[clap(short, long)]
    output: Option<String>,
    /// Parameters to be retained as settable NIY
    #[clap(short, long)]
    parameters: Vec<String>,
}

fn main() -> Result<()> {
    let opts = Options::parse();
    let lems = lems::file::LemsFile::from(&opts.include_dir, &opts.core)?;
    let xml  = std::fs::read_to_string(&opts.nml).map_err(|_| format!("File not found: {}", &opts.nml))?;
    let tree = roxmltree::Document::parse(&xml).map_err(|_| format!("Could not parse input : {}", &opts.nml))?;
    let node = tree.descendants()
                   .find(|n| n.tag_name().name() == opts.r#type)
                   .ok_or(format!("Doc does not contain instances of {}", &opts.r#type))?;
    let instance = Instance::new(&lems, &node)?;

    let nmodl = nmodl::to_nmodl(&instance)?;
    if let Some(file) = opts.output {
        write(&file, nmodl).map_err(|_| "Error writing output to NMODL.")?;
    } else {
        print!("{}", nmodl);
    };
    Ok(())
}
