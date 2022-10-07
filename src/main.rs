use clap::{Parser, Subcommand};

use nml2::{
    acc, bundle,
    error::Result,
    lems::{self, file::LemsFile},
    neuroml, nmodl,
    xml::XML,
};

#[derive(Parser)]
#[clap(name = "nmlcc")]
#[clap(version = "0.4.1-dev", author = "t.hater@fz-juelich.de")]
struct Cli {
    /// Verbosity level, defaults to 'WARN'.
    #[clap(short, long, parse(from_occurrences))]
    verbose: usize,
    /// Comma separated list of ion species to consider as given. The default is
    /// _Arbor's_ list of ions Calcium Ca 2+, Sodium Na 1+, and Potassium K 1+.
    /// NeuroML2 defaults to just Ca and Na, so if you are trying to port an
    /// NML2 model 1:1 pass --ions='na,ca'. All ion names will be turned into
    /// lowercase. Note that this gives the simulator's built-in, but does not
    /// allow for adding new species.
    #[clap(short, long, default_value = "ca, na, k")]
    ions: String,
    #[clap(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Export to NMODL
    Nmodl {
        /// NeuroML2 compliant XML files
        nml: Vec<String>,
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
        /// NeuroML2 compliant XML files
        nml: Vec<String>,
        /// Write ouput under this prefix
        #[clap(short, long, default_value = ".")]
        dir: String,
    },
    /// DWIM creation of an Arbor simulation template
    Bundle {
        /// NeuroML2 compliant XML file
        nml: String,
        /// Try to combine channels per segment group
        #[clap(short, long)]
        super_mechanisms: bool,
        /// Change catalogue prefix.
        /// Set to empty string if mechanisms do not collide with internal mechanisms
        #[clap(short, long, default_value="local_")]
        cat_prefix: String,
        /// Prefix to put bundle
        dir: String,
    },
}

fn get_runtime_types(lems: &mut LemsFile, nml: &[String]) -> Result<()> {
    neuroml::process_files(nml, |_, node| {
        if node.tag_name().name() == "ComponentType" {
            let ct: lems::raw::ComponentType = XML::from_node(node);
            lems.add_component_type(&ct)?;
        }
        Ok(())
    })
}

fn set_collector(v: usize) -> std::result::Result<(), tracing::dispatcher::SetGlobalDefaultError> {
    let lvl = match v {
        0 => tracing::Level::WARN,
        1 => tracing::Level::INFO,
        _ => tracing::Level::TRACE,
    };
    let collector = tracing_subscriber::fmt()
        .with_max_level(lvl)
        .with_target(false)
        .compact()
        .finish();
    tracing::subscriber::set_global_default(collector)
}

fn main() -> Result<()> {
    let opts = Cli::parse();
    let _g = set_collector(opts.verbose);

    let mut lems = lems::file::LemsFile::core();

    let ions = opts
        .ions
        .split(',')
        .map(|s| s.trim().to_lowercase())
        .collect::<Vec<_>>();
    match opts.cmd {
        Cmd::Nmodl {
            nml,
            parameter,
            dir,
        } => {
            get_runtime_types(&mut lems, &nml)?;
            nmodl::export(&lems, &nml, &parameter, &dir, &ions[..])?;
        }
        Cmd::Acc { nml, dir } => {
            get_runtime_types(&mut lems, &nml)?;
            acc::export(&lems, &nml, &dir, "")?;
        }
        Cmd::Bundle {
            nml,
            dir,
            super_mechanisms,
            cat_prefix
        } => {
            get_runtime_types(&mut lems, &[nml.clone()])?;
            bundle::export(&lems, &[nml], &dir, super_mechanisms, &ions[..], &cat_prefix)?;
        }
    }
    Ok(())
}
