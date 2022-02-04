use roxmltree::Node;
use std::collections::HashSet;
use std::path::PathBuf;
use tracing::trace;

use crate::error::{Error, Result};

pub mod raw;

pub fn process_files<F>(nmls: &[String], mut f: F) -> Result<()>
where
    F: FnMut(&str, &Node) -> Result<()>,
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
        if tree.root_element().tag_name().name() != "neuroml" {
            return Err(Error::Nml {
                what: format!("Not a NeuroML2 file {:?}", nml),
            });
        }
        for node in tree.descendants() {
            f(nml.to_str().unwrap(), &node)?;
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
