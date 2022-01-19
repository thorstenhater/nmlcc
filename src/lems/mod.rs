pub mod file;
pub mod raw;

use std::path::PathBuf;

use roxmltree::Document;
use tracing::trace;

use crate::{error::Error, xml, Result};

fn lems_error<T: Into<String>>(what: T) -> Error {
    Error::Lems { what: what.into() }
}

#[derive(Debug)]
pub struct Lems {
    pub component_types: Vec<raw::ComponentType>,
    pub units: Vec<raw::Unit>,
    pub dimensions: Vec<raw::Dimension>,
}

impl Lems {
    pub fn from_file(paths: &[String], names: &[String]) -> Result<Self> {
        let mut result = Lems {
            component_types: Vec::new(),
            units: Vec::new(),
            dimensions: Vec::new(),
        };
        let mut todo = names.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        let mut done = vec![];
        while let Some(name) = todo.pop() {
            if done.contains(&name) {
                continue;
            }
            let mut ok = false;
            for path in paths {
                let mut file = PathBuf::from(path);
                file.push(&name);
                trace!("Reading LEMS file {:?}", file);
                match std::fs::read_to_string(file) {
                    Ok(xml) => {
                        ok = true;
                        let doc = Document::parse(&xml)?;
                        let root = doc.root_element();
                        let raw: raw::Lems = match root.tag_name().name() {
                            "Lems" => Ok(xml::XML::from_node(&doc.root_element())),
                            t => Err(lems_error(format!("Unknown doc kind {}", t))),
                        }?;
                        for item in raw.body {
                            match item {
                                raw::LemsBody::Include(inc) => todo.push(inc.file.to_string()),
                                raw::LemsBody::ComponentType(ct) => result.component_types.push(ct),
                                raw::LemsBody::Unit(un) => result.units.push(un),
                                raw::LemsBody::Dimension(dm) => result.dimensions.push(dm),
                                _ => {}
                            }
                        }
                    },
                    Err(e) => trace!("{:?}", e),
                }
            }
            if ok {
                done.push(name.to_string());
            } else {
                return Err(lems_error(format!(
                    "Could not find LEMS file {} in paths {:?}",
                    name, paths
                )));
            }
        }
        Ok(result)
    }
}
