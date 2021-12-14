pub mod raw;
pub mod file;

use tracing::info;

use crate::xml;
use crate::Result;
use roxmltree::Document;

#[derive(Debug)]
pub struct Lems {
    pub component_types: Vec<raw::ComponentType>,
    pub units: Vec<raw::Unit>,
    pub dimensions: Vec<raw::Dimension>,
}

impl Lems {
    pub fn from_file(paths: &[String], names: &[String]) -> Result<Self> {
        let mut result = Lems { component_types: Vec::new(), units: Vec::new(), dimensions: Vec::new() };
        let mut todo = names.iter().map(|s| s.to_string()).collect::<Vec<_>>();
        let mut done = vec![];
        while let Some(name) = todo.pop() {
            if done.contains(&name) { continue; }
            let mut ok = false;
            for path in paths {
                info!("Trying {}/{}", path, name);
                if let Ok(xml) = std::fs::read_to_string(format!("{}/{}", path, name)) {
                    ok = true;
                    let doc = Document::parse(&xml).map_err(|_| format!(""))?;
                    let root = doc.root_element();
                    let raw: raw::Lems = match root.tag_name().name() {
                        "Lems" => Ok(xml::XML::from_node(&doc.root_element())),
                        t => Err(format!("Unknown doc kind {}", t)),
                    }?;
                    for item in raw.body {
                        match item {
                            raw::LemsBody::Include(inc)      => todo.push(inc.file.to_string()),
                            raw::LemsBody::ComponentType(ct) => result.component_types.push(ct),
                            raw::LemsBody::Unit(un)          => result.units.push(un),
                            raw::LemsBody::Dimension(dm)    => result.dimensions.push(dm),
                            _ => {},
                        }
                    }
                }
            }
            if ok {
                done.push(name.to_string());
            } else {
                return Err(format!("Could not find LEMS file {} in paths {:?}", name, paths));
            }
        }
        Ok(result)
    }
}
