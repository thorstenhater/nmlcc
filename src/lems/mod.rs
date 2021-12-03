pub mod raw;
pub mod file;

use roxmltree::Document;
use crate::xml;

use crate::Result;

#[derive(Debug)]
pub struct Lems {
    pub component_types: Vec<raw::ComponentType>,
    pub units: Vec<raw::Unit>,
    pub dimensions: Vec<raw::Dimension>,
}

impl Lems {
    pub fn from_file(path: &str, name: &str) -> Result<Self> {
        let mut result = Lems { component_types: Vec::new(), units: Vec::new(), dimensions: Vec::new() };
        let mut todo = vec![name.to_string()];
        let mut done = vec![];
        while let Some(name) = todo.pop() {
            if done.contains(&name) { continue; }
            done.push(name.to_string());
            let xml = std::fs::read_to_string(format!("{}/{}", path, name)).map_err(|_| "File not found")?;
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
        Ok(result)
    }
}
