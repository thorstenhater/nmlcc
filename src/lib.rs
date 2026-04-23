#![allow(soft_unstable)]

pub use std::collections::BTreeMap as Map;
pub use std::collections::BTreeSet as Set;

pub mod acc;
pub mod bundle;
pub mod error;
pub mod expr;
pub mod instance;
pub mod lems;
pub mod network;
pub mod neuroml;
pub mod nmodl;
pub mod variable;
pub mod xml;

pub fn get_runtime_types(lems: &mut lems::file::LemsFile, nml: &[String]) -> error::Result<()> {
    neuroml::process_files(nml, |_, node| {
        if node.tag_name().name() == "ComponentType" {
            let ct: lems::raw::ComponentType = xml::XML::from_node(node);
            lems.add_component_type(&ct)?;
        }
        Ok(())
    })
}
