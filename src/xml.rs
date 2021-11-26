#![allow(clippy::upper_case_acronyms)]

use roxmltree::Node;

pub trait XML {
     fn from_node(node: &Node) -> Self;
}

impl XML for String {
    fn from_node(node: &Node) -> Self {
        node.text().unwrap_or_else(|| panic!("Illegal conversion {:?} -> String", node)).to_string()
    }
}
