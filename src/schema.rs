use std::collections::HashMap as Map;
use roxmltree::{Document, Node, TextPos};
use std::fs::File;
use std::io::prelude::*;

type Result<T> = std::result::Result<T, String>;

#[derive(Debug, Clone)]
enum Kind {
    String,
    Integer,
    Float,
    Class(String),
}

fn pos_from_node(node: &Node) -> TextPos {
    let pos = node.range().start;
    let doc = node.document();
    doc.text_pos_at(pos)
}

impl Kind {
    fn from_str(tag: &str) -> Self {
        match tag {
            "xs:integer" | "xs:nonNegativeInteger" | "xs:positiveInteger" => Kind::Integer,
            "xs:float" | "xs:double" => Kind::Float,
            "xs:string" | "xs:anyURI" => Kind::String,
            s => Kind::Class(s.to_string()),
        }
    }

    fn to_rs(&self) -> String {
        match self {
            Kind::Float   => "f64".to_string(),
            Kind::Integer => "i64".to_string(),
            Kind::String  => "String".to_string(),
            Kind::Class(n) => n.to_string(),
        }
    }
}

#[derive(Debug, Clone)]
enum Element {
    Element(String, Kind),
    Group(String),
}

#[derive(Debug, Clone)]
enum Count {
    One,
    Opt(Option<String>),
}

#[derive(Debug, Clone)]
struct Member {
    name:  String,
    count: Count,
    data:  Kind,
}

#[derive(Debug, Clone)]
enum Body {
    Empty,
    Any,
    Alt(Vec<Element>),
}

impl Body {
    fn merge(this: &Self, other: &Self) -> Self {
        match this {
            Body::Empty => other.clone(),
            Body::Any => if let Body::Alt(_) = other {
                panic!("Any + Alt");
            } else {
                Body::Any
            },
            Body::Alt(xs) => match other {
                Body::Alt(ys) => {
                    let mut rs: Vec<Element> = xs.to_vec();
                    rs.extend_from_slice(ys);
                    Body::Alt(rs)
                },
                Body::Empty => this.clone(),
                Body::Any => panic!("Any + Alt"),
            }
        }
    }

    fn insert(&mut self, el: &Element) {
        match self {
            Body::Empty => *self = Body::Alt(vec![el.clone()]),
            Body::Alt(els) => els.push(el.clone()),
            _ => panic!("Cannot insert {:?} into {:?}", el, self),
        }
    }
}

#[derive(Debug, Clone)]
struct Type {
    name:    String,
    members: Vec<Member>,
    base:    Option<String>,
    body:    Body,
}

#[derive(Default, Debug)]
struct Schema {
    types:  Map<String, Type>,
    alias:  Map<String, Kind>,
    groups: Map<String, Body>,
}

fn restriction(node: &Node) -> Result<Kind> {
    node.attribute("base")
        .map(Kind::from_str)
        .ok_or_else(|| String::from("Restriction requires attribute 'base'"))
}

fn simple_type(node: &Node) -> Result<(String, Kind)> {
    let name = node.attribute("name").ok_or_else(|| String::from("Unnamed simple type."))?;
    for child in node.children() {
        if child.is_comment() || child.is_text() { continue; }
        match child.tag_name().name() {
            "restriction" => return Ok((name.to_string(), restriction(&child)?)),
            "annotation"  => {},
            t => return Err(format!("Invalid tag '{}' in simple type.", t)),
        }
    }
    Err(String::from("Invalid simple type."))
}

fn element(node: &Node) -> Result<Element> {
    let name = node.attribute("name").ok_or_else(|| String::from("Unnamed element."))?;
    let typ  = node.attribute("type").ok_or_else(|| String::from("Untyped element."))?;
    Ok(Element::Element(name.to_string(), Kind::from_str(typ)))
}

fn group_def(node: &Node) -> Result<(String, Body)>{
    let name = node.attribute("name").ok_or_else(|| String::from("Unnamed group"))?;
    for child in node.children() {
        if child.is_comment() || child.is_text() { continue; }
        match child.tag_name().name() {
            "sequence" => return Ok((name.to_string(), sequence(&child)?)),
            "annotation"  => {},
            t => return Err(format!("Invalid tag '{}' in simple type.", t)),
        }
    }
    Err(String::from("Incomplete Group"))
}

fn attribute(node: &Node) -> Result<Member> {
    let name = node.attribute("name")
                   .ok_or_else(|| String::from("Unnamed attribute"))?
                   .to_string();
    let data  = node.attribute("type")
                    .map(Kind::from_str)
                    .ok_or_else(|| format!("Untyped attribute '{}' ({:?})", name, pos_from_node(node)))?;
    let dflt = node.attribute("default").map(|s| s.to_string());
    let count = match node.attribute("use") {
        Some("required") => Count::One,
        Some("optional") => Count::Opt(dflt),
        Some(e) => return Err(format!("Unknown value for 'use': {}", e)),
        None => Count::Opt(dflt),
    };
    Ok(Member { name, count, data })
}

fn group_ref(node: &Node) -> Result<Element> {
    let name = node.attribute("ref")
                   .ok_or_else(|| String::from("Reference name required"))?
                   .to_string();
    Ok(Element::Group(name))
}

fn sequence(node: &Node) -> Result<Body> {
    let mut body = Body::Empty;
    for child in node.children() {
        if child.is_comment() || child.is_text() { continue; }
        match child.tag_name().name() {
            "annotation" => {},
            "element"    => body.insert(&element(&child)?),
            "sequence" | "all" | "choice" => body = Body::merge(&body, &sequence(&child)?),
            "any"        => body = Body::merge(&body, &Body::Any),
            "group"      => body.insert(&group_ref(&child)?),
            t => return Err(format!("Invalid tag '{}' in sequence.", t)),
        }
    }
    Ok(body)
}

fn extension(node: &Node) -> Result<(Body, Vec<Member>, String)> {
    let base = node.attribute("base")
                   .ok_or_else(|| String::from("Extension requires base"))?
                   .to_string();

    let mut members = Vec::new();
    let mut body = Body::Empty;
    for child in node.children() {
        if child.is_comment() || child.is_text() { continue; }
        match child.tag_name().name() {
            "annotation" => {},
            "attribute" => members.push(attribute(&child)?),
            "sequence" | "all" | "choice" => body = Body::merge(&body, &sequence(&child)?),
            e => return Err(format!("Unexpected tag in extension: {:?}", e)),
        }
    }
    Ok((body, members, base))
}

fn complex_content(node: &Node) -> Result<(Body, Vec<Member>, String)> {
    for child in node.children() {
        if child.is_comment() || child.is_text() { continue; }
        match child.tag_name().name() {
            "annotation" => {},
            "extension" => return extension(&child),
            e => return Err(format!("Unexpected tag in complex content: {:?}", e)),
        }
    }
    Err(String::from("Invalid complexContent"))
}

fn complex_type(node: &Node) -> Result<(String, Type)> {
    let name = node.attribute("name").ok_or_else(|| String::from("Unnamed type"))?;
    let mut members = Vec::new();
    let mut body = Body::Empty;
    let mut base = None;
    for child in node.children() {
        if child.is_comment() || child.is_text() { continue; }
        match child.tag_name().name() {
            "annotation"  => {},
            "sequence" | "choice" => body = Body::merge(&body, &sequence(&child)?),
            "attribute" => members.push(attribute(&child)?),
            "anyAttribute" => {} // TODO make something useful here
            "complexContent" => {
                if base.is_some() { return Err(String::from("ComplexType must have at most one base.")); }
                let (bd, mut ms, bs) = complex_content(&child)?;
                body = Body::merge(&body, &bd);
                base = Some(bs);
                members.append(&mut ms);
            },
            e => return Err(format!("Unexpected tag in complex type: {:?}", e)),
        }
    }
    Ok((name.to_string(), Type { name: name.to_string(), members, base, body }))
}

fn schema(xml: &str) -> Result<Schema> {
    let schema = Document::parse(xml).unwrap();
    let root   = schema.root_element();
    let mut state = Schema::default();
    for child in root.children() {
        if child.is_comment() || child.is_text() { continue; }
        match child.tag_name().name() {
            "complexType" => {
                let (k, v) = complex_type(&child)?;
                state.types.insert(k, v);
            },
            "simpleType"  => {
                let (k, v) = simple_type(&child)?;
                state.alias.insert(k, v);
            }
            "group" => {
                let (k, v) = group_def(&child)?;
                state.groups.insert(k, v);
            },
            "element" => {}, // We skip the root!
            tag => return Err(format!("Unknown tag {:?} in schema at {:?}", tag, pos_from_node(&child))),
        }
    }
    Ok(state)
}

fn inherit(tys: &Map<String, Type>) -> Map<String, Type> {
    let mut result = Map::new();
    for (n, ty) in tys {
        let mut t = ty.clone();
        let mut base = t.base.clone();
        while let Some(b) = base {
            if let Some(x) = tys.get(&b) {
                t.members.extend(x.members.clone());
                t.body = Body::merge(&t.body, &x.body);
                base = x.base.clone();
            } else {
                panic!("Found no base type {}", b);
            }
        }
        t.base = None;
        result.insert(n.to_string(), t);
    }
    result
}

// Flatten groups into concrete types
fn flatten(tys: &Map<String, Type>, groups: &Map<String, Body>) -> Map<String, Type> {
    let mut result = Map::new();
    for (n, ty) in tys {
        // only body can have groups
        let body: Body = match &ty.body {
            b @ (Body::Empty | Body::Any) => b.clone(),
            Body::Alt(ms) => {
                let mut body = Body::Empty;
                for m in ms {
                    match m {
                        Element::Group(g) => {
                            if let Some(es) = groups.get(g) {
                                body = Body::merge(&body, es);
                            } else {
                                panic!("Found no group {}", g);
                            }
                        },
                        e => { body.insert(e); },
                    }
                }
                body
            },
        };
        let mut t = ty.clone();
        t.body = body;
        result.insert(n.to_string(), t);
    }
    result
}

fn dealias(tys: &Map<String, Type>, alias: &Map<String, Kind>) -> Map<String, Type> {
    let mut result = Map::new();
    for (n, ty) in tys {
        let mut t = ty.clone();
        for m in &mut t.members {
            if let Kind::Class(c) = &m.data {
                if let Some(a) = alias.get(c) {
                    m.data = a.clone();
                }
            }
        }

        if let Body::Alt(es) = t.body {
            let mut rs = Vec::new();
            for e in es {
                let x = match e {
                    Element::Element(n, Kind::Class(c)) if alias.contains_key(&c) => Element::Element(n, alias.get(&c).unwrap().clone()),
                    a => a.clone(),
                };
                rs.push(x);
            }
            t.body = Body::Alt(rs);
        }
        result.insert(n.to_string(), t);
    }
    result
}

fn emit_src(state: &Schema) -> Result<String> {
    // Squeeze input
    let data = flatten(&state.types, &state.groups);
    let data = inherit(&data);
    let data = dealias(&data, &state.alias);

    let mut lines = vec![String::from("#![allow(non_camel_case_types, non_snake_case, unused_variables)]"),
                         String::from("#![allow(clippy::many_single_char_names, clippy::large_enum_variant)]"),
                         String::from("use roxmltree::Node;"),
                         String::from(""),
                         String::from("use crate::xml::XML;"),
                         String::from(""),];
    for (_, t) in data {
        let mut ty = Vec::new();
        let mut nd = Vec::new();
        let mut attrs = Vec::new();

        nd.push(format!("impl XML for {} {{", t.name));
        nd.push(String::from("  fn from_node(node: &Node) -> Self {"));

        ty.push("#[derive(Debug, Clone, PartialEq)]".to_string());
        ty.push(format!("pub struct {} {{", t.name));

        let mut bd = Vec::new();

        for m in &t.members {
            let name = match m.name.as_ref() {
                n @ ("as" | "type") => {
                    format!("r#{}", n)
                },
                "" => "body".to_string(),
                n => n.to_string(),
            };
            attrs.push(name.clone());
            let convert = match m.data {
                Kind::Float   => "|s| s.parse::<f64>().unwrap()",
                Kind::Integer => "|s| s.parse::<i64>().unwrap()",
                Kind::String  => "|s| s.to_string()",
                _ => "",
            };
            let unwrap = if let Count::Opt(None) = &m.count { "" } else { ".unwrap()" };
            let default = if let Count::Opt(Some(d)) = &m.count { format!(".or(Some(\"{}\"))", d) } else { "".to_string() };
            let kind = if let Count::Opt(None) = &m.count {format!("Option<{}>", m.data.to_rs()) } else { m.data.to_rs() };
            ty.push(format!("  pub {}: {},", name, kind));
            nd.push(format!("    let {} = node.attribute(\"{}\"){}.map({}){};",
                            name, m.name, default, convert, unwrap));
        }
        match t.body {
            Body::Alt(ref xs) => {
                let mut lp = vec![String::from("    let mut body = Vec::new();"),
                                  String::from("    for child in node.children() {"),
                                  String::from("      if child.is_comment() || child.is_text() { continue; }"),
                                  String::from("      match child.tag_name().name() {")];

                bd.push("#[derive(Debug, Clone, PartialEq)]".to_string());
                bd.push(format!("pub enum {}Body {{", t.name));

                for x in xs {
                    if let Element::Element(k, v) = x {
                        let v = v.to_rs();
                        bd.push(format!("  {}({}),", k, v));
                        lp.push(format!("        \"{}\" => body.push({}Body::{}({}::from_node(&child))),",
                                        k, t.name, k, v));
                    } else {
                        return Err(format!("Unresolved element {:?}", x));
                    }
                }
                lp.push(format!("        t => panic!(\"Unexpected tag {{}} in body of {}.\", t)", t.name));
                lp.push(String::from("      };"));
                lp.push(String::from("    }"));
                nd.append(&mut lp);
                attrs.push(String::from("body"));

                bd.push("}\n".to_string());

                ty.push(format!("  pub body: Vec<{}Body>", t.name));
            },
            Body::Any | Body::Empty => {}
        }

        nd.push(format!("    {} {{ {} }}", t.name, attrs.join(", ")));
        nd.push(String::from("  }"));
        nd.push(String::from("}"));
        nd.push(String::from(""));

        ty.push(String::from("}"));
        lines.append(&mut bd);
        lines.append(&mut ty);
        lines.push(format!(""));
        lines.append(&mut nd);
    }

    Ok(lines.join("\n"))
}

fn main() -> Result<()> {
    let cwd = std::env::current_dir().map_err(|e| e.to_string())?;

    let nm = format!("{}/ext/NeuroML2/Schemas/NeuroML2/NeuroML_v2.2.xsd", cwd.to_string_lossy());
    let xml = std::fs::read_to_string(nm).map_err(|e| e.to_string())?;
    let data = schema(&xml)?;
    let mut out = File::create("src/neuroml/raw.rs").map_err(|e| e.to_string())?;
    out.write(emit_src(&data)?.as_bytes()).map_err(|e| e.to_string())?;

    let nm = format!("{}/ext/LEMS/Schemas/LEMS/LEMS_v0.7.6.xsd", cwd.to_string_lossy());
    let xml = std::fs::read_to_string(nm).map_err(|e| e.to_string())?;
    let data = schema(&xml)?;
    let mut out = File::create("src/lems/raw.rs").map_err(|e| e.to_string())?;
    out.write(emit_src(&data)?.as_bytes()).map_err(|e| e.to_string())?;

    Ok(())
}
