use std::env::args;
use std::collections::HashMap as Map;
use quick_xml::{
    Reader,
    events::{Event, BytesStart}};
use color_eyre::eyre::{WrapErr, Result, eyre};

#[derive(Debug, Clone)]
enum Kind {
    String,
    Integer,
    Float,
    Class(String),
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
    Any,
    Alt(Vec<Element>),
}

impl Body {
    fn merge(this: &Self, other: &Self) -> Self {
        match this {
            Body::Any => if let Body::Any = other {
                Body::Any
            } else {
                panic!("Any + Alt");
            }
            Body::Alt(xs) => if let Body::Alt(ys) = other {
                let mut rs: Vec<Element> = xs.to_vec();
                rs.extend_from_slice(ys);
                Body::Alt(rs)
            } else {
                panic!("Any + Alt");
            }

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

struct State<'a> {
    reader: Reader<&'a [u8]>,
    buffer: Vec<u8>,
    types:  Map<String, Type>,
    alias:  Map<String, Kind>,
    groups: Map<String, Vec<Element>>,
}

impl<'a> State<'a> {
    fn new(xml: &'a str) -> Self {
        let mut reader = Reader::from_str(xml);
        reader.trim_text(true)
              .expand_empty_elements(true);
        let buffer = Vec::new();
        let types = Map::new();
        let alias = Map::new();
        let groups = Map::new();
        State { reader, buffer, types, alias, groups }
    }
}

fn annotation(state: &mut State) {
    loop {
        match state.reader.read_event(&mut state.buffer) {
            Ok(Event::End(e))   if e.name() == b"xs:annotation" => break,
            _ => {},
        }
    }
}

fn restriction(state: &mut State, bytes: BytesStart) -> Option<Kind> {
    let mut kind = None;
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            b"base" => kind = Some(Kind::from_str(state.reader.decode(&attr.value).unwrap())),
            e => panic!("Unexpected attribute in restriction: {:?}", state.reader.decode(e)),
        }
    }

    loop {
        match state.reader.read_event(&mut state.buffer) {
            Ok(Event::End(e)) if e.name() == b"xs:restriction" => break,
            Ok(_) => {},
            e => panic!("Unexpected tag in restriction: {:?}", e),
        }
    }
    kind
}

fn simple_type(state: &mut State, bytes: BytesStart) {
    let mut name = Vec::new();
    let mut base = None;
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            b"name" => name = attr.value.to_vec(),
            e => panic!("Unexpected attribute in simple type ({:?}): {:?}",  state.reader.decode(&name), state.reader.decode(e)),
        }
    }

    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Start(e)) if e.name() == b"xs:annotation" => annotation(state),
            Ok(Event::Comment(_))  => {},
            Ok(Event::Start(e)) if e.name() == b"xs:restriction" => {
                let e = e.into_owned();
                base = restriction(state, e);
            },
            Ok(Event::End(e))   if e.name() == b"xs:simpleType" => break,
            e => panic!("Unexpected tag in simple type: {:?}", e),
        }
    }
    state.alias.insert(state.reader.decode(&name).unwrap().to_string(), base.unwrap());
}

fn element(state: &mut State, bytes: BytesStart) -> Element {
    let mut n = None;
    let mut t = None;
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            b"name" => n = Some(state.reader.decode(&attr.value).unwrap().to_string()),
            b"type" => t = Some(Kind::from_str(state.reader.decode(&attr.value).unwrap())),
            b"minOccurs" | b"maxOccurs" => {}
            e => panic!("Unexpected attribute in element: {:?}", state.reader.decode(&e).unwrap()),
        }
    };

    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Comment(_))  => {},
            Ok(Event::End(e)) if e.name() == b"xs:element" => break,
            e => panic!("Unexpected tag in element: {:?}", e),
        }
    }
    Element::Element(n.unwrap(), t.unwrap())
}

fn any(state: &mut State) {
    loop {
        match state.reader.read_event(&mut state.buffer) {
            Ok(Event::End(e)) if e.name() == b"xs:any" => break,
            Ok(Event::Comment(_)) => {},
            e => panic!("Unexpected tag in any: {:?}", e),
        }
    }
}

fn group_ref(state: &mut State, bytes: BytesStart) -> Element {
    let mut n = None;
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            b"ref" => n = Some(state.reader.decode(&attr.value).unwrap().to_string()),
            e => panic!("Unexpected attribute in group_ref: {:?}", state.reader.decode(&e).unwrap()),
        }
    };

    loop {
        match state.reader.read_event(&mut state.buffer) {
            Ok(Event::End(e)) if e.name() == b"xs:group" => break,
            Ok(Event::Comment(_)) => {},
            e => panic!("Unexpected tag in group: {:?}", e),
        }
    }
    Element::Group(n.expect("No name given to group reference."))
}

fn group_def(state: &mut State, bytes: BytesStart) {
    let mut n = None;
    let mut r = None;
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            b"name" => n = Some(state.reader.decode(&attr.value).unwrap().to_string()),
            e => panic!("Unexpected attribute in group_ref: {:?}", state.reader.decode(&e).unwrap()),
        }
    };

    loop {
        match state.reader.read_event(&mut state.buffer) {
            Ok(Event::End(e)) if e.name() == b"xs:group" => break,
            Ok(Event::Start(e)) if e.name() == b"xs:annotation" => annotation(state),
            Ok(Event::Start(e)) if e.name() == b"xs:sequence" => {
                let (a, s) = sequence(state);
                r = Some(s);
                // TODO look at any
            },
            Ok(Event::Comment(_)) => {},
            e => panic!("Unexpected tag in group: {:?}", e),
        }
    }
    state.groups.insert(n.expect("No name given to group definition."),
                        r.expect("No data given to group definition."));
}


fn sequence(state: &mut State) -> (bool, Vec<Element>) {
    let mut result = Vec::new();
    let mut have_any = false;
    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Start(e)) if e.name() == b"xs:annotation" => annotation(state),
            Ok(Event::Start(e)) if e.name() == b"xs:group" => {
                let e = e.into_owned();
                let r = group_ref(state, e);
                result.push(r);
            },
            Ok(Event::Start(e)) if e.name() == b"xs:choice" => {
                let r = choice(state);
                result.extend(r);
            },
            Ok(Event::Comment(_))  => {},
            Ok(Event::Start(e)) if e.name() == b"xs:any" => {
                any(state);
                have_any = true;
            }
            Ok(Event::Start(e)) if e.name() == b"xs:element" => {
                let e = e.into_owned();
                let r = element(state, e);
                result.push(r);
            },
            Ok(Event::End(e)) if e.name() == b"xs:sequence" => break,
            e => panic!("Unexpected tag in sequence: {:?}", e),
        }
    }
    if have_any {
        if !result.is_empty() {
            panic!("Any + non-empty body in sequence");
        }
    }
    (have_any, result)
}

fn choice(state: &mut State) -> (bool, Vec<Element>) {
    let mut result = Vec::new();
    let mut have_any = false;
    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Start(e)) if e.name() == b"xs:annotation" => annotation(state),
            Ok(Event::Comment(_))  => {},
            Ok(Event::Start(e)) if e.name() == b"xs:any" => {
                have_any = true;
                any(state);
            }
            Ok(Event::Start(e)) if e.name() == b"xs:group" => {
                let e = e.into_owned();
                let r = group_ref(state, e);
                result.push(r);
            },
            Ok(Event::Start(e)) if e.name() == b"xs:element" => {
                let e = e.into_owned();
                let r = element(state, e);
                result.push(r);
            },
            Ok(Event::End(e)) if e.name() == b"xs:choice" => break,
            e => panic!("Unexpected tag in choice: {:?}", e),
        }
    }
    if have_any {
        if !result.is_empty() {
            panic!("Any + non-empty body in choice");
        }
    }
    (have_any, result)
}

fn all(state: &mut State) -> Vec<Element> {
    let mut result = Vec::new();
    let mut have_any = false;
    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Start(e)) if e.name() == b"xs:annotation" => annotation(state),
            Ok(Event::Comment(_))  => {},
            Ok(Event::Start(e)) if e.name() == b"xs:any" => {
                any(state);
                have_any = true;
            }
            Ok(Event::Start(e)) if e.name() == b"xs:element" => {
                let e = e.into_owned();
                let r = element(state, e);
                result.push(r);
            },
            Ok(Event::End(e)) if e.name() == b"xs:all" => break,
            e => panic!("Unexpected tag in all: {:?}", e),
        }
    }
    if have_any {
        if !result.is_empty() {
            panic!("Any + non-empty body in all");
        }
    }
    result
}

fn attribute(state: &mut State, bytes: BytesStart) -> Member {
    let mut t = None;
    let mut n = None;
    let mut u = Some(Count::Opt(None));
    let mut d = None;
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            b"name" => n = Some(state.reader.decode(&attr.value).unwrap().to_string()),
            b"type" => t = Some(Kind::from_str(state.reader.decode(&attr.value).unwrap())),
            b"use" => u = match attr.value.as_ref() {
                b"required" => Some(Count::One),
                b"optional" => Some(Count::Opt(None)),
                e => { panic!("Attribute unknown count {:?}", state.reader.decode(e)); },
            },
            b"default" => d = Some(state.reader.decode(&attr.value).unwrap().to_string()),
            e => panic!("Unexpected attribute in element: {:?}", state.reader.decode(&e).unwrap()),
        }
    };

    loop {
        match state.reader.read_event(&mut state.buffer) {
            Ok(Event::Start(e)) if e.name() == b"xs:annotation" => annotation(state),
            Ok(Event::End(e)) if e.name() == b"xs:attribute" => break,
            Ok(Event::Comment(_)) => {},
            e => panic!("Unexpected tag in attribute: {:?}", e),
        }
    }
    let count = match u {
        Some(Count::Opt(_)) => Count::Opt(d),
        Some(x) if d.is_none() => x,
        _ => { panic!("Illegal data count in '{:?}'", bytes.unescape_and_decode(&state.reader)); },
    };
    let name = n.expect("No name defined for attribute.");
    let data = t.expect(&format!("No data defined in attribute '{}'.", name));
    Member { name, count, data }
}

fn extension(state: &mut State, bytes: BytesStart) -> (Vec<Member>, String) {
    let mut base = None;
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            b"base" => base = Some(state.reader.decode(&attr.value).unwrap().to_string()),
            e => panic!("Unexpected tag in extension: {:?}", state.reader.decode(&e).unwrap()),
        }
    }

    let mut members = Vec::new();
    let mut body = Vec::new();
    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Start(e)) if e.name() == b"xs:annotation" => annotation(state),
            Ok(Event::Start(e)) if e.name() == b"xs:sequence" => {
                let (any, alt) = sequence(state);
                body.extend(alt);
                // TODO look at any
            }
            Ok(Event::Start(e)) if e.name() == b"xs:choice" => {
                let alt = choice(state);
                body.extend(alt);
            }
            Ok(Event::Start(e)) if e.name() == b"xs:all" => {
                let alt = all(state);
                body.extend(alt);
            }
            Ok(Event::Start(e)) if e.name() == b"xs:attribute" => {
                let e = e.into_owned();
                let m = attribute(state, e);
                members.push(m);
            }
            Ok(Event::Comment(_))  => {},
            Ok(Event::End(e)) if e.name() == b"xs:extension" => break,
            e => panic!("Unexpected tag in extension: {:?}", e),
        }
    }
    (members, base.unwrap())
}

fn complex_content(state: &mut State, bytes: BytesStart) -> (Vec<Member>, String) {
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            e => panic!("Unexpected tag in complex content: {:?}", state.reader.decode(&e).unwrap()),
        }
    }

    let mut result = None;
    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Start(e)) if e.name() == b"xs:annotation" => annotation(state),
            Ok(Event::Start(e)) if e.name() == b"xs:extension" => {
                let e = e.into_owned();
                result = Some(extension(state, e));
            }
            Ok(Event::Comment(_))  => {},
            Ok(Event::End(e)) if e.name() == b"xs:complexContent" => break,
            e => panic!("Unexpected tag in complex content: {:?}", e),
        }
    }
    result.unwrap()
}

fn any_attribute(state: &mut State, bytes: BytesStart) {
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            b"processContents" => {},
            e => panic!("Unexpected tag in anyAttribute: {:?}", state.reader.decode(&e).unwrap()),
        }
    }
    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Comment(_))  => {},
            Ok(Event::End(e)) if e.name() == b"xs:anyAttribute" => break,
            e => panic!("Unexpected tag in anyAttribute: {:?}", e),
        }
    }
}

fn complex_type(state: &mut State, bytes: BytesStart) {
    let mut name = None;
    for attr in bytes.attributes() {
        let attr = attr.unwrap();
        match attr.key {
            b"name" => name = Some(state.reader.decode(&attr.value).unwrap().to_string()),
            e => panic!("Unexpected tag in complex type: {:?}", state.reader.decode(&e).unwrap()),
        }
    };

    let mut members = Vec::new();
    let mut body = Vec::new();
    let mut base = None;
    let mut have_any = false;
    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Start(e)) if e.name() == b"xs:annotation" => annotation(state),
            Ok(Event::Start(e)) if e.name() == b"xs:sequence" => {
                let (a, rs) = sequence(state);
                body.extend(rs);
                have_any |= a;
            }
            Ok(Event::Start(e)) if e.name() == b"xs:choice" => {
                let alt = choice(state);
                body.extend(alt);
            }
            Ok(Event::Start(e)) if e.name() == b"xs:attribute" => {
                let e = e.into_owned();
                let m = attribute(state, e);
                members.push(m);
            }
            Ok(Event::Start(e)) if e.name() == b"xs:anyAttribute" => {
                let e = e.into_owned();
                any_attribute(state, e);
                have_any = true;
            }
            Ok(Event::Start(e)) if e.name() == b"xs:complexContent" => {
                let e = e.into_owned();
                let (mut ms, b) = complex_content(state, e);
                base = Some(b);
                members.append(&mut ms);
            },
            Ok(Event::Comment(_))  => {},
            Ok(Event::End(e)) if e.name() == b"xs:complexType" => break,
            e => panic!("Unexpected tag in complex type: {:?}", e),
        }
    }
    let body = if have_any {
        if !body.is_empty() {
            panic!("Any + non-empty body in complex_type");
        }
        Body::Any
    } else {
        Body::Alt(body)
    };
    let name = name.expect("Unamed type.");
    state.types.insert(name.clone(), Type { name, members, base, body });
}

fn resolve(tys: &Map<String, Type>, alias: &Map<String, Kind>, groups: &Map<String, Vec<Element>>) -> Map<String, Type> {
    // Splat in base types
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
    let tys = result;
    // Resolve aliases and groups
    result = Map::new();
    for (n, ty) in tys {
        let mut members = Vec::new();
        for m in &ty.members {
            let data = if let Kind::Class(k) = &m.data {
                if let Some(x) = alias.get(k) {
                    x.clone()
                } else {
                    panic!("Could not resolve alias {}", k);
                }
            } else {
                m.data.clone()
            };
            members.push(Member {name: m.name.clone(), data, count: m.count.clone()})
        }
        let body = if let Body::Alt(ms) = ty.body {
            let mut xs = Vec::new();
            for m in ms {
                match m {
                    e @ Element::Element(_, _) => xs.push(e.clone()),
                    Element::Group(g) => {
                        if let Some(es) = groups.get(&g) {
                            xs.extend(es.iter().cloned());
                        } else {
                            panic!("Could not resolve group {}", g);
                        }
                    }
                    x => panic!("Unexpected body element {:?}", x),
                }
            }
            Body::Alt(xs)
        } else {
            Body::Any
        };
        result.insert(n.to_string(), Type { name: ty.name.clone(), members, base: ty.base.clone(), body });
    }
    result
}

fn schema(state: &mut State) -> Result<()> {
    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Start(e)) if e.name() == b"xs:simpleType"  => {
                let e = e.into_owned();
                simple_type(state, e);
            },
            Ok(Event::Start(e)) if e.name() == b"xs:complexType" => {
                let e = e.into_owned();
                complex_type(state, e);
            },
            Ok(Event::Start(e)) if e.name() == b"xs:group" => {
                let e = e.into_owned();
                group_def(state, e);
            },
            Ok(Event::End(e)) if e.name() == b"xs:schema" => break,
            Ok(Event::Comment(_)) => {},
            Ok(e)  => { return Err(eyre!("Unexpected tag in schema: {:?}", e)); },
            Err(e) => { return Err(eyre!("Error in input file {:}", e)); },
        }
    }
    Ok(())
}

fn parse(file: &str) -> Result<Map<String, Type>> {
    let xml = std::fs::read_to_string(file)?;
    let mut state = State::new(&xml);
    loop {
        let evt = state.reader.read_event(&mut state.buffer);
        match evt {
            Ok(Event::Start(e)) if e.name() == b"xs:schema" => schema(&mut state)?,
            Ok(Event::Eof) => break,
            Ok(Event::Comment(_)) | Ok(Event::Decl(_)) => {},
            Ok(e)  => { return Err(eyre!("Unknown tag at top level: {:?}", e)); },
            Err(e) => { return Err(eyre!("Error in input file {:}", e)); },
        }

    }
    Ok(resolve(&state.types, &state.alias, &state.groups))
}

fn main() -> Result<()> {
    color_eyre::install()?;
    let file = args().nth(1)
                     .ok_or(eyre!("Usage: schema <schema.xml>"))
                     .wrap_err("Failed to process args.")?;
    let data = parse(&file)?;

    let mut lines = Vec::new();
    let mut tags = Vec::new();

    lines.push(String::from("use serde::Deserialize;"));
    lines.push(String::from("use std::collections::HashMap as Map;"));
    lines.push(String::from("use serde_json::Value;"));
    lines.push(String::from(""));
    lines.push(String::from("pub fn mk_1_f64() -> f64 { 1.0 }"));
    lines.push(String::from("pub fn mk_0_5_f64() -> f64 { 0.5 }"));
    lines.push(String::from(""));
    for (n, t) in data {
        let mut ty = Vec::new();
        ty.push("#[derive(Debug, Deserialize)]".to_string());
        ty.push(format!("pub struct {} {{", t.name));
        for m in t.members {
            let mut attrs = Vec::new();
            let name = match m.name.as_ref() {
                n @ ("as" | "type") => {
                    attrs.push(format!("rename=\"{}\"", n));
                    format!("r#{}", n)
                },
                "" => "body".to_string(),
                n => n.to_string(),
            };
            let mut kind = match m.data {
                Kind::Float   => "f64".to_string(),
                Kind::Integer => "i64".to_string(),
                Kind::String  => "String".to_string(),
                Kind::Class(n) => panic!("Unresolved alias {:?}", n),
            };
                // Kind::Alt(ref xs) => {
                //     lines.push(format!("#[derive(Debug, Deserialize)]"));
                //     lines.push(format!("pub enum {}Body {{", t.name));
                //     for x in xs {
                //         if let Element::Element(k, v) = x {
                //             lines.push(format!("  {}({}),", k, v));
                //         } else {
                //             panic!("Unresolved element {:?}", x);
                //         }
                //     }
                //     lines.push(format!("}}"));
                //     lines.push(format!(""));
                //     attrs.push(format!("rename=\"$value\""));
                //     format!("Vec<{}Body>", t.name)
                // },
                // Kind::Any => {
                //     attrs.push(String::from("default"));
                //     attrs.push(String::from("flatten"));
                //     format!("Map<String, Value>")
                // }

            match m.count {
                Count::One => {},
                Count::Opt(d) => {
                    if let Kind::String = m.data {
                        kind = format!("Option<{}>", kind);
                    } else {
                        if d.is_some() && d.clone().unwrap() != "0" {
                            let val = str::replace(&d.unwrap(), ".", "_");
                            attrs.push(format!("default=\"mk_{}_{}\"", val, kind));
                        } else {
                            attrs.push(format!("default"));
                        }
                    }
                },
            }
            if !attrs.is_empty() { ty.push(format!("  #[serde({})]", attrs.join(", "))); }
            ty.push(format!("  {}: {},", name, kind));
        }
        tags.push(format!("\"{}\".as_bytes().to_vec()", t.name));
        ty.push(String::from("}"));
        ty.push(String::from(""));
        lines.append(&mut ty);
    }

    lines.push(format!("pub fn known_tags() -> Vec<Vec<u8>> {{"));
    lines.push(format!("vec![{}]", tags.join(", ")));
    lines.push(format!("}}"));
    println!("{}", lines.join("\n"));
    Ok(())
}
