use std::env::args;
use quick_xml::{
    Reader,
    events::Event,};

#[derive(Debug)]
enum Data {
    String,
    Integer,
    Float,
    Element(String),
    Alt(Vec<String>)
}

#[derive(Debug)]
enum Count {
    One,
    Opt(Option<String>),
    Many,
}

#[derive(Debug)]
struct Member {
    name:  String,
    count: Count,
    data:  Data,
}

#[derive(Debug, Default)]
struct Ty {
    name:    String,
    members: Vec<Member>,
}

fn parse_schema(file: &str) -> Vec<Ty> {
    let xml = std::fs::read_to_string(file).unwrap();
    let mut reader = Reader::from_str(&xml);
    reader.trim_text(true)
          .expand_empty_elements(true);
    let mut buf = Vec::new();
    let mut typ = Ty::default();
    let mut alt = Vec::new();
    let mut res = Vec::new();
    loop {
        match reader.read_event(&mut buf) {
            Ok(Event::Eof) => break,
            Ok(Event::Start(e)) if e.name() == b"xs:complexType" => {
                typ = Ty::default();
                for attr in e.attributes() {
                    let attr = attr.unwrap();
                    match attr.key {
                        b"name" => typ.name = reader.decode(&attr.value).unwrap().to_string(),
                        e => { println!("Key: {:?}", reader.decode(e).unwrap()); },
                    }
                }
            },
            Ok(Event::End(e)) if e.name() == b"xs:complexType" => {
                res.push(typ);
                typ = Ty::default();
            },
            Ok(Event::Start(e)) if e.name() == b"xs:attribute" => {
                let mut name    = "".to_string();
                let mut count   = Count::One;
                let mut data    = Data::String;
                let mut default = None;
                for attr in e.attributes() {
                    let attr = attr.unwrap();
                    match attr.key {
                        b"name" => name = reader.decode(&attr.value).unwrap().to_string(),
                        b"default" => default = Some(reader.decode(&attr.value).unwrap().to_string()),
                        b"type" => data = match &*attr.value {
                            b"xs:integer" => Data::Integer,
                            b"xs:float" | b"xs:double" => Data::Float,
                            b"xs:string"  => Data::String,
                            e => Data::Element(reader.decode(e).unwrap().to_string()),
                        },
                        b"use" => count = match &*attr.value {
                            b"optional" => Count::Opt(None),
                            b"required" => Count::One,
                            _ => panic!("Invalid Count"),
                        },
                        e => { println!("Key: {:?}", reader.decode(e).unwrap()); },
                    }
                }
                if let Count::Opt(_) = count {
                    count = Count::Opt(default)
                }
                typ.members.push(Member { name, count, data });
            },
            Ok(Event::End(e)) if e.name() == b"xs:attribute" => {},
            Ok(Event::Start(e)) if e.name() == b"xs:sequence" => {
                alt = vec![];
            },
            Ok(Event::End(e)) if e.name() == b"xs:sequence" => {
                typ.members.push(Member { name: "body".to_string(),
                                          count: Count::Many,
                                          data: Data::Alt(alt),});
                alt = vec![];
            },
            Ok(Event::Start(e)) if e.name() == b"xs:element" => {
                let mut nm = "".to_string();
                let mut ty = "".to_string();
                for attr in e.attributes() {
                    let attr = attr.unwrap();
                    match attr.key {
                        b"name" => nm = reader.decode(&attr.value).unwrap().to_string(),
                        b"type" => ty = reader.decode(&attr.value).unwrap().to_string(),
                        b"minOccurs" | b"maxOccurs" => {},
                        e => { println!("Key {:?} {:?}", e, b"name"); },
                    }
                }
                assert_eq!(nm, ty);
                alt.push(nm);
            },
            Ok(Event::End(e)) if e.name() == b"xs:element" => {},
            Ok(_) => {},
            Err(e) => panic!("Error in input file {:}", e),
        }
    }
    res
}

fn main() {
    let file = args().nth(1).ok_or("Usage: schema <schema.xml>").unwrap();
    let data = parse_schema(&file);

    let mut lines = Vec::new();

    lines.push(String::from("use serde::Deserialize;"));
    lines.push(String::from(""));
    lines.push(String::from("fn mk_1_f64() -> f64 { 1.0 }"));
    lines.push(String::from(""));
    for t in data {
        let mut ty = Vec::new();
        ty.push("#[derive(Debug, PartialEq, Eq, Deserialize)]".to_string());
        ty.push(format!("struct {} {{", t.name));
        for m in t.members {
            match m.count {
                Count::One => {
                    let dt = match m.data {
                        Data::Float   => "f64",
                        Data::Integer => "i64",
                        Data::String  => "String",
                        x => panic!("Invalid data kind {:?} not a simple type.", x),
                    };
                    ty.push(format!("  {}: {},", m.name, dt));
                }

                Count::Opt(d) => {
                    match m.data {
                        Data::Float => {
                            if d.is_some() && d.clone().unwrap() != "0" {
                                ty.push(format!("  #[serde(default=\"mk_{}_f64\")]", &d.unwrap()));
                            } else {
                                ty.push(format!("  #[serde(default)]"));
                            }
                            ty.push(format!("  {}: f64,", m.name));
                        },
                        Data::Integer => {
                            if d.is_some() && d.clone().unwrap() != "0" {
                                ty.push(format!("  #[serde(default=\"mk_{}_i64\")]", &d.unwrap()));
                            } else {
                                ty.push(format!("  #[serde(default)]"));
                            }

                            ty.push(format!("  {}: f64,", m.name));
                        },
                        Data::String  => {
                            ty.push(format!("  {}: Option<String>,", m.name));
                        },
                        x => panic!("Invalid data kind {:?} not a simple type.", x),
                    }
                },
                Count::Many => {
                    match m.data {
                        Data::Alt(xs) => {
                            ty.push(format!("  #[serde(rename=\"$value\")]"));
                            ty.push(format!("  body: Vec<{}Body>,", t.name));
                            lines.push(format!("#[derive(Debug, Deserialize)]"));
                            lines.push(format!("enum {}Body {{", t.name));
                            for x in xs {
                                lines.push(format!("  {}({}),", x, x));
                            }
                            lines.push(format!("}}"));
                            lines.push(format!(""));
                        }
                        d => panic!("Invalid data kind {:?} in generated type {}.", d, t.name),
                    }
                },
            }
        }
        ty.push(String::from("}"));
        ty.push(String::from(""));
        lines.append(&mut ty);
    }
    println!("{}", lines.join("\n"));
}
