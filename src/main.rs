use std::env::args;
use serde::Deserialize;
use quick_xml::{
    Reader, Writer,
    events::{Event, BytesEnd, BytesStart, attributes::Attribute},
    de::from_reader};
use std::collections::HashSet as Set;
use std::io::Cursor;
use std::borrow::Cow;

fn one_f64() -> f64 {
    1.0
}

#[derive(Debug, Default, Deserialize)]
struct Dimension {
    name: String,                // The name to be used when referring to this dimension from variable declaration or units
    #[serde(default)]
    m: i32,                      // Mass
    #[serde(default)]
    l: i32,                      // Length
    #[serde(default)]
    t: i32,                      // Time
    #[serde(default)]
    i: i32,                      // Current
    #[serde(default)]
    k: i32,                      // Temperature
    #[serde(default)]
    n: i32,                      // Amount of substance
    #[serde(default)]
    j: i32,                      // Luminous intensity
}

#[derive(Debug, Default, Deserialize)]
struct Unit {
    symbol:    String,           // used to refer to this unit inside compound expressions coutaining a number and a unit symbol. Such expressions can only occur on the right hand side of assignments statements.
    dimension: String,           // Reference to the dimension for this unit
    #[serde(default)]
    power:     i32,              // Power of ten
    #[serde(default="one_f64")]
    scale:     f64,              // Scale, only to be used for scales which are not powers of ten
    #[serde(default)]
    offset:    f64,              // Offset for non zero-offset units
}

#[derive(Debug, Default, Deserialize)]
struct Constant {
    name:        String,         // A readable name for the constant.
    value:       String,         // Physical quantity incl. units
    dimension:   Option<String>, // [Opt] Reference to used dimension
    description: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
struct Assertion {
}

#[derive(Debug, Default, Deserialize)]
struct StateVariable {
    name:      String,
    dimension: String,
    exposure:  String,
}

#[derive(Debug, Default, Deserialize)]
struct DerivedVariable {
    name:        String,
    dimension:   Option<String>,
    exposure:    Option<String>,
    value:       Option<String>,
    select:      Option<String>,
    description: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
struct Case {
    condition: Option<String>,
    value:     String,
}

#[derive(Debug, Default, Deserialize)]
struct OnStart {
}

#[derive(Debug, Default, Deserialize)]
struct OnEntry {
}

#[derive(Debug, Default, Deserialize)]
struct OnEvent {
    port: String,
}

#[derive(Debug, Default, Deserialize)]
struct ConditionalDerivedVariable {
    name:        String,
    dimension:   Option<String>,
    exposure:    Option<String>,
    cases:       Vec<Case>,
}


#[derive(Debug, Default, Deserialize)]
struct TimeDerivative {
    variable: String,
    value: String,
}

#[derive(Debug, Deserialize)]
enum DynamicsBody {
    StateVariable,
    DerivedVariable,
    ConditionalDerivedVariable,
    TimeDerivative,
}

#[derive(Debug, Default, Deserialize)]
struct Dynamics {
    #[serde(rename="$value")]
    body: Vec<DynamicsBody>,
}

#[derive(Debug, Default, Deserialize)]
struct Exposure {
    name:        String,
    dimension:   String,
    description: String,
}

#[derive(Debug, Default, Deserialize)]
struct Requirement {
    name:        String,
    dimension:   String,
    description: String,
    exposure:    String,
}

#[derive(Debug, Default, Deserialize)]
struct ComponentType {
    name:         String,         // The name of the component type, can be used as XML element when defining components.
    extends:      Option<String>, // The component type that this type inherits field definitions for, if any
    description:  Option<String>,
    #[serde(default)]
    parameters:   Vec<Parameter>,
    #[serde(default)]
    dynamics:     Vec<Dynamics>,
    #[serde(default)]
    exposures:    Vec<Exposure>,
    #[serde(default)]
    requirements: Vec<Requirement>,
}


#[derive(Debug, Default, Deserialize)]
struct Parameter {
    name:      String,
    dimension: String,
}

#[derive(Debug, Default, Deserialize)]
struct DerivedParameter {
    name:        String,
    dimension:   Option<String>,
    value:       String,
}


#[derive(Debug, Deserialize)]
enum ComponentBody {
    Component(Component)
}

#[derive(Debug, Default, Deserialize)]
struct Component {
    name:    String,
    extends: Option<String>,
    #[serde(rename="$value", default)]
    body:    Vec<ComponentBody>,
}

#[derive(Debug, Default, Deserialize)]
struct Target {
    component: String,
    #[serde(rename="reportFile")]
    report:     Option<String>,
    #[serde(rename="timesFile")]
    times:     Option<String>,

}

#[derive(Debug, Default, Deserialize)]
struct Include {
    name: String,
}


#[derive(Debug, Deserialize)]
enum LemsBody {
    Include(Include),
    Dimension(Dimension),
    Constant(Constant),
    Unit(Unit),
    Assertion(Assertion),
    ComponentType(ComponentType),
    Component(Component),
    Target(Target),
}

#[derive(Debug, Deserialize)]
struct Lems {
    description: Option<String>,
    #[serde(rename="$value")]
    body:        Vec<LemsBody>,
}

fn main() {
    let mut known_tags = Set::new();
    for tag in ["Unit",
                "Dimension",
                "Lems",
                "ComponentType",
                "Component",
                "Parameter",
                "StateVariable",
                "TimeDerivative",
    ] {
        known_tags.insert(tag.as_bytes());
    }
    let file = args().nth(1).ok_or("Usage: nml2 <nml-or-lems-file.xml>").unwrap();
    let xml = std::fs::read_to_string(file).unwrap();
    let mut reader = Reader::from_str(&xml);
    reader.trim_text(true)
          .expand_empty_elements(true);

    let mut buf = Vec::new();
    let mut writer = Writer::new_with_indent(Cursor::new(Vec::new()), b' ', 4);
    loop {
        match reader.read_event(&mut buf) {
            Ok(Event::Eof) => break,
            Ok(Event::Start(ref e)) if !known_tags.contains(e.name()) => {
                let mut el = BytesStart::owned(b"Component".to_vec(), "Component".len());
                el.push_attribute(Attribute {key: b"name", value: Cow::Owned(e.name().to_owned())});
                assert!(writer.write_event(Event::Start(el)).is_ok());
            },
            Ok(Event::End(ref e)) if !known_tags.contains(e.name()) => {
                let el = BytesEnd::borrowed(b"Component");
                assert!(writer.write_event(Event::End(el)).is_ok());
            },
            Ok(e) => { assert!(writer.write_event(&e).is_ok()); },
            Err(e) => panic!("Error in input file {:}", e),
        }
        assert!(writer.write_indent().is_ok());
    }
    let mut xml = writer.into_inner();
    xml.set_position(0);
    let result: Lems = from_reader(xml).unwrap();
    println!("Result: {:?}", result);
}
