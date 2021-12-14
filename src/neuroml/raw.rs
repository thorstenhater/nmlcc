#![allow(non_camel_case_types, non_snake_case, unused_variables)]
#![allow(clippy::many_single_char_names, clippy::large_enum_variant)]
use roxmltree::Node;

use crate::xml::XML;

#[derive(Debug, Clone, PartialEq)]
pub enum GateHHUndeterminedBody {
  notes(String),
  q10Settings(Q10Settings),
  forwardRate(HHRate),
  reverseRate(HHRate),
  timeCourse(HHTime),
  steadyState(HHVariable),
  subGate(GateFractionalSubgate),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateHHUndetermined {
  pub instances: i64,
  pub r#type: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateHHUndeterminedBody>
}

impl XML for GateHHUndetermined {
  fn from_node(node: &Node) -> Self {
    let instances = node.attribute("instances").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let r#type = node.attribute("type").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateHHUndeterminedBody::notes(String::from_node(&child))),
        "q10Settings" => body.push(GateHHUndeterminedBody::q10Settings(Q10Settings::from_node(&child))),
        "forwardRate" => body.push(GateHHUndeterminedBody::forwardRate(HHRate::from_node(&child))),
        "reverseRate" => body.push(GateHHUndeterminedBody::reverseRate(HHRate::from_node(&child))),
        "timeCourse" => body.push(GateHHUndeterminedBody::timeCourse(HHTime::from_node(&child))),
        "steadyState" => body.push(GateHHUndeterminedBody::steadyState(HHVariable::from_node(&child))),
        "subGate" => body.push(GateHHUndeterminedBody::subGate(GateFractionalSubgate::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateHHUndetermined.", t)
      };
    }
    GateHHUndetermined { instances, r#type, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpaceStructure {
  pub xSpacing: Option<f64>,
  pub ySpacing: Option<f64>,
  pub zSpacing: Option<f64>,
  pub xStart: f64,
  pub yStart: f64,
  pub zStart: f64,
}

impl XML for SpaceStructure {
  fn from_node(node: &Node) -> Self {
    let xSpacing = node.attribute("xSpacing").map(|s| s.parse::<f64>().unwrap());
    let ySpacing = node.attribute("ySpacing").map(|s| s.parse::<f64>().unwrap());
    let zSpacing = node.attribute("zSpacing").map(|s| s.parse::<f64>().unwrap());
    let xStart = node.attribute("xStart").or(Some("0")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let yStart = node.attribute("yStart").or(Some("0")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let zStart = node.attribute("zStart").or(Some("0")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    SpaceStructure { xSpacing, ySpacing, zSpacing, xStart, yStart, zStart }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntracellularProperties2CaPoolsBody {
  species(Species),
  resistivity(Resistivity),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntracellularProperties2CaPools {
  pub body: Vec<IntracellularProperties2CaPoolsBody>
}

impl XML for IntracellularProperties2CaPools {
  fn from_node(node: &Node) -> Self {
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "species" => body.push(IntracellularProperties2CaPoolsBody::species(Species::from_node(&child))),
        "resistivity" => body.push(IntracellularProperties2CaPoolsBody::resistivity(Resistivity::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IntracellularProperties2CaPools.", t)
      };
    }
    IntracellularProperties2CaPools { body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SpikeArrayBody {
  spike(Spike),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpikeArray {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SpikeArrayBody>
}

impl XML for SpikeArray {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "spike" => body.push(SpikeArrayBody::spike(Spike::from_node(&child))),
        "notes" => body.push(SpikeArrayBody::notes(String::from_node(&child))),
        "property" => body.push(SpikeArrayBody::property(Property::from_node(&child))),
        "annotation" => body.push(SpikeArrayBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SpikeArray.", t)
      };
    }
    SpikeArray { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AdExIaFCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AdExIaFCell {
  pub gL: String,
  pub EL: String,
  pub reset: String,
  pub VT: String,
  pub thresh: String,
  pub delT: String,
  pub tauw: String,
  pub refract: String,
  pub a: String,
  pub b: String,
  pub C: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<AdExIaFCellBody>
}

impl XML for AdExIaFCell {
  fn from_node(node: &Node) -> Self {
    let gL = node.attribute("gL").map(|s| s.to_string()).unwrap();
    let EL = node.attribute("EL").map(|s| s.to_string()).unwrap();
    let reset = node.attribute("reset").map(|s| s.to_string()).unwrap();
    let VT = node.attribute("VT").map(|s| s.to_string()).unwrap();
    let thresh = node.attribute("thresh").map(|s| s.to_string()).unwrap();
    let delT = node.attribute("delT").map(|s| s.to_string()).unwrap();
    let tauw = node.attribute("tauw").map(|s| s.to_string()).unwrap();
    let refract = node.attribute("refract").map(|s| s.to_string()).unwrap();
    let a = node.attribute("a").map(|s| s.to_string()).unwrap();
    let b = node.attribute("b").map(|s| s.to_string()).unwrap();
    let C = node.attribute("C").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(AdExIaFCellBody::notes(String::from_node(&child))),
        "property" => body.push(AdExIaFCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(AdExIaFCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of AdExIaFCell.", t)
      };
    }
    AdExIaFCell { gL, EL, reset, VT, thresh, delT, tauw, refract, a, b, C, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExplicitInput {
  pub target: String,
  pub input: String,
  pub destination: Option<String>,
}

impl XML for ExplicitInput {
  fn from_node(node: &Node) -> Self {
    let target = node.attribute("target").map(|s| s.to_string()).unwrap();
    let input = node.attribute("input").map(|s| s.to_string()).unwrap();
    let destination = node.attribute("destination").map(|s| s.to_string());
    ExplicitInput { target, input, destination }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ComponentTypeBody {
  Property(LEMS_Property),
  Parameter(Parameter),
  Constant(Constant),
  Exposure(Exposure),
  Requirement(Requirement),
  InstanceRequirement(InstanceRequirement),
  Dynamics(Dynamics),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ComponentType {
  pub name: String,
  pub extends: Option<String>,
  pub description: Option<String>,
  pub body: Vec<ComponentTypeBody>
}

impl XML for ComponentType {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let extends = node.attribute("extends").map(|s| s.to_string());
    let description = node.attribute("description").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "Property" => body.push(ComponentTypeBody::Property(LEMS_Property::from_node(&child))),
        "Parameter" => body.push(ComponentTypeBody::Parameter(Parameter::from_node(&child))),
        "Constant" => body.push(ComponentTypeBody::Constant(Constant::from_node(&child))),
        "Exposure" => body.push(ComponentTypeBody::Exposure(Exposure::from_node(&child))),
        "Requirement" => body.push(ComponentTypeBody::Requirement(Requirement::from_node(&child))),
        "InstanceRequirement" => body.push(ComponentTypeBody::InstanceRequirement(InstanceRequirement::from_node(&child))),
        "Dynamics" => body.push(ComponentTypeBody::Dynamics(Dynamics::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ComponentType.", t)
      };
    }
    ComponentType { name, extends, description, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SpikeGeneratorPoissonBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpikeGeneratorPoisson {
  pub averageRate: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SpikeGeneratorPoissonBody>
}

impl XML for SpikeGeneratorPoisson {
  fn from_node(node: &Node) -> Self {
    let averageRate = node.attribute("averageRate").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(SpikeGeneratorPoissonBody::notes(String::from_node(&child))),
        "property" => body.push(SpikeGeneratorPoissonBody::property(Property::from_node(&child))),
        "annotation" => body.push(SpikeGeneratorPoissonBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SpikeGeneratorPoisson.", t)
      };
    }
    SpikeGeneratorPoisson { averageRate, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChannelDensityNonUniformBody {
  variableParameter(VariableParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelDensityNonUniform {
  pub ionChannel: String,
  pub erev: String,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ChannelDensityNonUniformBody>
}

impl XML for ChannelDensityNonUniform {
  fn from_node(node: &Node) -> Self {
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "variableParameter" => body.push(ChannelDensityNonUniformBody::variableParameter(VariableParameter::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ChannelDensityNonUniform.", t)
      };
    }
    ChannelDensityNonUniform { ionChannel, erev, ion, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IF_curr_expBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IF_curr_exp {
  pub tau_m: f64,
  pub tau_refrac: f64,
  pub v_reset: f64,
  pub v_rest: f64,
  pub v_thresh: f64,
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IF_curr_expBody>
}

impl XML for IF_curr_exp {
  fn from_node(node: &Node) -> Self {
    let tau_m = node.attribute("tau_m").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_refrac = node.attribute("tau_refrac").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_reset = node.attribute("v_reset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_rest = node.attribute("v_rest").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_thresh = node.attribute("v_thresh").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(IF_curr_expBody::notes(String::from_node(&child))),
        "property" => body.push(IF_curr_expBody::property(Property::from_node(&child))),
        "annotation" => body.push(IF_curr_expBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IF_curr_exp.", t)
      };
    }
    IF_curr_exp { tau_m, tau_refrac, v_reset, v_rest, v_thresh, cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InhomogeneousValue {
  pub inhomogeneousParameter: String,
  pub value: String,
}

impl XML for InhomogeneousValue {
  fn from_node(node: &Node) -> Self {
    let inhomogeneousParameter = node.attribute("inhomogeneousParameter").map(|s| s.to_string()).unwrap();
    let value = node.attribute("value").map(|s| s.to_string()).unwrap();
    InhomogeneousValue { inhomogeneousParameter, value }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateKSBody {
  notes(String),
  q10Settings(Q10Settings),
  closedState(ClosedState),
  openState(OpenState),
  forwardTransition(ForwardTransition),
  reverseTransition(ReverseTransition),
  tauInfTransition(TauInfTransition),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateKS {
  pub instances: i64,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateKSBody>
}

impl XML for GateKS {
  fn from_node(node: &Node) -> Self {
    let instances = node.attribute("instances").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateKSBody::notes(String::from_node(&child))),
        "q10Settings" => body.push(GateKSBody::q10Settings(Q10Settings::from_node(&child))),
        "closedState" => body.push(GateKSBody::closedState(ClosedState::from_node(&child))),
        "openState" => body.push(GateKSBody::openState(OpenState::from_node(&child))),
        "forwardTransition" => body.push(GateKSBody::forwardTransition(ForwardTransition::from_node(&child))),
        "reverseTransition" => body.push(GateKSBody::reverseTransition(ReverseTransition::from_node(&child))),
        "tauInfTransition" => body.push(GateKSBody::tauInfTransition(TauInfTransition::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateKS.", t)
      };
    }
    GateKS { instances, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AlphaCurrSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AlphaCurrSynapse {
  pub tau_syn: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<AlphaCurrSynapseBody>
}

impl XML for AlphaCurrSynapse {
  fn from_node(node: &Node) -> Self {
    let tau_syn = node.attribute("tau_syn").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(AlphaCurrSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(AlphaCurrSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(AlphaCurrSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of AlphaCurrSynapse.", t)
      };
    }
    AlphaCurrSynapse { tau_syn, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Include {
  pub segmentGroup: String,
}

impl XML for Include {
  fn from_node(node: &Node) -> Self {
    let segmentGroup = node.attribute("segmentGroup").map(|s| s.to_string()).unwrap();
    Include { segmentGroup }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IonChannelScalableBody {
  q10ConductanceScaling(Q10ConductanceScaling),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IonChannelScalable {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IonChannelScalableBody>
}

impl XML for IonChannelScalable {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "q10ConductanceScaling" => body.push(IonChannelScalableBody::q10ConductanceScaling(Q10ConductanceScaling::from_node(&child))),
        "notes" => body.push(IonChannelScalableBody::notes(String::from_node(&child))),
        "property" => body.push(IonChannelScalableBody::property(Property::from_node(&child))),
        "annotation" => body.push(IonChannelScalableBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IonChannelScalable.", t)
      };
    }
    IonChannelScalable { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinuousConnectionInstance {
  pub preComponent: String,
  pub postComponent: String,
  pub preCell: String,
  pub preSegment: i64,
  pub preFractionAlong: f64,
  pub postCell: String,
  pub postSegment: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for ContinuousConnectionInstance {
  fn from_node(node: &Node) -> Self {
    let preComponent = node.attribute("preComponent").map(|s| s.to_string()).unwrap();
    let postComponent = node.attribute("postComponent").map(|s| s.to_string()).unwrap();
    let preCell = node.attribute("preCell").map(|s| s.to_string()).unwrap();
    let preSegment = node.attribute("preSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCell = node.attribute("postCell").map(|s| s.to_string()).unwrap();
    let postSegment = node.attribute("postSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ContinuousConnectionInstance { preComponent, postComponent, preCell, preSegment, preFractionAlong, postCell, postSegment, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinuousConnection {
  pub preComponent: String,
  pub postComponent: String,
  pub preCell: String,
  pub preSegment: i64,
  pub preFractionAlong: f64,
  pub postCell: String,
  pub postSegment: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for ContinuousConnection {
  fn from_node(node: &Node) -> Self {
    let preComponent = node.attribute("preComponent").map(|s| s.to_string()).unwrap();
    let postComponent = node.attribute("postComponent").map(|s| s.to_string()).unwrap();
    let preCell = node.attribute("preCell").map(|s| s.to_string()).unwrap();
    let preSegment = node.attribute("preSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCell = node.attribute("postCell").map(|s| s.to_string()).unwrap();
    let postSegment = node.attribute("postSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ContinuousConnection { preComponent, postComponent, preCell, preSegment, preFractionAlong, postCell, postSegment, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChannelDensityNernstCa2Body {
  variableParameter(VariableParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelDensityNernstCa2 {
  pub ionChannel: String,
  pub condDensity: Option<String>,
  pub segmentGroup: String,
  pub segment: Option<String>,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ChannelDensityNernstCa2Body>
}

impl XML for ChannelDensityNernstCa2 {
  fn from_node(node: &Node) -> Self {
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let condDensity = node.attribute("condDensity").map(|s| s.to_string());
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    let segment = node.attribute("segment").map(|s| s.to_string());
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "variableParameter" => body.push(ChannelDensityNernstCa2Body::variableParameter(VariableParameter::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ChannelDensityNernstCa2.", t)
      };
    }
    ChannelDensityNernstCa2 { ionChannel, condDensity, segmentGroup, segment, ion, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseConductanceBasedSynapseTwoBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseConductanceBasedSynapseTwo {
  pub gbase1: String,
  pub gbase2: String,
  pub erev: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BaseConductanceBasedSynapseTwoBody>
}

impl XML for BaseConductanceBasedSynapseTwo {
  fn from_node(node: &Node) -> Self {
    let gbase1 = node.attribute("gbase1").map(|s| s.to_string()).unwrap();
    let gbase2 = node.attribute("gbase2").map(|s| s.to_string()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(BaseConductanceBasedSynapseTwoBody::notes(String::from_node(&child))),
        "property" => body.push(BaseConductanceBasedSynapseTwoBody::property(Property::from_node(&child))),
        "annotation" => body.push(BaseConductanceBasedSynapseTwoBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BaseConductanceBasedSynapseTwo.", t)
      };
    }
    BaseConductanceBasedSynapseTwo { gbase1, gbase2, erev, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HH_cond_expBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HH_cond_exp {
  pub v_offset: f64,
  pub e_rev_E: f64,
  pub e_rev_I: f64,
  pub e_rev_K: f64,
  pub e_rev_Na: f64,
  pub e_rev_leak: f64,
  pub g_leak: f64,
  pub gbar_K: f64,
  pub gbar_Na: f64,
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<HH_cond_expBody>
}

impl XML for HH_cond_exp {
  fn from_node(node: &Node) -> Self {
    let v_offset = node.attribute("v_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_E = node.attribute("e_rev_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_I = node.attribute("e_rev_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_K = node.attribute("e_rev_K").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_Na = node.attribute("e_rev_Na").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_leak = node.attribute("e_rev_leak").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let g_leak = node.attribute("g_leak").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let gbar_K = node.attribute("gbar_K").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let gbar_Na = node.attribute("gbar_Na").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(HH_cond_expBody::notes(String::from_node(&child))),
        "property" => body.push(HH_cond_expBody::property(Property::from_node(&child))),
        "annotation" => body.push(HH_cond_expBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of HH_cond_exp.", t)
      };
    }
    HH_cond_exp { v_offset, e_rev_E, e_rev_I, e_rev_K, e_rev_Na, e_rev_leak, g_leak, gbar_K, gbar_Na, cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseSynapse {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BaseSynapseBody>
}

impl XML for BaseSynapse {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(BaseSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(BaseSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(BaseSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BaseSynapse.", t)
      };
    }
    BaseSynapse { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GapJunctionBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GapJunction {
  pub conductance: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GapJunctionBody>
}

impl XML for GapJunction {
  fn from_node(node: &Node) -> Self {
    let conductance = node.attribute("conductance").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GapJunctionBody::notes(String::from_node(&child))),
        "property" => body.push(GapJunctionBody::property(Property::from_node(&child))),
        "annotation" => body.push(GapJunctionBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GapJunction.", t)
      };
    }
    GapJunction { conductance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SegmentParent {
  pub segment: i64,
  pub fractionAlong: f64,
}

impl XML for SegmentParent {
  fn from_node(node: &Node) -> Self {
    let segment = node.attribute("segment").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let fractionAlong = node.attribute("fractionAlong").or(Some("1")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    SegmentParent { segment, fractionAlong }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElectricalConnectionInstanceW {
  pub weight: f64,
  pub synapse: String,
  pub preCell: String,
  pub preSegment: i64,
  pub preFractionAlong: f64,
  pub postCell: String,
  pub postSegment: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for ElectricalConnectionInstanceW {
  fn from_node(node: &Node) -> Self {
    let weight = node.attribute("weight").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let synapse = node.attribute("synapse").map(|s| s.to_string()).unwrap();
    let preCell = node.attribute("preCell").map(|s| s.to_string()).unwrap();
    let preSegment = node.attribute("preSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCell = node.attribute("postCell").map(|s| s.to_string()).unwrap();
    let postSegment = node.attribute("postSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ElectricalConnectionInstanceW { weight, synapse, preCell, preSegment, preFractionAlong, postCell, postSegment, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Region {
  pub space: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for Region {
  fn from_node(node: &Node) -> Self {
    let space = node.attribute("space").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    Region { space, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChannelDensityNonUniformNernstBody {
  variableParameter(VariableParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelDensityNonUniformNernst {
  pub ionChannel: String,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ChannelDensityNonUniformNernstBody>
}

impl XML for ChannelDensityNonUniformNernst {
  fn from_node(node: &Node) -> Self {
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "variableParameter" => body.push(ChannelDensityNonUniformNernstBody::variableParameter(VariableParameter::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ChannelDensityNonUniformNernst.", t)
      };
    }
    ChannelDensityNonUniformNernst { ionChannel, ion, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateHHRatesBody {
  notes(String),
  q10Settings(Q10Settings),
  forwardRate(HHRate),
  reverseRate(HHRate),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateHHRates {
  pub instances: i64,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateHHRatesBody>
}

impl XML for GateHHRates {
  fn from_node(node: &Node) -> Self {
    let instances = node.attribute("instances").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateHHRatesBody::notes(String::from_node(&child))),
        "q10Settings" => body.push(GateHHRatesBody::q10Settings(Q10Settings::from_node(&child))),
        "forwardRate" => body.push(GateHHRatesBody::forwardRate(HHRate::from_node(&child))),
        "reverseRate" => body.push(GateHHRatesBody::reverseRate(HHRate::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateHHRates.", t)
      };
    }
    GateHHRates { instances, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelDensityGHK2 {
  pub ionChannel: String,
  pub condDensity: Option<String>,
  pub segmentGroup: String,
  pub segment: Option<String>,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for ChannelDensityGHK2 {
  fn from_node(node: &Node) -> Self {
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let condDensity = node.attribute("condDensity").map(|s| s.to_string());
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    let segment = node.attribute("segment").map(|s| s.to_string());
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ChannelDensityGHK2 { ionChannel, condDensity, segmentGroup, segment, ion, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Connection {
  pub preCellId: String,
  pub preSegmentId: i64,
  pub preFractionAlong: f64,
  pub postCellId: String,
  pub postSegmentId: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for Connection {
  fn from_node(node: &Node) -> Self {
    let preCellId = node.attribute("preCellId").map(|s| s.to_string()).unwrap();
    let preSegmentId = node.attribute("preSegmentId").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCellId = node.attribute("postCellId").map(|s| s.to_string()).unwrap();
    let postSegmentId = node.attribute("postSegmentId").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    Connection { preCellId, preSegmentId, preFractionAlong, postCellId, postSegmentId, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InstanceRequirement {
  pub name: String,
  pub r#type: String,
}

impl XML for InstanceRequirement {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let r#type = node.attribute("type").map(|s| s.to_string()).unwrap();
    InstanceRequirement { name, r#type }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InputW {
  pub weight: f64,
  pub id: i64,
  pub target: String,
  pub destination: String,
  pub segmentId: Option<i64>,
  pub fractionAlong: Option<f64>,
}

impl XML for InputW {
  fn from_node(node: &Node) -> Self {
    let weight = node.attribute("weight").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let target = node.attribute("target").map(|s| s.to_string()).unwrap();
    let destination = node.attribute("destination").map(|s| s.to_string()).unwrap();
    let segmentId = node.attribute("segmentId").map(|s| s.parse::<i64>().unwrap());
    let fractionAlong = node.attribute("fractionAlong").map(|s| s.parse::<f64>().unwrap());
    InputW { weight, id, target, destination, segmentId, fractionAlong }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseCurrentBasedSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseCurrentBasedSynapse {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BaseCurrentBasedSynapseBody>
}

impl XML for BaseCurrentBasedSynapse {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(BaseCurrentBasedSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(BaseCurrentBasedSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(BaseCurrentBasedSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BaseCurrentBasedSynapse.", t)
      };
    }
    BaseCurrentBasedSynapse { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LEMS_Property {
  pub defaultValue: Option<f64>,
  pub name: String,
  pub dimension: String,
  pub description: Option<String>,
}

impl XML for LEMS_Property {
  fn from_node(node: &Node) -> Self {
    let defaultValue = node.attribute("defaultValue").map(|s| s.parse::<f64>().unwrap());
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    LEMS_Property { defaultValue, name, dimension, description }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constant {
  pub name: String,
  pub dimension: String,
  pub value: String,
  pub description: Option<String>,
}

impl XML for Constant {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let value = node.attribute("value").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    Constant { name, dimension, value, description }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FitzHughNagumo1969CellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FitzHughNagumo1969Cell {
  pub a: String,
  pub b: String,
  pub I: String,
  pub phi: String,
  pub V0: String,
  pub W0: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<FitzHughNagumo1969CellBody>
}

impl XML for FitzHughNagumo1969Cell {
  fn from_node(node: &Node) -> Self {
    let a = node.attribute("a").map(|s| s.to_string()).unwrap();
    let b = node.attribute("b").map(|s| s.to_string()).unwrap();
    let I = node.attribute("I").map(|s| s.to_string()).unwrap();
    let phi = node.attribute("phi").map(|s| s.to_string()).unwrap();
    let V0 = node.attribute("V0").map(|s| s.to_string()).unwrap();
    let W0 = node.attribute("W0").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(FitzHughNagumo1969CellBody::notes(String::from_node(&child))),
        "property" => body.push(FitzHughNagumo1969CellBody::property(Property::from_node(&child))),
        "annotation" => body.push(FitzHughNagumo1969CellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of FitzHughNagumo1969Cell.", t)
      };
    }
    FitzHughNagumo1969Cell { a, b, I, phi, V0, W0, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DerivedVariable {
  pub value: Option<String>,
  pub select: Option<String>,
  pub name: String,
  pub dimension: String,
  pub description: Option<String>,
  pub exposure: Option<String>,
}

impl XML for DerivedVariable {
  fn from_node(node: &Node) -> Self {
    let value = node.attribute("value").map(|s| s.to_string());
    let select = node.attribute("select").map(|s| s.to_string());
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    let exposure = node.attribute("exposure").map(|s| s.to_string());
    DerivedVariable { value, select, name, dimension, description, exposure }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HHRate {
  pub r#type: String,
  pub rate: Option<String>,
  pub midpoint: Option<String>,
  pub scale: Option<String>,
}

impl XML for HHRate {
  fn from_node(node: &Node) -> Self {
    let r#type = node.attribute("type").map(|s| s.to_string()).unwrap();
    let rate = node.attribute("rate").map(|s| s.to_string());
    let midpoint = node.attribute("midpoint").map(|s| s.to_string());
    let scale = node.attribute("scale").map(|s| s.to_string());
    HHRate { r#type, rate, midpoint, scale }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ElectricalProjectionBody {
  electricalConnection(ElectricalConnection),
  electricalConnectionInstance(ElectricalConnectionInstance),
  electricalConnectionInstanceW(ElectricalConnectionInstanceW),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElectricalProjection {
  pub presynapticPopulation: String,
  pub postsynapticPopulation: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ElectricalProjectionBody>
}

impl XML for ElectricalProjection {
  fn from_node(node: &Node) -> Self {
    let presynapticPopulation = node.attribute("presynapticPopulation").map(|s| s.to_string()).unwrap();
    let postsynapticPopulation = node.attribute("postsynapticPopulation").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "electricalConnection" => body.push(ElectricalProjectionBody::electricalConnection(ElectricalConnection::from_node(&child))),
        "electricalConnectionInstance" => body.push(ElectricalProjectionBody::electricalConnectionInstance(ElectricalConnectionInstance::from_node(&child))),
        "electricalConnectionInstanceW" => body.push(ElectricalProjectionBody::electricalConnectionInstanceW(ElectricalConnectionInstanceW::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ElectricalProjection.", t)
      };
    }
    ElectricalProjection { presynapticPopulation, postsynapticPopulation, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IF_curr_alphaBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IF_curr_alpha {
  pub tau_m: f64,
  pub tau_refrac: f64,
  pub v_reset: f64,
  pub v_rest: f64,
  pub v_thresh: f64,
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IF_curr_alphaBody>
}

impl XML for IF_curr_alpha {
  fn from_node(node: &Node) -> Self {
    let tau_m = node.attribute("tau_m").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_refrac = node.attribute("tau_refrac").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_reset = node.attribute("v_reset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_rest = node.attribute("v_rest").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_thresh = node.attribute("v_thresh").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(IF_curr_alphaBody::notes(String::from_node(&child))),
        "property" => body.push(IF_curr_alphaBody::property(Property::from_node(&child))),
        "annotation" => body.push(IF_curr_alphaBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IF_curr_alpha.", t)
      };
    }
    IF_curr_alpha { tau_m, tau_refrac, v_reset, v_rest, v_thresh, cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SpikeSourcePoissonBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpikeSourcePoisson {
  pub start: String,
  pub duration: String,
  pub rate: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SpikeSourcePoissonBody>
}

impl XML for SpikeSourcePoisson {
  fn from_node(node: &Node) -> Self {
    let start = node.attribute("start").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let rate = node.attribute("rate").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(SpikeSourcePoissonBody::notes(String::from_node(&child))),
        "property" => body.push(SpikeSourcePoissonBody::property(Property::from_node(&child))),
        "annotation" => body.push(SpikeSourcePoissonBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SpikeSourcePoisson.", t)
      };
    }
    SpikeSourcePoisson { start, duration, rate, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RampGeneratorBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RampGenerator {
  pub delay: String,
  pub duration: String,
  pub startAmplitude: String,
  pub finishAmplitude: String,
  pub baselineAmplitude: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<RampGeneratorBody>
}

impl XML for RampGenerator {
  fn from_node(node: &Node) -> Self {
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let startAmplitude = node.attribute("startAmplitude").map(|s| s.to_string()).unwrap();
    let finishAmplitude = node.attribute("finishAmplitude").map(|s| s.to_string()).unwrap();
    let baselineAmplitude = node.attribute("baselineAmplitude").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(RampGeneratorBody::notes(String::from_node(&child))),
        "property" => body.push(RampGeneratorBody::property(Property::from_node(&child))),
        "annotation" => body.push(RampGeneratorBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of RampGenerator.", t)
      };
    }
    RampGenerator { delay, duration, startAmplitude, finishAmplitude, baselineAmplitude, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DecayingPoolConcentrationModelBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecayingPoolConcentrationModel {
  pub ion: String,
  pub restingConc: String,
  pub decayConstant: String,
  pub shellThickness: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<DecayingPoolConcentrationModelBody>
}

impl XML for DecayingPoolConcentrationModel {
  fn from_node(node: &Node) -> Self {
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let restingConc = node.attribute("restingConc").map(|s| s.to_string()).unwrap();
    let decayConstant = node.attribute("decayConstant").map(|s| s.to_string()).unwrap();
    let shellThickness = node.attribute("shellThickness").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(DecayingPoolConcentrationModelBody::notes(String::from_node(&child))),
        "property" => body.push(DecayingPoolConcentrationModelBody::property(Property::from_node(&child))),
        "annotation" => body.push(DecayingPoolConcentrationModelBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of DecayingPoolConcentrationModel.", t)
      };
    }
    DecayingPoolConcentrationModel { ion, restingConc, decayConstant, shellThickness, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockMechanism {
  pub r#type: String,
  pub species: String,
  pub blockConcentration: String,
  pub scalingConc: String,
  pub scalingVolt: String,
}

impl XML for BlockMechanism {
  fn from_node(node: &Node) -> Self {
    let r#type = node.attribute("type").map(|s| s.to_string()).unwrap();
    let species = node.attribute("species").map(|s| s.to_string()).unwrap();
    let blockConcentration = node.attribute("blockConcentration").map(|s| s.to_string()).unwrap();
    let scalingConc = node.attribute("scalingConc").map(|s| s.to_string()).unwrap();
    let scalingVolt = node.attribute("scalingVolt").map(|s| s.to_string()).unwrap();
    BlockMechanism { r#type, species, blockConcentration, scalingConc, scalingVolt }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CellSet {
  pub select: String,
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for CellSet {
  fn from_node(node: &Node) -> Self {
    let select = node.attribute("select").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    CellSet { select, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MembranePropertiesBody {
  channelPopulation(ChannelPopulation),
  channelDensity(ChannelDensity),
  channelDensityVShift(ChannelDensityVShift),
  channelDensityNernst(ChannelDensityNernst),
  channelDensityGHK(ChannelDensityGHK),
  channelDensityGHK2(ChannelDensityGHK2),
  channelDensityNonUniform(ChannelDensityNonUniform),
  channelDensityNonUniformNernst(ChannelDensityNonUniformNernst),
  channelDensityNonUniformGHK(ChannelDensityNonUniformGHK),
  spikeThresh(SpikeThresh),
  specificCapacitance(SpecificCapacitance),
  initMembPotential(InitMembPotential),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MembraneProperties {
  pub body: Vec<MembranePropertiesBody>
}

impl XML for MembraneProperties {
  fn from_node(node: &Node) -> Self {
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "channelPopulation" => body.push(MembranePropertiesBody::channelPopulation(ChannelPopulation::from_node(&child))),
        "channelDensity" => body.push(MembranePropertiesBody::channelDensity(ChannelDensity::from_node(&child))),
        "channelDensityVShift" => body.push(MembranePropertiesBody::channelDensityVShift(ChannelDensityVShift::from_node(&child))),
        "channelDensityNernst" => body.push(MembranePropertiesBody::channelDensityNernst(ChannelDensityNernst::from_node(&child))),
        "channelDensityGHK" => body.push(MembranePropertiesBody::channelDensityGHK(ChannelDensityGHK::from_node(&child))),
        "channelDensityGHK2" => body.push(MembranePropertiesBody::channelDensityGHK2(ChannelDensityGHK2::from_node(&child))),
        "channelDensityNonUniform" => body.push(MembranePropertiesBody::channelDensityNonUniform(ChannelDensityNonUniform::from_node(&child))),
        "channelDensityNonUniformNernst" => body.push(MembranePropertiesBody::channelDensityNonUniformNernst(ChannelDensityNonUniformNernst::from_node(&child))),
        "channelDensityNonUniformGHK" => body.push(MembranePropertiesBody::channelDensityNonUniformGHK(ChannelDensityNonUniformGHK::from_node(&child))),
        "spikeThresh" => body.push(MembranePropertiesBody::spikeThresh(SpikeThresh::from_node(&child))),
        "specificCapacitance" => body.push(MembranePropertiesBody::specificCapacitance(SpecificCapacitance::from_node(&child))),
        "initMembPotential" => body.push(MembranePropertiesBody::initMembPotential(InitMembPotential::from_node(&child))),
        t => panic!("Unexpected tag {} in body of MembraneProperties.", t)
      };
    }
    MembraneProperties { body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SpikeGeneratorBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpikeGenerator {
  pub period: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SpikeGeneratorBody>
}

impl XML for SpikeGenerator {
  fn from_node(node: &Node) -> Self {
    let period = node.attribute("period").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(SpikeGeneratorBody::notes(String::from_node(&child))),
        "property" => body.push(SpikeGeneratorBody::property(Property::from_node(&child))),
        "annotation" => body.push(SpikeGeneratorBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SpikeGenerator.", t)
      };
    }
    SpikeGenerator { period, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ContinuousProjectionBody {
  continuousConnection(ContinuousConnection),
  continuousConnectionInstance(ContinuousConnectionInstance),
  continuousConnectionInstanceW(ContinuousConnectionInstanceW),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinuousProjection {
  pub presynapticPopulation: String,
  pub postsynapticPopulation: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ContinuousProjectionBody>
}

impl XML for ContinuousProjection {
  fn from_node(node: &Node) -> Self {
    let presynapticPopulation = node.attribute("presynapticPopulation").map(|s| s.to_string()).unwrap();
    let postsynapticPopulation = node.attribute("postsynapticPopulation").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "continuousConnection" => body.push(ContinuousProjectionBody::continuousConnection(ContinuousConnection::from_node(&child))),
        "continuousConnectionInstance" => body.push(ContinuousProjectionBody::continuousConnectionInstance(ContinuousConnectionInstance::from_node(&child))),
        "continuousConnectionInstanceW" => body.push(ContinuousProjectionBody::continuousConnectionInstanceW(ContinuousConnectionInstanceW::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ContinuousProjection.", t)
      };
    }
    ContinuousProjection { presynapticPopulation, postsynapticPopulation, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChannelDensityVShiftBody {
  variableParameter(VariableParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelDensityVShift {
  pub vShift: String,
  pub ionChannel: String,
  pub condDensity: Option<String>,
  pub erev: String,
  pub segmentGroup: String,
  pub segment: Option<i64>,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ChannelDensityVShiftBody>
}

impl XML for ChannelDensityVShift {
  fn from_node(node: &Node) -> Self {
    let vShift = node.attribute("vShift").map(|s| s.to_string()).unwrap();
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let condDensity = node.attribute("condDensity").map(|s| s.to_string());
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    let segment = node.attribute("segment").map(|s| s.parse::<i64>().unwrap());
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "variableParameter" => body.push(ChannelDensityVShiftBody::variableParameter(VariableParameter::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ChannelDensityVShift.", t)
      };
    }
    ChannelDensityVShift { vShift, ionChannel, condDensity, erev, segmentGroup, segment, ion, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BiophysicalPropertiesBody {
  membraneProperties(MembraneProperties),
  intracellularProperties(IntracellularProperties),
  extracellularProperties(ExtracellularProperties),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BiophysicalProperties {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BiophysicalPropertiesBody>
}

impl XML for BiophysicalProperties {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "membraneProperties" => body.push(BiophysicalPropertiesBody::membraneProperties(MembraneProperties::from_node(&child))),
        "intracellularProperties" => body.push(BiophysicalPropertiesBody::intracellularProperties(IntracellularProperties::from_node(&child))),
        "extracellularProperties" => body.push(BiophysicalPropertiesBody::extracellularProperties(ExtracellularProperties::from_node(&child))),
        "notes" => body.push(BiophysicalPropertiesBody::notes(String::from_node(&child))),
        "property" => body.push(BiophysicalPropertiesBody::property(Property::from_node(&child))),
        "annotation" => body.push(BiophysicalPropertiesBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BiophysicalProperties.", t)
      };
    }
    BiophysicalProperties { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConnectionWD {
  pub weight: f64,
  pub delay: String,
  pub preCellId: String,
  pub preSegmentId: i64,
  pub preFractionAlong: f64,
  pub postCellId: String,
  pub postSegmentId: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for ConnectionWD {
  fn from_node(node: &Node) -> Self {
    let weight = node.attribute("weight").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let preCellId = node.attribute("preCellId").map(|s| s.to_string()).unwrap();
    let preSegmentId = node.attribute("preSegmentId").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCellId = node.attribute("postCellId").map(|s| s.to_string()).unwrap();
    let postSegmentId = node.attribute("postSegmentId").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ConnectionWD { weight, delay, preCellId, preSegmentId, preFractionAlong, postCellId, postSegmentId, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntracellularPropertiesBody {
  species(Species),
  resistivity(Resistivity),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntracellularProperties {
  pub body: Vec<IntracellularPropertiesBody>
}

impl XML for IntracellularProperties {
  fn from_node(node: &Node) -> Self {
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "species" => body.push(IntracellularPropertiesBody::species(Species::from_node(&child))),
        "resistivity" => body.push(IntracellularPropertiesBody::resistivity(Resistivity::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IntracellularProperties.", t)
      };
    }
    IntracellularProperties { body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseConductanceBasedSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseConductanceBasedSynapse {
  pub gbase: String,
  pub erev: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BaseConductanceBasedSynapseBody>
}

impl XML for BaseConductanceBasedSynapse {
  fn from_node(node: &Node) -> Self {
    let gbase = node.attribute("gbase").map(|s| s.to_string()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(BaseConductanceBasedSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(BaseConductanceBasedSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(BaseConductanceBasedSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BaseConductanceBasedSynapse.", t)
      };
    }
    BaseConductanceBasedSynapse { gbase, erev, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChannelPopulationBody {
  variableParameter(VariableParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelPopulation {
  pub ionChannel: String,
  pub number: i64,
  pub erev: String,
  pub segmentGroup: String,
  pub segment: Option<i64>,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ChannelPopulationBody>
}

impl XML for ChannelPopulation {
  fn from_node(node: &Node) -> Self {
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let number = node.attribute("number").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    let segment = node.attribute("segment").map(|s| s.parse::<i64>().unwrap());
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "variableParameter" => body.push(ChannelPopulationBody::variableParameter(VariableParameter::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ChannelPopulation.", t)
      };
    }
    ChannelPopulation { ionChannel, number, erev, segmentGroup, segment, ion, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SineGeneratorDLBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SineGeneratorDL {
  pub delay: String,
  pub phase: String,
  pub duration: String,
  pub amplitude: String,
  pub period: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SineGeneratorDLBody>
}

impl XML for SineGeneratorDL {
  fn from_node(node: &Node) -> Self {
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let phase = node.attribute("phase").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let amplitude = node.attribute("amplitude").map(|s| s.to_string()).unwrap();
    let period = node.attribute("period").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(SineGeneratorDLBody::notes(String::from_node(&child))),
        "property" => body.push(SineGeneratorDLBody::property(Property::from_node(&child))),
        "annotation" => body.push(SineGeneratorDLBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SineGeneratorDL.", t)
      };
    }
    SineGeneratorDL { delay, phase, duration, amplitude, period, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PopulationBody {
  layout(Layout),
  instance(Instance),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Population {
  pub component: String,
  pub size: Option<i64>,
  pub r#type: Option<String>,
  pub extracellularProperties: Option<String>,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<PopulationBody>
}

impl XML for Population {
  fn from_node(node: &Node) -> Self {
    let component = node.attribute("component").map(|s| s.to_string()).unwrap();
    let size = node.attribute("size").map(|s| s.parse::<i64>().unwrap());
    let r#type = node.attribute("type").map(|s| s.to_string());
    let extracellularProperties = node.attribute("extracellularProperties").map(|s| s.to_string());
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "layout" => body.push(PopulationBody::layout(Layout::from_node(&child))),
        "instance" => body.push(PopulationBody::instance(Instance::from_node(&child))),
        "notes" => body.push(PopulationBody::notes(String::from_node(&child))),
        "property" => body.push(PopulationBody::property(Property::from_node(&child))),
        "annotation" => body.push(PopulationBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Population.", t)
      };
    }
    Population { component, size, r#type, extracellularProperties, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Izhikevich2007CellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Izhikevich2007Cell {
  pub v0: String,
  pub k: String,
  pub vr: String,
  pub vt: String,
  pub vpeak: String,
  pub a: String,
  pub b: String,
  pub c: String,
  pub d: String,
  pub C: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<Izhikevich2007CellBody>
}

impl XML for Izhikevich2007Cell {
  fn from_node(node: &Node) -> Self {
    let v0 = node.attribute("v0").map(|s| s.to_string()).unwrap();
    let k = node.attribute("k").map(|s| s.to_string()).unwrap();
    let vr = node.attribute("vr").map(|s| s.to_string()).unwrap();
    let vt = node.attribute("vt").map(|s| s.to_string()).unwrap();
    let vpeak = node.attribute("vpeak").map(|s| s.to_string()).unwrap();
    let a = node.attribute("a").map(|s| s.to_string()).unwrap();
    let b = node.attribute("b").map(|s| s.to_string()).unwrap();
    let c = node.attribute("c").map(|s| s.to_string()).unwrap();
    let d = node.attribute("d").map(|s| s.to_string()).unwrap();
    let C = node.attribute("C").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(Izhikevich2007CellBody::notes(String::from_node(&child))),
        "property" => body.push(Izhikevich2007CellBody::property(Property::from_node(&child))),
        "annotation" => body.push(Izhikevich2007CellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Izhikevich2007Cell.", t)
      };
    }
    Izhikevich2007Cell { v0, k, vr, vt, vpeak, a, b, c, d, C, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InputListBody {
  input(Input),
  inputW(InputW),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InputList {
  pub population: String,
  pub component: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<InputListBody>
}

impl XML for InputList {
  fn from_node(node: &Node) -> Self {
    let population = node.attribute("population").map(|s| s.to_string()).unwrap();
    let component = node.attribute("component").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "input" => body.push(InputListBody::input(Input::from_node(&child))),
        "inputW" => body.push(InputListBody::inputW(InputW::from_node(&child))),
        t => panic!("Unexpected tag {} in body of InputList.", t)
      };
    }
    InputList { population, component, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableParameterBody {
  inhomogeneousValue(InhomogeneousValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableParameter {
  pub parameter: String,
  pub segmentGroup: String,
  pub body: Vec<VariableParameterBody>
}

impl XML for VariableParameter {
  fn from_node(node: &Node) -> Self {
    let parameter = node.attribute("parameter").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").map(|s| s.to_string()).unwrap();
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "inhomogeneousValue" => body.push(VariableParameterBody::inhomogeneousValue(InhomogeneousValue::from_node(&child))),
        t => panic!("Unexpected tag {} in body of VariableParameter.", t)
      };
    }
    VariableParameter { parameter, segmentGroup, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForwardTransition {
  pub from: String,
  pub to: String,
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for ForwardTransition {
  fn from_node(node: &Node) -> Self {
    let from = node.attribute("from").map(|s| s.to_string()).unwrap();
    let to = node.attribute("to").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ForwardTransition { from, to, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IonChannelVShiftBody {
  gate(GateHHUndetermined),
  gateHHrates(GateHHRates),
  gateHHratesTau(GateHHRatesTau),
  gateHHtauInf(GateHHTauInf),
  gateHHratesInf(GateHHRatesInf),
  gateHHratesTauInf(GateHHRatesTauInf),
  gateHHInstantaneous(GateHHInstantaneous),
  gateFractional(GateFractional),
  q10ConductanceScaling(Q10ConductanceScaling),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IonChannelVShift {
  pub vShift: String,
  pub species: Option<String>,
  pub r#type: Option<String>,
  pub conductance: Option<String>,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IonChannelVShiftBody>
}

impl XML for IonChannelVShift {
  fn from_node(node: &Node) -> Self {
    let vShift = node.attribute("vShift").map(|s| s.to_string()).unwrap();
    let species = node.attribute("species").map(|s| s.to_string());
    let r#type = node.attribute("type").map(|s| s.to_string());
    let conductance = node.attribute("conductance").map(|s| s.to_string());
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "gate" => body.push(IonChannelVShiftBody::gate(GateHHUndetermined::from_node(&child))),
        "gateHHrates" => body.push(IonChannelVShiftBody::gateHHrates(GateHHRates::from_node(&child))),
        "gateHHratesTau" => body.push(IonChannelVShiftBody::gateHHratesTau(GateHHRatesTau::from_node(&child))),
        "gateHHtauInf" => body.push(IonChannelVShiftBody::gateHHtauInf(GateHHTauInf::from_node(&child))),
        "gateHHratesInf" => body.push(IonChannelVShiftBody::gateHHratesInf(GateHHRatesInf::from_node(&child))),
        "gateHHratesTauInf" => body.push(IonChannelVShiftBody::gateHHratesTauInf(GateHHRatesTauInf::from_node(&child))),
        "gateHHInstantaneous" => body.push(IonChannelVShiftBody::gateHHInstantaneous(GateHHInstantaneous::from_node(&child))),
        "gateFractional" => body.push(IonChannelVShiftBody::gateFractional(GateFractional::from_node(&child))),
        "q10ConductanceScaling" => body.push(IonChannelVShiftBody::q10ConductanceScaling(Q10ConductanceScaling::from_node(&child))),
        "notes" => body.push(IonChannelVShiftBody::notes(String::from_node(&child))),
        "property" => body.push(IonChannelVShiftBody::property(Property::from_node(&child))),
        "annotation" => body.push(IonChannelVShiftBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IonChannelVShift.", t)
      };
    }
    IonChannelVShift { vShift, species, r#type, conductance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct GridLayout {
  pub xSize: Option<i64>,
  pub ySize: Option<i64>,
  pub zSize: Option<i64>,
}

impl XML for GridLayout {
  fn from_node(node: &Node) -> Self {
    let xSize = node.attribute("xSize").map(|s| s.parse::<i64>().unwrap());
    let ySize = node.attribute("ySize").map(|s| s.parse::<i64>().unwrap());
    let zSize = node.attribute("zSize").map(|s| s.parse::<i64>().unwrap());
    GridLayout { xSize, ySize, zSize }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateHHTauInfBody {
  notes(String),
  q10Settings(Q10Settings),
  timeCourse(HHTime),
  steadyState(HHVariable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateHHTauInf {
  pub instances: i64,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateHHTauInfBody>
}

impl XML for GateHHTauInf {
  fn from_node(node: &Node) -> Self {
    let instances = node.attribute("instances").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateHHTauInfBody::notes(String::from_node(&child))),
        "q10Settings" => body.push(GateHHTauInfBody::q10Settings(Q10Settings::from_node(&child))),
        "timeCourse" => body.push(GateHHTauInfBody::timeCourse(HHTime::from_node(&child))),
        "steadyState" => body.push(GateHHTauInfBody::steadyState(HHVariable::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateHHTauInf.", t)
      };
    }
    GateHHTauInf { instances, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LinearGradedSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct LinearGradedSynapse {
  pub conductance: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<LinearGradedSynapseBody>
}

impl XML for LinearGradedSynapse {
  fn from_node(node: &Node) -> Self {
    let conductance = node.attribute("conductance").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(LinearGradedSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(LinearGradedSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(LinearGradedSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of LinearGradedSynapse.", t)
      };
    }
    LinearGradedSynapse { conductance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SegmentBody {
  parent(SegmentParent),
  proximal(Point3DWithDiam),
  distal(Point3DWithDiam),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Segment {
  pub name: Option<String>,
  pub id: i64,
  pub neuroLexId: Option<String>,
  pub body: Vec<SegmentBody>
}

impl XML for Segment {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "parent" => body.push(SegmentBody::parent(SegmentParent::from_node(&child))),
        "proximal" => body.push(SegmentBody::proximal(Point3DWithDiam::from_node(&child))),
        "distal" => body.push(SegmentBody::distal(Point3DWithDiam::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Segment.", t)
      };
    }
    Segment { name, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedDimensionalType {
  pub name: String,
  pub dimension: String,
  pub description: Option<String>,
}

impl XML for NamedDimensionalType {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    NamedDimensionalType { name, dimension, description }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NetworkBody {
  space(Space),
  region(Region),
  extracellularProperties(ExtracellularPropertiesLocal),
  population(Population),
  cellSet(CellSet),
  synapticConnection(SynapticConnection),
  projection(Projection),
  electricalProjection(ElectricalProjection),
  continuousProjection(ContinuousProjection),
  explicitInput(ExplicitInput),
  inputList(InputList),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Network {
  pub r#type: Option<String>,
  pub temperature: Option<String>,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<NetworkBody>
}

impl XML for Network {
  fn from_node(node: &Node) -> Self {
    let r#type = node.attribute("type").map(|s| s.to_string());
    let temperature = node.attribute("temperature").map(|s| s.to_string());
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "space" => body.push(NetworkBody::space(Space::from_node(&child))),
        "region" => body.push(NetworkBody::region(Region::from_node(&child))),
        "extracellularProperties" => body.push(NetworkBody::extracellularProperties(ExtracellularPropertiesLocal::from_node(&child))),
        "population" => body.push(NetworkBody::population(Population::from_node(&child))),
        "cellSet" => body.push(NetworkBody::cellSet(CellSet::from_node(&child))),
        "synapticConnection" => body.push(NetworkBody::synapticConnection(SynapticConnection::from_node(&child))),
        "projection" => body.push(NetworkBody::projection(Projection::from_node(&child))),
        "electricalProjection" => body.push(NetworkBody::electricalProjection(ElectricalProjection::from_node(&child))),
        "continuousProjection" => body.push(NetworkBody::continuousProjection(ContinuousProjection::from_node(&child))),
        "explicitInput" => body.push(NetworkBody::explicitInput(ExplicitInput::from_node(&child))),
        "inputList" => body.push(NetworkBody::inputList(InputList::from_node(&child))),
        "notes" => body.push(NetworkBody::notes(String::from_node(&child))),
        "property" => body.push(NetworkBody::property(Property::from_node(&child))),
        "annotation" => body.push(NetworkBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Network.", t)
      };
    }
    Network { r#type, temperature, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PulseGeneratorDLBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PulseGeneratorDL {
  pub delay: String,
  pub duration: String,
  pub amplitude: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<PulseGeneratorDLBody>
}

impl XML for PulseGeneratorDL {
  fn from_node(node: &Node) -> Self {
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let amplitude = node.attribute("amplitude").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(PulseGeneratorDLBody::notes(String::from_node(&child))),
        "property" => body.push(PulseGeneratorDLBody::property(Property::from_node(&child))),
        "annotation" => body.push(PulseGeneratorDLBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of PulseGeneratorDL.", t)
      };
    }
    PulseGeneratorDL { delay, duration, amplitude, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InhomogeneousParameterBody {
  proximal(ProximalDetails),
  distal(DistalDetails),
}

#[derive(Debug, Clone, PartialEq)]
pub struct InhomogeneousParameter {
  pub variable: String,
  pub metric: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<InhomogeneousParameterBody>
}

impl XML for InhomogeneousParameter {
  fn from_node(node: &Node) -> Self {
    let variable = node.attribute("variable").map(|s| s.to_string()).unwrap();
    let metric = node.attribute("metric").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "proximal" => body.push(InhomogeneousParameterBody::proximal(ProximalDetails::from_node(&child))),
        "distal" => body.push(InhomogeneousParameterBody::distal(DistalDetails::from_node(&child))),
        t => panic!("Unexpected tag {} in body of InhomogeneousParameter.", t)
      };
    }
    InhomogeneousParameter { variable, metric, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChannelDensityNonUniformGHKBody {
  variableParameter(VariableParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelDensityNonUniformGHK {
  pub ionChannel: String,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ChannelDensityNonUniformGHKBody>
}

impl XML for ChannelDensityNonUniformGHK {
  fn from_node(node: &Node) -> Self {
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "variableParameter" => body.push(ChannelDensityNonUniformGHKBody::variableParameter(VariableParameter::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ChannelDensityNonUniformGHK.", t)
      };
    }
    ChannelDensityNonUniformGHK { ionChannel, ion, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReactionScheme {
  pub source: String,
  pub r#type: String,
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for ReactionScheme {
  fn from_node(node: &Node) -> Self {
    let source = node.attribute("source").map(|s| s.to_string()).unwrap();
    let r#type = node.attribute("type").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ReactionScheme { source, r#type, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VoltageClampTripleBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VoltageClampTriple {
  pub active: f64,
  pub delay: String,
  pub duration: String,
  pub conditioningVoltage: String,
  pub testingVoltage: String,
  pub returnVoltage: String,
  pub simpleSeriesResistance: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<VoltageClampTripleBody>
}

impl XML for VoltageClampTriple {
  fn from_node(node: &Node) -> Self {
    let active = node.attribute("active").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let conditioningVoltage = node.attribute("conditioningVoltage").map(|s| s.to_string()).unwrap();
    let testingVoltage = node.attribute("testingVoltage").map(|s| s.to_string()).unwrap();
    let returnVoltage = node.attribute("returnVoltage").map(|s| s.to_string()).unwrap();
    let simpleSeriesResistance = node.attribute("simpleSeriesResistance").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(VoltageClampTripleBody::notes(String::from_node(&child))),
        "property" => body.push(VoltageClampTripleBody::property(Property::from_node(&child))),
        "annotation" => body.push(VoltageClampTripleBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of VoltageClampTriple.", t)
      };
    }
    VoltageClampTriple { active, delay, duration, conditioningVoltage, testingVoltage, returnVoltage, simpleSeriesResistance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
}

impl XML for Annotation {
  fn from_node(node: &Node) -> Self {
    Annotation {  }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChannelDensityNernstBody {
  variableParameter(VariableParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelDensityNernst {
  pub ionChannel: String,
  pub condDensity: Option<String>,
  pub segmentGroup: String,
  pub segment: Option<String>,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ChannelDensityNernstBody>
}

impl XML for ChannelDensityNernst {
  fn from_node(node: &Node) -> Self {
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let condDensity = node.attribute("condDensity").map(|s| s.to_string());
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    let segment = node.attribute("segment").map(|s| s.to_string());
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "variableParameter" => body.push(ChannelDensityNernstBody::variableParameter(VariableParameter::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ChannelDensityNernst.", t)
      };
    }
    ChannelDensityNernst { ionChannel, condDensity, segmentGroup, segment, ion, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExtracellularPropertiesBody {
  species(Species),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExtracellularProperties {
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ExtracellularPropertiesBody>
}

impl XML for ExtracellularProperties {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "species" => body.push(ExtracellularPropertiesBody::species(Species::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ExtracellularProperties.", t)
      };
    }
    ExtracellularProperties { id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LayoutBody {
  random(RandomLayout),
  grid(GridLayout),
  unstructured(UnstructuredLayout),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Layout {
  pub space: Option<String>,
  pub body: Vec<LayoutBody>
}

impl XML for Layout {
  fn from_node(node: &Node) -> Self {
    let space = node.attribute("space").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "random" => body.push(LayoutBody::random(RandomLayout::from_node(&child))),
        "grid" => body.push(LayoutBody::grid(GridLayout::from_node(&child))),
        "unstructured" => body.push(LayoutBody::unstructured(UnstructuredLayout::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Layout.", t)
      };
    }
    Layout { space, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Spike {
  pub time: String,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for Spike {
  fn from_node(node: &Node) -> Self {
    let time = node.attribute("time").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    Spike { time, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum InstanceBody {
  location(Location),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Instance {
  pub id: Option<i64>,
  pub i: Option<i64>,
  pub j: Option<i64>,
  pub k: Option<i64>,
  pub body: Vec<InstanceBody>
}

impl XML for Instance {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap());
    let i = node.attribute("i").map(|s| s.parse::<i64>().unwrap());
    let j = node.attribute("j").map(|s| s.parse::<i64>().unwrap());
    let k = node.attribute("k").map(|s| s.parse::<i64>().unwrap());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "location" => body.push(InstanceBody::location(Location::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Instance.", t)
      };
    }
    Instance { id, i, j, k, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpThreeSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpThreeSynapse {
  pub tauDecay1: String,
  pub tauDecay2: String,
  pub tauRise: String,
  pub gbase1: String,
  pub gbase2: String,
  pub erev: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ExpThreeSynapseBody>
}

impl XML for ExpThreeSynapse {
  fn from_node(node: &Node) -> Self {
    let tauDecay1 = node.attribute("tauDecay1").map(|s| s.to_string()).unwrap();
    let tauDecay2 = node.attribute("tauDecay2").map(|s| s.to_string()).unwrap();
    let tauRise = node.attribute("tauRise").map(|s| s.to_string()).unwrap();
    let gbase1 = node.attribute("gbase1").map(|s| s.to_string()).unwrap();
    let gbase2 = node.attribute("gbase2").map(|s| s.to_string()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(ExpThreeSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(ExpThreeSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(ExpThreeSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ExpThreeSynapse.", t)
      };
    }
    ExpThreeSynapse { tauDecay1, tauDecay2, tauRise, gbase1, gbase2, erev, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StateVariable {
  pub name: String,
  pub dimension: String,
  pub description: Option<String>,
  pub exposure: Option<String>,
}

impl XML for StateVariable {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    let exposure = node.attribute("exposure").map(|s| s.to_string());
    StateVariable { name, dimension, description, exposure }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseProjection {
  pub presynapticPopulation: String,
  pub postsynapticPopulation: String,
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for BaseProjection {
  fn from_node(node: &Node) -> Self {
    let presynapticPopulation = node.attribute("presynapticPopulation").map(|s| s.to_string()).unwrap();
    let postsynapticPopulation = node.attribute("postsynapticPopulation").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    BaseProjection { presynapticPopulation, postsynapticPopulation, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum RampGeneratorDLBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct RampGeneratorDL {
  pub delay: String,
  pub duration: String,
  pub startAmplitude: String,
  pub finishAmplitude: String,
  pub baselineAmplitude: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<RampGeneratorDLBody>
}

impl XML for RampGeneratorDL {
  fn from_node(node: &Node) -> Self {
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let startAmplitude = node.attribute("startAmplitude").map(|s| s.to_string()).unwrap();
    let finishAmplitude = node.attribute("finishAmplitude").map(|s| s.to_string()).unwrap();
    let baselineAmplitude = node.attribute("baselineAmplitude").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(RampGeneratorDLBody::notes(String::from_node(&child))),
        "property" => body.push(RampGeneratorDLBody::property(Property::from_node(&child))),
        "annotation" => body.push(RampGeneratorDLBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of RampGeneratorDL.", t)
      };
    }
    RampGeneratorDL { delay, duration, startAmplitude, finishAmplitude, baselineAmplitude, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EIF_cond_alpha_isfa_istaBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EIF_cond_alpha_isfa_ista {
  pub a: f64,
  pub b: f64,
  pub delta_T: f64,
  pub tau_w: f64,
  pub v_spike: f64,
  pub e_rev_E: f64,
  pub e_rev_I: f64,
  pub tau_m: f64,
  pub tau_refrac: f64,
  pub v_reset: f64,
  pub v_rest: f64,
  pub v_thresh: f64,
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<EIF_cond_alpha_isfa_istaBody>
}

impl XML for EIF_cond_alpha_isfa_ista {
  fn from_node(node: &Node) -> Self {
    let a = node.attribute("a").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let b = node.attribute("b").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let delta_T = node.attribute("delta_T").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_w = node.attribute("tau_w").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_spike = node.attribute("v_spike").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_E = node.attribute("e_rev_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_I = node.attribute("e_rev_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_m = node.attribute("tau_m").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_refrac = node.attribute("tau_refrac").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_reset = node.attribute("v_reset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_rest = node.attribute("v_rest").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_thresh = node.attribute("v_thresh").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(EIF_cond_alpha_isfa_istaBody::notes(String::from_node(&child))),
        "property" => body.push(EIF_cond_alpha_isfa_istaBody::property(Property::from_node(&child))),
        "annotation" => body.push(EIF_cond_alpha_isfa_istaBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of EIF_cond_alpha_isfa_ista.", t)
      };
    }
    EIF_cond_alpha_isfa_ista { a, b, delta_T, tau_w, v_spike, e_rev_E, e_rev_I, tau_m, tau_refrac, v_reset, v_rest, v_thresh, cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PulseGeneratorBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PulseGenerator {
  pub delay: String,
  pub duration: String,
  pub amplitude: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<PulseGeneratorBody>
}

impl XML for PulseGenerator {
  fn from_node(node: &Node) -> Self {
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let amplitude = node.attribute("amplitude").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(PulseGeneratorBody::notes(String::from_node(&child))),
        "property" => body.push(PulseGeneratorBody::property(Property::from_node(&child))),
        "annotation" => body.push(PulseGeneratorBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of PulseGenerator.", t)
      };
    }
    PulseGenerator { delay, duration, amplitude, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompoundInputBody {
  pulseGenerator(PulseGenerator),
  sineGenerator(SineGenerator),
  rampGenerator(RampGenerator),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundInput {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<CompoundInputBody>
}

impl XML for CompoundInput {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "pulseGenerator" => body.push(CompoundInputBody::pulseGenerator(PulseGenerator::from_node(&child))),
        "sineGenerator" => body.push(CompoundInputBody::sineGenerator(SineGenerator::from_node(&child))),
        "rampGenerator" => body.push(CompoundInputBody::rampGenerator(RampGenerator::from_node(&child))),
        "notes" => body.push(CompoundInputBody::notes(String::from_node(&child))),
        "property" => body.push(CompoundInputBody::property(Property::from_node(&child))),
        "annotation" => body.push(CompoundInputBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of CompoundInput.", t)
      };
    }
    CompoundInput { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct OpenState {
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for OpenState {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    OpenState { id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FitzHughNagumoCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FitzHughNagumoCell {
  pub I: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<FitzHughNagumoCellBody>
}

impl XML for FitzHughNagumoCell {
  fn from_node(node: &Node) -> Self {
    let I = node.attribute("I").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(FitzHughNagumoCellBody::notes(String::from_node(&child))),
        "property" => body.push(FitzHughNagumoCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(FitzHughNagumoCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of FitzHughNagumoCell.", t)
      };
    }
    FitzHughNagumoCell { I, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ContinuousConnectionInstanceW {
  pub weight: f64,
  pub preComponent: String,
  pub postComponent: String,
  pub preCell: String,
  pub preSegment: i64,
  pub preFractionAlong: f64,
  pub postCell: String,
  pub postSegment: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for ContinuousConnectionInstanceW {
  fn from_node(node: &Node) -> Self {
    let weight = node.attribute("weight").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let preComponent = node.attribute("preComponent").map(|s| s.to_string()).unwrap();
    let postComponent = node.attribute("postComponent").map(|s| s.to_string()).unwrap();
    let preCell = node.attribute("preCell").map(|s| s.to_string()).unwrap();
    let preSegment = node.attribute("preSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCell = node.attribute("postCell").map(|s| s.to_string()).unwrap();
    let postSegment = node.attribute("postSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ContinuousConnectionInstanceW { weight, preComponent, postComponent, preCell, preSegment, preFractionAlong, postCell, postSegment, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CellBody {
  morphology(Morphology),
  biophysicalProperties(BiophysicalProperties),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cell {
  pub morphology: Option<String>,
  pub biophysicalProperties: Option<String>,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<CellBody>
}

impl XML for Cell {
  fn from_node(node: &Node) -> Self {
    let morphology = node.attribute("morphology").map(|s| s.to_string());
    let biophysicalProperties = node.attribute("biophysicalProperties").map(|s| s.to_string());
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "morphology" => body.push(CellBody::morphology(Morphology::from_node(&child))),
        "biophysicalProperties" => body.push(CellBody::biophysicalProperties(BiophysicalProperties::from_node(&child))),
        "notes" => body.push(CellBody::notes(String::from_node(&child))),
        "property" => body.push(CellBody::property(Property::from_node(&child))),
        "annotation" => body.push(CellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Cell.", t)
      };
    }
    Cell { morphology, biophysicalProperties, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IafTauRefCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IafTauRefCell {
  pub refract: String,
  pub leakReversal: String,
  pub thresh: String,
  pub reset: String,
  pub tau: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IafTauRefCellBody>
}

impl XML for IafTauRefCell {
  fn from_node(node: &Node) -> Self {
    let refract = node.attribute("refract").map(|s| s.to_string()).unwrap();
    let leakReversal = node.attribute("leakReversal").map(|s| s.to_string()).unwrap();
    let thresh = node.attribute("thresh").map(|s| s.to_string()).unwrap();
    let reset = node.attribute("reset").map(|s| s.to_string()).unwrap();
    let tau = node.attribute("tau").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(IafTauRefCellBody::notes(String::from_node(&child))),
        "property" => body.push(IafTauRefCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(IafTauRefCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IafTauRefCell.", t)
      };
    }
    IafTauRefCell { refract, leakReversal, thresh, reset, tau, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseConnectionNewFormat {
  pub preCell: String,
  pub preSegment: i64,
  pub preFractionAlong: f64,
  pub postCell: String,
  pub postSegment: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for BaseConnectionNewFormat {
  fn from_node(node: &Node) -> Self {
    let preCell = node.attribute("preCell").map(|s| s.to_string()).unwrap();
    let preSegment = node.attribute("preSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCell = node.attribute("postCell").map(|s| s.to_string()).unwrap();
    let postSegment = node.attribute("postSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    BaseConnectionNewFormat { preCell, preSegment, preFractionAlong, postCell, postSegment, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateHHRatesInfBody {
  notes(String),
  q10Settings(Q10Settings),
  forwardRate(HHRate),
  reverseRate(HHRate),
  steadyState(HHVariable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateHHRatesInf {
  pub instances: i64,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateHHRatesInfBody>
}

impl XML for GateHHRatesInf {
  fn from_node(node: &Node) -> Self {
    let instances = node.attribute("instances").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateHHRatesInfBody::notes(String::from_node(&child))),
        "q10Settings" => body.push(GateHHRatesInfBody::q10Settings(Q10Settings::from_node(&child))),
        "forwardRate" => body.push(GateHHRatesInfBody::forwardRate(HHRate::from_node(&child))),
        "reverseRate" => body.push(GateHHRatesInfBody::reverseRate(HHRate::from_node(&child))),
        "steadyState" => body.push(GateHHRatesInfBody::steadyState(HHVariable::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateHHRatesInf.", t)
      };
    }
    GateHHRatesInf { instances, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case {
  pub condition: Option<String>,
  pub value: String,
}

impl XML for Case {
  fn from_node(node: &Node) -> Self {
    let condition = node.attribute("condition").map(|s| s.to_string());
    let value = node.attribute("value").map(|s| s.to_string()).unwrap();
    Case { condition, value }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpTwoSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpTwoSynapse {
  pub tauDecay: String,
  pub tauRise: String,
  pub gbase: String,
  pub erev: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ExpTwoSynapseBody>
}

impl XML for ExpTwoSynapse {
  fn from_node(node: &Node) -> Self {
    let tauDecay = node.attribute("tauDecay").map(|s| s.to_string()).unwrap();
    let tauRise = node.attribute("tauRise").map(|s| s.to_string()).unwrap();
    let gbase = node.attribute("gbase").map(|s| s.to_string()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(ExpTwoSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(ExpTwoSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(ExpTwoSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ExpTwoSynapse.", t)
      };
    }
    ExpTwoSynapse { tauDecay, tauRise, gbase, erev, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElectricalConnectionInstance {
  pub synapse: String,
  pub preCell: String,
  pub preSegment: i64,
  pub preFractionAlong: f64,
  pub postCell: String,
  pub postSegment: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for ElectricalConnectionInstance {
  fn from_node(node: &Node) -> Self {
    let synapse = node.attribute("synapse").map(|s| s.to_string()).unwrap();
    let preCell = node.attribute("preCell").map(|s| s.to_string()).unwrap();
    let preSegment = node.attribute("preSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCell = node.attribute("postCell").map(|s| s.to_string()).unwrap();
    let postSegment = node.attribute("postSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ElectricalConnectionInstance { synapse, preCell, preSegment, preFractionAlong, postCell, postSegment, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Resistivity {
  pub value: String,
  pub segmentGroup: String,
}

impl XML for Resistivity {
  fn from_node(node: &Node) -> Self {
    let value = node.attribute("value").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    Resistivity { value, segmentGroup }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateHHInstantaneousBody {
  notes(String),
  steadyState(HHVariable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateHHInstantaneous {
  pub instances: i64,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateHHInstantaneousBody>
}

impl XML for GateHHInstantaneous {
  fn from_node(node: &Node) -> Self {
    let instances = node.attribute("instances").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateHHInstantaneousBody::notes(String::from_node(&child))),
        "steadyState" => body.push(GateHHInstantaneousBody::steadyState(HHVariable::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateHHInstantaneous.", t)
      };
    }
    GateHHInstantaneous { instances, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Point3DWithDiam {
  pub x: f64,
  pub y: f64,
  pub z: f64,
  pub diameter: f64,
}

impl XML for Point3DWithDiam {
  fn from_node(node: &Node) -> Self {
    let x = node.attribute("x").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let y = node.attribute("y").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let z = node.attribute("z").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let diameter = node.attribute("diameter").map(|s| s.parse::<f64>().unwrap()).unwrap();
    Point3DWithDiam { x, y, z, diameter }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum VoltageClampBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct VoltageClamp {
  pub delay: String,
  pub duration: String,
  pub targetVoltage: String,
  pub simpleSeriesResistance: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<VoltageClampBody>
}

impl XML for VoltageClamp {
  fn from_node(node: &Node) -> Self {
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let targetVoltage = node.attribute("targetVoltage").map(|s| s.to_string()).unwrap();
    let simpleSeriesResistance = node.attribute("simpleSeriesResistance").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(VoltageClampBody::notes(String::from_node(&child))),
        "property" => body.push(VoltageClampBody::property(Property::from_node(&child))),
        "annotation" => body.push(VoltageClampBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of VoltageClamp.", t)
      };
    }
    VoltageClamp { delay, duration, targetVoltage, simpleSeriesResistance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SpikeGeneratorRandomBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpikeGeneratorRandom {
  pub maxISI: String,
  pub minISI: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SpikeGeneratorRandomBody>
}

impl XML for SpikeGeneratorRandom {
  fn from_node(node: &Node) -> Self {
    let maxISI = node.attribute("maxISI").map(|s| s.to_string()).unwrap();
    let minISI = node.attribute("minISI").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(SpikeGeneratorRandomBody::notes(String::from_node(&child))),
        "property" => body.push(SpikeGeneratorRandomBody::property(Property::from_node(&child))),
        "annotation" => body.push(SpikeGeneratorRandomBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SpikeGeneratorRandom.", t)
      };
    }
    SpikeGeneratorRandom { maxISI, minISI, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BlockingPlasticSynapseBody {
  plasticityMechanism(PlasticityMechanism),
  blockMechanism(BlockMechanism),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockingPlasticSynapse {
  pub tauDecay: String,
  pub tauRise: String,
  pub gbase: String,
  pub erev: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BlockingPlasticSynapseBody>
}

impl XML for BlockingPlasticSynapse {
  fn from_node(node: &Node) -> Self {
    let tauDecay = node.attribute("tauDecay").map(|s| s.to_string()).unwrap();
    let tauRise = node.attribute("tauRise").map(|s| s.to_string()).unwrap();
    let gbase = node.attribute("gbase").map(|s| s.to_string()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "plasticityMechanism" => body.push(BlockingPlasticSynapseBody::plasticityMechanism(PlasticityMechanism::from_node(&child))),
        "blockMechanism" => body.push(BlockingPlasticSynapseBody::blockMechanism(BlockMechanism::from_node(&child))),
        "notes" => body.push(BlockingPlasticSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(BlockingPlasticSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(BlockingPlasticSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BlockingPlasticSynapse.", t)
      };
    }
    BlockingPlasticSynapse { tauDecay, tauRise, gbase, erev, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Requirement {
  pub name: String,
  pub dimension: String,
  pub description: Option<String>,
}

impl XML for Requirement {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    Requirement { name, dimension, description }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IzhikevichCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IzhikevichCell {
  pub v0: String,
  pub thresh: String,
  pub a: String,
  pub b: String,
  pub c: String,
  pub d: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IzhikevichCellBody>
}

impl XML for IzhikevichCell {
  fn from_node(node: &Node) -> Self {
    let v0 = node.attribute("v0").map(|s| s.to_string()).unwrap();
    let thresh = node.attribute("thresh").map(|s| s.to_string()).unwrap();
    let a = node.attribute("a").map(|s| s.to_string()).unwrap();
    let b = node.attribute("b").map(|s| s.to_string()).unwrap();
    let c = node.attribute("c").map(|s| s.to_string()).unwrap();
    let d = node.attribute("d").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(IzhikevichCellBody::notes(String::from_node(&child))),
        "property" => body.push(IzhikevichCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(IzhikevichCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IzhikevichCell.", t)
      };
    }
    IzhikevichCell { v0, thresh, a, b, c, d, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IncludeType {
  pub href: String,
}

impl XML for IncludeType {
  fn from_node(node: &Node) -> Self {
    let href = node.attribute("href").map(|s| s.to_string()).unwrap();
    IncludeType { href }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExtracellularPropertiesLocalBody {
  species(Species),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExtracellularPropertiesLocal {
  pub body: Vec<ExtracellularPropertiesLocalBody>
}

impl XML for ExtracellularPropertiesLocal {
  fn from_node(node: &Node) -> Self {
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "species" => body.push(ExtracellularPropertiesLocalBody::species(Species::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ExtracellularPropertiesLocal.", t)
      };
    }
    ExtracellularPropertiesLocal { body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IafTauCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IafTauCell {
  pub leakReversal: String,
  pub thresh: String,
  pub reset: String,
  pub tau: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IafTauCellBody>
}

impl XML for IafTauCell {
  fn from_node(node: &Node) -> Self {
    let leakReversal = node.attribute("leakReversal").map(|s| s.to_string()).unwrap();
    let thresh = node.attribute("thresh").map(|s| s.to_string()).unwrap();
    let reset = node.attribute("reset").map(|s| s.to_string()).unwrap();
    let tau = node.attribute("tau").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(IafTauCellBody::notes(String::from_node(&child))),
        "property" => body.push(IafTauCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(IafTauCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IafTauCell.", t)
      };
    }
    IafTauCell { leakReversal, thresh, reset, tau, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ElectricalConnection {
  pub synapse: String,
  pub preCell: String,
  pub preSegment: i64,
  pub preFractionAlong: f64,
  pub postCell: String,
  pub postSegment: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for ElectricalConnection {
  fn from_node(node: &Node) -> Self {
    let synapse = node.attribute("synapse").map(|s| s.to_string()).unwrap();
    let preCell = node.attribute("preCell").map(|s| s.to_string()).unwrap();
    let preSegment = node.attribute("preSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCell = node.attribute("postCell").map(|s| s.to_string()).unwrap();
    let postSegment = node.attribute("postSegment").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ElectricalConnection { synapse, preCell, preSegment, preFractionAlong, postCell, postSegment, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseCellMembPotCapBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseCellMembPotCap {
  pub C: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BaseCellMembPotCapBody>
}

impl XML for BaseCellMembPotCap {
  fn from_node(node: &Node) -> Self {
    let C = node.attribute("C").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(BaseCellMembPotCapBody::notes(String::from_node(&child))),
        "property" => body.push(BaseCellMembPotCapBody::property(Property::from_node(&child))),
        "annotation" => body.push(BaseCellMembPotCapBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BaseCellMembPotCap.", t)
      };
    }
    BaseCellMembPotCap { C, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum basePyNNIaFCondCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct basePyNNIaFCondCell {
  pub e_rev_E: f64,
  pub e_rev_I: f64,
  pub tau_m: f64,
  pub tau_refrac: f64,
  pub v_reset: f64,
  pub v_rest: f64,
  pub v_thresh: f64,
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<basePyNNIaFCondCellBody>
}

impl XML for basePyNNIaFCondCell {
  fn from_node(node: &Node) -> Self {
    let e_rev_E = node.attribute("e_rev_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_I = node.attribute("e_rev_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_m = node.attribute("tau_m").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_refrac = node.attribute("tau_refrac").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_reset = node.attribute("v_reset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_rest = node.attribute("v_rest").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_thresh = node.attribute("v_thresh").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(basePyNNIaFCondCellBody::notes(String::from_node(&child))),
        "property" => body.push(basePyNNIaFCondCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(basePyNNIaFCondCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of basePyNNIaFCondCell.", t)
      };
    }
    basePyNNIaFCondCell { e_rev_E, e_rev_I, tau_m, tau_refrac, v_reset, v_rest, v_thresh, cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompoundInputDLBody {
  pulseGeneratorDL(PulseGeneratorDL),
  sineGeneratorDL(SineGeneratorDL),
  rampGeneratorDL(RampGeneratorDL),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompoundInputDL {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<CompoundInputDLBody>
}

impl XML for CompoundInputDL {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "pulseGeneratorDL" => body.push(CompoundInputDLBody::pulseGeneratorDL(PulseGeneratorDL::from_node(&child))),
        "sineGeneratorDL" => body.push(CompoundInputDLBody::sineGeneratorDL(SineGeneratorDL::from_node(&child))),
        "rampGeneratorDL" => body.push(CompoundInputDLBody::rampGeneratorDL(RampGeneratorDL::from_node(&child))),
        "notes" => body.push(CompoundInputDLBody::notes(String::from_node(&child))),
        "property" => body.push(CompoundInputDLBody::property(Property::from_node(&child))),
        "annotation" => body.push(CompoundInputDLBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of CompoundInputDL.", t)
      };
    }
    CompoundInputDL { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RandomLayout {
  pub number: Option<i64>,
  pub region: Option<String>,
}

impl XML for RandomLayout {
  fn from_node(node: &Node) -> Self {
    let number = node.attribute("number").map(|s| s.parse::<i64>().unwrap());
    let region = node.attribute("region").map(|s| s.to_string());
    RandomLayout { number, region }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum basePyNNCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct basePyNNCell {
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<basePyNNCellBody>
}

impl XML for basePyNNCell {
  fn from_node(node: &Node) -> Self {
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(basePyNNCellBody::notes(String::from_node(&child))),
        "property" => body.push(basePyNNCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(basePyNNCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of basePyNNCell.", t)
      };
    }
    basePyNNCell { cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseConnection {
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for BaseConnection {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    BaseConnection { id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Q10ConductanceScaling {
  pub q10Factor: String,
  pub experimentalTemp: String,
}

impl XML for Q10ConductanceScaling {
  fn from_node(node: &Node) -> Self {
    let q10Factor = node.attribute("q10Factor").map(|s| s.to_string()).unwrap();
    let experimentalTemp = node.attribute("experimentalTemp").map(|s| s.to_string()).unwrap();
    Q10ConductanceScaling { q10Factor, experimentalTemp }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnstructuredLayout {
  pub number: Option<i64>,
}

impl XML for UnstructuredLayout {
  fn from_node(node: &Node) -> Self {
    let number = node.attribute("number").map(|s| s.parse::<i64>().unwrap());
    UnstructuredLayout { number }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseCell {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BaseCellBody>
}

impl XML for BaseCell {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(BaseCellBody::notes(String::from_node(&child))),
        "property" => body.push(BaseCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(BaseCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BaseCell.", t)
      };
    }
    BaseCell { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IonChannelBody {
  gate(GateHHUndetermined),
  gateHHrates(GateHHRates),
  gateHHratesTau(GateHHRatesTau),
  gateHHtauInf(GateHHTauInf),
  gateHHratesInf(GateHHRatesInf),
  gateHHratesTauInf(GateHHRatesTauInf),
  gateHHInstantaneous(GateHHInstantaneous),
  gateFractional(GateFractional),
  q10ConductanceScaling(Q10ConductanceScaling),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IonChannel {
  pub species: Option<String>,
  pub r#type: Option<String>,
  pub conductance: Option<String>,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IonChannelBody>
}

impl XML for IonChannel {
  fn from_node(node: &Node) -> Self {
    let species = node.attribute("species").map(|s| s.to_string());
    let r#type = node.attribute("type").map(|s| s.to_string());
    let conductance = node.attribute("conductance").map(|s| s.to_string());
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "gate" => body.push(IonChannelBody::gate(GateHHUndetermined::from_node(&child))),
        "gateHHrates" => body.push(IonChannelBody::gateHHrates(GateHHRates::from_node(&child))),
        "gateHHratesTau" => body.push(IonChannelBody::gateHHratesTau(GateHHRatesTau::from_node(&child))),
        "gateHHtauInf" => body.push(IonChannelBody::gateHHtauInf(GateHHTauInf::from_node(&child))),
        "gateHHratesInf" => body.push(IonChannelBody::gateHHratesInf(GateHHRatesInf::from_node(&child))),
        "gateHHratesTauInf" => body.push(IonChannelBody::gateHHratesTauInf(GateHHRatesTauInf::from_node(&child))),
        "gateHHInstantaneous" => body.push(IonChannelBody::gateHHInstantaneous(GateHHInstantaneous::from_node(&child))),
        "gateFractional" => body.push(IonChannelBody::gateFractional(GateFractional::from_node(&child))),
        "q10ConductanceScaling" => body.push(IonChannelBody::q10ConductanceScaling(Q10ConductanceScaling::from_node(&child))),
        "notes" => body.push(IonChannelBody::notes(String::from_node(&child))),
        "property" => body.push(IonChannelBody::property(Property::from_node(&child))),
        "annotation" => body.push(IonChannelBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IonChannel.", t)
      };
    }
    IonChannel { species, r#type, conductance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Species {
  pub id: String,
  pub concentrationModel: String,
  pub ion: Option<String>,
  pub initialConcentration: String,
  pub initialExtConcentration: String,
  pub segmentGroup: String,
}

impl XML for Species {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let concentrationModel = node.attribute("concentrationModel").map(|s| s.to_string()).unwrap();
    let ion = node.attribute("ion").map(|s| s.to_string());
    let initialConcentration = node.attribute("initialConcentration").map(|s| s.to_string()).unwrap();
    let initialExtConcentration = node.attribute("initialExtConcentration").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    Species { id, concentrationModel, ion, initialConcentration, initialExtConcentration, segmentGroup }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseWithoutId {
  pub neuroLexId: Option<String>,
}

impl XML for BaseWithoutId {
  fn from_node(node: &Node) -> Self {
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    BaseWithoutId { neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ProjectionBody {
  connection(Connection),
  connectionWD(ConnectionWD),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Projection {
  pub synapse: String,
  pub presynapticPopulation: String,
  pub postsynapticPopulation: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ProjectionBody>
}

impl XML for Projection {
  fn from_node(node: &Node) -> Self {
    let synapse = node.attribute("synapse").map(|s| s.to_string()).unwrap();
    let presynapticPopulation = node.attribute("presynapticPopulation").map(|s| s.to_string()).unwrap();
    let postsynapticPopulation = node.attribute("postsynapticPopulation").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "connection" => body.push(ProjectionBody::connection(Connection::from_node(&child))),
        "connectionWD" => body.push(ProjectionBody::connectionWD(ConnectionWD::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Projection.", t)
      };
    }
    Projection { synapse, presynapticPopulation, postsynapticPopulation, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AlphaCondSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AlphaCondSynapse {
  pub e_rev: f64,
  pub tau_syn: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<AlphaCondSynapseBody>
}

impl XML for AlphaCondSynapse {
  fn from_node(node: &Node) -> Self {
    let e_rev = node.attribute("e_rev").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn = node.attribute("tau_syn").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(AlphaCondSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(AlphaCondSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(AlphaCondSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of AlphaCondSynapse.", t)
      };
    }
    AlphaCondSynapse { e_rev, tau_syn, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SpikeGeneratorRefPoissonBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpikeGeneratorRefPoisson {
  pub minimumISI: String,
  pub averageRate: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SpikeGeneratorRefPoissonBody>
}

impl XML for SpikeGeneratorRefPoisson {
  fn from_node(node: &Node) -> Self {
    let minimumISI = node.attribute("minimumISI").map(|s| s.to_string()).unwrap();
    let averageRate = node.attribute("averageRate").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(SpikeGeneratorRefPoissonBody::notes(String::from_node(&child))),
        "property" => body.push(SpikeGeneratorRefPoissonBody::property(Property::from_node(&child))),
        "annotation" => body.push(SpikeGeneratorRefPoissonBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SpikeGeneratorRefPoisson.", t)
      };
    }
    SpikeGeneratorRefPoisson { minimumISI, averageRate, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
  pub x: f64,
  pub y: f64,
  pub z: f64,
}

impl XML for Location {
  fn from_node(node: &Node) -> Self {
    let x = node.attribute("x").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let y = node.attribute("y").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let z = node.attribute("z").map(|s| s.parse::<f64>().unwrap()).unwrap();
    Location { x, y, z }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IonChannelHHBody {
  gate(GateHHUndetermined),
  gateHHrates(GateHHRates),
  gateHHratesTau(GateHHRatesTau),
  gateHHtauInf(GateHHTauInf),
  gateHHratesInf(GateHHRatesInf),
  gateHHratesTauInf(GateHHRatesTauInf),
  gateHHInstantaneous(GateHHInstantaneous),
  gateFractional(GateFractional),
  q10ConductanceScaling(Q10ConductanceScaling),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IonChannelHH {
  pub species: Option<String>,
  pub r#type: Option<String>,
  pub conductance: Option<String>,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IonChannelHHBody>
}

impl XML for IonChannelHH {
  fn from_node(node: &Node) -> Self {
    let species = node.attribute("species").map(|s| s.to_string());
    let r#type = node.attribute("type").map(|s| s.to_string());
    let conductance = node.attribute("conductance").map(|s| s.to_string());
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "gate" => body.push(IonChannelHHBody::gate(GateHHUndetermined::from_node(&child))),
        "gateHHrates" => body.push(IonChannelHHBody::gateHHrates(GateHHRates::from_node(&child))),
        "gateHHratesTau" => body.push(IonChannelHHBody::gateHHratesTau(GateHHRatesTau::from_node(&child))),
        "gateHHtauInf" => body.push(IonChannelHHBody::gateHHtauInf(GateHHTauInf::from_node(&child))),
        "gateHHratesInf" => body.push(IonChannelHHBody::gateHHratesInf(GateHHRatesInf::from_node(&child))),
        "gateHHratesTauInf" => body.push(IonChannelHHBody::gateHHratesTauInf(GateHHRatesTauInf::from_node(&child))),
        "gateHHInstantaneous" => body.push(IonChannelHHBody::gateHHInstantaneous(GateHHInstantaneous::from_node(&child))),
        "gateFractional" => body.push(IonChannelHHBody::gateFractional(GateFractional::from_node(&child))),
        "q10ConductanceScaling" => body.push(IonChannelHHBody::q10ConductanceScaling(Q10ConductanceScaling::from_node(&child))),
        "notes" => body.push(IonChannelHHBody::notes(String::from_node(&child))),
        "property" => body.push(IonChannelHHBody::property(Property::from_node(&child))),
        "annotation" => body.push(IonChannelHHBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IonChannelHH.", t)
      };
    }
    IonChannelHH { species, r#type, conductance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EIF_cond_exp_isfa_istaBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EIF_cond_exp_isfa_ista {
  pub a: f64,
  pub b: f64,
  pub delta_T: f64,
  pub tau_w: f64,
  pub v_spike: f64,
  pub e_rev_E: f64,
  pub e_rev_I: f64,
  pub tau_m: f64,
  pub tau_refrac: f64,
  pub v_reset: f64,
  pub v_rest: f64,
  pub v_thresh: f64,
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<EIF_cond_exp_isfa_istaBody>
}

impl XML for EIF_cond_exp_isfa_ista {
  fn from_node(node: &Node) -> Self {
    let a = node.attribute("a").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let b = node.attribute("b").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let delta_T = node.attribute("delta_T").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_w = node.attribute("tau_w").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_spike = node.attribute("v_spike").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_E = node.attribute("e_rev_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_I = node.attribute("e_rev_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_m = node.attribute("tau_m").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_refrac = node.attribute("tau_refrac").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_reset = node.attribute("v_reset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_rest = node.attribute("v_rest").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_thresh = node.attribute("v_thresh").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(EIF_cond_exp_isfa_istaBody::notes(String::from_node(&child))),
        "property" => body.push(EIF_cond_exp_isfa_istaBody::property(Property::from_node(&child))),
        "annotation" => body.push(EIF_cond_exp_isfa_istaBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of EIF_cond_exp_isfa_ista.", t)
      };
    }
    EIF_cond_exp_isfa_ista { a, b, delta_T, tau_w, v_spike, e_rev_E, e_rev_I, tau_m, tau_refrac, v_reset, v_rest, v_thresh, cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpOneSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpOneSynapse {
  pub tauDecay: String,
  pub gbase: String,
  pub erev: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ExpOneSynapseBody>
}

impl XML for ExpOneSynapse {
  fn from_node(node: &Node) -> Self {
    let tauDecay = node.attribute("tauDecay").map(|s| s.to_string()).unwrap();
    let gbase = node.attribute("gbase").map(|s| s.to_string()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(ExpOneSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(ExpOneSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(ExpOneSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ExpOneSynapse.", t)
      };
    }
    ExpOneSynapse { tauDecay, gbase, erev, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseNonNegativeIntegerId {
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for BaseNonNegativeIntegerId {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    BaseNonNegativeIntegerId { id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BiophysicalProperties2CaPoolsBody {
  membraneProperties2CaPools(MembraneProperties2CaPools),
  intracellularProperties2CaPools(IntracellularProperties2CaPools),
  extracellularProperties(ExtracellularProperties),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BiophysicalProperties2CaPools {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BiophysicalProperties2CaPoolsBody>
}

impl XML for BiophysicalProperties2CaPools {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "membraneProperties2CaPools" => body.push(BiophysicalProperties2CaPoolsBody::membraneProperties2CaPools(MembraneProperties2CaPools::from_node(&child))),
        "intracellularProperties2CaPools" => body.push(BiophysicalProperties2CaPoolsBody::intracellularProperties2CaPools(IntracellularProperties2CaPools::from_node(&child))),
        "extracellularProperties" => body.push(BiophysicalProperties2CaPoolsBody::extracellularProperties(ExtracellularProperties::from_node(&child))),
        "notes" => body.push(BiophysicalProperties2CaPoolsBody::notes(String::from_node(&child))),
        "property" => body.push(BiophysicalProperties2CaPoolsBody::property(Property::from_node(&child))),
        "annotation" => body.push(BiophysicalProperties2CaPoolsBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BiophysicalProperties2CaPools.", t)
      };
    }
    BiophysicalProperties2CaPools { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IafRefCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IafRefCell {
  pub refract: String,
  pub leakReversal: String,
  pub thresh: String,
  pub reset: String,
  pub C: String,
  pub leakConductance: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IafRefCellBody>
}

impl XML for IafRefCell {
  fn from_node(node: &Node) -> Self {
    let refract = node.attribute("refract").map(|s| s.to_string()).unwrap();
    let leakReversal = node.attribute("leakReversal").map(|s| s.to_string()).unwrap();
    let thresh = node.attribute("thresh").map(|s| s.to_string()).unwrap();
    let reset = node.attribute("reset").map(|s| s.to_string()).unwrap();
    let C = node.attribute("C").map(|s| s.to_string()).unwrap();
    let leakConductance = node.attribute("leakConductance").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(IafRefCellBody::notes(String::from_node(&child))),
        "property" => body.push(IafRefCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(IafRefCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IafRefCell.", t)
      };
    }
    IafRefCell { refract, leakReversal, thresh, reset, C, leakConductance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Exposure {
  pub name: String,
  pub dimension: String,
  pub description: Option<String>,
}

impl XML for Exposure {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    Exposure { name, dimension, description }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct InitMembPotential {
  pub value: String,
  pub segmentGroup: String,
}

impl XML for InitMembPotential {
  fn from_node(node: &Node) -> Self {
    let value = node.attribute("value").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    InitMembPotential { value, segmentGroup }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TimedSynapticInputBody {
  spike(Spike),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TimedSynapticInput {
  pub synapse: String,
  pub spikeTarget: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<TimedSynapticInputBody>
}

impl XML for TimedSynapticInput {
  fn from_node(node: &Node) -> Self {
    let synapse = node.attribute("synapse").map(|s| s.to_string()).unwrap();
    let spikeTarget = node.attribute("spikeTarget").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "spike" => body.push(TimedSynapticInputBody::spike(Spike::from_node(&child))),
        "notes" => body.push(TimedSynapticInputBody::notes(String::from_node(&child))),
        "property" => body.push(TimedSynapticInputBody::property(Property::from_node(&child))),
        "annotation" => body.push(TimedSynapticInputBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of TimedSynapticInput.", t)
      };
    }
    TimedSynapticInput { synapse, spikeTarget, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IF_cond_expBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IF_cond_exp {
  pub e_rev_E: f64,
  pub e_rev_I: f64,
  pub tau_m: f64,
  pub tau_refrac: f64,
  pub v_reset: f64,
  pub v_rest: f64,
  pub v_thresh: f64,
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IF_cond_expBody>
}

impl XML for IF_cond_exp {
  fn from_node(node: &Node) -> Self {
    let e_rev_E = node.attribute("e_rev_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_I = node.attribute("e_rev_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_m = node.attribute("tau_m").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_refrac = node.attribute("tau_refrac").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_reset = node.attribute("v_reset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_rest = node.attribute("v_rest").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_thresh = node.attribute("v_thresh").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(IF_cond_expBody::notes(String::from_node(&child))),
        "property" => body.push(IF_cond_expBody::property(Property::from_node(&child))),
        "annotation" => body.push(IF_cond_expBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IF_cond_exp.", t)
      };
    }
    IF_cond_exp { e_rev_E, e_rev_I, tau_m, tau_refrac, v_reset, v_rest, v_thresh, cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Property {
  pub tag: String,
  pub value: String,
}

impl XML for Property {
  fn from_node(node: &Node) -> Self {
    let tag = node.attribute("tag").map(|s| s.to_string()).unwrap();
    let value = node.attribute("value").map(|s| s.to_string()).unwrap();
    Property { tag, value }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateFractionalSubgateBody {
  notes(String),
  q10Settings(Q10Settings),
  steadyState(HHVariable),
  timeCourse(HHTime),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateFractionalSubgate {
  pub fractionalConductance: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateFractionalSubgateBody>
}

impl XML for GateFractionalSubgate {
  fn from_node(node: &Node) -> Self {
    let fractionalConductance = node.attribute("fractionalConductance").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateFractionalSubgateBody::notes(String::from_node(&child))),
        "q10Settings" => body.push(GateFractionalSubgateBody::q10Settings(Q10Settings::from_node(&child))),
        "steadyState" => body.push(GateFractionalSubgateBody::steadyState(HHVariable::from_node(&child))),
        "timeCourse" => body.push(GateFractionalSubgateBody::timeCourse(HHTime::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateFractionalSubgate.", t)
      };
    }
    GateFractionalSubgate { fractionalConductance, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GradedSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GradedSynapse {
  pub conductance: String,
  pub delta: String,
  pub Vth: String,
  pub k: String,
  pub erev: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GradedSynapseBody>
}

impl XML for GradedSynapse {
  fn from_node(node: &Node) -> Self {
    let conductance = node.attribute("conductance").map(|s| s.to_string()).unwrap();
    let delta = node.attribute("delta").map(|s| s.to_string()).unwrap();
    let Vth = node.attribute("Vth").map(|s| s.to_string()).unwrap();
    let k = node.attribute("k").map(|s| s.to_string()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GradedSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(GradedSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(GradedSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GradedSynapse.", t)
      };
    }
    GradedSynapse { conductance, delta, Vth, k, erev, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PinskyRinzelCA3CellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PinskyRinzelCA3Cell {
  pub iSoma: String,
  pub iDend: String,
  pub gc: String,
  pub gLs: String,
  pub gLd: String,
  pub gNa: String,
  pub gKdr: String,
  pub gCa: String,
  pub gKahp: String,
  pub gKC: String,
  pub gNmda: String,
  pub gAmpa: String,
  pub eNa: String,
  pub eCa: String,
  pub eK: String,
  pub eL: String,
  pub qd0: String,
  pub pp: String,
  pub alphac: String,
  pub betac: String,
  pub cm: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<PinskyRinzelCA3CellBody>
}

impl XML for PinskyRinzelCA3Cell {
  fn from_node(node: &Node) -> Self {
    let iSoma = node.attribute("iSoma").map(|s| s.to_string()).unwrap();
    let iDend = node.attribute("iDend").map(|s| s.to_string()).unwrap();
    let gc = node.attribute("gc").map(|s| s.to_string()).unwrap();
    let gLs = node.attribute("gLs").map(|s| s.to_string()).unwrap();
    let gLd = node.attribute("gLd").map(|s| s.to_string()).unwrap();
    let gNa = node.attribute("gNa").map(|s| s.to_string()).unwrap();
    let gKdr = node.attribute("gKdr").map(|s| s.to_string()).unwrap();
    let gCa = node.attribute("gCa").map(|s| s.to_string()).unwrap();
    let gKahp = node.attribute("gKahp").map(|s| s.to_string()).unwrap();
    let gKC = node.attribute("gKC").map(|s| s.to_string()).unwrap();
    let gNmda = node.attribute("gNmda").map(|s| s.to_string()).unwrap();
    let gAmpa = node.attribute("gAmpa").map(|s| s.to_string()).unwrap();
    let eNa = node.attribute("eNa").map(|s| s.to_string()).unwrap();
    let eCa = node.attribute("eCa").map(|s| s.to_string()).unwrap();
    let eK = node.attribute("eK").map(|s| s.to_string()).unwrap();
    let eL = node.attribute("eL").map(|s| s.to_string()).unwrap();
    let qd0 = node.attribute("qd0").map(|s| s.to_string()).unwrap();
    let pp = node.attribute("pp").map(|s| s.to_string()).unwrap();
    let alphac = node.attribute("alphac").map(|s| s.to_string()).unwrap();
    let betac = node.attribute("betac").map(|s| s.to_string()).unwrap();
    let cm = node.attribute("cm").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(PinskyRinzelCA3CellBody::notes(String::from_node(&child))),
        "property" => body.push(PinskyRinzelCA3CellBody::property(Property::from_node(&child))),
        "annotation" => body.push(PinskyRinzelCA3CellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of PinskyRinzelCA3Cell.", t)
      };
    }
    PinskyRinzelCA3Cell { iSoma, iDend, gc, gLs, gLd, gNa, gKdr, gCa, gKahp, gKC, gNmda, gAmpa, eNa, eCa, eK, eL, qd0, pp, alphac, betac, cm, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ClosedState {
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for ClosedState {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ClosedState { id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateHHRatesTauInfBody {
  notes(String),
  q10Settings(Q10Settings),
  forwardRate(HHRate),
  reverseRate(HHRate),
  timeCourse(HHTime),
  steadyState(HHVariable),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateHHRatesTauInf {
  pub instances: i64,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateHHRatesTauInfBody>
}

impl XML for GateHHRatesTauInf {
  fn from_node(node: &Node) -> Self {
    let instances = node.attribute("instances").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateHHRatesTauInfBody::notes(String::from_node(&child))),
        "q10Settings" => body.push(GateHHRatesTauInfBody::q10Settings(Q10Settings::from_node(&child))),
        "forwardRate" => body.push(GateHHRatesTauInfBody::forwardRate(HHRate::from_node(&child))),
        "reverseRate" => body.push(GateHHRatesTauInfBody::reverseRate(HHRate::from_node(&child))),
        "timeCourse" => body.push(GateHHRatesTauInfBody::timeCourse(HHTime::from_node(&child))),
        "steadyState" => body.push(GateHHRatesTauInfBody::steadyState(HHVariable::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateHHRatesTauInf.", t)
      };
    }
    GateHHRatesTauInf { instances, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DistalDetails {
  pub normalizationEnd: f64,
}

impl XML for DistalDetails {
  fn from_node(node: &Node) -> Self {
    let normalizationEnd = node.attribute("normalizationEnd").map(|s| s.parse::<f64>().unwrap()).unwrap();
    DistalDetails { normalizationEnd }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TimeDerivative {
  pub variable: String,
  pub value: String,
}

impl XML for TimeDerivative {
  fn from_node(node: &Node) -> Self {
    let variable = node.attribute("variable").map(|s| s.to_string()).unwrap();
    let value = node.attribute("value").map(|s| s.to_string()).unwrap();
    TimeDerivative { variable, value }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpCurrSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpCurrSynapse {
  pub tau_syn: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ExpCurrSynapseBody>
}

impl XML for ExpCurrSynapse {
  fn from_node(node: &Node) -> Self {
    let tau_syn = node.attribute("tau_syn").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(ExpCurrSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(ExpCurrSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(ExpCurrSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ExpCurrSynapse.", t)
      };
    }
    ExpCurrSynapse { tau_syn, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReverseTransition {
  pub from: String,
  pub to: String,
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for ReverseTransition {
  fn from_node(node: &Node) -> Self {
    let from = node.attribute("from").map(|s| s.to_string()).unwrap();
    let to = node.attribute("to").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ReverseTransition { from, to, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Base {
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for Base {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    Base { id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IF_cond_alphaBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IF_cond_alpha {
  pub e_rev_E: f64,
  pub e_rev_I: f64,
  pub tau_m: f64,
  pub tau_refrac: f64,
  pub v_reset: f64,
  pub v_rest: f64,
  pub v_thresh: f64,
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IF_cond_alphaBody>
}

impl XML for IF_cond_alpha {
  fn from_node(node: &Node) -> Self {
    let e_rev_E = node.attribute("e_rev_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let e_rev_I = node.attribute("e_rev_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_m = node.attribute("tau_m").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_refrac = node.attribute("tau_refrac").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_reset = node.attribute("v_reset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_rest = node.attribute("v_rest").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_thresh = node.attribute("v_thresh").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(IF_cond_alphaBody::notes(String::from_node(&child))),
        "property" => body.push(IF_cond_alphaBody::property(Property::from_node(&child))),
        "annotation" => body.push(IF_cond_alphaBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IF_cond_alpha.", t)
      };
    }
    IF_cond_alpha { e_rev_E, e_rev_I, tau_m, tau_refrac, v_reset, v_rest, v_thresh, cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpikeThresh {
  pub value: String,
  pub segmentGroup: String,
}

impl XML for SpikeThresh {
  fn from_node(node: &Node) -> Self {
    let value = node.attribute("value").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    SpikeThresh { value, segmentGroup }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NeuroMLDocumentBody {
  include(IncludeType),
  extracellularProperties(ExtracellularProperties),
  intracellularProperties(IntracellularProperties),
  morphology(Morphology),
  ionChannel(IonChannel),
  ionChannelHH(IonChannelHH),
  ionChannelVShift(IonChannelVShift),
  ionChannelKS(IonChannelKS),
  decayingPoolConcentrationModel(DecayingPoolConcentrationModel),
  fixedFactorConcentrationModel(FixedFactorConcentrationModel),
  alphaCurrentSynapse(AlphaCurrentSynapse),
  alphaSynapse(AlphaSynapse),
  expOneSynapse(ExpOneSynapse),
  expTwoSynapse(ExpTwoSynapse),
  expThreeSynapse(ExpThreeSynapse),
  blockingPlasticSynapse(BlockingPlasticSynapse),
  doubleSynapse(DoubleSynapse),
  gapJunction(GapJunction),
  silentSynapse(SilentSynapse),
  linearGradedSynapse(LinearGradedSynapse),
  gradedSynapse(GradedSynapse),
  biophysicalProperties(BiophysicalProperties),
  cell(Cell),
  cell2CaPools(Cell2CaPools),
  baseCell(BaseCell),
  iafTauCell(IafTauCell),
  iafTauRefCell(IafTauRefCell),
  iafCell(IafCell),
  iafRefCell(IafRefCell),
  izhikevichCell(IzhikevichCell),
  izhikevich2007Cell(Izhikevich2007Cell),
  adExIaFCell(AdExIaFCell),
  fitzHughNagumoCell(FitzHughNagumoCell),
  fitzHughNagumo1969Cell(FitzHughNagumo1969Cell),
  pinskyRinzelCA3Cell(PinskyRinzelCA3Cell),
  pulseGenerator(PulseGenerator),
  pulseGeneratorDL(PulseGeneratorDL),
  sineGenerator(SineGenerator),
  sineGeneratorDL(SineGeneratorDL),
  rampGenerator(RampGenerator),
  rampGeneratorDL(RampGeneratorDL),
  compoundInput(CompoundInput),
  compoundInputDL(CompoundInputDL),
  voltageClamp(VoltageClamp),
  voltageClampTriple(VoltageClampTriple),
  spikeArray(SpikeArray),
  timedSynapticInput(TimedSynapticInput),
  spikeGenerator(SpikeGenerator),
  spikeGeneratorRandom(SpikeGeneratorRandom),
  spikeGeneratorPoisson(SpikeGeneratorPoisson),
  spikeGeneratorRefPoisson(SpikeGeneratorRefPoisson),
  poissonFiringSynapse(PoissonFiringSynapse),
  transientPoissonFiringSynapse(TransientPoissonFiringSynapse),
  IF_curr_alpha(IF_curr_alpha),
  IF_curr_exp(IF_curr_exp),
  IF_cond_alpha(IF_cond_alpha),
  IF_cond_exp(IF_cond_exp),
  EIF_cond_exp_isfa_ista(EIF_cond_exp_isfa_ista),
  EIF_cond_alpha_isfa_ista(EIF_cond_alpha_isfa_ista),
  HH_cond_exp(HH_cond_exp),
  expCondSynapse(ExpCondSynapse),
  alphaCondSynapse(AlphaCondSynapse),
  expCurrSynapse(ExpCurrSynapse),
  alphaCurrSynapse(AlphaCurrSynapse),
  SpikeSourcePoisson(SpikeSourcePoisson),
  network(Network),
  ComponentType(ComponentType),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NeuroMLDocument {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<NeuroMLDocumentBody>
}

impl XML for NeuroMLDocument {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "include" => body.push(NeuroMLDocumentBody::include(IncludeType::from_node(&child))),
        "extracellularProperties" => body.push(NeuroMLDocumentBody::extracellularProperties(ExtracellularProperties::from_node(&child))),
        "intracellularProperties" => body.push(NeuroMLDocumentBody::intracellularProperties(IntracellularProperties::from_node(&child))),
        "morphology" => body.push(NeuroMLDocumentBody::morphology(Morphology::from_node(&child))),
        "ionChannel" => body.push(NeuroMLDocumentBody::ionChannel(IonChannel::from_node(&child))),
        "ionChannelHH" => body.push(NeuroMLDocumentBody::ionChannelHH(IonChannelHH::from_node(&child))),
        "ionChannelVShift" => body.push(NeuroMLDocumentBody::ionChannelVShift(IonChannelVShift::from_node(&child))),
        "ionChannelKS" => body.push(NeuroMLDocumentBody::ionChannelKS(IonChannelKS::from_node(&child))),
        "decayingPoolConcentrationModel" => body.push(NeuroMLDocumentBody::decayingPoolConcentrationModel(DecayingPoolConcentrationModel::from_node(&child))),
        "fixedFactorConcentrationModel" => body.push(NeuroMLDocumentBody::fixedFactorConcentrationModel(FixedFactorConcentrationModel::from_node(&child))),
        "alphaCurrentSynapse" => body.push(NeuroMLDocumentBody::alphaCurrentSynapse(AlphaCurrentSynapse::from_node(&child))),
        "alphaSynapse" => body.push(NeuroMLDocumentBody::alphaSynapse(AlphaSynapse::from_node(&child))),
        "expOneSynapse" => body.push(NeuroMLDocumentBody::expOneSynapse(ExpOneSynapse::from_node(&child))),
        "expTwoSynapse" => body.push(NeuroMLDocumentBody::expTwoSynapse(ExpTwoSynapse::from_node(&child))),
        "expThreeSynapse" => body.push(NeuroMLDocumentBody::expThreeSynapse(ExpThreeSynapse::from_node(&child))),
        "blockingPlasticSynapse" => body.push(NeuroMLDocumentBody::blockingPlasticSynapse(BlockingPlasticSynapse::from_node(&child))),
        "doubleSynapse" => body.push(NeuroMLDocumentBody::doubleSynapse(DoubleSynapse::from_node(&child))),
        "gapJunction" => body.push(NeuroMLDocumentBody::gapJunction(GapJunction::from_node(&child))),
        "silentSynapse" => body.push(NeuroMLDocumentBody::silentSynapse(SilentSynapse::from_node(&child))),
        "linearGradedSynapse" => body.push(NeuroMLDocumentBody::linearGradedSynapse(LinearGradedSynapse::from_node(&child))),
        "gradedSynapse" => body.push(NeuroMLDocumentBody::gradedSynapse(GradedSynapse::from_node(&child))),
        "biophysicalProperties" => body.push(NeuroMLDocumentBody::biophysicalProperties(BiophysicalProperties::from_node(&child))),
        "cell" => body.push(NeuroMLDocumentBody::cell(Cell::from_node(&child))),
        "cell2CaPools" => body.push(NeuroMLDocumentBody::cell2CaPools(Cell2CaPools::from_node(&child))),
        "baseCell" => body.push(NeuroMLDocumentBody::baseCell(BaseCell::from_node(&child))),
        "iafTauCell" => body.push(NeuroMLDocumentBody::iafTauCell(IafTauCell::from_node(&child))),
        "iafTauRefCell" => body.push(NeuroMLDocumentBody::iafTauRefCell(IafTauRefCell::from_node(&child))),
        "iafCell" => body.push(NeuroMLDocumentBody::iafCell(IafCell::from_node(&child))),
        "iafRefCell" => body.push(NeuroMLDocumentBody::iafRefCell(IafRefCell::from_node(&child))),
        "izhikevichCell" => body.push(NeuroMLDocumentBody::izhikevichCell(IzhikevichCell::from_node(&child))),
        "izhikevich2007Cell" => body.push(NeuroMLDocumentBody::izhikevich2007Cell(Izhikevich2007Cell::from_node(&child))),
        "adExIaFCell" => body.push(NeuroMLDocumentBody::adExIaFCell(AdExIaFCell::from_node(&child))),
        "fitzHughNagumoCell" => body.push(NeuroMLDocumentBody::fitzHughNagumoCell(FitzHughNagumoCell::from_node(&child))),
        "fitzHughNagumo1969Cell" => body.push(NeuroMLDocumentBody::fitzHughNagumo1969Cell(FitzHughNagumo1969Cell::from_node(&child))),
        "pinskyRinzelCA3Cell" => body.push(NeuroMLDocumentBody::pinskyRinzelCA3Cell(PinskyRinzelCA3Cell::from_node(&child))),
        "pulseGenerator" => body.push(NeuroMLDocumentBody::pulseGenerator(PulseGenerator::from_node(&child))),
        "pulseGeneratorDL" => body.push(NeuroMLDocumentBody::pulseGeneratorDL(PulseGeneratorDL::from_node(&child))),
        "sineGenerator" => body.push(NeuroMLDocumentBody::sineGenerator(SineGenerator::from_node(&child))),
        "sineGeneratorDL" => body.push(NeuroMLDocumentBody::sineGeneratorDL(SineGeneratorDL::from_node(&child))),
        "rampGenerator" => body.push(NeuroMLDocumentBody::rampGenerator(RampGenerator::from_node(&child))),
        "rampGeneratorDL" => body.push(NeuroMLDocumentBody::rampGeneratorDL(RampGeneratorDL::from_node(&child))),
        "compoundInput" => body.push(NeuroMLDocumentBody::compoundInput(CompoundInput::from_node(&child))),
        "compoundInputDL" => body.push(NeuroMLDocumentBody::compoundInputDL(CompoundInputDL::from_node(&child))),
        "voltageClamp" => body.push(NeuroMLDocumentBody::voltageClamp(VoltageClamp::from_node(&child))),
        "voltageClampTriple" => body.push(NeuroMLDocumentBody::voltageClampTriple(VoltageClampTriple::from_node(&child))),
        "spikeArray" => body.push(NeuroMLDocumentBody::spikeArray(SpikeArray::from_node(&child))),
        "timedSynapticInput" => body.push(NeuroMLDocumentBody::timedSynapticInput(TimedSynapticInput::from_node(&child))),
        "spikeGenerator" => body.push(NeuroMLDocumentBody::spikeGenerator(SpikeGenerator::from_node(&child))),
        "spikeGeneratorRandom" => body.push(NeuroMLDocumentBody::spikeGeneratorRandom(SpikeGeneratorRandom::from_node(&child))),
        "spikeGeneratorPoisson" => body.push(NeuroMLDocumentBody::spikeGeneratorPoisson(SpikeGeneratorPoisson::from_node(&child))),
        "spikeGeneratorRefPoisson" => body.push(NeuroMLDocumentBody::spikeGeneratorRefPoisson(SpikeGeneratorRefPoisson::from_node(&child))),
        "poissonFiringSynapse" => body.push(NeuroMLDocumentBody::poissonFiringSynapse(PoissonFiringSynapse::from_node(&child))),
        "transientPoissonFiringSynapse" => body.push(NeuroMLDocumentBody::transientPoissonFiringSynapse(TransientPoissonFiringSynapse::from_node(&child))),
        "IF_curr_alpha" => body.push(NeuroMLDocumentBody::IF_curr_alpha(IF_curr_alpha::from_node(&child))),
        "IF_curr_exp" => body.push(NeuroMLDocumentBody::IF_curr_exp(IF_curr_exp::from_node(&child))),
        "IF_cond_alpha" => body.push(NeuroMLDocumentBody::IF_cond_alpha(IF_cond_alpha::from_node(&child))),
        "IF_cond_exp" => body.push(NeuroMLDocumentBody::IF_cond_exp(IF_cond_exp::from_node(&child))),
        "EIF_cond_exp_isfa_ista" => body.push(NeuroMLDocumentBody::EIF_cond_exp_isfa_ista(EIF_cond_exp_isfa_ista::from_node(&child))),
        "EIF_cond_alpha_isfa_ista" => body.push(NeuroMLDocumentBody::EIF_cond_alpha_isfa_ista(EIF_cond_alpha_isfa_ista::from_node(&child))),
        "HH_cond_exp" => body.push(NeuroMLDocumentBody::HH_cond_exp(HH_cond_exp::from_node(&child))),
        "expCondSynapse" => body.push(NeuroMLDocumentBody::expCondSynapse(ExpCondSynapse::from_node(&child))),
        "alphaCondSynapse" => body.push(NeuroMLDocumentBody::alphaCondSynapse(AlphaCondSynapse::from_node(&child))),
        "expCurrSynapse" => body.push(NeuroMLDocumentBody::expCurrSynapse(ExpCurrSynapse::from_node(&child))),
        "alphaCurrSynapse" => body.push(NeuroMLDocumentBody::alphaCurrSynapse(AlphaCurrSynapse::from_node(&child))),
        "SpikeSourcePoisson" => body.push(NeuroMLDocumentBody::SpikeSourcePoisson(SpikeSourcePoisson::from_node(&child))),
        "network" => body.push(NeuroMLDocumentBody::network(Network::from_node(&child))),
        "ComponentType" => body.push(NeuroMLDocumentBody::ComponentType(ComponentType::from_node(&child))),
        "notes" => body.push(NeuroMLDocumentBody::notes(String::from_node(&child))),
        "property" => body.push(NeuroMLDocumentBody::property(Property::from_node(&child))),
        "annotation" => body.push(NeuroMLDocumentBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of NeuroMLDocument.", t)
      };
    }
    NeuroMLDocument { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FixedFactorConcentrationModelBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FixedFactorConcentrationModel {
  pub ion: String,
  pub restingConc: String,
  pub decayConstant: String,
  pub rho: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<FixedFactorConcentrationModelBody>
}

impl XML for FixedFactorConcentrationModel {
  fn from_node(node: &Node) -> Self {
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let restingConc = node.attribute("restingConc").map(|s| s.to_string()).unwrap();
    let decayConstant = node.attribute("decayConstant").map(|s| s.to_string()).unwrap();
    let rho = node.attribute("rho").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(FixedFactorConcentrationModelBody::notes(String::from_node(&child))),
        "property" => body.push(FixedFactorConcentrationModelBody::property(Property::from_node(&child))),
        "annotation" => body.push(FixedFactorConcentrationModelBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of FixedFactorConcentrationModel.", t)
      };
    }
    FixedFactorConcentrationModel { ion, restingConc, decayConstant, rho, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum StandaloneBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Standalone {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<StandaloneBody>
}

impl XML for Standalone {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(StandaloneBody::notes(String::from_node(&child))),
        "property" => body.push(StandaloneBody::property(Property::from_node(&child))),
        "annotation" => body.push(StandaloneBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Standalone.", t)
      };
    }
    Standalone { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelDensityGHK {
  pub ionChannel: String,
  pub permeability: String,
  pub segmentGroup: String,
  pub segment: Option<String>,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
}

impl XML for ChannelDensityGHK {
  fn from_node(node: &Node) -> Self {
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let permeability = node.attribute("permeability").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    let segment = node.attribute("segment").map(|s| s.to_string());
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    ChannelDensityGHK { ionChannel, permeability, segmentGroup, segment, ion, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SpecificCapacitance {
  pub value: String,
  pub segmentGroup: String,
}

impl XML for SpecificCapacitance {
  fn from_node(node: &Node) -> Self {
    let value = node.attribute("value").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    SpecificCapacitance { value, segmentGroup }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SpaceBody {
  structure(SpaceStructure),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Space {
  pub basedOn: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SpaceBody>
}

impl XML for Space {
  fn from_node(node: &Node) -> Self {
    let basedOn = node.attribute("basedOn").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "structure" => body.push(SpaceBody::structure(SpaceStructure::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Space.", t)
      };
    }
    Space { basedOn, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Q10Settings {
  pub r#type: String,
  pub fixedQ10: Option<String>,
  pub q10Factor: Option<String>,
  pub experimentalTemp: Option<String>,
}

impl XML for Q10Settings {
  fn from_node(node: &Node) -> Self {
    let r#type = node.attribute("type").map(|s| s.to_string()).unwrap();
    let fixedQ10 = node.attribute("fixedQ10").map(|s| s.to_string());
    let q10Factor = node.attribute("q10Factor").map(|s| s.to_string());
    let experimentalTemp = node.attribute("experimentalTemp").map(|s| s.to_string());
    Q10Settings { r#type, fixedQ10, q10Factor, experimentalTemp }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IafCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IafCell {
  pub leakReversal: String,
  pub thresh: String,
  pub reset: String,
  pub C: String,
  pub leakConductance: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IafCellBody>
}

impl XML for IafCell {
  fn from_node(node: &Node) -> Self {
    let leakReversal = node.attribute("leakReversal").map(|s| s.to_string()).unwrap();
    let thresh = node.attribute("thresh").map(|s| s.to_string()).unwrap();
    let reset = node.attribute("reset").map(|s| s.to_string()).unwrap();
    let C = node.attribute("C").map(|s| s.to_string()).unwrap();
    let leakConductance = node.attribute("leakConductance").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(IafCellBody::notes(String::from_node(&child))),
        "property" => body.push(IafCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(IafCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IafCell.", t)
      };
    }
    IafCell { leakReversal, thresh, reset, C, leakConductance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SynapticConnection {
  pub from: String,
  pub to: String,
  pub synapse: String,
  pub destination: Option<String>,
}

impl XML for SynapticConnection {
  fn from_node(node: &Node) -> Self {
    let from = node.attribute("from").map(|s| s.to_string()).unwrap();
    let to = node.attribute("to").map(|s| s.to_string()).unwrap();
    let synapse = node.attribute("synapse").map(|s| s.to_string()).unwrap();
    let destination = node.attribute("destination").map(|s| s.to_string());
    SynapticConnection { from, to, synapse, destination }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TauInfTransitionBody {
  steadyState(HHVariable),
  timeCourse(HHTime),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TauInfTransition {
  pub from: String,
  pub to: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<TauInfTransitionBody>
}

impl XML for TauInfTransition {
  fn from_node(node: &Node) -> Self {
    let from = node.attribute("from").map(|s| s.to_string()).unwrap();
    let to = node.attribute("to").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "steadyState" => body.push(TauInfTransitionBody::steadyState(HHVariable::from_node(&child))),
        "timeCourse" => body.push(TauInfTransitionBody::timeCourse(HHTime::from_node(&child))),
        t => panic!("Unexpected tag {} in body of TauInfTransition.", t)
      };
    }
    TauInfTransition { from, to, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PoissonFiringSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PoissonFiringSynapse {
  pub averageRate: String,
  pub synapse: String,
  pub spikeTarget: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<PoissonFiringSynapseBody>
}

impl XML for PoissonFiringSynapse {
  fn from_node(node: &Node) -> Self {
    let averageRate = node.attribute("averageRate").map(|s| s.to_string()).unwrap();
    let synapse = node.attribute("synapse").map(|s| s.to_string()).unwrap();
    let spikeTarget = node.attribute("spikeTarget").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(PoissonFiringSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(PoissonFiringSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(PoissonFiringSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of PoissonFiringSynapse.", t)
      };
    }
    PoissonFiringSynapse { averageRate, synapse, spikeTarget, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PlasticityMechanism {
  pub r#type: String,
  pub initReleaseProb: f64,
  pub tauRec: String,
  pub tauFac: Option<String>,
}

impl XML for PlasticityMechanism {
  fn from_node(node: &Node) -> Self {
    let r#type = node.attribute("type").map(|s| s.to_string()).unwrap();
    let initReleaseProb = node.attribute("initReleaseProb").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tauRec = node.attribute("tauRec").map(|s| s.to_string()).unwrap();
    let tauFac = node.attribute("tauFac").map(|s| s.to_string());
    PlasticityMechanism { r#type, initReleaseProb, tauRec, tauFac }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Input {
  pub id: i64,
  pub target: String,
  pub destination: String,
  pub segmentId: Option<i64>,
  pub fractionAlong: Option<f64>,
}

impl XML for Input {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let target = node.attribute("target").map(|s| s.to_string()).unwrap();
    let destination = node.attribute("destination").map(|s| s.to_string()).unwrap();
    let segmentId = node.attribute("segmentId").map(|s| s.parse::<i64>().unwrap());
    let fractionAlong = node.attribute("fractionAlong").map(|s| s.parse::<f64>().unwrap());
    Input { id, target, destination, segmentId, fractionAlong }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpCondSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpCondSynapse {
  pub e_rev: f64,
  pub tau_syn: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ExpCondSynapseBody>
}

impl XML for ExpCondSynapse {
  fn from_node(node: &Node) -> Self {
    let e_rev = node.attribute("e_rev").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn = node.attribute("tau_syn").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(ExpCondSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(ExpCondSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(ExpCondSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ExpCondSynapse.", t)
      };
    }
    ExpCondSynapse { e_rev, tau_syn, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum PathBody {
  from(SegmentEndPoint),
  to(SegmentEndPoint),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Path {
  pub body: Vec<PathBody>
}

impl XML for Path {
  fn from_node(node: &Node) -> Self {
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "from" => body.push(PathBody::from(SegmentEndPoint::from_node(&child))),
        "to" => body.push(PathBody::to(SegmentEndPoint::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Path.", t)
      };
    }
    Path { body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DynamicsBody {
  StateVariable(StateVariable),
  DerivedVariable(DerivedVariable),
  ConditionalDerivedVariable(ConditionalDerivedVariable),
  TimeDerivative(TimeDerivative),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Dynamics {
  pub body: Vec<DynamicsBody>
}

impl XML for Dynamics {
  fn from_node(node: &Node) -> Self {
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "StateVariable" => body.push(DynamicsBody::StateVariable(StateVariable::from_node(&child))),
        "DerivedVariable" => body.push(DynamicsBody::DerivedVariable(DerivedVariable::from_node(&child))),
        "ConditionalDerivedVariable" => body.push(DynamicsBody::ConditionalDerivedVariable(ConditionalDerivedVariable::from_node(&child))),
        "TimeDerivative" => body.push(DynamicsBody::TimeDerivative(TimeDerivative::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Dynamics.", t)
      };
    }
    Dynamics { body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateHHRatesTauBody {
  notes(String),
  q10Settings(Q10Settings),
  forwardRate(HHRate),
  reverseRate(HHRate),
  timeCourse(HHTime),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateHHRatesTau {
  pub instances: i64,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateHHRatesTauBody>
}

impl XML for GateHHRatesTau {
  fn from_node(node: &Node) -> Self {
    let instances = node.attribute("instances").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateHHRatesTauBody::notes(String::from_node(&child))),
        "q10Settings" => body.push(GateHHRatesTauBody::q10Settings(Q10Settings::from_node(&child))),
        "forwardRate" => body.push(GateHHRatesTauBody::forwardRate(HHRate::from_node(&child))),
        "reverseRate" => body.push(GateHHRatesTauBody::reverseRate(HHRate::from_node(&child))),
        "timeCourse" => body.push(GateHHRatesTauBody::timeCourse(HHTime::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateHHRatesTau.", t)
      };
    }
    GateHHRatesTau { instances, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BaseVoltageDepSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseVoltageDepSynapse {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BaseVoltageDepSynapseBody>
}

impl XML for BaseVoltageDepSynapse {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(BaseVoltageDepSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(BaseVoltageDepSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(BaseVoltageDepSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BaseVoltageDepSynapse.", t)
      };
    }
    BaseVoltageDepSynapse { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
  pub name: String,
  pub dimension: String,
  pub description: Option<String>,
}

impl XML for Parameter {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    Parameter { name, dimension, description }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum GateFractionalBody {
  notes(String),
  q10Settings(Q10Settings),
  subGate(GateFractionalSubgate),
}

#[derive(Debug, Clone, PartialEq)]
pub struct GateFractional {
  pub instances: i64,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<GateFractionalBody>
}

impl XML for GateFractional {
  fn from_node(node: &Node) -> Self {
    let instances = node.attribute("instances").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(GateFractionalBody::notes(String::from_node(&child))),
        "q10Settings" => body.push(GateFractionalBody::q10Settings(Q10Settings::from_node(&child))),
        "subGate" => body.push(GateFractionalBody::subGate(GateFractionalSubgate::from_node(&child))),
        t => panic!("Unexpected tag {} in body of GateFractional.", t)
      };
    }
    GateFractional { instances, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct NamedDimensionalVariable {
  pub name: String,
  pub dimension: String,
  pub description: Option<String>,
  pub exposure: Option<String>,
}

impl XML for NamedDimensionalVariable {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    let exposure = node.attribute("exposure").map(|s| s.to_string());
    NamedDimensionalVariable { name, dimension, description, exposure }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TransientPoissonFiringSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct TransientPoissonFiringSynapse {
  pub averageRate: String,
  pub delay: String,
  pub duration: String,
  pub synapse: String,
  pub spikeTarget: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<TransientPoissonFiringSynapseBody>
}

impl XML for TransientPoissonFiringSynapse {
  fn from_node(node: &Node) -> Self {
    let averageRate = node.attribute("averageRate").map(|s| s.to_string()).unwrap();
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let synapse = node.attribute("synapse").map(|s| s.to_string()).unwrap();
    let spikeTarget = node.attribute("spikeTarget").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(TransientPoissonFiringSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(TransientPoissonFiringSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(TransientPoissonFiringSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of TransientPoissonFiringSynapse.", t)
      };
    }
    TransientPoissonFiringSynapse { averageRate, delay, duration, synapse, spikeTarget, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BasePynnSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasePynnSynapse {
  pub tau_syn: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<BasePynnSynapseBody>
}

impl XML for BasePynnSynapse {
  fn from_node(node: &Node) -> Self {
    let tau_syn = node.attribute("tau_syn").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(BasePynnSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(BasePynnSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(BasePynnSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of BasePynnSynapse.", t)
      };
    }
    BasePynnSynapse { tau_syn, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HHVariable {
  pub r#type: String,
  pub rate: Option<f64>,
  pub midpoint: Option<String>,
  pub scale: Option<String>,
}

impl XML for HHVariable {
  fn from_node(node: &Node) -> Self {
    let r#type = node.attribute("type").map(|s| s.to_string()).unwrap();
    let rate = node.attribute("rate").map(|s| s.parse::<f64>().unwrap());
    let midpoint = node.attribute("midpoint").map(|s| s.to_string());
    let scale = node.attribute("scale").map(|s| s.to_string());
    HHVariable { r#type, rate, midpoint, scale }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AlphaCurrentSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AlphaCurrentSynapse {
  pub tau: String,
  pub ibase: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<AlphaCurrentSynapseBody>
}

impl XML for AlphaCurrentSynapse {
  fn from_node(node: &Node) -> Self {
    let tau = node.attribute("tau").map(|s| s.to_string()).unwrap();
    let ibase = node.attribute("ibase").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(AlphaCurrentSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(AlphaCurrentSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(AlphaCurrentSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of AlphaCurrentSynapse.", t)
      };
    }
    AlphaCurrentSynapse { tau, ibase, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SegmentGroupBody {
  notes(String),
  property(Property),
  annotation(Annotation),
  member(Member),
  include(Include),
  path(Path),
  subTree(SubTree),
  inhomogeneousParameter(InhomogeneousParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SegmentGroup {
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SegmentGroupBody>
}

impl XML for SegmentGroup {
  fn from_node(node: &Node) -> Self {
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(SegmentGroupBody::notes(String::from_node(&child))),
        "property" => body.push(SegmentGroupBody::property(Property::from_node(&child))),
        "annotation" => body.push(SegmentGroupBody::annotation(Annotation::from_node(&child))),
        "member" => body.push(SegmentGroupBody::member(Member::from_node(&child))),
        "include" => body.push(SegmentGroupBody::include(Include::from_node(&child))),
        "path" => body.push(SegmentGroupBody::path(Path::from_node(&child))),
        "subTree" => body.push(SegmentGroupBody::subTree(SubTree::from_node(&child))),
        "inhomogeneousParameter" => body.push(SegmentGroupBody::inhomogeneousParameter(InhomogeneousParameter::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SegmentGroup.", t)
      };
    }
    SegmentGroup { id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum basePyNNIaFCellBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct basePyNNIaFCell {
  pub tau_m: f64,
  pub tau_refrac: f64,
  pub v_reset: f64,
  pub v_rest: f64,
  pub v_thresh: f64,
  pub cm: f64,
  pub i_offset: f64,
  pub tau_syn_E: f64,
  pub tau_syn_I: f64,
  pub v_init: f64,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<basePyNNIaFCellBody>
}

impl XML for basePyNNIaFCell {
  fn from_node(node: &Node) -> Self {
    let tau_m = node.attribute("tau_m").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_refrac = node.attribute("tau_refrac").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_reset = node.attribute("v_reset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_rest = node.attribute("v_rest").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_thresh = node.attribute("v_thresh").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let cm = node.attribute("cm").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let i_offset = node.attribute("i_offset").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_E = node.attribute("tau_syn_E").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let tau_syn_I = node.attribute("tau_syn_I").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let v_init = node.attribute("v_init").map(|s| s.parse::<f64>().unwrap()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(basePyNNIaFCellBody::notes(String::from_node(&child))),
        "property" => body.push(basePyNNIaFCellBody::property(Property::from_node(&child))),
        "annotation" => body.push(basePyNNIaFCellBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of basePyNNIaFCell.", t)
      };
    }
    basePyNNIaFCell { tau_m, tau_refrac, v_reset, v_rest, v_thresh, cm, i_offset, tau_syn_E, tau_syn_I, v_init, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Cell2CaPoolsBody {
  biophysicalProperties2CaPools(BiophysicalProperties2CaPools),
  morphology(Morphology),
  biophysicalProperties(BiophysicalProperties),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cell2CaPools {
  pub morphology: Option<String>,
  pub biophysicalProperties: Option<String>,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<Cell2CaPoolsBody>
}

impl XML for Cell2CaPools {
  fn from_node(node: &Node) -> Self {
    let morphology = node.attribute("morphology").map(|s| s.to_string());
    let biophysicalProperties = node.attribute("biophysicalProperties").map(|s| s.to_string());
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "biophysicalProperties2CaPools" => body.push(Cell2CaPoolsBody::biophysicalProperties2CaPools(BiophysicalProperties2CaPools::from_node(&child))),
        "morphology" => body.push(Cell2CaPoolsBody::morphology(Morphology::from_node(&child))),
        "biophysicalProperties" => body.push(Cell2CaPoolsBody::biophysicalProperties(BiophysicalProperties::from_node(&child))),
        "notes" => body.push(Cell2CaPoolsBody::notes(String::from_node(&child))),
        "property" => body.push(Cell2CaPoolsBody::property(Property::from_node(&child))),
        "annotation" => body.push(Cell2CaPoolsBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Cell2CaPools.", t)
      };
    }
    Cell2CaPools { morphology, biophysicalProperties, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AlphaSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AlphaSynapse {
  pub tau: String,
  pub gbase: String,
  pub erev: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<AlphaSynapseBody>
}

impl XML for AlphaSynapse {
  fn from_node(node: &Node) -> Self {
    let tau = node.attribute("tau").map(|s| s.to_string()).unwrap();
    let gbase = node.attribute("gbase").map(|s| s.to_string()).unwrap();
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(AlphaSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(AlphaSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(AlphaSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of AlphaSynapse.", t)
      };
    }
    AlphaSynapse { tau, gbase, erev, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SilentSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SilentSynapse {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SilentSynapseBody>
}

impl XML for SilentSynapse {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(SilentSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(SilentSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(SilentSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SilentSynapse.", t)
      };
    }
    SilentSynapse { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MorphologyBody {
  segment(Segment),
  segmentGroup(SegmentGroup),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Morphology {
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<MorphologyBody>
}

impl XML for Morphology {
  fn from_node(node: &Node) -> Self {
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "segment" => body.push(MorphologyBody::segment(Segment::from_node(&child))),
        "segmentGroup" => body.push(MorphologyBody::segmentGroup(SegmentGroup::from_node(&child))),
        "notes" => body.push(MorphologyBody::notes(String::from_node(&child))),
        "property" => body.push(MorphologyBody::property(Property::from_node(&child))),
        "annotation" => body.push(MorphologyBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of Morphology.", t)
      };
    }
    Morphology { metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ConditionalDerivedVariableBody {
  Case(Case),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalDerivedVariable {
  pub name: String,
  pub dimension: String,
  pub description: Option<String>,
  pub exposure: Option<String>,
  pub body: Vec<ConditionalDerivedVariableBody>
}

impl XML for ConditionalDerivedVariable {
  fn from_node(node: &Node) -> Self {
    let name = node.attribute("name").map(|s| s.to_string()).unwrap();
    let dimension = node.attribute("dimension").map(|s| s.to_string()).unwrap();
    let description = node.attribute("description").map(|s| s.to_string());
    let exposure = node.attribute("exposure").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "Case" => body.push(ConditionalDerivedVariableBody::Case(Case::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ConditionalDerivedVariable.", t)
      };
    }
    ConditionalDerivedVariable { name, dimension, description, exposure, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BaseConnectionOldFormat {
  pub preCellId: String,
  pub preSegmentId: i64,
  pub preFractionAlong: f64,
  pub postCellId: String,
  pub postSegmentId: i64,
  pub postFractionAlong: f64,
  pub id: i64,
  pub neuroLexId: Option<String>,
}

impl XML for BaseConnectionOldFormat {
  fn from_node(node: &Node) -> Self {
    let preCellId = node.attribute("preCellId").map(|s| s.to_string()).unwrap();
    let preSegmentId = node.attribute("preSegmentId").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let preFractionAlong = node.attribute("preFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let postCellId = node.attribute("postCellId").map(|s| s.to_string()).unwrap();
    let postSegmentId = node.attribute("postSegmentId").or(Some("0")).map(|s| s.parse::<i64>().unwrap()).unwrap();
    let postFractionAlong = node.attribute("postFractionAlong").or(Some("0.5")).map(|s| s.parse::<f64>().unwrap()).unwrap();
    let id = node.attribute("id").map(|s| s.parse::<i64>().unwrap()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    BaseConnectionOldFormat { preCellId, preSegmentId, preFractionAlong, postCellId, postSegmentId, postFractionAlong, id, neuroLexId }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HHTime {
  pub r#type: String,
  pub rate: Option<String>,
  pub midpoint: Option<String>,
  pub scale: Option<String>,
  pub tau: Option<String>,
}

impl XML for HHTime {
  fn from_node(node: &Node) -> Self {
    let r#type = node.attribute("type").map(|s| s.to_string()).unwrap();
    let rate = node.attribute("rate").map(|s| s.to_string());
    let midpoint = node.attribute("midpoint").map(|s| s.to_string());
    let scale = node.attribute("scale").map(|s| s.to_string());
    let tau = node.attribute("tau").map(|s| s.to_string());
    HHTime { r#type, rate, midpoint, scale, tau }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
  pub segment: i64,
}

impl XML for Member {
  fn from_node(node: &Node) -> Self {
    let segment = node.attribute("segment").map(|s| s.parse::<i64>().unwrap()).unwrap();
    Member { segment }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SubTreeBody {
  from(SegmentEndPoint),
  to(SegmentEndPoint),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SubTree {
  pub body: Vec<SubTreeBody>
}

impl XML for SubTree {
  fn from_node(node: &Node) -> Self {
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "from" => body.push(SubTreeBody::from(SegmentEndPoint::from_node(&child))),
        "to" => body.push(SubTreeBody::to(SegmentEndPoint::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SubTree.", t)
      };
    }
    SubTree { body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum MembraneProperties2CaPoolsBody {
  channelDensityNernstCa2(ChannelDensityNernstCa2),
  channelPopulation(ChannelPopulation),
  channelDensity(ChannelDensity),
  channelDensityVShift(ChannelDensityVShift),
  channelDensityNernst(ChannelDensityNernst),
  channelDensityGHK(ChannelDensityGHK),
  channelDensityGHK2(ChannelDensityGHK2),
  channelDensityNonUniform(ChannelDensityNonUniform),
  channelDensityNonUniformNernst(ChannelDensityNonUniformNernst),
  channelDensityNonUniformGHK(ChannelDensityNonUniformGHK),
  spikeThresh(SpikeThresh),
  specificCapacitance(SpecificCapacitance),
  initMembPotential(InitMembPotential),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MembraneProperties2CaPools {
  pub body: Vec<MembraneProperties2CaPoolsBody>
}

impl XML for MembraneProperties2CaPools {
  fn from_node(node: &Node) -> Self {
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "channelDensityNernstCa2" => body.push(MembraneProperties2CaPoolsBody::channelDensityNernstCa2(ChannelDensityNernstCa2::from_node(&child))),
        "channelPopulation" => body.push(MembraneProperties2CaPoolsBody::channelPopulation(ChannelPopulation::from_node(&child))),
        "channelDensity" => body.push(MembraneProperties2CaPoolsBody::channelDensity(ChannelDensity::from_node(&child))),
        "channelDensityVShift" => body.push(MembraneProperties2CaPoolsBody::channelDensityVShift(ChannelDensityVShift::from_node(&child))),
        "channelDensityNernst" => body.push(MembraneProperties2CaPoolsBody::channelDensityNernst(ChannelDensityNernst::from_node(&child))),
        "channelDensityGHK" => body.push(MembraneProperties2CaPoolsBody::channelDensityGHK(ChannelDensityGHK::from_node(&child))),
        "channelDensityGHK2" => body.push(MembraneProperties2CaPoolsBody::channelDensityGHK2(ChannelDensityGHK2::from_node(&child))),
        "channelDensityNonUniform" => body.push(MembraneProperties2CaPoolsBody::channelDensityNonUniform(ChannelDensityNonUniform::from_node(&child))),
        "channelDensityNonUniformNernst" => body.push(MembraneProperties2CaPoolsBody::channelDensityNonUniformNernst(ChannelDensityNonUniformNernst::from_node(&child))),
        "channelDensityNonUniformGHK" => body.push(MembraneProperties2CaPoolsBody::channelDensityNonUniformGHK(ChannelDensityNonUniformGHK::from_node(&child))),
        "spikeThresh" => body.push(MembraneProperties2CaPoolsBody::spikeThresh(SpikeThresh::from_node(&child))),
        "specificCapacitance" => body.push(MembraneProperties2CaPoolsBody::specificCapacitance(SpecificCapacitance::from_node(&child))),
        "initMembPotential" => body.push(MembraneProperties2CaPoolsBody::initMembPotential(InitMembPotential::from_node(&child))),
        t => panic!("Unexpected tag {} in body of MembraneProperties2CaPools.", t)
      };
    }
    MembraneProperties2CaPools { body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ChannelDensityBody {
  variableParameter(VariableParameter),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ChannelDensity {
  pub ionChannel: String,
  pub condDensity: Option<String>,
  pub erev: String,
  pub segmentGroup: String,
  pub segment: Option<i64>,
  pub ion: String,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<ChannelDensityBody>
}

impl XML for ChannelDensity {
  fn from_node(node: &Node) -> Self {
    let ionChannel = node.attribute("ionChannel").map(|s| s.to_string()).unwrap();
    let condDensity = node.attribute("condDensity").map(|s| s.to_string());
    let erev = node.attribute("erev").map(|s| s.to_string()).unwrap();
    let segmentGroup = node.attribute("segmentGroup").or(Some("all")).map(|s| s.to_string()).unwrap();
    let segment = node.attribute("segment").map(|s| s.parse::<i64>().unwrap());
    let ion = node.attribute("ion").map(|s| s.to_string()).unwrap();
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "variableParameter" => body.push(ChannelDensityBody::variableParameter(VariableParameter::from_node(&child))),
        t => panic!("Unexpected tag {} in body of ChannelDensity.", t)
      };
    }
    ChannelDensity { ionChannel, condDensity, erev, segmentGroup, segment, ion, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProximalDetails {
  pub translationStart: f64,
}

impl XML for ProximalDetails {
  fn from_node(node: &Node) -> Self {
    let translationStart = node.attribute("translationStart").map(|s| s.parse::<f64>().unwrap()).unwrap();
    ProximalDetails { translationStart }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IonChannelKSBody {
  gateKS(GateKS),
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IonChannelKS {
  pub species: Option<String>,
  pub conductance: Option<String>,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<IonChannelKSBody>
}

impl XML for IonChannelKS {
  fn from_node(node: &Node) -> Self {
    let species = node.attribute("species").map(|s| s.to_string());
    let conductance = node.attribute("conductance").map(|s| s.to_string());
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "gateKS" => body.push(IonChannelKSBody::gateKS(GateKS::from_node(&child))),
        "notes" => body.push(IonChannelKSBody::notes(String::from_node(&child))),
        "property" => body.push(IonChannelKSBody::property(Property::from_node(&child))),
        "annotation" => body.push(IonChannelKSBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of IonChannelKS.", t)
      };
    }
    IonChannelKS { species, conductance, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SegmentEndPoint {
  pub segment: i64,
}

impl XML for SegmentEndPoint {
  fn from_node(node: &Node) -> Self {
    let segment = node.attribute("segment").map(|s| s.parse::<i64>().unwrap()).unwrap();
    SegmentEndPoint { segment }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DoubleSynapseBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DoubleSynapse {
  pub synapse1: String,
  pub synapse2: String,
  pub synapse1Path: String,
  pub synapse2Path: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<DoubleSynapseBody>
}

impl XML for DoubleSynapse {
  fn from_node(node: &Node) -> Self {
    let synapse1 = node.attribute("synapse1").map(|s| s.to_string()).unwrap();
    let synapse2 = node.attribute("synapse2").map(|s| s.to_string()).unwrap();
    let synapse1Path = node.attribute("synapse1Path").map(|s| s.to_string()).unwrap();
    let synapse2Path = node.attribute("synapse2Path").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(DoubleSynapseBody::notes(String::from_node(&child))),
        "property" => body.push(DoubleSynapseBody::property(Property::from_node(&child))),
        "annotation" => body.push(DoubleSynapseBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of DoubleSynapse.", t)
      };
    }
    DoubleSynapse { synapse1, synapse2, synapse1Path, synapse2Path, metaid, id, neuroLexId, body }
  }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SineGeneratorBody {
  notes(String),
  property(Property),
  annotation(Annotation),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SineGenerator {
  pub delay: String,
  pub phase: String,
  pub duration: String,
  pub amplitude: String,
  pub period: String,
  pub metaid: Option<String>,
  pub id: String,
  pub neuroLexId: Option<String>,
  pub body: Vec<SineGeneratorBody>
}

impl XML for SineGenerator {
  fn from_node(node: &Node) -> Self {
    let delay = node.attribute("delay").map(|s| s.to_string()).unwrap();
    let phase = node.attribute("phase").map(|s| s.to_string()).unwrap();
    let duration = node.attribute("duration").map(|s| s.to_string()).unwrap();
    let amplitude = node.attribute("amplitude").map(|s| s.to_string()).unwrap();
    let period = node.attribute("period").map(|s| s.to_string()).unwrap();
    let metaid = node.attribute("metaid").map(|s| s.to_string());
    let id = node.attribute("id").map(|s| s.to_string()).unwrap();
    let neuroLexId = node.attribute("neuroLexId").map(|s| s.to_string());
    let mut body = Vec::new();
    for child in node.children() {
      if child.is_comment() || child.is_text() { continue; }
      match child.tag_name().name() {
        "notes" => body.push(SineGeneratorBody::notes(String::from_node(&child))),
        "property" => body.push(SineGeneratorBody::property(Property::from_node(&child))),
        "annotation" => body.push(SineGeneratorBody::annotation(Annotation::from_node(&child))),
        t => panic!("Unexpected tag {} in body of SineGenerator.", t)
      };
    }
    SineGenerator { delay, phase, duration, amplitude, period, metaid, id, neuroLexId, body }
  }
}
