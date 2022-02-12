use nml2::{instance::Instance, lems::file::LemsFile, nmodl::to_nmodl};

use roxmltree::Document;

#[test]
fn simple_synapse() {
    let lems = LemsFile::from(
        &[String::from("ext/NeuroML2/NeuroML2CoreTypes")],
        &[String::from("NeuroML2CoreTypes.xml")],
    )
    .unwrap();
    let tree = Document::parse(r#"<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
    id="simple-synapse">
    <expOneSynapse id="sy1" gbase="0.5nS" erev="0mV" tauDecay="3ms" />
</neuroml>
"#).unwrap();
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("expOneSynapse"))
        .unwrap();
    let inst = Instance::new(&lems, &node).unwrap();
    assert_eq!(
        to_nmodl(&inst, "-*").unwrap(),
        r#"NEURON {
  POINT_PROCESS sy1
  NONSPECIFIC_CURRENT i
}

STATE { g }

INITIAL {
  g = 0
}

DERIVATIVE dstate {
  g' = -0.3333333333333333 * g
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  i = -1 * g * v
}

NET_RECEIVE(weight) {
  g = g + 0.0005 * weight
}

"#
    );
    assert_eq!(
        to_nmodl(&inst, "+*").unwrap(),
        r#"NEURON {
  POINT_PROCESS sy1
  NONSPECIFIC_CURRENT i
  RANGE erev, gbase, tauDecay
}

PARAMETER {
  erev = 0 (mV)
  gbase = 0.0005 (uS)
  tauDecay = 3 (ms)
}

STATE { g }

INITIAL {
  g = 0
}

DERIVATIVE dstate {
  g' = -1 * g * tauDecay^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  i = g * (erev + -1 * v)
}

NET_RECEIVE(weight) {
  g = g + gbase * weight
}

"#
    );
}

#[test]
fn simple_gap_junction() {
    let lems = LemsFile::from(
        &[String::from("ext/NeuroML2/NeuroML2CoreTypes")],
        &[String::from("NeuroML2CoreTypes.xml")],
    )
    .unwrap();
    let tree = Document::parse(r#"<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
    id="simple-gap-junction">
    <gapJunction id="gj1" conductance="10pS"/>
</neuroml>"#).unwrap();
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("gapJunction"))
        .unwrap();
    let inst = Instance::new(&lems, &node).unwrap();
    assert_eq!(
        to_nmodl(&inst, "-*").unwrap(),
        r#"NEURON {
  JUNCTION gj1
  NONSPECIFIC_CURRENT i
  RANGE conductance, weight
}

PARAMETER {
  conductance = 0.00001 (uS)
  weight = 1
}

BREAKPOINT {
  i = conductance * weight * (v_peer + -1 * v)
}

"#
    );
    assert_eq!(
        to_nmodl(&inst, "+*").unwrap(),
        r#"NEURON {
  JUNCTION gj1
  NONSPECIFIC_CURRENT i
  RANGE conductance, weight
}

PARAMETER {
  conductance = 0.00001 (uS)
  weight = 1
}

BREAKPOINT {
  i = conductance * weight * (v_peer + -1 * v)
}

"#
    );
}
