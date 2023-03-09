use nml2::{instance::Instance, lems::file::LemsFile, nmodl::to_nmodl};

use roxmltree::Document;

use pretty_assertions::assert_eq;

fn ions() -> Vec<String> {
    vec![String::from("na"), String::from("ca"), String::from("k")]
}

#[test]
fn simple_synapse() {
    let lems = LemsFile::core();
    let tree = Document::parse(r#"<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
      xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
    id="simple-synapse">
    <expOneSynapse id="sy1" gbase="0.5nS" erev="0mV" />
</neuroml>
"#).unwrap();
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("expOneSynapse"))
        .unwrap();
    let inst = Instance::new(&lems, &node).unwrap();
    assert_eq!(
        to_nmodl(&inst, "-*", "baseSynapse", &ions()).unwrap(),
        r#"NEURON {
  POINT_PROCESS sy1
  NONSPECIFIC_CURRENT i
  RANGE tauDecay
}

PARAMETER {
  tauDecay
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
  i = g * v
}

NET_RECEIVE(weight) {
  g = g + 0.0005 * weight
}

"#
    );
    assert_eq!(
        to_nmodl(&inst, "+*", "baseSynapse", &ions()).unwrap(),
        r#"NEURON {
  POINT_PROCESS sy1
  NONSPECIFIC_CURRENT i
  RANGE erev, gbase, tauDecay
}

PARAMETER {
  erev = 0 (mV)
  gbase = 0.0005 (uS)
  tauDecay
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
  i = -1 * g * (erev + -1 * v)
}

NET_RECEIVE(weight) {
  g = g + gbase * weight
}

"#
    );
}

#[test]
fn simple_gap_junction() {
    let lems = LemsFile::core();
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
        to_nmodl(&inst, "-*", "baseSynapse", &ions()).unwrap(),
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
        to_nmodl(&inst, "+*", "baseSynapse", &ions()).unwrap(),
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

#[test]
fn simple_ion_channel() {
    let lems = LemsFile::core();
    let tree = Document::parse(r#"<?xml version="1.0" encoding="UTF-8"?>

<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2  ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
         id="NML2_SimpleIonChannel">

    <!-- Example of a simple Na+ ion channel in NeuroML 2 -->

    <ionChannelHH id="NaConductance" conductance="10pS" species="na">
        <gateHHrates id="m" instances="3">
            <forwardRate type="HHExpLinearRate" rate="1per_ms" midpoint="-40mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="4per_ms" midpoint="-65mV" scale="-18mV"/>
        </gateHHrates>

        <gateHHrates id="h" instances="1">
            <forwardRate type="HHExpRate" rate="0.07per_ms" midpoint="-65mV" scale="-20mV"/>
            <reverseRate type="HHSigmoidRate" rate="1per_ms" midpoint="-35mV" scale="10mV"/>
        </gateHHrates>
    </ionChannelHH>

</neuroml>"#).unwrap();
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("ionChannelHH"))
        .unwrap();
    let inst = Instance::new(&lems, &node).unwrap();
    assert_eq!(
        to_nmodl(&inst, "+*", "baseIonChannel", &ions()).unwrap(),
        r#"NEURON {
  SUFFIX NaConductance
  USEION na WRITE ina READ ena
  RANGE conductance, gates_h_forwardRate_midpoint, gates_h_forwardRate_rate, gates_h_forwardRate_scale, gates_h_instances, gates_h_reverseRate_midpoint, gates_h_reverseRate_rate, gates_h_reverseRate_scale, gates_m_forwardRate_midpoint, gates_m_forwardRate_rate, gates_m_forwardRate_scale, gates_m_instances, gates_m_reverseRate_midpoint, gates_m_reverseRate_rate, gates_m_reverseRate_scale
}

PARAMETER {
  conductance = 0.00001 (uS)
  gates_h_forwardRate_midpoint = -65 (mV)
  gates_h_forwardRate_rate = 0.07000000029802322 (per_ms)
  gates_h_forwardRate_scale = -20 (mV)
  gates_h_instances = 1
  gates_h_reverseRate_midpoint = -35 (mV)
  gates_h_reverseRate_rate = 1 (per_ms)
  gates_h_reverseRate_scale = 10 (mV)
  gates_m_forwardRate_midpoint = -40 (mV)
  gates_m_forwardRate_rate = 1 (per_ms)
  gates_m_forwardRate_scale = 10 (mV)
  gates_m_instances = 3
  gates_m_reverseRate_midpoint = -65 (mV)
  gates_m_reverseRate_rate = 4 (per_ms)
  gates_m_reverseRate_scale = -18 (mV)
}

STATE { gates_h_q gates_m_q }

INITIAL {
  LOCAL gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r, gates_m_inf, gates_h_forwardRate_r, gates_h_reverseRate_r, gates_h_inf

  gates_m_reverseRate_r = gates_m_reverseRate_rate * exp((v + -1 * gates_m_reverseRate_midpoint) * gates_m_reverseRate_scale^-1)
  gates_m_forwardRate_x = (v + -1 * gates_m_forwardRate_midpoint) * gates_m_forwardRate_scale^-1
  if (gates_m_forwardRate_x != 0) {
    gates_m_forwardRate_r = gates_m_forwardRate_rate * gates_m_forwardRate_x * (1 + -1 * exp(-1 * gates_m_forwardRate_x))^-1
  } else {
    gates_m_forwardRate_r = gates_m_forwardRate_rate
  }
  gates_m_inf = gates_m_forwardRate_r * (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_h_forwardRate_r = gates_h_forwardRate_rate * exp((v + -1 * gates_h_forwardRate_midpoint) * gates_h_forwardRate_scale^-1)
  gates_h_reverseRate_r = gates_h_reverseRate_rate * (1 + exp(-1 * (v + -1 * gates_h_reverseRate_midpoint) * gates_h_reverseRate_scale^-1))^-1
  gates_h_inf = gates_h_forwardRate_r * (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_q = gates_h_inf
  gates_m_q = gates_m_inf
}

DERIVATIVE dstate {
  LOCAL gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r, gates_m_inf, gates_m_tau, gates_h_forwardRate_r, gates_h_reverseRate_r, gates_h_inf, gates_h_tau

  gates_m_reverseRate_r = gates_m_reverseRate_rate * exp((v + -1 * gates_m_reverseRate_midpoint) * gates_m_reverseRate_scale^-1)
  gates_m_forwardRate_x = (v + -1 * gates_m_forwardRate_midpoint) * gates_m_forwardRate_scale^-1
  if (gates_m_forwardRate_x != 0) {
    gates_m_forwardRate_r = gates_m_forwardRate_rate * gates_m_forwardRate_x * (1 + -1 * exp(-1 * gates_m_forwardRate_x))^-1
  } else {
    gates_m_forwardRate_r = gates_m_forwardRate_rate
  }
  gates_m_inf = gates_m_forwardRate_r * (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_m_tau = (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_h_forwardRate_r = gates_h_forwardRate_rate * exp((v + -1 * gates_h_forwardRate_midpoint) * gates_h_forwardRate_scale^-1)
  gates_h_reverseRate_r = gates_h_reverseRate_rate * (1 + exp(-1 * (v + -1 * gates_h_reverseRate_midpoint) * gates_h_reverseRate_scale^-1))^-1
  gates_h_inf = gates_h_forwardRate_r * (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_tau = (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_q' = (gates_h_inf + -1 * gates_h_q) * gates_h_tau^-1
  gates_m_q' = (gates_m_inf + -1 * gates_m_q) * gates_m_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL gates_h_fcond, gates_m_fcond, fopen0, g

  gates_h_fcond = gates_h_q^gates_h_instances
  gates_m_fcond = gates_m_q^gates_m_instances
  fopen0 = gates_h_fcond * gates_m_fcond
  g = conductance * fopen0
  ina = g * (v + -1 * ena)
}

"#
    );
    assert_eq!(
        to_nmodl(&inst, "-*", "baseIonChannel", &ions()).unwrap(),
        r#"NEURON {
  SUFFIX NaConductance
  USEION na WRITE ina READ ena
  RANGE conductance
}

PARAMETER {
  conductance = 0.00001 (uS)
}

STATE { gates_h_q gates_m_q }

INITIAL {
  LOCAL gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r, gates_m_inf, gates_h_forwardRate_r, gates_h_reverseRate_r, gates_h_inf

  gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  gates_m_forwardRate_x = 0.1 * (40 + v)
  if (gates_m_forwardRate_x != 0) {
    gates_m_forwardRate_r = gates_m_forwardRate_x * (1 + -1 * exp(-1 * gates_m_forwardRate_x))^-1
  } else {
    gates_m_forwardRate_r = 1
  }
  gates_m_inf = gates_m_forwardRate_r * (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  gates_h_inf = gates_h_forwardRate_r * (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_q = gates_h_inf
  gates_m_q = gates_m_inf
}

DERIVATIVE dstate {
  LOCAL gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r, gates_m_inf, gates_m_tau, gates_h_forwardRate_r, gates_h_reverseRate_r, gates_h_inf, gates_h_tau

  gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  gates_m_forwardRate_x = 0.1 * (40 + v)
  if (gates_m_forwardRate_x != 0) {
    gates_m_forwardRate_r = gates_m_forwardRate_x * (1 + -1 * exp(-1 * gates_m_forwardRate_x))^-1
  } else {
    gates_m_forwardRate_r = 1
  }
  gates_m_inf = gates_m_forwardRate_r * (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_m_tau = (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  gates_h_inf = gates_h_forwardRate_r * (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_tau = (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_q' = (gates_h_inf + -1 * gates_h_q) * gates_h_tau^-1
  gates_m_q' = (gates_m_inf + -1 * gates_m_q) * gates_m_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL gates_m_fcond, fopen0, g

  gates_m_fcond = gates_m_q * gates_m_q * gates_m_q
  fopen0 = gates_h_q * gates_m_fcond
  g = conductance * fopen0
  ina = g * (v + -1 * ena)
}

"#
    );
}

#[test]
fn simple_passive_channel() {
    let lems = LemsFile::core();
    let tree = Document::parse(r#"<?xml version="1.0" encoding="UTF-8"?>
<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2  https://raw.githubusercontent.com/NeuroML/NeuroML2/master/Schemas/NeuroML2/NeuroML_v2beta3.xsd"
         id="passiveChan">
    <ionChannelHH id="passiveChan" conductance="10pS" type="ionChannelPassive" species="leak"/>
</neuroml>
"#).unwrap();
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("ionChannelHH"))
        .unwrap();
    let inst = Instance::new(&lems, &node).unwrap();
    assert_eq!(
        to_nmodl(&inst, "+*", "baseIonChannel", &ions()).unwrap(),
        r#"NEURON {
  SUFFIX passiveChan
  NONSPECIFIC_CURRENT ileak
  RANGE conductance, eleak
}

PARAMETER {
  conductance = 0.00001 (uS)
  eleak = 0 (mV)
}

BREAKPOINT {
  LOCAL g

  g = conductance
  ileak = g * (v + -1 * eleak)
}

"#
    );
    assert_eq!(
        to_nmodl(&inst, "-*", "baseIonChannel", &ions()).unwrap(),
        r#"NEURON {
  SUFFIX passiveChan
  NONSPECIFIC_CURRENT ileak
  RANGE conductance, eleak
}

PARAMETER {
  conductance = 0.00001 (uS)
  eleak = 0 (mV)
}

BREAKPOINT {
  LOCAL g

  g = conductance
  ileak = g * (v + -1 * eleak)
}

"#
    );
}

#[test]
fn simple_ion_channel_unknown_ion() {
    let lems = LemsFile::core();
    let tree = Document::parse(r#"<?xml version="1.0" encoding="UTF-8"?>

<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2  ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
         id="NML2_SimpleIonChannel">

    <!-- Example of a simple Na+ ion channel in NeuroML 2 -->

    <ionChannelHH id="NaConductance" conductance="10pS" species="na">
        <gateHHrates id="m" instances="3">
            <forwardRate type="HHExpLinearRate" rate="1per_ms" midpoint="-40mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="4per_ms" midpoint="-65mV" scale="-18mV"/>
        </gateHHrates>

        <gateHHrates id="h" instances="1">
            <forwardRate type="HHExpRate" rate="0.07per_ms" midpoint="-65mV" scale="-20mV"/>
            <reverseRate type="HHSigmoidRate" rate="1per_ms" midpoint="-35mV" scale="10mV"/>
        </gateHHrates>
    </ionChannelHH>

</neuroml>"#).unwrap();
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("ionChannelHH"))
        .unwrap();
    let inst = Instance::new(&lems, &node).unwrap();
    assert_eq!(
        to_nmodl(&inst, "+*", "baseIonChannel", &[]).unwrap(),
        r#"NEURON {
  SUFFIX NaConductance
  NONSPECIFIC_CURRENT ina
  RANGE conductance, ena, gates_h_forwardRate_midpoint, gates_h_forwardRate_rate, gates_h_forwardRate_scale, gates_h_instances, gates_h_reverseRate_midpoint, gates_h_reverseRate_rate, gates_h_reverseRate_scale, gates_m_forwardRate_midpoint, gates_m_forwardRate_rate, gates_m_forwardRate_scale, gates_m_instances, gates_m_reverseRate_midpoint, gates_m_reverseRate_rate, gates_m_reverseRate_scale
}

PARAMETER {
  conductance = 0.00001 (uS)
  ena = 0 (mV)
  gates_h_forwardRate_midpoint = -65 (mV)
  gates_h_forwardRate_rate = 0.07000000029802322 (per_ms)
  gates_h_forwardRate_scale = -20 (mV)
  gates_h_instances = 1
  gates_h_reverseRate_midpoint = -35 (mV)
  gates_h_reverseRate_rate = 1 (per_ms)
  gates_h_reverseRate_scale = 10 (mV)
  gates_m_forwardRate_midpoint = -40 (mV)
  gates_m_forwardRate_rate = 1 (per_ms)
  gates_m_forwardRate_scale = 10 (mV)
  gates_m_instances = 3
  gates_m_reverseRate_midpoint = -65 (mV)
  gates_m_reverseRate_rate = 4 (per_ms)
  gates_m_reverseRate_scale = -18 (mV)
}

STATE { gates_h_q gates_m_q }

INITIAL {
  LOCAL gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r, gates_m_inf, gates_h_forwardRate_r, gates_h_reverseRate_r, gates_h_inf

  gates_m_reverseRate_r = gates_m_reverseRate_rate * exp((v + -1 * gates_m_reverseRate_midpoint) * gates_m_reverseRate_scale^-1)
  gates_m_forwardRate_x = (v + -1 * gates_m_forwardRate_midpoint) * gates_m_forwardRate_scale^-1
  if (gates_m_forwardRate_x != 0) {
    gates_m_forwardRate_r = gates_m_forwardRate_rate * gates_m_forwardRate_x * (1 + -1 * exp(-1 * gates_m_forwardRate_x))^-1
  } else {
    gates_m_forwardRate_r = gates_m_forwardRate_rate
  }
  gates_m_inf = gates_m_forwardRate_r * (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_h_forwardRate_r = gates_h_forwardRate_rate * exp((v + -1 * gates_h_forwardRate_midpoint) * gates_h_forwardRate_scale^-1)
  gates_h_reverseRate_r = gates_h_reverseRate_rate * (1 + exp(-1 * (v + -1 * gates_h_reverseRate_midpoint) * gates_h_reverseRate_scale^-1))^-1
  gates_h_inf = gates_h_forwardRate_r * (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_q = gates_h_inf
  gates_m_q = gates_m_inf
}

DERIVATIVE dstate {
  LOCAL gates_m_reverseRate_r, gates_m_forwardRate_x, gates_m_forwardRate_r, gates_m_inf, gates_m_tau, gates_h_forwardRate_r, gates_h_reverseRate_r, gates_h_inf, gates_h_tau

  gates_m_reverseRate_r = gates_m_reverseRate_rate * exp((v + -1 * gates_m_reverseRate_midpoint) * gates_m_reverseRate_scale^-1)
  gates_m_forwardRate_x = (v + -1 * gates_m_forwardRate_midpoint) * gates_m_forwardRate_scale^-1
  if (gates_m_forwardRate_x != 0) {
    gates_m_forwardRate_r = gates_m_forwardRate_rate * gates_m_forwardRate_x * (1 + -1 * exp(-1 * gates_m_forwardRate_x))^-1
  } else {
    gates_m_forwardRate_r = gates_m_forwardRate_rate
  }
  gates_m_inf = gates_m_forwardRate_r * (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_m_tau = (gates_m_forwardRate_r + gates_m_reverseRate_r)^-1
  gates_h_forwardRate_r = gates_h_forwardRate_rate * exp((v + -1 * gates_h_forwardRate_midpoint) * gates_h_forwardRate_scale^-1)
  gates_h_reverseRate_r = gates_h_reverseRate_rate * (1 + exp(-1 * (v + -1 * gates_h_reverseRate_midpoint) * gates_h_reverseRate_scale^-1))^-1
  gates_h_inf = gates_h_forwardRate_r * (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_tau = (gates_h_forwardRate_r + gates_h_reverseRate_r)^-1
  gates_h_q' = (gates_h_inf + -1 * gates_h_q) * gates_h_tau^-1
  gates_m_q' = (gates_m_inf + -1 * gates_m_q) * gates_m_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL gates_h_fcond, gates_m_fcond, fopen0, g

  gates_h_fcond = gates_h_q^gates_h_instances
  gates_m_fcond = gates_m_q^gates_m_instances
  fopen0 = gates_h_fcond * gates_m_fcond
  g = conductance * fopen0
  ina = g * (v + -1 * ena)
}

"#
    );
}

#[test]
fn exp_two_syn() {
    let lems = LemsFile::core();
    let tree = Document::parse(r#"<?xml version="1.0" encoding="ISO-8859-1"?>
<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 https://raw.github.com/NeuroML/NeuroML2/development/Schemas/NeuroML2/NeuroML_v2beta4.xsd"
         id="GABAA">
    <expTwoSynapse id="GABAA" tauRise="3.1E-4s" tauDecay="0.0088333s" gbase="3.14159E-10S" erev="-0.07V"/>
</neuroml>
"#).unwrap();
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("expTwoSynapse"))
        .unwrap();
    let inst = Instance::new(&lems, &node).unwrap();
    assert_eq!(
        to_nmodl(&inst, "+*", "baseSynapse", &[]).unwrap(),
        r"NEURON {
  POINT_PROCESS GABAA
  NONSPECIFIC_CURRENT i
  RANGE erev, gbase, tauDecay, tauRise
}

PARAMETER {
  erev = -70.00000029802322 (mV)
  gbase = 0.0003141590043664877 (uS)
  tauDecay = 8.833300322294235 (ms)
  tauRise = 0.3100000030826777 (ms)
}

STATE { A B }

INITIAL {
  A = 0
  B = A
}

DERIVATIVE dstate {
  A' = -1 * A * tauRise^-1
  B' = -1 * B * tauDecay^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL g

  g = gbase * (B + -1 * A)
  i = -1 * g * (erev + -1 * v)
}

NET_RECEIVE(weight) {
  LOCAL peakTime, waveformFactor

  peakTime = tauDecay * tauRise * (tauDecay + -1 * tauRise)^-1 * log(tauDecay * tauRise^-1)
  waveformFactor = (-1 * exp(-1 * peakTime * tauRise^-1) + exp(-1 * peakTime * tauDecay^-1))^-1
  A = A + waveformFactor * weight
  B = B + waveformFactor * weight
}

"
    );
}

#[test]
fn q_10_settings() {
    let lems = LemsFile::core();
    let tree = Document::parse(r#"<?xml version="1.0" encoding="ISO-8859-1"?>
  <neuroml xmlns="http://www.neuroml.org/schema/neuroml2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 https://raw.github.com/NeuroML/NeuroML2/development/Schemas/NeuroML2/NeuroML_v2beta4.xsd" id="Golgi_H_CML">
    <ionChannel id="Golgi_H_CML" conductance="10pS" type="ionChannelHH" species="h">
      <gate id="n" type="gateHHrates" instances="1">
        <q10Settings type="q10ExpTemp" q10Factor="3" experimentalTemp="17.350264793 degC"/>
        <forwardRate type="HHExpRate" rate="0.8per_s" scale="-0.01100110011V" midpoint="-0.075V"/>
        <reverseRate type="HHExpRate" rate="0.8per_s" scale="0.01100110011V" midpoint="-0.075V"/>
      </gate>
  </ionChannel>
</neuroml>"#).unwrap();
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("ionChannel"))
        .unwrap();
    let inst = Instance::new(&lems, &node).unwrap();
    assert_eq!(
        to_nmodl(&inst, "+*", "baseIonChannel", &ions()).unwrap(),
        r#"NEURON {
  SUFFIX Golgi_H_CML
  NONSPECIFIC_CURRENT ih
  RANGE conductance, eh, gates_n_forwardRate_midpoint, gates_n_forwardRate_rate, gates_n_forwardRate_scale, gates_n_instances, gates_n_q10Settings_Unknown_experimentalTemp, gates_n_q10Settings_Unknown_q10Factor, gates_n_reverseRate_midpoint, gates_n_reverseRate_rate, gates_n_reverseRate_scale
}

CONSTANT {
  gates_n_q10Settings_Unknown_TENDEGREES = 10 (K)
}

PARAMETER {
  celsius
  conductance = 0.00001 (uS)
  eh = 0 (mV)
  gates_n_forwardRate_midpoint = -75.00000298023224 (mV)
  gates_n_forwardRate_rate = 0.0008000000119209289 (per_ms)
  gates_n_forwardRate_scale = -11.001099832355976 (mV)
  gates_n_instances = 1
  gates_n_q10Settings_Unknown_experimentalTemp = 290.50026550292966 (K)
  gates_n_q10Settings_Unknown_q10Factor = 3
  gates_n_reverseRate_midpoint = -75.00000298023224 (mV)
  gates_n_reverseRate_rate = 0.0008000000119209289 (per_ms)
  gates_n_reverseRate_scale = 11.001099832355976 (mV)
}

STATE { gates_n_q }

INITIAL {
  LOCAL gates_n_forwardRate_r, gates_n_reverseRate_r, gates_n_inf

  gates_n_forwardRate_r = gates_n_forwardRate_rate * exp((v + -1 * gates_n_forwardRate_midpoint) * gates_n_forwardRate_scale^-1)
  gates_n_reverseRate_r = gates_n_reverseRate_rate * exp((v + -1 * gates_n_reverseRate_midpoint) * gates_n_reverseRate_scale^-1)
  gates_n_inf = gates_n_forwardRate_r * (gates_n_forwardRate_r + gates_n_reverseRate_r)^-1
  gates_n_q = gates_n_inf
}

DERIVATIVE dstate {
  LOCAL gates_n_forwardRate_r, gates_n_reverseRate_r, gates_n_inf, temperature, gates_n_q10Settings_Unknown_q10, gates_n_tau

  gates_n_forwardRate_r = gates_n_forwardRate_rate * exp((v + -1 * gates_n_forwardRate_midpoint) * gates_n_forwardRate_scale^-1)
  gates_n_reverseRate_r = gates_n_reverseRate_rate * exp((v + -1 * gates_n_reverseRate_midpoint) * gates_n_reverseRate_scale^-1)
  gates_n_inf = gates_n_forwardRate_r * (gates_n_forwardRate_r + gates_n_reverseRate_r)^-1
  temperature = 273.1499938964844 + celsius
  gates_n_q10Settings_Unknown_q10 = gates_n_q10Settings_Unknown_q10Factor^((temperature + -1 * gates_n_q10Settings_Unknown_experimentalTemp) * gates_n_q10Settings_Unknown_TENDEGREES^-1)
  gates_n_tau = (gates_n_q10Settings_Unknown_q10 * (gates_n_forwardRate_r + gates_n_reverseRate_r))^-1
  gates_n_q' = (gates_n_inf + -1 * gates_n_q) * gates_n_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL gates_n_fcond, g

  gates_n_fcond = gates_n_q^gates_n_instances
  g = conductance * gates_n_fcond
  ih = g * (v + -1 * eh)
}

"#
    );
}

// #[test]
// fn non_specific_ion_channel() {
// let lems = LemsFile::core();
// let tree = Document::parse(r#"<?xml version="1.0" encoding="UTF-8"?>
// <neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
// xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
// xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2  ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
// id="NML2_SimpleIonChannel">
// <ionChannelHH id="NaConductance" conductance="10pS" species="na">
// <gateHHrates id="m" instances="1">
// <forwardRate type="HHExpLinearRate" rate="1per_ms" midpoint="-40mV" scale="10mV"/>
// <reverseRate type="HHExpRate" rate="4per_ms" midpoint="-65mV" scale="-18mV"/>
// </gateHHrates>
// </ionChannelHH>

// <ionChannelHH id="NaConductance" conductance="10pS" species="k">
// <gateHHrates id="m" instances="1">
// <forwardRate type="HHExpLinearRate" rate="1per_ms" midpoint="-40mV" scale="10mV"/>
// <reverseRate type="HHExpRate" rate="4per_ms" midpoint="-65mV" scale="-18mV"/>
// </gateHHrates>
// </ionChannelHH>
// </neuroml>"#).unwrap();
// let node = tree
// .descendants()
// .find(|n| n.has_tag_name("ionChannelHH"))
// .unwrap();
// let inst = Instance::new(&lems, &node).unwrap();
// assert_eq!(to_nmodl(&inst, "-*", "baseIonChannel", &[]).unwrap(), r#""#);
// }
