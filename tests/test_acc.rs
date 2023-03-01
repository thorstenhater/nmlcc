use nml2::{
    acc::{self, Decor, Sexp},
    bundle::{build_super_mechanisms, Cell, CellData},
    error::Result,
    instance::Instance,
    lems::file::LemsFile,
    neuroml::{self, raw::BiophysicalProperties},
    nmodl,
    xml::XML,
    Map,
};

use pretty_assertions::assert_eq;

use roxmltree::Document;

#[test]
fn acc_biophys() {
    let known_ions = vec![String::from("ca"), String::from("k"), String::from("na")];
    let lems = LemsFile::core();
    let tree = Document::parse(
r#"<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
            id="hh">
  <biophysicalProperties id="properties">
    <membraneProperties>
      <channelDensity id="leak" ionChannel="passiveChan" condDensity="3.0 S_per_m2" erev="-54.3mV" ion="non_specific"/>
      <channelDensity id="naChans" ionChannel="naChan" condDensity="120.0 mS_per_cm2" erev="50.0 mV" ion="na"/>
      <channelDensity id="kChans" ionChannel="kChan" condDensity="360 S_per_m2" erev="-77mV" ion="k"/>
      <spikeThresh value="-20mV"/>
      <specificCapacitance value="1.0 uF_per_cm2"/>
      <initMembPotential value="-65mV"/>
    </membraneProperties>
    <intracellularProperties>
      <resistivity value="0.03 kohm_cm"/>
    </intracellularProperties>
  </biophysicalProperties>
</neuroml>
"#).unwrap();
    let node = &tree
        .descendants()
        .find(|n| n.has_tag_name("biophysicalProperties"))
        .unwrap();
    let prop: neuroml::raw::BiophysicalProperties = XML::from_node(node);
    let result = acc::biophys(&prop, &lems, &known_ions, &Default::default());
    assert!(result.is_ok());
    let mut result = result.unwrap();
    result.decor.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let mut expect = vec![
        Decor::mechanism(
            "all",
            "naChan",
            &Map::from([(String::from("conductance"), String::from("0.12"))]),
        ),
        Decor::mechanism(
            "all",
            "kChan",
            &Map::from([(String::from("conductance"), String::from("0.036"))]),
        ),
        Decor::mechanism(
            "all",
            "passiveChan",
            &Map::from([
                (String::from("conductance"), String::from("0.0003")),
                (String::from("e"), String::from("-54.29999923706055")),
            ]),
        ),
        Decor::er("all", "na", "50"),
        Decor::er("all", "k", "-77"),
        Decor::cm("all", "0.01"),
        Decor::vm("all", "-65"),
        Decor::ra("all", "29.999999329447746"),
    ];
    expect.sort_by(|a, b| a.partial_cmp(b).unwrap());
    assert_eq!(result.decor, expect);
}

#[test]
fn acc_super() {
    let known_ions = vec![String::from("ca"), String::from("k"), String::from("na")];
    let lems = LemsFile::core();
    let tree = Document::parse(
r#"<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
            id="hh">

    <ionChannelHH id="passiveChan" conductance="10pS"/>

    <ionChannelHH id="naChan" conductance="10pS"  species="na">
        <gateHHrates id="m" instances="3">
            <forwardRate type="HHExpLinearRate" rate="1per_ms" midpoint="-40mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="4per_ms" midpoint="-65mV" scale="-18mV"/>
        </gateHHrates>
        <gateHHrates id="h" instances="1">
            <forwardRate type="HHExpRate" rate="0.07per_ms" midpoint="-65mV" scale="-20mV"/>
            <reverseRate type="HHSigmoidRate" rate="1per_ms" midpoint="-35mV" scale="10mV"/>
        </gateHHrates>
    </ionChannelHH>


    <ionChannelHH id="kChan" conductance="10pS" species="k">
        <gateHHrates id="n" instances="4">
            <forwardRate type="HHExpLinearRate" rate="0.1per_ms" midpoint="-55mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="0.125per_ms" midpoint="-65mV" scale="-80mV"/>
        </gateHHrates>
    </ionChannelHH>

  <cell id="cell">
    <morphology id="morph">
      <segment id="0" name="soma">
        <proximal x="0" y="0" z="0" diameter="42"/>
        <distal   x="0" y="0" z="0" diameter="42"/>
      </segment>

      <segmentGroup id="soma_group">
        <member segment="0"/>
      </segmentGroup>
    </morphology>

    <biophysicalProperties id="properties">
      <membraneProperties>
        <channelDensity id="leak" ionChannel="passiveChan" condDensity="3.0 S_per_m2" erev="-54.3mV" ion="non_specific"/>
        <channelDensity id="naChans" ionChannel="naChan" condDensity="120.0 mS_per_cm2" erev="50.0 mV" ion="na"/>
        <channelDensity id="kChans" ionChannel="kChan" condDensity="360 S_per_m2" erev="-77mV" ion="k"/>
        <spikeThresh value="-20mV"/>
        <specificCapacitance value="1.0 uF_per_cm2"/>
        <initMembPotential value="-65mV"/>
      </membraneProperties>

      <intracellularProperties>
        <resistivity value="0.03 kohm_cm"/>
      </intracellularProperties>
    </biophysicalProperties>
  </cell>
</neuroml>
"#).unwrap();

    let node = &tree
        .descendants()
        .find(|n| n.has_tag_name("biophysicalProperties"))
        .unwrap();
    let props: Map<String, BiophysicalProperties> =
        Map::from([("cell".to_string(), XML::from_node(node))]);
    let ins = tree
        .descendants()
        .filter(|n| lems.derived_from(n.tag_name().name(), "baseIonChannel"))
        .map(|n| Instance::new(&lems, &n))
        .collect::<Result<Vec<Instance>>>()
        .unwrap();
    assert_eq!(ins.len(), 3);
    let data = CellData {
        bio_phys: props,
        density: ins,
        synapse: vec![],
        c_model: vec![],
        i_param: Map::from([("cell".to_string(), Map::new())]),
    };
    let cells = build_super_mechanisms(&data, &lems, &known_ions[..]).unwrap();
    assert_eq!(cells.len(), 1);
    let Cell { decor, channels } = &cells["cell"];
    assert_eq!(
        decor.to_sexp(),
        "(arbor-component
  (meta-data (version \"0.1-dev\"))
  (decor
    (paint (region \"all\") (density (mechanism \"cell_all\" )))
    (default (ion-reversal-potential \"na\" 50))
    (default (ion-reversal-potential \"k\" -77))
    (default (membrane-capacitance 0.01))
    (default (membrane-potential -65))
    (default (axial-resistivity 29.999999329447746))))
"
    );
    assert_eq!(channels.len(), 1);
    let (rg, ch) = &channels[0];
    assert_eq!(rg, "all");
    let ch = nmodl::mk_nmodl(ch.clone()).unwrap();
    assert_eq!(
        ch,
        r#"NEURON {
  SUFFIX cell_all
  USEION k WRITE ik READ ek
  USEION na WRITE ina READ ena
  NONSPECIFIC_CURRENT i
}

STATE { kChan_gates_n_q naChan_gates_h_q naChan_gates_m_q }

INITIAL {
  LOCAL naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_inf, naChan_gates_h_forwardRate_r, naChan_gates_h_reverseRate_r, naChan_gates_h_inf, kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf

  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) {
    naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1
  } else {
    naChan_gates_m_forwardRate_r = 1
  }
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) {
    kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1
  } else {
    kChan_gates_n_forwardRate_r = 0.10000000149011612
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_q = kChan_gates_n_inf
  naChan_gates_h_q = naChan_gates_h_inf
  naChan_gates_m_q = naChan_gates_m_inf
}

DERIVATIVE dstate {
  LOCAL naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_inf, naChan_gates_m_tau, naChan_gates_h_forwardRate_r, naChan_gates_h_reverseRate_r, naChan_gates_h_inf, naChan_gates_h_tau, kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf, kChan_gates_n_tau

  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) {
    naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1
  } else {
    naChan_gates_m_forwardRate_r = 1
  }
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_m_tau = (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  naChan_gates_h_tau = (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) {
    kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1
  } else {
    kChan_gates_n_forwardRate_r = 0.10000000149011612
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_tau = (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_q' = (kChan_gates_n_inf + -1 * kChan_gates_n_q) * kChan_gates_n_tau^-1
  naChan_gates_h_q' = (naChan_gates_h_inf + -1 * naChan_gates_h_q) * naChan_gates_h_tau^-1
  naChan_gates_m_q' = (naChan_gates_m_inf + -1 * naChan_gates_m_q) * naChan_gates_m_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL naChan_gates_m_fcond, naChan_fopen0, naChan_g, g_na, kChan_gates_n_fcond, kChan_g, g_k

  naChan_gates_m_fcond = naChan_gates_m_q * naChan_gates_m_q * naChan_gates_m_q
  naChan_fopen0 = naChan_gates_h_q * naChan_gates_m_fcond
  naChan_g = 0.12 * naChan_fopen0
  g_na = naChan_g
  kChan_gates_n_fcond = kChan_gates_n_q * kChan_gates_n_q * kChan_gates_n_q * kChan_gates_n_q
  kChan_g = 0.036 * kChan_gates_n_fcond
  g_k = kChan_g
  i = 0.0003 * (54.29999923706055 + v)
  ik = g_k * (v + -1 * ek)
  ina = g_na * (v + -1 * ena)
}

"#
    );
}

#[test]
fn acc_super_non_specific() {
    let known_ions = vec![String::from("ca"), String::from("k"), String::from("na")];
    let lems = LemsFile::core();
    let tree = Document::parse(
r#"<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
            id="hh">

    <ionChannelHH id="passiveChan" conductance="10pS"/>

    <ionChannelHH id="naChan" conductance="10pS"  species="hcn">
        <gateHHrates id="m" instances="3">
            <forwardRate type="HHExpLinearRate" rate="1per_ms" midpoint="-40mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="4per_ms" midpoint="-65mV" scale="-18mV"/>
        </gateHHrates>
        <gateHHrates id="h" instances="1">
            <forwardRate type="HHExpRate" rate="0.07per_ms" midpoint="-65mV" scale="-20mV"/>
            <reverseRate type="HHSigmoidRate" rate="1per_ms" midpoint="-35mV" scale="10mV"/>
        </gateHHrates>
    </ionChannelHH>


    <ionChannelHH id="kChan" conductance="10pS" species="k">
        <gateHHrates id="n" instances="4">
            <forwardRate type="HHExpLinearRate" rate="0.1per_ms" midpoint="-55mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="0.125per_ms" midpoint="-65mV" scale="-80mV"/>
        </gateHHrates>
    </ionChannelHH>

  <cell id="cell">
    <morphology id="morph">
      <segment id="0" name="soma">
        <proximal x="0" y="0" z="0" diameter="42"/>
        <distal   x="0" y="0" z="0" diameter="42"/>
      </segment>

      <segmentGroup id="soma_group">
        <member segment="0"/>
      </segmentGroup>
    </morphology>

    <biophysicalProperties id="properties">
      <membraneProperties>
        <channelDensity id="leak" ionChannel="passiveChan" condDensity="3.0 S_per_m2" erev="-54.3mV" ion="non_specific"/>
        <channelDensity id="naChans" ionChannel="naChan" condDensity="120.0 mS_per_cm2" erev="50.0 mV" ion="na"/>
        <channelDensity id="kChans" ionChannel="kChan" condDensity="360 S_per_m2" erev="-77mV" ion="k"/>
        <spikeThresh value="-20mV"/>
        <specificCapacitance value="1.0 uF_per_cm2"/>
        <initMembPotential value="-65mV"/>
      </membraneProperties>

      <intracellularProperties>
        <resistivity value="0.03 kohm_cm"/>
      </intracellularProperties>
    </biophysicalProperties>
  </cell>
</neuroml>
"#).unwrap();

    let node = &tree
        .descendants()
        .find(|n| n.has_tag_name("biophysicalProperties"))
        .unwrap();
    let props: Map<String, BiophysicalProperties> =
        Map::from([("cell".to_string(), XML::from_node(node))]);
    let ins = tree
        .descendants()
        .filter(|n| lems.derived_from(n.tag_name().name(), "baseIonChannel"))
        .map(|n| Instance::new(&lems, &n))
        .collect::<Result<Vec<Instance>>>()
        .unwrap();
    assert_eq!(ins.len(), 3);
    let data = CellData {
        bio_phys: props,
        density: ins,
        synapse: vec![],
        c_model: vec![],
        i_param: Map::from([("cell".to_string(), Map::new())]),
    };
    let cells = build_super_mechanisms(&data, &lems, &known_ions[..]).unwrap();
    assert_eq!(cells.len(), 1);
    let Cell { decor, channels } = &cells["cell"];
    assert_eq!(
        decor.to_sexp(),
        "(arbor-component
  (meta-data (version \"0.1-dev\"))
  (decor
    (paint (region \"all\") (density (mechanism \"cell_all\" )))
    (default (ion-reversal-potential \"na\" 50))
    (default (ion-reversal-potential \"k\" -77))
    (default (membrane-capacitance 0.01))
    (default (membrane-potential -65))
    (default (axial-resistivity 29.999999329447746))))
"
    );
    assert_eq!(channels.len(), 1);
    let (rg, ch) = &channels[0];
    assert_eq!(rg, "all");
    let ch = nmodl::mk_nmodl(ch.clone()).unwrap();
    assert_eq!(
        ch,
"NEURON {
  SUFFIX cell_all
  USEION k WRITE ik READ ek
  NONSPECIFIC_CURRENT i
}

STATE { kChan_gates_n_q naChan_gates_h_q naChan_gates_m_q }

INITIAL {
  LOCAL naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_inf, naChan_gates_h_forwardRate_r, naChan_gates_h_reverseRate_r, naChan_gates_h_inf, kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf

  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) {
    naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1
  } else {
    naChan_gates_m_forwardRate_r = 1
  }
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) {
    kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1
  } else {
    kChan_gates_n_forwardRate_r = 0.10000000149011612
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_q = kChan_gates_n_inf
  naChan_gates_h_q = naChan_gates_h_inf
  naChan_gates_m_q = naChan_gates_m_inf
}

DERIVATIVE dstate {
  LOCAL naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_inf, naChan_gates_m_tau, naChan_gates_h_forwardRate_r, naChan_gates_h_reverseRate_r, naChan_gates_h_inf, naChan_gates_h_tau, kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf, kChan_gates_n_tau

  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) {
    naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1
  } else {
    naChan_gates_m_forwardRate_r = 1
  }
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_m_tau = (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  naChan_gates_h_tau = (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) {
    kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1
  } else {
    kChan_gates_n_forwardRate_r = 0.10000000149011612
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_tau = (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_q' = (kChan_gates_n_inf + -1 * kChan_gates_n_q) * kChan_gates_n_tau^-1
  naChan_gates_h_q' = (naChan_gates_h_inf + -1 * naChan_gates_h_q) * naChan_gates_h_tau^-1
  naChan_gates_m_q' = (naChan_gates_m_inf + -1 * naChan_gates_m_q) * naChan_gates_m_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL kChan_gates_n_fcond, kChan_g, g_k, naChan_gates_m_fcond, naChan_fopen0, naChan_g

  kChan_gates_n_fcond = kChan_gates_n_q * kChan_gates_n_q * kChan_gates_n_q * kChan_gates_n_q
  kChan_g = 0.036 * kChan_gates_n_fcond
  g_k = kChan_g
  naChan_gates_m_fcond = naChan_gates_m_q * naChan_gates_m_q * naChan_gates_m_q
  naChan_fopen0 = naChan_gates_h_q * naChan_gates_m_fcond
  naChan_g = 0.12 * naChan_fopen0
  i = 0.0003 * (54.29999923706055 + v) + naChan_g * (-50 + v)
  ik = g_k * (v + -1 * ek)
}

"
    );
}

#[test]
fn acc_super_conc_model() {
    let known_ions = vec![String::from("ca"), String::from("k"), String::from("na")];
    let lems = LemsFile::core();
    let tree = Document::parse(
r#"<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
            id="hh">

    <concentrationModel id="simple_decay" type="fixedFactorConcentrationModel"
                        ion="ca"
                        restingConc="3e-6 mM"
                        decayConstant="1.0 ms"
                        rho="3e-1 mol_per_m_per_A_per_s"/>

    <ionChannelHH id="passiveChan" conductance="10pS"/>

    <ionChannelHH id="naChan" conductance="10pS"  species="hcn">
        <gateHHrates id="m" instances="3">
            <forwardRate type="HHExpLinearRate" rate="1per_ms" midpoint="-40mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="4per_ms" midpoint="-65mV" scale="-18mV"/>
        </gateHHrates>
        <gateHHrates id="h" instances="1">
            <forwardRate type="HHExpRate" rate="0.07per_ms" midpoint="-65mV" scale="-20mV"/>
            <reverseRate type="HHSigmoidRate" rate="1per_ms" midpoint="-35mV" scale="10mV"/>
        </gateHHrates>
    </ionChannelHH>


    <ionChannelHH id="kChan" conductance="10pS" species="k">
        <gateHHrates id="n" instances="4">
            <forwardRate type="HHExpLinearRate" rate="0.1per_ms" midpoint="-55mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="0.125per_ms" midpoint="-65mV" scale="-80mV"/>
        </gateHHrates>
    </ionChannelHH>

  <cell id="cell">
    <morphology id="morph">
      <segment id="0" name="soma">
        <proximal x="0" y="0" z="0" diameter="42"/>
        <distal   x="0" y="0" z="0" diameter="42"/>
      </segment>

      <segmentGroup id="soma_group">
        <member segment="0"/>
      </segmentGroup>
    </morphology>

    <biophysicalProperties id="properties">
      <membraneProperties>
        <channelDensity id="leak" ionChannel="passiveChan" condDensity="3.0 S_per_m2" erev="-54.3mV" ion="non_specific"/>
        <channelDensity id="naChans" ionChannel="naChan" condDensity="120.0 mS_per_cm2" erev="50.0 mV" ion="na"/>
        <channelDensity id="kChans" ionChannel="kChan" condDensity="360 S_per_m2" erev="-77mV" ion="k"/>
        <spikeThresh value="-20mV"/>
        <specificCapacitance value="1.0 uF_per_cm2"/>
        <initMembPotential value="-65mV"/>
      </membraneProperties>

      <intracellularProperties>
        <resistivity value="0.03 kohm_cm"/>
        <species id="ca" ion="ca" concentrationModel="CaDynamics_E2_NML2" initialConcentration="5.0E-11 mol_per_cm3" initialExtConcentration="2.0E-6 mol_per_cm3"/>

      </intracellularProperties>
    </biophysicalProperties>
  </cell>
</neuroml>"#).unwrap();

    let node = &tree
        .descendants()
        .find(|n| n.has_tag_name("biophysicalProperties"))
        .unwrap();
    let props: Map<String, BiophysicalProperties> =
        Map::from([("cell".to_string(), XML::from_node(node))]);
    let ich = tree
        .descendants()
        .filter(|n| lems.derived_from(n.tag_name().name(), "baseIonChannel"))
        .map(|n| Instance::new(&lems, &n))
        .collect::<Result<Vec<Instance>>>()
        .unwrap();
    assert_eq!(ich.len(), 3);
    let ccm = tree
        .descendants()
        .filter(|n| lems.derived_from(n.tag_name().name(), "concentrationModel"))
        .map(|n| Instance::new(&lems, &n))
        .collect::<Result<Vec<Instance>>>()
        .unwrap();
    assert_eq!(ccm.len(), 1);
    let data = CellData {
        bio_phys: props,
        density: ich,
        synapse: vec![],
        c_model: ccm,
        i_param: Map::from([("cell".to_string(), Map::new())]),
    };
    let cells = build_super_mechanisms(&data, &lems, &known_ions[..]).unwrap();
    assert_eq!(cells.len(), 1);
    let Cell { decor, channels } = &cells["cell"];
    assert_eq!(
        decor.to_sexp(),
        "(arbor-component
  (meta-data (version \"0.1-dev\"))
  (decor
    (paint (region \"all\") (density (mechanism \"cell_all\" )))
    (default (ion-reversal-potential \"na\" 50))
    (default (ion-reversal-potential \"k\" -77))
    (default (membrane-capacitance 0.01))
    (default (membrane-potential -65))
    (default (axial-resistivity 29.999999329447746))
    (default (ion-internal-concentration \"ca\" 0.0000500000006675716))
    (default (ion-external-concentration \"ca\" 1.9999999949504854))
    (paint (region \"all\") (density (mechanism \"CaDynamics_E2_NML2\" (\"initialConcentration\" 0.0000500000006675716))))))
"
    );
    assert_eq!(channels.len(), 1);
    let (rg, ch) = &channels[0];
    assert_eq!(rg, "all");
    let ch = nmodl::mk_nmodl(ch.clone()).unwrap();
    assert_eq!(ch,
"NEURON {
  SUFFIX cell_all
  USEION k WRITE ik READ ek
  NONSPECIFIC_CURRENT i
}

STATE { kChan_gates_n_q naChan_gates_h_q naChan_gates_m_q }

INITIAL {
  LOCAL naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_inf, naChan_gates_h_forwardRate_r, naChan_gates_h_reverseRate_r, naChan_gates_h_inf, kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf

  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) {
    naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1
  } else {
    naChan_gates_m_forwardRate_r = 1
  }
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) {
    kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1
  } else {
    kChan_gates_n_forwardRate_r = 0.10000000149011612
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_q = kChan_gates_n_inf
  naChan_gates_h_q = naChan_gates_h_inf
  naChan_gates_m_q = naChan_gates_m_inf
}

DERIVATIVE dstate {
  LOCAL naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_inf, naChan_gates_m_tau, naChan_gates_h_forwardRate_r, naChan_gates_h_reverseRate_r, naChan_gates_h_inf, naChan_gates_h_tau, kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf, kChan_gates_n_tau

  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) {
    naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1
  } else {
    naChan_gates_m_forwardRate_r = 1
  }
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_m_tau = (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  naChan_gates_h_tau = (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) {
    kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1
  } else {
    kChan_gates_n_forwardRate_r = 0.10000000149011612
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_tau = (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_q' = (kChan_gates_n_inf + -1 * kChan_gates_n_q) * kChan_gates_n_tau^-1
  naChan_gates_h_q' = (naChan_gates_h_inf + -1 * naChan_gates_h_q) * naChan_gates_h_tau^-1
  naChan_gates_m_q' = (naChan_gates_m_inf + -1 * naChan_gates_m_q) * naChan_gates_m_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL kChan_gates_n_fcond, kChan_g, g_k, naChan_gates_m_fcond, naChan_fopen0, naChan_g

  kChan_gates_n_fcond = kChan_gates_n_q * kChan_gates_n_q * kChan_gates_n_q * kChan_gates_n_q
  kChan_g = 0.036 * kChan_gates_n_fcond
  g_k = kChan_g
  naChan_gates_m_fcond = naChan_gates_m_q * naChan_gates_m_q * naChan_gates_m_q
  naChan_fopen0 = naChan_gates_h_q * naChan_gates_m_fcond
  naChan_g = 0.12 * naChan_fopen0
  i = 0.0003 * (54.29999923706055 + v) + naChan_g * (-50 + v)
  ik = g_k * (v + -1 * ek)
}

"
    );
}

#[test]
fn acc_super_nernst() {
    let known_ions = vec![String::from("ca"), String::from("k"), String::from("na")];
    let lems = LemsFile::core();
    let tree = Document::parse(
r#"<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
            xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
            xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2 ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
            id="hh">

    <ionChannelHH id="passiveChan" conductance="10pS"/>

    <ionChannelHH id="naChan" conductance="10pS"  species="na">
        <gateHHrates id="m" instances="3">
            <forwardRate type="HHExpLinearRate" rate="1per_ms" midpoint="-40mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="4per_ms" midpoint="-65mV" scale="-18mV"/>
        </gateHHrates>
        <gateHHrates id="h" instances="1">
            <forwardRate type="HHExpRate" rate="0.07per_ms" midpoint="-65mV" scale="-20mV"/>
            <reverseRate type="HHSigmoidRate" rate="1per_ms" midpoint="-35mV" scale="10mV"/>
        </gateHHrates>
    </ionChannelHH>


    <ionChannelHH id="kChan" conductance="10pS" species="k">
        <gateHHrates id="n" instances="4">
            <forwardRate type="HHExpLinearRate" rate="0.1per_ms" midpoint="-55mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="0.125per_ms" midpoint="-65mV" scale="-80mV"/>
        </gateHHrates>
    </ionChannelHH>

  <cell id="cell">
    <morphology id="morph">
      <segment id="0" name="soma">
        <proximal x="0" y="0" z="0" diameter="42"/>
        <distal   x="0" y="0" z="0" diameter="42"/>
      </segment>

      <segmentGroup id="soma_group">
        <member segment="0"/>
      </segmentGroup>
    </morphology>

    <biophysicalProperties id="properties">
      <membraneProperties>
        <channelDensity id="leak" ionChannel="passiveChan" condDensity="3.0 S_per_m2" erev="-54.3mV" ion="non_specific"/>
        <channelDensity id="naChans" ionChannel="naChan" condDensity="120.0 mS_per_cm2" erev="50.0 mV" ion="na"/>
        <channelDensityNernst id="kChans" ionChannel="kChan" condDensity="360 S_per_m2" erev="-77mV" ion="k"/>
        <spikeThresh value="-20mV"/>
        <specificCapacitance value="1.0 uF_per_cm2"/>
        <initMembPotential value="-65mV"/>
      </membraneProperties>

      <intracellularProperties>
        <resistivity value="0.03 kohm_cm"/>
      </intracellularProperties>
    </biophysicalProperties>
  </cell>
</neuroml>
"#).unwrap();

    let node = &tree
        .descendants()
        .find(|n| n.has_tag_name("biophysicalProperties"))
        .unwrap();
    let props: Map<String, BiophysicalProperties> =
        Map::from([("cell".to_string(), XML::from_node(node))]);
    let ins = tree
        .descendants()
        .filter(|n| lems.derived_from(n.tag_name().name(), "baseIonChannel"))
        .map(|n| Instance::new(&lems, &n))
        .collect::<Result<Vec<Instance>>>()
        .unwrap();
    assert_eq!(ins.len(), 3);
    let data = CellData {
        bio_phys: props,
        density: ins,
        synapse: vec![],
        c_model: vec![],
        i_param: Map::from([("cell".to_string(), Map::new())]),
    };
    let cells = build_super_mechanisms(&data, &lems, &known_ions[..]).unwrap();
    assert_eq!(cells.len(), 1);
    let Cell { decor, channels } = &cells["cell"];
    assert_eq!(
        decor.to_sexp(),
        "(arbor-component
  (meta-data (version \"0.1-dev\"))
  (decor
    (paint (region \"all\") (density (mechanism \"cell_all\" )))
    (default (ion-reversal-potential \"na\" 50))
    (default (ion-reversal-potential-method \"k\" (mechanism \"nernst/k\")))
    (default (membrane-capacitance 0.01))
    (default (membrane-potential -65))
    (default (axial-resistivity 29.999999329447746))))
"
    );
    assert_eq!(channels.len(), 1);
    let (rg, ch) = &channels[0];
    assert_eq!(rg, "all");
    let ch = nmodl::mk_nmodl(ch.clone()).unwrap();
    assert_eq!(
        ch,
        r#"NEURON {
  SUFFIX cell_all
  USEION k WRITE ik READ ek
  USEION na WRITE ina READ ena
  NONSPECIFIC_CURRENT i
}

STATE { kChan_gates_n_q naChan_gates_h_q naChan_gates_m_q }

INITIAL {
  LOCAL naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_inf, naChan_gates_h_forwardRate_r, naChan_gates_h_reverseRate_r, naChan_gates_h_inf, kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf

  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) {
    naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1
  } else {
    naChan_gates_m_forwardRate_r = 1
  }
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) {
    kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1
  } else {
    kChan_gates_n_forwardRate_r = 0.10000000149011612
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_q = kChan_gates_n_inf
  naChan_gates_h_q = naChan_gates_h_inf
  naChan_gates_m_q = naChan_gates_m_inf
}

DERIVATIVE dstate {
  LOCAL naChan_gates_m_reverseRate_r, naChan_gates_m_forwardRate_x, naChan_gates_m_forwardRate_r, naChan_gates_m_inf, naChan_gates_m_tau, naChan_gates_h_forwardRate_r, naChan_gates_h_reverseRate_r, naChan_gates_h_inf, naChan_gates_h_tau, kChan_gates_n_reverseRate_r, kChan_gates_n_forwardRate_x, kChan_gates_n_forwardRate_r, kChan_gates_n_inf, kChan_gates_n_tau

  naChan_gates_m_reverseRate_r = 4 * exp(-0.05555555555555555 * (65 + v))
  naChan_gates_m_forwardRate_x = 0.1 * (40 + v)
  if (naChan_gates_m_forwardRate_x != 0) {
    naChan_gates_m_forwardRate_r = naChan_gates_m_forwardRate_x * (1 + -1 * exp(-1 * naChan_gates_m_forwardRate_x))^-1
  } else {
    naChan_gates_m_forwardRate_r = 1
  }
  naChan_gates_m_inf = naChan_gates_m_forwardRate_r * (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_m_tau = (naChan_gates_m_forwardRate_r + naChan_gates_m_reverseRate_r)^-1
  naChan_gates_h_forwardRate_r = 0.07000000029802322 * exp(-0.05 * (65 + v))
  naChan_gates_h_reverseRate_r = (1 + exp(-0.1 * (35 + v)))^-1
  naChan_gates_h_inf = naChan_gates_h_forwardRate_r * (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  naChan_gates_h_tau = (naChan_gates_h_forwardRate_r + naChan_gates_h_reverseRate_r)^-1
  kChan_gates_n_reverseRate_r = 0.125 * exp(-0.0125 * (65 + v))
  kChan_gates_n_forwardRate_x = 0.1 * (55 + v)
  if (kChan_gates_n_forwardRate_x != 0) {
    kChan_gates_n_forwardRate_r = 0.10000000149011612 * kChan_gates_n_forwardRate_x * (1 + -1 * exp(-1 * kChan_gates_n_forwardRate_x))^-1
  } else {
    kChan_gates_n_forwardRate_r = 0.10000000149011612
  }
  kChan_gates_n_inf = kChan_gates_n_forwardRate_r * (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_tau = (kChan_gates_n_forwardRate_r + kChan_gates_n_reverseRate_r)^-1
  kChan_gates_n_q' = (kChan_gates_n_inf + -1 * kChan_gates_n_q) * kChan_gates_n_tau^-1
  naChan_gates_h_q' = (naChan_gates_h_inf + -1 * naChan_gates_h_q) * naChan_gates_h_tau^-1
  naChan_gates_m_q' = (naChan_gates_m_inf + -1 * naChan_gates_m_q) * naChan_gates_m_tau^-1
}

BREAKPOINT {
  SOLVE dstate METHOD cnexp
  LOCAL naChan_gates_m_fcond, naChan_fopen0, naChan_g, g_na, kChan_gates_n_fcond, kChan_g, g_k

  naChan_gates_m_fcond = naChan_gates_m_q * naChan_gates_m_q * naChan_gates_m_q
  naChan_fopen0 = naChan_gates_h_q * naChan_gates_m_fcond
  naChan_g = 0.12 * naChan_fopen0
  g_na = naChan_g
  kChan_gates_n_fcond = kChan_gates_n_q * kChan_gates_n_q * kChan_gates_n_q * kChan_gates_n_q
  kChan_g = 0.036 * kChan_gates_n_fcond
  g_k = kChan_g
  i = 0.0003 * (54.29999923706055 + v)
  ik = g_k * (v + -1 * ek)
  ina = g_na * (v + -1 * ena)
}

"#
    );
}
