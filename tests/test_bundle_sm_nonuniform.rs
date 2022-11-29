use nml2::{
    lems::file::LemsFile,
    xml::XML,
    bundle::ion_channel_assignments,
    Map
};
use roxmltree::Document;

#[test]
fn check_nonunforms_are_merged() {
    let lems = LemsFile::core();
    let tree = Document::parse(r#"
        <neuroml>
        <cell id="Cell1">
                <biophysicalProperties id="biophys">
                    <membraneProperties>

                        <channelDensityNonUniform id="a" ionChannel="a" ion="a" erev="0 mV">
                            <variableParameter parameter="condDensity" segmentGroup="g">
                                <inhomogeneousValue inhomogeneousParameter="Param" value="1"/>
                            </variableParameter>
                        </channelDensityNonUniform>

                        <channelDensity condDensity="1 mS_per_cm2" id="b" ionChannel="b" segmentGroup="g" ion="b" erev="0 mV"/>

                        <channelDensityNonUniformNernst id="c" ionChannel="c" ion="c">
                            <variableParameter parameter="condDensity" segmentGroup="g">
                                <inhomogeneousValue inhomogeneousParameter="Param" value="1"/>
                            </variableParameter>
                        </channelDensityNonUniformNernst>

                    </membraneProperties>
                    <intracellularProperties>
                    </intracellularProperties>
                </biophysicalProperties>
            </cell>
        </neuroml>
    "#).unwrap();
    let biophys = XML::from_node(tree.descendants()
        .find(|c| c.tag_name().name() == "biophysicalProperties")
        .iter()
        .next()
        .unwrap());
    let props = Map::from([
          (String::from("Cell1"), biophys)
    ]);
    let sms =ion_channel_assignments(&props, &lems).unwrap();
    assert!(sms.len() == 1);
    let k = (String::from("Cell1"), String::from("g"));
    assert!(sms.contains_key(&k));
    assert!(sms[&k].len() == 3);
}
