use nml2::{
    expr::Quantity,
    instance::Instance,
    lems::file::LemsFile,
    network::{Connection, Loc, Network},
    neuroml::raw::ConnectionWD,
    xml::XML,
};

use roxmltree::Document;

use pretty_assertions::assert_eq;

#[test]
fn weighted_connection() {
    let lems = LemsFile::core();
    let tree = Document::parse(r#"<?xml version="1.0" encoding="UTF-8"?>
<neuroml xmlns="http://www.neuroml.org/schema/neuroml2" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2  https://raw.githubusercontent.com/NeuroML/NeuroML2/development/Schemas/NeuroML2/NeuroML_v2beta4.xsd" id="network_GranCellLayer">

    <network id="network" type="networkWithTemperature" temperature="32.0 degC">
        <population id="A" component="Golgi" type="populationList" size="0"/>
        <population id="B" component="Granule" type="populationList" size="0"/>
        <projection synapse="GABAA" id="NetConn_Golgis_Grans" presynapticPopulation="Golgis" postsynapticPopulation="Grans">
            <connectionWD id="0" preCellId="../Golgis/0/Golgi_98" postCellId="../Grans/0/Granule_98" preSegmentId="0" preFractionAlong="0.0" postSegmentId="0" postFractionAlong="0.5" delay="4.2 ms" weight="51.38689"/>
        </projection>
    </network>
</neuroml>"#).unwrap();
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("connectionWD"))
        .unwrap();
    let conn = ConnectionWD::from_node(&node);
    assert_eq!(conn.weight, 51.38689);
    assert_eq!(conn.delay, "4.2 ms");
    let node = tree
        .descendants()
        .find(|n| n.has_tag_name("network"))
        .unwrap();
    let inst = Instance::new(&lems, &node).unwrap();
    let netw = Network::new(&inst).unwrap();
    assert_eq!(
        netw.projections[0].connections[0],
        Connection {
            from: Loc {
                cell: 0,
                segment: 0,
                fraction: "0.0".to_string()
            },
            to: Loc {
                cell: 0,
                segment: 0,
                fraction: "0.5".to_string()
            },
            weight: Quantity {
                value: 51.38689041137695,
                unit: None
            },
            delay: Quantity {
                value: 4.199999809265137,
                unit: Some("ms".to_string())
            }
        }
    );
}
