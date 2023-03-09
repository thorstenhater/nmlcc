use nml2::{
    bundle,
    error::Result,
    lems::{self, file::LemsFile},
    neuroml,
    xml::XML,
};
use std::process::Command;
use tempfile;

fn get_runtime_types(lems: &mut LemsFile, nml: &[String]) -> Result<()> {
    // from main.rs .. should import actually
    neuroml::process_files(nml, |_, node| {
        if node.tag_name().name() == "ComponentType" {
            let ct: lems::raw::ComponentType = XML::from_node(node);
            lems.add_component_type(&ct)?;
        }
        Ok(())
    })
}

#[test]
fn hay11() -> Result<()> {
    let nml = String::from(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/hay11/TestL5PC.net.nml"
    ));
    let tmpdir = tempfile::TempDir::new()?;
    let dir = String::from(tmpdir.path().to_str().unwrap());
    let cxx = true;
    let py = true;
    let super_mechanisms = false;
    let cat_prefix = String::from("local_");
    let ions = vec![String::from("ca"), String::from("na"), String::from("k")];
    let mut lems = lems::file::LemsFile::core();
    get_runtime_types(&mut lems, &[nml.to_string()]).unwrap();
    bundle::export(
        &lems,
        &[nml],
        &ions[..],
        bundle::Bundle {
            dir: dir.to_string(),
            cxx,
            py,
            super_mechanisms,
            cat_prefix,
        },
    )?;
    let path_main_py = tmpdir.path().join("main.py");
    let main_py = std::fs::read_to_string(&path_main_py)?;
    let main_py = main_py.replace("sim.run(1000, 0.025)", "sim.run(10, 0.025)");
    std::fs::write(path_main_py, main_py)?;
    Command::new("python3")
        .args(["main.py", "dat/network_L5bPyrCellHayEtAl2011.json"])
        .current_dir(tmpdir.path())
        .status()?;
    assert!(tmpdir.path().join("result.pdf").exists());
    Command::new("bash")
        .args(["run.sh", "network_L5bPyrCellHayEtAl2011"])
        .current_dir(tmpdir.path())
        .status()?;
    tmpdir.close()?;
    Ok(())
}
