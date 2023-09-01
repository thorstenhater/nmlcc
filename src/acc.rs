use crate::{
    error::{Error, Result},
    expr::Expr,
    expr::Quantity,
    lems::file::LemsFile,
    neuroml::{
        process_files,
        raw::{
            BiophysicalProperties, BiophysicalPropertiesBody, ChannelDensity, ChannelDensityNernst,
            ChannelDensityNonUniform, ChannelDensityNonUniformNernst, ExtracellularProperties,
            InhomogeneousParameter, InitMembPotential, IntracellularProperties,
            IntracellularPropertiesBody, MembraneProperties, MembranePropertiesBody, Resistivity,
            Species, SpecificCapacitance, VariableParameter,
        },
    },
    nml2_error,
    xml::XML,
    Map, Set,
};

use std::fmt::Write as _;
use std::fs::write;
use std::path::PathBuf;
use tracing::{info, trace, warn};

#[derive(Debug, Default, PartialEq, PartialOrd)]
pub struct Cell {
    pub decor: Vec<Decor>,
    pub segments_groups: Map<String, Set<u64>>,
    pub spike_threshold: Option<f64>,
    pub morph_id: String,
}

impl Cell {
    fn append(&mut self, rhs: &Self) -> Result<()> {
        if self.spike_threshold.is_some() {
            if self.spike_threshold.is_some() && self.spike_threshold != rhs.spike_threshold {
                return Err(nml2_error!("Duplicated, mismatching spike threshold."));
            }
        } else {
            self.spike_threshold = rhs.spike_threshold;
        }
        self.decor.extend_from_slice(&rhs.decor[..]);
        self.segments_groups.extend(rhs.segments_groups.clone());
        Ok(())
    }
}

pub fn to_cell_list(lems: &LemsFile, nml: &[String], ions: &[String]) -> Result<Map<String, Cell>> {
    let mut cells = Map::new();
    let mut segment_groups: Map<String, Map<String, Set<u64>>> = Map::new();
    process_files(nml, |_, node| {
        // Fetch cell information
        // - bio-physical properties
        // - cell -> morphology mapping
        // NOTE we do _not_ collect inline segment group info as we might
        // encounter standalone morphologies + references
        if node.tag_name().name() == "cell" {
            if let Some(cid) = node.attribute("id") {
                let mut cell: Cell = Default::default();
                let inhomogeneous_parameters = if let Ok(ihp) = parse_inhomogeneous_parameters(node)
                {
                    ihp
                } else {
                    Map::new()
                };
                for bpp in node.children() {
                    if bpp.tag_name().name() == "biophysicalProperties" {
                        let tmp = &mut biophys(
                            &XML::from_node(&bpp),
                            lems,
                            ions,
                            &inhomogeneous_parameters,
                        )?;
                        cell.append(tmp)?;
                    }
                    if bpp.tag_name().name() == "morphology" {
                        if let Some(mid) = bpp.attribute("id") {
                            cell.morph_id = mid.to_string();
                        } else {
                            return Err(nml2_error!("Cell {cid} contains morphology w/o id"));
                        }
                    }
                }
                *cells.entry(cid.to_string()).or_default() = cell;
            }
        }
        // if we are in a morphology, either standalone or inline, collect
        // segment group info
        if node.tag_name().name() == "morphology" {
            if let Some(mid) = node.attribute("id") {
                for sg in node.children() {
                    if sg.tag_name().name() == "segmentGroup" {
                        if let Some(gid) = sg.attribute("id") {
                            for s in sg.children() {
                                if s.tag_name().name() == "member" {
                                    let sid =
                                        s.attribute("segment").unwrap().parse::<u64>().unwrap();
                                    segment_groups
                                        .entry(mid.to_string())
                                        .or_default()
                                        .entry(gid.to_string())
                                        .or_default()
                                        .insert(sid);
                                }
                            }
                        } else {
                            return Err(nml2_error!(
                                "Morphology {mid} contains segment group w/o id"
                            ));
                        }
                    }
                }
            }
        }
        Ok(())
    })?;
    // Now we have all info, and can resolve cell segment groups.
    for cell in cells.values_mut() {
        let mid = &cell.morph_id;
        if let Some(sgroups) = segment_groups.get(mid) {
            cell.segments_groups = sgroups.clone();
        }
    }
    Ok(cells)
}

pub fn parse_inhomogeneous_parameters(
    cell: &roxmltree::Node<'_, '_>,
) -> Result<Map<String, ParsedInhomogeneousParameter>> {
    let mut inhomogeneous_parameters = Map::new();
    let m = cell
        .children()
        .find(|n| n.has_tag_name("morphology"))
        .ok_or(nml2_error!("no morphology tag"))?;
    for ihp in m
        .children()
        .filter(|n| n.has_tag_name("segmentGroup"))
        .flat_map(|n| n.children())
        .filter(|n| n.has_tag_name("inhomogeneousParameter"))
    {
        if let Some(segment_group_id) = ihp.parent().and_then(|p| p.attribute("id")) {
            use crate::neuroml::raw::InhomogeneousParameterBody::*;
            use crate::neuroml::raw::{DistalDetails, ProximalDetails};
            let ihp: InhomogeneousParameter = XML::from_node(&ihp);
            if ihp.metric != "Path Length from root" {
                return Err(nml2_error!(
                    "Only path length from root is supported as inhomogeneous parameter metric"
                ));
            }
            let mut subtract_the_minimum = false;
            for elem in ihp.body {
                match elem {
                    proximal(ProximalDetails { translationStart }) => {
                        if translationStart == 0.0 {
                            subtract_the_minimum = true;
                        } else {
                            return Err(acc_unimplemented(
                                "Proximal translationStart must be 0 in InhomogeneousParameter",
                            ));
                        }
                    }
                    distal(DistalDetails { .. }) => return Err(acc_unimplemented(
                        "Endpoint normalization for inhomogeneous parameters is not yet supported",
                    )),
                }
            }
            let metric = if subtract_the_minimum {
                Expr::ProximalDistanceFromRegion(segment_group_id.to_string())
            } else {
                Expr::DistanceFromRoot()
            };

            inhomogeneous_parameters.insert(
                ihp.id,
                ParsedInhomogeneousParameter {
                    variable: ihp.variable,
                    region: segment_group_id.to_string(),
                    metric,
                },
            );
        } else {
            return Err(acc_unimplemented(
                "inhomogeneousParameter definition must be inside segmentGroup group with id",
            ));
        }
    }
    Ok(inhomogeneous_parameters)
}

pub fn export(
    lems: &LemsFile,
    nml: &[String],
    pfx: &str,
    ions: &[String],
    cat_prefix: &str,
) -> Result<()> {
    trace!("Creating path {}", pfx);
    std::fs::create_dir_all(pfx)?;
    let cells = to_cell_list(lems, nml, ions)?;
    for (name, cell) in cells {
        let mut file = PathBuf::from(pfx);
        file.push(name);
        file.set_extension("acc");
        info!("Writing ACC to {:?}", &file);
        let decor = cell
            .decor
            .iter()
            .map(|d| d.add_catalogue_prefix(cat_prefix))
            .collect::<Vec<_>>();
        write(&file, decor.to_sexp())?;
    }
    Ok(())
}

fn acc_unimplemented(f: &str) -> Error {
    Error::Acc {
        what: format!("Feature '{f}' not implemented for ACC export."),
    }
}

pub trait Sexp {
    fn to_sexp(&self) -> String;
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct ParsedInhomogeneousParameter {
    variable: String, // p
    region: String,   // apicalDends
    metric: Expr,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub struct MechVariableParameter {
    value: String, // 10 * p
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub enum Paintable {
    Xi(String, String),
    Xo(String, String),
    Ra(String),
    Vm(String),
    Cm(String),
    Er(String, String),
    Em(String, String),
    Mech(String, Map<String, String>),
    NonUniformMech {
        name: String,
        ps: Map<String, String>,
        ns: Map<String, MechVariableParameter>,
    },
}

impl Paintable {
    fn normalise(&self, lems: &LemsFile) -> Result<Self> {
        let norm = |v: &str| -> Result<String> {
            let q = Quantity::parse(v)?;
            let u = lems.normalise_quantity(&q)?;
            Ok(format!("{}", u.value))
        };
        let norm_map = |ps: &Map<String, String>| -> Result<Map<String, String>> {
            ps.iter()
                .map(|(k, v)| norm(v).map(|v| (k.to_string(), v)))
                .collect::<Result<_>>()
        };
        let r = match self {
            Paintable::Xi(i, v) => Paintable::Xi(i.clone(), norm(v)?),
            Paintable::Xo(i, v) => Paintable::Xo(i.clone(), norm(v)?),
            Paintable::Ra(v) => Paintable::Ra(norm(v)?),
            Paintable::Vm(v) => Paintable::Vm(norm(v)?),
            Paintable::Cm(v) => Paintable::Cm(norm(v)?),
            Paintable::Er(i, v) => Paintable::Er(i.clone(), norm(v)?),
            Paintable::Em(i, m) if m == "nernst" => Paintable::Em(i.clone(), m.clone()),
            Paintable::Em(i, m) => Paintable::Em(i.clone(), norm(m)?),
            Paintable::Mech(m, ps) => Paintable::Mech(m.clone(), norm_map(ps)?),
            Paintable::NonUniformMech { name: m, ps, ns } => Paintable::NonUniformMech {
                name: m.clone(),
                ps: norm_map(ps)?,
                ns: ns.clone(),
            },
        };
        Ok(r)
    }
}

impl Sexp for Expr {
    fn to_sexp(&self) -> String {
        fn op_to_sexp(op: &str, args: &[Expr]) -> String {
            format!(
                "({op} {})",
                args.iter()
                    .map(|x| x.to_sexp())
                    .collect::<Vec<_>>()
                    .join(" ")
            )
        }
        match self {
            Expr::F64(x) => format!("{x}"),
            Expr::Var(x) => x.to_string(),
            Expr::Add(x) => op_to_sexp("add", x),
            Expr::Mul(x) => op_to_sexp("mul", x),
            Expr::Pow(x) => op_to_sexp("pow", x),
            Expr::Exp(x) => format!("(exp {})", x.to_sexp()),
            Expr::Log(x) => format!("(log {})", x.to_sexp()),
            Expr::Sqrt(x) => format!("(sqrt {})", x.to_sexp()),
            Expr::Fun(nm, x) => {
                format!("({} {})", if nm == "H" { "step" } else { nm }, x.to_sexp())
            }
            Expr::ProximalDistanceFromRegion(region) => {
                format!("(proximal-distance (region \"{region}\"))")
            }
            Expr::DistanceFromRoot() => "(distance (root))".to_string(),
        }
    }
}

impl Sexp for String {
    fn to_sexp(&self) -> String {
        self.clone()
    }
}

impl<K, V> Sexp for Map<K, V>
where
    K: Sexp,
    V: Sexp,
{
    fn to_sexp(&self) -> String {
        self.iter()
            .map(|(k, v)| format!("(\"{}\" {})", k.to_sexp(), v.to_sexp()))
            .collect::<Vec<_>>()
            .join(" ")
    }
}

impl Sexp for Paintable {
    fn to_sexp(&self) -> String {
        match self {
            Paintable::Xi(i, v) => format!("(ion-internal-concentration \"{i}\" {v})"),
            Paintable::Xo(i, v) => format!("(ion-external-concentration \"{i}\" {v})"),
            Paintable::Er(i, v) => format!("(ion-reversal-potential \"{i}\" {v})"),
            Paintable::Em(i, v) => {
                format!("(ion-reversal-potential-method \"{i}\" (mechanism \"{v}/{i}\"))")
            }
            Paintable::Ra(v) => format!("(axial-resistivity {v})"),
            Paintable::Vm(v) => format!("(membrane-potential {v})"),
            Paintable::Cm(v) => format!("(membrane-capacitance {v})"),
            Paintable::Mech(m, gs) => format!("(density (mechanism \"{m}\" {}))", gs.to_sexp()),
            Paintable::NonUniformMech { name: m, ps, ns } => {
                let ns = ns
                    .iter()
                    .map(|(k, v)| (k.to_string(), v.value.to_string()))
                    .collect::<Map<String, String>>()
                    .to_sexp();
                format!(
                    "(scaled-mechanism (density (mechanism \"{m}\" {})) {ns})",
                    ps.to_sexp()
                )
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd)]
pub enum Decor {
    Default(Paintable),
    Paint(String, Paintable), // region -> what
}

impl Decor {
    fn new(g: &str, p: Paintable, f: bool) -> Self {
        if g.is_empty() || g == "all" {
            if f {
                Decor::Paint("all".to_string(), p)
            } else {
                Decor::Default(p)
            }
        } else {
            Decor::Paint(g.to_string(), p)
        }
    }

    pub fn add_catalogue_prefix(&self, pfx: &str) -> Self {
        match self {
            Self::Paint(r, Paintable::Mech(name, ps)) => Self::Paint(
                r.clone(),
                Paintable::Mech(format!("{pfx}{name}"), ps.clone()),
            ),
            Self::Paint(r, Paintable::NonUniformMech { name, ps, ns }) => Self::Paint(
                r.clone(),
                Paintable::NonUniformMech {
                    name: format!("{pfx}{name}"),
                    ps: ps.clone(),
                    ns: ns.clone(),
                },
            ),
            _ => self.clone(),
        }
    }

    fn normalise(&self, lems: &LemsFile) -> Result<Self> {
        match self {
            Decor::Default(p) => Ok(Decor::Default(p.normalise(lems)?)),
            Decor::Paint(r, p) => Ok(Decor::Paint(r.clone(), p.normalise(lems)?)),
        }
    }

    pub fn vm(group: &str, value: &str) -> Self {
        Decor::new(group, Paintable::Vm(value.to_string()), false)
    }

    pub fn xi(group: &str, ion: &str, value: &str) -> Self {
        Decor::new(
            group,
            Paintable::Xi(ion.to_string(), value.to_string()),
            false,
        )
    }

    pub fn xo(group: &str, ion: &str, value: &str) -> Self {
        Decor::new(
            group,
            Paintable::Xo(ion.to_string(), value.to_string()),
            false,
        )
    }

    pub fn ra(group: &str, value: &str) -> Self {
        Decor::new(group, Paintable::Ra(value.to_string()), false)
    }

    pub fn er(group: &str, ion: &str, value: &str) -> Self {
        Decor::new(
            group,
            Paintable::Er(ion.to_string(), value.to_string()),
            false,
        )
    }

    pub fn mechanism(group: &str, mech: &str, gs: &Map<String, String>) -> Self {
        Decor::new(group, Paintable::Mech(mech.to_string(), gs.clone()), true)
    }

    pub fn nernst(ion: &str) -> Self {
        Decor::new(
            "",
            Paintable::Em(ion.to_string(), "nernst".to_string()), // TODO This is poison
            false,
        )
    }

    pub fn non_uniform_mechanism(
        segment_group: &str,
        ion: &str,
        gs: &Map<String, String>,
        ns: &Map<String, MechVariableParameter>,
    ) -> Self {
        Decor::new(
            segment_group,
            Paintable::NonUniformMech {
                name: ion.to_string(),
                ps: gs.clone(),
                ns: ns.clone(),
            },
            true,
        )
    }

    pub fn cm(group: &str, value: &str) -> Self {
        Decor::new(group, Paintable::Cm(value.to_string()), false)
    }
}

impl Sexp for Decor {
    fn to_sexp(&self) -> String {
        match self {
            Decor::Default(i) => format!("(default {})", i.to_sexp()),
            Decor::Paint(r, i) => {
                format!("(paint (region \"{r}\") {})", i.to_sexp())
            }
        }
    }
}

impl Sexp for Vec<Decor> {
    fn to_sexp(&self) -> String {
        let mut result = String::from(
            "(arbor-component
  (meta-data (version \"0.1-dev\"))
  (decor
",
        );
        for it in self {
            writeln!(result, "    {}", it.to_sexp()).unwrap();
        }
        result.pop();
        result.push_str("))\n");
        result
    }
}

pub fn biophys(
    prop: &BiophysicalProperties,
    lems: &LemsFile,
    ions: &[String],
    inhomogeneous_parameters: &Map<String, ParsedInhomogeneousParameter>,
) -> Result<Cell> {
    use BiophysicalPropertiesBody::*;
    let mut result: Cell = Default::default();
    for item in &prop.body {
        match item {
            membraneProperties(m) => {
                let cell = &mut membrane(m, ions, inhomogeneous_parameters)?;
                result.append(cell)?;
            }
            intracellularProperties(i) => result.decor.append(&mut intra(i)?),
            extracellularProperties(e) => result.decor.append(&mut extra(e)?),
            property(_) | notes(_) | annotation(_) => {}
        }
    }
    for it in result.decor.iter_mut() {
        *it = it.normalise(lems)?;
    }
    Ok(result)
}

fn make_variable_parameter_map(
    vp: &VariableParameter,
    inhomogeneous_parameters: &Map<String, ParsedInhomogeneousParameter>,
) -> Result<Map<String, MechVariableParameter>> {
    use crate::neuroml::raw::VariableParameterBody::inhomogeneousValue;
    if vp.body.len() != 1 {
        return Err(acc_unimplemented(
            "InhomogeneousValue must contain a single InhomogeneousParameter",
        ));
    }
    let inhomogeneousValue(ival) = &vp.body.first().ok_or(nml2_error!(
        "expected InhomogeneousValue in VariableParameter"
    ))?;
    let ihv = inhomogeneous_parameters
        .get(&ival.inhomogeneousParameter)
        .ok_or(nml2_error!(
            "Inhomogeneous parameter definition {} not found",
            ival.inhomogeneousParameter
        ))?;

    let parameter = rename_cond_density_to_conductance(&vp.parameter);

    let expr = Expr::parse(&ival.value)?
        .map(&|ex| -> _ {
            if ex.is_var_with_name(&ihv.variable) {
                ihv.metric.clone()
            } else {
                ex.clone()
            }
        })
        .to_sexp();

    let instance = MechVariableParameter { value: expr };

    let ns = Map::from([(parameter, instance)]);
    Ok(ns)
}

fn rename_cond_density_to_conductance(x: &str) -> String {
    if x == "condDensity" {
        String::from("conductance")
    } else {
        x.to_string()
    }
}

fn membrane(
    membrane: &MembraneProperties,
    known_ions: &[String],
    inhomogeneous_parameters: &Map<String, ParsedInhomogeneousParameter>,
) -> Result<Cell> {
    use MembranePropertiesBody::*;
    let mut result: Cell = Default::default();
    for item in &membrane.body {
        match item {
            channelDensity(ChannelDensity {
                ionChannel,
                condDensity,
                erev,
                segmentGroup,
                ion,
                body,
                ..
            }) => {
                if !body.is_empty() {
                    return Err(acc_unimplemented(
                        "Non-empty body in ChannelDensityProperties",
                    ));
                }
                let mut gs = simple_ion(known_ions, &mut result.decor, ion, segmentGroup, erev)?;
                if let Some(g) = condDensity {
                    gs.insert(String::from("conductance"), g.clone());
                }
                result
                    .decor
                    .push(Decor::mechanism(segmentGroup, ionChannel, &gs));
            }
            channelDensityNernst(ChannelDensityNernst {
                ionChannel,
                condDensity,
                segmentGroup,
                ion,
                body,
                ..
            }) => {
                if !body.is_empty() {
                    return Err(acc_unimplemented("Non-empty body in ChannelDensityNernst"));
                }
                let gs = if let Some(g) = condDensity {
                    Map::from([(String::from("conductance"), g.clone())])
                } else {
                    Map::new()
                };
                result
                    .decor
                    .push(Decor::mechanism(segmentGroup, ionChannel, &gs));
                result.decor.push(Decor::nernst(ion));
            }
            channelDensityNonUniform(ChannelDensityNonUniform {
                ionChannel,
                ion,
                body,
                erev,
                ..
            }) => {
                use crate::neuroml::raw::ChannelDensityNonUniformBody::variableParameter;
                let variableParameter(vp) = &body.first().ok_or(nml2_error!(
                    "expected VariableParameter in ChannelDensityNonUniform"
                ))?;
                let ns = make_variable_parameter_map(vp, inhomogeneous_parameters)?;
                let ps = simple_ion(known_ions, &mut result.decor, ion, &vp.segmentGroup, erev)?;
                result.decor.push(Decor::non_uniform_mechanism(
                    &vp.segmentGroup,
                    ionChannel,
                    &ps,
                    &ns,
                ));
            }
            channelDensityNonUniformNernst(ChannelDensityNonUniformNernst {
                ionChannel,
                ion,
                body,
                ..
            }) => {
                use crate::neuroml::raw::ChannelDensityNonUniformNernstBody::variableParameter;
                let variableParameter(vp) = &body.first().ok_or(nml2_error!(
                    "expected VariableParameter in ChannelDensityNonUniformNernst"
                ))?;
                let ns = make_variable_parameter_map(vp, inhomogeneous_parameters)?;
                result.decor.push(Decor::nernst(ion));
                result.decor.push(Decor::non_uniform_mechanism(
                    &vp.segmentGroup,
                    ionChannel,
                    &Map::new(),
                    &ns,
                ));
            }
            spikeThresh(v) => result.spike_threshold = Some(Quantity::parse(&v.value)?.value),
            specificCapacitance(SpecificCapacitance {
                value,
                segmentGroup,
            }) => result.decor.push(Decor::cm(segmentGroup, value)),
            initMembPotential(InitMembPotential {
                value,
                segmentGroup,
            }) => result.decor.push(Decor::vm(segmentGroup, value)),
            channelPopulation(_)
            | channelDensityVShift(_)
            | channelDensityGHK(_)
            | channelDensityGHK2(_)
            | channelDensityNonUniformGHK(_) => {
                return Err(acc_unimplemented("Complex channel type"))
            }
        }
    }
    Ok(result)
}

fn intra(intra: &IntracellularProperties) -> Result<Vec<Decor>> {
    use IntracellularPropertiesBody::*;
    let mut result = Vec::new();
    for item in &intra.body {
        match &item {
            species(Species {
                ion: Some(ion),
                initialConcentration,
                initialExtConcentration,
                segmentGroup,
                concentrationModel,
                ..
            }) => {
                result.push(Decor::xi(segmentGroup, ion, initialConcentration));
                result.push(Decor::xo(segmentGroup, ion, initialExtConcentration));
                result.push(Decor::mechanism(
                    segmentGroup,
                    concentrationModel,
                    &Map::from([(
                        String::from("initialConcentration"),
                        initialConcentration.to_string(),
                    )]),
                ))
            }
            resistivity(Resistivity {
                value,
                segmentGroup,
            }) => result.push(Decor::ra(segmentGroup, value)),
            _ => {}
        }
    }
    Ok(result)
}

fn simple_ion(
    known_ions: &[String],
    result: &mut Vec<Decor>,
    ion: &str,
    group: &str,
    erev: &str,
) -> Result<Map<String, String>> {
    if known_ions.contains(&ion.to_string()) {
        for item in result.iter() {
            if let Decor::Paint(r, Paintable::Er(i, e)) = item {
                // We are trying to add the same key with a...
                if r == group && ion == i {
                    return if e == erev {
                        // ... matching value: SKIP
                        Ok(Default::default())
                    } else {
                        // ... mismatch: ERROR
                        Err(nml2_error!("Overwriting different Er[{ion}] on {group}."))
                    };
                }
            }
        }
        // New value: add it.
        result.push(Decor::er(group, ion, erev));
        Ok(Map::new())
    } else {
        Ok(Map::from([(
            format!("e{}", if ion == "non_specific" { "" } else { ion }),
            erev.to_string(),
        )]))
    }
}

fn extra(_: &ExtracellularProperties) -> Result<Vec<Decor>> {
    warn!("Not handling extracellular settings, if required please file an issue.");
    Ok(Vec::new())
}

#[test]
fn test_simple_ion() {
    let known_ions = ["k".to_string()];
    let mut result = Vec::new();
    assert!(simple_ion(&known_ions, &mut result, "k", "soma", "-80").is_ok());
    assert!(result.len() == 1);
    assert!(simple_ion(&known_ions, &mut result, "k", "soma", "-80").is_ok());
    assert!(result.len() == 1);
    assert!(simple_ion(&known_ions, &mut result, "k", "soma", "-90").is_err());
    assert!(result.len() == 1);
    assert!(simple_ion(&known_ions, &mut result, "k", "dend", "-90").is_ok());
    assert!(result.len() == 2);
    assert!(simple_ion(&known_ions, &mut result, "na", "soma", "-90")
        .and_then(|gs| if gs.len() == 1 {
            Ok(())
        } else {
            Err(nml2_error!("non_specific"))
        })
        .is_ok());
    assert!(result.len() == 2);
}
