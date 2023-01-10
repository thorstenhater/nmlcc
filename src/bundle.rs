use crate::{
    acc::{self, Decor, Paintable, ParsedInhomogeneousParameter, Sexp},
    error::{Error, Result},
    expr::{Expr, Quantity, Stmnt},
    instance::{Collapsed, Context, Instance},
    lems::file::LemsFile,
    network::{self, get_cell_id, Connection, Network, Projection},
    neuroml::raw::{
        BiophysicalProperties, BiophysicalPropertiesBody, ChannelDensity, MembranePropertiesBody,
        PoissonFiringSynapse, PulseGenerator,
    },
    neuroml::{
        process_files,
        raw::{ChannelDensityNernst, ChannelDensityNonUniform, ChannelDensityNonUniformNernst},
    },
    nml2_error,
    nmodl::{self, Nmodl},
    xml::XML,
    Map, Set,
};
use serde::Serialize;
use serde_json;
use std::fs::{create_dir_all, write};
use tracing::{info, trace};

pub struct Bundle {
    pub dir: String,
    pub cxx: bool,
    pub py: bool,
    pub super_mechanisms: bool,
    pub cat_prefix: String,
}


pub fn export(
    lems: &LemsFile,
    nml: &[String],
    ions: &[String],
    cfg: Bundle,
) -> Result<()> {
    export_template(lems, nml, &cfg.dir)?;

    // We always export these to keep synapse etc alive
    nmodl::export(lems, nml, "-*", &format!("{}/cat", &cfg.dir), ions)?;

    if cfg.super_mechanisms {
        export_with_super_mechanisms(lems, nml, &cfg.dir, ions, &cfg.cat_prefix)?;
    } else {
        acc::export(lems, nml, &format!("{}/acc", &cfg.dir), ions, &cfg.cat_prefix)?;
    }
    if cfg.py {
        write(&format!("{}/main.py", &cfg.dir), mk_main_py()?)?;
    }
    if cfg.cxx {
        write(&format!("{}/run.sh", &cfg.dir), mk_bash()?)?;
        write(&format!("{}/CMakeLists.txt", &cfg.dir), mk_cmake()?)?;
        write(&format!("{}/main.cxx", &cfg.dir), mk_main_cxx()?)?;
    }
    Ok(())
}

type ConnectionData = (i64, String, String, String, f64, f64);

#[derive(Serialize)]
struct SimulationData {
    gid_to_cell: Vec<String>,
    cell_to_morph: Map<String, String>,
    gid_to_inputs: Map<i64, Vec<(i64, String, String)>>,
    gid_to_synapses: Map<i64, Vec<(i64, String, String)>>,
    gid_to_detectors: Map<i64, Vec<(i64, String)>>,
    gid_to_connections: Map<i64, Vec<ConnectionData>>,
    // Inputs & Stimuli
    i_clamps: Map<String, (f64, f64, f64)>,
    regular_generators: Map<String, ()>,
    poisson_generators: Map<String, (String, f64, f64)>,
    // cells
    count: usize,
}

impl SimulationData {
    fn new(
        cell_to_morph: &Map<String, String>,
        stimuli: &Map<String, Input>,
        net: &Network,
    ) -> Result<Self> {
        let mut gid = 0;
        let mut gid_to_cell = Vec::new();
        let mut pop_to_gid = Map::new();
        for (id, pop) in &net.populations {
            pop_to_gid.insert(id.clone(), gid);
            let cell = pop.component.to_string();
            for _ in &pop.members {
                gid_to_cell.push(cell.clone());
                gid += 1;
            }
        }
        let count = gid;

        let mut gid_to_inputs = Map::new();
        let mut gid_to_synapses: Map<_, Vec<_>> = Map::new();
        for network::Input {
            source,
            target,
            segment,
            fraction,
        } in &net.inputs
        {
            let (pop, id) = get_cell_id(target)?;
            let fst = pop_to_gid[&pop];
            let idx = if let Some(p) = net.populations.get(&pop) {
                p.members
                    .iter()
                    .position(|ix| id == *ix as i64)
                    .ok_or_else(|| nml2_error!("Bad index {id} in population {pop}."))?
            } else {
                return Err(nml2_error!("Indexing into an unknown population: {pop}."));
            };
            let gid: i64 = (fst + idx) as i64;
            let val = (*segment, fraction.to_string(), source.clone());
            gid_to_inputs.entry(gid).or_insert_with(Vec::new).push(val);
            match stimuli.get(source) {
                Some(Input::Poisson(synapse, _, _)) => {
                    gid_to_synapses.entry(gid).or_default().push((
                        *segment,
                        fraction.clone(),
                        synapse.clone(),
                    ));
                }
                None | Some(Input::Pulse(..)) => {},
            }
        }

        // In arbor
        let mut gid_to_detectors: Map<_, Vec<_>> = Map::new();
        let mut gid_to_connections = Map::new();
        for Projection {
            synapse,
            pre,
            post,
            connections,
        } in &net.projections
        {
            let pre = pop_to_gid[pre] as i64;
            let post = pop_to_gid[post] as i64;
            for Connection {
                from,
                to,
                weight,
                delay,
            } in connections
            {
                let from_gid = pre + from.cell;
                let to_gid = post + to.cell;
                gid_to_detectors
                    .entry(from_gid)
                    .or_default()
                    .push((from.segment, from.fraction.clone()));
                gid_to_synapses.entry(to_gid).or_default().push((
                    to.segment,
                    to.fraction.clone(),
                    synapse.clone(),
                ));
                gid_to_connections
                    .entry(to_gid)
                    .or_insert_with(Vec::new)
                    .push((
                        from_gid,
                        from.to_label(),
                        synapse.to_string(),
                        to.to_label(),
                        *weight,
                        *delay,
                    ));
            }
        }

        let mut i_clamps = Map::new();
        let mut poisson_generators = Map::new();
        let regular_generators = Map::new();
        for (lbl, stimulus) in stimuli {
            match stimulus {
                Input::Pulse(delay, dt, stop) => {
                    i_clamps.insert(lbl.to_string(), (*delay, *dt, *stop));
                }
                Input::Poisson(syn, avg, wgt) => {
                    poisson_generators.insert(lbl.to_string(), (syn.to_string(), *avg, *wgt));
                }
            }
        }

        Ok(SimulationData {
            gid_to_cell,
            cell_to_morph: cell_to_morph.clone(),
            gid_to_inputs,
            gid_to_synapses,
            gid_to_detectors,
            gid_to_connections,
            i_clamps,
            regular_generators,
            poisson_generators,
            count,
        })
    }
}

fn mk_bash() -> Result<String> {
   Ok("#!/usr/bin/env bash

set -euxo pipefail

cmake -S . -B build
cmake --build build
cp build/main .
arbor-build-catalogue local cat
./main $*
".to_string())

}

fn mk_cmake() -> Result<String> {
    Ok("
cmake_minimum_required(VERSION 3.22)
project(arbor-ring LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 17)

find_package(arbor REQUIRED)
find_package(nlohmann_json 3.11.0 REQUIRED)

add_executable(main main.cxx)
target_link_libraries(main PUBLIC arbor::arbor arbor::arborio arbor::arborenv nlohmann_json::nlohmann_json)
".to_string())
}

fn mk_main_cxx() -> Result<String> {
    Ok("
#include <unordered_map>
#include <vector>
#include <string>
#include <filesystem>
#include <iostream>
#include <fstream>
#include <iterator>

#include <arbor/context.hpp>
#include <arbor/load_balance.hpp>
#include <arbor/cable_cell.hpp>
#include <arbor/simulation.hpp>
#include <arbor/recipe.hpp>

#include <arborio/neuroml.hpp>
#include <arborio/cableio.hpp>

#include <nlohmann/json.hpp>
using json = nlohmann::json;

arb::mechanism_catalogue compile(const std::filesystem::path& here) {
    auto fn = here / \"local-catalogue.so\";
    return arb::load_catalogue(fn);
}

std::string read_file(const std::filesystem::path& fn) {
    auto fd = std::ifstream{fn};
    return std::string(std::istreambuf_iterator<char>{fd}, {});
}


arb::locset on_segment(size_t seg, const std::string& frac) {
    return arb::ls::on_components(std::stod(frac), arb::reg::named(std::to_string(seg)));
};

std::string mk_label(const std::string& pfx, size_t seg, const std::string& frac) {
    return pfx + \"@seg_\" + std::to_string(seg) + \"_frac_\" + frac;
}

arb::cell_local_label_type mk_lid(const std::string& l) {
     return {l, arb::lid_selection_policy::round_robin};
}

arb::cell_global_label_type mk_gid(arb::cell_gid_type c, const std::string& l) {
     return {c, {l, arb::lid_selection_policy::round_robin}};
}

struct recipe: public arb::recipe {

    recipe(const std::filesystem::path& cwd, const std::string& network): here{cwd} {
        auto cat = compile(here);
        gprop.default_parameters = arb::neuron_parameter_defaults;
        gprop.catalogue.import(cat, prefix);

        std::ifstream fd{(here / \"dat\" / network).replace_extension(\"json\")};
        auto data = json::parse(fd);

        gid_to_cell = data[\"gid_to_cell\"];
        cell_to_morph = data[\"cell_to_morph\"];
        i_clamps = data[\"i_clamps\"];
        poisson_generators = data[\"poisson_generators\"];
        regular_generators = data[\"regular_generators\"];
        for (const auto& [k, v]: data[\"gid_to_inputs\"].items()) gid_to_inputs[std::stoi(k)] = v;
        for (const auto& [k, v]: data[\"gid_to_synapses\"].items()) gid_to_synapses[std::stoi(k)] = v;
        for (const auto& [k, v]: data[\"gid_to_detectors\"].items()) gid_to_detectors[std::stoi(k)] = v;
        for (const auto& [k, v]: data[\"gid_to_connections\"].items()) gid_to_connections[std::stoi(k)] = v;
        count = data[\"count\"];
    }

    std::any get_global_properties(arb::cell_kind) const override { return gprop; }
    arb::cell_size_type num_cells() const override { return count; }
    std::vector<arb::probe_info> get_probes(arb::cell_gid_type gid) const override { return {arb::cable_probe_membrane_voltage{arb::ls::location(0, 0.5)}}; }

    arb::util::unique_any get_cell_description(arb::cell_gid_type gid) const override {
        const auto& cid = gid_to_cell.at(gid);
        const auto& mid = cell_to_morph.at(cid);

        auto mrf = read_file((here / \"mrf\" / mid).replace_extension(\"nml\"));
        auto acc = read_file((here / \"acc\" / cid).replace_extension(\"acc\"));

        auto nml = arborio::neuroml{mrf};
        auto morph = nml.morphology(mid, arborio::neuroml_options::allow_spherical_root);

        auto label = arb::label_dict{};
        label.set(\"all\", arb::reg::all());
        label.import(morph->segments);
        label.import(morph->named_segments);
        label.import(morph->groups);

        auto decor = std::get<arb::decor>(arborio::parse_component(acc)->component);

        if (gid_to_inputs.count(gid)) {
            for (const auto& [seg, frac, inp]: gid_to_inputs.at(gid)) {
                if (i_clamps.count(inp)) {
                    auto tag = on_segment(seg, frac);
                    auto lbl = mk_label(\"ic_\" + inp, seg, frac);
                    const auto& [lag, dur, amp] = i_clamps.at(inp);
                    decor.place(tag, arb::i_clamp{lag, dur, amp}, lbl);
                }
            }
        }

        if (gid_to_synapses.count(gid)) {
            for (const auto& [seg, frac, syn]: gid_to_synapses.at(gid)) {
                auto tag = on_segment(seg, frac);
                auto lbl = mk_label(\"syn_\" + syn, seg, frac);
                decor.place(tag, arb::synapse{prefix + syn}, lbl);
            }
        }

        if (gid_to_detectors.count(gid)) {
            for (const auto& [seg, frac]: gid_to_detectors.at(gid)) {
                auto tag = on_segment(seg, frac);
                auto lbl = mk_label(\"sd\", seg, frac);
                decor.place(tag, arb::threshold_detector{-40}, lbl); // TODO figure out a better threshold.
            }
        }

        return arb::cable_cell{morph->morphology, decor, label};
    }

    arb::cell_kind get_cell_kind(arb::cell_gid_type) const override { return arb::cell_kind::cable; }

    std::vector<arb::cell_connection> connections_on(arb::cell_gid_type gid) const override {
        std::vector<arb::cell_connection> res;
        if (gid_to_connections.count(gid)) {
            for (const auto& [src, dec, syn, loc, wgt, del]: gid_to_connections.at(gid)) {
                auto from = mk_gid(src, \"sd@\" + dec);
                auto to = mk_lid(\"syn_\" + syn + \"@\" + loc);
                res.emplace_back(from, to, wgt, del);
            }
        }
        return res;
    }

    std::vector<arb::event_generator> event_generators(arb::cell_gid_type gid) const override {
        std::vector<arb::event_generator> res;
        if (gid_to_inputs.count(gid)) {
            for (const auto& [seg, frac, inp]: gid_to_inputs.at(gid)) {
                if (poisson_generators.count(inp)) {
                    const auto& [syn, avg, wgt] = poisson_generators.at(inp);
                    auto tgt = mk_label(\"syn_\" + syn, seg, frac);
                    res.push_back(arb::poisson_generator(tgt, wgt, 0, avg, std::mt19937_64{gid}));
                }
                else if (regular_generators.count(inp)) {
                    throw std::runtime_error{\"Regular generators not implemented yet.\"};
                }
                else if (i_clamps.count(inp)) {
                    // OK, handled those above
                }
                else {
                    // throw std::runtime_error{\"Unknown generator type.\"};
                }
            }
        }
        return res;
    }

    std::vector<std::string> gid_to_cell;
    std::unordered_map<std::string, std::string> cell_to_morph;
    std::unordered_map<int, std::vector<std::tuple<int, std::string, std::string>>> gid_to_inputs;
    std::unordered_map<int, std::vector<std::tuple<int, std::string, std::string>>> gid_to_synapses;
    std::unordered_map<int, std::vector<std::tuple<int, std::string>>> gid_to_detectors;
    std::unordered_map<int, std::vector<std::tuple<int, std::string, std::string, std::string, double, double>>> gid_to_connections;
    std::unordered_map<std::string, std::tuple<double, double, double>> i_clamps;
    std::unordered_map<std::string, std::tuple<>> regular_generators;
    std::unordered_map<std::string, std::tuple<std::string, double, double>> poisson_generators;

    arb::cell_size_type count;
    std::string prefix = \"local_\";
    arb::cable_cell_global_properties gprop;

    std::filesystem::path here;
};

int main(int argc, char* argv[]) {
    if (argc != 2) {
        std::cerr << \"Usage: main <network>\\n\";
        return -42;
    }
    auto cwd = std::filesystem::path{argv[0]}.parent_path();
    auto net = std::string{argv[1]};
    auto ctx = arb::make_context();
    auto mdl = recipe(cwd, net);
    auto ddc = arb::partition_load_balance(mdl, ctx);
    auto sim = arb::simulation(mdl, ctx, ddc);

    sim.run(1000, 0.025);
}
".to_string())
}

fn mk_main_py() -> Result<String> {
    Ok("#!/usr/bin/env python3
import arbor as A

import subprocess as sp
from pathlib import Path
from time import perf_counter as pc
import sys
import json

here = Path(__file__).parent

def compile(here):
    here = here.resolve()
    fn   = here / 'local-catalogue.so'
    cat  = here / 'cat'
    recompile = False
    if fn.exists():
        for src in cat.glob('*.mod'):
            src = Path(src).resolve()
            if src.stat().st_mtime > fn.stat().st_mtime:
                recompile = True
                break
    else:
        recompile = True
    if recompile:
        sp.run(f'arbor-build-catalogue local {cat}', shell=True, check=True)
    return A.load_catalogue(fn)

class recipe(A.recipe):
    def __init__(self, network):
        A.recipe.__init__(self)
        self.seed = 42
        self.prefix = 'local_'
        self.props = A.neuron_cable_properties()
        cat = compile(here)
        self.props.catalogue.extend(cat, self.prefix)

        with open((here / network).with_suffix('.json')) as fd:
            data = json.load(fd)

        self.gid_to_cell = data['gid_to_cell']
        self.cell_to_morph = data['cell_to_morph']
        self.i_clamps = data['i_clamps']
        self.poisson_generators = data['poisson_generators']
        self.regular_generators = data['regular_generators']
        self.gid_to_inputs = data['gid_to_inputs']
        self.gid_to_synapses = data['gid_to_synapses']
        self.gid_to_detectors = data['gid_to_detectors']
        self.gid_to_connections = data['gid_to_connections']
        self.count = data['count']

    def num_cells(self):
        return self.count

    def cell_kind(self, _):
        return A.cell_kind.cable

    def cell_description(self, gid):
        cid = self.gid_to_cell[gid]
        mrf = self.cell_to_morph[cid]
        nml = A.neuroml(f'{here}/mrf/{mrf}.nml').morphology(mrf, allow_spherical_root=True)
        lbl = A.label_dict()
        lbl.append(nml.segments())
        lbl.append(nml.named_segments())
        lbl.append(nml.groups())
        lbl['all'] = '(all)'
        dec = A.load_component(f'{here}/acc/{cid}.acc').component
        dec.discretization(A.cv_policy_every_segment())
        if gid in self.gid_to_inputs:
            for seg, frac, inp in self.gid_to_inputs[gid]:
                tag = f'(on-components {frac} (region \"{seg}\"))'
                if inp in self.i_clamps:
                    lag, dur, amp = self.i_clamps[inp]
                    dec.place(tag, A.iclamp(lag, dur, amp), f'ic_{inp}@seg_{seg}_frac_{frac}')
        if gid in self.gid_to_synapses:
            for seg, frac, syn in self.gid_to_synapses[gid]:
                tag = f'(on-components {frac} (region \"{seg}\"))'
                dec.place(tag, A.synapse(self.prefix + syn), f'syn_{syn}@seg_{seg}_frac_{frac}')
        if gid in self.gid_to_detectors:
            for seg, frac in self.gid_to_detectors[gid]:
                tag = f'(on-components {frac} (region \"{seg}\"))'
                dec.place(tag, A.threshold_detector(-40), f'sd@seg_{seg}_frac_{frac}') # -40 is a phony value!!!
        return A.cable_cell(nml.morphology, dec, lbl)

    def probes(self, _):
        # Example: probe center of the root (likely the soma)
        return [A.cable_probe_membrane_voltage('(location 0 0.5)')]

    def global_properties(self, kind):
        return self.props

    def connections_on(self, gid):
        res = []
        if gid in self.gid_to_connections:
            for src, dec, syn, loc, w, d in self.gid_to_connections[gid]:
                conn = A.connection((src, A.cell_local_label(f'sd@{dec}', A.selection_policy.round_robin)), A.cell_local_label(f'syn_{syn}@{loc}', A.selection_policy.round_robin), w, d)
                res.append(conn)
        return res

    def event_generators(self, gid):
        res = []
        if gid in self.gid_to_inputs:
            for seg, frac, inp in self.gid_to_inputs[gid]:
                tag = f'(on-components {frac} (region \"{seg}\"))'
                if inp in self.poisson_generators:
                    syn, avg, wgt = self.poisson_generators[inp]
                    res.append(A.event_generator(f'syn_{syn}@seg_{seg}_frac_{frac}', wgt, A.poisson_schedule(0, avg, gid)))
                if inp in self.regular_generators:
                    raise \"oops\"
        return res

if len(sys.argv) != 2:
    print('Usage: main.py <network-name>')
    exit(-42)

ctx = A.context()
mdl = recipe(sys.argv[1])
ddc = A.partition_load_balance(mdl, ctx)
sim = A.simulation(mdl, ctx, ddc)
hdl = sim.sample((0, 0), A.regular_schedule(0.1))

print('Running simulation for 1s...')
t0 = pc()
sim.run(1000, 0.025)
t1 = pc()
print(f'Simulation done, took: {t1-t0:.3f}s')

print('Trying to plot...')
try:
  import pandas as pd
  import seaborn as sns

  for data, meta in sim.samples(hdl):
    df = pd.DataFrame({'t/ms': data[:, 0], 'U/mV': data[:, 1]})
    sns.relplot(data=df, kind='line', x='t/ms', y='U/mV', ci=None).savefig('result.pdf')
  print('Ok')
except:
  print('Failure, are seaborn and matplotlib installed?')
".to_string())
}

fn mk_mrf(id: &str, mrf: &str) -> String {
    format!(
        r#"<?xml version="1.0" encoding="UTF-8"?>

<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2  https://raw.githubusercontent.com/NeuroML/NeuroML2/master/Schemas/NeuroML2/NeuroML_v2beta3.xsd"
    id="{id}">
   {mrf}
</neuroml>
"#
    )
}

#[derive(Debug)]
#[non_exhaustive]
enum Input {
    Pulse(f64, f64, f64),
    Poisson(String, f64, f64),
}

fn export_template(lems: &LemsFile, nml: &[String], bundle: &str) -> Result<()> {
    trace!("Creating bundle {bundle}");
    create_dir_all(bundle)?;
    create_dir_all(format!("{bundle}/mrf"))?;
    create_dir_all(format!("{bundle}/acc"))?;
    create_dir_all(format!("{bundle}/cat"))?;
    create_dir_all(format!("{bundle}/dat"))?;

    let norm = |v: &str| -> Result<String> {
        let q = Quantity::parse(v)?;
        let u = lems.normalise_quantity(&q)?.value;
        Ok(format!("{u}"))
    };

    let mut inputs = Map::new();
    let mut cells = Map::new();
    let mut nets = Vec::new();
    process_files(nml, |_, node| {
        let doc = node.document().input_text();
        match node.tag_name().name() {
            "morphology" => {
                let id = node
                    .attribute("id")
                    .ok_or_else(|| nml2_error!("Morph has no id"))?;
                trace!("Writing morphology to {bundle}/mrf/{id}");
                write(
                    format!("{bundle}/mrf/{id}.nml"),
                    mk_mrf(id, &doc[node.range()]),
                )?;
            }
            "cell" => {
                let cell = node
                    .attribute("id")
                    .ok_or_else(|| nml2_error!("Cell has no id"))?;
                for mrf in node.children() {
                    if mrf.tag_name().name() == "morphology" {
                        let morph = mrf
                            .attribute("id")
                            .ok_or_else(|| nml2_error!("Morph has no id"))?;
                        cells.insert(cell.to_string(), morph.to_string());
                    }
                }
            }
            "pulseGenerator" => {
                let ic: PulseGenerator = XML::from_node(node);
                inputs.insert(
                    ic.id.to_string(),
                    Input::Pulse(
                        norm(&ic.delay)?.parse::<f64>().unwrap(),
                        norm(&ic.duration)?.parse::<f64>().unwrap(),
                        norm(&ic.amplitude)?.parse::<f64>().unwrap(),
                    ),
                );
            }
            "poissonFiringSynapse" => {
                let ic: PoissonFiringSynapse = XML::from_node(node);
                let weight = 1.0;
                inputs.insert(
                    ic.id.to_string(),
                    Input::Poisson(
                        ic.synapse,
                        norm(&ic.averageRate)?.parse::<f64>().unwrap(),
                        weight,
                    ),
                );
            }
            "network" => {
                let inst = Instance::new(lems, node)?;
                let net = Network::new(&inst)?;
                nets.push(net);
            }
            _ => {}
        }
        Ok(())
    })?;

    for net in &nets {
        write(
            format!("{bundle}/dat/{}.json", net.name),
            serde_json::to_string_pretty(&SimulationData::new(&cells, &inputs, net)?).unwrap(),
        )?;
    }
    Ok(())
}

#[derive(Clone, Debug)]
enum RevPot {
    Const(Quantity),
    Nernst,
}

#[derive(Clone, Debug)]
enum IonChannelConductanceParameter {
    DefaultConductance, // NonUniform doesn't give it
    FixedConductance(Quantity),
}

impl IonChannelConductanceParameter {
    fn parse(lems: &LemsFile, g: &Option<String>) -> Result<Self> {
        Ok(if let Some(g) = g {
            Self::FixedConductance(lems.normalise_quantity(&Quantity::parse(g)?)?)
        } else {
            Self::DefaultConductance
        })
    }
}

#[derive(Clone, Debug)]
pub struct IonChannel {
    name: String,
    reversal_potential: RevPot,
    conductance: IonChannelConductanceParameter,
}

pub fn export_with_super_mechanisms(
    lems: &LemsFile,
    nml: &[String],
    bundle: &str,
    ions: &[String],
    cat_prefix: &str,
) -> Result<()> {
    let cells = read_cell_data(nml, lems)?;
    let merge = build_super_mechanisms(&cells, lems, ions)?;

    for (id, cell) in merge {
        for (reg, chan) in cell.channels {
            let nmodl = nmodl::mk_nmodl(chan)?;
            let path = format!("{bundle}/cat/{id}_{reg}.mod");
            info!("Writing Super-Mechanism NMODL for cell '{id}' region '{reg}' to {path:?}",);
            write(&path, nmodl)?;
        }
        let path = format!("{bundle}/acc/{id}.acc");
        info!("Writing Super Mechanism ACC to {path:?}");
        let decor = cell
            .decor
            .iter()
            .map(|d| d.add_catalogue_prefix(cat_prefix))
            .collect::<Vec<_>>();
        write(&path, decor.to_sexp())?;
    }
    Ok(())
}

#[derive(Default)]
pub struct Cell {
    pub decor: Vec<Decor>,
    pub channels: Vec<(String, Nmodl)>,
}

pub fn build_super_mechanisms(
    cells: &CellData,
    lems: &LemsFile,
    ions: &[String],
) -> Result<Map<String, Cell>> {
    let sms = ion_channel_assignments(&cells.bio_phys, lems)?;
    let dec = split_decor(cells, lems, ions)?;
    let mrg = merge_ion_channels(&sms, cells, ions)?;

    Ok(dec
        .into_iter()
        .map(|(cell, decor)| {
            let channels = mrg.get(&cell).cloned().unwrap_or_default();
            (cell, Cell { decor, channels })
        })
        .collect())
}

#[derive(Default)]
pub struct CellData {
    pub bio_phys: Map<String, BiophysicalProperties>,
    pub density: Vec<Instance>,
    pub synapse: Vec<Instance>,
    pub c_model: Vec<Instance>,
    pub i_param: Map<String, Map<String, ParsedInhomogeneousParameter>>,
}

fn read_cell_data(nml: &[String], lems: &LemsFile) -> Result<CellData> {
    let mut result = CellData::default();
    process_files(nml, |_, node| {
        let tag = node.tag_name().name();
        if tag == "cell" {
            let id = node.attribute("id").ok_or(nml2_error!("Cell without id"))?;
            result
                .i_param
                .insert(id.to_string(), acc::parse_inhomogeneous_parameters(node)?);
            node.children()
                .find(|c| c.tag_name().name() == "biophysicalProperties")
                .into_iter()
                .for_each(|p| {
                    result.bio_phys.insert(id.to_string(), XML::from_node(&p));
                });
        } else if lems.derived_from(tag, "baseIonChannel") {
            result.density.push(Instance::new(lems, node)?);
        } else if lems.derived_from(tag, "concentrationModel") {
            result.c_model.push(Instance::new(lems, node)?);
        } else if lems.derived_from(tag, "baseSynapse") {
            result.synapse.push(Instance::new(lems, node)?);
        }
        Ok(())
    })?;
    Ok(result)
}

pub fn ion_channel_assignments(
    props: &Map<String, BiophysicalProperties>,
    lems: &LemsFile,
) -> Result<Map<(String, String), Vec<IonChannel>>> {
    use BiophysicalPropertiesBody::*;
    use MembranePropertiesBody::*;
    fn segment_group_or_all(x: &str) -> String {
        if x.is_empty() {
            String::from("all")
        } else {
            x.to_string()
        }
    }
    let mut result: Map<_, Vec<IonChannel>> = Map::new();
    for (id, prop) in props.iter() {
        for item in prop.body.iter() {
            if let membraneProperties(membrane) = item {
                for item in membrane.body.iter() {
                    match item {
                        channelDensity(ChannelDensity {
                            ionChannel,
                            condDensity: g,
                            erev,
                            segmentGroup,
                            ..
                        }) => {
                            let region = segment_group_or_all(segmentGroup);
                            let name = ionChannel.to_string();
                            let conductance = IonChannelConductanceParameter::parse(lems, g)?;
                            let reversal_potential =
                                RevPot::Const(lems.normalise_quantity(&Quantity::parse(erev)?)?);
                            result
                                .entry((id.to_string(), region))
                                .or_default()
                                .push(IonChannel {
                                    name,
                                    reversal_potential,
                                    conductance,
                                });
                        }
                        channelDensityNernst(ChannelDensityNernst {
                            ionChannel,
                            condDensity: g,
                            segmentGroup,
                            ..
                        }) => {
                            let region = segment_group_or_all(segmentGroup);
                            let name = ionChannel.to_string();
                            let conductance = IonChannelConductanceParameter::parse(lems, g)?;
                            let reversal_potential = RevPot::Nernst;
                            result
                                .entry((id.to_string(), region))
                                .or_default()
                                .push(IonChannel {
                                    name,
                                    reversal_potential,
                                    conductance,
                                });
                        }
                        channelDensityNonUniform(ChannelDensityNonUniform {
                            ionChannel,
                            body,
                            erev,
                            ..
                        }) => {
                            use crate::neuroml::raw::ChannelDensityNonUniformBody::variableParameter;
                            let variableParameter(vp) = &body.first().ok_or(nml2_error!(
                                "expected VariableParameter in ChannelDensityNonUniform"
                            ))?;
                            let name = ionChannel.to_string();
                            let region = segment_group_or_all(&vp.segmentGroup);
                            let conductance = IonChannelConductanceParameter::DefaultConductance;
                            let reversal_potential =
                                RevPot::Const(lems.normalise_quantity(&Quantity::parse(erev)?)?);
                            result
                                .entry((id.to_string(), region))
                                .or_default()
                                .push(IonChannel {
                                    name,
                                    reversal_potential,
                                    conductance,
                                });
                        }
                        channelDensityNonUniformNernst(ChannelDensityNonUniformNernst {
                            ionChannel,
                            body,
                            ..
                        }) => {
                            use crate::neuroml::raw::ChannelDensityNonUniformNernstBody::variableParameter;
                            let variableParameter(vp) = &body.first().ok_or(nml2_error!(
                                "expected VariableParameter in ChannelDensityNonUniform"
                            ))?;
                            let region = segment_group_or_all(&vp.segmentGroup);
                            let name = ionChannel.to_string();
                            let conductance = IonChannelConductanceParameter::DefaultConductance;
                            let reversal_potential = RevPot::Nernst;
                            result
                                .entry((id.to_string(), region))
                                .or_default()
                                .push(IonChannel {
                                    name,
                                    reversal_potential,
                                    conductance,
                                });
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    Ok(result)
}

fn split_decor(
    cells: &CellData,
    lems: &LemsFile,
    ions: &[String],
) -> Result<Map<String, Vec<Decor>>> {
    // Now splat in the remaining decor, but skip density mechs (these were merged)
    let densities = cells
        .density
        .iter()
        .map(|m| m.id.as_deref().unwrap().to_string())
        .collect::<Set<String>>();
    let ihp = &cells.i_param;
    let mut result: Map<String, Vec<Decor>> = Map::new();
    for (id, prop) in cells.bio_phys.iter() {
        let mut seen = Set::new();
        let mut sm = Vec::new();
        let biophys = acc::biophys(
            prop,
            lems,
            ions,
            ihp.get(id).ok_or(nml2_error!("should never happen"))?,
        )?;
        let mut non_uniform_args: Map<String, Map<String, acc::MechVariableParameter>> = Map::new();
        // collect non uniform args as they must be kept as PARAMETERS
        for d in biophys.iter() {
            match d {
                Decor::Paint(ref r, Paintable::NonUniformMech { ref name, ns, .. })
                    if densities.contains(name) =>
                {
                    for (k, v) in ns.iter() {
                        non_uniform_args
                            .entry(r.to_owned())
                            .or_insert_with(Map::new)
                            .insert(format!("{name}_{k}"), v.to_owned());
                    }
                }
                _ => (),
            }
        }
        for d in biophys {
            match d {
                Decor::Paint(r, Paintable::Mech(name, _)) if densities.contains(&name) => {
                    if !seen.contains(&r) {
                        if let Some(ns) = non_uniform_args.get(&r) {
                            sm.push(acc::Decor::non_uniform_mechanism(
                                &r,
                                &format!("{id}_{r}"),
                                &Map::new(),
                                ns,
                            ));
                        } else {
                            sm.push(acc::Decor::mechanism(&r, &format!("{id}_{r}"), &Map::new()));
                        }
                        seen.insert(r.to_string());
                    }
                }
                Decor::Paint(r, Paintable::NonUniformMech { name, .. })
                    if densities.contains(&name) =>
                {
                    if !seen.contains(&r) {
                        if let Some(ns) = non_uniform_args.get(&r) {
                            sm.push(acc::Decor::non_uniform_mechanism(
                                &r,
                                &format!("{id}_{r}"),
                                &Map::new(),
                                ns,
                            ));
                        } else {
                            panic!("no");
                        }
                        seen.insert(r.to_string());
                    }
                }
                _ => sm.push(d),
            }
        }
        result.insert(id.to_string(), sm);
    }
    Ok(result)
}

fn merge_ion_channels(
    channel_mappings: &Map<(String, String), Vec<IonChannel>>, // cell x region -> [channel]
    cell: &CellData,
    known_ions: &[String],
) -> Result<Map<String, Vec<(String, Nmodl)>>> {
    let mut result: Map<String, Vec<_>> = Map::new();
    let mut filter = String::from("-*");
    for ((id, reg), channels) in channel_mappings {
        let mut collapsed = Collapsed::new(&Some(format!("{id}_{reg}")));
        let mut ions: Map<_, Vec<_>> = Map::new();
        for channel in channels {
            for instance in cell.density.iter() {
                if instance.id == Some(channel.name.to_string()) {
                    let mut instance = instance.clone();
                    let ion = instance
                        .attributes
                        .get("species")
                        .cloned()
                        .unwrap_or_default();
                    // Set Parameters e, g
                    if let IonChannelConductanceParameter::FixedConductance(g) =
                        &channel.conductance
                    {
                        instance
                            .parameters
                            .insert(String::from("conductance"), g.clone());
                    } else {
                        filter.push_str(&format!(",+{}/conductance", channel.name));
                    }
                    if let RevPot::Const(q) = &channel.reversal_potential {
                        instance.parameters.insert(format!("e{ion}"), q.clone());
                        instance.component_type.parameters.push(format!("e{ion}"));
                    }
                    ions.entry(ion.clone())
                        .or_default()
                        .push(channel.name.clone());
                    collapsed.add(&instance, &Context::default(), None)?;
                }
            }
        }

        // Add iX
        let mut outputs = Map::new();
        let mut variables = Map::new();
        let mut ns = Vec::new();
        for (ion, mechs) in &ions {
            if !ion.is_empty() && known_ions.contains(ion) {
                // A non-empty, known ion species X maps to
                //   USEION X READ eX WRITE iX
                // and we can sum the conductivities M_g_X
                // where M is the mechanism
                // NOTE we can probably claim that an empty ion is never known...
                let ix = Stmnt::Ass(
                    format!("i{ion}"),
                    Expr::parse(&format!("g_{ion}*(v - e{ion})"))?,
                );
                outputs.insert(format!("i{ion}"), ix);
                let g = mechs
                    .iter()
                    .map(|m| format!("{m}_g"))
                    .collect::<Vec<_>>()
                    .join(" + ");
                let ig = Stmnt::Ass(format!("g_{ion}"), Expr::parse(&g)?);
                variables.insert(format!("g_{ion}"), ig);
            } else {
                // Anything else becomes
                //   NONSPECIFIC_CURRENT iX
                // and
                //   RANGE PARAMETER M_eX
                // where M is the mechanism's prefix
                // and we sum currents as sum(g_)
                let i = mechs
                    .iter()
                    .map(|m| format!("{m}_g*(v - {m}_e{ion})"))
                    .collect::<Vec<_>>()
                    .join(" + ");
                ns.push((format!("i{ion}"), i));
            }
        }

        match ns.as_slice() {
            [] => {}
            [(name, e)] => {
                let ix = Stmnt::Ass(name.to_owned(), Expr::parse(e)?);
                outputs.insert(name.to_owned(), ix);
            }
            ns => {
                // prevent outputting multiple NONSPECIFIC statements
                // collapse all into a single NONSPECIFIC_CURRENT i
                let i = ns
                    .iter()
                    .map(|(_, v)| v.to_owned())
                    .collect::<Vec<_>>()
                    .join(" + ");
                let ix = Stmnt::Ass(String::from("i"), Expr::parse(&i)?);
                outputs.insert(String::from("i"), ix);
            }
        }

        let mut n = nmodl::Nmodl::from(&collapsed, known_ions, &filter)?;
        n.add_outputs(&outputs);
        n.add_variables(&outputs);
        n.add_variables(&variables);
        result.entry(id.clone()).or_default().push((reg.clone(), n));
    }
    Ok(result)
}
