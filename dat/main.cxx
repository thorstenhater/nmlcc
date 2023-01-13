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
            for (const auto& [seg, frac, thr]: gid_to_detectors.at(gid)) {
                auto tag = on_segment(seg, frac);
                auto lbl = mk_label(\"sd\", seg, frac);
                decor.place(tag, arb::threshold_detector{thr}, lbl);
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
                auto delay = std::max(min_delay, del);
                auto to = mk_lid(\"syn_\" + syn + \"@\" + loc);
                res.emplace_back(from, to, wgt, delay);
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
    std::unordered_map<int, std::vector<std::tuple<int, std::string, double>>> gid_to_detectors;
    std::unordered_map<int, std::vector<std::tuple<int, std::string, std::string, std::string, double, double>>> gid_to_connections;
    std::unordered_map<std::string, std::tuple<double, double, double>> i_clamps;
    std::unordered_map<std::string, std::tuple<>> regular_generators;
    std::unordered_map<std::string, std::tuple<std::string, double, double>> poisson_generators;

    arb::cell_size_type count;
    std::string prefix = \"local_\";
    arb::cable_cell_global_properties gprop;

    std::filesystem::path here;

    double min_delay = 0.025;
};

int main(int argc, char* argv[]) {
    double dt = 0.025;  // ms
    double T  = 1000.0; // ms

    if (argc != 2) {
        std::cerr << \"Usage: main <network>\\n\";
        return -42;
    }
    auto cwd = std::filesystem::path{argv[0]}.parent_path();
    auto net = std::string{argv[1]};
    auto ctx = arb::make_context();
    auto mdl = recipe(cwd, net);
    mdl.min_delay = dt;
    auto ddc = arb::partition_load_balance(mdl, ctx);
    auto sim = arb::simulation(mdl, ctx, ddc);
    sim.set_binning_policy(arb::binning_kind::regular, dt);
    sim.run(T, dt);
}
