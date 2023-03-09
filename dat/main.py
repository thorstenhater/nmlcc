#!/usr/bin/env python3

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
        self.gid_to_inputs = { int(k): v for k, v in data['gid_to_inputs'].items() }
        self.gid_to_synapses = { int(k): v for k, v in data['gid_to_synapses'].items() }
        self.gid_to_detectors = { int(k): v for k, v in data['gid_to_detectors'].items() }
        self.gid_to_connections = { int(k): v for k, v in data['gid_to_connections'].items() }
        self.cell_to_morph = data['cell_to_morph']
        self.i_clamps = data['i_clamps']
        self.poisson_generators = data['poisson_generators']
        self.regular_generators = data['regular_generators']
        self.count = data['count']

    def num_cells(self):
        return self.count

    def cell_kind(self, _):
        return A.cell_kind.cable

    def cell_description(self, gid):
        cid = self.gid_to_cell[gid]
        mrf = self.cell_to_morph[cid]
        nml = A.neuroml(f'{here}/mrf/{cid}_{mrf}.nml').morphology(mrf, allow_spherical_root=True)
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
            for seg, frac, thr in self.gid_to_detectors[gid]:
                tag = f'(on-components {frac} (region \"{seg}\"))'
                dec.place(tag, A.threshold_detector(thr), f'sd@seg_{seg}_frac_{frac}')
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
                if inp in self.poisson_generators:
                    syn, avg, wgt = self.poisson_generators[inp]
                    res.append(A.event_generator(f'syn_{syn}@seg_{seg}_frac_{frac}', wgt, A.poisson_schedule(0, avg, gid)))
                elif inp in self.regular_generators:
                    raise NotImplementedError()
                else:
                    pass
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
