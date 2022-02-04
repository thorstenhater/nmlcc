#!/usr/bin/env python3
import arbor as A

import pandas as pd
import seaborn as sns

from time import perf_counter as pc
import subprocess as sp
from pathlib import Path

print(A.__config__)
print(A.__path__)

# Auto-generated file, please copy to eg main.py

here = Path(__file__).parent

def nml_load_cell():
    nml = A.neuroml(here / 'mrf' / 'hhcell.nml').cell_morphology("hhcell", allow_spherical_root=True)
    lbl = A.label_dict()
    lbl.append(nml.segments())
    lbl.append(nml.named_segments())
    lbl.append(nml.groups())
    lbl['all'] = '(all)'
    dec = A.load_component(here / 'acc' / 'hhcell.acc').component
    return nml.morphology, lbl, dec

def mk_cat():
    sp.run('arbor-build-catalogue local cat', shell=True, check=True)
    res = A.default_catalogue()
    cat = A.load_catalogue(here / 'local-catalogue.so')
    res.extend(cat, '')
    return res

morph, labels, decor = nml_load_cell()

# Place your stimuli here (add a locset and uncomment)
# decor.place('<FIXME>', A.iclamp(100, 100, 0.10000000149011612), 'pulseGen1')
# decor.place('<FIXME>', A.iclamp(300, 100, 0.3499999940395355), 'pulseGen2')
decor.place('(location 0 0.5)', A.iclamp(100, 100, 0.1),  "stim1")
decor.place('(location 0 0.5)', A.iclamp(300, 100, 0.35), "stim2")

decor.discretization(A.cv_policy_max_extent(0.1))

cell = A.cable_cell(morph, labels, decor)
sim  = A.single_cell_model(cell)

sim.properties.catalogue = mk_cat()

# Add probes here (example below)
# sim.probe('voltage', '<FIXME>', frequency=10)
sim.probe('voltage', '(location 0 0.5)', frequency=10)

t0 = pc()
sim.run(1000, 0.0025)
t1 = pc()
print(t1 - t0)

tr = sim.traces[0]
df = pd.DataFrame({'t/ms': tr.time,
                   'U/mV': tr.value})

sns.relplot(data=df, kind="line", x="t/ms", y="U/mV", ci=None).savefig('hhcell.pdf')
