#!/usr/bin/env python3
import arbor as A

import subprocess as sp
from pathlib import Path
from time import perf_counter as pc

import pandas as pd
import seaborn as sns

# Auto-generated file, please copy to eg main.py

here = Path(__file__).parent

def nml_load_cell(cid):
    nml = A.neuroml(str(here / 'morph.nml')).cell_morphology(cid, allow_spherical_root=True)
    lbl = A.label_dict()
    lbl.append(nml.segments())
    lbl.append(nml.named_segments())
    lbl.append(nml.groups())
    lbl['all'] = '(all)'
    dec = A.load_component(str(here / 'acc' / f'{cid}.acc')).component
    return nml.morphology, lbl, dec

def mk_cat():
    global cat
    global res
    sp.run('arbor-build-catalogue local cat', shell=True, check=True)
    res = A.default_catalogue()
    cat = A.load_catalogue(str(here / 'local-catalogue.so'))
    res.extend(cat, '')
    return res

# Enter cell id here
cid = 'hhcell'

morph, labels, decor = nml_load_cell(cid)

# Add things to the decor here
decor.place('(location 0 0.5)', A.iclamp(100, 100, 0.1),  "stim1")
decor.place('(location 0 0.5)', A.iclamp(300, 100, 0.35), "stim2")

# decor.discretization(A.cv_policy_every_segment())
decor.discretization(A.cv_policy_max_extent(0.1))

cell = A.cable_cell(morph, labels, decor)
sim  = A.single_cell_model(cell)

sim.properties.catalogue = mk_cat()
sim.probe('voltage', '(location 0 0.5)', frequency=10)

# Now run the simulation
t0 = pc()
sim.run(1000, 0.0025)
t1 = pc()

print(t1 - t0)

tr = sim.traces[0]
df = pd.DataFrame({'t/ms': tr.time,
                   'U/mV': tr.value})

sns.relplot(data=df, kind="line", x="t/ms", y="U/mV",ci=None).savefig('hhcell.pdf')
