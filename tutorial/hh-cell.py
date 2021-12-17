#!/usr/bin/env python3
import arbor as A
from arbor import density as M, location as L

import pandas as pd
from pandas import DataFrame as DF
import seaborn as sns

# Print config
print(A.config())

# Read the NeuroML morphology from file.
fn = "../../hodgkin_huxley_tutorial/Tutorial/Source/hhcell.cell.nml"
nml = A.neuroml(fn).cell_morphology("hhcell", allow_spherical_root=True)

# Move morphology markings to a label dict
lbl = A.label_dict()
lbl.append(nml.segments())
lbl.append(nml.named_segments())
lbl.append(nml.groups())

# Manually transfer parameters from hhcell.cell.nml into a decor
dec = A.decor()

# biophysical properties from hhcell.cell.nml
dec.set_property(Vm=-65,   # mV
                 cm=1e-6,  # F/cm^2
                 rL=30,    # Ohm cm
                 tempK=None)

# again from hhcell.cell.nml
dec.set_ion('na', rev_pot= 50)
dec.set_ion('k',  rev_pot=-77)

# Paint channels on soma
dec.paint('(all)', M('pas/e=-54.387', {'g': 0.3}))              # (ms/cm^2) Take the default leak channel here to mix it up
dec.paint('(all)', M('kChan',         {'conductance': 120.0}))  # (mS/cm^2)
dec.paint('(all)', M('naChan',        {'conductance': 36.0}))   # (mS/cm^2)

# Stimuli from HHCellNetwork.net.nml
dec.place('(root)', A.iclamp(100, 100, current=0.1), "iclamp")

# Make simulation
cell = A.cable_cell(nml.morphology, lbl, dec)
sim = A.single_cell_model(cell)

# Add our catalogue
cat = A.load_catalogue('hh-catalogue.so')
cat.extend(A.default_catalogue(), '')
sim.catalogue = cat

# Probe voltage at center
sim.probe('voltage', where='(root)', frequency=50)

# Parameters from LEMS_HH_Simulation.xml
sim.run(450, 0.01)

df = pd.concat([DF({'t': t.time, 'U': t.value}) for t in sim.traces], ignore_index=True)
sns.relplot(data=df, kind="line", x="t", y="U").savefig('single_cell_nml.pdf')
