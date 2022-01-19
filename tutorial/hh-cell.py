#!/usr/bin/env python3
import arbor as A

def nml_load_cell(fn, cid):
    nml = A.neuroml(fn).cell_morphology(cid, allow_spherical_root=True)
    lbl = A.label_dict()
    lbl.append(nml.segments())
    lbl.append(nml.named_segments())
    lbl.append(nml.groups())
    lbl['all'] = "(all)"
    return nml.morphology, lbl

def mk_cat(*cats):
    res = A.default_catalogue()
    for cat in cats:
        res.extend(A.load_catalogue(cat), '')
    return res

dec = A.load_component('hhcell.acc').component
dec.place('(root)', A.iclamp(100, 100, current=0.1), "iclamp")
morph, lbl = nml_load_cell("../hodgkin_huxley_tutorial/Tutorial/Source/hhcell.cell.nml", "hhcell")
cell = A.cable_cell(morph, lbl, dec)
sim = A.single_cell_model(cell)
sim.catalogue = mk_cat('hh-catalogue.so')
sim.probe('voltage', where='(root)', frequency=50)

sim.run(450, 0.01)
