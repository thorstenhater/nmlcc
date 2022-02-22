# Introduction

Compile NML2 definitions to data compatible with Arbor
<https://github.com/arbor-sim/arbor>. The `nmlcc` tool will consume a list of
LEMS files containing NML2 conformant `ComponentType` definitions and a NeuroML2
file (`.nml`). From there, a series of exporters can be used to produce a
templated Arbor simulation that can be customised to your needs.

## When Should I Use This Instead of `JNML`?

1. If you are using Arbor anyway, this project produces output directly aimed at
  Arbor.
    - No editing of NMODL files.
    - No manual assigments of ion channels and parameters.
    - Basic support for stimuli.
    - Can emit a full simulation at once
2. The performance of `nmlcc`'s NMODL channels is far superior, see below.
   Measured end-to-end simulation in Arbor (includes model construction) gives
   at least a 2x improvement. This is the result of automating lots of manual
   transformations we have learned while porting models to Arbor. In addition,
   mechanisms can be combined into 'super-mechansims' (SM) per assigned segment
   group and parameters inlined where possible, giving access to constant
   folding (CF).
3. If you are not using Arbor, you might still be able to profit from NMODL
   output. However this is not tested, use at your own risk. In general `nmlcc`
   produces simpler and cleaner NMODL files for what the models it understands
   than `jnml`.

## When Shouldn't I Use This?

1. This project is quite early in its lifecycle, so expect bugs and missing
  features.
2. If you are not using Arbor. While NMODL export might work for NEURON, we do
  not test this nor do we plan support for it.
3. If you are not interested in multi-compartment models. Currently only
  MC-cells are supported.
4. If you need automated export of networks. We are currently looking into this,
    but it is not yet supported.
5. If you are reliant on other features of `jnml` and/or `jlems` that are not
  supported in `nmlcc`.

## Performance

Casual benchmarking on a 2018 i5 MacBook Pro gives these results

- HH tutorial cell from `https://github.com/openworm/hodgkin_huxley_tutorial`
  - simulation settings: `t=1000 ms` and `dt=0.0025`
  - soma-only morphology, `d=17.8um` discretized into `0.1um` segments
  - three ion channels were assigned to the full cell: `k`, `na`, and `passive`
- Arbor 0.6 Release
  - Arbor was built from source using `-march=native` and the `release` profile.
  - using a single core of an 2018 Intel Core i5 
  - times are measured across `sim.run(...)`, thus model building is included

| `ARB_VECTORIZE`              | **OFF** t<sub>wall</sub>/s | Speed-up | **ON** t<sub>wall</sub>/s | Speed-up |
|------------------------------|----------------------------|----------|---------------------------|----------|
| jnml                         | 13.266                     | 1.0      | 7.988                     | 1.0      |
| nmlcc 0.2                    | 5.934                      | 2.2      | 2.827                     | 2.8      |
| \+ CF + SM †                 | 6.210                      | 2.1      | 2.727                     | 2.9      |
| Arbor HH                     | 6.376                      | 2.1      | 2.960                     | 2.7      |
| hand-optimised               | 5.829                      | 2.3      | 2.911                     | 2.7      |
| \+ CF                        | 5.731                      | 2.3      | 2.782                     | 2.9      |
| \+ SM                        | 5.212                      | 2.5      | 2.504                     | 3.2      |

† Built using `nmlcc bundle --super-mechanisms`

## Why is it so Fast?

`nmlcc` codifies a lot of the transformations on NMODL that we (the Arbor team)
have learned over the last years of using, porting, and optimising NMODL. Here
is an incomplete list

- Eliminate `RANGE` variables. Prefer (re-)computing everything locally instead,
  except the most expensive terms and those only if static.
- Similarly prefer `CONSTANT` over `PARAMETER`.
- Do not use `PROCEDURE`. These have to use `ASSIGNED RANGE` variables to return
  values. Inline the compuations instead or use `FUNCTION`.
  
It's a small set of quite obvious ideas once you have seen how NMODL is
implemented by Arbor. Most of this is made possible by looking at the whole
simulation encode in NML2 at once and building a bespoke models for Arbor from
it. Also, NML2 requires a tiny subset of NMODL to implement. In the past, we
applied this by hand to the `jnml` output, which is quite conservative.
Crucially, `nmlcc` automates this process.

We take this idea one step further in the `bundle` exporter, if given the
`--super-mechanisms` flag. There we group all density mechanisms assigned to a
common subset of the morphology and generate a single NMODL file per such
subset. Then, all parameter assigments are hard-coded into the NMODL output.
This opens up more opportunities for optimisation and eliminates calls from
Arbor into the mechanisms.

# Getting Started

Install a recent version of the Rust language, using `rustup` or your favourite
package manager. Then, clone this repo and try an example
``` shell
git clone git@github.com:thorstenhater/nmlcc.git --recursive
cd nmlcc
cargo run -- nmodl example/nml-simple-ion-channels.xml
```

This will build the `nmlcc` compiler and all its dependencies, which can take a
bit. The final output should be a file `NaConductance.mod` in the current
directory.

For an introduction on how to run an example in Arbor, see the tutorial in the
`docs` directory. If you want an quick and easy way to convert NML2 cells to an
Arbor single cell model, take a look at the `bundle` exporter
[here](https://github.com/thorstenhater/nmlcc#producing-a-ready-to-run-bundle-from-nml2-bundle).

# Usage

**Note** we use `nmlcc` as if calling the tool directly, when using `cargo`,
replace `nmlcc` with `cargo run -- [args]` instead.

## General Options

`-v/--verbose`  
Provide more output, defaults to warnings only, `-v` escalates to `INFO` and
`-vv` to `TRACE`.

## Generate `NMODL` from NeuroML2 Dynamics

`nmlcc nmodl <options> <input.nml ...>` generates NMODL files that can be
compiled into Arbor catalogues. Files will be written to `<id>.mod` in the
current directory where `<id>` is the NML2 component id.

All `ComponentType`s extending one of the following base classes will be turned
into an NMODL file
- `gapJunction` produces gap junction models.
- `baseSynapse` (excluding `gapJunction`) will be exported as synapse models.
- `ionChannelHH`, `ionChannel`, and `ionChannelKS` become conductance based
  mechanisms producing an ionic current.
- `concentrationModel` becomes a density mechanism writing internal and external
  ion concentrations. **In contrast to NML2 native models `nmlcc` replaces the
  `Ca` ion with the one speficied by the `species` attribute.**
  
File names will be chosen as `<id>.mod` where the `id` is the NML2 `id`
designator.

### Options

`--dir=<dir>`  
store ouput under this directory, defaults to current directory.

`--parameter=+p,-q,..`  
will choose parameters to retain as tweakable, defaults to `+*` keeping
all

- `-q` excludes parameter `q` from the final list, unless overridden
- `+p` similarly, will add `p`
- a selector can **end** on wildcard `*` to select all suffixes
    - a wildcard anywhere else will be considered a literal `*`
      character
    - wildcards must be ordered from least to most specific, ie `foo_bar_*`
      must come **after** `foo_*` to have effect
- consequently, `-q_*,+q_a_*,-q_a_b` will remove all parameters starting with
  `q_`, except if they start with `q_a`, but remove `q_a_b`.
- when compiling channels derived from the following base types, we
  will alter the parameter list slightly in order to play nicely with
  export to ACC
  `baseIonChannel`  
  `+conductance`, if non-specific currents are used `+conductance,+e`

  `baseVoltageDepSynapse`  
  `+gbase,+erev`

  `gapJunction`  
  `+weight,+conductance`

### Example: Export a Simple Exponential Synapse

``` shell
$> nmlcc nmodl --type=gapJunction --parameter='-*' example/nml-gap-junction.xml
$> cat gj1.mod
NEURON {
  SUFFIX gj1
  NONSPECIFIC_CURRENT i
  RANGE weight, conductance
}

PARAMETER {
  weight = 1
  conductance = 0.00000001 (mS)
}

BREAKPOINT {
  i = conductance * weight * (v_peer + -1 * v)
}
```

Despite being obviously auto-generated code, the produced NMODL files are quite
clean and easy to tune further, if you need to eeke out the last bit of
performance from your model.

## Exporting Cells to Arbor Cable Cell Format (`ACC`)

`nmlcc acc <options> <input.nml ...>` extracts a Arbor Cable Cell description
based on the `biophysicalProperties` found in the input file(s). Output will be
stored as `<id>.acc` with `id` being the NML2 id of the associated cells. If you
pass in a file containing whole `<network>` object, `nmlcc` will try to wire up
stimuli as well.

### Options

`--dir=<dir>`  
store ouput under this directory, defaults to current directory.

### Example: Fetch Parameter Assignments from a Simple Cell Model

``` shell
$> nmlcc acc --cell=hhcell example/nml-hh-cell.nml
$> cat hhcell.acc
(arbor-component
  (meta-data (version "0.1-dev"))
  (decor
    (paint (region "all") (density (mechanism "passiveChan" ("e" -54.387001037597656) ("conductance" 0.30000001192092896))))
    (default (ion-reversal-potential "na" 50))
    (paint (region "all") (density (mechanism "naChan" ("conductance" 120))))
    (default (ion-reversal-potential "k" -77))
    (paint (region "all") (density (mechanism "kChan" ("conductance" 36))))
    (default (membrane-capacitance 1))
    (default (membrane-potential -65.4000015258789))
    (default (axial-resistivity 0.029999999329447746))))

```

## Producing a Ready-to-Run Bundle from NML2

**Note**: `bundle` accepts a *single* NML2 file instead of a list like the
remainder of commands and expects to find at least a `cell` instance and
preferably a `network`. If no `network` is found, some settings will not be
available, eg adding ion species, concentration models, and temperature.

`nmlcc bundle <input.nml> <output>` combines the last two commands into a
convenient package. The NML2 file `<input.nml>` must contain all morphologies
needed for the relevant cells, prerably it should a complete `<network>`
definition. The bundle exporter generates a directory `<output>` and fills it
like follows (`id` refers to the NML `id` attribute found on the `cell`
component)

`acc/*.acc`  
ACC files, one per cell found in `<input.nml>`, named `<id>.acc`.

`cat/*.nmodl`  
NMODL files, one per `ComponentType` derived from either
`baseIonChannel` or `baseSynapse`, with parameter filters set to `-*`.

`mrf/*.nml`  
NML2 files containing extracted morphologies, one per `cell`, stored as
`<id>.nml`

`main.<id>.py`  
template python script, one per `id`, to

1. Build and install the catalogue from the NMODL file.
2. Load the morphologies, parameter assignments, and labels.
3. Connect stimuli, currently `PulseGenerator` only
4. Construct and execute a simulation of 1000ms

You will want to tweak in a few settings
- Probes to measure observables, by default the membrane potential at the soma
  is probed.
- Extraction of measurement traces, by default we try to import `seaborn` and
  `matplotlib` and plot the soma probe.
- Simulation settings; defaults are time `t=1000ms` and `dt=0.005ms`.


### Options

`--super-mechanisms`  
try to produce combined ion-channels per segment group while inlining
all parameters. Can give a \~20-30% speed boost depending on your
problem.

# Current Limitations

- units are not treated completly, rather upon seeing a quantity, it will be
  converted to a 'blessed' unit for that dimension, eg `1 m` will become `100
  cm` internally. This can have some consequences for accuracy.
- ACC export is only valid for Arbor `0.6`.
- No support for 
  - networks
  - simulations
  - cells other than the multi-compartment `<cell>` kind
- See also our issue tracker.

# Bootstrapping the Compiler

This project comes with a pre-built data model in `src/lems/raw.rs` and
`src/neuroml/raw.rs`. If you change the underlying LEMS/NML2 definitions
or edit `src/schema.rs` you'll need to rebuild the data model by running
this command
``` shell
cargo run --bin schema
```

This will allow for tweaking the versions of the NML2/LEMS schemata or adjusting
them by hand. The default state is produced by running this script
``` shell
bash bootstrap.sh
```
which will

- bring in the LEMS and NML2 schemata
- (and **remove them** if present)
- slightly modify both of them
- build the data model from the schemata

By default the following definitions are used

NML2  
`development` branch; XSD `v2.2`

LEMS  
`development` branch; XSD `v0.7.6`

Afterwards, you will need to re-compile the `nmlcc` binary (`cargo build` or
`cargo run`). Note that the core definitions found in
`ext/NeuroML2/NeuroML2CoreTypes` are embedded into the `nmlcc` binary. This
allows for moving it around without keeping track of NML2 definition.
