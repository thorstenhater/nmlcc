# Introduction

Compile NML2 definitions to data compatible with Arbor
<https://github.com/arbor-sim/arbor>. The `nmlcc` tool will consume a
list of LEMS files containing NML2 conformant `ComponentType`
definitions and a NeuroML2 file (`.nml`). From there, a series of
exporters can be used to produce a templated Arbor simulation that can
be customised to your needs.

## When Should I Use This Instead of `JNML`?

1.  If you are using Arbor anyway, this project produces output directly
    aimed at Arbor.
    -   No editing of NMODL files.
    -   No manual assigments of ion channels and parameters.
    -   Basic support for stimuli.
    -   Can emit a full simulation at once
2.  The performance of `nmlcc`'s NMODL channels is far superior, see
    below. Measured end-to-end simulation in Arbor (includes model
    construction) gives at least a 2x improvement. This is the result of
    automating lots of manual transformations we have learned while
    porting models to Arbor. In addition, mechanisms can be combined
    into '<u>super-mechansims</u>' (SM) per assigned segment group and
    parameters inlined where possible, giving access to constant folding
    (CF).
3.  If you are not using Arbor, you might still be able to profit from
    NMODL output. However this is not tested, use at your own risk.

## When Shouldn't I Use This?

1.  This project is quite early in its lifecycle, so expect bugs and
    missing features.
2.  If you are not using Arbor. While NMODL export might work for
    NEURON, we do not test this nor do we plan support for it.
3.  If you are not interested in multi-compartment models. Currently
    only MC-cells are supported.
4.  If you need automated export of networks. We are currently looking
    into this, but it is not yet supported.
5.  If you are reliant on other features of `jnml` and/or `jlems` that
    are not supported in `nmlcc`.

## Performance

Casual benchmarking on a 2018 i5 MacBook Pro gives these results

-   HH tutorial cell from
    `https://github.com/openworm/hodgkin_huxley_tutorial`
-   simulation settings: `t=1000 ms` and `dt=0.0025`
-   soma-only, `d=17.8um` discretized into `0.1um` segments

<!-- -->

-   Arbor 0.6 Release, single thread
    -   times are measured across `sim.run(...)`
    -   thus model building is included

| ``ARB_VECTORIZE``        | **OFF** t<sub>wall</sub>/s | Speed-up | **ON** t<sub>wall</sub>/s | Speed-up |
|--------------------------|----------------------------|----------|---------------------------|----------|
| jnml                     | 13.266                     | 1.0      | 7.988                     | 1.0      |
| nmlcc 0.2                | 5.934                      | 2.2      | 2.827                     | 2.8      |
| \+ CF + SM †             | 6.210                      | 2.1      | 2.727                     | 2.9      |
| Arbor HH                 | 6.376                      | 2.1      | 2.960                     | 2.7      |
| hand-optimised           | 5.829                      | 2.3      | 2.911                     | 2.7      |
| \+ CF                    | 5.731                      | 2.3      | 2.782                     | 2.9      |
| \+ SM                    | 5.212                      | 2.5      | 2.504                     | 3.2      |

† Built using `nmlcc bundle --super-mechanisms`

# Getting Started

Install a recent version of the Rust language, using `rustup` or your
favourite package manager.

Then, clone this repo and try an example

``` shell
git clone git@github.com:thorstenhater/nmlcc.git
cd nmlcc
cargo run -- nmodl --type=ionChannelHH example/nml-simple-ion-channels.xml
```

This will build the `nmlcc` compiler and all its dependencies, which can
take a bit. The final output should be a file `NaConductance.mod` in the
current directory.

For an introduction on how to run an example in Arbor, see the tutorial
in the `docs` directory. If you want an quick and easy way to convert
NML2 cells to an Arbor single cell model, take a look at the `bundle`
exporter
[here](https://github.com/thorstenhater/nmlcc#producing-a-ready-to-run-bundle-from-nml2-bundle).

# Usage

**Note** we use `nmlcc` as if calling the tool directly, when using
`cargo`, replace `nmlcc` with `cargo run -- [args]` instead.

## Generate `NMODL` from NeuroML2 Dynamics

`nmlcc nmodl <options> <input.nml>` generates NMODL files that can be
compiled into Arbor catalogues. Files will be written to `<id>.mod` in
the current directory where `<id>` is the nml2 component id.

### Options

`--type=<ct>`  
Gives a base `ComponentType` named `ct` from which the `.mod` files will
be built. If multiple instances deriving from `ct` are found, one NMODL
file is written per instance.

`--dir=<dir>`  
store ouput under this directory, defaults to current directory.

`--parameter=+p,-q,..`  
will choose parameters to retain as tweakable, defaults to `+*` keeping
all

-   `-q` excludes parameter `q` from the final list, unless overridden
-   `+p` similarly, will add `p`
-   a selector can <u>end</u> on wildcard `*` to select all suffixes
    -   a wildcard anywhere else will be considered a literal `*`
        character
    -   wildcards must be ordered from least to most specific, ie
        `foo_bar_*` must come <u>after</u> `foo_*` to have effect
-   consequently, `-q_*,+q_a_*,-q_a_b` will remove all parameters
    starting with `q_`, except if they start with `q_a`, but remove
    `q_a_b`.
-   when compiling channels derived from the following base types, we
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

## Exporting Cells to Arbor Cable Cell Format (`ACC`)

`nmlcc acc <options> <input.nml>` extracts a Arbor Cable Cell
description based on the `biophysicalProperties`. Output will be stored
as `<id>.acc` with `id` being the NML2 id.

### Options

`--cell=<id>`  
selects a cell for export by NML2 id, if not given all cells will be
processed

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

`nmlcc bundle <input.nml> <output>` combines the last two commands into
a convenient package. The NML2 file `<input.nml>` must contain all
morphologies needed for the relevant cells. It generates a directory
`<output>` and fills it like follows (`id` refers to the NML `id`
attribute found on the `cell` component)

`acc/*.acc`  
ACC files, one per cell found in `<input.nml>`, named `<id>.acc`.

`cat/*.nmodl`  
NMODL files, one per `ComponentType` derived from either
`baseIonChannel` or `baseSynapse`, with parameter filters set to `-*`.

`mrf/*.nml`  
NML2 files containing extracted morphologies, one per <u>cell</u>,
stored as `<id>.nml`

`main.<id>.py`  
template python script, one per `id`, to

1.  Build and install the catalogue from the NMODL file.
2.  Load the morphologies, parameter assignments, and labels.
3.  Construct and execute simulation

You will need to fill in a few bits, marked `<FIXME>`, namely

-   Locations for all stimuli (currently `PulseGenerator` only)
-   Probes to measure observables, an example is provided
-   Extraction of measurement traces
-   Tweak simulation time `t` and `dt`

### Options

`--super-mechanisms`  
try to produce combined ion-channels per segment group while inlining
all parameters. Can give a \~20-30% speed boost depending on your
problem.

# Current Limitations

-   units will not be treated completly, rather upon seeing a quantity,
    it will be converted to a 'blessed' unit for that dimension, eg
    `1 m` will become `100
     cm` internally. This can have some consequences for accuracy.
-   ACC export is only valid for Arbor `0.6`.
-   No support for networks.
-   Currently, running `nmlcc` is only possible from the top-level
    directory (git working copy).

# Bootstrapping the Compiler

This project comes with a pre-built data model in `src/lems/raw.rs` and
`src/neuroml/raw.rs`. If you change the underlying LEMS/NML2 definitions
or edit `src/schema.rs` you'll need to rebuild the data model by running
this command

``` shell
cargo run --bin schema
```

This will allow for tweaking the versions of the NML2/LEMS schemata or
adjusting them by hand.

The default state is produced by this script

``` shell
bash bootstrap.sh
```

which will

-   bring in the LEMS and NML2 schemata
-   (and **remove them** if present)
-   slightly modify both of them
-   build the data model from the schemata

By default the following definitions are used

NML2  
`development` branch; XSD `v2.2`

LEMS  
`development` branch; XSD `v0.7.6`

Afterwards, you will need to re-compile the `nmlcc` binary
(`cargo build` or `cargo run`).
