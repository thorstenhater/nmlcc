Note: This file also exists in Markdown format in this repo, generated
at an unspecified point in time with
`pandoc -f org -t gfm -o nml.md nml.org`.

# Introduction

This document describes my current knowledge of NeuroML2 and in a way
aims to replace its missing developer documentation. It is likely flawed
and constantly evolving. However, this text is needed as the actual
projects' documentation is sparse and sometimes confusing and, most
importantly, thin on the high concepts.

NeuroML2 is an effort to provide portable descriptions of
neuro-scientific simulations, including networks, and morphologically
detailled neurons. NeuroML2 supersedes NeuroML1 and is not a simple
iteration. Therefore, NeuroML will be used synonymously with NeuroML2
and abbreviated as NML or NML2.

Despite its flaws, NeuroML2 is a valiant effort and an important feature
to support.

# NeuroML2 and LEMS

**TL;DR**: NeuroML2 is a library of definitions written in LEMS, which
is written in XML. All NeuroML2 is LEMS.

Here I lay out the overall look and feel of NeuroML2, links to in-depth
sections follow.

NeuroML2 is – in contrast to v1 – implemented in LEMS[^1], modelling
language for chemical, physical, and biological processes. Furthermore,
NML2 has a kind of symbiotic relationship with LEMS in the large, which
will be explored in depth later.

Both NML2 and LEMS are written in XML, making the human interaction
slightly awkward. Schemata (XSD) exist in the relevant git repositories.
These form the basis of our exploration of NML2.

The fundamental relationship between LEMS and NML2 is this: every
functional item in NML2 has a matching definition in LEMS. So, to
interpret a NML2 file, we need to look up these definitions in the
corresponding LEMS files. An example might look like this

-   LEMS definition

    ``` xml
    <ComponentType name="ion_channel">
        <Parameter name="g" dimension="conductance"/>
    </ComponentType>
    ```

-   NML2 file

    ``` xml
    <ion_channel g="5 uS" />
    ```

The tag `<ComponentType name="foo">` introduces a new type, which can be
instanced by using `<Component type="foo" id="bar">` or the shorthand
`<foo
id="bar">`, the latter being the idiomatic use, especially in NML2. For
brevity, I skipped the definition of units and dimensions. Because of
this model, no discussion (or implementation) can proceed without
constantly switching between LEMS and NML2.

Both LEMS and NML2 follow an inheritance model akin to object-oriented
programming. This means

-   A `ComponentType` may specify its 'base class' using
    `extends="base"`.
-   Whenever a value of type `base` is required, a derived type may be
    substituted.
-   *Thankfully* we only have to deal with single inheritance

Again an example

-   LEMS definition

    ``` xml
    <ComponentType name="gate" />

    <ComponentType name="hh_exp_gate" extends="gate" />
    <ComponentType name="hh_lin_gate" extends="gate" />

    <ComponentType name="ion_channel">
      <Children name="gates" type="gate"/>
    </ComponentType>
    ```

-   NML2

    ``` xml
    <ion_channel>
      <hh_exp_gate id="exp" />
      <hh_lin_gate id="lin" />
      <hh_lin_gate id="lin'" />
    </ion_channel>
    ```

There is also the concept of public and private data members. In
particular access is mediated by 'exposures', which declare externally
readable members.

LEMS:

``` xml
<ComponentType name="q10_settings">
  <Exposure name="q10" dimension="none"/>
</ComponentType>

<ComponentType name="q10_fix" extends="base_q10">
  <Parameter name="fixed_q10" dimension="none"/>
  <Dynamics>
    <DerivedVariable name="internal_q10" exposure="q10" dimension="none" value="fixed_q10"/>
  </Dynamics>
</ComponentType>
```

Here we also introduced the tags `Parameter` (externally settable value)
and `DerivedVariable` (dependent quantity). `Parameter` values are set
using XML attributes. The internal variable `q10_fix` is only readable
through the exposure `q10`, even in enclosing scopes.

NML2:

``` xml
<foo>
  <q10_fix id="fixed" q10_fix="42">
  <Dynamics>
    <DerivedVariable name="my_q10" dimension="none" select="q10_fix/q10"/>
  </Dynamics>
</foo>
```

Again we encounter two new bits of related functionality: paths and
select. The rules are as follows:

If a `ComponentType` declares a `Child` of type `type` with an exposed
value `name`, that value can be used via `select` by addressing the
*path* `type/name`. `Child` implies a single instance of type `type` to
be used. However, multiple instances can be part of a `ComponentType`,
which is declared using `Children`. Addressing values from children is
done slightly differently. A *single* child's value is addressed using
`select="child_id/name"`. It is possible to accumulate over children
using `select="children[*]/name
reduce="multiply"` or `select="children[*]/name reduce="add"`. More
complicated selection is possible, see later.

Example, assume the definitions from above being available

LEMS:

``` xml
<ComponentType name="q10_user">
  <Children name="q10s" type="q10_settings">

  <Dynamics>
    <DerivedVariable name="prod_q10"  dimension="none" select="q10[*]/q10" reduce="multiply"/>
    <DerivedVariable name="first_q10" dimension="none" select="fixed_1/q10" />
  </Dynamics>
</ComponentType>
```

NML2:

``` xml
<q10_user>
  <q10_fix id="fixed_1" q10_fix="1">
  <q10_fix id="fixed_2" q10_fix="2">
  <q10_fix id="fixed_3" q10_fix="4">
  <q10_fix id="fixed_4" q10_fix="8">
</q10_user>
```

Here, `prod_q10=64` and `first_q10=1`, although is general there is no
way of knowing `fixed_1` being present, unless enforced otherwise.

# Inheritance

As alluded to above, the LEMS/NML2 model for describing relations
between components is similar to the 'is-a' type inheritance model used
in Python or C++. To illustrate, we use the HH model from the NML2
'stdlib found in `Channels.xml`

``` xml
<ComponentType name="baseVoltageDepRate">
  <Exposure name="r" dimension="per_time"/>
  <Requirement name="v" dimension="voltage"/>
</ComponentType>

<ComponentType name="baseHHRate" extends="baseVoltageDepRate">
  <Parameter name="rate" dimension="per_time"/>
  <Parameter name="midpoint" dimension="voltage"/>
  <Parameter name="scale" dimension="voltage"/>
</ComponentType>

<ComponentType name="HHExpRate" extends="baseHHRate">
  <Dynamics>
    <DerivedVariable name="r" exposure="r" value="rate * exp((v - midpoint)/scale)" dimension="per_time"/>
  </Dynamics>
</ComponentType>

<ComponentType name="HHSigmoidRate" extends="baseHHRate">
  <Dynamics>
    <DerivedVariable name="r" exposure="r" value="rate / (1 + exp(0 - (v - midpoint)/scale))" dimension="per_time"/>
  </Dynamics>
</ComponentType>

<ComponentType name="HHExpLinearRate" extends="baseHHRate">
  <Dynamics>
    <DerivedVariable name="x" value="(v - midpoint) / scale" dimension="none"/>
    <ConditionalDerivedVariable name="r" exposure="r" dimension="per_time">
      <Case condition="x .neq. 0" value="rate * x / (1 - exp(0 - x))"/>
      <Case condition="x .eq. 0"  value="rate"/>
    </ConditionalDerivedVariable>
  </Dynamics>
</ComponentType>
```

Note the following consequences of the 'is-a' model

-   derived items have access to their bases' `Parameter` values.
-   types can have a `Requirement` on the presence of certain variable
    in the surrounding scope.
-   `HHExpRate` can be inserted instead of a `baseHHRate` **or**
    `baseVoltageDepRate`.

**QUESTION** from the examples and discussions I have the impression
that having a `Dynamics` item in both base and derived items will cause
the item from base to overwritten. Is this correct and where is it
documented? **ANSWER** Correct, but nowhere written.

# Recap So Far

As we have learned in the sections above, NML2 is *written in* LEMS,
where the salient definitions can be found in the LEMS files provided
within the NeuroML2 repositories. This means each XML tag in an NML
document, eg `<ionChannelHH>`, can be looked up in the corresponding
LEMS file, in this case `Channels.xml`.

In addition, both LEMS and NML2 have – minimal, ie a working NML2/LEMS
document will likely validate, but not every validating document is
working NML2/LEMS – XSD schemata attached.[^2]

To compose a concrete component from an NML2 document we will be
required to

-   parse the related LEMS files
-   build an inheritance tree (since requirements are in terms of 'base
    classes')
-   extract dynamics and other items from the LEMS descriptions
-   obtain parameters and similar from the NML2 XML node
-   recursively instantiate children of the object

From there, we can interpret the structure and integrate the equations
of motion or produce an optimised represe first and interpret the result
or export it to another format like native code or NMODL.

# A Larger Example

We are almost in a position to decode our first practical example

``` xml
<?xml version="1.0" encoding="UTF-8"?>
<neuroml xmlns="http://www.neuroml.org/schema/neuroml2"
         xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         xsi:schemaLocation="http://www.neuroml.org/schema/neuroml2  ../Schemas/NeuroML2/NeuroML_v2beta4.xsd"
         id="NML2_SimpleIonChannel">
    <ionChannelHH id="NaConductance" conductance="10pS" species="na">
        <gateHHrates id="m" instances="3">
            <forwardRate type="HHExpLinearRate" rate="1per_ms" midpoint="-40mV" scale="10mV"/>
            <reverseRate type="HHExpRate" rate="4per_ms" midpoint="-65mV" scale="-18mV"/>
        </gateHHrates>
        <gateHHrates id="h" instances="1">
            <forwardRate type="HHExpRate" rate="0.07per_ms" midpoint="-65mV" scale="-20mV"/>
            <reverseRate type="HHSigmoidRate" rate="1per_ms" midpoint="-35mV" scale="10mV"/>
        </gateHHrates>
    </ionChannelHH>
</neuroml>
```

To do so, we have to find the LEMS definitions, of which will we use
simplified versions.

First, we take a look at the ion channel.

``` xml
<ComponentType name="baseIonChannel">
  <Parameter name="conductance" dimension="conductance"/>
  <Exposure name="g" dimension="conductance"/>
  <Exposure name="fopen" dimension="none"/>
  <Requirement name="v" dimension="voltage"/>
</ComponentType>

<ComponentType name="ionChannelHH" extends="baseIonChannel">
  <Children name="gates" type="gate"/>
  <Dynamics>
    <DerivedVariable name="fopen" dimension="none" select="gates[*]/fcond" reduce="multiply"/>
    <DerivedVariable name="g" exposure="g" value="conductance * fopen" dimension="conductance"/>
  </Dynamics>
</ComponentType>
```

Recall the definitions for the `rate` hierarchy above. Finally, we need
to inspect the `gates`

``` xml
<ComponentType name="gate">
    <Parameter name="instances" dimension="none"/>
    <Exposure name="fcond" dimension="none"/>
    <Exposure name="q" dimension="none"/>
</ComponentType>

<ComponentType name="gateHHrates" extends="gate">
  <Child name="forwardRate" type="baseVoltageDepRate"/>
  <Child name="reverseRate" type="baseVoltageDepRate"/>

  <Exposure name="alpha" dimension="per_time"/>
  <Exposure name="beta"  dimension="per_time"/>
  <Exposure name="tau"   dimension="time"/>
  <Exposure name="inf"   dimension="none"/>

  <Dynamics>
    <StateVariable name="q" dimension="none" exposure="q"/>
    <DerivedVariable name="alpha" dimension="per_time" exposure="alpha" select="forwardRate/r"/>
    <DerivedVariable name="beta"  dimension="per_time" exposure="beta"  select="reverseRate/r"/>
    <DerivedVariable name="fcond" dimension="none"     exposure="fcond" value="q^instances"/>
    <DerivedVariable name="inf"   dimension="none"     exposure="inf"   value="alpha/(alpha+beta)"/>
    <DerivedVariable name="tau"   dimension="time"     exposure="tau"   value="1/(alpha+beta)"/>

    <TimeDerivative variable="q" value="(inf - q) / tau"/>

    <OnStart>
      <StateAssignment variable="q" value="inf"/>
    </OnStart>
  </Dynamics>
</ComponentType>
```

Finally, we can put this together to extract the meaning of
`gateHHrates`

where *a* and *b* are defined by the `forwardRate` and `reverseRate`
items. Now, we can compose this into `ionChannelHH` while simplifying
the equations and renaming quantities

NB. while we *are* using the same symbols here, *r*<sub>*e**x**p*</sub>
signifies two independent quantities in the equations for *m* and *h*.

As notational convenience, we used greek letters for parameters.
Unsurprisingly, we find the HH neuron model here.

# How to Interpret NML2

In the last part we saw that *intuitively* NML2/LEMS is quite
straightforward to interpret. However, we will need to formalise the
process to be able to deal with NML2 in general. Starting with this
section, the document will delve into the technical details. Our current
goal will be to write a tool able to translate at least the
`ionChannelHH` model into a symbolic description. Although NML2 can be
extended using LEMS, we will use the static schema provided in the NML2
repository as is.

Here is a list of invariants we need to uphold

-   Each type must be aware of its base type, eg `gateHHrates <- gate`,
    required to

    -   check and sort `Children`, eg given
        `<Children type="gate" name="gates"/>` we need to collect
        everything deriving from `gate` into an array `gates`.
    -   compose the derived items parameters and variables from the
        inherited items
    -   we cannot shorten `A <- B <- C` to `A <- C` since `C` might be
        used as either

-   Each item must be aware of its enclosing items

    ``` example
    NaConductance - gates +- m +- forwardRate
                          |    +- reverseRate
                          |
                          +- h +- forwardRate
                               +- reverseRate
    ```

    This is needed to facilitate path-based selection.

We need to situationally account for these cases

-   `A` declares a `<Child name="child" type="B">`

    ``` xml
    <A>
      <child type="B" />
    </A>
    ```

    or

    ``` xml
    <A>
      <child type="Derived_from_B" />
    </A>
    ```

    **QUESTION** is this acceptable as well?

    ``` xml
    <A>
      <!-- or name="child"? -->
      <B id="child" />
    </A>
    ```

-   `A` declares a `<Children name="children" type="B">`

    ``` xml
    <A>
      <B id="B1" />
      <Derived_from_B id="B2" />
    </A>
    ```

    The name `children` now relates to a collection of things deriving
    from `B`, ie `\forall T \sup B: String -> T`, akin to dynamic
    polymorphism/existentials.

    **NOTE** the dichotomy between `<name type="T">` and `<T id="name">`
    for `Child` and `Children` is a bit annoying.

    The possible instantiations of `ionChannel` are

    `baseIonChannel`  
    the base class

    `ionChannel` / `ionChannelHH`  
    the are identical, but both present for convenience

    `ionChannelKS`  
    ion channel with a `gate` based on a kinetic scheme

    `ionChannelVShift`  
    ion channel with a voltage offset.

    Practically, we only expect to support `ionChannel` for now and
    voltage shifted channels being a trivial extension later. Kinetic
    schemata are out of scope and `baseIonChannel` is of no practical
    use.

    However, it infeasible to generate a fixed set of prebuilt
    implementations as `ionChannel` instances can have an arbitrary
    number of `gate` instances which implement most of the
    functionality. This invalidates previous plans for implementation
    following that strategy.

    **QUESTION** Is the `id` field always allowed? **ANSWER** Yes, `id`
    is implied.

## Scoping

-   derived variables:
    -   local: visible
    -   enclosing: exposure
    -   enclosed: exposure
-   state variables:
    -   local: visible
    -   enclosing: exposure
    -   enclosed: exposure
-   parameters
    -   local: visible
    -   enclosing: private
    -   enclosed: private
-   constants:
    -   local: visible
    -   enclosing: private
    -   enclosed: private

## Flattening Instances

In order to produce efficient representations, we will need to collapse
instances and types into a flat format, eg

``` xml
<ComponentType name="B">
  <Exposure name="eX">
  <DerivedVariable name="X" exposure="eX"/>
</ComponentType>

<ComponentType name="A">
  <Child name="ab">
  <Dynamics>
    <DerivedVariable name="Y" select="B/eX"/>
  <Dynamics>
</ComponentType>
```

should be flattened into

``` xml
<ComponentType name="C">
  <Exposure name="ab_eX">
  <Dynamics>
    <DerivedVariable name="ab_X" exposure="ab_eX"/>
    <DerivedVariable name="Y" value="ab_eX"/>
  </Dynamics>
</ComponentType>
```

# Synapses

Next, we consider synapses, which have a different hierarchy, which is
reproduced in the following, again, simplified for ease of reading [^3]

``` xml
<ComponentType name="basePointCurrent" extends="baseStandalone">
  <Exposure name="i" dimension="current" description="The time varying current produced"/>
</ComponentType>

<ComponentType name="baseSynapse" extends="basePointCurrent">
  <!-- Todo: see why Property isn't inherited by extended types -->
  <Property name="weight" dimension="none" defaultValue="1"/>
  <EventPort name="in" direction="in"/>
</ComponentType>

<ComponentType name="baseVoltageDepSynapse" extends="baseSynapse">
  <Requirement name="v" dimension="voltage" description="voltage exposed by the parent"/>
</ComponentType>

<ComponentType name="baseConductanceBasedSynapse" extends="baseVoltageDepSynapse">
  <Parameter name="gbase" dimension="conductance" description="Baseline conductance, ig the max conductance following a single spike"/>
  <Parameter name="erev" dimension="voltage" description="Reversal potential of the synapse"/>
  <Exposure name="g" dimension="conductance" description="Time varying conductance through the synapse"/>
</ComponentType>

<ComponentType name="expOneSynapse" extends="baseConductanceBasedSynapse">
  <Parameter name="tauDecay" dimension="time" description="Time course of decay"/>
  <Dynamics>
    <StateVariable name="g" dimension="conductance" exposure="g"/>
    <DerivedVariable name="i" exposure="i" dimension="current" value="g * (erev - v)" />
    <TimeDerivative variable="g" value="-g / tauDecay" />
    <OnStart>
      <StateAssignment variable="g" value="0" />
    </OnStart>
    <OnEvent port="in">
      <StateAssignment variable="g" value="g + (weight * gbase)" />
    </OnEvent>
  </Dynamics>
</ComponentType>
```

There's some additions to our knowledge of NML2 here:

`Property`  
Paraphrasing the LEMS documentation: 'like `Parameter`, but may be
different for each instance'. So far, we did not encounter the
`Instance` concept in LEMS and as it is used here for the synapse
weight, we are going to largely ignore it. [^4]

`OnEvent`  
Contains assignments to state variables, similar to `OnStart`, but now
triggered upon incoming events, ie spikes. May choose a port.

Finally, we need to discuss `EventPorts`. In contrast to simulators like
Arbor and Neuron NML2 can differentiate between sources of incoming
events and selectively emit events to channels. In pratice, however,
only three such ports seem to be used in NML2 [^5]+"' \| sort -u\`\]

in  
Receive spikes

relay  
Forward spikes to sub-components

spike  
Emit spikes

### Implementation Details

ports  
Of these we only need to support `in` and `relay`, the former is
covered, the latter currently not.

weight  
As Arbor uses this construction for NMODL input

``` example
net_receive(weight) { ... }
```

we need to use our prior knowledge about the semantics of the parameter
and hard-code it into our NMODL translator.

vpeer  
Similar to `weight`, sometimes needed for `gradedSynapse`, can be
modelled in Arbor's NMODL dialect.

# Gap Junctions

Gap junctions are similar to synapses and have the following hierarchy

\<ComponentType name="gapJunction" extends="baseSynapse"/>

\<Property name="weight" dimension="none" defaultValue="1"/> \<Parameter
name="conductance" dimension="conductance"/> \<Exposure name="i"
dimension="current"/> \<Requirement name="v" dimension="voltage"/>
\<InstanceRequirement name="peer" type="gapJunction"/>

\<Dynamics> \<DerivedVariable name="vpeer" dimension="voltage"
select="peer/v"/> \<DerivedVariable name="i" exposure="i" value="weight
\* conductance \* (vpeer - v)"/> \</Dynamics> \</ComponentType>

The issue here with our scheme is that neither `peer` nor `vpeer` exist
in our implementation of NML2. However, Arbor's NMODL dialect exposes
`v_peer` as a global property.

# NMODL Export

While we have hinted at NMODL and Arbor a few times so far, our
implementation of NML2 has been almost completely generic. To export to
NMODL though, we need to bridge a significant gap. After we have
processed the instantiations into either an instance or a collapsed
instance, we edit the resulting model based on special cases.

At the moment, let us focus on current based models only, ie all our
instances need to produce an ionic transmembrane current, dubbed `iX` in
NMODL. Later, we might extend this to concentration models. For synapses
and gap junctions this is defined directly. Ion channels produce only a
conductance value, `g`, so we add a derived variable `iX =g (v - eX)`
where `X` is replaced by the ionic species.

Based on the respective sections on implementation details, we edit the
syntax trees to eliminate explicit definitions of `vpeer` and similar
values, replacing them with Arbor's built-in variables.

We then build individual blocks of the NMODL language from the AST by
inspecting dependencies of the state variables and building the
appropriate chains of expressions. All intermediates are stored as
`LOCAL` to minimise memory accesses.

The style defined by NML2, especially the

# Kinetic Schemes

Kinetic schemes describe ion channels by a set of discrete states and
transition probabilities between those states. Again we reproduce a
simplified version of the `ionChannelKS` hierarchy.

``` xml
<ComponentType name="ionChannelKS" extends="baseIonChannel">
    <Children name="conductanceScaling" type="baseConductanceScaling"/>
    <Children name="gates" type="gateKS"/>
    <Text name="species"/>
    <Dynamics>
        <DerivedVariable name="fopen" exposure="fopen" dimension="none" select="gates[*]/fcond" reduce="multiply"/>
        <DerivedVariable name="g" exposure="g" dimension="conductance" value="fopen * conductance"/>
    </Dynamics>
</ComponentType>
```

Apart from the type of `gates` this is functionally identical to
`ionChannelHH`. Now we delve into the definitions of `gateKS`

``` xml
<ComponentType name="gateKS" extends="baseGate">
    <Children name="states" type="KSState"/>
    <Children name="transitions" type="KSTransition"/>
    <Children name="q10Settings" type="baseQ10Settings"/>
    <Exposure name="rateScale" dimension="none"/>

    <Dynamics>
        <DerivedVariable name="rateScale" exposure="rateScale" dimension="none" select="q10Settings[*]/q10" reduce="multiply"/>
        <DerivedVariable name="q" exposure="q" dimension="none" select="states[*]/q" reduce="add"/>
        <DerivedVariable name="fcond" exposure="fcond" dimension="none" value="q^instances"/>
        <KineticScheme name="ks" nodes="states"
                       stateVariable="occupancy" edges="transitions"
                       edgeSource="from" edgeTarget="to"
                       forwardRate="rf" reverseRate="rr"/>
    </Dynamics>
</ComponentType>
```

Again, not much is new here, apart from `KineticScheme`, a list of
states is given in `states` and their transition rates in `transitions`.
Next, we look into the description of these transitons

``` xml
<ComponentType name="KSTransition">
    <Link name="from" type="KSState"/>
    <Link name="to" type="KSState"/>
    <Exposure name="rf" dimension="per_time"/>
    <Exposure name="rr" dimension="per_time"/>
</ComponentType>

<ComponentType name="forwardTransition" extends="KSTransition">
    <Constant name="SEC" dimension="time" value="1s"/>
    <Child name="rate" type="baseHHRate"/>
    <Dynamics>
        <DerivedVariable name="rf0" dimension="per_time" select="rate/r"/>
        <DerivedVariable name="rf" exposure="rf" dimension="per_time" value="rf0"/>
        <DerivedVariable name="rr" exposure="rr" dimension="per_time" value="0/SEC"/>
    </Dynamics>
</ComponentType>

<ComponentType name="reverseTransition" extends="KSTransition">
    <Constant name="SEC" dimension="time" value="1s"/>
    <Child name="rate" type="baseHHRate"/>
    <Dynamics>
        <DerivedVariable name="rr0" dimension="per_time" select="rate/r"/>
        <DerivedVariable name="rf" exposure="rf" dimension="per_time" value="0/SEC"/>
        <DerivedVariable name="rr" exposure="rr" dimension="per_time" value="rr0"/>
    </Dynamics>
</ComponentType>
```

Again not much of a surprise here, but we do need to figure out what
`Link` means. The final component is the linked state

``` xml
<ComponentType name="KSState">
    <Parameter name="relativeConductance" dimension="none"/>
    <Exposure name="occupancy" dimension="none"/>
    <Exposure name="q" dimension="none"/>
    <Dynamics>
        <StateVariable name="occupancy" exposure="occupancy" dimension="none"/>
        <DerivedVariable name="q" dimension="none" exposure="q" value="relativeConductance * occupancy"/>
    </Dynamics>
</ComponentType>

<ComponentType name="closedState" extends="KSState" description="A _KSState_ with _relativeConductance of 0">
    <Fixed parameter="relativeConductance" value="0"/>
</ComponentType>

<ComponentType name="openState" extends="KSState" description="A _KSState_ with _relativeConductance of 1">
    <Fixed parameter="relativeConductance" value="1"/>
</ComponentType>
```

Now, let us consider a simple example from the NML2 sources.

``` xml
<ionChannelKS conductance="10pS" id="k_fwd_rev" species="k">
    <gateKS id="n" instances="4">
        <closedState id="c1"/>
        <openState id="o1"/>
        <forwardTransition from="c1" id="ft" to="o1">
            <rate midpoint="-55mV" rate="0.1per_ms" scale="10mV" type="HHExpLinearRate"/>
        </forwardTransition>
        <reverseTransition from="c1" id="rt" to="o1">
            <rate midpoint="-65mV" rate="0.125per_ms" scale="-80mV" type="HHExpRate"/>
        </reverseTransition>
    </gateKS>
</ionChannelKS>
```

we can interpret this as

-   define two populations transitioning between states open `o1` and
    closed `c1`
    -   call these fractions `o1_occupancy` and `c1_occupancy`
    -   conserve `o1_occupancy + c1_occupancy = 1` for all times
    -   transition `c1 -> o1` with rate `ft/rate/r`
    -   transition `o1 -> c1` with rate `rt/rate/r`
-   calculate
    `ft/rate/r`  
    as prescribed by `HHExpLinearRate`

    `rt/rate/r`  
    as prescribed by `HHExpRate`

In NMODL we would like to generate a `KINETIC` block like this

``` example
KINETIC scheme {
  : ... snip ...
  gates_n_states_o1_to_c1 = gates_n_transitions_ft_rr + gates_n_transitions_rt_rr
  gates_n_states_c1_to_o1 = gates_n_transitions_ft_rf + gates_n_transitions_rt_rf
  gates_m_states_o1_to_c1 = gates_m_transitions_ft_rr + gates_m_transitions_rt_rr
  gates_m_states_c1_to_o1 = gates_m_transitions_ft_rf + gates_m_transitions_rt_rf

  ~ gates_n_states_o1_occupancy <-> gates_n_states_c1_occupancy (gates_n_states_o1_to_c1, gates_n_states_c1_to_o1)
  ~ gates_m_states_o1_occupancy <-> gates_m_states_c1_occupancy (gates_m_states_o1_to_c1, gates_m_states_c1_to_o1)
}
```

where multiple rates between the same two states were merged. Note that

-   `gates_n_transitions_ft_rr = 0`
-   `gates_n_transitions_ft_rf = 0`
-   `gates_m_transitions_ft_rr = 0`
-   `gates_m_transitions_ft_rf = 0`

## On `KineticScheme`

`KineticScheme` seems to break the abstractions put in place. Looking at
`nodes` and `edges` which <u>must</u> be of kind `children`, otherwise
the semantics would not work out. Similarly, both must be located
directly under the scheme in the hierarchy. However, a `select`
statement would have expressed the same thing more idiomatically.

# The Final Building Block

So far, we have dealt exclusively with channels, but NML2 requires some
more levels above this for completely specilying the dynamics

Cell  
defines dynamics of membrane potential in terms of currents (mediated by
channels)

BioPhys Properties  
specifies capacitance, resistivities.

Membrane Properties  
initial potential, channels.

Channel Density  
gives the area density of a channel.

In order to interact with cable models like Arbor or Neuron, we need to
pull these layers into the channel descriptions – partially at least. In
the end we would like to automatically compose Arbor simulations from
NML2 files, but for now, we have to extract those values manually.

Working our way up

``` xml
<ComponentType name="baseChannelDensity">
    <ComponentReference name="ionChannel" type="baseIonChannel"/>
    <Exposure name="iDensity" dimension="currentDensity"/>
    <Requirement name="v" dimension="voltage"/>
</ComponentType>

<ComponentType name="baseChannelDensityCond" extends="baseChannelDensity">
    <Parameter name="condDensity" dimension="conductanceDensity"/>
    <Exposure name="gDensity" dimension="conductanceDensity"/>
</ComponentType>

<ComponentType name="channelDensity" extends="baseChannelDensityCond">
    <Parameter name="erev" dimension="voltage" description="The reversal potential of the current produced"/>
    <Constant name="vShift" dimension="voltage" value="0mV"/>
    <Dynamics>
        <DerivedVariable name="channelf" dimension="none" select="ionChannel/fopen"/>
        <DerivedVariable name="gDensity" dimension="conductanceDensity" exposure="gDensity" value="condDensity * channelf"/>
        <DerivedVariable name="iDensity"  dimension="currentDensity" exposure="iDensity" value="gDensity * (erev - v)"/>
    </Dynamics>
</ComponentType>
```

As noted under <span class="spurious-link"
target="NMODL Export">*NMODL Export*</span> we compute `iX = g(E - U)`
where `g` is the `ionChannel`'s conductance, see above
`g = fopen * conductance`. The parameter `condDensity` added in
`channelDensityCond` is identical to `conductance`. Thus, we need to
retain `conductance` and set it to the value of `condDensity`.

# Much Time Has Passed

# Networks

Networks are important for us for both actual network support and
simulation inputs. We note the following about the network layout in
NML2.

Instances for input and synapse appear at top-level, next to cells and
networks. Thus any `.nml` file is eligible to define them.

Networks are defined in terms of <u>populations</u> and
<u>projections</u>.

## Populations

Populations can be written in two ways inside a `network`

`<population component="X" size="n"/>`  
`n` duplicates of `X`; using `MultiInstantiate` which seems to have no
particular definition.

`<populationList component="X">`  
give children as list of `<instance>` with a 3D position, all have type
`X`.

For us, there seems to be no practical difference between them. In both
cases `X` names a `ComponentType` deriving `baseCell`.

## Projections

Projections specify pre- and post-synaptic populations and a connection
type, eg a synapse. Then, a list of connections is given as tuples
`(pre: location, post:
location)` where `location` takes the form
`(cell: id, segment: id, fraction:
[0, 1])`

Target cells are <u>selected</u> by the relative paths and an index, eg
`target=../pop0[0]` addresses the population `pop0`, one tag up from
here, and picks cell zero from there. If `pop0` has a single member, the
index is optional. Alternatively it seems (?) we can address using
`pop/<id>/Cell`

### Questions

-   Does `[ix]` query by id or offset?
-   What's the difference between `pop/id/Cell` and `/pop[0]`?
-   Why do connections have to traverse the tree and do not work
    relative to their pre/post populations? Seems counterintuitive.
-   How does Arbor translate segments into its internal rep? Can we
    directly use it like `branch`?

### Example

``` xml
<network id="MultiCompCellNetwork">
    <population id="pop0" type="populationList" component="Cell">
        <instance id="0">
            <location x="0" y="0" z="0"/>
        </instance>
        <instance id="1">
            <location x="30" y="0" z="0"/>
        </instance>
    </population>

    <projection id="AMPA-syn" presynapticPulation="pop" postsynapticPulation="pop" synapse="AMPA">
      <connection id="0"
                  preCellId="../pop/0/Cell" preSegmentId="0" preFractionAlong="0.5"
                  postCellId="../pop/1/Cell" postSegmentId="0" postFractionAlong="0.5"/>
      <connection id="1"
                  preCellId="../pop/0/Cell" preSegmentId="0" preFractionAlong="0.5"
                  postCellId="../pop/1/Cell" postSegmentId="3" postFractionAlong="0.3"/>
    </projection>
</network>
```

## Inputs

-   inputs are similar to (one-sided) connections
    -   targets are written as
        -   id, default = 0
        -   fraction, default = 0.5
-   stimuli derive `basePointCurrent`
    -   pulse, sine, cosine
-   relevant tags:
    `explicitInput`  

    `inputList`  

### Example

``` xml
<pulseGenerator id="pulseGen1" delay="100ms" duration="100ms" amplitude="0.10nA"/>
<pulseGenerator id="pulseGen2" delay="300ms" duration="100ms" amplitude="0.35nA"/>

<network id="HHCellNetwork">
    <population id="hhpop" component="hhcell" size="1"/>
    <explicitInput target="hhpop[0]" input="pulseGen1"/>
    <explicitInput target="hhpop[0]" input="pulseGen2"/>
</network>
```

[^1]: Low-entropy modelling language. Using the moniker 'low-entropy'
    for an XML based format must be an attempt at humor.

[^2]: For example the use of `sequence` in XSD implies order, but I am
    pretty sure this is not upheld everywhere. Also, most often
    `sequence` is used together with `count=unbounded`, which is
    semantically incorrect in many cases

[^3]: Note the comment, which we ignore is if it was fixed and fix in
    our own code.

[^4]: Arbor adds this on the library side to connections.

[^5]: Confirmed via \`rg -Io 'port="\[^"
