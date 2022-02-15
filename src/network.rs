#![allow(dead_code, unused)]

use tracing::{trace, warn};

use crate::{error::Result, expr::Quantity, lems::file::LemsFile, neuroml::raw};

#[derive(Clone, Debug)]
pub struct Network {
    pub temperature: f64,
    pub inputs: Vec<Input>,
}

#[derive(Clone, Debug)]
pub struct Input {
    /// id of source
    pub source: String,
    /// path to target component
    pub target: String,
    /// segment in target, default to 0
    pub segment: i64,
    /// fraction of segment to target, default 0.5
    pub fraction: f64,
}

impl Network {
    pub fn new(lems: &LemsFile, net: &raw::Network) -> Result<Self> {
        let temperature = net.temperature.as_deref().unwrap_or("0K");
        let temperature = Quantity::parse(temperature)?;
        let temperature = lems.normalise_quantity(&temperature)?.value;

        let mut inputs = Vec::new();
        use raw::NetworkBody::*;
        for item in &net.body {
            match item {
                projection(p) => proj(p)?,
                population(p) => pop(p)?,
                inputList(i) => input_list(i, &mut inputs)?,
                explicitInput(i) => explicit_input(i, &mut inputs)?,
                _ => trace!("Ignored {:?} in Network", item),
            }
        }

        Ok(Network {
            temperature,
            inputs,
        })
    }
}

fn pop(p: &raw::Population) -> Result<()> {
    Ok(())
}

fn proj(p: &raw::Projection) -> Result<()> {
    Ok(())
}

fn explicit_input(p: &raw::ExplicitInput, inputs: &mut Vec<Input>) -> Result<()> {
    warn!("Using 'ExplicitInput' is discouraged. Treated as targetting segment=0 fraction=0.5.");
    let raw::ExplicitInput { target, input, .. } = p;
    let fraction = 0.5;
    let segment = 0;
    let target = target.to_string();
    let source = input.to_string();
    inputs.push(Input {
        fraction,
        segment,
        target,
        source,
    });
    Ok(())
}

fn input_list(list: &raw::InputList, inputs: &mut Vec<Input>) -> Result<()> {
    let source = &list.component;
    for it in &list.body {
        match it {
            // TODO(TH; Semantics): What is destination's meaning here?
            raw::InputListBody::input(raw::Input {
                target,
                segmentId,
                fractionAlong,
                ..
            }) => {
                let fraction = fractionAlong.unwrap_or(0.5);
                let segment = segmentId.unwrap_or(0);
                let target = target.to_string();
                let source = source.to_string();
                inputs.push(Input {
                    fraction,
                    segment,
                    target,
                    source,
                });
            }
            // TODO(TH): How to handle this? What's a weight on a iclamp anyhow?
            raw::InputListBody::inputW(_) => todo!(),
        }
    }
    Ok(())
}
