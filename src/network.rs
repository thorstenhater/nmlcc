use tracing::trace;

use crate::{error::Result, expr::Quantity, lems::file::LemsFile, neuroml::raw};

pub struct Network {
    pub temp: f64,
}

impl Network {
    pub fn new(lems: &LemsFile, net: &raw::Network) -> Result<Self> {
        let t = net.temperature.as_deref().unwrap_or("0K");
        let t = Quantity::parse(t)?;
        let t = lems.normalise_quantity(&t)?;

        use raw::NetworkBody::*;
        for item in &net.body {
            match item {
                projection(p) => proj(p)?,
                population(p) => pop(p)?,
                inputList(i) => {},
                explicitInput(i) => stim(i)?,
                _ => trace!("Ignored {:?} in Network", item),
            }
        }

        Ok(Network { temp: t.value })
    }
}


fn pop(p: &raw::Population) -> Result<()> {
    Ok(())
}

fn proj(p: &raw::Projection) -> Result<()> {
    Ok(())
}

fn stim(p: &raw::ExplicitInput) -> Result<()> {
    Ok(())
}
