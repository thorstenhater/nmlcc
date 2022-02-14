#![allow(dead_code, unused)]

use tracing::trace;

use crate::{error::Result, expr::Quantity, lems::file::LemsFile, neuroml::raw, Map};

pub struct Population {
    pub members: Vec<i64>, //list of IDs.
    pub prototype: String,
}




pub struct Network {
    pub temperature: f64,
    pub populations: Map<String,Population>,
    
    
    
    /* Fill out w stuff from neuroml/raw.rs (Network, Networkbody, Population, PopulationBody)

    3 things important:
    * populations (=> map)
    * projections
    * connections ?



    */
}

impl Network {
    pub fn new(lems: &LemsFile, net: &raw::Network) -> Result<Self> {
        let t = net.temperature.as_deref().unwrap_or("0K");
        let t = Quantity::parse(t)?;
        let t = lems.normalise_quantity(&t)?;
        let mut populations = Map::new();

        use raw::NetworkBody::*;
        for item in &net.body {
            match item {
                projection(p) => proj(p)?,
                population(p) => pop(p,&mut populations)?,
                inputList(i) => {}
                explicitInput(i) => stim(i)?,
                _ => trace!("Ignored {:?} in Network", item),
            }
        }

        Ok(Network { temperature: t.value,
                     populations
                     })
    }
}

fn pop(p: &raw::Population, ps: &mut Map<String,Population>) -> Result<()> {
    let mut members = Vec::new();
    let prototype = p.component.to_string();

    for item in &p.body{
        match item {
            instance(i) => member.push(i.id.unwrap()), //id is optional, so may explode.
            _ => trace!("Ignored {:?} in Population", item),
        }
    }

    let pop = Population{ members,
                          prototype
                          };
    ps.insert(p.id.to_string(),pop);
    Ok(())
}

fn proj(p: &raw::Projection) -> Result<()> {
    Ok(())
}

fn stim(p: &raw::ExplicitInput) -> Result<()> {
    Ok(())
}
