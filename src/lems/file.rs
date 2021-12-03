use std::collections::HashMap as Map;
use tracing::info;

use super::{Lems,
            raw::{Unit, Dimension}};
use crate::{Result,
            instance::ComponentType,
            expr::Quantity};
use std::convert::TryInto;

fn normalise_quantity(quantity: &Quantity,
                      units: &Map<String, Unit>,       // Known units by name
                      blessed: &Map<String, Unit>)     // Blessed units by dimension
                      -> Result<Quantity> {
    if let Some(u) = quantity.unit.as_deref() {
        if let Some(v) = units.get(u) {
            if let Some(w) = blessed.get(&v.dimension) {
                // We are scared by those
                assert!(v.offset == 0.0);
                assert!(w.offset == 0.0);
                // Compute conversion
                // f v.scale 10^v.power = w.scale 10^w.power
                // => f = 10^(w.power - v.power)
                let e: i32 = (w.power - v.power).try_into()
                                                .map_err(|_|
                                                         format!("Couldn't convert {} to i32",
                                                                 w.power - v.power))?;
                let f = (w.scale/v.scale)*f64::powi(10.0, e);
                if f != 1.0 { info!("Adjusting {} -> {} by {}", v.symbol, w.symbol, f); }
                Ok(Quantity { value: quantity.value*f, unit: Some(w.symbol.to_string()) })
            } else {
                Err(format!("Failed to find a blessed unit for dimension {}", v.dimension))
            }
        } else {
            Err(format!("Failed to find unit {} for quantity {:?}", u, quantity))
        }
    } else {
        // Non-dimensional units get just passed on.
        Ok(quantity.clone())
    }
}

/// Processed LEMS data
#[derive(Clone, Debug, Default)]
pub struct LemsFile {
    /// Inheritance hierarchy derived -> base
    pub base_of: Map<String, String>,
    /// Name -> Type
    pub types: Map<String, ComponentType>,
    /// symbol -> unit
    pub units: Map<String, Unit>,
    /// name -> dimension
    pub dimensions: Map<String, Dimension>,
    /// dimension -> unit
    pub blessed_units: Map<String, Unit>,
}

impl LemsFile {
    /// Pull LEMS from file
    pub fn from(dn: &str, file: &str) -> Result<Self> { Self::from_raw(&Lems::from_file(dn, file)?) }

    /// Ingest raw LEMS and munge into a digestible form
    fn from_raw(raw: &Lems) -> Result<Self> {
        // Chosen from physiological/NRN/ARB units
        let blessed = [("mV",              "voltage"),
                       ("kohm",            "resistance"),
                       ("mS",              "conductance"),
                       ("cm",              "length"),
                       ("cm2",             "area"),
                       ("cm3",             "volume"),
                       ("ms",              "time"),
                       ("per_ms",          "per_time"),
                       ("mS_per_cm2",      "conductanceDensity"),
                       ("uF",              "capacitance"),
                       ("uF_per_cm2",      "specificCapacitance"),
                       ("kohm_cm",         "resistivity"),
                       ("nA_ms_per_amol",  "charge_per_mole"),
                       ("uA",              "current"),
                       ("uA_per_cm2",      "currentDensity"),
                       ("mol_per_cm3",     "concentration"),
                       ("K",               "temperature"),
                       ("J_per_K_per_mol", "idealGasConstantDims"),
                       ("nS_per_mV",       "conductance_per_voltage"),];

        let dimensions = raw.dimensions.iter().map(|d| (d.name.to_string(), d.clone())).collect();
        let units: Map<_, _> = raw.units.iter().map(|d| (d.symbol.to_string(), d.clone())).collect();
        let blessed_units = blessed.iter()
                                   .map(|(s, d)| (d.to_string(), units.get(&s.to_string()).unwrap().clone()))
                                   .collect();

        let mut types = Map::new();
        let mut base_of = Map::new();
        for ct in &raw.component_types {
            if let Some(base) = &ct.extends { base_of.insert(ct.name.to_string(), base.to_string()); }
            let mut ctype = ComponentType::from_lems(ct)?;
            ctype.constants = ctype.constants.iter()
                                             .map(|(s, c)| (s.to_string(),
                                                            normalise_quantity(c, &units, &blessed_units).unwrap()))
                                             .collect();
            types.insert(ct.name.to_string(), ctype);
        }
        Ok(Self { base_of, types, units, dimensions, blessed_units })
    }

    pub fn normalise_quantity(&self, quantity: &Quantity) -> Result<Quantity> {
        normalise_quantity(quantity, &self.units, &self.blessed_units)
    }


    /// Check if type `d` derives from type `b`.
    pub fn derived_from(&self, d: &str, b: &str) -> bool {
        let mut d = d;
        loop {
            if d == b { return true; }
            if let Some(k) = self.base_of.get(d) {
                d = k;
                continue;
            }
            break;
        }
        false
    }

    /// Flatten inheritance hierarchy. This will take a component-type name
    /// (must be present) and compose a final type from it by walking the
    /// inheritance chain. The result will be built by appending all members
    /// while later (='more derived') items take precedence.
    pub fn compose_component_type(&self, id: &str) -> Result<ComponentType> {
        let mut result = self.types.get(id).ok_or(format!("No such type: {}.", id))?.clone();
        let mut base = result.base.as_ref();
        while let Some(id) = base {
            let ty = self.types.get(id).ok_or(format!("No such type: {}.", id))?;
            for (k,v) in &ty.child {
                result.child.entry(k.to_string()).or_insert_with(|| v.to_string());
            }
            for (k,v) in &ty.children {
                result.children.entry(k.to_string()).or_insert_with(|| v.to_string());
            }
            for (e, d) in &ty.exposures {
                if !result.exposures.contains_key(e) { result.exposures.insert(e.clone(), d.clone()); }
            }
            for p in &ty.parameters {
                if !result.parameters.contains(p) { result.parameters.push(p.clone()); }
            }
            for p in &ty.variables {
                if !result.variables.contains(p) { result.variables.push(p.clone()); }
            }
            base = ty.base.as_ref();
        }
        Ok(result)
    }
}
