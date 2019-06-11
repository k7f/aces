use std::{collections::{BTreeMap, BTreeSet}, sync::{Mutex, Arc}, fmt::Debug, error::Error};
use regex::Regex;
use yaml_rust::{Yaml, YamlLoader};
use crate::{Context, Face, context::NameSpace, error::AcesError};

trait UpdatableVec<V> {
    fn vec_update(&mut self, value: &V);
}

trait UpdatableMap<K, V> {
    fn map_update(&mut self, key: K, value: &V);
}

type MonoForYaml = Vec<usize>;
type PolyForYaml = Vec<MonoForYaml>;

impl UpdatableVec<usize> for MonoForYaml {
    fn vec_update(&mut self, value: &usize) {
        if let Err(ndx) = self.binary_search(value) {
            self.insert(ndx, *value);
        }
    }
}

impl UpdatableVec<MonoForYaml> for PolyForYaml {
    fn vec_update(&mut self, value: &MonoForYaml) {
        if let Err(ndx) = self.binary_search(value) {
            self.insert(ndx, value.to_owned());
        }
    }
}

impl UpdatableMap<usize, PolyForYaml> for BTreeMap<usize, PolyForYaml> {
    fn map_update(&mut self, key: usize, value: &PolyForYaml) {
        self.entry(key)
            .and_modify(|poly| {
                for new_mono in value {
                    poly.vec_update(new_mono)
                }
            }).or_insert(value.to_owned());
    }
}

fn do_take_id(
    nodes: &mut NameSpace,
    name: &str,
    single_word_only: bool
) -> Result<usize, Box<dyn Error>>
{
    if single_word_only && name.contains(char::is_whitespace) {
        Err(Box::new(AcesError::SpecShortPolyWithWords))
    } else {
        Ok(nodes.take_id(name))
    }
}

fn post_process_port_spec(
    nodes: &mut NameSpace,
    spec: &str,
    single_word_only: bool,
) -> Result<Vec<usize>, Box<dyn Error>> {
    if spec.contains(',') {
        let result: Result<Vec<usize>, Box<dyn Error>> = spec.split(',')
            .map(|s| do_take_id(nodes, s.trim(), single_word_only))
            .collect();
        let ids = result?;

        Ok(ids)
    } else {
        let id = do_take_id(nodes, spec.trim(), single_word_only)?;

        Ok(vec![id])
    }
}

fn do_parse_port_spec(
    nodes: &mut NameSpace,
    spec: &str,
    single_word_only: bool
) -> Result<Option<(Vec<usize>, Face)>, Box<dyn Error>>
{
    lazy_static! {
        // Node name (untrimmed, unseparated) is any nonempty string not ending in '>' or '<'.
        // Removal of leading and trailing whitespace is done in post processing,
        // as well as comma-separation.
        static ref TX_RE: Regex = Regex::new(r"^(.*[^><])(>+|\s+effects)$").unwrap();
        static ref RX_RE: Regex = Regex::new(r"^(.*[^><])(<+|\s+causes)$").unwrap();
    }
    if let Some(cap) = TX_RE.captures(spec) {
        let ids = post_process_port_spec(nodes, &cap[1], single_word_only)?;

        Ok(Some((ids, Face::Tx)))

    } else if let Some(cap) = RX_RE.captures(spec) {
        let ids = post_process_port_spec(nodes, &cap[1], single_word_only)?;

        Ok(Some((ids, Face::Rx)))
    } else {
        Ok(None)
    }
}

fn parse_port_spec(ctx: &Arc<Mutex<Context>>, spec: &str) -> Option<(Vec<usize>, Face)> {
    let ref mut nodes = ctx.lock().unwrap().nodes;

    do_parse_port_spec(nodes, spec, false).unwrap_or_else(|_| {
        unreachable!()
    })
}

fn parse_link_spec(
    ctx: &Arc<Mutex<Context>>,
    spec: &str,
    valid_face: Face,
    single_word_only: bool,
) -> Result<(usize, bool), Box<dyn Error>>
{
    let ref mut nodes = ctx.lock().unwrap().nodes;

    let link_with_colink = do_parse_port_spec(nodes, spec, single_word_only)?;

    if let Some((ids, face)) = link_with_colink {
        if face == valid_face {
            if ids.len() == 1 {
                Ok((ids[0], true))
            } else {
                Err(Box::new(AcesError::SpecLinkList))
            }
        } else {
            Err(Box::new(AcesError::SpecLinkReversed))
        }
    } else {
        let id = do_take_id(nodes, spec, single_word_only)?;

        Ok((id, false))
    }
}

#[derive(Default, Debug)]
struct SpecForYaml {
    name:    Option<String>,
    meta:    BTreeMap<String, Yaml>,
    causes:  BTreeMap<usize, PolyForYaml>,
    effects: BTreeMap<usize, PolyForYaml>,
    carrier: BTreeSet<usize>,
}

impl SpecForYaml {
    fn add_ports(
        &mut self,
        ctx: &Arc<Mutex<Context>>,
        node_ids: &[usize],
        face: Face,
        poly_yaml: &Yaml
    ) -> Result<(), Box<dyn Error>>
    {
        assert!(!node_ids.is_empty());

        let mut poly_spec = PolyForYaml::new();

        match poly_yaml {
            Yaml::String(other_name) => {
                let (other_id, with_colink) =
                    parse_link_spec(ctx, other_name.trim(), !face, true)?;

                poly_spec.vec_update(&vec![other_id]);

                if with_colink {
                    if face == Face::Tx {
                        self.causes.map_update(other_id, &vec![node_ids.to_owned()]);
                    } else {
                        self.effects.map_update(other_id, &vec![node_ids.to_owned()]);
                    }
                }
            }
            Yaml::Array(table) => {
                let mut is_flat = true;

                for value in table {
                    match value {
                        Yaml::String(other_name) => {
                            let (other_id, with_colink) =
                                parse_link_spec(ctx, other_name.trim(), !face, true)?;

                            poly_spec.vec_update(&vec![other_id]);

                            if with_colink {
                                if face == Face::Tx {
                                    self.causes.map_update(other_id, &vec![node_ids.to_owned()]);
                                } else {
                                    self.effects.map_update(other_id, &vec![node_ids.to_owned()]);
                                }
                            }
                        }
                        Yaml::Array(table) => {
                            is_flat = false;

                            let mut mono_spec = MonoForYaml::new();
                            for value in table {
                                if let Some(other_name) = value.as_str() {
                                    let (other_id, with_colink) =
                                        parse_link_spec(ctx, other_name.trim(), !face, false)?;

                                    mono_spec.vec_update(&other_id);

                                    if with_colink {
                                        if face == Face::Tx {
                                            self.causes.map_update(other_id, &vec![node_ids.to_owned()]);
                                        } else {
                                            self.effects.map_update(other_id, &vec![node_ids.to_owned()]);
                                        }
                                    }
                                } else {
                                    return Err(Box::new(AcesError::SpecLinkInvalid))
                                }
                            }
                            poly_spec.vec_update(&mono_spec);
                        }
                        _ => {
                            return Err(Box::new(AcesError::SpecMonoInvalid))
                        }
                    }
                }
                if is_flat {
                    return Err(Box::new(AcesError::SpecPolyAmbiguous))
                }

            }
            _ => {
                return Err(Box::new(AcesError::SpecPolyInvalid))
            }
        }

        if face == Face::Tx {
            for &id in node_ids {
                self.effects.map_update(id, &poly_spec);
            }
        } else {
            for &id in node_ids {
                self.causes.map_update(id, &poly_spec);
            }
        }
        Ok(())
    }

    fn add_entry(&mut self, ctx: &Arc<Mutex<Context>>, key: &Yaml, value: &Yaml) -> Result<(), Box<dyn Error>> {
        if let Some(key) = key.as_str() {
            let key = key.trim();
            let port = parse_port_spec(ctx, key);

            if let Some((node_ids, face)) = port {

                self.add_ports(ctx, &node_ids, face, value)

            } else if key == "name" {
                if let Some(name) = value.as_str() {
                    if self.name.is_none() {
                        self.name = Some(name.trim().to_owned());

                        Ok(())
                    } else {
                        Err(Box::new(AcesError::SpecNameDup))
                    }
                } else {
                    Err(Box::new(AcesError::SpecNameNotString))
                }
            } else {
                // FIXME handle duplicates
                self.meta.insert(key.to_string(), value.clone());

                Ok(())
            }
        } else {
            Err(Box::new(AcesError::SpecKeyNotString))
        }
    }

    fn from_yaml(ctx: &Arc<Mutex<Context>>, yaml: &Yaml) -> Result<Self, Box<dyn Error>> {
        if let Yaml::Hash(ref dict) = yaml {

            let mut spec = Self::default();

            for (key, value) in dict {
                spec.add_entry(ctx, key, value)?;
            }

            spec.carrier = spec.effects.keys()
                .chain(spec.causes.keys())
                .copied().collect();

            Ok(spec)
        } else {
            Err(Box::new(AcesError::SpecNotADict))
        }
    }

    fn from_str(ctx: &Arc<Mutex<Context>>, raw_spec: &str) -> Result<Self, Box<dyn Error>> {
        let docs = YamlLoader::load_from_str(raw_spec.into())?;

        if docs.is_empty() {
            Err(Box::new(AcesError::SpecEmpty))

        } else if docs.len() == 1 {
            let spec = Self::from_yaml(ctx, &docs[0])?;
            Ok(spec)
        } else {
            Err(Box::new(AcesError::SpecMultiple))
        }
    }
}

// FIXME define specific iterators for return types below
pub(crate) trait CESSpec: Debug {
    fn get_raw(&self) -> Option<&str>;
    fn get_name(&self) -> Option<&str>;
    fn get_carrier_ids(&self) -> Vec<usize>;
    fn get_causes_by_id(&self, node_id: usize) -> Option<&Vec<Vec<usize>>>;
    fn get_effects_by_id(&self, node_id: usize) -> Option<&Vec<Vec<usize>>>;
}

impl CESSpec for String {
    fn get_raw(&self) -> Option<&str> {
        Some(self)
    }

    fn get_name(&self) -> Option<&str> {
        panic!("Attempt to access a phantom specification.")
    }

    fn get_carrier_ids(&self) -> Vec<usize> {
        panic!("Attempt to access a phantom specification.")
    }

    fn get_causes_by_id(&self, _node_id: usize) -> Option<&Vec<Vec<usize>>> {
        panic!("Attempt to access a phantom specification.")
    }

    fn get_effects_by_id(&self, _node_id: usize) -> Option<&Vec<Vec<usize>>> {
        panic!("Attempt to access a phantom specification.")
    }
}

impl CESSpec for SpecForYaml {
    fn get_raw(&self) -> Option<&str> {
        None  // FIXME
    }

    fn get_name(&self) -> Option<&str> {
        if let Some (ref name) = self.name {
            Some(name.as_ref())
        } else {
            None
        }
    }

    fn get_carrier_ids(&self) -> Vec<usize> {
        self.carrier.iter().copied().collect()
    }

    fn get_causes_by_id(&self, node_id: usize) -> Option<&Vec<Vec<usize>>> {
        self.causes.get(&node_id)
    }

    fn get_effects_by_id(&self, node_id: usize) -> Option<&Vec<Vec<usize>>> {
        self.effects.get(&node_id)
    }
}

pub(crate) fn spec_from_str(
    ctx:  &Arc<Mutex<Context>>,
    raw_spec: &str,
) -> Result<Box<dyn CESSpec>, Box<dyn Error>>
{
    // FIXME infer the format of spec string: yaml, sexpr, ...
    Ok(Box::new(SpecForYaml::from_str(ctx, raw_spec)?))
}
