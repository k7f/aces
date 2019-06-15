use std::{
    collections::{BTreeMap, BTreeSet},
    sync::{Arc, Mutex},
    fmt::Debug,
    error::Error,
};
use regex::Regex;
use yaml_rust::{Yaml, YamlLoader};
use crate::{Context, ID, Face, name::NameSpace, error::AcesError};

trait UpdatableVec<V> {
    fn vec_update(&mut self, value: &V);
}

trait UpdatableMap<K, V> {
    fn map_update(&mut self, key: K, value: &V);
}

type MonoForYaml = Vec<ID>;
type PolyForYaml = Vec<MonoForYaml>;

impl UpdatableVec<ID> for MonoForYaml {
    fn vec_update(&mut self, value: &ID) {
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

impl UpdatableMap<ID, PolyForYaml> for BTreeMap<ID, PolyForYaml> {
    fn map_update(&mut self, key: ID, value: &PolyForYaml) {
        self.entry(key)
            .and_modify(|poly| {
                for new_mono in value {
                    poly.vec_update(new_mono)
                }
            })
            .or_insert_with(|| value.to_owned());
    }
}

fn do_take_id<S: AsRef<str>>(
    nodes: &mut NameSpace,
    name: S,
    single_word_only: bool,
) -> Result<ID, Box<dyn Error>> {
    if single_word_only && name.as_ref().contains(char::is_whitespace) {
        Err(Box::new(AcesError::SpecShortPolyWithWords))
    } else {
        Ok(nodes.take_id(name))
    }
}

fn post_process_port_spec<S: AsRef<str>>(
    nodes: &mut NameSpace,
    spec: S,
    single_word_only: bool,
) -> Result<Vec<ID>, Box<dyn Error>> {
    if spec.as_ref().contains(',') {
        let result: Result<Vec<ID>, Box<dyn Error>> = spec
            .as_ref()
            .split(',')
            .map(|s| do_take_id(nodes, s.trim(), single_word_only))
            .collect();
        let ids = result?;

        Ok(ids)
    } else {
        let id = do_take_id(nodes, spec.as_ref().trim(), single_word_only)?;

        Ok(vec![id])
    }
}

type PortParsed = (Vec<ID>, Face);

fn do_parse_port_spec<S: AsRef<str>>(
    nodes: &mut NameSpace,
    spec: S,
    single_word_only: bool,
) -> Result<Option<PortParsed>, Box<dyn Error>> {
    lazy_static! {
        // Node name (untrimmed, unseparated) is any nonempty string not ending in '>' or '<'.
        // Removal of leading and trailing whitespace is done in post processing,
        // as well as comma-separation.
        static ref TX_RE: Regex = Regex::new(r"^(.*[^><])(>+|\s+effects)$").unwrap();
        static ref RX_RE: Regex = Regex::new(r"^(.*[^><])(<+|\s+causes)$").unwrap();
    }
    if let Some(cap) = TX_RE.captures(spec.as_ref()) {
        let ids = post_process_port_spec(nodes, &cap[1], single_word_only)?;

        Ok(Some((ids, Face::Tx)))
    } else if let Some(cap) = RX_RE.captures(spec.as_ref()) {
        let ids = post_process_port_spec(nodes, &cap[1], single_word_only)?;

        Ok(Some((ids, Face::Rx)))
    } else {
        Ok(None)
    }
}

fn parse_port_spec<S: AsRef<str>>(ctx: &Arc<Mutex<Context>>, spec: S) -> Option<PortParsed> {
    let ref mut nodes = ctx.lock().unwrap().nodes;

    do_parse_port_spec(nodes, spec, false).unwrap_or_else(|_| unreachable!())
}

fn parse_link_spec<S: AsRef<str> + Copy>(
    ctx: &Arc<Mutex<Context>>,
    spec: S,
    valid_face: Face,
    single_word_only: bool,
) -> Result<(ID, bool), Box<dyn Error>> {
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
    causes:  BTreeMap<ID, PolyForYaml>,
    effects: BTreeMap<ID, PolyForYaml>,
    carrier: BTreeSet<ID>,
}

impl SpecForYaml {
    fn add_ports(
        &mut self,
        ctx: &Arc<Mutex<Context>>,
        ids: &[ID],
        face: Face,
        poly_yaml: &Yaml,
    ) -> Result<(), Box<dyn Error>> {
        assert!(!ids.is_empty());

        let mut poly_spec = PolyForYaml::new();

        match poly_yaml {
            Yaml::String(other_name) => {
                let (other_id, with_colink) = parse_link_spec(ctx, other_name.trim(), !face, true)?;

                poly_spec.vec_update(&vec![other_id]);

                if with_colink {
                    if face == Face::Tx {
                        self.causes.map_update(other_id, &vec![ids.to_owned()]);
                    } else {
                        self.effects.map_update(other_id, &vec![ids.to_owned()]);
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
                                    self.causes.map_update(other_id, &vec![ids.to_owned()]);
                                } else {
                                    self.effects.map_update(other_id, &vec![ids.to_owned()]);
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
                                            self.causes.map_update(other_id, &vec![ids.to_owned()]);
                                        } else {
                                            self.effects
                                                .map_update(other_id, &vec![ids.to_owned()]);
                                        }
                                    }
                                } else {
                                    return Err(Box::new(AcesError::SpecLinkInvalid))
                                }
                            }
                            poly_spec.vec_update(&mono_spec);
                        }
                        _ => return Err(Box::new(AcesError::SpecMonoInvalid)),
                    }
                }
                if is_flat {
                    return Err(Box::new(AcesError::SpecPolyAmbiguous))
                }
            }
            _ => return Err(Box::new(AcesError::SpecPolyInvalid)),
        }

        if face == Face::Tx {
            for &id in ids {
                self.effects.map_update(id, &poly_spec);
            }
        } else {
            for &id in ids {
                self.causes.map_update(id, &poly_spec);
            }
        }
        Ok(())
    }

    fn add_entry(
        &mut self,
        ctx: &Arc<Mutex<Context>>,
        key: &Yaml,
        value: &Yaml,
    ) -> Result<(), Box<dyn Error>> {
        if let Some(key) = key.as_str() {
            let key = key.trim();
            let port_parsed = parse_port_spec(ctx, key);

            if let Some((ids, face)) = port_parsed {
                self.add_ports(ctx, &ids, face, value)
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

            spec.carrier = spec.effects.keys().chain(spec.causes.keys()).copied().collect();

            Ok(spec)
        } else {
            Err(Box::new(AcesError::SpecNotADict))
        }
    }

    fn from_str<S: AsRef<str>>(
        ctx: &Arc<Mutex<Context>>,
        raw_spec: S,
    ) -> Result<Self, Box<dyn Error>> {
        let docs = YamlLoader::load_from_str(raw_spec.as_ref())?;

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
    fn get_carrier_ids(&self) -> Vec<ID>;
    fn get_causes_by_id(&self, id: ID) -> Option<&Vec<Vec<ID>>>;
    fn get_effects_by_id(&self, id: ID) -> Option<&Vec<Vec<ID>>>;
}

impl CESSpec for String {
    fn get_raw(&self) -> Option<&str> {
        Some(self)
    }

    fn get_name(&self) -> Option<&str> {
        panic!("Attempt to access a phantom specification.")
    }

    fn get_carrier_ids(&self) -> Vec<ID> {
        panic!("Attempt to access a phantom specification.")
    }

    fn get_causes_by_id(&self, _id: ID) -> Option<&Vec<Vec<ID>>> {
        panic!("Attempt to access a phantom specification.")
    }

    fn get_effects_by_id(&self, _id: ID) -> Option<&Vec<Vec<ID>>> {
        panic!("Attempt to access a phantom specification.")
    }
}

impl CESSpec for SpecForYaml {
    fn get_raw(&self) -> Option<&str> {
        None // FIXME
    }

    fn get_name(&self) -> Option<&str> {
        if let Some(ref name) = self.name {
            Some(name.as_ref())
        } else {
            None
        }
    }

    fn get_carrier_ids(&self) -> Vec<ID> {
        self.carrier.iter().copied().collect()
    }

    fn get_causes_by_id(&self, id: ID) -> Option<&Vec<Vec<ID>>> {
        self.causes.get(&id)
    }

    fn get_effects_by_id(&self, id: ID) -> Option<&Vec<Vec<ID>>> {
        self.effects.get(&id)
    }
}

pub(crate) fn spec_from_str<S: AsRef<str>>(
    ctx: &Arc<Mutex<Context>>,
    raw_spec: S,
) -> Result<Box<dyn CESSpec>, Box<dyn Error>> {
    // FIXME infer the format of spec string: yaml, sexpr, ...
    Ok(Box::new(SpecForYaml::from_str(ctx, raw_spec)?))
}
