use std::{collections::BTreeMap, sync::{Mutex, Arc}, fmt::Debug, error::Error};
use regex::Regex;
use yaml_rust::{Yaml, YamlLoader};
use crate::{Context, context::NameSpace, error::AcesError};

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
    as_source: bool,
    single_word_only: bool,
) -> Result<(Vec<usize>, bool), Box<dyn Error>> {
    if spec.contains(',') {
        let result: Result<Vec<usize>, Box<dyn Error>> = spec.split(',')
            .map(|s| do_take_id(nodes, s.trim(), single_word_only))
            .collect();
        let ids = result?;

        Ok((ids, as_source))
    } else {
        let id = do_take_id(nodes, spec.trim(), single_word_only)?;

        Ok((vec![id], as_source))
    }
}

fn do_parse_port_spec(
    nodes: &mut NameSpace,
    spec: &str,
    single_word_only: bool
) -> Result<Option<(Vec<usize>, bool)>, Box<dyn Error>>
{
    lazy_static! {
        // Node name (untrimmed, unseparated) is any nonempty string not ending in '>' or '<'.
        // Removal of leading and trailing whitespace is done in post processing,
        // as well as comma-separation.
        static ref SOURCE_RE: Regex = Regex::new(r"^(.*[^><])(>+|\s+effects)$").unwrap();
        static ref SINK_RE: Regex = Regex::new(r"^(.*[^><])(<+|\s+causes)$").unwrap();
    }
    if let Some(cap) = SOURCE_RE.captures(spec) {
        let result = post_process_port_spec(nodes, &cap[1], true, single_word_only)?;

        Ok(Some(result))

    } else if let Some(cap) = SINK_RE.captures(spec) {
        let result = post_process_port_spec(nodes, &cap[1], false, single_word_only)?;

        Ok(Some(result))
    } else {
        Ok(None)
    }
}

fn parse_port_spec(ctx: &Arc<Mutex<Context>>, spec: &str) -> Option<(Vec<usize>, bool)> {
    let ref mut nodes = ctx.lock().unwrap().nodes;

    do_parse_port_spec(nodes, spec, false).unwrap_or_else(|_| {
        unreachable!()
    })
}

fn parse_link_spec(
    ctx: &Arc<Mutex<Context>>,
    spec: &str,
    valid_as_source: bool,
    single_word_only: bool,
) -> Result<(usize, bool), Box<dyn Error>>
{
    let ref mut nodes = ctx.lock().unwrap().nodes;

    let result = do_parse_port_spec(nodes, spec, single_word_only)?;
    if let Some((ids, as_source)) = result {
        if as_source == valid_as_source {
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

#[derive(Debug)]
struct SpecForYaml {
    name:    String,
    meta:    BTreeMap<String, Yaml>,
    causes:  BTreeMap<usize, PolyForYaml>,
    effects: BTreeMap<usize, PolyForYaml>,
}

impl SpecForYaml {
    fn add_ports(
        &mut self,
        ctx: &Arc<Mutex<Context>>,
        node_ids: &[usize],
        as_source: bool,
        poly_yaml: &Yaml
    ) -> Result<(), Box<dyn Error>>
    {
        assert!(!node_ids.is_empty());

        let mut poly_spec = PolyForYaml::new();

        match poly_yaml {
            Yaml::String(other_name) => {
                let (other_id, with_colink) =
                    parse_link_spec(ctx, other_name.trim(), !as_source, true)?;

                poly_spec.vec_update(&vec![other_id]);

                if with_colink {
                    if as_source {
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
                                parse_link_spec(ctx, other_name.trim(), !as_source, true)?;

                            poly_spec.vec_update(&vec![other_id]);

                            if with_colink {
                                if as_source {
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
                                        parse_link_spec(ctx, other_name.trim(), !as_source, false)?;

                                    mono_spec.vec_update(&other_id);

                                    if with_colink {
                                        if as_source {
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

        if as_source {
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

            if let Some((node_ids, as_source)) = port {

                self.add_ports(ctx, &node_ids, as_source, value)

            } else if key == "name" {
                if let Some(name) = value.as_str() {
                    if self.name.is_empty() {
                        self.name.push_str(name.trim());

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
        let mut spec = Self {
            name:    Default::default(),
            meta:    Default::default(),
            causes:  Default::default(),
            effects: Default::default(),
        };

        if let Yaml::Hash(ref dict) = yaml {
            for (key, value) in dict {
                spec.add_entry(ctx, key, value)?;
            }
            Ok(spec)
        } else {
            Err(Box::new(AcesError::SpecNotADict))
        }
    }

    fn from_str(ctx: &Arc<Mutex<Context>>, spec: &str) -> Result<Self, Box<dyn Error>> {
        let docs = YamlLoader::load_from_str(spec.into())?;

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
    fn get_name(&self) -> &str;
    fn get_causes_by_id(&self, node_id: usize) -> Option<&Vec<Vec<usize>>>;
    fn get_effects_by_id(&self, node_id: usize) -> Option<&Vec<Vec<usize>>>;
    fn get_source_ids(&self) -> Vec<usize>;
    fn get_sink_ids(&self) -> Vec<usize>;
}

impl CESSpec for SpecForYaml {
    fn get_name(&self) -> &str {
        self.name.as_str()
    }

    fn get_causes_by_id(&self, node_id: usize) -> Option<&Vec<Vec<usize>>> {
        self.causes.get(&node_id)
    }

    fn get_effects_by_id(&self, node_id: usize) -> Option<&Vec<Vec<usize>>> {
        self.effects.get(&node_id)
    }

    fn get_source_ids(&self) -> Vec<usize> {
        self.effects.keys().copied().collect()
    }

    fn get_sink_ids(&self) -> Vec<usize> {
        self.causes.keys().copied().collect()
    }
}

pub(crate) fn spec_from_str(
    ctx:  &Arc<Mutex<Context>>,
    spec: &str,
) -> Result<Box<dyn CESSpec>, Box<dyn Error>>
{
    // FIXME infer the format of spec string: yaml, sexpr, ...
    Ok(Box::new(SpecForYaml::from_str(ctx, spec)?))
}
