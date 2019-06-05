use std::{collections::BTreeMap, sync::{Mutex, Arc}, fmt::Debug, error::Error};
use yaml_rust::{Yaml, YamlLoader};
use crate::{NodeSpace, error::AcesError};

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

#[derive(Debug)]
struct SpecForYaml {
    nodes:   Arc<Mutex<NodeSpace>>,
    name:    String,
    meta:    BTreeMap<String, Yaml>,
    causes:  BTreeMap<usize, PolyForYaml>,
    effects: BTreeMap<usize, PolyForYaml>,
}

impl SpecForYaml {
    fn add_ports(&mut self, node_ids: &[usize], as_source: bool, poly_yaml: &Yaml) -> Result<(), Box<dyn Error>> {
        assert!(!node_ids.is_empty());

        let mut poly_spec = PolyForYaml::new();

        match poly_yaml {
            Yaml::String(other_name) => {
                let (other_id, with_colink) =
                    self.nodes.lock().unwrap().parse_link_spec(other_name.trim(), !as_source, true)?;

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
                                self.nodes.lock().unwrap().parse_link_spec(other_name.trim(), !as_source, true)?;

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
                                        self.nodes.lock().unwrap().parse_link_spec(other_name.trim(), !as_source, false)?;

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

    fn add_entry(&mut self, key: &Yaml, value: &Yaml) -> Result<(), Box<dyn Error>> {
        if let Some(key) = key.as_str() {
            let key = key.trim();
            let port = self.nodes.lock().unwrap().parse_port_spec(key);

            if let Some((node_ids, as_source)) = port {

                self.add_ports(&node_ids, as_source, value)

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

    fn from_yaml(yaml: &Yaml, nodes: Arc<Mutex<NodeSpace>>) -> Result<Self, Box<dyn Error>> {
        let mut spec = Self {
            nodes,
            name:    Default::default(),
            meta:    Default::default(),
            causes:  Default::default(),
            effects: Default::default(),
        };

        if let Yaml::Hash(ref dict) = yaml {
            for (key, value) in dict {
                spec.add_entry(key, value)?;
            }
            Ok(spec)
        } else {
            Err(Box::new(AcesError::SpecNotADict))
        }
    }

    fn from_str(spec: &str, nodes: Arc<Mutex<NodeSpace>>) -> Result<Self, Box<dyn Error>> {
        let docs = YamlLoader::load_from_str(spec.into())?;

        if docs.is_empty() {
            Err(Box::new(AcesError::SpecEmpty))

        } else if docs.len() == 1 {
            let spec = Self::from_yaml(&docs[0], nodes)?;

            Ok(spec)
        } else {
            Err(Box::new(AcesError::SpecMultiple))
        }
    }

}

pub(crate) trait CESSpec: Debug {
    fn get_name(&self) -> &str;
    fn get_causes(&self, node_name: &str) -> Option<&Vec<Vec<usize>>>;
    fn get_effects(&self, node_name: &str) -> Option<&Vec<Vec<usize>>>;
}

impl CESSpec for SpecForYaml {
    fn get_name(&self) -> &str {
        self.name.as_str()
    }

    fn get_causes(&self, node_name: &str) -> Option<&Vec<Vec<usize>>> {
        self.nodes.lock().unwrap()
            .get_id(node_name)
            .and_then(|ref id| self.causes.get(id))
    }

    fn get_effects(&self, node_name: &str) -> Option<&Vec<Vec<usize>>> {
        self.nodes.lock().unwrap()
            .get_id(node_name)
            .and_then(|ref id| self.effects.get(id))
    }
}

pub(crate) fn spec_from_str(
    spec: &str,
    nodes: Arc<Mutex<NodeSpace>>
) -> Result<Box<dyn CESSpec>, Box<dyn Error>>
{
    // FIXME infer the format of spec string: yaml, sexpr, ...
    Ok(Box::new(SpecForYaml::from_str(spec, nodes)?))
}
