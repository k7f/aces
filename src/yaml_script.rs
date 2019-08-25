use std::{collections::BTreeMap, fmt, error::Error};
use regex::Regex;
use yaml_rust::{Yaml, YamlLoader};
use crate::{
    ContextHandle, NodeID, node,
    name::NameSpace,
    Content, PartialContent,
    content::{PolyForContent, MonoForContent},
};

#[derive(Debug, Clone)]
pub(crate) enum YamlScriptError {
    Empty,
    Multiple,
    NotADict,
    KeyNotString,
    NameNotString,
    NameDup,
    PolyInvalid,
    PolyAmbiguous,
    ShortPolyWithWords,
    MonoInvalid,
    LinkInvalid,
    LinkReversed,
    LinkList,
}

impl fmt::Display for YamlScriptError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl Error for YamlScriptError {
    fn description(&self) -> &str {
        use YamlScriptError::*;

        match self {
            Empty => "YAML description is empty",
            Multiple => "Multiple YAML descriptions",
            NotADict => "Bad YAML description (not a dictionary)",
            KeyNotString => "Non-string key in YAML description",
            NameNotString => "Non-string c-e structure name in YAML description",
            NameDup => "Duplicated c-e structure name in YAML description",
            PolyInvalid => "Invalid YAML description of a polynomial",
            PolyAmbiguous => "Ambiguous YAML description of a polynomial",
            ShortPolyWithWords => {
                "Multi-word node name is invalid in short YAML description of a polynomial"
            }
            MonoInvalid => "Invalid monomial in YAML description of a polynomial",
            LinkInvalid => "Invalid link in YAML description of a polynomial",
            LinkReversed => "Reversed link in YAML description of a polynomial",
            LinkList => "Link list is invalid in YAML description of a polynomial",
        }
    }
}

fn do_share_name<S: AsRef<str>>(
    nodes: &mut NameSpace,
    name: S,
    single_word_only: bool,
) -> Result<NodeID, Box<dyn Error>> {
    if single_word_only && name.as_ref().contains(char::is_whitespace) {
        Err(Box::new(YamlScriptError::ShortPolyWithWords))
    } else {
        Ok(NodeID(nodes.share_name(name)))
    }
}

fn post_process_port_description<S: AsRef<str>>(
    nodes: &mut NameSpace,
    description: S,
    single_word_only: bool,
) -> Result<Vec<NodeID>, Box<dyn Error>> {
    if description.as_ref().contains(',') {
        let result: Result<Vec<NodeID>, Box<dyn Error>> = description
            .as_ref()
            .split(',')
            .map(|s| do_share_name(nodes, s.trim(), single_word_only))
            .collect();
        let ids = result?;

        Ok(ids)
    } else {
        let id = do_share_name(nodes, description.as_ref().trim(), single_word_only)?;

        Ok(vec![id])
    }
}

type PortParsed = (Vec<NodeID>, node::Face);

fn do_parse_port_description<S: AsRef<str>>(
    nodes: &mut NameSpace,
    description: S,
    single_word_only: bool,
) -> Result<Option<PortParsed>, Box<dyn Error>> {
    lazy_static! {
        // Node name (untrimmed, unseparated) is any nonempty string not ending in '>' or '<'.
        // Removal of leading and trailing whitespace is done in post processing,
        // as well as comma-separation.
        static ref TX_RE: Regex = Regex::new(r"^(.*[^><])(>+|\s+effects)$").unwrap();
        static ref RX_RE: Regex = Regex::new(r"^(.*[^><])(<+|\s+causes)$").unwrap();
    }
    if let Some(cap) = TX_RE.captures(description.as_ref()) {
        let ids = post_process_port_description(nodes, &cap[1], single_word_only)?;

        Ok(Some((ids, node::Face::Tx)))
    } else if let Some(cap) = RX_RE.captures(description.as_ref()) {
        let ids = post_process_port_description(nodes, &cap[1], single_word_only)?;

        Ok(Some((ids, node::Face::Rx)))
    } else {
        Ok(None)
    }
}

fn parse_port_description<S: AsRef<str>>(
    ctx: &ContextHandle,
    description: S,
) -> Option<PortParsed> {
    let ref mut nodes = ctx.lock().unwrap().nodes;

    do_parse_port_description(nodes, description, false).unwrap_or_else(|_| unreachable!())
}

fn parse_link_description<S: AsRef<str> + Copy>(
    ctx: &ContextHandle,
    description: S,
    valid_face: node::Face,
    single_word_only: bool,
) -> Result<(NodeID, bool), Box<dyn Error>> {
    let ref mut nodes = ctx.lock().unwrap().nodes;

    let link_with_colink = do_parse_port_description(nodes, description, single_word_only)?;

    if let Some((ids, face)) = link_with_colink {
        if face == valid_face {
            if ids.len() == 1 {
                Ok((ids[0], true))
            } else {
                Err(Box::new(YamlScriptError::LinkList))
            }
        } else {
            Err(Box::new(YamlScriptError::LinkReversed))
        }
    } else {
        let id = do_share_name(nodes, description, single_word_only)?;

        Ok((id, false))
    }
}

/// Intermediate representation of a c-e structure.
///
/// This is returned by the parser of YAML-formatted strings and then
/// transformed into internal data structures, wich are used for
/// analysis or during simulation.
#[derive(Debug)]
pub(crate) struct YamlContent {
    name:    Option<String>,
    meta:    BTreeMap<String, Yaml>,
    content: PartialContent,
}

impl YamlContent {
    pub(crate) fn new(ctx: &ContextHandle) -> Self {
        YamlContent {
            name:    Default::default(),
            meta:    Default::default(),
            content: PartialContent::new(ctx),
        }
    }

    fn add_ports(
        &mut self,
        ids: &[NodeID],
        face: node::Face,
        poly_yaml: &Yaml,
    ) -> Result<(), Box<dyn Error>> {
        assert!(!ids.is_empty());

        let mut poly_content = PolyForContent::new();

        match poly_yaml {
            Yaml::String(other_name) => {
                let (other_id, with_colink) = parse_link_description(
                    self.content.get_context(),
                    other_name.trim(),
                    !face,
                    true,
                )?;

                poly_content.add_mono(vec![other_id]);

                if with_colink {
                    if face == node::Face::Tx {
                        self.content.add_to_causes(other_id, &[ids.to_owned()]);
                    } else {
                        self.content.add_to_effects(other_id, &[ids.to_owned()]);
                    }
                }
            }
            Yaml::Array(table) => {
                let mut is_flat = true;

                for value in table {
                    match value {
                        Yaml::String(other_name) => {
                            let (other_id, with_colink) = parse_link_description(
                                self.content.get_context(),
                                other_name.trim(),
                                !face,
                                true,
                            )?;

                            poly_content.add_mono(vec![other_id]);

                            if with_colink {
                                if face == node::Face::Tx {
                                    self.content.add_to_causes(other_id, &[ids.to_owned()]);
                                } else {
                                    self.content.add_to_effects(other_id, &[ids.to_owned()]);
                                }
                            }
                        }
                        Yaml::Array(table) => {
                            is_flat = false;

                            let mut mono_content = MonoForContent::new();
                            for value in table {
                                if let Some(other_name) = value.as_str() {
                                    let (other_id, with_colink) = parse_link_description(
                                        self.content.get_context(),
                                        other_name.trim(),
                                        !face,
                                        false,
                                    )?;

                                    mono_content.add_node(other_id);

                                    if with_colink {
                                        if face == node::Face::Tx {
                                            self.content.add_to_causes(other_id, &[ids.to_owned()]);
                                        } else {
                                            self.content
                                                .add_to_effects(other_id, &[ids.to_owned()]);
                                        }
                                    }
                                } else {
                                    return Err(Box::new(YamlScriptError::LinkInvalid))
                                }
                            }
                            poly_content.add_mono(mono_content.into_content());
                        }
                        _ => return Err(Box::new(YamlScriptError::MonoInvalid)),
                    }
                }
                if is_flat {
                    return Err(Box::new(YamlScriptError::PolyAmbiguous))
                }
            }
            _ => return Err(Box::new(YamlScriptError::PolyInvalid)),
        }

        if face == node::Face::Tx {
            for &id in ids {
                self.content.add_to_effects(id, poly_content.as_content());
            }
        } else {
            for &id in ids {
                self.content.add_to_causes(id, poly_content.as_content());
            }
        }
        Ok(())
    }

    fn add_entry(&mut self, key: &Yaml, value: &Yaml) -> Result<(), Box<dyn Error>> {
        if let Some(key) = key.as_str() {
            let key = key.trim();
            let port_parsed = parse_port_description(self.content.get_context(), key);

            if let Some((ids, face)) = port_parsed {
                self.add_ports(&ids, face, value)
            } else if key == "name" {
                if let Some(name) = value.as_str() {
                    if self.name.is_none() {
                        self.name = Some(name.trim().to_owned());

                        Ok(())
                    } else {
                        Err(Box::new(YamlScriptError::NameDup))
                    }
                } else {
                    Err(Box::new(YamlScriptError::NameNotString))
                }
            } else {
                // FIXME handle duplicates
                self.meta.insert(key.to_string(), value.clone());

                Ok(())
            }
        } else {
            Err(Box::new(YamlScriptError::KeyNotString))
        }
    }

    fn from_yaml(ctx: &ContextHandle, yaml: &Yaml) -> Result<Self, Box<dyn Error>> {
        if let Yaml::Hash(ref dict) = yaml {
            let mut result = Self::new(ctx);

            for (key, value) in dict {
                result.add_entry(key, value)?;
            }

            Ok(result)
        } else {
            Err(Box::new(YamlScriptError::NotADict))
        }
    }

    pub(crate) fn from_str<S: AsRef<str>>(
        ctx: &ContextHandle,
        script: S,
    ) -> Result<Self, Box<dyn Error>> {
        let docs = YamlLoader::load_from_str(script.as_ref())?;

        if docs.is_empty() {
            Err(Box::new(YamlScriptError::Empty))
        } else if docs.len() == 1 {
            let result = Self::from_yaml(ctx, &docs[0])?;
            Ok(result)
        } else {
            Err(Box::new(YamlScriptError::Multiple))
        }
    }
}

impl Content for YamlContent {
    fn get_script(&self) -> Option<&str> {
        None // FIXME
    }

    fn get_name(&self) -> Option<&str> {
        if let Some(ref name) = self.name {
            Some(name.as_ref())
        } else {
            None
        }
    }

    fn get_carrier_ids(&mut self) -> Vec<NodeID> {
        self.content.get_carrier_ids()
    }

    fn get_causes_by_id(&self, id: NodeID) -> Option<&Vec<Vec<NodeID>>> {
        self.content.get_causes_by_id(id)
    }

    fn get_effects_by_id(&self, id: NodeID) -> Option<&Vec<Vec<NodeID>>> {
        self.content.get_effects_by_id(id)
    }
}
