use std::{collections::BTreeMap, error::Error};
use regex::Regex;
use crate::error::AcesError;

#[derive(Default, Debug)]
pub(crate) struct NodeSpace {
    names: Vec<String>,
    ids:   BTreeMap<String, usize>,  // FIXME borrow names
}

impl NodeSpace {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn get_name(&self, id: usize) -> Option<&str> {
        self.names.get(id).map(|s| s.as_str())
    }

    pub(crate) fn get_id(&self, name: &str) -> Option<usize> {
        self.ids.get(name).copied()
    }

    fn do_take_id(&mut self, name: &str, single_word_only: bool) -> Result<usize, Box<dyn Error>> {
        if single_word_only && name.contains(char::is_whitespace) {
            Err(Box::new(AcesError::SpecShortPolyWithWords))
        } else {
            let id = self.ids.get(name).copied().unwrap_or_else(|| {
                let id = self.names.len();

                self.names.push(name.to_string());
                self.ids.insert(name.to_string(), id);

                id
            });

            Ok(id)
        }
    }

    pub(crate) fn take_id(&mut self, name: &str) -> usize {
        self.do_take_id(name, false).unwrap_or_else(|_| {
            unreachable!()
        })
    }

    fn post_process_port_spec(
        &mut self,
        spec: &str,
        as_source: bool,
        single_word_only: bool,
    ) -> Result<(Vec<usize>, bool), Box<dyn Error>> {
        if spec.contains(',') {
            let result: Result<Vec<usize>, Box<dyn Error>> = spec.split(',')
                .map(|s| self.do_take_id(s.trim(), single_word_only))
                .collect();
            let ids = result?;

            Ok((ids, as_source))
        } else {
            let id = self.do_take_id(spec.trim(), single_word_only)?;

            Ok((vec![id], as_source))
        }
    }

    fn do_parse_port_spec(&mut self, spec: &str, single_word_only: bool) -> Result<Option<(Vec<usize>, bool)>, Box<dyn Error>> {
        lazy_static! {
            // Node name (untrimmed, unseparated) is any nonempty string not ending in '>' or '<'.
            // Removal of leading and trailing whitespace is done in post processing,
            // as well as comma-separation.
            static ref SOURCE_RE: Regex = Regex::new(r"^(.*[^><])(>+|\s+effects)$").unwrap();
            static ref SINK_RE: Regex = Regex::new(r"^(.*[^><])(<+|\s+causes)$").unwrap();
        }
        if let Some(cap) = SOURCE_RE.captures(spec) {
            let result = self.post_process_port_spec(&cap[1], true, single_word_only)?;

            Ok(Some(result))

        } else if let Some(cap) = SINK_RE.captures(spec) {
            let result = self.post_process_port_spec(&cap[1], false, single_word_only)?;

            Ok(Some(result))
        } else {
            Ok(None)
        }
    }

    pub(crate) fn parse_port_spec(&mut self, spec: &str) -> Option<(Vec<usize>, bool)> {
        self.do_parse_port_spec(spec, false).unwrap_or_else(|_| {
            unreachable!()
        })
    }

    pub(crate) fn parse_link_spec(
        &mut self,
        spec: &str,
        valid_as_source: bool,
        single_word_only: bool,
    ) -> Result<(usize, bool), Box<dyn Error>> {
        let result = self.do_parse_port_spec(spec, single_word_only)?;
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
            let id = self.do_take_id(spec, single_word_only)?;

            Ok((id, false))
        }
    }
}
