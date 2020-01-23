use std::collections::BTreeMap;
use crate::NodeID;

#[derive(Clone, Default, Debug)]
pub(crate) struct Props {
    pub(crate) title:  Option<String>,
    pub(crate) labels: BTreeMap<NodeID, String>,
    // FIXME node and link geometry, colour, annotations, etc.
}

impl Props {
    pub(crate) fn clear(&mut self) {
        *self = Default::default();
    }
}
