use std::collections::BTreeMap;
use crate::DotId;

#[derive(Clone, Default, Debug)]
pub(crate) struct Props {
    pub(crate) title:  Option<String>,
    pub(crate) labels: BTreeMap<DotId, String>,
    // FIXME dot and link geometry, colour, annotations, etc.
}

impl Props {
    pub(crate) fn clear(&mut self) {
        *self = Default::default();
    }
}
