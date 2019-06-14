use crate::ID;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct NodeID(pub(crate) ID);

impl NodeID {
    #[inline]
    pub const fn get(self) -> ID {
        self.0
    }
}

impl From<NodeID> for ID {
    fn from(id: NodeID) -> Self {
        id.0
    }
}
