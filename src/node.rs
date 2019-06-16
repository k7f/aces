use crate::ID;

/// An identifier of a single node used in c-e structures.
///
/// In line with the theory, the set of nodes is a shared resource
/// common to all c-e structures.  On the other hand, properties of a
/// node depend on a particular c-e structure, visualization method,
/// etc.  Therefore, there is no type `Node` in _aces_.  Instead,
/// structural information is stored in [`CES`] objects and accessed
/// through structural identifiers, [`PortID`] and [`LinkID`].
/// Remaining node-related data is retrieved through `NodeID`s from
/// [`Context`] instances (many such instances may coexist in the
/// program).
///
/// [`PortID`]: crate::PortID
/// [`LinkID`]: crate::LinkID
/// [`CES`]: crate::CES
/// [`Context`]: crate::Context
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
