use std::fmt;
use crate::{ID, Context, ExclusivelyContextual, InContext, Atomic, AcesError, AcesErrorKind, sat};

/// An identifier of a single node used in c-e structures.
///
/// In line with the theory, the set of nodes is a shared resource
/// common to all c-e structures.  On the other hand, properties of a
/// node depend on a particular c-e structure, visualization method,
/// etc.
///
/// Therefore, there is no type `Node` in _aces_.  Instead, structural
/// information is stored in [`CEStructure`] objects and accessed
/// through structural identifiers, [`PortID`], [`LinkID`], [`ForkID`]
/// and [`JoinID`].  Remaining node-related data is retrieved through
/// `NodeID`s from [`Context`] instances (many such instances may
/// coexist in the program).
///
/// [`PortID`]: crate::PortID
/// [`LinkID`]: crate::LinkID
/// [`ForkID`]: crate::ForkID
/// [`JoinID`]: crate::JoinID
/// [`CEStructure`]: crate::CEStructure
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct NodeID(pub(crate) ID);

impl NodeID {
    #[inline]
    pub const fn get(self) -> ID {
        self.0
    }
}

impl From<ID> for NodeID {
    #[inline]
    fn from(id: ID) -> Self {
        NodeID(id)
    }
}

impl From<NodeID> for ID {
    #[inline]
    fn from(id: NodeID) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for NodeID {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let name = ctx
            .get_node_name(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeMissingForID(*self)))?;
        Ok(name.to_owned())
    }
}

impl Atomic for NodeID {
    fn into_node_id(this: InContext<Self>) -> Option<NodeID> {
        Some(*this.get_thing())
    }

    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Face {
    Tx,
    Rx,
}

impl std::ops::Not for Face {
    type Output = Face;

    fn not(self) -> Self::Output {
        match self {
            Face::Tx => Face::Rx,
            Face::Rx => Face::Tx,
        }
    }
}

impl fmt::Display for Face {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Face::Tx => write!(f, ">"),
            Face::Rx => write!(f, "<"),
        }
    }
}
