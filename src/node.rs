use std::fmt;
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

#[derive(Clone, Copy, PartialEq, Debug)]
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

/// A maximum number of tokens a node may hold.
///
/// This is the type of values of the function _cap<sub>U</sub>_,
/// which maps nodes in the carrier of a c-e structure _U_ to their
/// capacities.  Valid capacities are positive integers or _&omega;_
/// (infinite capacity).  The default value is 1.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
#[repr(transparent)]
pub struct Capacity(usize);

impl Capacity {
    pub fn new_omega() -> Self {
        Capacity(0)
    }

    pub fn new_finite(value: usize) -> Option<Self> {
        if value > 0 {
            Some(Capacity(value))
        } else {
            None
        }
    }

    pub fn is_omega(self) -> bool {
        self.0 == 0
    }

    pub fn is_finite(self) -> bool {
        self.0 > 0
    }
}

impl Default for Capacity {
    fn default() -> Self {
        Capacity(1)
    }
}
