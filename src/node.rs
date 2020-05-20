use std::{collections::BTreeSet, fmt, hash};
use crate::{
    AnyId, AtomId, Context, ExclusivelyContextual, InContext, Atomic, AcesError, AcesErrorKind, sat,
};

/// An identifier of a single node used in c-e structures.
///
/// In line with the theory, the set of nodes is a shared resource
/// common to all c-e structures.  On the other hand, properties of a
/// node depend on a particular c-e structure, visualization method,
/// etc.
///
/// Therefore, there is no type `Node` in _aces_.  Instead, structural
/// information is stored in [`CEStructure`] objects and accessed
/// through structural identifiers, [`PortId`], [`LinkId`], [`ForkId`]
/// and [`JoinId`].  Remaining node-related data is retrieved through
/// `NodeId`s from [`Context`] instances (many such instances may
/// coexist in a single process).
///
/// [`PortId`]: crate::PortId
/// [`LinkId`]: crate::LinkId
/// [`ForkId`]: crate::ForkId
/// [`JoinId`]: crate::JoinId
/// [`CEStructure`]: crate::CEStructure
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct NodeId(pub(crate) AnyId);

impl NodeId {
    #[inline]
    pub const fn get(self) -> AnyId {
        self.0
    }
}

impl From<AnyId> for NodeId {
    #[inline]
    fn from(id: AnyId) -> Self {
        NodeId(id)
    }
}

impl From<NodeId> for AnyId {
    #[inline]
    fn from(id: NodeId) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for NodeId {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let name = ctx
            .get_node_name(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeMissingForId(*self)))?;
        Ok(name.to_owned())
    }
}

impl Atomic for NodeId {
    fn into_node_id(this: InContext<Self>) -> Option<NodeId> {
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

/// An identifier of a [`NodeSet`], a type derived from [`AtomId`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct NodeSetId(pub(crate) AtomId);

impl NodeSetId {
    #[inline]
    pub const fn get(self) -> AtomId {
        self.0
    }
}

impl From<AtomId> for NodeSetId {
    #[inline]
    fn from(id: AtomId) -> Self {
        NodeSetId(id)
    }
}

impl From<NodeSetId> for AtomId {
    #[inline]
    fn from(id: NodeSetId) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for NodeSetId {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let node_set = ctx
            .get_node_set(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::NodeSetMissingForId(*self)))?;
        node_set.format_locked(ctx)
    }
}

/// A set of nodes: pre-set or post-set of a transition or a suit of a
/// harc.
///
/// Represented as an ordered and deduplicated `Vec` of `NodeId`s.
#[derive(Clone, Eq, Debug)]
pub struct NodeSet {
    pub(crate) atom_id:  Option<AtomId>,
    pub(crate) node_ids: Vec<NodeId>,
}

impl NodeSet {
    /// [`NodeSet`] constructor.
    ///
    /// See also  [`NodeSet::new_unchecked()`].
    pub fn new<I>(node_ids: I) -> Self
    where
        I: IntoIterator<Item = NodeId>,
    {
        let node_ids: BTreeSet<_> = node_ids.into_iter().collect();

        if node_ids.is_empty() {
            // FIXME
        }

        Self::new_unchecked(node_ids)
    }

    /// A more efficient variant of [`NodeSet::new()`].
    ///
    /// Note: new [`NodeSet`] is created under the assumption that
    /// `node_ids` are nonempty and listed in ascending order.  If the
    /// caller fails to provide an ordered node set, the library may
    /// panic in some other call (the constructor itself panics
    /// immediately in debug mode).
    pub fn new_unchecked<I>(node_ids: I) -> Self
    where
        I: IntoIterator<Item = NodeId>,
    {
        let node_ids: Vec<_> = node_ids.into_iter().collect();
        trace!("New node set: {:?}", node_ids);

        if cfg!(debug_assertions) {
            let mut niter = node_ids.iter();

            if let Some(nid) = niter.next() {
                let mut prev_nid = *nid;

                for &nid in niter {
                    assert!(prev_nid < nid, "Unordered node set");
                    prev_nid = nid;
                }
            } else {
                panic!("Empty node set")
            }
        }

        NodeSet { atom_id: None, node_ids }
    }

    #[inline]
    pub fn get_atom_id(&self) -> AtomId {
        self.atom_id.expect("Attempt to access an uninitialized node set")
    }

    #[inline]
    pub fn get_id(&self) -> Option<NodeSetId> {
        self.atom_id.map(NodeSetId)
    }

    #[inline]
    pub fn get_node_ids(&self) -> &[NodeId] {
        self.node_ids.as_slice()
    }
}

impl PartialEq for NodeSet {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.node_ids == other.node_ids
    }
}

impl hash::Hash for NodeSet {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.node_ids.hash(state);
    }
}

impl ExclusivelyContextual for NodeSet {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let node_names: Result<Vec<_>, AcesError> = self
            .node_ids
            .iter()
            .map(|&node_id| {
                ctx.get_node_name(node_id)
                    .ok_or_else(|| AcesErrorKind::NodeMissingForId(node_id).into())
            })
            .collect();

        Ok(format!("({:?})", node_names?))
    }
}
