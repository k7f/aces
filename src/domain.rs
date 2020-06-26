use std::{collections::BTreeSet, fmt, hash};
use crate::{
    AnyId, AtomId, Context, ExclusivelyContextual, InContext, Atomic, AcesError, AcesErrorKind, sat,
};

/// An identifier of a domain element used in c-e structures.
///
/// In line with the theory, the set of all dots is a shared resource
/// common to all c-e structures.  On the other hand, properties of a
/// dot depend on a particular c-e structure, visualization method,
/// etc.
///
/// Therefore, there is no type `Dot` in _aces_.  Instead, structural
/// information is stored in [`CEStructure`] objects and accessed
/// through structural identifiers, [`PortId`], [`LinkId`], [`ForkId`]
/// and [`JoinId`].  Remaining dot-related data is retrieved through
/// `DotId`s from [`Context`] instances (many such instances may
/// coexist in a single process).
///
/// [`PortId`]: crate::PortId
/// [`LinkId`]: crate::LinkId
/// [`ForkId`]: crate::ForkId
/// [`JoinId`]: crate::JoinId
/// [`CEStructure`]: crate::CEStructure
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct DotId(pub(crate) AnyId);

impl DotId {
    #[inline]
    pub const fn get(self) -> AnyId {
        self.0
    }
}

impl From<AnyId> for DotId {
    #[inline]
    fn from(id: AnyId) -> Self {
        DotId(id)
    }
}

impl From<DotId> for AnyId {
    #[inline]
    fn from(id: DotId) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for DotId {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let name = ctx
            .get_dot_name(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::DotMissingForId(*self)))?;
        Ok(name.to_owned())
    }
}

impl Atomic for DotId {
    fn into_dot_id(this: InContext<Self>) -> Option<DotId> {
        Some(*this.get_thing())
    }

    fn into_sat_literal(self, negated: bool) -> sat::Literal {
        sat::Literal::from_atom_id(self.get(), negated)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub enum Polarity {
    Tx,
    Rx,
}

impl std::ops::Not for Polarity {
    type Output = Polarity;

    fn not(self) -> Self::Output {
        match self {
            Polarity::Tx => Polarity::Rx,
            Polarity::Rx => Polarity::Tx,
        }
    }
}

impl fmt::Display for Polarity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Polarity::Tx => write!(f, ">"),
            Polarity::Rx => write!(f, "<"),
        }
    }
}

/// An identifier of a [`Dotset`], a type derived from [`AtomId`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub struct DotsetId(pub(crate) AtomId);

impl DotsetId {
    #[inline]
    pub const fn get(self) -> AtomId {
        self.0
    }
}

impl From<AtomId> for DotsetId {
    #[inline]
    fn from(id: AtomId) -> Self {
        DotsetId(id)
    }
}

impl From<DotsetId> for AtomId {
    #[inline]
    fn from(id: DotsetId) -> Self {
        id.0
    }
}

impl ExclusivelyContextual for DotsetId {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let dotset = ctx
            .get_dotset(*self)
            .ok_or_else(|| AcesError::from(AcesErrorKind::DotsetMissingForId(*self)))?;
        dotset.format_locked(ctx)
    }
}

/// A set of dots: pre-set or post-set of a transition or a pit of a
/// wedge.
///
/// Represented as an ordered and deduplicated `Vec` of `DotId`s.
#[derive(Clone, Eq, Debug)]
pub struct Dotset {
    pub(crate) atom_id:  Option<AtomId>,
    pub(crate) dot_ids: Vec<DotId>,
}

impl Dotset {
    /// [`Dotset`] constructor.
    ///
    /// See also  [`Dotset::new_unchecked()`].
    pub fn new<I>(dot_ids: I) -> Self
    where
        I: IntoIterator<Item = DotId>,
    {
        let dot_ids: BTreeSet<_> = dot_ids.into_iter().collect();

        if dot_ids.is_empty() {
            // FIXME
        }

        Self::new_unchecked(dot_ids)
    }

    /// A more efficient variant of [`Dotset::new()`].
    ///
    /// Note: new [`Dotset`] is created under the assumption that
    /// `dot_ids` are nonempty and listed in ascending order.  If
    /// the caller fails to provide an ordered dotset, the library
    /// may panic in some other call (the constructor itself panics
    /// immediately in debug mode).
    pub fn new_unchecked<I>(dot_ids: I) -> Self
    where
        I: IntoIterator<Item = DotId>,
    {
        let dot_ids: Vec<_> = dot_ids.into_iter().collect();
        trace!("New dotset: {:?}", dot_ids);

        if cfg!(debug_assertions) {
            let mut niter = dot_ids.iter();

            if let Some(nid) = niter.next() {
                let mut prev_nid = *nid;

                for &nid in niter {
                    assert!(prev_nid < nid, "Unordered dotset");
                    prev_nid = nid;
                }
            } else {
                panic!("Empty dotset")
            }
        }

        Dotset { atom_id: None, dot_ids }
    }

    #[inline]
    pub fn get_atom_id(&self) -> AtomId {
        self.atom_id.expect("Attempt to access an uninitialized dotset")
    }

    #[inline]
    pub fn get_id(&self) -> Option<DotsetId> {
        self.atom_id.map(DotsetId)
    }

    #[inline]
    pub fn get_dot_ids(&self) -> &[DotId] {
        self.dot_ids.as_slice()
    }
}

impl PartialEq for Dotset {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.dot_ids == other.dot_ids
    }
}

impl hash::Hash for Dotset {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.dot_ids.hash(state);
    }
}

impl ExclusivelyContextual for Dotset {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let dot_names: Result<Vec<_>, AcesError> = self
            .dot_ids
            .iter()
            .map(|&dot_id| {
                ctx.get_dot_name(dot_id)
                    .ok_or_else(|| AcesErrorKind::DotMissingForId(dot_id).into())
            })
            .collect();

        Ok(format!("({:?})", dot_names?))
    }
}
