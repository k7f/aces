use std::{fmt, hash};
use crate::{
    Context, ContextHandle, ExclusivelyContextual, Polarity, AtomId, ForkId, JoinId, AcesError,
    AcesErrorKind,
    domain::{Dotset, DotId, DotsetId},
};

/// A common type of one-to-many and many-to-one elements of the fuset
/// representation of c-e structures.
///
/// A wide edge represents a monomial attached to a dot.  There are
/// two variants of a `Wedge`: [`Join`] represents causes and [`Fork`]
/// represents effects.
#[derive(Clone, Eq)]
pub struct Wedge {
    pub(crate) atom_id: Option<AtomId>,
    polarity:           Polarity,
    tip_id:             DotId,
    pit_id:             DotsetId,
}

impl Wedge {
    pub(crate) fn new(polarity: Polarity, tip_id: DotId, pit_id: DotsetId) -> Self {
        Wedge { atom_id: None, polarity, tip_id, pit_id }
    }

    /// [`Fork`]'s constructor.
    ///
    /// See also  [`Wedge::new_fork_unchecked()`].
    pub fn new_fork<I>(ctx: &ContextHandle, tip_id: DotId, arm_ids: I) -> ForkId
    where
        I: IntoIterator<Item = DotId>,
    {
        let pit = Dotset::new(arm_ids);
        trace!("New fork: {:?} -> {:?}", tip_id, pit);
        ctx.lock().unwrap().share_fork_from_tip_and_pit(tip_id, pit)
    }

    /// [`Join`]'s constructor.
    ///
    /// See also  [`Wedge::new_join_unchecked()`].
    pub fn new_join<I>(ctx: &ContextHandle, tip_id: DotId, arm_ids: I) -> JoinId
    where
        I: IntoIterator<Item = DotId>,
    {
        let pit = Dotset::new(arm_ids);
        trace!("New join: {:?} <- {:?}", tip_id, pit);
        ctx.lock().unwrap().share_join_from_tip_and_pit(tip_id, pit)
    }

    /// A more efficient variant of [`Wedge::new_fork()`].
    ///
    /// Note: new [`Fork`] is created under the assumption that
    /// `arm_ids` are nonempty and listed in ascending order.  If the
    /// caller fails to provide an ordered pit, the library may panic
    /// in some other call (the constructor itself panics immediately
    /// in debug mode).
    #[inline]
    pub fn new_fork_unchecked<I>(ctx: &ContextHandle, tip_id: DotId, arm_ids: I) -> ForkId
    where
        I: IntoIterator<Item = DotId>,
    {
        let pit = Dotset::new_unchecked(arm_ids);
        trace!("New fork: {:?} -> {:?}", tip_id, pit);
        ctx.lock().unwrap().share_fork_from_tip_and_pit(tip_id, pit)
    }

    /// A more efficient variant of [`Wedge::new_join()`].
    ///
    /// Note: new [`Join`] is created under the assumption that
    /// `arm_ids` are nonempty and listed in ascending order.  If the
    /// caller fails to provide an ordered pit, the library may panic
    /// in some other call (the constructor itself panics immediately
    /// in debug mode).
    #[inline]
    pub fn new_join_unchecked<I>(ctx: &ContextHandle, tip_id: DotId, arm_ids: I) -> JoinId
    where
        I: IntoIterator<Item = DotId>,
    {
        let pit = Dotset::new_unchecked(arm_ids);
        trace!("New join: {:?} <- {:?}", tip_id, pit);
        ctx.lock().unwrap().share_join_from_tip_and_pit(tip_id, pit)
    }

    #[inline]
    pub fn get_atom_id(&self) -> AtomId {
        match self.polarity {
            Polarity::Tx => self.atom_id.expect("Attempt to access an uninitialized fork"),
            Polarity::Rx => self.atom_id.expect("Attempt to access an uninitialized join"),
        }
    }

    #[inline]
    pub fn get_fork_id(&self) -> Option<ForkId> {
        match self.polarity {
            Polarity::Tx => Some(ForkId(self.get_atom_id())),
            Polarity::Rx => None,
        }
    }

    #[inline]
    pub fn get_join_id(&self) -> Option<JoinId> {
        match self.polarity {
            Polarity::Tx => None,
            Polarity::Rx => Some(JoinId(self.get_atom_id())),
        }
    }

    #[inline]
    pub fn get_polarity(&self) -> Polarity {
        self.polarity
    }

    #[inline]
    pub fn get_tip_id(&self) -> DotId {
        self.tip_id
    }

    #[inline]
    pub fn get_pit_id(&self) -> DotsetId {
        self.pit_id
    }
}

impl fmt::Debug for Wedge {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {{ atom_id: ",
            match self.polarity {
                Polarity::Tx => "Fork",
                Polarity::Rx => "Join",
            }
        )?;
        self.atom_id.fmt(f)?;
        write!(f, ", tip_id: ")?;
        self.tip_id.fmt(f)?;
        write!(f, ", pit_id: ")?;
        self.pit_id.fmt(f)?;
        write!(f, " }}")
    }
}

impl PartialEq for Wedge {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.polarity == other.polarity
            && self.tip_id == other.tip_id
            && self.pit_id == other.pit_id
    }
}

impl hash::Hash for Wedge {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.polarity.hash(state);
        self.tip_id.hash(state);
        self.pit_id.hash(state);
    }
}

impl ExclusivelyContextual for Wedge {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let tip_name = ctx.get_dot_name(self.tip_id).ok_or(match self.polarity {
            Polarity::Tx => AcesError::from(AcesErrorKind::DotMissingForFork(Polarity::Tx)),
            Polarity::Rx => AcesError::from(AcesErrorKind::DotMissingForJoin(Polarity::Rx)),
        })?;

        let pit = ctx
            .get_dotset(self.pit_id)
            .ok_or_else(|| AcesError::from(AcesErrorKind::DotsetMissingForId(self.pit_id)))?;

        let arm_names: Result<Vec<_>, AcesError> = pit
            .get_dot_ids()
            .iter()
            .map(|&dot_id| {
                ctx.get_dot_name(dot_id).ok_or(match self.polarity {
                    Polarity::Tx => AcesError::from(AcesErrorKind::DotMissingForFork(Polarity::Rx)),
                    Polarity::Rx => AcesError::from(AcesErrorKind::DotMissingForJoin(Polarity::Tx)),
                })
            })
            .collect();

        match self.polarity {
            Polarity::Tx => Ok(format!("({} > {:?})", tip_name, arm_names?)),
            Polarity::Rx => Ok(format!("({:?} > {})", arm_names?, tip_name)),
        }
    }
}

/// Forward wide edge: representation of effects.
pub type Fork = Wedge;

/// Backward wide edge: representation of causes.
pub type Join = Wedge;
