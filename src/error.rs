use std::{num, fmt, error::Error};
use crate::{
    ContextHandle, Contextual, Polarity, DotId, AtomId, PortId, LinkId, ForkId, JoinId, FusetId,
    Multiplicity, Capacity, domain::DotsetId,
};

#[derive(Clone, Debug)]
pub enum AcesErrorKind {
    ContextMismatch,
    PolynomialPolarityMismatch,
    WedgeNotAForkMismatch(JoinId),
    WedgeNotAJoinMismatch(ForkId),
    DotMissingForId(DotId),
    AtomMissingForId(AtomId),
    PortMissingForId(PortId),
    LinkMissingForId(LinkId),
    WedgeMissingForId(AtomId),
    ForkMissingForId(ForkId),
    JoinMissingForId(JoinId),
    DotsetMissingForId(DotsetId),
    FusetMissingForId(FusetId),
    BottomAtomAccess,
    AtomicsNotOrdered,

    DotMissingForPort(Polarity),
    DotMissingForLink(Polarity),
    DotMissingForFork(Polarity),
    DotMissingForJoin(Polarity),
    FiringDotMissing(Polarity, DotId),
    FiringDotDuplicated(Polarity, DotId),
    FiringOverlap,
    IncoherentStructure(String, u32, (Polarity, String, String)),

    LeakedInhibitor(DotId, Multiplicity),
    StateUnderflow(DotId, Multiplicity, Multiplicity),
    StateOverflow(DotId, Multiplicity, Multiplicity),
    CapacityOverflow(DotId, Capacity, Multiplicity),
    MultiplicityOverflow(String),
    ParseIntError(num::ParseIntError),
    ParseFloatError(num::ParseFloatError),

    ModuleSolving,
    EmptySolving,
    EmptyClauseRejectedByFormula(String),
    EmptyClauseRejectedBySolver(String),
    EmptyCausesOfInternalDot(String),
    EmptyEffectsOfInternalDot(String),
    DotsetUsedAsSATLiteral(DotsetId),
    FusetUsedAsSATLiteral(FusetId),

    UnlistedAtomicInMonomial,
    IncoherencyLeak,
    NoModelToInhibit,

    UnknownScriptFormat,
}

impl fmt::Display for AcesErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AcesErrorKind::*;

        match self {
            ContextMismatch => write!(f, "Context mismatch"),
            PolynomialPolarityMismatch => {
                write!(f, "Attempt to combine polynomials with opposite polarities")
            }
            WedgeNotAForkMismatch(join_id) => write!(f, "Expected fork, but wedge is {:?}", join_id),
            WedgeNotAJoinMismatch(fork_id) => write!(f, "Expected join, but wedge is {:?}", fork_id),
            DotMissingForId(dot_id) => write!(f, "There is no dot with {:?}", dot_id),
            AtomMissingForId(atom_id) => write!(f, "There is no atom with {:?}", atom_id),
            PortMissingForId(port_id) => write!(f, "There is no port with {:?}", port_id),
            LinkMissingForId(link_id) => write!(f, "There is no link with {:?}", link_id),
            WedgeMissingForId(wedge_id) => write!(f, "There is no wedge with {:?}", wedge_id),
            ForkMissingForId(fork_id) => write!(f, "There is no fork with {:?}", fork_id),
            JoinMissingForId(join_id) => write!(f, "There is no join with {:?}", join_id),
            DotsetMissingForId(pit_id) => write!(f, "There is no dotset with {:?}", pit_id),
            FusetMissingForId(fuset_id) => write!(f, "There is no fuset with {:?}", fuset_id),
            BottomAtomAccess => write!(f, "Attempt to access the bottom atom"),
            AtomicsNotOrdered => write!(f, "Atomics have to be given in strictly increasing order"),

            DotMissingForPort(polarity) => write!(
                f,
                "Missing dot for {} port",
                if *polarity == Polarity::Tx { "sending" } else { "receiving" }
            ),
            DotMissingForLink(polarity) => write!(
                f,
                "Missing {} dot for link",
                if *polarity == Polarity::Tx { "sending" } else { "receiving" }
            ),
            DotMissingForFork(polarity) => write!(
                f,
                "Missing {} dot for fork",
                if *polarity == Polarity::Tx { "sending" } else { "receiving" }
            ),
            DotMissingForJoin(polarity) => write!(
                f,
                "Missing {} dot for join",
                if *polarity == Polarity::Tx { "sending" } else { "receiving" }
            ),
            FiringDotMissing(polarity, dot_id) => write!(
                f,
                "Missing {} dot {:?} in firing component",
                if *polarity == Polarity::Tx { "sending" } else { "receiving" },
                dot_id
            ),
            FiringDotDuplicated(polarity, dot_id) => write!(
                f,
                "Duplicated {} dot {:?} in firing component",
                if *polarity == Polarity::Tx { "sending" } else { "receiving" },
                dot_id
            ),
            FiringOverlap => write!(f, "Attempt to create a non-disjoint firing component"),
            IncoherentStructure(ces_name, num_thin_links, (polarity, tx_name, rx_name)) => {
                if *num_thin_links == 1 {
                    write!(
                        f,
                        "Structure '{}' is incoherent; there is one thin link: ({} {} {})",
                        ces_name, tx_name, polarity, rx_name
                    )
                } else {
                    write!(
                        f,
                        "Structure '{}' is incoherent; there are thin links: ({} {} {}) and {} \
                         more..",
                        ces_name,
                        tx_name,
                        polarity,
                        rx_name,
                        *num_thin_links - 1,
                    )
                }
            }

            LeakedInhibitor(dot_id, tokens_before) => {
                write!(f, "Leaked inhibitor at {:?} firing from {} tokens", dot_id, tokens_before)
            }
            StateUnderflow(dot_id, tokens_before, num_tokens) => write!(
                f,
                "State underflow at {:?} after subtracting {} from {}",
                dot_id, num_tokens, tokens_before
            ),
            StateOverflow(dot_id, tokens_before, num_tokens) => write!(
                f,
                "State overflow at {:?} after adding {} to {}",
                dot_id, num_tokens, tokens_before
            ),
            CapacityOverflow(dot_id, capacity, num_tokens) => {
                write!(f, "Capacity overflow at {:?} set to {} > {}", dot_id, num_tokens, capacity)
            }
            MultiplicityOverflow(when_happened) => {
                write!(f, "Multiplicity overflow when {}", when_happened)
            }
            ParseIntError(err) => err.fmt(f),
            ParseFloatError(err) => err.fmt(f),

            ModuleSolving => write!(f, "Attempt to solve a module"),
            EmptySolving => write!(f, "Attempt to solve an empty structure"),
            EmptyClauseRejectedByFormula(name) => {
                write!(f, "Empty {} clause rejected by formula", name)
            }
            EmptyClauseRejectedBySolver(name) => {
                write!(f, "Empty {} clause rejected by solver", name)
            }
            EmptyCausesOfInternalDot(name) => {
                write!(f, "Empty cause polynomial of internal dot '{}'", name)
            }
            EmptyEffectsOfInternalDot(name) => {
                write!(f, "Empty effect polynomial of internal dot '{}'", name)
            }
            DotsetUsedAsSATLiteral(pit_id) => write!(f, "{:?} used as SAT literal", pit_id),
            FusetUsedAsSATLiteral(fuset_id) => write!(f, "{:?} used as SAT literal", fuset_id),

            UnlistedAtomicInMonomial => write!(f, "Monomial contains an unlisted atomic"),
            IncoherencyLeak => write!(f, "Unexpected incoherence of a c-e structure"),
            NoModelToInhibit => write!(f, "Attempt to inhibit a nonexistent model"),

            UnknownScriptFormat => write!(f, "Unrecognized script format"),
        }
    }
}

impl AcesErrorKind {
    pub fn with_context(self, context: &ContextHandle) -> AcesError {
        AcesError { context: Some(context.clone()), kind: self }
    }
}

impl From<num::ParseIntError> for AcesErrorKind {
    #[inline]
    fn from(kind: num::ParseIntError) -> Self {
        AcesErrorKind::ParseIntError(kind)
    }
}

impl From<num::ParseFloatError> for AcesErrorKind {
    #[inline]
    fn from(kind: num::ParseFloatError) -> Self {
        AcesErrorKind::ParseFloatError(kind)
    }
}

#[derive(Clone, Debug)]
pub struct AcesError {
    context: Option<ContextHandle>,
    kind:    AcesErrorKind,
}

impl From<AcesErrorKind> for AcesError {
    #[inline]
    fn from(kind: AcesErrorKind) -> Self {
        AcesError { context: None, kind }
    }
}

impl fmt::Display for AcesError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AcesErrorKind::*;

        if let Some(ref ctx) = self.context {
            match self.kind {
                WedgeNotAForkMismatch(join_id) => {
                    write!(f, "Expected fork, but wedge is a join {}", join_id.with(ctx))
                }
                WedgeNotAJoinMismatch(fork_id) => {
                    write!(f, "Expected join, but wedge is a fork {}", fork_id.with(ctx))
                }
                FiringDotMissing(polarity, dot_id) => write!(
                    f,
                    "Missing {} dot \"{}\" in firing component",
                    if polarity == Polarity::Tx { "sending" } else { "receiving" },
                    dot_id.with(ctx),
                ),
                FiringDotDuplicated(polarity, dot_id) => write!(
                    f,
                    "Duplicated {} dot \"{}\" in firing component",
                    if polarity == Polarity::Tx { "sending" } else { "receiving" },
                    dot_id.with(ctx),
                ),
                LeakedInhibitor(dot_id, tokens_before) => write!(
                    f,
                    "Leaked inhibitor at dot \"{}\" firing from {} tokens",
                    dot_id.with(ctx),
                    tokens_before
                ),
                StateUnderflow(dot_id, tokens_before, num_tokens) => write!(
                    f,
                    "State underflow at dot \"{}\" after subtracting {} from {}",
                    dot_id.with(ctx),
                    num_tokens,
                    tokens_before
                ),
                StateOverflow(dot_id, tokens_before, num_tokens) => write!(
                    f,
                    "State overflow at dot \"{}\" after adding {} to {}",
                    dot_id.with(ctx),
                    num_tokens,
                    tokens_before
                ),
                CapacityOverflow(dot_id, capacity, num_tokens) => write!(
                    f,
                    "Capacity overflow at dot \"{}\" set to {} > {}",
                    dot_id.with(ctx),
                    num_tokens,
                    capacity
                ),

                ref kind => kind.fmt(f),
            }
        } else {
            self.kind.fmt(f)
        }
    }
}

impl Error for AcesError {}
