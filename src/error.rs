use std::{num, fmt, error::Error};
use crate::{
    ContextHandle, Contextual, Face, NodeId, AtomId, PortId, LinkId, ForkId, JoinId, Multiplicity,
    Capacity, node::NodeSetId,
};

#[derive(Clone, Debug)]
pub enum AcesErrorKind {
    ContextMismatch,
    PolynomialFaceMismatch,
    HarcNotAForkMismatch(JoinId),
    HarcNotAJoinMismatch(ForkId),
    NodeMissingForId(NodeId),
    AtomMissingForId(AtomId),
    PortMissingForId(PortId),
    LinkMissingForId(LinkId),
    HarcMissingForId(AtomId),
    ForkMissingForId(ForkId),
    JoinMissingForId(JoinId),
    NodeSetMissingForId(NodeSetId),
    BottomAtomAccess,
    AtomicsNotOrdered,

    NodeMissingForPort(Face),
    NodeMissingForLink(Face),
    NodeMissingForFork(Face),
    NodeMissingForJoin(Face),
    FiringNodeMissing(Face, NodeId),
    FiringNodeDuplicated(Face, NodeId),
    FiringOverlap,
    IncoherentStructure(String, u32, (Face, String, String)),

    LeakedInhibitor(NodeId, Multiplicity),
    StateUnderflow(NodeId, Multiplicity, Multiplicity),
    StateOverflow(NodeId, Multiplicity, Multiplicity),
    CapacityOverflow(NodeId, Capacity, Multiplicity),
    MultiplicityOverflow(String),
    ParseIntError(num::ParseIntError),
    ParseFloatError(num::ParseFloatError),

    EmptyClauseRejectedByFormula(String),
    EmptyClauseRejectedBySolver(String),
    EmptyCausesOfInternalNode(String),
    EmptyEffectsOfInternalNode(String),
    NodeSetUsedAsSATLiteral(NodeSetId),

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
            PolynomialFaceMismatch => {
                write!(f, "Attempt to combine polynomials attached to opposite faces")
            }
            HarcNotAForkMismatch(join_id) => write!(f, "Expected fork, but harc is {:?}", join_id),
            HarcNotAJoinMismatch(fork_id) => write!(f, "Expected join, but harc is {:?}", fork_id),
            NodeMissingForId(node_id) => write!(f, "There is no node with {:?}", node_id),
            AtomMissingForId(atom_id) => write!(f, "There is no atom with {:?}", atom_id),
            PortMissingForId(port_id) => write!(f, "There is no port with {:?}", port_id),
            LinkMissingForId(link_id) => write!(f, "There is no link with {:?}", link_id),
            HarcMissingForId(harc_id) => write!(f, "There is no harc with {:?}", harc_id),
            ForkMissingForId(fork_id) => write!(f, "There is no fork with {:?}", fork_id),
            JoinMissingForId(join_id) => write!(f, "There is no join with {:?}", join_id),
            NodeSetMissingForId(mono_id) => write!(f, "There is no node set with {:?}", mono_id),
            BottomAtomAccess => write!(f, "Attempt to access the bottom atom"),
            AtomicsNotOrdered => write!(f, "Atomics have to be given in strictly increasing order"),

            NodeMissingForPort(face) => write!(
                f,
                "Missing node for {} port",
                if *face == Face::Tx { "sending" } else { "receiving" }
            ),
            NodeMissingForLink(face) => write!(
                f,
                "Missing {} node for link",
                if *face == Face::Tx { "sending" } else { "receiving" }
            ),
            NodeMissingForFork(face) => write!(
                f,
                "Missing {} node for fork",
                if *face == Face::Tx { "sending" } else { "receiving" }
            ),
            NodeMissingForJoin(face) => write!(
                f,
                "Missing {} node for join",
                if *face == Face::Tx { "sending" } else { "receiving" }
            ),
            FiringNodeMissing(face, node_id) => write!(
                f,
                "Missing {} node {:?} in firing component",
                if *face == Face::Tx { "sending" } else { "receiving" },
                node_id
            ),
            FiringNodeDuplicated(face, node_id) => write!(
                f,
                "Duplicated {} node {:?} in firing component",
                if *face == Face::Tx { "sending" } else { "receiving" },
                node_id
            ),
            FiringOverlap => write!(f, "Attempt to create a non-disjoint firing component"),
            IncoherentStructure(ces_name, num_thin_links, (face, tx_name, rx_name)) => {
                if *num_thin_links == 1 {
                    write!(
                        f,
                        "Structure '{}' is incoherent; there is one thin link: ({} {} {})",
                        ces_name, tx_name, face, rx_name
                    )
                } else {
                    write!(
                        f,
                        "Structure '{}' is incoherent; there are thin links: ({} {} {}) and {} \
                         more..",
                        ces_name,
                        tx_name,
                        face,
                        rx_name,
                        *num_thin_links - 1,
                    )
                }
            }

            LeakedInhibitor(node_id, tokens_before) => {
                write!(f, "Leaked inhibitor at {:?} firing from {} tokens", node_id, tokens_before)
            }
            StateUnderflow(node_id, tokens_before, num_tokens) => write!(
                f,
                "State underflow at {:?} after subtracting {} from {}",
                node_id, num_tokens, tokens_before
            ),
            StateOverflow(node_id, tokens_before, num_tokens) => write!(
                f,
                "State overflow at {:?} after adding {} to {}",
                node_id, num_tokens, tokens_before
            ),
            CapacityOverflow(node_id, capacity, num_tokens) => {
                write!(f, "Capacity overflow at {:?} set to {} > {}", node_id, num_tokens, capacity)
            }
            MultiplicityOverflow(when_happened) => {
                write!(f, "Multiplicity overflow when {}", when_happened)
            }
            ParseIntError(err) => err.fmt(f),
            ParseFloatError(err) => err.fmt(f),

            EmptyClauseRejectedByFormula(name) => {
                write!(f, "Empty {} clause rejected by formula", name)
            }
            EmptyClauseRejectedBySolver(name) => {
                write!(f, "Empty {} clause rejected by solver", name)
            }
            EmptyCausesOfInternalNode(name) => {
                write!(f, "Empty cause polynomial of internal node '{}'", name)
            }
            EmptyEffectsOfInternalNode(name) => {
                write!(f, "Empty effect polynomial of internal node '{}'", name)
            }
            NodeSetUsedAsSATLiteral(mono_id) => write!(f, "{:?} used as SAT literal", mono_id),

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
                HarcNotAForkMismatch(join_id) => {
                    write!(f, "Expected fork, but harc is a join {}", join_id.with(ctx))
                }
                HarcNotAJoinMismatch(fork_id) => {
                    write!(f, "Expected join, but harc is a fork {}", fork_id.with(ctx))
                }
                FiringNodeMissing(face, node_id) => write!(
                    f,
                    "Missing {} node \"{}\" in firing component",
                    if face == Face::Tx { "sending" } else { "receiving" },
                    node_id.with(ctx),
                ),
                FiringNodeDuplicated(face, node_id) => write!(
                    f,
                    "Duplicated {} node \"{}\" in firing component",
                    if face == Face::Tx { "sending" } else { "receiving" },
                    node_id.with(ctx),
                ),
                LeakedInhibitor(node_id, tokens_before) => write!(
                    f,
                    "Leaked inhibitor at node \"{}\" firing from {} tokens",
                    node_id.with(ctx),
                    tokens_before
                ),
                StateUnderflow(node_id, tokens_before, num_tokens) => write!(
                    f,
                    "State underflow at node \"{}\" after subtracting {} from {}",
                    node_id.with(ctx),
                    num_tokens,
                    tokens_before
                ),
                StateOverflow(node_id, tokens_before, num_tokens) => write!(
                    f,
                    "State overflow at node \"{}\" after adding {} to {}",
                    node_id.with(ctx),
                    num_tokens,
                    tokens_before
                ),
                CapacityOverflow(node_id, capacity, num_tokens) => write!(
                    f,
                    "Capacity overflow at node \"{}\" set to {} > {}",
                    node_id.with(ctx),
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
