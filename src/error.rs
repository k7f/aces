use std::{fmt, error::Error};
use crate::{
    ContextHandle, Contextual, Face, AtomID, NodeID, PortID, LinkID, ForkID, JoinID, Multiplicity,
};

#[derive(Clone, Debug)]
pub enum AcesErrorKind {
    ContextMismatch,
    PolynomialFaceMismatch,
    HarcNotAForkMismatch(JoinID),
    HarcNotAJoinMismatch(ForkID),
    NodeMissingForID(NodeID),
    AtomMissingForID(AtomID),
    PortMissingForID(PortID),
    LinkMissingForID(LinkID),
    HarcMissingForID(AtomID),
    ForkMissingForID(ForkID),
    JoinMissingForID(JoinID),
    BottomAtomAccess,
    AtomicsNotOrdered,

    NodeMissingForPort(Face),
    NodeMissingForLink(Face),
    NodeMissingForFork(Face),
    NodeMissingForJoin(Face),
    FiringNodeMissing(Face),
    FiringNodeDuplicated(Face),
    IncoherentStructure(String, u32, (Face, String, String)),

    LeakedInhibitor(NodeID, Multiplicity),
    StateUnderflow(NodeID, Multiplicity, Multiplicity),
    StateOverflow(NodeID, Multiplicity, Multiplicity),

    EmptyClauseRejectedByFormula(String),
    EmptyClauseRejectedBySolver(String),
    EmptyCausesOfInternalNode(String),
    EmptyEffectsOfInternalNode(String),

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
            NodeMissingForID(node_id) => write!(f, "There is no node with {:?}", node_id),
            AtomMissingForID(atom_id) => write!(f, "There is no atom with {:?}", atom_id),
            PortMissingForID(port_id) => write!(f, "There is no port with {:?}", port_id),
            LinkMissingForID(link_id) => write!(f, "There is no link with {:?}", link_id),
            HarcMissingForID(harc_id) => write!(f, "There is no harc with {:?}", harc_id),
            ForkMissingForID(fork_id) => write!(f, "There is no fork with {:?}", fork_id),
            JoinMissingForID(join_id) => write!(f, "There is no join with {:?}", join_id),
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
            FiringNodeMissing(face) => write!(
                f,
                "Missing {} node in firing component",
                if *face == Face::Tx { "sending" } else { "receiving" }
            ),
            FiringNodeDuplicated(face) => write!(
                f,
                "Duplicated {} node in firing component",
                if *face == Face::Tx { "sending" } else { "receiving" }
            ),
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

#[derive(Clone, Debug)]
pub struct AcesError {
    context: Option<ContextHandle>,
    kind:    AcesErrorKind,
}

impl From<AcesErrorKind> for AcesError {
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

                ref kind => kind.fmt(f),
            }
        } else {
            self.kind.fmt(f)
        }
    }
}

impl Error for AcesError {}
