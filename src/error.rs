use std::{fmt, error::Error};
use crate::{node, LinkID};

#[derive(Debug, Clone)]
pub enum AcesError {
    ContextMismatch,
    PortMismatch,
    HarcMismatch,
    NodeMissingForID,
    AtomMissingForID,
    PortMissingForID,
    LinkMissingForID(LinkID),
    HarcMissingForID,
    ForkMissingForID,
    JoinMissingForID,
    BottomAtomAccess,
    AtomicsNotOrdered,

    NodeMissingForPort(node::Face),
    NodeMissingForLink(node::Face),
    NodeMissingForFork(node::Face),
    NodeMissingForJoin(node::Face),
    FiringNodeMissing(node::Face),
    FiringNodeDuplicated(node::Face),
    IncoherentStructure(String),

    LeakedInhibitor,
    StateUnderflow,
    StateOverflow,

    EmptyClauseRejectedByFormula(String),
    EmptyClauseRejectedBySolver(String),
    EmptyCausesOfInternalNode(String),
    EmptyEffectsOfInternalNode(String),

    UnlistedAtomicInMonomial,
    IncoherencyLeak,
    NoModelToInhibit,

    UnknownScriptFormat,
}

impl fmt::Display for AcesError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AcesError::*;

        match self {
            ContextMismatch => write!(f, "Context mismatch"),
            PortMismatch => write!(f, "Port (dock) mismatch"),
            HarcMismatch => write!(f, "Harc (fork/join) mismatch"),
            NodeMissingForID => write!(f, "Node is missing for ID"),
            AtomMissingForID => write!(f, "Atom is missing for ID"),
            PortMissingForID => write!(f, "Port is missing for ID"),
            LinkMissingForID(link_id) => write!(f, "There is no link with ID {:?}", link_id),
            HarcMissingForID => write!(f, "Harc is missing for ID"),
            ForkMissingForID => write!(f, "Fork is missing for ID"),
            JoinMissingForID => write!(f, "Join is missing for ID"),
            BottomAtomAccess => write!(f, "Attempt to access the bottom atom"),
            AtomicsNotOrdered => write!(f, "Atomics have to be given in strictly increasing order"),

            NodeMissingForPort(face) => write!(
                f,
                "Missing node for {} port",
                if *face == node::Face::Tx { "sending" } else { "receiving" }
            ),
            NodeMissingForLink(face) => write!(
                f,
                "Missing {} node for link",
                if *face == node::Face::Tx { "sending" } else { "receiving" }
            ),
            NodeMissingForFork(face) => write!(
                f,
                "Missing {} node for fork",
                if *face == node::Face::Tx { "sending" } else { "receiving" }
            ),
            NodeMissingForJoin(face) => write!(
                f,
                "Missing {} node for join",
                if *face == node::Face::Tx { "sending" } else { "receiving" }
            ),
            FiringNodeMissing(face) => write!(
                f,
                "Missing {} node in firing component",
                if *face == node::Face::Tx { "sending" } else { "receiving" }
            ),
            FiringNodeDuplicated(face) => write!(
                f,
                "Duplicated {} node in firing component",
                if *face == node::Face::Tx { "sending" } else { "receiving" }
            ),
            IncoherentStructure(name) => write!(f, "Structure '{}' is incoherent", name),

            LeakedInhibitor => write!(f, "Leaked inhibitor"),
            StateUnderflow => write!(f, "State underflow"),
            StateOverflow => write!(f, "State overflow"),

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

impl Error for AcesError {}
