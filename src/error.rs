use std::{fmt, error::Error};
use crate::node;

#[derive(Debug, Clone)]
pub enum AcesError {
    ContextMismatch,
    PortMismatch,
    NodeMissingForID,
    PortMissingForID,
    LinkMissingForID,
    ForkMissingForID,
    JoinMissingForID,
    AtomicsNotOrdered,

    NodeMissingForPort(node::Face),
    NodeMissingForLink(node::Face),
    NodeMissingForFork(node::Face),
    NodeMissingForJoin(node::Face),
    FiringNodeMissing(node::Face),
    FiringNodeDuplicated(node::Face),
    IncoherentStructure(String),

    EmptyCausesOfInternalNode(String),
    EmptyEffectsOfInternalNode(String),

    UnlistedAtomicInMonomial,
    IncoherencyLeak,
}

impl fmt::Display for AcesError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AcesError::*;

        match self {
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
            EmptyCausesOfInternalNode(name) => {
                write!(f, "Empty cause polynomial of internal node '{}'", name)
            }
            EmptyEffectsOfInternalNode(name) => {
                write!(f, "Empty effect polynomial of internal node '{}'", name)
            }
            _ => write!(f, "{}", self.description()),
        }
    }
}

impl Error for AcesError {
    fn description(&self) -> &str {
        use AcesError::*;

        match self {
            ContextMismatch => "Context mismatch",
            PortMismatch => "Port mismatch",
            NodeMissingForID => "Node is missing for ID",
            PortMissingForID => "Port is missing for ID",
            LinkMissingForID => "Link is missing for ID",
            ForkMissingForID => "Fork is missing for ID",
            JoinMissingForID => "Join is missing for ID",
            AtomicsNotOrdered => "Atomics have to be given in strictly increasing order",

            NodeMissingForPort(_) => "Missing node for port",
            NodeMissingForLink(_) => "Missing node for link",
            NodeMissingForFork(_) => "Missing node for fork",
            NodeMissingForJoin(_) => "Missing node for join",
            FiringNodeMissing(_) => "Missing node in firing component",
            FiringNodeDuplicated(_) => "Duplicated node in firing component",
            IncoherentStructure(_) => "Incoherent c-e structure",

            EmptyCausesOfInternalNode(_) => "Empty cause polynomial of internal node",
            EmptyEffectsOfInternalNode(_) => "Empty effect polynomial of internal node",

            UnlistedAtomicInMonomial => "Monomial contains an unlisted atomic",
            IncoherencyLeak => "Unexpected incoherence of a c-e structure",
        }
    }
}
