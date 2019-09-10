use std::{fmt, error::Error};
use crate::{node, LinkID};

#[derive(Debug, Clone)]
pub enum AcesError {
    ContextMismatch,
    PortMismatch,
    NodeMissingForID,
    AtomMissingForID,
    PortMissingForID,
    LinkMissingForID(LinkID),
    SplitMissingForID,
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

    EmptyClauseRejectedByFormula(String),
    EmptyClauseRejectedBySolver(String),
    EmptyCausesOfInternalNode(String),
    EmptyEffectsOfInternalNode(String),

    UnlistedAtomicInMonomial,
    IncoherencyLeak,
    NoModelToInhibit,
}

impl fmt::Display for AcesError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AcesError::*;

        match self {
            LinkMissingForID(link_id) => {
                write!(f, "There is no link with ID {:?}", link_id)
            }
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
            AtomMissingForID => "Atom is missing for ID",
            PortMissingForID => "Port is missing for ID",
            LinkMissingForID(_) => "Link is missing for ID",
            SplitMissingForID => "Split is missing for ID",
            ForkMissingForID => "Fork is missing for ID",
            JoinMissingForID => "Join is missing for ID",
            BottomAtomAccess => "Attempt to access the bottom atom",
            AtomicsNotOrdered => "Atomics have to be given in strictly increasing order",

            NodeMissingForPort(_) => "Missing node for port",
            NodeMissingForLink(_) => "Missing node for link",
            NodeMissingForFork(_) => "Missing node for fork",
            NodeMissingForJoin(_) => "Missing node for join",
            FiringNodeMissing(_) => "Missing node in firing component",
            FiringNodeDuplicated(_) => "Duplicated node in firing component",
            IncoherentStructure(_) => "Incoherent c-e structure",

            EmptyClauseRejectedByFormula(_) => "Empty clause rejected by formula",
            EmptyClauseRejectedBySolver(_) => "Empty clause rejected by solver",
            EmptyCausesOfInternalNode(_) => "Empty cause polynomial of internal node",
            EmptyEffectsOfInternalNode(_) => "Empty effect polynomial of internal node",

            UnlistedAtomicInMonomial => "Monomial contains an unlisted atomic",
            IncoherencyLeak => "Unexpected incoherence of a c-e structure",
            NoModelToInhibit => "Attempt to inhibit a nonexistent model",
        }
    }
}
