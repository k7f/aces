use std::{fmt, error::Error};
use crate::node;

#[derive(Debug, Clone)]
pub enum AcesError {
    ContextMismatch,
    PortMismatch,
    NodeMissingForID,
    PortMissingForID,
    LinkMissingForID,
    AtomicsNotOrdered,

    NodeMissingForPort(node::Face),
    NodeMissingForLink(node::Face),
    CESIsIncoherent(String),
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
            CESIsIncoherent(name) => write!(f, "Structure '{}' is incoherent", name),
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
            AtomicsNotOrdered => "Atomics have to be given in strictly increasing order",

            NodeMissingForPort(_) => "Missing node for port",
            NodeMissingForLink(_) => "Missing node for link",
            CESIsIncoherent(_) => "Incoherent CES",
        }
    }
}
