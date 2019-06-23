use std::{fmt, error::Error};
use crate::node;

#[derive(Debug, Clone)]
pub(crate) enum AcesError {
    SpecEmpty,
    SpecMultiple,
    SpecNotADict,
    SpecKeyNotString,
    SpecNameNotString,
    SpecNameDup,
    SpecPolyInvalid,
    SpecPolyAmbiguous,
    SpecShortPolyWithWords,
    SpecMonoInvalid,
    SpecLinkInvalid,
    SpecLinkReversed,
    SpecLinkList,

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
            SpecEmpty => "Empty specification",
            SpecMultiple => "Multiple specifications",
            SpecNotADict => "Bad specification (not a dictionary)",
            SpecKeyNotString => "Non-string key in specification",
            SpecNameNotString => "Non-string CES name in specification",
            SpecNameDup => "Duplicated CES name in specification",
            SpecPolyInvalid => "Invalid polynomial specification",
            SpecPolyAmbiguous => "Ambiguous polynomial specification",
            SpecShortPolyWithWords => {
                "Multi-word node name is invalid in short polynomial specification"
            }
            SpecMonoInvalid => "Invalid monomial in polynomial specification",
            SpecLinkInvalid => "Invalid link in polynomial specification",
            SpecLinkReversed => "Reversed link in polynomial specification",
            SpecLinkList => "Link list is invalid in polynomial specification",

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
