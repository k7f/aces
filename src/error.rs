use std::{fmt, error::Error};

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

    NodeMissingForSource,
    NodeMissingForSink,

    CESIsIncoherent(String),
}

impl fmt::Display for AcesError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use AcesError::*;

        match self {
            CESIsIncoherent(name) => write!(f, "Structure '{}' is incoherent", &name),
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
            SpecShortPolyWithWords => "Multi-word node name is invalid in short polynomial specification",
            SpecMonoInvalid => "Invalid monomial in polynomial specification",
            SpecLinkInvalid => "Invalid link in polynomial specification",
            SpecLinkReversed => "Reversed link in polynomial specification",
            SpecLinkList => "Link list is invalid in polynomial specification",

            NodeMissingForSource => "Missing node for source",
            NodeMissingForSink => "Missing node for sink",

            CESIsIncoherent(_) => "Incoherent CES",
        }
    }
}
