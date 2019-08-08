use std::{
    path::{Path, PathBuf},
    fmt::{self, Debug},
    error::Error,
};
use crate::{NodeID, ContextHandle, yaml_script::YamlContent};

#[derive(Debug, Clone)]
pub(crate) enum ContentError {
    OriginMismatch(ContentOrigin),
}

impl fmt::Display for ContentError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.description())
    }
}

impl Error for ContentError {
    fn description(&self) -> &str {
        use ContentError::*;

        match self {
            OriginMismatch(_) => "Content origin mismatch",
        }
    }
}

#[derive(Clone, Debug)]
pub enum ContentOrigin {
    Interactive,
    CesScript(Option<PathBuf>),
    CexScript(Option<PathBuf>),
}

impl ContentOrigin {
    pub fn interactive() -> Self {
        ContentOrigin::Interactive
    }

    pub fn ces_stream() -> Self {
        ContentOrigin::CesScript(None)
    }

    pub fn cex_stream() -> Self {
        ContentOrigin::CexScript(None)
    }

    pub fn ces_script<P: AsRef<Path>>(path: P) -> Self {
        let path = path.as_ref().to_path_buf();

        ContentOrigin::CesScript(Some(path))
    }

    pub fn cex_script<P: AsRef<Path>>(path: P) -> Self {
        let path = path.as_ref().to_path_buf();

        ContentOrigin::CexScript(Some(path))
    }
}

// FIXME define specific iterators for return types below
/// An abstraction over script formats: various ways c-e structures
/// are described in text.
///
/// This trait is implemented by intermediate representation types,
/// [`YamlContent`] and `ascesis::*`.
///
/// Note: types implementing `Content` trait shouldn't own
/// [`ContextHandle`]s, because `Content` trait objects are owned by
/// [`CES`] structs, along with [`ContextHandle`]s themselves.
///
/// [`CES`]: crate::CES
/// [`ContextHandle`]: crate::ContextHandle
/// [`YamlContent`]: crate::yaml_script::YamlContent
pub trait Content: Debug {
    /// `Script` is a content description in text, for example,
    /// YAML-formatted string or _Ascesis_ source.
    fn get_script(&self) -> Option<&str>;
    fn get_name(&self) -> Option<&str>;
    fn get_carrier_ids(&self) -> Vec<NodeID>;
    fn get_causes_by_id(&self, id: NodeID) -> Option<&Vec<Vec<NodeID>>>;
    fn get_effects_by_id(&self, id: NodeID) -> Option<&Vec<Vec<NodeID>>>;
}

impl Content for String {
    fn get_script(&self) -> Option<&str> {
        Some(self)
    }

    fn get_name(&self) -> Option<&str> {
        panic!("Attempt to access a phantom content.")
    }

    fn get_carrier_ids(&self) -> Vec<NodeID> {
        panic!("Attempt to access a phantom content.")
    }

    fn get_causes_by_id(&self, _id: NodeID) -> Option<&Vec<Vec<NodeID>>> {
        panic!("Attempt to access a phantom content.")
    }

    fn get_effects_by_id(&self, _id: NodeID) -> Option<&Vec<Vec<NodeID>>> {
        panic!("Attempt to access a phantom content.")
    }
}

/// Parses a string description of a c-e structure.
///
/// Returns a [`Content`] trait object if parsing was successful,
/// where a concrete type depends on a content description format.
///
/// Errors depend on a content description format.
pub(crate) fn content_from_str<S: AsRef<str>>(
    ctx: &ContextHandle,
    script: S,
) -> Result<Box<dyn Content>, Box<dyn Error>> {
    {
        // FIXME find a better way of releasing the lock, than wrapping in a block...
        let ref origin = ctx.lock().unwrap().origin;
        match origin {
            ContentOrigin::CexScript(_) => {}
            _ => return Err(Box::new(ContentError::OriginMismatch(origin.clone()))),
        }
    }

    // FIXME infer the format of content description: yaml, sexpr, ...
    // FIXME or better still, deal with formats as plugins and delegate the inference.
    Ok(Box::new(YamlContent::from_str(ctx, script)?))
}
