use std::{fmt::Debug, error::Error};
use crate::{ContextHandle, ID, yaml_script::YamlContent};

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
    fn get_carrier_ids(&self) -> Vec<ID>;
    fn get_causes_by_id(&self, id: ID) -> Option<&Vec<Vec<ID>>>;
    fn get_effects_by_id(&self, id: ID) -> Option<&Vec<Vec<ID>>>;
}

impl Content for String {
    fn get_script(&self) -> Option<&str> {
        Some(self)
    }

    fn get_name(&self) -> Option<&str> {
        panic!("Attempt to access a phantom content.")
    }

    fn get_carrier_ids(&self) -> Vec<ID> {
        panic!("Attempt to access a phantom content.")
    }

    fn get_causes_by_id(&self, _id: ID) -> Option<&Vec<Vec<ID>>> {
        panic!("Attempt to access a phantom content.")
    }

    fn get_effects_by_id(&self, _id: ID) -> Option<&Vec<Vec<ID>>> {
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
    // FIXME infer the format of string description: yaml, sexpr, ...
    Ok(Box::new(YamlContent::from_str(ctx, script)?))
}
