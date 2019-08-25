use std::{
    ops::{AddAssign, MulAssign},
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
    fmt::{self, Debug},
    error::Error,
};
use crate::{NodeID, ContextHandle, yaml_script::YamlContent};

#[derive(Clone, Debug)]
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
/// [`CEStructure`] structs, along with [`ContextHandle`]s themselves.
///
/// [`CEStructure`]: crate::CEStructure
/// [`ContextHandle`]: crate::ContextHandle
/// [`YamlContent`]: crate::yaml_script::YamlContent
pub trait Content: Debug {
    /// `Script` is a content description in text, for example,
    /// YAML-formatted string or _Ascesis_ source.
    fn get_script(&self) -> Option<&str>;
    fn get_name(&self) -> Option<&str>;
    fn get_carrier_ids(&mut self) -> Vec<NodeID>;
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

    fn get_carrier_ids(&mut self) -> Vec<NodeID> {
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

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Default, Debug)]
pub(crate) struct MonoForContent {
    content: Vec<NodeID>,
}

impl MonoForContent {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn add_node(&mut self, id: NodeID) {
        if let Err(pos) = self.content.binary_search(&id) {
            self.content.insert(pos, id);
        }
    }

    pub(crate) fn into_content(self) -> Vec<NodeID> {
        self.content
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Default, Debug)]
pub(crate) struct PolyForContent {
    content: Vec<Vec<NodeID>>,
}

impl PolyForContent {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn add_mono(&mut self, mono: Vec<NodeID>) {
        if let Err(pos) = self.content.binary_search(&mono) {
            self.content.insert(pos, mono);
        }
    }

    pub(crate) fn multiply_by_mono(&mut self, mono: Vec<NodeID>) {
        if self.content.is_empty() {
            self.add_mono(mono);
        } else {
            let mut old_poly = Vec::new();
            old_poly.append(&mut self.content);

            for mut old_mono in old_poly {
                for node_id in mono.iter() {
                    if let Err(pos) = old_mono.binary_search(node_id) {
                        old_mono.insert(pos, *node_id);
                    }
                }
                self.add_mono(old_mono);
            }
        }
    }

    pub(crate) fn as_content(&self) -> &Vec<Vec<NodeID>> {
        &self.content
    }
}

impl AddAssign for PolyForContent {
    fn add_assign(&mut self, other: Self) {
        for mono in other.content {
            self.add_mono(mono);
        }
    }
}

#[derive(Clone, Default, Debug)]
struct MapForContent {
    content: BTreeMap<NodeID, PolyForContent>,
}

impl MapForContent {
    // `poly.is_empty()` test is ommited here, because it is done by the
    // only callers, `PartialContent::add_to_causes()` and
    // `PartialContent::add_to_effects()`.
    fn add_poly(&mut self, id: NodeID, poly: &[Vec<NodeID>]) {
        self.content
            .entry(id)
            .and_modify(|old_poly| {
                for mono in poly {
                    old_poly.add_mono(mono.to_vec());
                }
            })
            .or_insert_with(|| PolyForContent { content: poly.to_vec() });
    }

    // `poly.is_empty()` test is ommited here, because it is done by the
    // only callers, `PartialContent::multiply_causes()` and
    // `PartialContent::multiply_effects()`.
    fn multiply_by_poly(&mut self, id: NodeID, poly: &[Vec<NodeID>]) {
        self.content
            .entry(id)
            .and_modify(|old_poly| {
                let mut new_poly = PolyForContent::new();
                for mono in poly {
                    let mut aux_poly = old_poly.clone();
                    aux_poly.multiply_by_mono(mono.to_vec());
                    new_poly += aux_poly;
                }
                *old_poly = new_poly;
            })
            .or_insert_with(|| PolyForContent { content: poly.to_vec() });
    }
}

#[derive(Clone, Default, Debug)]
struct CarrierForContent {
    content: Option<BTreeSet<NodeID>>,
}

impl CarrierForContent {
    fn touch(&mut self, id: NodeID) {
        if let Some(ref content) = self.content {
            if content.contains(&id) {
                return
            }
        }
        self.content = None;
    }

    fn maybe_update(&mut self, causes: &MapForContent, effects: &MapForContent) {
        if self.content.is_none() {
            self.content =
                Some(effects.content.keys().chain(causes.content.keys()).copied().collect());
        }
    }

    fn as_owned_content(&self) -> Vec<NodeID> {
        self.content.as_ref().unwrap().iter().copied().collect()
    }
}

#[derive(Clone, Debug)]
pub struct PartialContent {
    context: ContextHandle,
    carrier: CarrierForContent,
    causes:  MapForContent,
    effects: MapForContent,
}

impl PartialContent {
    pub fn new(ctx: &ContextHandle) -> Self {
        PartialContent {
            context: ctx.clone(),
            carrier: Default::default(),
            causes:  Default::default(),
            effects: Default::default(),
        }
    }

    pub fn get_context(&self) -> &ContextHandle {
        &self.context
    }

    pub fn add_to_causes(&mut self, id: NodeID, poly: &[Vec<NodeID>]) {
        if !poly.is_empty() {
            self.causes.add_poly(id, poly);
            self.carrier.touch(id);
        }
    }

    pub fn add_to_effects(&mut self, id: NodeID, poly: &[Vec<NodeID>]) {
        if !poly.is_empty() {
            self.effects.add_poly(id, poly);
            self.carrier.touch(id);
        }
    }

    pub fn multiply_causes(&mut self, id: NodeID, poly: &[Vec<NodeID>]) {
        if !poly.is_empty() {
            self.causes.multiply_by_poly(id, poly);
            self.carrier.touch(id);
        }
    }

    pub fn multiply_effects(&mut self, id: NodeID, poly: &[Vec<NodeID>]) {
        if !poly.is_empty() {
            self.effects.multiply_by_poly(id, poly);
            self.carrier.touch(id);
        }
    }
}

impl AddAssign for PartialContent {
    fn add_assign(&mut self, other: Self) {
        for (id, poly) in other.causes.content {
            self.add_to_causes(id, &poly.content);
        }

        for (id, poly) in other.effects.content {
            self.add_to_effects(id, &poly.content);
        }
    }
}

impl MulAssign for PartialContent {
    fn mul_assign(&mut self, other: Self) {
        for (id, poly) in other.causes.content {
            self.multiply_causes(id, &poly.content);
        }

        for (id, poly) in other.effects.content {
            self.multiply_effects(id, &poly.content);
        }
    }
}

impl Content for PartialContent {
    fn get_script(&self) -> Option<&str> {
        None
    }

    fn get_name(&self) -> Option<&str> {
        None
    }

    fn get_carrier_ids(&mut self) -> Vec<NodeID> {
        self.carrier.maybe_update(&self.causes, &self.effects);
        self.carrier.as_owned_content()
    }

    fn get_causes_by_id(&self, id: NodeID) -> Option<&Vec<Vec<NodeID>>> {
        self.causes.content.get(&id).map(|poly| poly.as_content())
    }

    fn get_effects_by_id(&self, id: NodeID) -> Option<&Vec<Vec<NodeID>>> {
        self.effects.content.get(&id).map(|poly| poly.as_content())
    }
}

pub trait CompilableAsContent {
    fn compile_as_content(&self, ctx: &ContextHandle) -> Result<PartialContent, Box<dyn Error>>;
}
