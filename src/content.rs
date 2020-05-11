use std::{
    ops::{AddAssign, MulAssign},
    collections::{BTreeMap, BTreeSet},
    path::Path,
    fmt,
    error::Error,
};
use crate::{NodeId, ContextHandle};

pub trait ContentFormat: fmt::Debug {
    // Note: this can't be declared as an associated const due to
    // object safety constraints.
    fn expected_extensions(&self) -> &[&str];

    fn path_is_acceptable(&self, path: &Path) -> bool {
        if let Some(ext) = path.extension() {
            if !ext.is_empty() {
                if let Some(ext) = ext.to_str() {
                    for expected in self.expected_extensions() {
                        if ext.eq_ignore_ascii_case(expected) {
                            return true
                        }
                    }
                }

                // Reject unexpected nonempty extension, including any
                // extension that isn't a valid UTF8.
                return false
            }
        }

        // Accept empty extension.
        true
    }

    fn script_is_acceptable(&self, script: &str) -> bool;

    fn script_to_content(
        &self,
        cxt: &ContextHandle,
        script: &str,
    ) -> Result<Box<dyn Content>, Box<dyn Error>>;
}

#[derive(Clone, Default, Debug)]
pub struct InteractiveFormat;

impl InteractiveFormat {
    pub fn new() -> Self {
        Default::default()
    }
}

impl ContentFormat for InteractiveFormat {
    fn expected_extensions(&self) -> &[&str] {
        &[]
    }

    fn script_is_acceptable(&self, _script: &str) -> bool {
        false
    }

    fn script_to_content(
        &self,
        _cxt: &ContextHandle,
        _script: &str,
    ) -> Result<Box<dyn Content>, Box<dyn Error>> {
        panic!("Attempt to bypass a script acceptance test.")
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
/// [`YamlContent`]: crate::yaml_script::YamlContent
pub trait Content: fmt::Debug {
    /// `Script` is a content description in text, for example,
    /// YAML-formatted string or _Ascesis_ source.
    fn get_script(&self) -> Option<&str>;
    fn get_name(&self) -> Option<&str>;
    fn get_carrier_ids(&mut self) -> Vec<NodeId>;
    fn get_causes_by_id(&self, id: NodeId) -> Option<&Vec<Vec<NodeId>>>;
    fn get_effects_by_id(&self, id: NodeId) -> Option<&Vec<Vec<NodeId>>>;
}

impl Content for String {
    fn get_script(&self) -> Option<&str> {
        Some(self)
    }

    fn get_name(&self) -> Option<&str> {
        panic!("Attempt to access a phantom content.")
    }

    fn get_carrier_ids(&mut self) -> Vec<NodeId> {
        panic!("Attempt to access a phantom content.")
    }

    fn get_causes_by_id(&self, _id: NodeId) -> Option<&Vec<Vec<NodeId>>> {
        panic!("Attempt to access a phantom content.")
    }

    fn get_effects_by_id(&self, _id: NodeId) -> Option<&Vec<Vec<NodeId>>> {
        panic!("Attempt to access a phantom content.")
    }
}

impl<'a, C: Content + 'a> From<C> for Box<dyn Content + 'a> {
    fn from(content: C) -> Box<dyn Content + 'a> {
        Box::new(content)
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Default, Debug)]
pub(crate) struct MonoForContent {
    content: Vec<NodeId>,
}

impl MonoForContent {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn add_node(&mut self, id: NodeId) {
        if let Err(pos) = self.content.binary_search(&id) {
            self.content.insert(pos, id);
        }
    }

    pub(crate) fn into_content(self) -> Vec<NodeId> {
        self.content
    }
}

#[derive(PartialOrd, Ord, PartialEq, Eq, Clone, Default, Debug)]
pub(crate) struct PolyForContent {
    content: Vec<Vec<NodeId>>,
}

impl PolyForContent {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn add_mono(&mut self, mono: Vec<NodeId>) {
        if let Err(pos) = self.content.binary_search(&mono) {
            self.content.insert(pos, mono);
        }
    }

    pub(crate) fn multiply_by_mono(&mut self, mono: Vec<NodeId>) {
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

    pub(crate) fn as_content(&self) -> &Vec<Vec<NodeId>> {
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
    content: BTreeMap<NodeId, PolyForContent>,
}

impl MapForContent {
    // `poly.is_empty()` test is ommited here, because it is done by the
    // only callers, `PartialContent::add_to_causes()` and
    // `PartialContent::add_to_effects()`.
    fn add_poly(&mut self, id: NodeId, poly: &[Vec<NodeId>]) {
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
    fn multiply_by_poly(&mut self, id: NodeId, poly: &[Vec<NodeId>]) {
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
    content: Option<BTreeSet<NodeId>>,
}

impl CarrierForContent {
    fn touch(&mut self, id: NodeId) {
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

    fn as_owned_content(&self) -> Vec<NodeId> {
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

    pub fn add_to_causes(&mut self, id: NodeId, poly: &[Vec<NodeId>]) {
        if !poly.is_empty() {
            self.causes.add_poly(id, poly);
            self.carrier.touch(id);
        }
    }

    pub fn add_to_effects(&mut self, id: NodeId, poly: &[Vec<NodeId>]) {
        if !poly.is_empty() {
            self.effects.add_poly(id, poly);
            self.carrier.touch(id);
        }
    }

    pub fn multiply_causes(&mut self, id: NodeId, poly: &[Vec<NodeId>]) {
        if !poly.is_empty() {
            self.causes.multiply_by_poly(id, poly);
            self.carrier.touch(id);
        }
    }

    pub fn multiply_effects(&mut self, id: NodeId, poly: &[Vec<NodeId>]) {
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

    fn get_carrier_ids(&mut self) -> Vec<NodeId> {
        self.carrier.maybe_update(&self.causes, &self.effects);
        self.carrier.as_owned_content()
    }

    fn get_causes_by_id(&self, id: NodeId) -> Option<&Vec<Vec<NodeId>>> {
        self.causes.content.get(&id).map(|poly| poly.as_content())
    }

    fn get_effects_by_id(&self, id: NodeId) -> Option<&Vec<Vec<NodeId>>> {
        self.effects.content.get(&id).map(|poly| poly.as_content())
    }
}

pub trait CompilableMut {
    fn compile_mut(&mut self, ctx: &ContextHandle) -> Result<bool, Box<dyn Error>>;
}

pub trait Compilable {
    fn compile(&self, ctx: &ContextHandle) -> Result<bool, Box<dyn Error>>;
}

pub trait CompilableAsContent {
    /// Get a compiled [`PartialContent`] of `self`.
    ///
    /// Expected to return a content previously stored in the given
    /// [`Context`], if possible, or to compile `self` and return the
    /// result.  Expected to return error if not all dependencies of
    /// `self` are retrievable from the given [`Context`] as a
    /// [`PartialContent`].
    ///
    /// Note: unlike [`compile_as_dependency()`], this function isn't
    /// expected to store compilation result in the [`Context`].
    ///
    /// [`Context`]: crate::Context
    /// [`compile_as_dependency()`]: CompilableAsDependency::compile_as_dependency()
    fn get_compiled_content(&self, ctx: &ContextHandle) -> Result<PartialContent, Box<dyn Error>>;

    /// Check whether all dependencies of `self` are retrievable from
    /// the given [`Context`].
    ///
    /// Expected to return `None` if none of dependencies is missing
    /// from [`Context`], or a name of a missing dependency, chosen
    /// arbitrarily.
    ///
    /// [`Context`]: crate::Context
    fn check_dependencies(&self, _ctx: &ContextHandle) -> Option<String> {
        None
    }
}

pub trait CompilableAsDependency: CompilableAsContent {
    /// Compile `self` and store the result in the given [`Context`].
    ///
    /// Expected to return `None` if none of dependencies is missing
    /// from [`Context`], or a name of a missing dependency, chosen
    /// arbitrarily.
    ///
    /// [`Context`]: crate::Context
    fn compile_as_dependency(&self, ctx: &ContextHandle) -> Result<Option<String>, Box<dyn Error>>;
}
