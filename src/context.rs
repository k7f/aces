use std::{
    cmp, fmt,
    collections::{btree_map, BTreeMap, BTreeSet},
    iter::FromIterator,
    sync::{Arc, Mutex},
};
use crate::{
    PartialContent, Port, Link, Harc, Fork, Join, Face, NodeId, AtomId, PortId, LinkId, ForkId,
    JoinId, Semantics, Capacity, Weight, FlowWeight, AcesError, AcesErrorKind,
    node::{NodeSet, NodeSetId},
    name::{NameSpace, NameId},
    atom::{AtomSpace, Atom},
    sat, solver, runner, vis,
};

/// A handle to a [`Context`] instance.
///
/// All [`Context`] handles used in _aces_ have type
/// `Arc<Mutex<Context>>`.  They are stored permanently in the
/// following structs: [`CEStructure`], [`sat::Formula`],
/// [`solver::Solver`], and [`solver::Solution`].
///
/// For another way of binding [`Context`] to data see [`Contextual`]
/// trait and [`InContext`] struct.
///
/// [`CEStructure`]: crate::CEStructure
/// [`sat::Formula`]: crate::sat::Formula
/// [`solver::Solver`]: crate::solver::Solver
/// [`solver::Solution`]: crate::solver::Solution
pub type ContextHandle = Arc<Mutex<Context>>;

/// A representation of shared state.
///
/// This is an umbrella type which, currently, includes a collection
/// of [`Atom`]s, two symbol tables (one for structure names, and
/// another for node names), node capacities, hyper weights, flow
/// weights, and [`PartialContent`] of any c-e structure created in
/// this `Context`.
///
/// For usage, see [`ContextHandle`] type, [`Contextual`] trait and
/// [`InContext`] struct.
///
/// [`Atom`]: crate::atom::Atom
#[derive(Debug)]
pub struct Context {
    magic_id:      u64,    // group identifier
    name_id:       NameId, // given name
    globals:       NameSpace,
    nodes:         NameSpace,
    atoms:         AtomSpace,
    content:       BTreeMap<NameId, PartialContent>,
    capacities:    BTreeMap<NodeId, Capacity>,
    hyper_weights: BTreeMap<AtomId, Weight>,
    flow_weights:  BTreeMap<NodeId, Vec<FlowWeight>>,
    solver_props:  solver::Props,
    runner_props:  runner::Props,
    vis_props:     vis::Props,
}

impl Context {
    /// Creates a new toplevel `Context` instance and returns the
    /// corresponding [`ContextHandle`].
    ///
    /// Calling this method is the only public way of creating
    /// toplevel `Context` instances.
    pub fn new_toplevel<S: AsRef<str>>(name: S) -> ContextHandle {
        let magic_id = rand::random();

        let mut globals = NameSpace::default();
        let name_id = globals.share_name(name);

        let ctx = Self {
            magic_id,
            name_id,
            globals,
            nodes: Default::default(),
            atoms: Default::default(),
            content: Default::default(),
            capacities: Default::default(),
            hyper_weights: Default::default(),
            flow_weights: Default::default(),
            solver_props: Default::default(),
            runner_props: Default::default(),
            vis_props: Default::default(),
        };

        Arc::new(Mutex::new(ctx))
    }

    /// Clears content map, capacities, weights and runtime
    /// attributes, but doesn't destroy shared resources.
    ///
    /// This method preserves the collection of [`Atom`]s, the two
    /// symbol tables, and the `Context`'s own given name and group
    /// identifier.
    pub fn reset(&mut self) {
        // Fields unchanged: magic_id, name_id, globals, nodes, atoms.
        self.content.clear();
        self.capacities.clear();
        self.hyper_weights.clear();
        self.flow_weights.clear();
        self.solver_props.clear();
        self.runner_props.clear();
        self.vis_props.clear();
    }

    /// Creates a new derived `Context` instance and returns a
    /// corresponding [`ContextHandle`].
    ///
    /// Calling this method is the only public way of creating derived
    /// `Context` instances.
    pub fn new_derived<S: AsRef<str>>(name: S, parent: &ContextHandle) -> ContextHandle {
        let ctx = {
            let mut parent = parent.lock().unwrap();

            let magic_id = parent.magic_id;

            // NameId of child name in parent and child is the same
            // (but not in further ancestors).  See `partial_cmp`.
            let name_id = parent.globals.share_name(name);

            let globals = parent.globals.clone();
            let nodes = parent.nodes.clone();
            let atoms = parent.atoms.clone();
            let content = parent.content.clone();
            let capacities = parent.capacities.clone();
            let hyper_weights = parent.hyper_weights.clone();
            let flow_weights = parent.flow_weights.clone();
            let solver_props = parent.solver_props.clone();
            let runner_props = parent.runner_props.clone();
            let vis_props = parent.vis_props.clone();

            Self {
                magic_id,
                name_id,
                globals,
                nodes,
                atoms,
                content,
                capacities,
                hyper_weights,
                flow_weights,
                solver_props,
                runner_props,
                vis_props,
            }
        };

        Arc::new(Mutex::new(ctx))
    }

    pub fn is_toplevel(&self) -> bool {
        self.name_id == unsafe { NameId::new_unchecked(1) }
    }

    pub fn is_derived(&self) -> bool {
        self.name_id > unsafe { NameId::new_unchecked(1) }
    }

    pub fn get_name(&self) -> &str {
        self.globals.get_name(self.name_id).expect("Invalid context.")
    }

    // Nodes

    // FIXME support node iteration (define a generic NameSpace iterator)

    #[inline]
    pub fn share_node_name<S: AsRef<str>>(&mut self, node_name: S) -> NodeId {
        NodeId(self.nodes.share_name(node_name))
    }

    #[inline]
    pub fn get_node_name(&self, node_id: NodeId) -> Option<&str> {
        self.nodes.get_name(node_id.get())
    }

    #[inline]
    pub fn get_node_id<S: AsRef<str>>(&self, node_name: S) -> Option<NodeId> {
        self.nodes.get_id(node_name).map(NodeId)
    }

    // Atoms

    #[inline]
    pub(crate) fn get_atom(&self, atom_id: AtomId) -> Option<&Atom> {
        self.atoms.get_atom(atom_id)
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_atom_id(&self, atom: &Atom) -> Option<AtomId> {
        self.atoms.get_atom_id(atom)
    }

    #[inline]
    pub fn is_port(&self, atom_id: AtomId) -> bool {
        self.atoms.is_port(atom_id)
    }

    #[inline]
    pub fn is_link(&self, atom_id: AtomId) -> bool {
        self.atoms.is_link(atom_id)
    }

    #[inline]
    pub fn is_harc(&self, atom_id: AtomId) -> bool {
        self.atoms.is_harc(atom_id)
    }

    #[inline]
    pub fn is_fork(&self, atom_id: AtomId) -> bool {
        self.atoms.is_fork(atom_id)
    }

    #[inline]
    pub fn is_join(&self, atom_id: AtomId) -> bool {
        self.atoms.is_join(atom_id)
    }

    #[inline]
    pub fn share_port(&mut self, port: &mut Port) -> PortId {
        self.atoms.share_port(port)
    }

    #[inline]
    pub fn share_link(&mut self, link: &mut Link) -> LinkId {
        self.atoms.share_link(link)
    }

    #[inline]
    pub fn share_fork(&mut self, fork: &mut Fork) -> ForkId {
        self.atoms.share_fork(fork)
    }

    #[inline]
    pub fn share_join(&mut self, join: &mut Join) -> JoinId {
        self.atoms.share_join(join)
    }

    #[inline]
    pub(crate) fn share_fork_from_host_and_suit(
        &mut self,
        host_id: NodeId,
        suit: NodeSet,
    ) -> ForkId {
        self.atoms.share_fork_from_host_and_suit(host_id, suit)
    }

    #[inline]
    pub(crate) fn share_join_from_host_and_suit(
        &mut self,
        host_id: NodeId,
        suit: NodeSet,
    ) -> JoinId {
        self.atoms.share_join_from_host_and_suit(host_id, suit)
    }

    #[inline]
    pub fn share_node_set(&mut self, mono: &mut NodeSet) -> NodeSetId {
        self.atoms.share_node_set(mono)
    }

    #[inline]
    pub fn get_port(&self, port_id: PortId) -> Option<&Port> {
        self.atoms.get_port(port_id)
    }

    #[inline]
    pub fn get_link(&self, link_id: LinkId) -> Option<&Link> {
        self.atoms.get_link(link_id)
    }

    #[inline]
    pub fn get_harc(&self, atom_id: AtomId) -> Option<&Harc> {
        self.atoms.get_harc(atom_id)
    }

    #[inline]
    pub fn get_fork(&self, fork_id: ForkId) -> Option<&Fork> {
        self.atoms.get_fork(fork_id)
    }

    #[inline]
    pub fn get_join(&self, join_id: JoinId) -> Option<&Join> {
        self.atoms.get_join(join_id)
    }

    #[inline]
    pub fn get_node_set(&self, mono_id: NodeSetId) -> Option<&NodeSet> {
        self.atoms.get_node_set(mono_id)
    }

    #[inline]
    pub fn get_anti_port_id(&self, port_id: PortId) -> Option<PortId> {
        self.atoms.get_anti_port_id(port_id)
    }

    // Content

    pub fn add_content<S: AsRef<str>>(
        &mut self,
        name: S,
        content: PartialContent,
    ) -> Option<PartialContent> {
        let name_id = self.globals.share_name(name);

        self.content.insert(name_id, content)
    }

    pub fn get_content<S: AsRef<str>>(&self, name: S) -> Option<&PartialContent> {
        self.globals.get_id(name).and_then(|id| self.content.get(&id))
    }

    pub fn has_content<S: AsRef<str>>(&self, name: S) -> bool {
        self.globals.get_id(name).map_or(false, |id| self.content.contains_key(&id))
    }

    // Capacities

    pub fn set_capacity_by_name<S: AsRef<str>>(
        &mut self,
        node_name: S,
        cap: Capacity,
    ) -> Option<Capacity> {
        let node_id = self.share_node_name(node_name.as_ref());

        self.capacities.insert(node_id, cap)
    }

    #[inline]
    pub fn get_capacity(&self, node_id: NodeId) -> Capacity {
        self.capacities.get(&node_id).copied().unwrap_or_else(Capacity::one)
    }

    // Weights

    pub fn set_hyper_weight_by_names<S, I>(
        &mut self,
        face: Face,
        host_name: S,
        suit_names: I,
        weight: Weight,
    ) -> Option<Weight>
    where
        S: AsRef<str>,
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        let host_id = self.share_node_name(host_name.as_ref());
        let suit_ids = suit_names.into_iter().map(|n| self.share_node_name(n.as_ref()));
        let suit = NodeSet::new(suit_ids);

        let atom_id = match face {
            Face::Tx => self.share_fork_from_host_and_suit(host_id, suit).get(),
            Face::Rx => self.share_join_from_host_and_suit(host_id, suit).get(),
        };

        self.hyper_weights.insert(atom_id, weight)
    }

    /// Attach a given `weight` to a node specified by `host_name` and
    /// a transition specified by `pre_set_names` and
    /// `post_set_names`.
    ///
    /// On success, return either the given weight, if it has already
    /// been attached to the same node and transition, or the lowest
    /// weight that has already been attached to the same node and
    /// transition, or `None` if no weight has been attached so far.
    pub fn set_flow_weight_by_names<S, I, J>(
        &mut self,
        face: Face,
        host_name: S,
        pre_set_names: I,
        post_set_names: J,
        weight: Weight,
    ) -> Result<Option<Weight>, AcesError>
    where
        S: AsRef<str>,
        I: IntoIterator,
        I::Item: AsRef<str>,
        J: IntoIterator,
        J::Item: AsRef<str>,
    {
        let host_id = self.share_node_name(host_name.as_ref());
        let pre_set = BTreeSet::from_iter(
            pre_set_names.into_iter().map(|n| self.share_node_name(n.as_ref())),
        );
        let post_set = BTreeSet::from_iter(
            post_set_names.into_iter().map(|n| self.share_node_name(n.as_ref())),
        );

        if match face {
            Face::Tx => &pre_set,
            Face::Rx => &post_set,
        }
        .contains(&host_id)
        {
            if pre_set.is_disjoint(&post_set) {
                let mut pre_set = NodeSet::new_unchecked(pre_set);
                let pre_set_id = self.share_node_set(&mut pre_set);
                let mut post_set = NodeSet::new_unchecked(post_set);
                let post_set_id = self.share_node_set(&mut post_set);

                match self.flow_weights.entry(host_id) {
                    btree_map::Entry::Occupied(entry) => {
                        let weights = entry.into_mut();
                        let new_weight = FlowWeight::new(pre_set_id, post_set_id, weight);

                        match weights.binary_search(&new_weight) {
                            Ok(_) => Ok(Some(weight)),
                            Err(pos) => {
                                weights.insert(pos, new_weight);

                                if pos > 0 {
                                    if let Some(old_weight) = weights.get(pos - 1) {
                                        if old_weight.pre_set == pre_set_id
                                            && old_weight.post_set == post_set_id
                                        {
                                            return Ok(Some(old_weight.weight))
                                        }
                                    }
                                }

                                if let Some(old_weight) = weights.get(pos + 1) {
                                    if old_weight.pre_set == pre_set_id
                                        && old_weight.post_set == post_set_id
                                    {
                                        return Ok(Some(old_weight.weight))
                                    }
                                }

                                Ok(None)
                            }
                        }
                    }
                    btree_map::Entry::Vacant(entry) => {
                        let new_weight = FlowWeight::new(pre_set_id, post_set_id, weight);

                        entry.insert(vec![new_weight]);

                        Ok(None)
                    }
                }
            } else {
                Err(AcesErrorKind::FiringOverlap.into())
            }
        } else {
            Err(AcesErrorKind::FiringNodeMissing(face, host_id).into())
        }
    }

    #[inline]
    pub fn set_hyper_inhibitor_by_names<S, I>(
        &mut self,
        face: Face,
        host_name: S,
        suit_names: I,
    ) -> Option<Weight>
    where
        S: AsRef<str>,
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        self.set_hyper_weight_by_names(face, host_name, suit_names, Weight::omega())
    }

    #[inline]
    pub fn set_hyper_holder_by_names<S, I>(
        &mut self,
        face: Face,
        host_name: S,
        suit_names: I,
    ) -> Option<Weight>
    where
        S: AsRef<str>,
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        self.set_hyper_weight_by_names(face, host_name, suit_names, Weight::zero())
    }

    pub fn get_weight(
        &mut self,
        atom_id: AtomId,
        pre_set_id: NodeSetId,
        post_set_id: NodeSetId,
    ) -> Result<Weight, AcesError> {
        if let Some(weight) = self.hyper_weights.get(&atom_id) {
            Ok(*weight)
        } else if let Some(harc) = self.get_harc(atom_id) {
            let node_id = harc.get_host_id();
            let mut test_weight = FlowWeight::new(pre_set_id, post_set_id, Weight::zero());

            if let Some(weights) = self.flow_weights.get(&node_id) {
                match weights.binary_search(&test_weight) {
                    Ok(_) => Ok(test_weight.weight),
                    Err(pos) => {
                        if let Some(flow_weight) = weights.get(pos) {
                            test_weight.weight = flow_weight.weight;
                            if test_weight == *flow_weight {
                                Ok(test_weight.weight)
                            } else {
                                Ok(Weight::one())
                            }
                        } else {
                            Ok(Weight::one())
                        }
                    }
                }
            } else {
                Ok(Weight::one())
            }
        } else {
            Err(AcesErrorKind::HarcMissingForId(atom_id).into())
        }
    }

    // Solver props

    pub fn set_encoding(&mut self, encoding: sat::Encoding) {
        self.solver_props.sat_encoding = Some(encoding);
    }

    pub fn get_encoding(&self) -> Option<sat::Encoding> {
        self.solver_props.sat_encoding
    }

    pub fn set_search(&mut self, search: sat::Search) {
        self.solver_props.sat_search = Some(search);
    }

    pub fn get_search(&self) -> Option<sat::Search> {
        self.solver_props.sat_search
    }

    // Runner props

    pub fn set_semantics(&mut self, semantics: Semantics) {
        self.runner_props.semantics = Some(semantics);
    }

    pub fn get_semantics(&self) -> Option<Semantics> {
        self.runner_props.semantics
    }

    pub fn set_max_steps(&mut self, max_steps: usize) {
        self.runner_props.max_steps = Some(max_steps);
    }

    pub fn get_max_steps(&self) -> Option<usize> {
        self.runner_props.max_steps
    }

    pub fn set_num_passes(&mut self, num_passes: usize) {
        self.runner_props.num_passes = Some(num_passes);
    }

    pub fn get_num_passes(&self) -> Option<usize> {
        self.runner_props.num_passes
    }

    // Vis props

    pub fn set_title<S: AsRef<str>>(&mut self, title: S) {
        self.vis_props.title = Some(title.as_ref().to_owned());
    }

    pub fn get_title(&self) -> Option<&str> {
        self.vis_props.title.as_deref()
    }

    pub fn set_label<S: AsRef<str>>(&mut self, node_id: NodeId, label: S) {
        self.vis_props.labels.insert(node_id, label.as_ref().to_owned());
    }

    pub fn get_label(&self, node_id: NodeId) -> Option<&str> {
        self.vis_props.labels.get(&node_id).map(|t| t.as_str())
    }
}

impl PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        self.magic_id == other.magic_id && self.name_id == other.name_id
    }
}

impl Eq for Context {}

// Ancestors are _greater_ than their offspring.
impl PartialOrd for Context {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if self.magic_id == other.magic_id {
            if other.globals.get_id(self.get_name()) == Some(self.name_id) {
                // Since NameId of self's name in self and other is
                // the same, self is either an ancestor of other, or a
                // direct child of other, or equals the other.
                Some(other.name_id.cmp(&self.name_id))
            } else if self.globals.get_id(other.get_name()) == Some(other.name_id) {
                // NameId of other's name in self and other is the
                // same...
                match other.name_id.cmp(&self.name_id) {
                    // ...other is an ancestor of self,
                    cmp::Ordering::Less => Some(cmp::Ordering::Less),
                    // ...self is the parent of other: this case
                    // should have already been covered,
                    cmp::Ordering::Greater => unreachable!(),
                    // ...they are equal: this case should have
                    // already been covered.
                    cmp::Ordering::Equal => unreachable!(),
                }
            } else if self.name_id == other.name_id {
                // This case should have already been covered.
                unreachable!()
            } else {
                None
            }
        } else {
            None
        }
    }
}

/// A trait for binding objects to [`Context`] temporarily, without
/// permanently storing (and synchronizing) context references inside
/// the objects.
///
/// See [`InContext`] for more details.
pub trait Contextual: Sized {
    fn format(&self, ctx: &ContextHandle) -> Result<String, AcesError>;

    #[inline]
    fn with(&self, ctx: &ContextHandle) -> InContext<Self> {
        InContext { context: ctx.clone(), thing: self }
    }

    #[inline]
    fn with_mut(&mut self, ctx: &ContextHandle) -> InContextMut<Self> {
        InContextMut { context: ctx.clone(), thing: self }
    }
}

/// A version of the [`Contextual`] trait to be used for fine-grained
/// access to [`Context`].
pub trait ExclusivelyContextual: Sized {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError>;
}

impl<T: ExclusivelyContextual> Contextual for T {
    #[inline]
    fn format(&self, ctx: &ContextHandle) -> Result<String, AcesError> {
        self.format_locked(&ctx.lock().unwrap())
    }
}

impl<T: ExclusivelyContextual> Contextual for Vec<T> {
    fn format(&self, ctx: &ContextHandle) -> Result<String, AcesError> {
        let mut elts = self.iter();

        if let Some(elt) = elts.next() {
            let ctx = ctx.lock().unwrap();
            let mut elts_repr = elt.format_locked(&ctx)?;

            for elt in elts {
                elts_repr.push_str(&format!(", {}", elt.format_locked(&ctx)?));
            }

            Ok(format!("{{{}}}", elts_repr))
        } else {
            Ok("{{}}".to_owned())
        }
    }
}

/// A short-term binding of [`Context`] and any immutable object of a
/// type that implements the [`Contextual`] trait.
///
/// The purpose of this type is to allow a transparent read access to
/// shared data, like names etc.  To prevent coarse-grained locking,
/// [`Context`] is internally represented by a [`ContextHandle`];
/// however, [`Context`] can't be modified through `InContext`.
pub struct InContext<'a, D: Contextual> {
    context: ContextHandle,
    thing:   &'a D,
}

impl<D: Contextual> InContext<'_, D> {
    pub fn using_context<T, F>(&self, f: F) -> T
    where
        F: FnOnce(&D, &Context) -> T,
    {
        let ctx = self.context.lock().unwrap();

        f(self.thing, &ctx)
    }

    #[inline]
    pub fn same_context(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.context, &other.context)
    }

    #[inline]
    pub fn get_context(&self) -> &ContextHandle {
        &self.context
    }

    #[inline]
    pub fn get_thing(&self) -> &D {
        self.thing
    }
}

impl<'a, D: Contextual> fmt::Display for InContext<'a, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.thing.format(&self.context).expect("Can't display"))
    }
}

/// A short-term binding of [`Context`] and any mutable object of a
/// type that implements the [`Contextual`] trait.
///
/// The purpose of this type is to allow a transparent read access to
/// shared data, like names etc.  To prevent coarse-grained locking,
/// [`Context`] is internally represented by a [`ContextHandle`];
/// however, [`Context`] can't be modified through `InContextMut`.
pub struct InContextMut<'a, D: Contextual> {
    context: ContextHandle,
    thing:   &'a mut D,
}

impl<D: Contextual> InContextMut<'_, D> {
    pub fn using_context<T, F>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut D, &Context) -> T,
    {
        let ctx = self.context.lock().unwrap();

        f(self.thing, &ctx)
    }

    #[inline]
    pub fn same_context(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.context, &other.context)
    }

    #[inline]
    pub fn get_context(&self) -> &ContextHandle {
        &self.context
    }

    #[inline]
    pub fn get_thing(&self) -> &D {
        self.thing
    }

    #[inline]
    pub fn get_thing_mut(&mut self) -> &mut D {
        self.thing
    }
}

impl<'a, D: Contextual> fmt::Display for InContextMut<'a, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.thing.format(&self.context).expect("Can't display"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_port(ctx: &ContextHandle, face: Face, host_name: &str) -> (Port, PortId) {
        let mut ctx = ctx.lock().unwrap();
        let node_id = ctx.share_node_name(host_name);
        let mut port = Port::new(face, node_id);
        let port_id = ctx.share_port(&mut port);

        (port, port_id)
    }

    #[test]
    fn test_partial_order() {
        let toplevel = Context::new_toplevel("toplevel");
        let derived = Context::new_derived("derived", &toplevel);

        assert_eq!(
            toplevel.lock().unwrap().partial_cmp(&derived.lock().unwrap()),
            Some(cmp::Ordering::Greater)
        );
    }

    #[test]
    fn test_derivation() {
        let toplevel = Context::new_toplevel("toplevel");
        let (a_port, a_port_id) = new_port(&toplevel, Face::Tx, "a");

        {
            let derived = Context::new_derived("derived", &toplevel);
            let (_, z_port_id) = new_port(&derived, Face::Rx, "z");

            assert_eq!(derived.lock().unwrap().get_port(a_port_id), Some(&a_port));
            assert_eq!(toplevel.lock().unwrap().get_port(z_port_id), None);
        }

        let (b_port, b_port_id) = new_port(&toplevel, Face::Tx, "b");

        assert_eq!(toplevel.lock().unwrap().get_port(b_port_id), Some(&b_port));
    }
}
