use std::{
    cmp, fmt,
    collections::BTreeMap,
    sync::{Arc, Mutex},
    error::Error,
};
use crate::{
    ContentOrigin, PartialContent, Port, Link, Split, Fork, Join, ID, NodeID, AtomID, PortID,
    LinkID, ForkID, JoinID, Semantics, Capacity, Weight,
    name::NameSpace,
    atom::{AtomSpace, Atom},
    node, sat, solver, runner,
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
/// another for node names), node capacities, split weights, and
/// [`PartialContent`] of any c-e structure created in this `Context`.
///
/// For usage, see [`ContextHandle`] type, [`Contextual`] trait and
/// [`InContext`] struct.
///
/// [`Atom`]: crate::atom::Atom
#[derive(Debug)]
pub struct Context {
    magic_id:     u64, // group ID
    name_id:      ID,  // given name
    origin:       ContentOrigin,
    globals:      NameSpace,
    nodes:        NameSpace,
    atoms:        AtomSpace,
    content:      BTreeMap<ID, PartialContent>,
    capacities:   BTreeMap<NodeID, Capacity>,
    weights:      BTreeMap<AtomID, Weight>,
    solver_props: solver::Props,
    runner_props: runner::Props,
}

impl Context {
    /// Creates a new toplevel `Context` instance and returns the
    /// corresponding [`ContextHandle`].
    ///
    /// Calling this method, or its variant, [`new_interactive()`], is
    /// the only public way of creating toplevel `Context` instances.
    ///
    /// [`new_interactive()`]: Context::new_interactive()
    pub fn new_toplevel<S: AsRef<str>>(name: S, origin: ContentOrigin) -> ContextHandle {
        let magic_id = rand::random();

        let mut globals = NameSpace::default();
        let name_id = globals.share_name(name);

        let ctx = Self {
            magic_id,
            name_id,
            origin,
            globals,
            nodes: Default::default(),
            atoms: Default::default(),
            content: Default::default(),
            capacities: Default::default(),
            weights: Default::default(),
            solver_props: Default::default(),
            runner_props: Default::default(),
        };

        Arc::new(Mutex::new(ctx))
    }

    /// Creates a new toplevel `Context` instance, sets content origin
    /// to [`ContentOrigin::Interactive`] and returns the
    /// corresponding [`ContextHandle`].
    ///
    /// This is a specialized variant of the [`new_toplevel()`]
    /// method.
    ///
    /// [`new_toplevel()`]: Context::new_toplevel()
    pub fn new_interactive<S: AsRef<str>>(name: S) -> ContextHandle {
        Context::new_toplevel(name, ContentOrigin::Interactive)
    }

    /// Clears content map, capacities, weights and runtime
    /// attributes, but doesn't destroy shared resources.
    ///
    /// This method preserves the collection of [`Atom`]s, the two
    /// symbol tables, and the `Context`'s own given name and group
    /// ID.
    pub fn reset(&mut self, new_origin: ContentOrigin) {
        // Fields unchanged: magic_id, name_id, globals, nodes, atoms.
        self.origin = new_origin;
        self.content.clear();
        self.capacities.clear();
        self.weights.clear();
        self.solver_props.clear();
        self.runner_props.clear();
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

            // ID of child name in parent and child is the same (but
            // not in further ancestors).  See `partial_cmp`.
            let name_id = parent.globals.share_name(name);

            let origin = parent.origin.clone();
            let globals = parent.globals.clone();
            let nodes = parent.nodes.clone();
            let atoms = parent.atoms.clone();
            let content = parent.content.clone();
            let capacities = parent.capacities.clone();
            let weights = parent.weights.clone();
            let solver_props = parent.solver_props.clone();
            let runner_props = parent.runner_props.clone();

            Self {
                magic_id,
                name_id,
                origin,
                globals,
                nodes,
                atoms,
                content,
                capacities,
                weights,
                solver_props,
                runner_props,
            }
        };

        Arc::new(Mutex::new(ctx))
    }

    pub fn is_toplevel(&self) -> bool {
        self.name_id == unsafe { ID::new_unchecked(1) }
    }

    pub fn is_derived(&self) -> bool {
        self.name_id > unsafe { ID::new_unchecked(1) }
    }

    pub fn get_name(&self) -> &str {
        self.globals.get_name(self.name_id).expect("Invalid context.")
    }

    pub fn get_origin(&self) -> &ContentOrigin {
        &self.origin
    }

    // Nodes

    #[inline]
    pub(crate) fn get_nodes(&self) -> &NameSpace {
        &self.nodes
    }

    #[inline]
    pub fn share_node_name<S: AsRef<str>>(&mut self, node_name: S) -> NodeID {
        NodeID(self.nodes.share_name(node_name))
    }

    #[inline]
    pub fn get_node_name(&self, node_id: NodeID) -> Option<&str> {
        self.nodes.get_name(node_id.get())
    }

    #[inline]
    pub fn get_node_id<S: AsRef<str>>(&self, node_name: S) -> Option<NodeID> {
        self.nodes.get_id(node_name).map(NodeID)
    }

    // Atoms

    #[inline]
    pub(crate) fn get_atom(&self, atom_id: AtomID) -> Option<&Atom> {
        self.atoms.get_atom(atom_id)
    }

    #[allow(dead_code)]
    #[inline]
    pub(crate) fn get_atom_id(&self, atom: &Atom) -> Option<AtomID> {
        self.atoms.get_atom_id(atom)
    }

    #[inline]
    pub fn is_port(&self, atom_id: AtomID) -> bool {
        self.atoms.is_port(atom_id)
    }

    #[inline]
    pub fn is_link(&self, atom_id: AtomID) -> bool {
        self.atoms.is_link(atom_id)
    }

    #[inline]
    pub fn is_split(&self, atom_id: AtomID) -> bool {
        self.atoms.is_split(atom_id)
    }

    #[inline]
    pub fn is_fork(&self, atom_id: AtomID) -> bool {
        self.atoms.is_fork(atom_id)
    }

    #[inline]
    pub fn is_join(&self, atom_id: AtomID) -> bool {
        self.atoms.is_join(atom_id)
    }

    #[inline]
    pub fn share_port(&mut self, port: &mut Port) -> PortID {
        self.atoms.share_port(port)
    }

    #[inline]
    pub fn share_link(&mut self, link: &mut Link) -> LinkID {
        self.atoms.share_link(link)
    }

    #[inline]
    pub fn share_fork(&mut self, fork: &mut Fork) -> ForkID {
        self.atoms.share_fork(fork)
    }

    #[inline]
    pub fn share_join(&mut self, join: &mut Join) -> JoinID {
        self.atoms.share_join(join)
    }

    #[inline]
    pub fn get_port(&self, port_id: PortID) -> Option<&Port> {
        self.atoms.get_port(port_id)
    }

    #[inline]
    pub fn get_link(&self, link_id: LinkID) -> Option<&Link> {
        self.atoms.get_link(link_id)
    }

    #[inline]
    pub fn get_split(&self, atom_id: AtomID) -> Option<&Split> {
        self.atoms.get_split(atom_id)
    }

    #[inline]
    pub fn get_fork(&self, fork_id: ForkID) -> Option<&Fork> {
        self.atoms.get_fork(fork_id)
    }

    #[inline]
    pub fn get_join(&self, join_id: JoinID) -> Option<&Join> {
        self.atoms.get_join(join_id)
    }

    #[inline]
    pub fn get_antiport_id(&self, port_id: PortID) -> Option<PortID> {
        self.atoms.get_antiport_id(port_id)
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

    pub fn get_capacity(&self, node_id: NodeID) -> Capacity {
        self.capacities.get(&node_id).copied().unwrap_or_else(Capacity::one)
    }

    // Weights

    pub fn set_weight_by_name<S, I>(
        &mut self,
        face: node::Face,
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
        let suit_ids = suit_names.into_iter().map(|n| self.share_node_name(n.as_ref())).collect();

        let atom_id = match face {
            node::Face::Tx => {
                let mut fork = Split::new_fork(host_id, suit_ids);
                let fork_id = self.share_fork(&mut fork);

                fork_id.get()
            }
            node::Face::Rx => {
                let mut join = Split::new_join(host_id, suit_ids);
                let join_id = self.share_join(&mut join);

                join_id.get()
            }
        };

        self.weights.insert(atom_id, weight)
    }

    pub fn set_inhibitor_by_name<S, I>(
        &mut self,
        face: node::Face,
        host_name: S,
        suit_names: I,
    ) -> Option<Weight>
    where
        S: AsRef<str>,
        I: IntoIterator,
        I::Item: AsRef<str>,
    {
        self.set_weight_by_name(face, host_name, suit_names, Weight::omega())
    }

    pub fn get_weight(&self, atom_id: AtomID) -> Weight {
        self.weights.get(&atom_id).copied().unwrap_or_else(Weight::one)
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
}

impl PartialEq for Context {
    fn eq(&self, other: &Self) -> bool {
        self.magic_id == other.magic_id && self.name_id == other.name_id
    }
}

impl Eq for Context {}

impl PartialOrd for Context {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        if self.magic_id == other.magic_id {
            if other.globals.get_id(self.get_name()) == Some(self.name_id) {
                // ID of self's name in self and other is the same...
                if self.name_id < other.name_id {
                    // ...self is an ancestor of other,
                    Some(cmp::Ordering::Greater)
                } else if self.name_id > other.name_id {
                    // ...other is _the_ parent of self,
                    Some(cmp::Ordering::Less)
                } else {
                    // ...they are equal.
                    Some(cmp::Ordering::Equal)
                }
            } else if self.globals.get_id(other.get_name()) == Some(other.name_id) {
                // ID of other's name in self and other is the same...
                if self.name_id < other.name_id {
                    // ...self is _the_ parent of other: this case
                    // should have already been covered,
                    unreachable!()
                } else if self.name_id > other.name_id {
                    // ...other is an ancestor of self,
                    Some(cmp::Ordering::Less)
                } else {
                    // ...they are equal: this case should have
                    // already been covered.
                    unreachable!()
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
    fn format(&self, ctx: &ContextHandle) -> Result<String, Box<dyn Error>>;

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
    fn format_locked(&self, ctx: &Context) -> Result<String, Box<dyn Error>>;
}

impl<T: ExclusivelyContextual> Contextual for T {
    #[inline]
    fn format(&self, ctx: &ContextHandle) -> Result<String, Box<dyn Error>> {
        self.format_locked(&ctx.lock().unwrap())
    }
}

impl<T: ExclusivelyContextual> Contextual for Vec<T> {
    fn format(&self, ctx: &ContextHandle) -> Result<String, Box<dyn Error>> {
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
    pub fn with_context<T, F>(&self, f: F) -> T
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
    pub fn with_context<T, F>(&mut self, f: F) -> T
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
    use crate::node;

    fn new_port(ctx: &ContextHandle, face: node::Face, host_name: &str) -> (Port, PortID) {
        let mut ctx = ctx.lock().unwrap();
        let node_id = ctx.share_node_name(host_name);
        let mut port = Port::new(face, node_id);
        let port_id = ctx.share_port(&mut port);

        (port, port_id)
    }

    #[test]
    fn test_partial_order() {
        let toplevel = Context::new_interactive("toplevel");
        let derived = Context::new_derived("derived", &toplevel);

        assert_eq!(
            toplevel.lock().unwrap().partial_cmp(&derived.lock().unwrap()),
            Some(cmp::Ordering::Greater)
        );
    }

    #[test]
    fn test_derivation() {
        let toplevel = Context::new_interactive("toplevel");
        let (a_port, a_port_id) = new_port(&toplevel, node::Face::Tx, "a");

        {
            let derived = Context::new_derived("derived", &toplevel);
            let (_, z_port_id) = new_port(&derived, node::Face::Rx, "z");

            assert_eq!(derived.lock().unwrap().get_port(a_port_id), Some(&a_port));
            assert_eq!(toplevel.lock().unwrap().get_port(z_port_id), None);
        }

        let (b_port, b_port_id) = new_port(&toplevel, node::Face::Tx, "b");

        assert_eq!(toplevel.lock().unwrap().get_port(b_port_id), Some(&b_port));
    }
}
