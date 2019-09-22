use std::{
    cmp, fmt,
    sync::{Arc, Mutex},
    error::Error,
};
use crate::{
    ContentOrigin, Port, Link, Split, Fork, Join, ID, NodeID, AtomID, PortID, LinkID, ForkID,
    JoinID,
    name::NameSpace,
    atom::{AtomSpace, Atom},
};

/// A handle to a [`Context`] instance.
///
/// All [`Context`] handles used in _aces_ have type
/// `Arc<Mutex<Context>>`.  They are stored permanently in the
/// following structs: [`CEStructure`], [`sat::Formula`],
/// [`sat::Solver`], and [`sat::Solution`].
///
/// For another way of binding [`Context`] to data see [`Contextual`]
/// trait and [`InContext`] struct.
///
/// [`CEStructure`]: crate::CEStructure
/// [`sat::Formula`]: crate::sat::Formula
/// [`sat::Solver`]: crate::sat::Solver
/// [`sat::Solution`]: crate::sat::Solution
pub type ContextHandle = Arc<Mutex<Context>>;

/// A representation of shared state.
///
/// This is an umbrella type which, currently, includes a collection
/// of [`Atom`]s and two symbol tables, one for structure names, and
/// another for node names.
///
/// For usage, see [`ContextHandle`] type, [`Contextual`] trait and
/// [`InContext`] struct.
///
/// [`Atom`]: crate::atom::Atom
#[derive(Debug)]
pub struct Context {
    pub(crate) magic_id: usize,
    pub(crate) name_id:  ID,
    pub(crate) origin:   ContentOrigin,
    pub(crate) globals:  NameSpace,
    pub(crate) nodes:    NameSpace,
    pub(crate) atoms:    AtomSpace,
}

impl Context {
    /// Creates a new toplevel `Context` instance and returns the
    /// corresponding [`ContextHandle`].
    ///
    /// Calling this method, or its variant, [`new_interactive()`], is
    /// the only public way of creating toplevel `Context` instances.
    ///
    /// [`new_interactive()`]: [`Context::new_interactive()`]
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
    /// [`new_toplevel()`]: [`Context::new_toplevel()`]
    pub fn new_interactive<S: AsRef<str>>(name: S) -> ContextHandle {
        Context::new_toplevel(name, ContentOrigin::Interactive)
    }

    pub fn reset(&mut self, new_origin: ContentOrigin) {
        self.origin = new_origin;
        // FIXME clear
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

            Self { magic_id, name_id, origin, globals, nodes, atoms }
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

    // Nodes

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
pub trait Contextual {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>>;
}

impl<T: Contextual> Contextual for Vec<T> {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let mut elts = self.iter();

        if let Some(elt) = elts.next() {
            let mut elts_repr = elt.format(ctx)?;

            for elt in elts {
                elts_repr.push_str(&format!(", {}", ctx.with(elt)));
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
/// [`Context`] can't be modified through `InContext`.  The purpose
/// of this type is to allow a transparent read access to shared data,
/// like names etc.
pub struct InContext<'a, D: Contextual> {
    context: &'a Context,
    thing:   &'a D,
}

impl<D: Contextual> InContext<'_, D> {
    #[inline]
    pub fn get_context(&self) -> &Context {
        self.context
    }

    #[inline]
    pub fn get_thing(&self) -> &D {
        self.thing
    }
}

impl<'a, D: Contextual> fmt::Display for InContext<'a, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.thing.format(self.context).expect("Can't display"))
    }
}

/// A short-term binding of [`Context`] and any mutable object of a
/// type that implements the [`Contextual`] trait.
///
/// [`Context`] can't be modified through `InContextMut`.  The purpose
/// of this type is to allow a transparent read access to shared data,
/// like names etc.
pub struct InContextMut<'a, D: Contextual> {
    context: &'a Context,
    thing:   &'a mut D,
}

impl<D: Contextual> InContextMut<'_, D> {
    #[inline]
    pub fn get_context(&self) -> &Context {
        self.context
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
        write!(f, "{}", self.thing.format(self.context).expect("Can't display"))
    }
}

impl Context {
    #[inline]
    pub fn with<'a, T: Contextual>(&'a self, thing: &'a T) -> InContext<'a, T> {
        InContext { context: self, thing }
    }

    #[inline]
    pub fn with_mut<'a, T: Contextual>(&'a self, thing: &'a mut T) -> InContextMut<'a, T> {
        InContextMut { context: self, thing }
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

        let derived = Context::new_derived("derived", &toplevel);
        let (b_port, b_port_id) = new_port(&toplevel, node::Face::Tx, "b");
        let (_, z_port_id) = new_port(&derived, node::Face::Rx, "z");

        assert_eq!(derived.lock().unwrap().get_port(a_port_id), Some(&a_port));
        assert_eq!(derived.lock().unwrap().get_port(b_port_id), Some(&b_port));
        assert_eq!(toplevel.lock().unwrap().get_port(z_port_id), None);
    }
}
