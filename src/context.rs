use std::{
    fmt::{self, Write},
    error::Error,
};
use crate::{
    Port, Face, Link, NodeID, PortID, LinkID, sat,
    name::NameSpace,
    atom::{AtomSpace, AtomID},
    error::AcesError,
};

#[derive(Default, Debug)]
pub struct Context {
    pub(crate) globals: NameSpace,
    pub(crate) nodes:   NameSpace,
    pub(crate) atoms:   AtomSpace,
}

impl Context {
    pub fn new() -> Self {
        Default::default()
    }

    // Nodes

    pub fn take_node_id<S: AsRef<str>>(&mut self, node_name: S) -> NodeID {
        NodeID(self.nodes.take_id(node_name))
    }

    pub fn get_node_name(&self, node_id: NodeID) -> Option<&str> {
        self.nodes.get_name(node_id.get())
    }

    pub fn get_node_id<S: AsRef<str>>(&self, node_name: S) -> Option<NodeID> {
        self.nodes.get_id(node_name).map(NodeID)
    }

    // Atoms

    pub(crate) fn is_port(&self, atom_id: AtomID) -> bool {
        self.atoms.is_port(atom_id)
    }

    pub fn take_port(&mut self, port: Port) -> PortID {
        self.atoms.take_port(port)
    }

    pub fn take_link(&mut self, link: Link) -> LinkID {
        self.atoms.take_link(link)
    }

    pub fn get_port(&self, port_id: PortID) -> Option<&Port> {
        self.atoms.get_port(port_id)
    }

    pub fn get_port_mut(&mut self, port_id: PortID) -> Option<&mut Port> {
        self.atoms.get_port_mut(port_id)
    }

    pub fn get_link(&self, link_id: LinkID) -> Option<&Link> {
        self.atoms.get_link(link_id)
    }

    pub fn get_link_mut(&mut self, link_id: LinkID) -> Option<&mut Link> {
        self.atoms.get_link_mut(link_id)
    }

    pub fn get_antiport_id(&self, port_id: PortID) -> Option<PortID> {
        self.atoms.get_antiport_id(port_id)
    }
}

pub trait Contextual {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>>;
}

impl Contextual for Port {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let node_name = ctx
            .get_node_name(self.get_node_id())
            .ok_or(AcesError::NodeMissingForPort(Face::Tx))?;

        Ok(format!("[{} {}]", node_name, self.get_face()))
    }
}

impl Contextual for Link {
    fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
        let tx_node_name = ctx
            .get_node_name(self.get_tx_node_id())
            .ok_or(AcesError::NodeMissingForPort(Face::Tx))?;
        let rx_node_name = ctx
            .get_node_name(self.get_rx_node_id())
            .ok_or(AcesError::NodeMissingForPort(Face::Rx))?;

        Ok(format!("({} > {})", tx_node_name, rx_node_name))
    }
}

impl Contextual for sat::Variable {
   fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
       let mut result = String::new();
       let atom_id = self.into_atom_id();

       if let Some(port) = ctx.get_port(PortID(atom_id)) {
           result.write_fmt(format_args!("{}", port.format(ctx)?))?;
       } else if let Some(link) = ctx.get_link(LinkID(atom_id)) {
           result.write_fmt(format_args!("{}", link.format(ctx)?))?;
       } else {
           result.push_str("???");
       }

       Ok(result)
   }
}

impl Contextual for sat::Literal {
   fn format(&self, ctx: &Context) -> Result<String, Box<dyn Error>> {
       if self.is_negative() {
           Ok(format!("~{}", sat::Variable(self.0.var()).format(ctx)?))
       } else {
           sat::Variable(self.0.var()).format(ctx)
       }
   }
}

/// A short-term binding of [`Context`] and any data implementing the
/// [`Contextual`] trait.
///
/// The main purpose of this type is to allow a transparent access to
/// names which are cached in a [`Context`].
pub struct WithContext<'a, D: Contextual>(&'a Context, &'a D);

impl<'a, D: Contextual> fmt::Display for WithContext<'a, D> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let WithContext(ctx, data) = *self;
        write!(f, "{}", data.format(ctx).expect("Can't display"))
    }
}

impl Context {
    pub fn with<'a, D: Contextual>(&'a self, data: &'a D) -> WithContext<'a, D> {
        WithContext(self, data)
    }
}
