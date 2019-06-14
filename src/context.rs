use std::{
    collections::BTreeMap,
    fmt::{self, Write},
    error::Error,
};
use crate::{
    ID, Port, Face, Link, NodeID, PortID, LinkID, sat,
    atom::{AtomSpace, AtomID},
    error::AcesError,
};

#[derive(Debug)]
pub(crate) struct NameSpace {
    names: Vec<String>,
    ids:   BTreeMap<String, ID>, // FIXME borrow names
}

impl NameSpace {
    pub(crate) fn get_name(&self, id: ID) -> Option<&str> {
        self.names.get(id.get()).map(|s| s.as_str())
    }

    pub(crate) fn get_id<S: AsRef<str>>(&self, name: S) -> Option<ID> {
        self.ids.get(name.as_ref()).copied()
    }

    pub(crate) fn take_id<S: AsRef<str>>(&mut self, name: S) -> ID {
        self.ids.get(name.as_ref()).copied().unwrap_or_else(|| {
            let id = unsafe { ID::new_unchecked(self.names.len()) };

            self.names.push(name.as_ref().to_string());
            self.ids.insert(name.as_ref().to_string(), id);

            id
        })
    }
}

impl Default for NameSpace {
    fn default() -> Self {
        Self { names: vec![String::new()], ids: Default::default() }
    }
}

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

    pub fn format_port(&self, port: &Port) -> Result<String, Box<dyn Error>> {
        let node_name = self
            .get_node_name(port.get_node_id())
            .ok_or(AcesError::NodeMissingForPort(Face::Tx))?;

        Ok(format!("[{} {}]", node_name, port.get_face()))
    }

    pub fn format_link(&self, link: &Link) -> Result<String, Box<dyn Error>> {
        let tx_node_name = self
            .get_node_name(link.get_tx_node_id())
            .ok_or(AcesError::NodeMissingForPort(Face::Tx))?;
        let rx_node_name = self
            .get_node_name(link.get_rx_node_id())
            .ok_or(AcesError::NodeMissingForPort(Face::Rx))?;

        Ok(format!("({} > {})", tx_node_name, rx_node_name))
    }

    pub fn format_variable(&self, var: sat::Variable) -> Result<String, Box<dyn Error>> {
        let mut result = String::new();
        let atom_id = var.into_atom_id();

        if let Some(port) = self.get_port(PortID(atom_id)) {
            result.write_fmt(format_args!("{}", self.format_port(port)?))?;
        } else if let Some(link) = self.get_link(LinkID(atom_id)) {
            result.write_fmt(format_args!("{}", self.format_link(link)?))?;
        } else {
            result.push_str("???");
        }

        Ok(result)
    }

    pub fn format_literal(&self, lit: sat::Literal) -> Result<String, Box<dyn Error>> {
        if lit.is_negative() {
            Ok(format!("~{}", self.format_variable(sat::Variable(lit.0.var()))?))
        } else {
            self.format_variable(sat::Variable(lit.0.var()))
        }
    }

    pub fn with_port<'a>(&'a self, port: &'a Port) -> Contextual<'a> {
        Contextual::Port(self, port)
    }

    pub fn with_link<'a>(&'a self, link: &'a Link) -> Contextual<'a> {
        Contextual::Link(self, link)
    }

    pub fn with_variable(&self, var: sat::Variable) -> Contextual {
        Contextual::Var(self, var)
    }

    pub fn with_literal(&self, lit: sat::Literal) -> Contextual {
        Contextual::Lit(self, lit)
    }
}

pub enum Contextual<'a> {
    Var(&'a Context, sat::Variable),
    Lit(&'a Context, sat::Literal),
    Port(&'a Context, &'a Port),
    Link(&'a Context, &'a Link),
}

impl<'a> fmt::Display for Contextual<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Contextual::*;

        write!(
            f,
            "{}",
            match self {
                &Var(ctx, lit) => ctx.format_variable(lit),
                &Lit(ctx, lit) => ctx.format_literal(lit),
                &Port(ctx, port) => ctx.format_port(&port),
                &Link(ctx, link) => ctx.format_link(&link),
            }
            .expect("Can't display")
        )
    }
}
