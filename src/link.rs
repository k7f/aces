use std::hash;
use crate::{
    Context, ExclusivelyContextual, Polarity, AtomId, PortId, LinkId, AcesError, AcesErrorKind,
    domain::DotId,
};

/// Representation of a link.
///
/// This is a bipolar, strong link (full directed edge), if used on
/// its own, or a unipolar link (half-edge, dart), if paired with a
/// [`Polarity`].  See [`FusetHolder`]'s private field `links`, or the
/// implementation of [`FusetHolder::check_coherence()`].
///
/// [`FusetHolder`]: crate::FusetHolder
/// [`FusetHolder::check_coherence()`]: crate::FusetHolder::check_coherence()
#[derive(Clone, Eq, Debug)]
pub struct Link {
    pub(crate) atom_id: Option<AtomId>,
    tx_port_id:         PortId,
    tx_dot_id:          DotId,
    rx_port_id:         PortId,
    rx_dot_id:          DotId,
}

impl Link {
    pub fn new(tx_port_id: PortId, tx_dot_id: DotId, rx_port_id: PortId, rx_dot_id: DotId) -> Self {
        Self { atom_id: None, tx_port_id, tx_dot_id, rx_port_id, rx_dot_id }
    }

    pub fn get_atom_id(&self) -> AtomId {
        self.atom_id.expect("Attempt to access an uninitialized link")
    }

    pub fn get_link_id(&self) -> LinkId {
        LinkId(self.get_atom_id())
    }

    pub fn get_port_id(&self, polarity: Polarity) -> PortId {
        if polarity == Polarity::Rx {
            self.rx_port_id
        } else {
            self.tx_port_id
        }
    }

    pub fn get_dot_id(&self, polarity: Polarity) -> DotId {
        if polarity == Polarity::Rx {
            self.rx_dot_id
        } else {
            self.tx_dot_id
        }
    }

    pub fn get_tx_port_id(&self) -> PortId {
        self.tx_port_id
    }

    pub fn get_tx_dot_id(&self) -> DotId {
        self.tx_dot_id
    }

    pub fn get_rx_port_id(&self) -> PortId {
        self.rx_port_id
    }

    pub fn get_rx_dot_id(&self) -> DotId {
        self.rx_dot_id
    }
}

impl PartialEq for Link {
    fn eq(&self, other: &Self) -> bool {
        self.tx_port_id == other.tx_port_id && self.rx_dot_id == other.rx_dot_id
    }
}

impl hash::Hash for Link {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.tx_port_id.hash(state);
        self.tx_dot_id.hash(state);
        self.rx_port_id.hash(state);
        self.rx_dot_id.hash(state);
    }
}

impl ExclusivelyContextual for Link {
    fn format_locked(&self, ctx: &Context) -> Result<String, AcesError> {
        let tx_dot_name = ctx
            .get_dot_name(self.get_tx_dot_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::DotMissingForLink(Polarity::Tx)))?;
        let rx_dot_name = ctx
            .get_dot_name(self.get_rx_dot_id())
            .ok_or_else(|| AcesError::from(AcesErrorKind::DotMissingForLink(Polarity::Rx)))?;

        Ok(format!("({} > {})", tx_dot_name, rx_dot_name))
    }
}
