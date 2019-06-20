use std::collections::HashMap;
use crate::ID;

/// An interned `String` representing the name of a symbol.
type Name = string_cache::DefaultAtom;

/// This type maintains a bijection between symbol [`Name`]s and their
/// serial [`ID`]s.
#[derive(Debug)]
pub(crate) struct NameSpace {
    names: Vec<Name>,
    ids:   HashMap<Name, ID>,
}

impl NameSpace {
    pub(crate) fn get_name(&self, id: ID) -> Option<&str> {
        self.names.get(id.get()).map(|n| n.as_ref())
    }

    pub(crate) fn get_id<S: AsRef<str>>(&self, name: S) -> Option<ID> {
        self.ids.get(&Name::from(name.as_ref())).copied()
    }

    pub(crate) fn share_name<S: AsRef<str>>(&mut self, name: S) -> ID {
        self.ids.get(&Name::from(name.as_ref())).copied().unwrap_or_else(|| {
            let id = unsafe { ID::new_unchecked(self.names.len()) };
            let name = Name::from(name.as_ref());

            self.names.push(name.clone());
            self.ids.insert(name, id);

            id
        })
    }
}

impl Default for NameSpace {
    fn default() -> Self {
        Self { names: vec![Name::from("")], ids: Default::default() }
    }
}
