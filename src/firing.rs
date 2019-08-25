use std::{collections::BTreeMap, error::Error};
use crate::{Context, Contextual, NodeID, ForkID, JoinID, monomial::Weight, node, sat};

#[derive(Default, Debug)]
pub struct FiringComponent {
    forks:    BTreeMap<ForkID, Weight>,
    joins:    BTreeMap<JoinID, Weight>,
    pre_set:  Vec<NodeID>,
    post_set: Vec<NodeID>,
}

impl From<sat::Solution> for FiringComponent {
    fn from(sol: sat::Solution) -> Self {
        FiringComponent {
            forks:    Default::default(), // FIXME
            joins:    Default::default(), // FIXME
            pre_set:  sol.get_pre_set().to_vec(),
            post_set: sol.get_post_set().to_vec(),
        }
    }
}

impl Contextual for FiringComponent {
    fn format(&self, ctx: &Context, dock: Option<node::Face>) -> Result<String, Box<dyn Error>> {
        let mut result = String::new();

        if self.pre_set.is_empty() {
            result.push_str("{} => {");
        } else {
            result.push('{');

            for node_id in self.pre_set.iter() {
                result.push(' ');
                result.push_str(node_id.format(ctx, dock)?.as_str());
            }

            result.push_str(" } => {");
        }

        if self.post_set.is_empty() {
            result.push('}');
        } else {
            for node_id in self.post_set.iter() {
                result.push(' ');
                result.push_str(node_id.format(ctx, dock)?.as_str());
            }

            result.push_str(" }");
        }

        Ok(result)
    }
}
