use std::sync::Arc;

use common_exception::Result;
use common_planners::PlanNode;

use crate::api::MergeExchange;
use crate::interpreters::fragments::partition_state::PartitionState;
use crate::interpreters::fragments::query_fragment::QueryFragment;
use crate::interpreters::fragments::query_fragment_actions::QueryFragmentAction;
use crate::interpreters::fragments::query_fragment_actions::QueryFragmentActions;
use crate::interpreters::fragments::query_fragment_actions::QueryFragmentsActions;
use crate::sessions::QueryContext;

#[derive(Debug)]
pub struct RootQueryFragment {
    ctx: Arc<QueryContext>,
    node: PlanNode,
    input: Box<dyn QueryFragment>,
}

impl RootQueryFragment {
    pub fn create(
        input: Box<dyn QueryFragment>,
        ctx: Arc<QueryContext>,
        node: &PlanNode,
    ) -> Result<Box<dyn QueryFragment>> {
        Ok(Box::new(RootQueryFragment {
            input,
            ctx,
            node: node.clone(),
        }))
    }
}

impl QueryFragment for RootQueryFragment {
    fn distribute_query(&self) -> Result<bool> {
        self.input.distribute_query()
    }

    fn get_out_partition(&self) -> Result<PartitionState> {
        Ok(PartitionState::NotPartition)
    }

    fn finalize(&self, actions: &mut QueryFragmentsActions) -> Result<()> {
        self.input.finalize(actions)?;
        let input_actions = actions.get_root_actions()?;
        let fragment_id = self.ctx.get_fragment_id();
        let mut fragment_actions = QueryFragmentActions::create(false, fragment_id);

        // This is an implicit stage. We run remaining plans on the current hosts
        for action in input_actions.get_actions() {
            fragment_actions.add_action(QueryFragmentAction::create(
                action.executor.clone(),
                self.input.rewrite_remote_plan(&self.node, &action.node)?,
            ));
        }

        fragment_actions.set_exchange(MergeExchange::create(actions.get_local_executor()));
        match input_actions.exchange_actions {
            true => actions.add_fragment_actions(fragment_actions),
            false => actions.update_root_fragment_actions(fragment_actions),
        }
    }

    fn rewrite_remote_plan(&self, node: &PlanNode, _: &PlanNode) -> Result<PlanNode> {
        // Do nothing, We will not call this method on RootQueryFragment
        Ok(node.clone())
    }
}
