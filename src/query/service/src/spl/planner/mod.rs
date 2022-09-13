use std::sync::Arc;

use common_ast::parser::spl_statement::parse_spl;
use common_ast::parser::spl_statement::tokenize_spl;
use common_ast::Backtrace;
use common_catalog::table_context::TableContext;
use common_exception::Result;
use parking_lot::RwLock;

use crate::clusters::ClusterHelper;
use crate::sql::optimizer::optimize;
use crate::sql::optimizer::OptimizerConfig;
use crate::sql::optimizer::OptimizerContext;
use crate::sql::plans::Plan;
use crate::sql::Binder;
use crate::sql::Metadata;
use crate::sql::MetadataRef;
use crate::sql::NameResolutionContext;
use crate::sql::Planner;

impl Planner {
    pub async fn plan_spl(&mut self, spl: &str) -> Result<(Plan, MetadataRef, Option<String>)> {
        let settings = self.ctx.get_settings();
        let sql_dialect = settings.get_sql_dialect()?;

        // Step 1: parse SQL text into AST
        let tokens = tokenize_spl(spl)?;
        let backtrace = Backtrace::new();
        let (stmt, format) = parse_spl(&tokens, sql_dialect, &backtrace)?;

        // Step 2: bind AST with catalog, and generate a pure logical SExpr
        let metadata = Arc::new(RwLock::new(Metadata::create()));
        let name_resolution_ctx = NameResolutionContext::try_from(settings.as_ref())?;
        let binder = Binder::new(
            self.ctx.clone(),
            self.ctx.get_catalog_manager()?,
            name_resolution_ctx,
            metadata.clone(),
        );
        let plan = binder.bind(&stmt).await?;

        // Step 3: optimize the SExpr with optimizers, and generate optimized physical SExpr
        let opt_ctx = Arc::new(OptimizerContext::new(OptimizerConfig {
            enable_distributed_optimization: !self.ctx.get_cluster().is_empty(),
        }));
        let optimized_plan = optimize(self.ctx.clone(), opt_ctx, plan)?;

        Ok((optimized_plan, metadata.clone(), format))
    }
}

#[cfg(test)]
mod test {
    use common_base::base::tokio;
    use common_config::Config;
    use common_exception::Result;
    use futures_util::TryStreamExt;

    use crate::interpreters::InterpreterFactoryV2;
    use crate::sessions::SessionManager;
    use crate::sessions::SessionType;
    use crate::sql::Planner;
    use crate::GlobalServices;

    #[tokio::test]
    async fn test_plan_spl() -> Result<()> {
        let mut config = Config::default();
        config.query.tenant_id = "admin".to_string();
        config.query.num_cpus = 1;

        GlobalServices::init(config).await?;

        let session_manager = SessionManager::instance();
        let executor_session =
            SessionManager::create_session(&session_manager, SessionType::Dummy).await?;
        let ctx = executor_session.create_query_context().await?;

        let spl = r#"search source=numbers(1000) number < 10 | FIELDS number"#;

        let mut planner = Planner::new(ctx.clone());
        let (plan, _, _) = planner.plan_spl(spl).await?;
        // dbg!(&plan);
        let interpreter = InterpreterFactoryV2::get(ctx, &plan)?;

        let stream = interpreter.execute().await?;
        let results = stream.try_collect::<Vec<_>>().await?;
        let pretty_results = common_datablocks::pretty_format_blocks(&results)?;
        println!("{}", pretty_results);
        // while let Some(Ok(block)) = stream.next().await {
        //     println!("{:?}", block);
        // }
        Ok(())
    }
}
