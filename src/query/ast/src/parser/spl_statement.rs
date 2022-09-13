use common_exception::ErrorCode;
use common_exception::Result;
use nom::combinator::consumed;
use nom::combinator::map;

use crate::ast::Query;
use crate::ast::SelectStmt;
use crate::ast::SelectTarget;
use crate::ast::SetExpr;
use crate::ast::Statement;
use crate::ast::StatementMsg;
use crate::ast::TableReference;
use crate::parser::expr::*;
use crate::parser::query::select_target;
use crate::parser::token::Token;
use crate::parser::token::TokenKind;
use crate::parser::token::Tokenizer;
use crate::parser::TokenKind::*;
use crate::rule;
use crate::util::*;
use crate::Backtrace;
use crate::Dialect;
use crate::DisplayError;
use crate::Input;

pub fn tokenize_spl(sql: &str) -> Result<Vec<Token>> {
    Tokenizer::new(sql).collect::<Result<Vec<_>>>()
}

/// Parse a SPL string into `Statement`s.
pub fn parse_spl<'a>(
    spl_tokens: &'a [Token<'a>],
    dialect: Dialect,
    backtrace: &'a Backtrace<'a>,
) -> Result<(Statement<'a>, Option<String>)> {
    match statement(Input(spl_tokens, dialect, backtrace)) {
        Ok((rest, stmts)) if rest[0].kind == TokenKind::EOI => Ok((stmts.stmt, stmts.format)),
        Ok((rest, _)) => Err(ErrorCode::SyntaxException(
            rest[0].display_error("unable to parse rest of the sql".to_string()),
        )),
        Err(nom::Err::Error(err) | nom::Err::Failure(err)) => {
            Err(ErrorCode::SyntaxException(err.display_error(())))
        }
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}

pub fn statement(i: Input) -> IResult<StatementMsg> {
    map(rule! {#query}, |query| {
        let stmt = Statement::Query(Box::new(query));
        StatementMsg { stmt, format: None }
    })(i)
}

pub fn query(i: Input) -> IResult<Query> {
    map(
        consumed(rule! {
            #set_operation: "`search source=mytable mycolumn=5 | FIELDS mycolumn1, mycolumn2`"
        }),
        |(span, body)| Query {
            span: span.0,
            with: None,
            body,
            order_by: vec![],
            limit: vec![],
            offset: None,
            format: None,
        },
    )(i)
}

pub fn set_operation(i: Input) -> IResult<SetExpr> {
    map(
        consumed(rule! {
            SEARCH ~ #source ~ (#expr)? ~ (#pipe_fields)? : "`search source=mytable mycolumn=5 | FIELDS mycolumn1, mycolumn2`"
        }),
        |(span, (_, source, selection, select_list))| {
            let statement = SelectStmt {
                span: span.0,
                distinct: false,
                select_list: select_list.unwrap_or(vec![]),
                from: vec![source],
                selection,
                group_by: vec![],
                having: None,
            };
            SetExpr::Select(Box::new(statement))
        },
    )(i)
}

pub fn source(i: Input) -> IResult<TableReference> {
    let table = map(consumed(rule! {#ident}), |(span, name)| {
        TableReference::Table {
            span: span.0,
            catalog: None,
            database: None,
            table: name,
            alias: None,
            travel_point: None,
        }
    });
    let table_function = map(
        consumed(rule! {
            #ident ~ "(" ~ #comma_separated_list0(expr) ~ ")"
        }),
        |(span, (name, _, params, _))| TableReference::TableFunction {
            span: span.0,
            name,
            params,
            alias: None,
        },
    );
    map(
        rule!(SOURCE ~ Eq ~ (#table_function | #table)),
        |(_, _, source)| source,
    )(i)
    // map(rule! {SOURCE ~ Eq ~ #table_reference}, |(_, _, table)| {
    //     table
    // })(i)
}

pub fn pipe_fields(i: Input) -> IResult<Vec<SelectTarget>> {
    map(
        rule! {
            Pipe ~ FIELDS ~ ^#comma_separated_list1(select_target)
        },
        |(_, _, fields)| fields,
    )(i)
}

#[cfg(test)]
mod test {
    use common_exception::Result;

    use crate::parser::spl_statement::*;
    use crate::parser::token::TokenKind;
    use crate::parser::token::Tokenizer;
    use crate::Backtrace;
    use crate::Dialect;
    use crate::Input;

    #[test]
    fn test_tokenize_spl() -> Result<()> {
        let spl =
            r#"search source=mytable mycolum=10 and column1 > 99 | FIELDS mycolumn2, mycolumn3"#;
        let tokens = tokenize_spl(spl)?;
        dbg!(&tokens);

        let dialect = Dialect::MySQL;
        let backtrace = Backtrace::new();
        let (statement, _) = parse_spl(&tokens, dialect, &backtrace)?;
        dbg!(statement);

        Ok(())
    }

    #[test]
    fn test_statement() -> Result<()> {
        let spl =
            r#"search source=mytable mycolum=10 and column1 > 99 | FIELDS mycolumn2, mycolumn3"#;
        let tokens = Tokenizer::new(spl).collect::<Result<Vec<_>>>()?;
        let dialect = Dialect::MySQL;
        let backtrace = Backtrace::new();
        let input = Input(&tokens, dialect, &backtrace);
        let (input, statement) = statement(input).unwrap();
        dbg!(input.0.len(), input[0].kind);
        dbg!(statement);
        Ok(())
    }

    #[test]
    fn test_query() -> Result<()> {
        let spl =
            r#"search source=mytable mycolum=10 and column1 > 99 | FIELDS mycolumn2, mycolumn3"#;
        let tokens = Tokenizer::new(spl).collect::<Result<Vec<_>>>()?;
        let dialect = Dialect::MySQL;
        let backtrace = Backtrace::new();
        let input = Input(&tokens, dialect, &backtrace);
        let (input, query) = query(input).unwrap();
        dbg!(query);
        assert_eq!(input.len(), 1);
        assert_eq!(input[0].kind, TokenKind::EOI);
        Ok(())
    }

    #[test]
    fn test_set_operation() -> Result<()> {
        let spl = r#"search source=number(1000) mycolum=10 and column1 > 99 | FIELDS mycolumn2, mycolumn3"#;
        let tokens = Tokenizer::new(spl).collect::<Result<Vec<_>>>()?;
        let dialect = Dialect::MySQL;
        let backtrace = Backtrace::new();
        let input = Input(&tokens, dialect, &backtrace);
        let (input, set_expr) = set_operation(input).unwrap();
        // dbg!(input, input.len(), input[0].kind);
        dbg!(set_expr);
        assert_eq!(input.len(), 1);
        assert_eq!(input[0].kind, TokenKind::EOI);
        Ok(())
    }

    #[test]
    fn test_pipe_fields() -> Result<()> {
        let spl = r#"| FIELDS mycolumn2, mycolumn3"#;
        let tokens = Tokenizer::new(spl).collect::<Result<Vec<_>>>()?;
        let dialect = Dialect::MySQL;
        let backtrace = Backtrace::new();
        let input = Input(&tokens, dialect, &backtrace);
        let (input, fields) = pipe_fields(input).unwrap();
        // dbg!(input, input.len(), input[0].kind);
        dbg!(fields);
        assert_eq!(input.len(), 1);
        assert_eq!(input[0].kind, TokenKind::EOI);
        Ok(())
    }

    #[test]
    fn test_source() -> Result<()> {
        {
            let spl = r#"source=mytable"#;
            let tokens = Tokenizer::new(spl).collect::<Result<Vec<_>>>()?;
            let dialect = Dialect::MySQL;
            let backtrace = Backtrace::new();
            let input = Input(&tokens, dialect, &backtrace);
            let (input, table_ref) = source(input).unwrap();
            // dbg!(input, input.len(), input[0].kind);
            dbg!(table_ref);
            assert_eq!(input.len(), 1);
            assert_eq!(input[0].kind, TokenKind::EOI);
        }
        {
            let spl = r#"source=number(1000)"#;
            let tokens = Tokenizer::new(spl).collect::<Result<Vec<_>>>()?;
            let dialect = Dialect::MySQL;
            let backtrace = Backtrace::new();
            let input = Input(&tokens, dialect, &backtrace);
            let (input, table_ref) = source(input).unwrap();
            // dbg!(input, input.len(), input[0].kind);
            dbg!(table_ref);
            assert_eq!(input.len(), 1);
            assert_eq!(input[0].kind, TokenKind::EOI);
        }
        Ok(())
    }
}
