use anyhow::Context;
use anyhow::{anyhow, Result};

use diesel::sql_query;
use diesel_migrations::{embed_migrations, EmbeddedMigrations, MigrationHarness};
use std::env;

use crate::models::*;
use crate::schema::*;
use diesel::prelude::*;

pub const MIGRATIONS: EmbeddedMigrations = embed_migrations!();

pub struct AsmData {
    pub conn: SqliteConnection,
}

impl AsmData {
    pub fn new_from_env() -> Result<Self> {
        let database_url = env::var("DATABASE_URL").unwrap_or_else(|_| ":memory:".to_owned());
        let mut conn = SqliteConnection::establish(&database_url)?;
        sql_query("PRAGMA FOREIGN_KEYS = ON;").execute(&mut conn)?;
        conn.run_pending_migrations(MIGRATIONS)
            .map_err(|e| anyhow!("Unable to run migrations: {e}"))?;

        Ok(AsmData::with_connection(conn))
    }

    pub fn from_env() -> Result<Self> {
        let database_url = env::var("DATABASE_URL").unwrap_or_else(|_| ":memory:".to_owned());
        let conn = SqliteConnection::establish(&database_url)?;

        Ok(AsmData::with_connection(conn))
    }

    pub fn with_connection(conn: SqliteConnection) -> Self {
        Self { conn }
    }

    pub fn add_program_block(&mut self, name: String) -> Result<()> {
        diesel::insert_into(program_blocks::table)
            .values(&ProgramBlock::new(name.clone()))
            .execute(&mut self.conn)
            .with_context(|| format!("inserting program block {name}"))?;

        Ok(())
    }

    pub fn get_program_block(&mut self, name: &str) -> Result<Option<ProgramBlock>> {
        use crate::schema::program_blocks::dsl::program_blocks;
        let block = program_blocks
            .find(name)
            .get_result(&mut self.conn)
            .optional()?;

        Ok(block)
    }

    pub fn get_label(&mut self, name: &str) -> Result<Option<Label>> {
        use crate::schema::labels::dsl::labels;
        let label = labels.find(name).get_result(&mut self.conn).optional()?;

        Ok(label)
    }

    pub fn set_current_location(&mut self, block: &ProgramBlock) -> Result<()> {
        use crate::schema::program_blocks::dsl::current_offset;
        diesel::update(block)
            .set(current_offset.eq(block.current_offset))
            .execute(&mut self.conn)
            .with_context(|| format!("updating program block {}", block.block_name))?;

        Ok(())
    }

    pub fn add_label(&mut self, label: &Label) -> Result<()> {
        diesel::insert_into(labels::table)
            .values(label)
            .execute(&mut self.conn)
            .with_context(|| format!("inserting label {}", label.label_name))?;

        Ok(())
    }

    pub fn add_line(&mut self, line: &Line) -> Result<()> {
        diesel::insert_into(lines::table)
            .values(line)
            .execute(&mut self.conn)
            .with_context(|| format!("inserting line {}", line.line_no))?;

        Ok(())
    }

    pub fn add_literal(&mut self, literal: &Literal) -> Result<()> {
        diesel::insert_into(literals::table)
            .values(literal)
            .execute(&mut self.conn)
            .with_context(|| format!("inserting literal {:#?}", literal.value))?;

        Ok(())
    }

    pub fn get_unaddressed_literals(
        &mut self,
        program_block: &ProgramBlock,
    ) -> Result<Vec<Literal>> {
        let literals = Literal::belonging_to(&program_block)
            .select(Literal::as_select())
            .filter(literals::offset.is_null())
            .load(&mut self.conn)?;

        Ok(literals)
    }

    pub fn add_ltorg(&mut self, ltorg: &Ltorg) -> Result<()> {
        diesel::insert_into(ltorgs::table)
            .values(ltorg)
            .execute(&mut self.conn)
            .with_context(|| format!("inserting ltorg {}", ltorg.offset))?;

        Ok(())
    }

    pub fn update_literals(&mut self, literals: &Vec<Literal>) -> Result<()> {
        use crate::schema::literals::offset;
        for literal in literals {
            diesel::update(literal)
                .set(offset.eq(literal.offset))
                .execute(&mut self.conn)
                .with_context(|| {
                    format!(
                        "updating literal {:?} in block {}",
                        literal.value, literal.block_name
                    )
                })?;
        }

        Ok(())
    }

    pub fn get_lines(&mut self) -> Result<Vec<Line>> {
        use crate::schema::lines::dsl::{line_no, lines};
        let line_list = lines.order(line_no.asc()).get_results(&mut self.conn)?;

        Ok(line_list)
    }
}
