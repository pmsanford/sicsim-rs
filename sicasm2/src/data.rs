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
    pub fn from_env() -> Result<Self> {
        let database_url = env::var("DATABASE_URL").unwrap_or_else(|_| ":memory:".to_owned());
        let mut conn = SqliteConnection::establish(&database_url)?;
        sql_query("PRAGMA FOREIGN_KEYS = ON;").execute(&mut conn)?;
        conn.revert_all_migrations(MIGRATIONS)
            .map_err(|e| anyhow!("Unable to clean database: {e}"))?;
        conn.run_pending_migrations(MIGRATIONS)
            .map_err(|e| anyhow!("Unable to run migrations: {e}"))?;

        Ok(Self { conn })
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
}
