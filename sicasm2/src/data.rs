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

    pub fn add_control_section(&mut self, section_name: String) -> Result<ControlSection> {
        let new_section = diesel::insert_into(control_sections::table)
            .values(&ControlSection::new(section_name.clone()))
            .get_result(&mut self.conn)
            .with_context(|| format!("inserting control section {section_name}"))?;

        Ok(new_section)
    }

    pub fn add_program_block(
        &mut self,
        section_name: String,
        name: String,
    ) -> Result<ProgramBlock> {
        use crate::schema::program_blocks::dsl;
        let new_block: ProgramBlock = diesel::insert_into(program_blocks::table)
            .values(ProgramBlockInsert::new(section_name, name.clone()))
            .get_result(&mut self.conn)
            .with_context(|| format!("inserting program block {name}"))?;

        let new_block = dsl::program_blocks
            .find(new_block.block_id)
            .get_result(&mut self.conn)
            .with_context(|| format!("retrieving block {}", new_block.block_id))?;

        Ok(new_block)
    }

    pub fn get_program_block(&mut self, id: i32) -> Result<Option<ProgramBlock>> {
        use crate::schema::program_blocks::dsl::program_blocks;
        let block = program_blocks
            .find(id)
            .get_result(&mut self.conn)
            .optional()?;

        Ok(block)
    }

    pub fn get_program_block_by_name(
        &mut self,
        section_name: &str,
        name: &str,
    ) -> Result<Option<ProgramBlock>> {
        // There's a unique constraint on (section_name, block_name)
        use crate::schema::program_blocks::dsl::{self, program_blocks};
        let block = program_blocks
            .filter(
                dsl::block_name
                    .eq(name)
                    .and(dsl::section_name.eq(section_name)),
            )
            .get_result(&mut self.conn)
            .optional()?;

        Ok(block)
    }

    pub fn get_control_sections(&mut self) -> Result<Vec<ControlSection>> {
        use crate::schema::control_sections::dsl::control_sections;
        Ok(control_sections.get_results(&mut self.conn)?)
    }

    pub fn get_program_blocks(&mut self, section_name: &str) -> Result<Vec<ProgramBlock>> {
        use crate::schema::program_blocks::dsl::{self, program_blocks};
        let block = program_blocks
            .filter(dsl::section_name.eq(section_name))
            .order(dsl::block_id)
            .get_results(&mut self.conn)?;

        Ok(block)
    }

    pub fn get_label_by_line(&mut self, number: usize) -> Result<Option<Label>> {
        use crate::schema::labels::dsl::{labels, line_no};
        let label = labels
            .filter(line_no.eq(number as i32))
            .get_result(&mut self.conn)
            .optional()?;

        Ok(label)
    }

    pub fn get_label(&mut self, section_name: &str, name: &str) -> Result<Option<Label>> {
        use crate::schema::labels::dsl::labels;
        let label = labels
            .find((section_name, name))
            .get_result(&mut self.conn)
            .optional()?;

        Ok(label)
    }

    pub fn set_start_location(&mut self, block: &ProgramBlock) -> Result<()> {
        use crate::schema::program_blocks::dsl::start_offset;
        diesel::update(block)
            .set(start_offset.eq(block.start_offset))
            .execute(&mut self.conn)
            .with_context(|| format!("updating program block {}", block.block_name))?;

        Ok(())
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
            .with_context(|| format!("inserting label {:?}", label))?;

        Ok(())
    }

    pub fn add_line(&mut self, line: &Line) -> Result<()> {
        diesel::insert_into(lines::table)
            .values(line)
            .execute(&mut self.conn)
            .with_context(|| format!("inserting line {}", line.line_no))?;

        Ok(())
    }

    pub fn literal_exists(&mut self, block_id: i32, literal_value: &Vec<u8>) -> Result<bool> {
        use literals::dsl::literals;

        Ok(literals
            .find((block_id, literal_value))
            .get_result::<Literal>(&mut self.conn)
            .optional()?
            .is_some())
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

    pub fn get_literal(&mut self, block_id: i32, literal: &[u8]) -> Result<Literal> {
        use crate::schema::literals::dsl::literals;
        let literal = literals
            .find((block_id, literal))
            .get_result(&mut self.conn)?;

        Ok(literal)
    }

    pub fn get_ltorg(&mut self, block_id: i32, offset: usize) -> Result<Ltorg> {
        use crate::schema::ltorgs::dsl::{self, ltorgs};
        let ltorg = ltorgs
            .filter(
                dsl::block_id
                    .eq(block_id)
                    .and(dsl::offset.eq(offset as i32)),
            )
            .get_result(&mut self.conn)?;

        Ok(ltorg)
    }

    pub fn get_final_ltorg(&mut self, block_id: i32) -> Result<Option<Ltorg>> {
        use crate::schema::ltorgs::dsl::{self, ltorgs};
        let ltorg = ltorgs
            .filter(dsl::block_id.eq(block_id))
            .order(dsl::offset.desc())
            .limit(1)
            .get_result(&mut self.conn)
            .optional()?;

        Ok(ltorg)
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
                        literal.value, literal.block_id
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

    pub fn get_labels(&mut self) -> Result<Vec<Label>> {
        use crate::schema::labels::dsl::labels;
        let label_list = labels.get_results(&mut self.conn)?;

        Ok(label_list)
    }

    pub fn get_section_length(&mut self, section_name: &str) -> Result<i32> {
        Ok(self
            .get_program_blocks(section_name)?
            .into_iter()
            .map(|block| block.current_offset)
            .sum())
    }

    pub fn create_control_section(&mut self, section_name: String) -> Result<ControlSection> {
        let csect = ControlSection { section_name };

        Ok(diesel::insert_into(control_sections::table)
            .values(csect)
            .get_result(&mut self.conn)?)
    }

    pub fn add_extref(&mut self, section_name: String, symbol_name: String) -> Result<Extref> {
        Ok(diesel::insert_into(extrefs::table)
            .values(Extref {
                section_name,
                symbol_name,
            })
            .get_result(&mut self.conn)?)
    }

    pub fn add_extdef(&mut self, section_name: String, symbol_name: String) -> Result<Extdef> {
        Ok(diesel::insert_into(extdefs::table)
            .values(Extdef {
                section_name,
                symbol_name,
            })
            .get_result(&mut self.conn)?)
    }

    pub fn get_extref(&mut self, section_name: &str, symbol_name: &str) -> Result<Option<Extref>> {
        use crate::schema::extrefs::dsl::extrefs;
        Ok(extrefs
            .find((section_name, symbol_name))
            .get_result(&mut self.conn)
            .optional()?)
    }
}
