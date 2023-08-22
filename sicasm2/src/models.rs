use crate::{
    parser::{AddressModifier, Argument, Directive},
    schema::*,
};
use diesel::prelude::*;

#[derive(Queryable, Debug, Identifiable, Associations, Insertable)]
#[diesel(primary_key(label_name))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
#[diesel(belongs_to(Line, foreign_key = line_no))]
#[diesel(belongs_to(ControlSection, foreign_key = section_name))]
pub struct Label {
    pub section_name: String,
    pub line_no: i32,
    pub label_name: String,
    pub offset: i32,
}

#[derive(Debug, Identifiable, Associations)]
#[diesel(primary_key(line_no))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
#[diesel(belongs_to(ProgramBlock, foreign_key = block_id))]
pub struct Line {
    pub block_id: i32,
    pub line_no: usize,
    pub directive: Directive,
    pub argument: Option<Argument>,
    pub address_modifier: AddressModifier,
    pub extended: bool,
    pub indexed: bool,
    pub size: usize,
    pub offset: usize,
    pub text: String,
}

use diesel::dsl;
impl<'insert> Insertable<lines::table> for &'insert Line {
    type Values = <(
        Option<dsl::Eq<lines::block_id, i32>>,
        Option<dsl::Eq<lines::line_no, i32>>,
        Option<dsl::Eq<lines::directive, String>>,
        Option<dsl::Eq<lines::argument, String>>,
        Option<dsl::Eq<lines::address_modifier, String>>,
        Option<dsl::Eq<lines::extended, i32>>,
        Option<dsl::Eq<lines::indexed, i32>>,
        Option<dsl::Eq<lines::size, i32>>,
        Option<dsl::Eq<lines::offset, i32>>,
        Option<dsl::Eq<lines::text, &'insert String>>,
    ) as Insertable<lines::table>>::Values;

    fn values(self) -> Self::Values {
        (
            std::option::Option::Some(lines::block_id.eq(self.block_id)),
            std::option::Option::Some(lines::line_no.eq(self.line_no as i32)),
            std::option::Option::Some(
                lines::directive.eq(serde_json::to_string(&self.directive).unwrap()),
            ),
            self.argument
                .as_ref()
                .map(|x| lines::argument.eq(serde_json::to_string(&x).unwrap())),
            std::option::Option::Some(
                lines::address_modifier.eq(serde_json::to_string(&self.address_modifier).unwrap()),
            ),
            std::option::Option::Some(lines::extended.eq(if self.extended { 1 } else { 0 })),
            std::option::Option::Some(lines::indexed.eq(if self.indexed { 1 } else { 0 })),
            std::option::Option::Some(lines::size.eq(self.size as i32)),
            std::option::Option::Some(lines::offset.eq(self.offset as i32)),
            std::option::Option::Some(lines::text.eq(&self.text)),
        )
            .values()
    }
}

impl Queryable<lines::SqlType, diesel::sqlite::Sqlite> for Line {
    type Row = (
        i32,
        i32,
        String,
        Option<String>,
        String,
        i32,
        i32,
        i32,
        i32,
        String,
    );

    fn build(row: Self::Row) -> diesel::deserialize::Result<Self> {
        Ok(Self {
            block_id: row.0,
            line_no: row.1.try_into()?,
            directive: serde_json::from_str(&row.2)?,
            argument: row.3.map(|arg| serde_json::from_str(&arg)).transpose()?,
            address_modifier: serde_json::from_str(&row.4)?,
            extended: row.5 > 0,
            indexed: row.6 > 0,
            size: row.7.try_into()?,
            offset: row.8.try_into()?,
            text: row.9,
        })
    }
}

#[derive(Queryable, Debug, Identifiable, Associations, Insertable, Selectable)]
#[diesel(primary_key(block_id, value))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
#[diesel(belongs_to(ProgramBlock, foreign_key = block_id))]
pub struct Literal {
    pub block_id: i32,
    pub offset: Option<i32>,
    pub value: Vec<u8>,
}

#[derive(Queryable, Debug, Identifiable, Associations, Insertable, Selectable)]
#[diesel(primary_key(block_id, offset))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
#[diesel(belongs_to(ProgramBlock, foreign_key = block_id))]
pub struct Ltorg {
    pub block_id: i32,
    pub offset: i32,
    pub data: Vec<u8>,
}

#[derive(Queryable, Debug, Identifiable, Selectable)]
#[diesel(primary_key(block_id))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct ProgramBlock {
    pub block_id: i32,
    pub section_name: String,
    pub block_name: String,
    pub current_offset: i32,
}

#[derive(Debug, Insertable)]
#[diesel(primary_key(block_id))]
#[diesel(table_name = program_blocks)]
pub struct ProgramBlockInsert {
    pub section_name: String,
    pub block_name: String,
    pub current_offset: i32,
}

impl ProgramBlockInsert {
    pub fn new(section: String, name: String) -> Self {
        Self {
            section_name: section,
            block_name: name,
            current_offset: 0
        }
    }
}

#[derive(Queryable, Debug, Identifiable, Selectable, Insertable)]
#[diesel(primary_key(section_name))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct ControlSection {
    pub section_name: String,
}

impl ControlSection {
    pub fn new(name: String) -> Self {
        ControlSection { section_name: name }
    }
}
