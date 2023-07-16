use crate::{
    parser::{AddressModifier, Argument, Directive},
    schema::*,
};
use diesel::prelude::*;

#[derive(Queryable, Debug, Identifiable, Associations, Insertable)]
#[diesel(primary_key(block_name, label_name))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
#[diesel(belongs_to(Line, foreign_key = line_no))]
#[diesel(belongs_to(ProgramBlock, foreign_key = block_name))]
pub struct Label {
    pub block_name: String,
    pub line_no: i32,
    pub label_name: String,
    pub offset: i32,
}

#[derive(Debug, Identifiable, Associations)]
#[diesel(primary_key(line_no))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
#[diesel(belongs_to(ProgramBlock, foreign_key = block_name))]
pub struct Line {
    pub block_name: String,
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
        Option<dsl::Eq<lines::block_name, &'insert String>>,
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
            std::option::Option::Some(lines::block_name.eq(&self.block_name)),
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
        String,
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
        println!("Row: {:?}", row);
        println!(
            "directive: {:?}",
            serde_json::from_str::<Directive>(&row.2)?
        );
        println!(
            "argument: {:?}",
            row.3
                .as_ref()
                .map(|arg| serde_json::from_str::<Argument>(&arg))
                .transpose()?
        );
        println!(
            "addr: {:?}",
            serde_json::from_str::<AddressModifier>(&row.4)?
        );
        Ok(Self {
            block_name: row.0,
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

#[derive(Queryable, Debug, Identifiable, Associations)]
#[diesel(primary_key(block_name, offset))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
#[diesel(belongs_to(ProgramBlock, foreign_key = block_name))]
pub struct Literal {
    pub block_name: String,
    pub offset: Option<i32>,
    pub value: Vec<u8>,
}

#[derive(Queryable, Debug, Identifiable, Associations)]
#[diesel(primary_key(block_name, offset))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
#[diesel(belongs_to(ProgramBlock, foreign_key = block_name))]
pub struct Ltorg {
    pub block_name: String,
    pub offset: i32,
    pub data: Vec<u8>,
}

#[derive(Queryable, Debug, Identifiable, Selectable, Insertable)]
#[diesel(primary_key(block_name))]
#[diesel(check_for_backend(diesel::sqlite::Sqlite))]
pub struct ProgramBlock {
    pub block_name: String,
    pub current_offset: i32,
}

impl ProgramBlock {
    pub fn new(name: String) -> Self {
        Self {
            block_name: name,
            current_offset: 0,
        }
    }
}
