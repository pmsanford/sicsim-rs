use anyhow::Result;
use diesel::{QueryDsl, RunQueryDsl};
use libsic::xe::op::VariableOp;
use sicasm2::{
    data::AsmData,
    models::Line,
    parser::{Argument, Directive, Op::Variable},
};

fn main() -> Result<()> {
    dotenvy::dotenv()?;
    let mut data = AsmData::from_env()?;
    data.add_program_block("test".into())?;

    let block = data.get_program_block("test")?;
    println!("Block: {:#?}", block);

    let line = Line {
        block_name: "test".into(),
        line_no: 1,
        directive: Directive::Op(Variable(VariableOp::ADD)),
        argument: Some(Argument::Value(sicasm2::parser::Value::Constant(vec![5]))),
        address_modifier: sicasm2::parser::AddressModifier::Immediate,
        extended: false,
        indexed: false,
        size: 3,
        offset: 0,
        text: "    ADD    #5".into(),
    };

    let dir = Directive::Op(Variable(VariableOp::ADD));
    let dir_ser = serde_json::to_string(&dir)?;
    println!("ser: {}", dir_ser);
    println!(
        "de: {:?}",
        serde_json::from_str::<Directive>(r#"{"Op": 24}"#)
    );

    diesel::insert_into(sicasm2::schema::lines::table)
        .values(line)
        .execute(&mut data.conn)?;
    let line: Line = sicasm2::schema::lines::dsl::lines
        .find(1)
        .get_result(&mut data.conn)?;

    println!("Roundtripped line: {:#?}", line);

    Ok(())
}
