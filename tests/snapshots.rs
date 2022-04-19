use sicasm::assemble_program;

#[test]
fn test_ltorg() {
    let program_text = include_str!("../programs/ltorg.sic");
    let assembled = assemble_program(program_text).unwrap();

    insta::assert_yaml_snapshot!(assembled);
}
