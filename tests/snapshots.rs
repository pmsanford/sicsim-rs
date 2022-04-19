use sicasm::assemble_program;

#[test]
fn test_ltorg() {
    let program_text = include_str!("../programs/ltorg.sic");
    let assembled = assemble_program(program_text).unwrap();

    insta::assert_yaml_snapshot!(assembled);
}

#[test]
fn test_copy_xe() {
    let program_text = include_str!("../programs/copy.sic");
    let assembled = assemble_program(program_text).unwrap();

    insta::assert_yaml_snapshot!(assembled);
}

#[test]
fn test_org() {
    let program_text = include_str!("../programs/org.sic");
    let assembled = assemble_program(program_text).unwrap();

    insta::assert_yaml_snapshot!(assembled);
}
