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

#[test]
fn test_word_label_vals() {
    let program_text = include_str!("../programs/word_label.sic");
    let assembled = assemble_program(program_text).unwrap();

    insta::assert_yaml_snapshot!(assembled);
}

#[test]
fn test_equ() {
    let program_text = include_str!("../programs/equ.sic");
    let assembled = assemble_program(program_text).unwrap();

    insta::assert_yaml_snapshot!(assembled);
}

#[test]
fn test_relocate() {
    let program_text = include_str!("../programs/relocatable.sic");
    let assembled = assemble_program(program_text).unwrap();

    insta::assert_yaml_snapshot!(assembled);
}
