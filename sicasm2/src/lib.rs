pub mod parser;

#[cfg(test)]
mod tests {
    use super::parser::*;

    #[test]
    fn parse_line() {
        let test_line = "FST     LDA    SND";

        assert!(asm_line(test_line).is_ok());

        let test_line = "FST    +LDA    SND";

        assert!(asm_line(test_line).is_ok());

        let test_line = "FST     LDA   #SND";

        assert!(asm_line(test_line).is_ok());

        let test_line = "FST     LDA   @SND";

        assert!(asm_line(test_line).is_ok());

        let test_line = "FST     LDA   SND,X";

        assert!(asm_line(test_line).is_ok());

        let test_line = "        LDA   SND,X";

        assert!(asm_line(test_line).is_ok());
    }

    #[test]
    fn parse_program() {
        let program = r#"
DISP    START   100
        LDA    #5
LOOP    MUL    #2
        COMP   @COMPLOC
        JGT     END
END     J       END
COMPLOC WORD    MAX
MAX     WORD    1000
        END     DISP
        "#;

        program
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(asm_line)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
    }
}
