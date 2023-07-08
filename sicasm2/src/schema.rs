// @generated automatically by Diesel CLI.

diesel::table! {
    labels (block_name, label_name) {
        block_name -> Text,
        line_no -> Integer,
        label_name -> Text,
        offset -> Integer,
    }
}

diesel::table! {
    line (line_no) {
        block_name -> Text,
        line_no -> Integer,
        directive -> Text,
        argument_type -> Text,
        argument_string -> Nullable<Text>,
        extended -> Integer,
        size -> Integer,
        offest -> Integer,
        text -> Text,
    }
}

diesel::table! {
    literals (block_name, offset) {
        block_name -> Text,
        arg_type -> Text,
        arg_string -> Text,
        offset -> Nullable<Integer>,
        value -> Binary,
    }
}

diesel::table! {
    ltorgs (block_name, offset) {
        block_name -> Text,
        offset -> Integer,
        data -> Binary,
    }
}

diesel::table! {
    program_blocks (block_name) {
        block_name -> Text,
        current_offset -> Integer,
    }
}

diesel::joinable!(labels -> program_blocks (block_name));
diesel::joinable!(line -> program_blocks (block_name));
diesel::joinable!(literals -> program_blocks (block_name));
diesel::joinable!(ltorgs -> program_blocks (block_name));

diesel::allow_tables_to_appear_in_same_query!(labels, line, literals, ltorgs, program_blocks,);
