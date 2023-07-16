// @generated automatically by Diesel CLI.

diesel::table! {
    labels (label_name) {
        block_name -> Text,
        line_no -> Integer,
        label_name -> Text,
        offset -> Integer,
    }
}

diesel::table! {
    lines (line_no) {
        block_name -> Text,
        line_no -> Integer,
        directive -> Text,
        argument -> Nullable<Text>,
        address_modifier -> Text,
        extended -> Integer,
        indexed -> Integer,
        size -> Integer,
        offset -> Integer,
        text -> Text,
    }
}

diesel::table! {
    literals (block_name, value) {
        block_name -> Text,
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

diesel::joinable!(labels -> lines (line_no));
diesel::joinable!(labels -> program_blocks (block_name));
diesel::joinable!(lines -> program_blocks (block_name));
diesel::joinable!(literals -> program_blocks (block_name));
diesel::joinable!(ltorgs -> program_blocks (block_name));

diesel::allow_tables_to_appear_in_same_query!(labels, lines, literals, ltorgs, program_blocks,);
