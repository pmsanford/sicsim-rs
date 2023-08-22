// @generated automatically by Diesel CLI.

diesel::table! {
    control_sections (section_name) {
        section_name -> Text,
    }
}

diesel::table! {
    labels (label_name) {
        section_name -> Text,
        line_no -> Integer,
        label_name -> Text,
        offset -> Integer,
    }
}

diesel::table! {
    lines (line_no) {
        block_id -> Integer,
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
    literals (block_id, value) {
        block_id -> Integer,
        offset -> Nullable<Integer>,
        value -> Binary,
    }
}

diesel::table! {
    ltorgs (block_id, offset) {
        block_id -> Integer,
        offset -> Integer,
        data -> Binary,
    }
}

diesel::table! {
    program_blocks (block_id) {
        block_id -> Integer,
        block_name -> Text,
        section_name -> Text,
        current_offset -> Integer,
        start_offset -> Nullable<Integer>,
    }
}

diesel::joinable!(labels -> control_sections (section_name));
diesel::joinable!(labels -> lines (line_no));
diesel::joinable!(lines -> program_blocks (block_id));
diesel::joinable!(literals -> program_blocks (block_id));
diesel::joinable!(ltorgs -> program_blocks (block_id));
diesel::joinable!(program_blocks -> control_sections (section_name));

diesel::allow_tables_to_appear_in_same_query!(
    control_sections,
    labels,
    lines,
    literals,
    ltorgs,
    program_blocks,
);
