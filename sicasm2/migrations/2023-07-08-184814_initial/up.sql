CREATE TABLE program_blocks (
  block_id integer primary key not null,
  block_name text not null,
  section_name text not null,
  current_offset integer not null,
  FOREIGN KEY(section_name) REFERENCES control_sections(section_name),
  UNIQUE(section_name, block_name)
);

CREATE TABLE literals (
  block_id integer not null,
  offset integer,
  value blob not null,
  FOREIGN KEY(block_id) REFERENCES program_blocks(block_id),
  PRIMARY KEY (block_id, value)
);

CREATE TABLE ltorgs (
  block_id integer not null,
  offset integer not null,
  data blob not null,
  FOREIGN KEY(block_id) REFERENCES program_blocks(block_id),
  PRIMARY KEY (block_id, offset)
);

CREATE TABLE labels (
  section_name text not null,
  line_no integer not null,
  label_name text not null,
  offset integer not null,
  FOREIGN KEY(section_name) REFERENCES control_sections(section_name),
  FOREIGN KEY(line_no) REFERENCES lines(line_no),
  PRIMARY KEY (label_name)
);

CREATE TABLE lines (
  block_id integer not null,
  line_no integer primary key not null,
  directive text not null,
  argument text,
  address_modifier text not null,
  extended integer not null default FALSE,
  indexed integer not null default FALSE,
  size integer not null,
  offset integer not null,
  text text not null,
  FOREIGN KEY(block_id) REFERENCES program_blocks(block_id)
);

CREATE TABLE control_sections (
  section_name text primary key not null
);
