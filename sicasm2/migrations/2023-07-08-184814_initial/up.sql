CREATE TABLE program_blocks (
  block_name text primary key not null,
  current_offset integer not null
);

CREATE TABLE literals (
  block_name text not null,
  arg_type text not null, -- "String", "Literal"
  arg_string text not null,
  offset integer,
  value blob not null,
  FOREIGN KEY(block_name) REFERENCES program_blocks(block_name),
  PRIMARY KEY (block_name, offset)
);

CREATE TABLE ltorgs (
  block_name text not null,
  offset integer not null,
  data blob not null,
  FOREIGN KEY(block_name) REFERENCES program_blocks(block_name),
  PRIMARY KEY (block_name, offset)
);

CREATE TABLE labels (
  block_name text not null,
  line_no integer not null,
  label_name text not null,
  offset integer not null,
  FOREIGN KEY(block_name) REFERENCES program_blocks(block_name),
  FOREIGN KEY(line_no) REFERENCES lines(line_no),
  PRIMARY KEY (block_name, label_name)
);

CREATE TABLE line (
  block_name text not null,
  line_no integer primary key not null,
  directive text not null,
  argument_type text not null, -- "None", "String", "Literal"
  argument_string text, -- NULL for "None"
  extended integer not null default FALSE,
  size integer not null,
  offest integer not null,
  text text not null,
  FOREIGN KEY(block_name) REFERENCES program_blocks(block_name)
);
