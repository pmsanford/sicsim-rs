CREATE TABLE program_blocks (
  block_name text primary key not null,
  current_offset integer not null
);

CREATE TABLE literals (
  block_name text not null,
  offset integer,
  value blob not null,
  FOREIGN KEY(block_name) REFERENCES program_blocks(block_name),
  PRIMARY KEY (block_name, value)
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
  PRIMARY KEY (label_name)
);

CREATE TABLE lines (
  block_name text not null,
  line_no integer primary key not null,
  directive text not null,
  argument text,
  address_modifier text not null,
  extended integer not null default FALSE,
  indexed integer not null default FALSE,
  size integer not null,
  offset integer not null,
  text text not null,
  FOREIGN KEY(block_name) REFERENCES program_blocks(block_name)
);
