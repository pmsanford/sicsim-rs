# sicsim-rs
A work-in-progress Rust simulator for the Simple Instructional Computer created by Leland Beck. Time spent on this comes in waves, but I do intend to eventually finish it to the book's specification.

## Components
### libsic
Implementation of the SIC and SIC/XE virtual machines. The SIC implementation is complete; The SIC/XE implementation supports all features of all existing simulators I'm aware of. Currently working on implementing interrupts and IO channels, but I have decided the easiest way to do that is implement an entire operating system (see sickos) so it might be a while.

### sicasm
A very hacky SIC assembler. I started implementing this as a temporary aid to implementing the VM; I've since realized this project is going to require a more extensible assembler to aid development.

### sicasm2
A reimplementation of the above assembler using a parser combinator approach to parsing and storing intermediate state in a sqlite database.

### sicdbg
Definitions for debugging symbol formats emitted by the assembler and shared by other components to allow linking executed instructions to source lines.

### sickos
A WIP operating system for SIC/XE that will include a dispatcher and program loader. Being developed in tandem with interrupts and IO channels. Also includes a test harness to run the operating system using libsic.

### siclsp
A language server implementatin for SIC source files. Currently only supports semantic tokens
![image](https://github.com/pmsanford/sicsim-rs/assets/1696007/b30975a6-bd39-4eff-bec3-158fbda54282)

### sicsim
An early visual simulator that allows stepping through the code, visualizing the memory, setting watches and breakpoints, and following the executing through multiple loaded programs:
![sicsim-demo](https://github.com/pmsanford/sicsim-rs/assets/1696007/354611e9-5c3b-4f04-a9db-4a19639c83a2)
