# sicsim-rs
A work-in-progress Rust simulator for the Simple Instructional Computer created by Leland Beck. Time spent on this comes in waves, but I do intend to eventually finish it to the book's specification.

# Components
## libsic
Implementation of the SIC and SIC/XE virtual machines. The SIC implementation is complete; The SIC/XE implementation supports all features of all existing simulators I'm aware of. Currently working on implementing interrupts and IO channels.

## sicasm
A very hacky SIC assembler. I started implementing this as a temporary aid to implementing the VM; I've since realized this project is going to require a more extensible assembler to aid development.

## sicasm2
A WIP reimplementation of the above assembler.

## sicdbg
Definitions for debugging symbol formats emitted by the assembler and shared by other components to allow linking executed instructions to source lines.

## sickos
A WIP operating system for SIC/XE that will include a dispatcher and program loader. Being developed in tandem with interrupts and IO channels. Also includes a test harness to run the operating system using libsic.

## sicsim
A binary to run the simulator. Currently just stubs.
