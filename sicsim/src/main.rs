#![allow(unused)]
mod memory;
mod registers;
mod symbols;
use std::{
    cell::RefCell,
    collections::{BTreeMap, HashMap, HashSet},
    error::Error,
    fs,
    io::{self, Stdout},
    path::PathBuf,
    rc::Rc,
    sync::{Arc, Mutex},
    time::Duration,
};

use crossterm::{
    event::{self, Event, KeyCode, KeyEvent, KeyModifiers},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use itertools::Itertools;
use libsic::{
    word::DWordExt,
    xe::{
        debugger::{SdbConsoleDebugger, SdbDebugger},
        load::ProgramLoader,
        vm::SicXeVm,
    },
    WordExt,
};
use ratatui::{
    prelude::{Constraint, CrosstermBackend, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Text},
    widgets::{Block, Borders, Cell, Clear, Paragraph, Row, Table, Tabs},
    Frame, Terminal,
};

use memory::memory_view;
use registers::registers_view;
use symbols::symbols_view;

fn main() -> Result<(), Box<dyn Error>> {
    let mut terminal = setup_terminal()?;
    let mut app = App::new(&mut terminal);
    app.run()?;
    restore_terminal(&mut terminal)?;
    Ok(())
}

fn setup_terminal() -> Result<Terminal<CrosstermBackend<Stdout>>, Box<dyn Error>> {
    let mut stdout = io::stdout();
    enable_raw_mode()?;
    execute!(stdout, EnterAlternateScreen)?;
    Ok(Terminal::new(CrosstermBackend::new(stdout))?)
}

fn restore_terminal(
    terminal: &mut Terminal<CrosstermBackend<Stdout>>,
) -> Result<(), Box<dyn Error>> {
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen,)?;
    Ok(terminal.show_cursor()?)
}

fn source_view(
    frame: &mut Frame<CrosstermBackend<Stdout>>,
    vm: &SicXeVm,
    debugger: &Mutex<SdbDebugger>,
) {
    let line = {
        let debugger = debugger.lock().expect("read mutex");
        let line = debugger.find_line_for(vm.PC.as_u32());
        line.map(|line| line.text.clone())
            .unwrap_or_else(String::new)
    };

    let source = Paragraph::new(line);
    let area = Rect::new(60, 11, 34, 1);
    frame.render_widget(source, area);
}

fn input_view(frame: &mut Frame<CrosstermBackend<Stdout>>, current_input: &str, title: &str) {
    let rect = Rect::new(10, 20, 74, 3);
    let paragraph = Paragraph::new(current_input)
        .block(Block::new().borders(Borders::ALL).title(title))
        .style(Style::default().bg(Color::Black));
    frame.render_widget(Clear, rect);
    frame.render_widget(paragraph, rect);
}

fn load_program(debugger: &mut SdbDebugger, loader: &mut ProgramLoader, name: &str, load_at: u32) {
    let mut path = PathBuf::from("../sickos/src/bin/");
    path.push(name);
    let program = fs::read_to_string(path.with_extension("ebj")).unwrap();
    let program_sdb = fs::read_to_string(path.with_extension("sdb")).unwrap();
    loader.load_string(&program, load_at);
    debugger.load(load_at, program_sdb).unwrap();
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum InputMode {
    Step,
    Jump,
    Breakpoint,
    CycleLimit,
    Symbols,
}

struct App<'a> {
    start_addr: usize,
    bytes_displayed: usize,
    vm: SicXeVm,
    debugger: Arc<Mutex<SdbDebugger>>,
    terminal: &'a mut Terminal<CrosstermBackend<Stdout>>,
    mode: InputMode,
    current_input: String,
    input_title: String,
    cycle_limit: u64,
    breakpoints: HashSet<u32>,
    programs: Vec<String>,
    labels: BTreeMap<String, BTreeMap<String, u32>>,
    symbol_index: usize,
}

impl<'a> App<'a> {
    fn new(terminal: &'a mut Terminal<CrosstermBackend<Stdout>>) -> Self {
        let mut vm = SicXeVm::empty();
        let mut loader = ProgramLoader::new();

        let mut debugger = SdbDebugger::new();
        load_program(&mut debugger, &mut loader, "bootloader", 0x0);

        load_program(&mut debugger, &mut loader, "work_areas", 0x100);

        load_program(&mut debugger, &mut loader, "program_int", 0x30);

        load_program(&mut debugger, &mut loader, "dispatcher", 0x200);

        load_program(&mut debugger, &mut loader, "wake_counter", 0x7D0);

        load_program(&mut debugger, &mut loader, "wake_counter", 0x7E0);

        load_program(&mut debugger, &mut loader, "wake_counter", 0x7F0);

        loader.copy_all_to(&mut vm);

        // Set interrupt timer at 10 sec
        vm.I = [0, 0, 10];
        // Need to start in privileged mode
        vm.SW[0] |= 0x80;
        vm.set_pc(0);
        let labels = debugger.get_labels();
        let programs = labels
            .iter()
            .flat_map(|(_, mp)| mp.keys())
            .unique()
            .sorted()
            .cloned()
            .collect::<Vec<_>>();
        let labels = programs
            .iter()
            .cloned()
            .map(|prg| {
                (
                    prg.clone(),
                    labels
                        .iter()
                        .filter(|(_, v)| v.contains_key(&prg))
                        .map(|(label, v)| (label.clone(), v[&prg]))
                        .collect::<BTreeMap<_, _>>(),
                )
            })
            .collect::<BTreeMap<_, _>>();

        let debugger = Arc::new(Mutex::new(debugger));

        vm.debugger = Some(Box::new(debugger.clone()));

        Self {
            start_addr: 0,
            bytes_displayed: 50 * 16,
            vm,
            debugger,
            terminal,
            mode: InputMode::Step,
            current_input: String::new(),
            input_title: String::new(),
            cycle_limit: 10000,
            breakpoints: HashSet::new(),
            symbol_index: 0,
            programs,
            labels,
        }
    }

    fn follow_pc(&mut self) {
        let pc = self.vm.PC.as_u32() as usize;
        let upper = self.start_addr + self.bytes_displayed - 4;

        if pc > upper || pc < self.start_addr {
            self.start_addr = pc - pc % 16;
        }
    }

    fn run(&mut self) -> Result<(), Box<dyn Error>> {
        loop {
            self.terminal.draw(|frame| {
                let pc = self.vm.PC.as_u32();

                memory_view(
                    frame,
                    &self.vm,
                    self.start_addr,
                    self.bytes_displayed,
                    &self.debugger,
                );

                symbols_view(
                    frame,
                    &self.vm,
                    &self.debugger,
                    &self.programs,
                    &self.labels,
                    self.symbol_index,
                );

                source_view(frame, &self.vm, &self.debugger);

                registers_view(frame, &self.vm, &self.debugger);

                if self.mode == InputMode::Jump
                    || self.mode == InputMode::Breakpoint
                    || self.mode == InputMode::CycleLimit
                {
                    input_view(frame, &self.current_input, &self.input_title);
                }
            })?;
            if event::poll(Duration::from_millis(250))? {
                if let Event::Key(key) = event::read()? {
                    match self.mode {
                        InputMode::Step => {
                            if self.handle_step_mode(key) {
                                break;
                            }
                        }
                        InputMode::Symbols => {
                            self.handle_symbol_tab(key);
                        }
                        InputMode::Jump | InputMode::Breakpoint | InputMode::CycleLimit => {
                            self.handle_input(key);
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn handle_input(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Char(c) => {
                self.current_input += &c.to_string();
            }
            KeyCode::Backspace => {
                if !self.current_input.is_empty() {
                    self.current_input = self.current_input[..self.current_input.len() - 1].into();
                }
            }
            KeyCode::Esc => {
                self.current_input = String::new();
                self.mode = InputMode::Step;
            }
            KeyCode::Enter => {
                match self.mode {
                    InputMode::Jump => {
                        if let Ok(new_addr) = usize::from_str_radix(&self.current_input, 16) {
                            self.start_addr = new_addr - new_addr % 16;
                        } else {
                            return;
                        }
                    }
                    InputMode::Breakpoint => {
                        if let Ok(new_addr) = u32::from_str_radix(&self.current_input, 16) {
                            self.breakpoints.insert(new_addr);
                        } else {
                            return;
                        }
                    }
                    InputMode::CycleLimit => {
                        if let Ok(new_limit) = self.current_input.parse::<u64>() {
                            self.cycle_limit = new_limit;
                        } else {
                            return;
                        }
                    }
                    InputMode::Symbols => {}
                    InputMode::Step => {}
                }
                self.mode = InputMode::Step;
            }
            _ => {}
        }
    }

    fn handle_symbol_tab(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Tab => {
                self.mode = InputMode::Step;
            }
            KeyCode::Left => {
                if self.symbol_index > 0 {
                    self.symbol_index -= 1;
                }
            }
            KeyCode::Right => {
                if self.symbol_index < self.programs.len() - 1 {
                    self.symbol_index += 1;
                }
            }
            _ => {}
        }
    }

    fn handle_step_mode(&mut self, key: KeyEvent) -> bool {
        if KeyCode::Char('q') == key.code {
            return true;
        }
        if KeyCode::Char('s') == key.code {
            self.vm.step();
            self.follow_pc();
        }
        if KeyCode::Tab == key.code {
            self.mode = InputMode::Symbols
        }
        if KeyCode::Char('f') == key.code && !key.modifiers.contains(KeyModifiers::CONTROL) {
            self.start_addr =
                (self.start_addr + 16).clamp(0, self.vm.memory.len() - self.bytes_displayed);
        }
        if KeyCode::Char('f') == key.code && key.modifiers.contains(KeyModifiers::CONTROL) {
            self.start_addr = (self.start_addr + self.bytes_displayed)
                .clamp(0, self.vm.memory.len() - self.bytes_displayed);
        }
        if KeyCode::Char('b') == key.code && !key.modifiers.contains(KeyModifiers::CONTROL) {
            if self.start_addr > 16 {
                self.start_addr =
                    (self.start_addr - 16).clamp(0, self.vm.memory.len() - self.bytes_displayed);
            } else {
                self.start_addr = 0
            }
        }
        if KeyCode::Char('j') == key.code {
            self.current_input = String::new();
            self.input_title = "Jump to".into();
            self.mode = InputMode::Jump;
        }
        if KeyCode::Char('S') == key.code {
            self.current_input = String::new();
            self.input_title = "Set breakpoint".into();
            self.mode = InputMode::Breakpoint;
        }
        if KeyCode::Char('C') == key.code {
            self.breakpoints.clear();
        }
        if KeyCode::Char('c') == key.code {
            self.current_input = format!("{}", self.cycle_limit);
            self.input_title = "Set cycle limit".into();
            self.mode = InputMode::CycleLimit;
        }
        if KeyCode::Char('b') == key.code && key.modifiers.contains(KeyModifiers::CONTROL) {
            if self.start_addr > self.bytes_displayed {
                self.start_addr -= self.bytes_displayed;
            } else {
                self.start_addr = 0;
            }
        }
        if KeyCode::Char('F') == key.code {
            self.start_addr = (self.start_addr + self.bytes_displayed * 16)
                .clamp(0, self.vm.memory.len() - self.bytes_displayed);
        }
        if KeyCode::Char('B') == key.code {
            if self.start_addr > self.bytes_displayed * 16 {
                self.start_addr -= self.bytes_displayed * 16;
            } else {
                self.start_addr = 0;
            }
        }
        if KeyCode::Char('g') == key.code {
            self.start_addr = 0;
        }
        if KeyCode::Char('G') == key.code {
            self.start_addr = self.vm.memory.len() - self.bytes_displayed;
        }
        if KeyCode::Char('m') == key.code {
            self.start_addr = self.vm.memory.len() / 2;
        }
        if KeyCode::Char('p') == key.code {
            let pc = self.vm.PC.as_u32() as usize;
            self.start_addr = pc - pc % 16;
        }
        if KeyCode::Char('r') == key.code {
            for _ in 0..self.cycle_limit {
                if self.breakpoints.contains(&self.vm.PC.as_u32()) {
                    break;
                }
                self.vm.step();
            }
            self.follow_pc();
        }
        false
    }
}
