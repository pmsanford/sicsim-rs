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
    word::{u32_to_word, DWordExt},
    xe::{
        debugger::{SdbConsoleDebugger, SdbDebugger},
        io_channel::{FileIODevice, IOChannel},
        load::ProgramLoader,
        vm::SicXeVm,
    },
    WordExt,
};
use ratatui::{
    prelude::{Constraint, CrosstermBackend, Rect},
    style::{Color, Modifier, Style},
    text::{Span, Text},
    widgets::{Block, Borders, Cell, Clear, List, ListItem, Paragraph, Row, Table, Tabs},
    Frame, Terminal,
};

use memory::memory_view;
use registers::registers_view;
use symbols::{symbols_view, SymbolsPanel};

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

fn source_line_view(
    frame: &mut Frame<CrosstermBackend<Stdout>>,
    vm: &SicXeVm,
    debugger: &Mutex<SdbDebugger>,
) {
    let (program, line) = {
        let debugger = debugger.lock().expect("read mutex");
        let program = debugger.get_program(vm.PC.as_u32());
        let line = debugger.find_line_for(vm.PC.as_u32()).clone();
        (program, line)
    };

    let program = Paragraph::new(format!(
        "Program: {}",
        program
            .map(|prog| prog.sdb.name)
            .unwrap_or_else(String::new)
    ));
    let area = Rect::new(60, 11, 34, 1);
    frame.render_widget(program, area);

    let source = Paragraph::new(
        line.map(|line| format!("{: >3}: {}", line.line_number, line.text))
            .unwrap_or_else(String::new),
    );
    let area = Rect::new(60, 12, 34, 1);
    frame.render_widget(source, area);
}

fn target_view(
    frame: &mut Frame<CrosstermBackend<Stdout>>,
    vm: &SicXeVm,
    debugger: &Mutex<SdbDebugger>,
) {
    let (target, indirect) = {
        let debugger = debugger.lock().expect("read mutex");
        let Some(op) = debugger.get_next_op(vm) else {
            return;
        };
        let (target, indirect) = debugger.find_target_address(vm, &op);
        (target, indirect)
    };
    let target_text = target
        .map(|target| format!("{:0>6X}", target))
        .unwrap_or_else(String::new);
    let indir_text = indirect
        .map(|indirect| format!("{:0>6X} -> ", indirect))
        .unwrap_or_else(String::new);
    if !target_text.is_empty() {
        let paragraph = Paragraph::new(format!("Target: {}{}", indir_text, target_text));
        let area = Rect::new(60, 13, 34, 1);
        frame.render_widget(paragraph, area);
    }
}

fn source_view(
    frame: &mut Frame<CrosstermBackend<Stdout>>,
    vm: &SicXeVm,
    debugger: &Mutex<SdbDebugger>,
) {
    let width = frame.size().width;
    if width >= 95 + 40 {
        let (program, line) = {
            let debugger = debugger.lock().expect("read mutex");
            let program = debugger.get_program(vm.PC.as_u32());
            let line = debugger.find_line_for(vm.PC.as_u32()).clone();
            (program, line)
        };

        if let Some(program) = program {
            let current_line = line.map(|line| line.line_number);
            let lines = program
                .sdb
                .lines
                .iter()
                .map(|line| {
                    (
                        line.line_number,
                        format!("{: >3}: {}", line.line_number, line.text),
                    )
                })
                .map(|(line_number, line)| {
                    let li = ListItem::new(line);
                    if current_line.map(|ln| ln == line_number).unwrap_or(false) {
                        li.style(Style::default().fg(Color::Red))
                    } else {
                        li
                    }
                })
                .collect::<Vec<_>>();
            let list = List::new(lines);
            let area = Rect::new(95, 0, 40, 50);
            frame.render_widget(list, area)
        }
    }
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
    let mut path = PathBuf::from("./");
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
    WatchName,
    WatchAddr,
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
    symbol_panel: SymbolsPanel,
    watch_name: String,
}

impl<'a> App<'a> {
    fn new(terminal: &'a mut Terminal<CrosstermBackend<Stdout>>) -> Self {
        let mut vm = SicXeVm::empty();
        let mut loader = ProgramLoader::new();

        let mut ioc0 = IOChannel::new(0);
        let mut ioc1 = IOChannel::new(1);

        let read_device = FileIODevice::new("./from.txt");
        ioc0.add_device(0, Box::new(read_device));
        let write_device = FileIODevice::new("./to.txt");
        ioc1.add_device(0, Box::new(write_device));

        vm.add_io_channel(0, ioc0);
        vm.add_io_channel(1, ioc1);

        let mut debugger = SdbDebugger::new();
        load_program(&mut debugger, &mut loader, "copy_io_channel", 0x500);
        load_program(&mut debugger, &mut loader, "noop_io_interrupt", 0x1000);

        vm.memory[0x193..0x196].copy_from_slice(&u32_to_word(0x1000));
        vm.memory[0x190] = 0x80;

        loader.copy_all_to(&mut vm);

        // Set interrupt timer at 10 sec
        //vm.I = [0, 0, 10];
        // Need to start in privileged mode
        vm.SW[0] |= 0x80;
        vm.set_pc(0x500);
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
            symbol_panel: SymbolsPanel::new(programs, labels),
            watch_name: String::new(),
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

                source_line_view(frame, &self.vm, &self.debugger);

                source_view(frame, &self.vm, &self.debugger);

                target_view(frame, &self.vm, &self.debugger);

                registers_view(frame, &self.vm, &self.debugger);

                self.symbol_panel.render(frame, &self.vm);

                if self.mode == InputMode::Jump
                    || self.mode == InputMode::Breakpoint
                    || self.mode == InputMode::CycleLimit
                    || self.mode == InputMode::WatchName
                    || self.mode == InputMode::WatchAddr
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
                        InputMode::Jump
                        | InputMode::Breakpoint
                        | InputMode::CycleLimit
                        | InputMode::WatchName
                        | InputMode::WatchAddr => {
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
                    InputMode::WatchName => {
                        self.watch_name = self.current_input.clone();
                        self.input_title = "Watch address".into();
                        self.current_input = String::default();
                        self.mode = InputMode::WatchAddr;
                        return;
                    }
                    InputMode::WatchAddr => {
                        if let Ok(new_addr) = u32::from_str_radix(&self.current_input, 16) {
                            self.symbol_panel
                                .add_watch(self.watch_name.clone(), new_addr);
                            self.watch_name = String::default();
                        } else {
                            return;
                        }
                    }
                }
                self.mode = InputMode::Step;
            }
            _ => {}
        }
    }

    fn handle_symbol_tab(&mut self, key: KeyEvent) {
        match key.code {
            KeyCode::Tab | KeyCode::Esc => {
                self.symbol_panel.cancel();
                self.symbol_panel.set_active(false);
                self.mode = InputMode::Step;
            }
            KeyCode::Char('f') => {
                self.symbol_panel.start();
            }
            KeyCode::Char('e') => {
                self.symbol_panel.edit();
            }
            KeyCode::Char('n') => {
                self.symbol_panel.cancel();
                self.symbol_panel.set_active(false);
                self.input_title = "Watch name".into();
                self.current_input = String::default();
                self.watch_name = String::default();
                self.mode = InputMode::WatchName;
            }
            KeyCode::Up => {
                self.symbol_panel.previous();
            }
            KeyCode::Down => {
                self.symbol_panel.next();
            }
            KeyCode::Enter => {
                self.symbol_panel.select();
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
            self.symbol_panel.set_active(true);
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
