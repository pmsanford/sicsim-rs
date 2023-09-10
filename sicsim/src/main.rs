#![allow(unused)]
use std::{
    cell::RefCell,
    collections::HashMap,
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
    widgets::{Block, Borders, Cell, Clear, Paragraph, Row, Table},
    Frame, Terminal,
};

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

fn address_to_coords(start_addr: u32, address: u32) -> Option<(usize, usize)> {
    if start_addr > address {
        return None;
    }
    let address = address - start_addr;
    let row = address / 16;
    let idx = address % 16 + 1;

    Some((row as usize, idx as usize))
}

fn memory_view(
    frame: &mut Frame<CrosstermBackend<Stdout>>,
    vm: &SicXeVm,
    start_addr: usize,
    bytes_displayed: usize,
    debugger: &Mutex<SdbDebugger>,
) {
    let mut highlights = HashMap::new();
    let (inst_size, target) = {
        let debugger = debugger.lock().expect("mutex read");
        let op = debugger.get_next_op(vm);
        let inst_size = op.map_or(1, |op| op.len());

        let target = op
            .map(|op| debugger.find_target_address(vm, &op))
            .and_then(|(target, _)| target);

        (inst_size, target)
    };

    if let Some(coords) = address_to_coords(start_addr as u32, vm.PC.as_u32()) {
        highlights.insert(coords, Color::Yellow);
        for i in 0..inst_size - 1 {
            highlights.insert(
                address_to_coords(start_addr as u32, vm.PC.as_u32() + i + 1).unwrap(),
                Color::Yellow,
            );
        }
    }

    if let Some(target) = target {
        if let Some(coords) = address_to_coords(start_addr as u32, target) {
            highlights.insert(coords, Color::Blue);
            highlights.insert(
                address_to_coords(start_addr as u32, target + 1).unwrap(),
                Color::Blue,
            );
            highlights.insert(
                address_to_coords(start_addr as u32, target + 2).unwrap(),
                Color::Blue,
            );
        }
    }

    let mut rows = vm.memory[start_addr..start_addr + bytes_displayed]
        .iter()
        .map(|b| format!("{:0>2X}", b))
        .collect::<Vec<_>>()
        .chunks(16)
        .enumerate()
        .map(|(idx, c)| {
            let mut items = Vec::from(c);
            items.insert(0, format!("0x{:0>4X}", idx * 16 + start_addr));
            let items = items
                .into_iter()
                .enumerate()
                .map(|(cell_idx, val)| {
                    let mut cell = Cell::from(val);
                    let coords = address_to_coords(start_addr as u32, vm.PC.as_u32());
                    if let Some(color) = coords.and_then(|coords| highlights.get(&(idx, cell_idx)))
                    {
                        cell = cell.style(Style::default().fg(*color));
                    }
                    cell
                })
                .collect::<Vec<_>>();
            Row::new(items)
        })
        .collect::<Vec<_>>();

    let mut header_values = (0..16).map(|c| format!("{: >2X}", c)).collect::<Vec<_>>();
    header_values.insert(0, "".to_owned());
    let first_row = Row::new(header_values).bottom_margin(1);
    rows.insert(0, first_row);

    let mut widths = vec![Constraint::Length(2); 16];
    widths.insert(0, Constraint::Length(7));

    let memory = Table::new(rows)
        .style(Style::default().fg(Color::White))
        .widths(&widths)
        .column_spacing(1);
    let area = Rect::new(0, 0, 75, 50);
    frame.render_widget(memory, area);
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

fn symbols_view(
    frame: &mut Frame<CrosstermBackend<Stdout>>,
    vm: &SicXeVm,
    debugger: &Mutex<SdbDebugger>,
) {
    let labels = debugger.lock().expect("read mutex").get_labels();
    let mut flattened = labels
        .iter()
        .flat_map(|(k, v)| {
            v.iter()
                .map(|(ik, iv)| (format!("{}-{}", ik, k), *iv))
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    flattened.sort_by_key(|i| i.0.clone());

    let mut rows = flattened
        .iter()
        .map(|(k, v)| {
            Row::new(vec![
                k.clone(),
                format!("{:0>6X}", vm.word_at(*v).unwrap().as_u32()),
            ])
        })
        .collect::<Vec<_>>();
    rows.insert(
        0,
        Row::new(vec!["Symbol".to_owned(), "Value".to_owned()]).bottom_margin(1),
    );

    let widths = vec![Constraint::Length(13); 2];
    let symbols = Table::new(rows).widths(&widths);
    let area = Rect::new(60, 13, 34, 35);
    frame.render_widget(symbols, area);
}

fn input_view(frame: &mut Frame<CrosstermBackend<Stdout>>, current_input: &str, title: &str) {
    let rect = Rect::new(10, 20, 74, 3);
    let paragraph = Paragraph::new(current_input)
        .block(Block::new().borders(Borders::ALL).title(title))
        .style(Style::default().bg(Color::Black));
    frame.render_widget(Clear, rect);
    frame.render_widget(paragraph, rect);
}

fn registers_view(
    frame: &mut Frame<CrosstermBackend<Stdout>>,
    vm: &SicXeVm,
    debugger: &Mutex<SdbDebugger>,
) {
    let rows = {
        let debugger = debugger.lock().expect("read mutex");
        let diff = debugger.last_change();
        vec![
            Row::new(vec![
                Cell::from("A".to_owned()),
                highlight_if(format!("{:0>6X}", vm.A.as_u32()), diff.A.is_some()),
            ]),
            Row::new(vec![
                Cell::from("X".to_owned()),
                highlight_if(format!("{:0>6X}", vm.X.as_u32()), diff.X.is_some()),
            ]),
            Row::new(vec![
                Cell::from("L".to_owned()),
                highlight_if(format!("{:0>6X}", vm.L.as_u32()), diff.L.is_some()),
            ]),
            Row::new(vec![
                Cell::from("B".to_owned()),
                highlight_if(format!("{:0>6X}", vm.B.as_u32()), diff.B.is_some()),
            ]),
            Row::new(vec![
                Cell::from("S".to_owned()),
                highlight_if(format!("{:0>6X}", vm.S.as_u32()), diff.S.is_some()),
            ]),
            Row::new(vec![
                Cell::from("T".to_owned()),
                highlight_if(format!("{:0>6X}", vm.T.as_u32()), diff.T.is_some()),
            ]),
            Row::new(vec![
                Cell::from("F".to_owned()),
                highlight_if(format!("{:0>12X}", vm.F.as_u64()), diff.F.is_some()),
            ]),
            Row::new(vec![
                Cell::from("PC".to_owned()),
                highlight_if(format!("{:0>6X}", vm.PC.as_u32()), diff.pc_jumped()),
            ]),
            Row::new(vec![
                Cell::from("SW".to_owned()),
                highlight_if(format!("{:0>6X}", vm.SW.as_u32()), diff.SW.is_some()),
            ]),
            Row::new(vec![
                Cell::from("I".to_owned()),
                highlight_if(format!("{:0>6X}", vm.I.as_u32()), diff.I.is_some()),
            ]),
        ]
    };

    let widths = vec![Constraint::Length(2), Constraint::Length(12)];
    let registers = Table::new(rows).widths(&widths);
    let area = Rect::new(60, 0, 34, 50);
    frame.render_widget(registers, area);
}

fn debug_view(frame: &mut Frame<CrosstermBackend<Stdout>>, val: String) {
    let paragraph = Paragraph::new(val);
    frame.render_widget(paragraph, Rect::new(60, 30, 30, 1));
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

                symbols_view(frame, &self.vm, &self.debugger);

                source_view(frame, &self.vm, &self.debugger);

                registers_view(frame, &self.vm, &self.debugger);

                if self.mode == InputMode::Jump {
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
                        InputMode::Jump => {
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
                    _ => {}
                }
                self.mode = InputMode::Step;
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
        false
    }
}

fn highlight_if(val: String, highlight: bool) -> Cell<'static> {
    let mut cell = Cell::from(val);

    if highlight {
        cell = cell.style(Style::default().fg(Color::Blue));
    }

    cell
}
