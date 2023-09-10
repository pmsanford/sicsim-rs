#![allow(unused)]
use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    fs,
    io::{self, Stdout},
    rc::Rc,
    sync::{Arc, Mutex},
    time::Duration,
};

use crossterm::{
    event::{self, Event, KeyCode, KeyModifiers},
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
    widgets::{Block, Cell, Paragraph, Row, Table},
    Frame, Terminal,
};

fn main() -> Result<(), Box<dyn Error>> {
    let mut terminal = setup_terminal()?;
    run(&mut terminal)?;
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
                    let (inst_size, target) = {
                        let debugger = debugger.lock().expect("mutex read");
                        let op = debugger.get_next_op(vm);
                        let inst_size = op.map_or(1, |op| op.len());

                        let target = op
                            .map(|op| debugger.find_target_address(vm, &op))
                            .and_then(|(target, _)| target);

                        (inst_size, target)
                    };
                    let coords = address_to_coords(start_addr as u32, vm.PC.as_u32());
                    if let Some((pc_row, pc_idx)) = coords {
                        if idx == pc_row
                            && (pc_idx..pc_idx + inst_size as usize).contains(&cell_idx)
                        {
                            cell = cell.style(
                                Style::default()
                                    .add_modifier(Modifier::BOLD)
                                    .fg(Color::Yellow),
                            );
                        }
                    }
                    if target
                        .and_then(|target| address_to_coords(start_addr as u32, target))
                        .map(|(row, col)| idx == row && (col..col + 3).contains(&cell_idx))
                        .unwrap_or(false)
                    {
                        cell = cell.style(
                            Style::default()
                                .add_modifier(Modifier::BOLD)
                                .fg(Color::Blue),
                        )
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

fn run(terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> Result<(), Box<dyn Error>> {
    let mut start_addr = 0;
    let bytes_displayed = 50 * 16;
    let mut vm = SicXeVm::empty();
    let mut loader = ProgramLoader::new();
    let cont = fs::read_to_string("test.ebj")?;
    let sdb = fs::read_to_string("test.sdb")?;

    let mut debugger = SdbDebugger::new();
    debugger.load(0, sdb).unwrap();
    let labels = debugger.get_labels();

    loader.load_string(&cont, 0);

    loader.copy_all_to(&mut vm);
    vm.set_pc(0);

    let debugger = Arc::new(Mutex::new(debugger));

    vm.debugger = Some(Box::new(debugger.clone()));

    loop {
        terminal.draw(|frame| {
            let pc = vm.PC.as_u32();

            memory_view(frame, &vm, start_addr, bytes_displayed, &debugger);

            symbols_view(frame, &vm, &debugger);

            source_view(frame, &vm, &debugger);

            registers_view(frame, &vm, &debugger);
        })?;
        if event::poll(Duration::from_millis(250))? {
            if let Event::Key(key) = event::read()? {
                if KeyCode::Char('q') == key.code {
                    break;
                }
                if KeyCode::Char('s') == key.code {
                    vm.step();
                }
                if KeyCode::Char('f') == key.code && !key.modifiers.contains(KeyModifiers::CONTROL)
                {
                    start_addr = (start_addr + 16).clamp(0, vm.memory.len() - bytes_displayed);
                }
                if KeyCode::Char('f') == key.code && key.modifiers.contains(KeyModifiers::CONTROL) {
                    start_addr =
                        (start_addr + bytes_displayed).clamp(0, vm.memory.len() - bytes_displayed);
                }
                if KeyCode::Char('b') == key.code && !key.modifiers.contains(KeyModifiers::CONTROL)
                {
                    if start_addr > 16 {
                        start_addr = (start_addr - 16).clamp(0, vm.memory.len() - bytes_displayed);
                    } else {
                        start_addr = 0
                    }
                }
                if KeyCode::Char('b') == key.code && key.modifiers.contains(KeyModifiers::CONTROL) {
                    if start_addr > bytes_displayed {
                        start_addr -= bytes_displayed;
                    } else {
                        start_addr = 0;
                    }
                }
                if KeyCode::Char('F') == key.code {
                    start_addr = (start_addr + bytes_displayed * 16)
                        .clamp(0, vm.memory.len() - bytes_displayed);
                }
                if KeyCode::Char('B') == key.code {
                    if start_addr > bytes_displayed * 16 {
                        start_addr -= bytes_displayed * 16;
                    } else {
                        start_addr = 0;
                    }
                }
                if KeyCode::Char('g') == key.code {
                    start_addr = 0;
                }
                if KeyCode::Char('G') == key.code {
                    start_addr = vm.memory.len() - bytes_displayed;
                }
                if KeyCode::Char('m') == key.code {
                    start_addr = vm.memory.len() / 2;
                }
                if KeyCode::Char('p') == key.code {
                    start_addr = vm.PC.as_u32() as usize;
                }
            }
        }
    }

    Ok(())
}

fn highlight_if(val: String, highlight: bool) -> Cell<'static> {
    let mut cell = Cell::from(val);

    if highlight {
        cell = cell.style(Style::default().fg(Color::Blue));
    }

    cell
}
