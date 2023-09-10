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
    event::{self, Event, KeyCode},
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
    Terminal,
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

fn address_to_coords(address: u32) -> (usize, usize) {
    let row = address / 16;
    let idx = address % 16 + 1;

    (row as usize, idx as usize)
}

fn run(terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> Result<(), Box<dyn Error>> {
    let mut vm = SicXeVm::empty();
    let mut loader = ProgramLoader::new();
    let cont = fs::read_to_string("test.ebj")?;
    let sdb = fs::read_to_string("test.sdb")?;

    let mut debugger = SdbDebugger::new();
    debugger.load(0, sdb).unwrap();
    let labels = debugger.get_labels();

    loader.load_string(&cont, 0);

    loader.copy_all_to(&mut vm);

    let debugger = Arc::new(Mutex::new(debugger));

    vm.debugger = Some(Box::new(debugger.clone()));

    loop {
        terminal.draw(|frame| {
            let pc = vm.PC.as_u32();
            let (pc_row, pc_idx) = address_to_coords(pc);
            let mut rows = vm.memory[..48]
                .iter()
                .map(|b| format!("{:0>2X}", b))
                .collect::<Vec<_>>()
                .chunks(16)
                .enumerate()
                .map(|(idx, c)| {
                    let mut items = Vec::from(c);
                    items.insert(0, format!("0x{:0>4X}", idx * 16));
                    let items = items
                        .into_iter()
                        .enumerate()
                        .map(|(cell_idx, val)| {
                            let cell = Cell::from(val);
                            let (inst_size, target) = {
                                let debugger = debugger.lock().expect("mutex read");
                                let inst_size = debugger.get_last_op().map_or(1, |op| op.len());
                                let op = debugger.get_next_op(&vm);

                                let target = op
                                    .map(|op| debugger.find_target_address(&vm, &op))
                                    .and_then(|(target, _)| target);

                                (inst_size, target)
                            };
                            if idx == pc_row
                                && (pc_idx..pc_idx + inst_size as usize).contains(&cell_idx)
                            {
                                cell.style(
                                    Style::default()
                                        .add_modifier(Modifier::BOLD)
                                        .fg(Color::Yellow),
                                )
                            } else if target
                                .map(address_to_coords)
                                .map(|(row, col)| idx == row && (col..col + 3).contains(&cell_idx))
                                .unwrap_or(false)
                            {
                                cell.style(
                                    Style::default()
                                        .add_modifier(Modifier::BOLD)
                                        .fg(Color::Blue),
                                )
                            } else {
                                cell
                            }
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

            let greeting = Table::new(rows)
                .style(Style::default().fg(Color::White))
                .widths(&widths)
                .column_spacing(1);
            let area = Rect::new(0, 20, 75, 30);
            frame.render_widget(greeting, area);

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
            let area = Rect::new(0, 0, 50, 50);
            frame.render_widget(symbols, area);

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
            let symbols = Table::new(rows).widths(&widths);
            let area = Rect::new(25, 0, 50, 50);
            frame.render_widget(symbols, area);
        })?;
        if event::poll(Duration::from_millis(250))? {
            if let Event::Key(key) = event::read()? {
                if KeyCode::Char('q') == key.code {
                    break;
                }
                if KeyCode::Char('s') == key.code {
                    vm.step();
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

fn run_old(terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> Result<(), Box<dyn Error>> {
    let data = [
        0x00u8, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E,
        0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1A, 0x1B, 0x1C, 0x1D,
        0x1E, 0x1F, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2A, 0x2B, 0x2C,
        0x2D, 0x2E, 0x2F,
    ];

    let mut vm = SicXeVm::empty();
    let mut loader = ProgramLoader::new();
    let cont = fs::read_to_string("test.ebj")?;

    loader.load_string(&cont, 0);

    loader.copy_all_to(&mut vm);

    loop {
        terminal.draw(|frame| {
            let mut rows = data
                .iter()
                .map(|b| format!("{:0>2X}", b))
                .collect::<Vec<_>>()
                .chunks(16)
                .enumerate()
                .map(|(idx, c)| {
                    let mut items = Vec::from(c);
                    items.insert(0, format!("0x{:0>4X}", idx * 16));
                    let items = items
                        .into_iter()
                        .enumerate()
                        .map(|(cell_idx, val)| {
                            let cell = Cell::from(val);
                            if idx == 1 && (1..4).contains(&cell_idx) {
                                cell.style(
                                    Style::default()
                                        .add_modifier(Modifier::BOLD)
                                        .fg(Color::Yellow),
                                )
                            } else {
                                cell
                            }
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
            let greeting = Table::new(rows)
                .style(Style::default().fg(Color::White))
                .widths(&widths)
                .column_spacing(1);
            frame.render_widget(greeting, frame.size());
        })?;
        if event::poll(Duration::from_millis(250))? {
            if let Event::Key(key) = event::read()? {
                if KeyCode::Char('q') == key.code {
                    break;
                }
            }
        }
    }

    Ok(())
}
