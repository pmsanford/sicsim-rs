#![allow(unused)]
use std::{
    collections::HashMap,
    error::Error,
    fs,
    io::{self, Stdout},
    time::Duration,
};

use crossterm::{
    event::{self, Event, KeyCode},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use libsic::{
    word::DWordExt,
    xe::{debugger::SdbConsoleDebugger, load::ProgramLoader, vm::SicXeVm},
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

fn run(terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> Result<(), Box<dyn Error>> {
    let mut vm = SicXeVm::empty();
    let mut loader = ProgramLoader::new();
    let cont = fs::read_to_string("test.ebj")?;
    let sdb = fs::read_to_string("test.sdb")?;

    let mut debugger = SdbConsoleDebugger::new();
    debugger.load(0, sdb).unwrap();
    let labels = debugger.get_labels();

    loader.load_string(&cont, 0);

    loader.copy_all_to(&mut vm);

    //vm.debugger = Some(Box::new(debugger));

    loop {
        terminal.draw(|frame| {
            let pc = vm.PC.as_u32() as usize;
            let pc_row = pc / 16;
            let pc_idx = pc % 16 + 1;
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
                            if idx == pc_row && cell_idx == pc_idx {
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

            let rows = vec![
                Row::new(vec!["A".to_owned(), format!("{:0>6X}", vm.A.as_u32())]),
                Row::new(vec!["X".to_owned(), format!("{:0>6X}", vm.X.as_u32())]),
                Row::new(vec!["L".to_owned(), format!("{:0>6X}", vm.L.as_u32())]),
                Row::new(vec!["B".to_owned(), format!("{:0>6X}", vm.B.as_u32())]),
                Row::new(vec!["S".to_owned(), format!("{:0>6X}", vm.S.as_u32())]),
                Row::new(vec!["T".to_owned(), format!("{:0>6X}", vm.T.as_u32())]),
                Row::new(vec!["F".to_owned(), format!("{:0>12X}", vm.F.as_u64())]),
                Row::new(vec!["PC".to_owned(), format!("{:0>6X}", vm.PC.as_u32())]),
                Row::new(vec!["SW".to_owned(), format!("{:0>6X}", vm.SW.as_u32())]),
                Row::new(vec!["I".to_owned(), format!("{:0>6X}", vm.I.as_u32())]),
            ];

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
