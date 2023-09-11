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

pub fn symbols_view(
    frame: &mut Frame<CrosstermBackend<Stdout>>,
    vm: &SicXeVm,
    debugger: &Mutex<SdbDebugger>,
    programs: &[String],
    labels: &BTreeMap<String, BTreeMap<String, u32>>,
    index: usize,
) {
    let tabs = Tabs::new(programs.to_vec())
        .block(Block::default().borders(Borders::ALL).title("Symbols"))
        .select(index)
        .highlight_style(Style::default().fg(Color::Red));
    let area = Rect::new(60, 13, 34, 3);
    frame.render_widget(tabs, area);
    let flattened = &labels[&programs[index]];
    let mut rows = flattened
        .iter()
        .sorted_by_key(|(_, v)| *v)
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
    let area = Rect::new(60, 16, 34, 35);
    frame.render_widget(symbols, area);
}
