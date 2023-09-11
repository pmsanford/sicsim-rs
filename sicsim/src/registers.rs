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

fn highlight_if(val: String, highlight: bool) -> Cell<'static> {
    let mut cell = Cell::from(val);

    if highlight {
        cell = cell.style(Style::default().fg(Color::Blue));
    }

    cell
}

pub fn registers_view(
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
