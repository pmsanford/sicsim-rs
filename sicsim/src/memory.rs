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

fn address_to_coords(start_addr: u32, address: u32) -> Option<(usize, usize)> {
    if start_addr > address {
        return None;
    }
    let address = address - start_addr;
    let row = address / 16;
    let idx = address % 16 + 1;

    Some((row as usize, idx as usize))
}

pub fn memory_view(
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
