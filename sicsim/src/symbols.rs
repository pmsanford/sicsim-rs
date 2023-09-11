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
    text::{Line, Span, Text},
    widgets::{
        Block, Borders, Cell, Clear, List, ListItem, ListState, Paragraph, Row, Table, Tabs,
    },
    Frame, Terminal,
};

enum PanelMode {
    View,
    ProgramSelect,
    LabelSelect,
    WatchEdit,
}

pub struct SymbolsPanel {
    programs: Vec<String>,
    labels: BTreeMap<String, BTreeMap<String, u32>>,
    watches: Vec<String>,
    watch_addresses: HashMap<String, u32>,
    mode: PanelMode,
    program: String,
    list_state: RefCell<ListState>,
    area: Rect,
    active: bool,
}

impl SymbolsPanel {
    pub fn new(programs: Vec<String>, labels: BTreeMap<String, BTreeMap<String, u32>>) -> Self {
        Self {
            programs,
            labels,
            watches: Vec::new(),
            watch_addresses: HashMap::new(),
            mode: PanelMode::View,
            program: String::default(),
            list_state: RefCell::new(ListState::default()),
            area: Rect::new(60, 16, 34, 35),
            active: false,
        }
    }

    fn max(&self) -> usize {
        match self.mode {
            PanelMode::View => 0,
            PanelMode::ProgramSelect => self.programs.len(),
            PanelMode::LabelSelect => self.labels[&self.program].len(),
            PanelMode::WatchEdit => self.watches.len(),
        }
    }

    pub fn add_watch(&mut self, name: String, addr: u32) {
        self.watches.push(name.clone());
        self.watch_addresses.insert(name, addr);
    }

    pub fn set_active(&mut self, active: bool) {
        self.active = active;
    }

    pub fn edit(&mut self) {
        let state = ListState::default().with_selected(Some(0));
        self.list_state = RefCell::new(state);
        self.mode = PanelMode::WatchEdit;
    }

    pub fn start(&mut self) {
        let state = ListState::default().with_selected(Some(0));
        self.list_state = RefCell::new(state);
        self.mode = PanelMode::ProgramSelect;
    }

    pub fn next(&mut self) {
        let selected = self
            .list_state
            .borrow()
            .selected()
            .map(|l| l + 1)
            .unwrap_or(0);
        if selected <= self.max() {
            self.list_state.borrow_mut().select(Some(selected));
        }
    }

    pub fn previous(&mut self) {
        let selected = self
            .list_state
            .borrow()
            .selected()
            .map(|l| if l > 0 { l - 1 } else { 0 })
            .unwrap_or(0);
        self.list_state.borrow_mut().select(Some(selected));
    }

    pub fn select(&mut self) {
        let Some(selected) = self.list_state.borrow().selected() else {
            return;
        };
        match self.mode {
            PanelMode::View => {}
            PanelMode::ProgramSelect => {
                self.program = self.programs[selected].clone();
                self.list_state = RefCell::new(ListState::default().with_selected(Some(0)));
                self.mode = PanelMode::LabelSelect;
            }
            PanelMode::LabelSelect => {
                let Some(selected) = self.list_state.borrow().selected() else {
                    return;
                };
                let label_name = self.labels[&self.program]
                    .keys()
                    .nth(selected)
                    .expect("label index mismatch")
                    .clone();
                let label_address = self.labels[&self.program][&label_name];
                self.watches.push(label_name.clone());
                self.watch_addresses.insert(label_name, label_address);

                self.list_state = RefCell::new(ListState::default().with_selected(Some(0)));
                self.mode = PanelMode::View;
            }
            PanelMode::WatchEdit => {
                let Some(selected) = self.list_state.borrow().selected() else {
                    return;
                };
                let label = self.watches.remove(selected);
                self.watch_addresses.remove(&label);

                self.list_state = RefCell::new(ListState::default().with_selected(Some(0)));
                self.mode = PanelMode::View;
            }
        }
    }

    pub fn cancel(&mut self) {
        self.list_state = RefCell::new(ListState::default().with_selected(Some(0)));
        self.mode = PanelMode::View;
    }

    pub fn render(&self, frame: &mut Frame<CrosstermBackend<Stdout>>, vm: &SicXeVm) {
        match self.mode {
            PanelMode::View => self.render_watches(frame, vm),
            PanelMode::ProgramSelect => self.render_programs(frame),
            PanelMode::LabelSelect => self.render_labels(frame),
            PanelMode::WatchEdit => self.render_watch_edit(frame),
        }
    }

    fn render_watch_edit(&self, frame: &mut Frame<CrosstermBackend<Stdout>>) {
        let items = self
            .watches
            .iter()
            .map(|watch| ListItem::new(watch.clone()))
            .collect::<Vec<_>>();
        let list = self.configure_list(List::new(items));
        frame.render_stateful_widget(list, self.area, &mut self.list_state.borrow_mut());
    }

    fn render_watches(&self, frame: &mut Frame<CrosstermBackend<Stdout>>, vm: &SicXeVm) {
        let rows = self
            .watches
            .iter()
            .map(|watch| {
                let address = self.watch_addresses[watch];
                Row::new(vec![
                    watch.clone(),
                    format!("{:0>6X}", vm.word_at(address).unwrap().as_u32()),
                ])
            })
            .collect::<Vec<_>>();
        let widths = vec![Constraint::Length(13); 2];
        let symbols = Table::new(rows).widths(&widths).block(self.block());
        frame.render_widget(symbols, self.area);
    }

    fn render_programs(&self, frame: &mut Frame<CrosstermBackend<Stdout>>) {
        let items = self
            .programs
            .iter()
            .map(|program| ListItem::new(program.clone()))
            .collect::<Vec<_>>();
        let list = self.configure_list(List::new(items));
        frame.render_stateful_widget(list, self.area, &mut self.list_state.borrow_mut())
    }

    fn render_labels(&self, frame: &mut Frame<CrosstermBackend<Stdout>>) {
        let items = self.labels[&self.program]
            .keys()
            .map(|label| ListItem::new(label.clone()))
            .collect::<Vec<_>>();
        let list = self.configure_list(List::new(items));
        frame.render_stateful_widget(list, self.area, &mut self.list_state.borrow_mut())
    }

    fn block(&self) -> Block {
        let color = if self.active {
            Color::Green
        } else {
            Color::default()
        };
        Block::default()
            .borders(Borders::ALL)
            .border_style(Style::default().fg(color))
    }

    fn configure_list<'a>(&'a self, list: List<'a>) -> List<'a> {
        list.highlight_symbol("> ").block(self.block())
    }
}

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
