use std::collections::HashMap;

use anyhow::Result;
use once_cell::sync::Lazy;
use rangemap::RangeMap;
use sicasm2::parser::{self, Label, ProgramLine, Value};
use tokio::sync::Mutex;
#[allow(unused_imports)]
use tower_lsp::{
    lsp_types::{
        DidChangeTextDocumentParams, DidOpenTextDocumentParams, InitializeParams, InitializeResult,
        MessageType, SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
        SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
        SemanticTokensParams, SemanticTokensResult, SemanticTokensServerCapabilities,
        ServerCapabilities, TextDocumentSyncCapability, TextDocumentSyncKind,
        WorkDoneProgressOptions,
    },
    Client, LanguageServer, LspService, Server,
};

static TYPE_MAP: Lazy<HashMap<SemanticTokenType, u32>> = Lazy::new(|| {
    HashMap::from([
        (SemanticTokenType::KEYWORD, 0),
        (SemanticTokenType::STRING, 1),
        (SemanticTokenType::NUMBER, 2),
        (SemanticTokenType::MODIFIER, 3),
        (SemanticTokenType::TYPE, 4),
        (SemanticTokenType::VARIABLE, 5),
        (SemanticTokenType::FUNCTION, 6),
        (SemanticTokenType::PARAMETER, 7),
        (SemanticTokenType::OPERATOR, 8),
        (SemanticTokenType::COMMENT, 9),
    ])
});

static MOD_MAP: Lazy<HashMap<SemanticTokenModifier, u32>> = Lazy::new(|| {
    HashMap::from([
        (SemanticTokenModifier::DEFINITION, 1),
        (SemanticTokenModifier::DECLARATION, 2),
    ])
});

#[allow(dead_code)]
struct Lsp {
    client: Client,
    text: Mutex<Option<String>>,
    label_usage_map: Mutex<RangeMap<u32, u32>>,
    label_reference_map: Mutex<RangeMap<u32, Vec<u32>>>,
    semantic_tokens: Mutex<Vec<SemanticToken>>,
}

impl Lsp {
    #[allow(unused_assignments)]
    async fn on_change(&self, text: String) -> Result<()> {
        *self.text.lock().await = Some(text.clone());
        let lines = parser::parse_program(&text)?;

        let mut tokens = Vec::new();

        let mut label_defs = HashMap::new();
        let mut label_references = HashMap::new();

        let mut last_line = 0;
        for line in lines {
            let mut last_col = 0;
            let ProgramLine::Assembly(data) = line.data else {
                if let ProgramLine::Comment(_) = line.data {
                    let token = SemanticToken {
                        delta_line: line.line_no as u32 - last_line,
                        delta_start: 0,
                        length: line.text.len() as u32,
                        token_type: TYPE_MAP[&SemanticTokenType::COMMENT],
                        token_modifiers_bitset: 0,
                    };
                    last_col = 0;
                    last_line = line.line_no as u32;

                    tokens.push(token);
                }
                continue;
            };
            if let Some(label) = data.label {
                let token = SemanticToken {
                    delta_line: line.line_no as u32 - last_line,
                    delta_start: 0,
                    length: label.0.len() as u32,
                    token_type: TYPE_MAP[&SemanticTokenType::VARIABLE],
                    token_modifiers_bitset: MOD_MAP[&SemanticTokenModifier::DEFINITION]
                        | MOD_MAP[&SemanticTokenModifier::DECLARATION],
                };

                last_col = 0;
                last_line = line.line_no as u32;

                label_defs.insert(label.0.clone(), (line.line_no as u32, label.0.len() as u32));

                tokens.push(token);
            }

            let dir_str = match data.directive {
                parser::Directive::Op(op) => match op {
                    parser::Op::OneByte(o) => o.to_string(),
                    parser::Op::OneReg(o) => o.to_string(),
                    parser::Op::TwoReg(o) => o.to_string(),
                    parser::Op::Shift(o) => o.to_string(),
                    parser::Op::Svc => "SVC".into(),
                    parser::Op::Variable(o) => o.to_string(),
                },
                parser::Directive::Command(c) => c.to_string(),
            };
            let dir_idx = line.text.find(&dir_str).unwrap() as u32;

            if data.extended {
                tokens.push(SemanticToken {
                    delta_line: line.line_no as u32 - last_line,
                    delta_start: dir_idx - 1 - last_col,
                    length: 1,
                    token_type: TYPE_MAP[&SemanticTokenType::OPERATOR],
                    token_modifiers_bitset: 0,
                });
                last_col = dir_idx - 1;
                last_line = line.line_no as u32;
            }

            tokens.push(SemanticToken {
                delta_line: line.line_no as u32 - last_line,
                delta_start: dir_idx - last_col,
                length: dir_str.len() as u32,
                token_type: TYPE_MAP[&if matches!(data.directive, parser::Directive::Command(_)) {
                    SemanticTokenType::KEYWORD
                } else {
                    SemanticTokenType::FUNCTION
                }],
                token_modifiers_bitset: 0,
            });

            last_col = dir_idx;
            last_line = line.line_no as u32;

            if let Some(arg) = data.argument {
                match arg {
                    parser::Argument::Value(Value::String(Label(s))) => {
                        let s_idx = line.text.rfind(&s).unwrap() as u32;
                        if matches!(
                            data.address_modifier,
                            parser::AddressModifier::Indirect | parser::AddressModifier::Immediate
                        ) {
                            tokens.push(SemanticToken {
                                delta_line: line.line_no as u32 - last_line,
                                delta_start: s_idx - 1 - last_col,
                                length: 1,
                                token_type: TYPE_MAP[&SemanticTokenType::OPERATOR],
                                token_modifiers_bitset: 0,
                            });
                            last_col = s_idx - 1;
                            last_line = line.line_no as u32;
                        }
                        tokens.push(SemanticToken {
                            delta_line: line.line_no as u32 - last_line,
                            delta_start: s_idx - last_col,
                            length: s.len() as u32,
                            token_type: TYPE_MAP[&SemanticTokenType::VARIABLE],
                            token_modifiers_bitset: 0,
                        });

                        label_references
                            .entry(s.clone())
                            .or_insert_with(Vec::new)
                            .push((s_idx, s.len() as u32));

                        last_col = s_idx;
                        last_line = line.line_no as u32;
                    }
                    parser::Argument::Value(Value::Number(n)) => {
                        let nstr = n.to_string();
                        let n_idx = line.text.find(&nstr).unwrap() as u32;
                        match data.address_modifier {
                            parser::AddressModifier::Unmodified => {}
                            parser::AddressModifier::Indirect
                            | parser::AddressModifier::Immediate => {
                                tokens.push(SemanticToken {
                                    delta_line: line.line_no as u32 - last_line,
                                    delta_start: n_idx - 1 - last_col,
                                    length: 1,
                                    token_type: TYPE_MAP[&SemanticTokenType::OPERATOR],
                                    token_modifiers_bitset: 0,
                                });

                                last_col = n_idx - 1;
                                last_line = line.line_no as u32;
                            }
                        }
                        tokens.push(SemanticToken {
                            delta_line: line.line_no as u32 - last_line,
                            delta_start: n_idx - last_col,
                            length: nstr.len() as u32,
                            token_type: TYPE_MAP[&SemanticTokenType::NUMBER],
                            token_modifiers_bitset: 0,
                        });

                        last_col = n_idx;
                        last_line = line.line_no as u32;
                    }
                    parser::Argument::Value(Value::Bytes(b)) => {
                        let b_str = b.iter().map(|b| format!("{:0>2X}", b)).collect::<Vec<_>>();
                        let b_str = b_str.join("");
                        let b_idx = line
                            .text
                            .to_lowercase()
                            .find(&b_str.to_lowercase())
                            .unwrap() as u32;

                        tokens.push(SemanticToken {
                            delta_line: line.line_no as u32 - last_line,
                            delta_start: b_idx - last_col,
                            length: b_str.len() as u32,
                            token_type: TYPE_MAP[&SemanticTokenType::NUMBER],
                            token_modifiers_bitset: 0,
                        });

                        last_col = b_idx;
                        last_line = line.line_no as u32;
                    }
                    parser::Argument::Value(Value::Chars(c)) => {
                        let b_str = c.iter().map(|b| *b as char).collect::<String>();
                        let b_idx = line
                            .text
                            .to_lowercase()
                            .find(&b_str.to_lowercase())
                            .unwrap() as u32;

                        tokens.push(SemanticToken {
                            delta_line: line.line_no as u32 - last_line,
                            delta_start: b_idx - last_col,
                            length: b_str.len() as u32,
                            token_type: TYPE_MAP[&SemanticTokenType::STRING],
                            token_modifiers_bitset: 0,
                        });

                        last_col = b_idx;
                        last_line = line.line_no as u32;
                    }
                    parser::Argument::Value(_) => {}
                    parser::Argument::Expr(_) => {}
                    parser::Argument::ExprCurrentOffset => {}
                }
            }
        }

        *self.semantic_tokens.lock().await = tokens;

        let mut usage_map = self.label_usage_map.lock().await;
        let mut reference_map = self.label_reference_map.lock().await;

        for (label, (loc, label_len)) in label_defs.iter() {
            if let Some(references) = label_references.get(label) {
                let mut reference_locations = Vec::new();
                for (start, len) in references.iter().copied() {
                    usage_map.insert(start..start + len, *loc);
                    reference_locations.push(start);
                }
                reference_map.insert(*loc..*loc + *label_len, reference_locations);
            }
        }

        Ok(())
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Lsp {
    async fn initialize(
        &self,
        _: InitializeParams,
    ) -> tower_lsp::jsonrpc::Result<InitializeResult> {
        let mut token_types = TYPE_MAP.iter().collect::<Vec<_>>();
        token_types.sort_by_key(|(_, v)| *v);
        let token_types = token_types
            .iter()
            .map(|(k, _)| (*k).clone())
            .collect::<Vec<_>>();
        let mut token_modifiers = MOD_MAP.iter().collect::<Vec<_>>();
        token_modifiers.sort_by_key(|(_, v)| *v);
        let token_modifiers = token_modifiers
            .iter()
            .map(|(k, _)| (*k).clone())
            .collect::<Vec<_>>();
        let tokens =
            SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
                work_done_progress_options: WorkDoneProgressOptions::default(),
                legend: SemanticTokensLegend {
                    token_types,
                    token_modifiers,
                },
                range: None,
                full: Some(SemanticTokensFullOptions::Bool(true)),
            });
        let capabilities = ServerCapabilities {
            semantic_tokens_provider: Some(tokens),
            text_document_sync: Some(TextDocumentSyncCapability::Kind(TextDocumentSyncKind::FULL)),
            ..ServerCapabilities::default()
        };
        let result = InitializeResult {
            capabilities,
            ..InitializeResult::default()
        };
        Ok(result)
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.on_change(params.text_document.text).await.unwrap();
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        self.on_change(params.content_changes[0].text.clone())
            .await
            .unwrap();
    }

    async fn semantic_tokens_full(
        &self,
        _params: SemanticTokensParams,
    ) -> tower_lsp::jsonrpc::Result<Option<SemanticTokensResult>> {
        let tokens = self.semantic_tokens.lock().await.clone();

        let result = SemanticTokensResult::Tokens(SemanticTokens {
            result_id: None,
            data: tokens,
        });

        Ok(Some(result))
    }

    async fn shutdown(&self) -> tower_lsp::jsonrpc::Result<()> {
        Ok(())
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Lsp {
        client,
        text: Mutex::new(None),
        label_usage_map: Mutex::new(RangeMap::new()),
        label_reference_map: Mutex::new(RangeMap::new()),
        semantic_tokens: Mutex::new(Vec::new()),
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;

    Ok(())
}
