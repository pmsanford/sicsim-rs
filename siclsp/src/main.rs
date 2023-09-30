use std::collections::HashMap;

use anyhow::Result;
use once_cell::sync::Lazy;
use sicasm2::parser::{self, ProgramLine};
use tokio::sync::Mutex;
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
    ])
});

struct Lsp {
    client: Client,
    text: Mutex<Option<String>>,
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
        let token_modifiers = vec![SemanticTokenModifier::DEFINITION];
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
        let mut text = self.text.lock().await;
        *text = Some(params.text_document.text);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let mut text = self.text.lock().await;
        *text = Some(params.content_changes[0].text.clone())
    }

    #[allow(unused_assignments)]
    async fn semantic_tokens_full(
        &self,
        _params: SemanticTokensParams,
    ) -> tower_lsp::jsonrpc::Result<Option<SemanticTokensResult>> {
        let Some(text) = self.text.lock().await.clone() else {
            return Ok(None);
        };
        let lines = parser::parse_program(&text)
            .map_err(|_| tower_lsp::jsonrpc::Error::internal_error())?;

        let mut tokens = Vec::new();

        let mut last_line = 0;
        for line in lines {
            let mut last_col = 0;
            let ProgramLine::Assembly(data) = line.data else {
                continue;
            };
            if let Some(label) = data.label {
                self.client
                    .log_message(
                        MessageType::WARNING,
                        format!("Label {} on line {}", label.0, line.line_no),
                    )
                    .await;
                let token = SemanticToken {
                    delta_line: line.line_no as u32 - last_line,
                    delta_start: 0,
                    length: label.0.len() as u32,
                    token_type: TYPE_MAP[&SemanticTokenType::VARIABLE],
                    token_modifiers_bitset: 0,
                };

                last_col = 0;
                last_line = line.line_no as u32;

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

            tokens.push(SemanticToken {
                delta_line: line.line_no as u32 - last_line,
                delta_start: dir_idx - last_col,
                length: dir_str.len() as u32,
                token_type: TYPE_MAP[&SemanticTokenType::KEYWORD],
                token_modifiers_bitset: 0,
            });

            last_col = dir_idx;
            last_line = line.line_no as u32;
        }

        self.client
            .log_message(MessageType::WARNING, format!("Found tokens: {:?}", tokens))
            .await;

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
    })
    .finish();

    Server::new(stdin, stdout, socket).serve(service).await;

    Ok(())
}
