use super::error::CypherError;

/// A lexical token together with the byte position where it starts in the source query.
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub position: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Ident(String),
    Int(i64),
    Str(String),
    LParen,
    RParen,
    LBracket,
    RBracket,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Dot,
    DotDot,
    Star,
    Pipe,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    Minus,
}

/// Tokenizes a Cypher query string into a flat token stream.
///
/// # Errors
///
/// Returns a [`CypherError::Syntax`] if an unterminated string or unexpected character is found.
pub fn tokenize(input: &str) -> Result<Vec<Token>, CypherError> {
    let chars: Vec<char> = input.chars().collect();
    let mut tokens = Vec::new();
    let mut index = 0;

    while index < chars.len() {
        let ch = chars[index];

        if ch.is_whitespace() {
            index += 1;
            continue;
        }

        let start = index;

        match ch {
            '(' => push(&mut tokens, TokenKind::LParen, start, &mut index),
            ')' => push(&mut tokens, TokenKind::RParen, start, &mut index),
            '[' => push(&mut tokens, TokenKind::LBracket, start, &mut index),
            ']' => push(&mut tokens, TokenKind::RBracket, start, &mut index),
            '{' => push(&mut tokens, TokenKind::LBrace, start, &mut index),
            '}' => push(&mut tokens, TokenKind::RBrace, start, &mut index),
            ',' => push(&mut tokens, TokenKind::Comma, start, &mut index),
            ':' => push(&mut tokens, TokenKind::Colon, start, &mut index),
            '*' => push(&mut tokens, TokenKind::Star, start, &mut index),
            '|' => push(&mut tokens, TokenKind::Pipe, start, &mut index),
            '-' => push(&mut tokens, TokenKind::Minus, start, &mut index),
            '=' => push(&mut tokens, TokenKind::Eq, start, &mut index),
            '.' => {
                if chars.get(index + 1) == Some(&'.') {
                    tokens.push(Token {
                        kind: TokenKind::DotDot,
                        position: start,
                    });
                    index += 2;
                } else {
                    push(&mut tokens, TokenKind::Dot, start, &mut index);
                }
            }
            '<' => {
                if chars.get(index + 1) == Some(&'=') {
                    tokens.push(Token {
                        kind: TokenKind::Lte,
                        position: start,
                    });
                    index += 2;
                } else if chars.get(index + 1) == Some(&'>') {
                    tokens.push(Token {
                        kind: TokenKind::Neq,
                        position: start,
                    });
                    index += 2;
                } else {
                    push(&mut tokens, TokenKind::Lt, start, &mut index);
                }
            }
            '>' => {
                if chars.get(index + 1) == Some(&'=') {
                    tokens.push(Token {
                        kind: TokenKind::Gte,
                        position: start,
                    });
                    index += 2;
                } else {
                    push(&mut tokens, TokenKind::Gt, start, &mut index);
                }
            }
            '\'' | '"' => {
                let (value, next) = lex_string(&chars, index, ch)?;
                tokens.push(Token {
                    kind: TokenKind::Str(value),
                    position: start,
                });
                index = next;
            }
            c if c.is_ascii_digit() => {
                let (value, next) = lex_number(&chars, index)?;
                tokens.push(Token {
                    kind: TokenKind::Int(value),
                    position: start,
                });
                index = next;
            }
            c if is_ident_start(c) => {
                let (value, next) = lex_ident(&chars, index);
                tokens.push(Token {
                    kind: TokenKind::Ident(value),
                    position: start,
                });
                index = next;
            }
            other => {
                return Err(CypherError::syntax(format!("unexpected character `{other}`"), start));
            }
        }
    }

    Ok(tokens)
}

fn push(tokens: &mut Vec<Token>, kind: TokenKind, position: usize, index: &mut usize) {
    tokens.push(Token { kind, position });
    *index += 1;
}

fn lex_string(chars: &[char], start: usize, quote: char) -> Result<(String, usize), CypherError> {
    let mut value = String::new();
    let mut index = start + 1;

    while index < chars.len() {
        let ch = chars[index];
        if ch == '\\' {
            if let Some(&next) = chars.get(index + 1) {
                match next {
                    'n' => value.push('\n'),
                    't' => value.push('\t'),
                    'r' => value.push('\r'),
                    '\\' => value.push('\\'),
                    '\'' => value.push('\''),
                    '"' => value.push('"'),
                    other => value.push(other),
                }
                index += 2;
                continue;
            }
            return Err(CypherError::syntax("unterminated escape in string literal", index));
        }
        if ch == quote {
            return Ok((value, index + 1));
        }
        value.push(ch);
        index += 1;
    }

    Err(CypherError::syntax("unterminated string literal", start))
}

fn lex_number(chars: &[char], start: usize) -> Result<(i64, usize), CypherError> {
    let mut index = start;
    while index < chars.len() && chars[index].is_ascii_digit() {
        index += 1;
    }
    let text: String = chars[start..index].iter().collect();
    let value = text
        .parse::<i64>()
        .map_err(|_| CypherError::syntax(format!("invalid integer `{text}`"), start))?;
    Ok((value, index))
}

fn lex_ident(chars: &[char], start: usize) -> (String, usize) {
    let mut index = start;
    while index < chars.len() && is_ident_continue(chars[index]) {
        index += 1;
    }
    let text: String = chars[start..index].iter().collect();
    (text, index)
}

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}
