use crate::char::CharExt;
use crate::span::{FromInnerAndSpan, Span};
use crate::token::{Token, TokenWithSpan};
use std::result;

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    chars: &'a [char],
    index: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(chars: &[char]) -> Lexer {
        Lexer { chars, index: 0 }
    }

    pub fn read_tokens(&mut self) -> Result<Vec<TokenWithSpan>> {
        let mut tokens = vec![];
        loop {
            let token = self.read_token()?;
            tokens.push(token);
            if token.token == Token::Eof {
                break;
            }
        }
        Ok(tokens)
    }

    pub fn read_token(&mut self) -> Result<TokenWithSpan> {
        loop {
            self.skip_whitespace();
            match self.peek_char_2() {
                ('/', '*') => self.skip_multi_line_comment()?,
                ('/', '/') => self.skip_single_line_comment()?,
                _ => break,
            }
        }
        match self.peek_char_2() {
            ('\0', _) => Ok(self.begin_span().end(self, Token::Eof)),
            ('!', '=') => Ok(self.read_punctuator_2(Token::Ne)),
            ('!', _) => Ok(self.read_punctuator(Token::Not)),
            ('&', '&') => Ok(self.read_punctuator_2(Token::AndAnd)),
            ('(', _) => Ok(self.read_punctuator(Token::LeftParen)),
            (')', _) => Ok(self.read_punctuator(Token::RightParen)),
            ('*', '=') => Ok(self.read_punctuator_2(Token::StarEq)),
            ('*', _) => Ok(self.read_punctuator(Token::Star)),
            ('+', '+') => Ok(self.read_punctuator_2(Token::PlusPlus)),
            ('+', '=') => Ok(self.read_punctuator_2(Token::PlusEq)),
            ('+', _) => Ok(self.read_punctuator(Token::Plus)),
            (',', _) => Ok(self.read_punctuator(Token::Comma)),
            ('-', '-') => Ok(self.read_punctuator_2(Token::MinusMinus)),
            ('-', '=') => Ok(self.read_punctuator_2(Token::MinusEq)),
            ('-', _) => Ok(self.read_punctuator(Token::Minus)),
            ('.', ch) if ch.is_decimal_digit() => self.read_decimal_literal(),
            ('.', _) => Ok(self.read_punctuator(Token::Dot)),
            ('/', '=') => Ok(self.read_punctuator_2(Token::SlashEq)),
            ('/', _) => Ok(self.read_punctuator(Token::Slash)),
            ('0', 'X') | ('0', 'x') => self.read_hex_integer_literal(),
            ('0', ch) if ch.is_octal_digit() => Ok(self.read_octal_integer_literal()),
            (':', _) => Ok(self.read_punctuator(Token::Colon)),
            (';', _) => Ok(self.read_punctuator(Token::Semicolon)),
            ('<', '=') => Ok(self.read_punctuator_2(Token::Le)),
            ('<', _) => Ok(self.read_punctuator(Token::Lt)),
            ('=', '=') => Ok(self.read_punctuator_2(Token::EqEq)),
            ('=', _) => Ok(self.read_punctuator(Token::Eq)),
            ('>', '=') => Ok(self.read_punctuator_2(Token::Ge)),
            ('>', _) => Ok(self.read_punctuator(Token::Gt)),
            ('?', _) => Ok(self.read_punctuator(Token::Question)),
            ('[', _) => Ok(self.read_punctuator(Token::LeftBracket)),
            (']', _) => Ok(self.read_punctuator(Token::RightBracket)),
            ('^', '^') => Ok(self.read_punctuator_2(Token::XorXor)),
            ('{', _) => Ok(self.read_punctuator(Token::LeftBrace)),
            ('|', '|') => Ok(self.read_punctuator_2(Token::OrOr)),
            ('}', _) => Ok(self.read_punctuator(Token::RightBrace)),
            (ch, _) if ch.is_decimal_digit() => self.read_decimal_literal(),
            (ch, _) if ch.is_identifier_start() => self.read_identifier_name(),
            (ch, _) => Err(self.error(ErrorKind::IllegalChar(ch))),
        }
    }

    fn skip_whitespace(&mut self) {
        while self.peek_char().is_ascii_whitespace() {
            self.skip_char();
        }
    }

    fn skip_multi_line_comment(&mut self) -> Result<()> {
        self.skip_char_2();
        loop {
            match self.peek_char_2() {
                ('\0', _) => return Err(self.error(ErrorKind::UnterminatedComment)),
                ('*', '/') => {
                    self.skip_char_2();
                    break;
                }
                _ => self.skip_char(),
            }
        }
        Ok(())
    }

    fn skip_single_line_comment(&mut self) -> Result<()> {
        self.skip_char_2();
        loop {
            match self.peek_char_2() {
                ('\0', _) => break,
                ('\n', _) => {
                    self.skip_char();
                    break;
                }
                ('\r', '\n') => {
                    self.skip_char_2();
                    break;
                }
                ('\r', _) => {
                    self.skip_char();
                    break;
                }
                _ => self.skip_char(),
            }
        }
        Ok(())
    }

    fn read_identifier_name(&mut self) -> Result<TokenWithSpan> {
        let span = self.begin_span();
        let mut string = String::new();
        string.push(self.read_char());
        self.read_chars_while(&mut string, |ch| ch.is_identifier_part());
        Ok(span.end(
            self,
            match string.as_str() {
                "attribute" => Token::Attribute,
                "bool" => Token::Bool,
                "break" => Token::Break,
                "bvec2" => Token::Bvec2,
                "bvec3" => Token::Bvec3,
                "bvec4" => Token::Bvec4,
                "const" => Token::Const,
                "continue" => Token::Continue,
                "discard" => Token::Discard,
                "do" => Token::Do,
                "else" => Token::Else,
                "false" => Token::BoolConstant(false),
                "float" => Token::Float,
                "for" => Token::For,
                "highp" => Token::Highp,
                "if" => Token::If,
                "in" => Token::In,
                "int" => Token::Int,
                "inout" => Token::Inout,
                "invariant" => Token::Invariant,
                "ivec2" => Token::Ivec2,
                "ivec3" => Token::Ivec3,
                "ivec4" => Token::Ivec4,
                "lowp" => Token::Lowp,
                "mat2" => Token::Mat2,
                "mat3" => Token::Mat3,
                "mat4" => Token::Mat4,
                "mediump" => Token::Mediump,
                "out" => Token::Out,
                "precision" => Token::Precision,
                "return" => Token::Return,
                "sampler2D" => Token::Sampler2D,
                "samplerCube" => Token::SamplerCube,
                "struct" => Token::Struct,
                "true" => Token::BoolConstant(true),
                "uniform" => Token::Uniform,
                "varying" => Token::Varying,
                "vec2" => Token::Vec2,
                "vec3" => Token::Vec3,
                "vec4" => Token::Vec4,
                "void" => Token::Void,
                "while" => Token::While,
                _ => Token::Identifier,
            },
        ))
    }

    fn read_hex_integer_literal(&mut self) -> Result<TokenWithSpan> {
        let span = self.begin_span();
        self.skip_char_2();
        let mut string = String::new();
        if let Some(ch) = self.read_char_if(|ch| ch.is_hex_digit()) {
            string.push(ch);
            self.read_chars_while(&mut string, |ch| ch.is_hex_digit());
        } else {
            return Err(self.error(ErrorKind::MissingHexDigits));
        }
        Ok(span.end(
            self,
            Token::IntConstant(i32::from_str_radix(&string, 16).unwrap()),
        ))
    }

    fn read_octal_integer_literal(&mut self) -> TokenWithSpan {
        let span = self.begin_span();
        self.skip_char();
        let mut string = String::new();
        self.read_chars_while(&mut string, |ch| ch.is_octal_digit());
        span.end(
            self,
            Token::IntConstant(i32::from_str_radix(&string, 8).unwrap()),
        )
    }

    fn read_decimal_literal(&mut self) -> Result<TokenWithSpan> {
        let span = self.begin_span();
        let mut string = String::new();
        if let Some(ch) = self.read_char_if(|ch| ch.is_decimal_digit()) {
            string.push(ch);
            self.read_chars_while(&mut string, |ch| ch.is_decimal_digit());
        } else {
            string.push('0');
        }
        let has_fractional_part = if let Some(ch) = self.read_char_if(|ch| ch == '.') {
            string.push(ch);
            self.read_chars_while(&mut string, |ch| ch.is_decimal_digit());
            true
        } else {
            false
        };
        let has_exponent_part = if let Some(ch) = self.read_char_if(|ch| ch == 'E' || ch == 'e') {
            string.push(ch);
            if let Some(ch) = self.read_char_if(|ch| ch == '+' || ch == '-') {
                string.push(ch);
            }
            if let Some(ch) = self.read_char_if(|ch| ch.is_decimal_digit()) {
                string.push(ch);
                self.read_chars_while(&mut string, |ch| ch.is_decimal_digit());
            } else {
                return Err(self.error(ErrorKind::MissingExponent));
            }
            true
        } else {
            false
        };
        if has_fractional_part || has_exponent_part {
            self.skip_char_if(|ch| ch == 'F' || ch == 'f');
            Ok(span.end(self, Token::FloatConstant(string.parse::<f32>().unwrap())))
        } else {
            Ok(span.end(self, Token::IntConstant(string.parse::<i32>().unwrap())))
        }
    }

    fn read_punctuator(&mut self, token: Token) -> TokenWithSpan {
        let span = self.begin_span();
        self.skip_char();
        span.end(self, token)
    }

    fn read_punctuator_2(&mut self, token: Token) -> TokenWithSpan {
        let span = self.begin_span();
        self.skip_char_2();
        span.end(self, token)
    }

    fn begin_span(&self) -> SpanTracker {
        SpanTracker { start: self.index }
    }

    fn error(&self, kind: ErrorKind) -> Error {
        Error {
            index: self.index,
            kind,
        }
    }

    fn peek_char(&self) -> char {
        self.chars.get(self.index).cloned().unwrap_or('\0')
    }

    fn peek_char_2(&self) -> (char, char) {
        (
            self.chars.get(self.index).cloned().unwrap_or('\0'),
            self.chars.get(self.index + 1).cloned().unwrap_or('\0'),
        )
    }

    fn skip_char(&mut self) {
        self.index += 1;
    }

    fn skip_char_2(&mut self) {
        self.index += 2;
    }

    fn skip_char_if<F>(&mut self, mut f: F) -> bool
    where
        F: FnMut(char) -> bool,
    {
        let ch = self.peek_char();
        if !f(ch) {
            return false;
        }
        self.skip_char();
        true
    }

    fn read_char(&mut self) -> char {
        let ch = self.peek_char();
        self.skip_char();
        ch
    }

    fn read_char_if<F>(&mut self, mut f: F) -> Option<char>
    where
        F: FnMut(char) -> bool,
    {
        let ch = self.peek_char();
        if !f(ch) {
            return None;
        }
        self.skip_char();
        Some(ch)
    }

    fn read_chars_while<F>(&mut self, string: &mut String, mut f: F)
    where
        F: FnMut(char) -> bool,
    {
        while let Some(ch) = self.read_char_if(&mut f) {
            string.push(ch);
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Error {
    index: usize,
    kind: ErrorKind,
}

#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum ErrorKind {
    UnterminatedComment,
    MissingHexDigits,
    MissingExponent,
    IllegalChar(char),
}

#[derive(Clone, Debug)]
struct SpanTracker {
    start: usize,
}

impl SpanTracker {
    fn end(self, lexer: &mut Lexer, token: Token) -> TokenWithSpan {
        TokenWithSpan::from_inner_and_span(token, Span::new(self.start, lexer.index))
    }
}
