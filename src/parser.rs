use thiserror::Error;

use crate::a2ml::*;
use crate::tokenizer::*;
use crate::A2lError;

struct TokenIter<'a> {
    tokens: &'a [A2lToken],
    pos: usize,
}

pub struct ParserState<'a> {
    token_cursor: TokenIter<'a>,
    filedata: &'a [String],
    pub(crate) filenames: &'a [String],
    pub(crate) last_token_position: u32,
    sequential_id: u32,
    pub(crate) log_msgs: &'a mut Vec<A2lError>,
    strict: bool,
    file_ver: f32,
    pub(crate) builtin_a2mlspec: Option<A2mlTypeSpec>,
    pub(crate) file_a2mlspec: Option<A2mlTypeSpec>,
}

/// describes the current parser context, giving the name of the current element and its file and line number
#[derive(Debug, Clone)]
pub struct ParseContext {
    pub element: String,
    pub fileid: usize,
    pub line: u32,
    pub inside_block: bool,
}
#[derive(Debug, Error)]
pub enum ParserError {
    #[error("{filename}:{error_line}: expected token of type {expected_ttype:?}, got {actual_ttype:?} (\"{actual_text}\") inside block {element} starting on line {block_line}")]
    UnexpectedTokenType {
        filename: String,
        error_line: u32,
        block_line: u32,
        element: String,
        actual_ttype: A2lTokenType,
        actual_text: String,
        expected_ttype: A2lTokenType,
    },

    #[error("{filename}:{error_line}: string \"{numstr}\" could not be interpreted as a number")]
    MalformedNumber {
        filename: String,
        error_line: u32,
        numstr: String,
    },

    #[error("{filename}:{error_line}: expected an enum value, but \"{enumtxt}\" is not part of the enum (located inside block {block} starting on line {block_line})")]
    InvalidEnumValue {
        filename: String,
        error_line: u32,
        enumtxt: String,
        block: String,
        block_line: u32,
    },

    #[error("{filename}:{error_line}: optional element {tag} occurs too often within block {block} starting on line {block_line}")]
    InvalidMultiplicityTooMany {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error("{filename}:{error_line}: element {tag} is missing in block {block} starting on line {block_line}")]
    InvalidMultiplicityNotPresent {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error("{filename}:{error_line}: element {tag} in block {block} starting on line {block_line} must be enclosed in /begin and /end")]
    IncorrectBlockError {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error("{filename}:{error_line}: element {tag} in block {block} starting on line {block_line} may not be enclosed in /begin and /end")]
    IncorrectKeywordError {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error("{filename}:{error_line}: Wrong end tag {tag} found at the end of block {block} starting on line {block_line}")]
    IncorrectEndTag {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error("{filename}:{error_line}: Unknown sub-block {tag} found inside block {block} starting on line {block_line}")]
    UnknownSubBlock {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error("{filename}:{error_line}: encountered end of file while not done parsing block {block} starting on line {block_line}")]
    UnexpectedEOF {
        filename: String,
        error_line: u32,
        block: String,
        block_line: u32,
    },

    #[error("{filename}:{error_line}: String \"{text}\" in block {block} is {length} bytes long, but the maximum allowed length is {max_length}")]
    StringTooLong {
        filename: String,
        error_line: u32,
        block: String,
        text: String,
        length: usize,
        max_length: usize,
    },

    #[error("{filename}:{error_line}: Sub-block \"{tag}\" in block {block} is deprecated after version {limit_ver:.2}, but the file declares version {file_ver:.2}")]
    BlockRefDeprecated {
        filename: String,
        error_line: u32,
        block: String,
        tag: String,
        limit_ver: f32,
        file_ver: f32,
    },

    #[error("{filename}:{error_line}: Sub-block \"{tag}\" in block {block} is available from version {limit_ver:.2}, but the file declares version {file_ver:.2}")]
    BlockRefTooNew {
        filename: String,
        error_line: u32,
        block: String,
        tag: String,
        limit_ver: f32,
        file_ver: f32,
    },

    #[error("{filename}:{error_line}: Enum item \"{tag}\" in block {block} is deprecated after version {limit_ver:.2}, but the file declares version {file_ver:.2}")]
    EnumRefDeprecated {
        filename: String,
        error_line: u32,
        block: String,
        tag: String,
        limit_ver: f32,
        file_ver: f32,
    },

    #[error("{filename}:{error_line}: Enum item \"{tag}\" in block {block} is available from version {limit_ver:.2}, but the file declares version {file_ver:.2}")]
    EnumRefTooNew {
        filename: String,
        error_line: u32,
        block: String,
        tag: String,
        limit_ver: f32,
        file_ver: f32,
    },

    #[error("{filename}:{error_line}: /begin in block {block} is not followed by a valid tag")]
    InvalidBegin {
        filename: String,
        error_line: u32,
        block: String,
    },

    #[error("{filename}:{error_line}: A2ML parser reports {errmsg}")]
    A2mlError {
        filename: String,
        error_line: u32,
        errmsg: String,
    },
}

// it pretends to be an Iter, but it really isn't
impl<'a> TokenIter<'a> {
    fn next(&mut self) -> Option<&'a A2lToken> {
        if self.pos < self.tokens.len() {
            let item = &self.tokens[self.pos];
            self.pos += 1;
            Some(item)
        } else {
            None
        }
    }

    fn peek(&mut self) -> Option<&'a A2lToken> {
        if self.pos < self.tokens.len() {
            let item = &self.tokens[self.pos];
            Some(item)
        } else {
            None
        }
    }

    fn back(&mut self) {
        self.pos -= 1;
    }
}

impl<'a> ParserState<'a> {
    pub(crate) fn new<'b>(
        tokenresult: &'b TokenResult,
        log_msgs: &'b mut Vec<A2lError>,
        strict: bool,
    ) -> ParserState<'b> {
        Self::new_internal(
            &tokenresult.tokens,
            &tokenresult.filedata,
            &tokenresult.filenames,
            log_msgs,
            strict,
        )
    }

    pub(crate) fn new_internal<'b>(
        tokens: &'b [A2lToken],
        filedata: &'b [String],
        filenames: &'b [String],
        log_msgs: &'b mut Vec<A2lError>,
        strict: bool,
    ) -> ParserState<'b> {
        ParserState {
            token_cursor: TokenIter { tokens, pos: 0 },
            filedata,
            filenames,
            last_token_position: 0,
            sequential_id: 0,
            log_msgs,
            strict,
            file_ver: 0f32,
            file_a2mlspec: None,
            builtin_a2mlspec: None,
        }
    }

    // get_token
    // get one token from the list of tokens and unwrap it
    pub fn get_token(&mut self, context: &ParseContext) -> Result<&'a A2lToken, ParserError> {
        if let Some(token) = self.token_cursor.next() {
            self.last_token_position = token.line;
            Ok(token)
        } else {
            Err(ParserError::unexpected_eof(self, context))
        }
    }

    pub fn undo_get_token(&mut self) {
        self.token_cursor.back();
    }

    pub(crate) fn peek_token(&mut self) -> Option<&'a A2lToken> {
        self.token_cursor.peek()
    }

    pub(crate) fn log_warning(&mut self, parse_error: ParserError) {
        self.log_msgs.push(A2lError::ParserError {
            parser_error: parse_error,
        });
    }

    pub fn error_or_log(&mut self, err: ParserError) -> Result<(), ParserError> {
        if self.strict {
            Err(err)
        } else {
            self.log_warning(err);
            Ok(())
        }
    }

    pub fn get_tokenpos(&self) -> usize {
        self.token_cursor.pos
    }

    pub fn set_tokenpos(&mut self, newpos: usize) {
        self.token_cursor.pos = newpos;
    }

    pub fn get_token_text(&self, token: &'a A2lToken) -> &'a str {
        let data = &self.filedata[token.fileid];
        &data[token.startpos..token.endpos]
    }

    pub fn get_current_line_offset(&self) -> u32 {
        if self.token_cursor.pos > 0 && self.token_cursor.pos < self.token_cursor.tokens.len() {
            let prev_line = self.token_cursor.tokens[self.token_cursor.pos - 1].line;
            let prev_fileid = self.token_cursor.tokens[self.token_cursor.pos - 1].fileid;
            let cur_line = self.token_cursor.tokens[self.token_cursor.pos].line;
            let cur_fileid = self.token_cursor.tokens[self.token_cursor.pos].fileid;

            // subtracting line numbers is only sane within a file
            if prev_fileid == cur_fileid {
                cur_line - prev_line
            } else {
                // if the tokens come from different files then there is no line offset and the value 2 is used for formatting
                2
            }
        } else {
            self.token_cursor.tokens[0].line - 1
        }
    }

    pub fn get_next_id(&mut self) -> u32 {
        self.sequential_id += 1;
        self.sequential_id
    }

    pub fn get_incfilename(&self, fileid: usize) -> Option<String> {
        if fileid == 0 || fileid >= self.filenames.len() {
            None
        } else {
            Some(self.filenames[fileid].to_owned())
        }
    }

    pub fn set_file_version(&mut self, major: u16, minor: u16) -> Result<(), A2lError> {
        self.file_ver = major as f32 + (minor as f32 / 100.0);
        Ok(())
    }

    pub fn check_block_version(
        &mut self,
        context: &ParseContext,
        tag: &str,
        min_ver: f32,
        max_ver: f32,
    ) -> Result<(), ParserError> {
        if self.file_ver < min_ver {
            self.error_or_log(ParserError::BlockRefTooNew {
                filename: self.filenames[context.fileid].to_owned(),
                error_line: self.last_token_position,
                block: context.element.to_owned(),
                tag: tag.to_string(),
                limit_ver: min_ver,
                file_ver: self.file_ver,
            })?;
        } else if self.file_ver > max_ver {
            self.log_warning(ParserError::BlockRefDeprecated {
                filename: self.filenames[context.fileid].to_owned(),
                error_line: self.last_token_position,
                block: context.element.to_owned(),
                tag: tag.to_string(),
                limit_ver: max_ver,
                file_ver: self.file_ver,
            });
        }
        Ok(())
    }

    pub fn check_enumitem_version(
        &mut self,
        context: &ParseContext,
        tag: &str,
        min_ver: f32,
        max_ver: f32,
    ) -> Result<(), ParserError> {
        if self.file_ver < min_ver {
            self.error_or_log(ParserError::EnumRefTooNew {
                filename: self.filenames[context.fileid].to_owned(),
                error_line: self.last_token_position,
                block: context.element.to_owned(),
                tag: tag.to_string(),
                limit_ver: min_ver,
                file_ver: self.file_ver,
            })?;
        } else if self.file_ver > max_ver {
            self.log_warning(ParserError::EnumRefDeprecated {
                filename: self.filenames[context.fileid].to_owned(),
                error_line: self.last_token_position,
                block: context.element.to_owned(),
                tag: tag.to_string(),
                limit_ver: max_ver,
                file_ver: self.file_ver,
            });
        }
        Ok(())
    }

    // expect_token get a token which has to be of a particular type (hence: expect)
    // getting a token of any other type is a ParserError
    pub fn expect_token(
        &mut self,
        context: &ParseContext,
        token_type: A2lTokenType,
    ) -> Result<&'a A2lToken, ParserError> {
        let token = self.get_token(context)?;

        if token.ttype != token_type {
            return Err(ParserError::unexpected_token_type(
                self, context, token, token_type,
            ));
        }

        Ok(token)
    }

    // get_string()
    // Get the content of a String token as a string
    pub fn get_string(&mut self, context: &ParseContext) -> Result<String, ParserError> {
        let text = if let Some(
            token @ A2lToken {
                ttype: A2lTokenType::Identifier,
                ..
            },
        ) = self.peek_token()
        {
            // an identifier can be used in place of a string, if the parser is not strict
            let text = self.get_identifier(context)?;
            self.error_or_log(ParserError::unexpected_token_type(
                self,
                context,
                token,
                A2lTokenType::String,
            ))?;
            text
        } else {
            let token = self.expect_token(context, A2lTokenType::String)?;
            let mut text = self.get_token_text(token);

            if text.starts_with('\"') {
                text = &text[1..text.len() - 1];
            }

            unescape_string(text)
        };

        Ok(text)
    }

    // get_string_maxlen()
    // Get the content of a String token as a string. Trigger an error if the string is longer than maxlen
    pub fn get_string_maxlen(
        &mut self,
        context: &ParseContext,
        maxlen: usize,
    ) -> Result<String, ParserError> {
        let text = self.get_string(context)?;
        if text.len() > maxlen {
            self.error_or_log(ParserError::StringTooLong {
                filename: self.filenames[context.fileid].to_owned(),
                error_line: self.last_token_position,
                block: context.element.to_owned(),
                text: text.clone(),
                length: text.len(),
                max_length: maxlen,
            })?
        }
        Ok(text)
    }

    // get_identifier()
    // Get the content of an Identifier token as a string
    pub fn get_identifier(&mut self, context: &ParseContext) -> Result<String, ParserError> {
        let token = self.expect_token(context, A2lTokenType::Identifier)?;
        let text = self.get_token_text(token);
        Ok(String::from(text))
    }

    // get_float()
    // Get the content of a Number token as a float
    // Since the Number token stores text internally, the text must be converted first
    pub fn get_float(&mut self, context: &ParseContext) -> Result<f32, ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        match text.parse::<f32>() {
            Ok(num) => Ok(num),
            Err(_) => Err(ParserError::malformed_number(self, context, text)),
        }
    }

    // get_double()
    // Get the content of a Number token as a double(f64)
    // Since the Number token stores text internally, the text must be converted first
    pub fn get_double(&mut self, context: &ParseContext) -> Result<f64, ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        match text.parse::<f64>() {
            Ok(num) => Ok(num),
            Err(_) => Err(ParserError::malformed_number(self, context, text)),
        }
    }

    // All the following get_integer_<foo> functions were supposed to be one generic function.
    // I was unable to make that work; the stumbling block is the possiblity for signed data
    // that is represented as hex to have the high bit set.
    // E.g. 0xffff is considered to be a valid signed int (i16); it is -1 in decimal notation.
    // I did not manage to decode this in a generic manner.

    pub fn get_integer_i8(&mut self, context: &ParseContext) -> Result<(i8, bool), ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u8::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num as i8, true)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    pub fn get_integer_u8(&mut self, context: &ParseContext) -> Result<(u8, bool), ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u8::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num, true)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    pub fn get_integer_i16(&mut self, context: &ParseContext) -> Result<(i16, bool), ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u16::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num as i16, true)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    pub fn get_integer_u16(&mut self, context: &ParseContext) -> Result<(u16, bool), ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u16::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num, true)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    pub fn get_integer_i32(&mut self, context: &ParseContext) -> Result<(i32, bool), ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u32::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num as i32, true)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    pub fn get_integer_u32(&mut self, context: &ParseContext) -> Result<(u32, bool), ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u32::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num, true)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    pub fn get_integer_u64(&mut self, context: &ParseContext) -> Result<(u64, bool), ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u64::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num, true)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    pub fn get_integer_i64(&mut self, context: &ParseContext) -> Result<(i64, bool), ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u64::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num as i64, true)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    // get_next_tag()
    // get the tag of the next item of a taggedstruct or taggedunion
    pub fn get_next_tag(
        &mut self,
        context: &ParseContext,
    ) -> Result<Option<(&'a A2lToken, bool, u32)>, ParserError> {
        let mut is_block = false;
        let tokenpos = self.get_tokenpos();
        let start_offset = self.get_current_line_offset();

        // if the next token is /begin, then set is_block and skip the token
        if let Some(A2lToken {
            ttype: A2lTokenType::Begin,
            ..
        }) = self.token_cursor.peek()
        {
            is_block = true;
            self.get_token(context)?;
        }

        let token = self.token_cursor.next();
        // get the tag or return None if the token is not an Identifier
        if let Some(
            tokenval @ A2lToken {
                ttype: A2lTokenType::Identifier,
                ..
            },
        ) = token
        {
            Ok(Some((tokenval, is_block, start_offset)))
        } else {
            self.set_tokenpos(tokenpos);
            if is_block {
                if let Some(token) = token {
                    // an Identifier must follow after a /begin
                    Err(ParserError::unexpected_token_type(
                        self,
                        context,
                        token,
                        A2lTokenType::Identifier,
                    ))
                } else {
                    Err(ParserError::unexpected_eof(self, context))
                }
            } else {
                // no tag? no problem!
                Ok(None)
            }
        }
    }

    // handle_unknown_taggedstruct_tag
    // perform error recovery if an unknown tag is found inside a taggedstruct and strict parsing is off
    pub fn handle_unknown_taggedstruct_tag(
        &mut self,
        context: &ParseContext,
        item_tag: &str,
        item_is_block: bool,
        stoplist: &[&str],
    ) -> Result<(), ParserError> {
        self.error_or_log(ParserError::unknown_sub_block(self, context, item_tag))?;
        // make sure there actually is a next token by doing get_token() + undo_get_token() rather than peek()
        let _ = self.get_token(context)?;
        self.undo_get_token();
        let startpos = self.get_tokenpos();
        let text = self.get_token_text(&self.token_cursor.tokens[startpos]);
        let errcontext =
            ParseContext::from_token(text, &self.token_cursor.tokens[startpos], item_is_block);

        let mut balance = 0;
        if item_is_block {
            balance = 1;
        }

        loop {
            let token = self.get_token(context)?;
            let text = self.get_token_text(token);
            match token.ttype {
                A2lTokenType::Begin => balance += 1,
                A2lTokenType::End => {
                    balance -= 1;
                    if balance == -1 {
                        self.token_cursor.back();
                        break;
                    }
                }
                A2lTokenType::Identifier => {
                    if item_is_block {
                        // the current ungknown item started with /begin ITEM_TAG, so it must end with /end ITEM_TAG.
                        // the stoplist is not relevant
                        if balance == 0 {
                            if text == item_tag {
                                break;
                            } else {
                                return Err(ParserError::incorrect_end_tag(self, &errcontext, text));
                            }
                        }
                    } else {
                        // this unknown item did not begin with /begin, so the end is reached when either:
                        // - the end tag of the parent block is found (balance == -1)
                        // - a tag belonging to the parent block (on the stoplist) is found (balance == 0)
                        // - the sequence /begin TAG for a tag on the stoplist is encountered (balance == 1)
                        if balance == 0 || balance == 1 {
                            let found = stoplist.iter().find(|entry| **entry == text);
                            if found.is_some() {
                                // found a tag belonging to a different TaggedItem of the parent struct. Put the token back and let the parent handle it
                                self.token_cursor.back();
                                if balance == 1 {
                                    self.token_cursor.back();
                                }
                                break;
                            }
                        }
                    }
                }
                _ => {
                    // once balance == 0 is reachd for a block, the next tag should be an Identifier
                    if item_is_block && balance == 0 {
                        return Err(ParserError::incorrect_end_tag(self, &errcontext, text));
                    }
                    // else: ignore the token
                }
            }
        }

        Ok(())
    }

    pub fn handle_multiplicity_error(
        &mut self,
        context: &ParseContext,
        tag: &str,
        is_error: bool,
    ) -> Result<(), ParserError> {
        if is_error {
            self.error_or_log(ParserError::invalid_multiplicity_too_many(
                self, context, tag,
            ))?;
        }
        Ok(())
    }
}

impl ParseContext {
    pub fn from_token(text: &str, token: &A2lToken, is_block: bool) -> ParseContext {
        ParseContext {
            element: text.to_string(),
            fileid: token.fileid,
            line: token.line,
            inside_block: is_block,
        }
    }
}

impl ParserError {
    pub(crate) fn unexpected_token_type(
        parser: &ParserState,
        context: &ParseContext,
        token: &A2lToken,
        expected_ttype: A2lTokenType,
    ) -> Self {
        Self::UnexpectedTokenType {
            filename: parser.filenames[context.fileid].to_owned(),
            error_line: parser.last_token_position,
            block_line: context.line,
            element: context.element.to_owned(),
            actual_ttype: token.ttype.clone(),
            actual_text: parser.get_token_text(token).to_owned(),
            expected_ttype,
        }
    }

    pub(crate) fn malformed_number(
        parser: &ParserState,
        context: &ParseContext,
        numstr: &str,
    ) -> Self {
        Self::MalformedNumber {
            filename: parser.filenames[context.fileid].to_owned(),
            error_line: parser.last_token_position,
            numstr: numstr.to_owned(),
        }
    }

    pub(crate) fn invalid_enum_value(
        parser: &ParserState,
        context: &ParseContext,
        enumitem: &str,
    ) -> Self {
        Self::InvalidEnumValue {
            filename: parser.filenames[context.fileid].to_owned(),
            error_line: parser.last_token_position,
            enumtxt: enumitem.to_owned(),
            block: context.element.to_owned(),
            block_line: context.line,
        }
    }

    pub(crate) fn invalid_multiplicity_too_many(
        parser: &ParserState,
        context: &ParseContext,
        tag: &str,
    ) -> Self {
        Self::InvalidMultiplicityTooMany {
            filename: parser.filenames[context.fileid].to_owned(),
            error_line: parser.last_token_position,
            tag: tag.to_string(),
            block: context.element.clone(),
            block_line: context.line,
        }
    }

    pub(crate) fn incorrect_end_tag(
        parser: &ParserState,
        context: &ParseContext,
        tag: &str,
    ) -> Self {
        Self::IncorrectEndTag {
            filename: parser.filenames[context.fileid].to_owned(),
            error_line: parser.last_token_position,
            tag: tag.to_owned(),
            block: context.element.to_owned(),
            block_line: context.line,
        }
    }

    pub(crate) fn unknown_sub_block(
        parser: &ParserState,
        context: &ParseContext,
        tag: &str,
    ) -> Self {
        Self::UnknownSubBlock {
            filename: parser.filenames[context.fileid].to_owned(),
            error_line: parser.last_token_position,
            tag: tag.to_owned(),
            block: context.element.to_owned(),
            block_line: context.line,
        }
    }

    pub(crate) fn unexpected_eof(parser: &ParserState, context: &ParseContext) -> Self {
        Self::UnexpectedEOF {
            filename: parser.filenames[context.fileid].to_owned(),
            error_line: parser.last_token_position,
            block: context.element.to_owned(),
            block_line: context.line,
        }
    }
}

fn unescape_string(text: &str) -> String {
    /* first check if any unescaping is needed at all */
    if text.chars().any(|c| c == '\\' || c == '"') {
        let input_chars: Vec<char> = text.chars().collect();
        let mut output_chars = Vec::<char>::new();

        let mut idx = 1;
        while idx < input_chars.len() {
            if (input_chars[idx - 1] == '\\' || input_chars[idx - 1] == '"')
                && input_chars[idx] == '"'
            {
                output_chars.push('"');
                idx += 1;
            } else if input_chars[idx - 1] == '\\' && input_chars[idx] == '\'' {
                output_chars.push('\'');
                idx += 1;
            } else if input_chars[idx - 1] == '\\' && input_chars[idx] == '\\' {
                output_chars.push('\\');
                idx += 1;
            } else if input_chars[idx - 1] == '\\' && input_chars[idx] == 'n' {
                output_chars.push('\n');
                idx += 1;
            } else if input_chars[idx - 1] == '\\' && input_chars[idx] == 'r' {
                output_chars.push('\r');
                idx += 1;
            } else if input_chars[idx - 1] == '\\' && input_chars[idx] == 't' {
                output_chars.push('\t');
                idx += 1;
            } else {
                output_chars.push(input_chars[idx - 1]);
            }

            idx += 1;
        }
        if idx == input_chars.len() {
            output_chars.push(input_chars[idx - 1]);
        }

        output_chars.iter().collect()
    } else {
        text.to_owned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parsing_numbers_test() {
        let input_text = r##"0 0x1 1.0e+2 1000 0 0.1 0x11 1.0e+2"##;
        let tokenresult = super::super::tokenizer::tokenize("test_input".to_owned(), 0, input_text);
        assert!(tokenresult.is_ok());

        let tokenresult = tokenresult.unwrap();
        let mut log_msgs = Vec::<A2lError>::new();
        let mut parser = ParserState::new(&tokenresult, &mut log_msgs, true);
        let context = ParseContext {
            element: "TEST".to_string(),
            fileid: 0,
            line: 0,
            inside_block: true,
        };

        // uint8: 0
        let res = parser.get_integer_u8(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, (0, false));

        // uint8: 0x1
        let res = parser.get_integer_u8(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, (1, true));

        // uint8: 1.0e+2
        let res = parser.get_integer_u8(&context);
        assert!(res.is_err());

        // uint8: 257
        let res = parser.get_integer_u8(&context);
        assert!(res.is_err());

        // float: 0
        let res = parser.get_float(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, 0f32);

        // float: 0.1
        let res = parser.get_float(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, 0.1f32);

        // float: 0x11
        let res = parser.get_float(&context);
        assert!(res.is_err());

        // float: 1.0e+2
        let res = parser.get_float(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, 100f32);
    }

    #[test]
    fn test_unescape_string() {
        // no escape
        let result = unescape_string(" ");
        assert_eq!(result, " ");
        // "" -> "
        let result = unescape_string(r#""""#);
        assert_eq!(result, r#"""#);
        // \" -> "
        let result = unescape_string(r#"\""#);
        assert_eq!(result, r#"""#);
        // \\ -> \
        let result = unescape_string(r#"\\"#);
        assert_eq!(result, r#"\"#);
        // \n -> (newline)
        let result = unescape_string(r#"\n"#);
        assert_eq!(result, "\n");
        // \r -> (carriage return)
        let result = unescape_string(r#"\r"#);
        assert_eq!(result, "\r");
        // \t -> (tab)
        let result = unescape_string(r#"\t"#);
        assert_eq!(result, "\t");
    }
}
