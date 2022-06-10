use super::tokenizer::*;
use crate::a2ml::*;

struct TokenIter<'a> {
    tokens: &'a [A2lToken],
    pos: usize,
}

pub struct ParserState<'a> {
    token_cursor: TokenIter<'a>,
    filedata: &'a [String],
    filenames: &'a [String],
    last_token_position: u32,
    sequential_id: u32,
    pub(crate) log_msgs: &'a mut Vec<String>,
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

#[derive(Debug)]
pub enum ParseError {
    UnexpectedTokenType(ParseContext, A2lTokenType, String, A2lTokenType),
    MalformedNumber(ParseContext, String),
    InvalidEnumValue(ParseContext, String),
    InvalidMultiplicityTooMany(ParseContext, String),
    InvalidMultiplicityNotPresent(ParseContext, String),
    IncorrectElemType(ParseContext, String, bool),
    IncorrectEndTag(ParseContext, String),
    UnknownSubBlock(ParseContext, String),
    UnexpectedEOF(ParseContext),
    StringTooLong(ParseContext, String, usize, usize),
    BlockRefDeprecated(ParseContext, String, f32, f32),
    BlockRefTooNew(ParseContext, String, f32, f32),
    EnumRefDeprecated(ParseContext, String, f32, f32),
    EnumRefTooNew(ParseContext, String, f32, f32),
    InvalidBegin(ParseContext),
    A2mlError(String),
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
    pub fn new<'b>(
        tokens: &'b [A2lToken],
        filedata: &'b [String],
        filenames: &'b [String],
        log_msgs: &'b mut Vec<String>,
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
    pub fn get_token(&mut self, context: &ParseContext) -> Result<&'a A2lToken, ParseError> {
        if let Some(token) = self.token_cursor.next() {
            self.last_token_position = token.line;
            Ok(token)
        } else {
            Err(ParseError::UnexpectedEOF(context.clone()))
        }
    }

    pub fn undo_get_token(&mut self) {
        self.token_cursor.back();
    }

    pub(crate) fn peek_token(&mut self) -> Option<&'a A2lToken> {
        self.token_cursor.peek()
    }

    pub(crate) fn log_warning(&mut self, parse_error: ParseError) {
        self.log_msgs
            .push(self.stringify_parse_error(&parse_error, false));
    }

    pub fn error_or_log(&mut self, err: ParseError) -> Result<(), ParseError> {
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

    pub fn set_file_version(&mut self, major: u16, minor: u16) -> Result<(), String> {
        self.file_ver = major as f32 + (minor as f32 / 100.0);
        Ok(())
    }

    pub fn check_block_version(
        &mut self,
        context: &ParseContext,
        tag: &str,
        min_ver: f32,
        max_ver: f32,
    ) -> Result<(), ParseError> {
        if self.file_ver < min_ver {
            self.error_or_log(ParseError::BlockRefTooNew(
                context.clone(),
                tag.to_string(),
                self.file_ver,
                min_ver,
            ))?;
        } else if self.file_ver > max_ver {
            self.log_warning(ParseError::BlockRefDeprecated(
                context.clone(),
                tag.to_string(),
                self.file_ver,
                max_ver,
            ));
        }
        Ok(())
    }

    pub fn check_enumitem_version(
        &mut self,
        context: &ParseContext,
        tag: &str,
        min_ver: f32,
        max_ver: f32,
    ) -> Result<(), ParseError> {
        if self.file_ver < min_ver {
            self.error_or_log(ParseError::EnumRefTooNew(
                context.clone(),
                tag.to_string(),
                self.file_ver,
                min_ver,
            ))?;
        } else if self.file_ver > max_ver {
            self.log_warning(ParseError::EnumRefDeprecated(
                context.clone(),
                tag.to_string(),
                self.file_ver,
                max_ver,
            ));
        }
        Ok(())
    }

    // expect_token get a token which has to be of a particular type (hence: expect)
    // getting a token of any other type is a ParseError
    pub fn expect_token(
        &mut self,
        context: &ParseContext,
        token_type: A2lTokenType,
    ) -> Result<&'a A2lToken, ParseError> {
        let token = self.get_token(context)?;

        if token.ttype != token_type {
            let text = self.get_token_text(token);
            return Err(ParseError::UnexpectedTokenType(
                context.clone(),
                token.ttype.clone(),
                text.to_string(),
                token_type,
            ));
        }

        Ok(token)
    }

    // get_string()
    // Get the content of a String token as a string
    pub fn get_string(&mut self, context: &ParseContext) -> Result<String, ParseError> {
        let text = if let Some(A2lToken {
            ttype: A2lTokenType::Identifier,
            ..
        }) = self.peek_token()
        {
            // an identifier can be used in place of a string, if the parser is not strict
            let text = self.get_identifier(context)?;
            self.error_or_log(ParseError::UnexpectedTokenType(
                context.clone(),
                A2lTokenType::Identifier,
                text.clone(),
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
    ) -> Result<String, ParseError> {
        let text = self.get_string(context)?;
        if text.len() > maxlen {
            self.error_or_log(ParseError::StringTooLong(
                context.clone(),
                text.clone(),
                maxlen,
                text.len(),
            ))?
        }
        Ok(text)
    }

    // get_identifier()
    // Get the content of an Identifier token as a string
    pub fn get_identifier(&mut self, context: &ParseContext) -> Result<String, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Identifier)?;
        let text = self.get_token_text(token);
        Ok(String::from(text))
    }

    // get_float()
    // Get the content of a Number token as a float
    // Since the Number token stores text internally, the text must be converted first
    pub fn get_float(&mut self, context: &ParseContext) -> Result<f32, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        match text.parse::<f32>() {
            Ok(num) => Ok(num),
            Err(_) => Err(ParseError::MalformedNumber(
                context.clone(),
                text.to_string(),
            )),
        }
    }

    // get_double()
    // Get the content of a Number token as a double(f64)
    // Since the Number token stores text internally, the text must be converted first
    pub fn get_double(&mut self, context: &ParseContext) -> Result<f64, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        match text.parse::<f64>() {
            Ok(num) => Ok(num),
            Err(_) => Err(ParseError::MalformedNumber(
                context.clone(),
                text.to_string(),
            )),
        }
    }

    // All the following get_integer_<foo> functions were supposed to be one generic function.
    // I was unable to make that work; the stumbling block is the possiblity for signed data
    // that is represented as hex to have the high bit set.
    // E.g. 0xffff is considered to be a valid signed int (i16); it is -1 in decimal notation.
    // I did not manage to decode this in a generic manner.

    pub fn get_integer_i8(&mut self, context: &ParseContext) -> Result<(i8, bool), ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u8::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num as i8, true)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        }
    }

    pub fn get_integer_u8(&mut self, context: &ParseContext) -> Result<(u8, bool), ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u8::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num, true)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        }
    }

    pub fn get_integer_i16(&mut self, context: &ParseContext) -> Result<(i16, bool), ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u16::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num as i16, true)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        }
    }

    pub fn get_integer_u16(&mut self, context: &ParseContext) -> Result<(u16, bool), ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u16::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num, true)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        }
    }

    pub fn get_integer_i32(&mut self, context: &ParseContext) -> Result<(i32, bool), ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u32::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num as i32, true)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        }
    }

    pub fn get_integer_u32(&mut self, context: &ParseContext) -> Result<(u32, bool), ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u32::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num, true)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        }
    }

    pub fn get_integer_u64(&mut self, context: &ParseContext) -> Result<(u64, bool), ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u64::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num, true)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        }
    }

    pub fn get_integer_i64(&mut self, context: &ParseContext) -> Result<(i64, bool), ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u64::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok((num as i64, true)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        } else {
            match text.parse() {
                Ok(num) => Ok((num, false)),
                Err(_) => Err(ParseError::MalformedNumber(
                    context.clone(),
                    text.to_string(),
                )),
            }
        }
    }

    // get_next_tag()
    // get the tag of the next item of a taggedstruct or taggedunion
    pub fn get_next_tag(
        &mut self,
        context: &ParseContext,
    ) -> Result<Option<(&'a A2lToken, bool, u32)>, ParseError> {
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
                    let errtext = self.get_token_text(token);
                    Err(ParseError::UnexpectedTokenType(
                        context.clone(),
                        token.ttype.clone(),
                        errtext.to_string(),
                        A2lTokenType::Identifier,
                    ))
                } else {
                    Err(ParseError::UnexpectedEOF(context.clone()))
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
    ) -> Result<(), ParseError> {
        self.error_or_log(ParseError::UnknownSubBlock(
            context.clone(),
            item_tag.to_string(),
        ))?;
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
                                return Err(ParseError::IncorrectEndTag(
                                    errcontext,
                                    text.to_string(),
                                ));
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
                        return Err(ParseError::IncorrectEndTag(errcontext, text.to_string()));
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
    ) -> Result<(), ParseError> {
        if is_error {
            self.error_or_log(ParseError::InvalidMultiplicityTooMany(
                context.clone(),
                tag.to_string(),
            ))?;
        }
        Ok(())
    }

    // stringify_parse_error()
    // Generate error messages for the various parse errors
    // Handling the error info this way opens up the possibility of passing ParseError data to the caller of the parser
    pub fn stringify_parse_error(&self, err: &ParseError, is_err: bool) -> String {
        let prefix = if is_err { "Error" } else { "Warning" };
        match err {
            ParseError::UnexpectedTokenType(context, actual_ttype, actual_text, expected_ttype) => {
                format!("{} on line {}: expected token of type {:?}, got {:?} (\"{}\") inside block {} starting on line {}", prefix, self.last_token_position, expected_ttype, actual_ttype, actual_text, context.element, context.line)
            }
            ParseError::MalformedNumber(_context, numstr) => {
                format!("{} on line {}: string \"{}\" could not be interpreted as a number", prefix, self.last_token_position, numstr)
            }
            ParseError::InvalidEnumValue(context, enval) => {
                format!("{} on line {}: expected an enum value, but \"{}\" is not part of the enum (located inside block {} starting on line {})", prefix, self.last_token_position, enval, context.element, context.line)
            }
            ParseError::InvalidMultiplicityTooMany(context, tag) => {
                format!("{} on line {}: optional element {} occurs too often within block {} starting on line {}", prefix, self.last_token_position, tag, context.element, context.line)
            }
            ParseError::InvalidMultiplicityNotPresent(context, tag) => {
                format!("{} on line {}: element {} is missing in block {} starting on line {}", prefix, self.last_token_position, tag, context.element, context.line)
            }
            ParseError::IncorrectElemType(context, tag, is_block) => {
                match is_block {
                    true => format!("{} on line {}: element {} in block {} starting on line {} must be enclosed in /begin and /end", prefix, self.last_token_position, tag, context.element, context.line),
                    false => format!("{} on line {}: element {} in block {} starting on line {} must not be enclosed in /begin and /end", prefix, self.last_token_position, tag, context.element, context.line)
                }
            }
            ParseError::UnexpectedEOF(context) => {
                format!("{} on line {}: encountered end of input while not done parsing block {} starting on line {}", prefix, self.last_token_position, context.element, context.line)
            }
            ParseError::UnknownSubBlock(context, tag) => {
                format!("{} on line {}: Unknown sub-block {} found inside block {} starting on line {}", prefix, self.last_token_position, tag, context.element, context.line)
            }
            ParseError::IncorrectEndTag(context, tag) => {
                format!("{} on line {}: Wrong end tag {} found at the end of block {} starting on line {}", prefix, self.last_token_position, tag, context.element, context.line)
            }
            ParseError::StringTooLong(context, text, maxlen, actual_len) => {
                format!("{} on line {}: String \"{}\" in block {} is {} bytes long, but the maximum allowed length is {}", prefix, self.last_token_position, text, context.element, actual_len, maxlen)
            }
            ParseError::BlockRefTooNew(context, tag, file_version, min_version) => {
                format!("{} on line {}: Sub-block \"{}\" in block {} is available from version {:.2}, but the file declares version {:.2}", prefix, self.last_token_position, tag, context.element, min_version, file_version)
            }
            ParseError::BlockRefDeprecated(context, tag, file_version, max_version) => {
                format!("{} on line {}: Sub-block \"{}\" in block {} is deprecated after version {:.2}, but the file declares version {:.2}", prefix, self.last_token_position, tag, context.element, max_version, file_version)
            }
            ParseError::EnumRefTooNew(context, tag, file_version, min_version) => {
                format!("{} on line {}: enum item \"{}\" in block {} is available from version {:.2}, but the file declares version {:.2}", prefix, self.last_token_position, tag, context.element, min_version, file_version)
            }
            ParseError::EnumRefDeprecated(context, tag, file_version, max_version) => {
                format!("{} on line {}: enum item \"{}\" in block {} is deprecated after version {:.2}, but the file declares version {:.2}", prefix, self.last_token_position, tag, context.element, max_version, file_version)
            }
            ParseError::InvalidBegin(context) => {
                format!("{} on line {}: /begin in block {} is not followed by a valid tag", prefix, self.last_token_position, context.element)
            }
            ParseError::A2mlError(errmsg) => {
                format!("{} in A2ML block starting on line {}: {}", prefix, self.last_token_position, errmsg)
            }
        }
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

#[test]
fn parsing_numbers_test() {
    let input_text = r##"0 0x1 1.0e+2 1000 0 0.1 0x11 1.0e+2"##;
    let tokenresult = super::tokenizer::tokenize("test_input".to_owned(), 0, input_text);
    assert!(tokenresult.is_ok());

    let tokenresult = tokenresult.unwrap();
    let mut log_msgs = Vec::<String>::new();
    let mut parser = ParserState::new(
        &tokenresult.tokens,
        &tokenresult.filedata,
        &tokenresult.filenames,
        &mut log_msgs,
        true,
    );
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
