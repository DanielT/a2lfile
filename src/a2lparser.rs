use std::collections::HashMap;

use crate::a2ml::*;
use super::a2ltokenizer::*;

struct TokenIter<'a> {
    tokens: &'a Vec<A2lToken>,
    pos: usize
}

pub struct ParserState<'a> {
    token_cursor: TokenIter<'a>,
    filedata: &'a Vec<String>,
    filenames: &'a Vec<String>,
    last_token_position: u32,
    pub(crate) logger: &'a mut dyn super::Logger,
    strict: bool,
    file_ver: f32,
    pub(crate) builtin_a2mlspec: Option<A2mlTypeSpec>,
    pub(crate) file_a2mlspec: Option<A2mlTypeSpec>
}

/// describes the current parser context, giving the name of the current element and its file and line number
#[derive(Debug, Clone)]
pub struct ParseContext {
    pub element: String,
    pub fileid: usize,
    pub line: u32,
    pub inside_block: bool
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
    EnumRefTooNew(ParseContext, String, f32, f32)
}


// it pretends to be an Iter, but it really isn't
impl<'a> TokenIter<'a> {
    fn next(self: &mut Self) -> Option<&'a A2lToken> {
        if self.pos < self.tokens.len() {
            let item = &self.tokens[self.pos];
            self.pos += 1;
            Some(item)
        } else {
            None
        }
    }

    fn peek(self: &mut Self) -> Option<&'a A2lToken> {
        if self.pos < self.tokens.len() {
            let item = &self.tokens[self.pos];
            Some(item)
        } else {
            None
        }
    }

    fn back(self: &mut Self) {
        self.pos -= 1;
    }
}


impl<'a> ParserState<'a> {
    pub fn new<'b>(tokens: &'b Vec<A2lToken>, filedata: &'b Vec<String>, filenames: &'b Vec<String>, logger: &'b mut dyn super::Logger, strict: bool) -> ParserState<'b> {
        ParserState {
            token_cursor: TokenIter{ tokens, pos: 0},
            filedata,
            filenames: filenames,
            last_token_position: 0,
            logger,
            strict,
            file_ver: 0f32,
            file_a2mlspec: None,
            builtin_a2mlspec: None
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
        self.logger.log_message(self.stringify_parse_error(&parse_error, false));
    }

    
    pub fn error_or_log(&mut self, err: ParseError) -> Result<(), ParseError> {
        if self.strict {
            Err(err)
        } else {
            self.log_warning(err);
            Ok(())
        }
    }

    
    pub fn get_tokenpos(self: &ParserState<'a>) -> usize {
        self.token_cursor.pos
    }


    pub fn set_tokenpos(self: &mut ParserState<'a>, newpos: usize) {
        self.token_cursor.pos = newpos;
    }


    pub fn get_token_text(self: &ParserState<'a>, token: &'a A2lToken) -> &'a str {
        let data = &self.filedata[token.fileid];
        &data[token.startpos .. token.endpos]
    }


    pub fn get_current_line(&self) -> u32 {
        if self.token_cursor.pos < self.token_cursor.tokens.len() {
            self.token_cursor.tokens[self.token_cursor.pos].line
        } else {
            if self.token_cursor.tokens.len() > 0 {
                self.token_cursor.tokens[self.token_cursor.tokens.len() - 1].line
            } else {
                0
            }
        }
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


    pub fn check_block_version(&mut self, context: &ParseContext, tag: &str, min_ver: f32, max_ver: f32) -> Result<(), ParseError> {
        if self.file_ver < min_ver {
            self.error_or_log(ParseError::BlockRefTooNew(context.clone(), tag.to_string(), self.file_ver, min_ver))?;
        } else if self.file_ver > max_ver {
            self.log_warning(ParseError::BlockRefDeprecated(context.clone(), tag.to_string(), self.file_ver, max_ver));
        }
        Ok(())
    }


    pub fn check_enumitem_version(&mut self, context: &ParseContext, tag: &str, min_ver: f32, max_ver: f32) -> Result<(), ParseError> {
        if self.file_ver < min_ver {
            self.error_or_log(ParseError::EnumRefTooNew(context.clone(), tag.to_string(), self.file_ver, min_ver))?;
        } else if self.file_ver > max_ver {
            self.log_warning(ParseError::EnumRefDeprecated(context.clone(), tag.to_string(), self.file_ver, max_ver));
        }
        Ok(())
    }


    // expect_token get a token which has to be of a particular type (hence: expect)
    // getting a token of any other type is a ParseError
    pub fn expect_token(&mut self, context: &ParseContext, token_type: A2lTokenType) -> Result<&'a A2lToken, ParseError> {
        let token = self.get_token(context)?;

        if token.ttype != token_type {
            let text = self.get_token_text(token);
            return Err(ParseError::UnexpectedTokenType(context.clone(), token.ttype.clone(), text.to_string(), token_type));
        }

        Ok(token)
    }


    // get_string()
    // Get the content of a String token as a string
    pub fn get_string(&mut self, context: &ParseContext) -> Result<String, ParseError> {
        let text = if let Some(A2lToken {ttype: A2lTokenType::Identifier, ..}) = self.peek_token() {
            // an identifier can be used in place of a string, if the parser is not strict
            let text = self.get_identifier(context)?;
            self.error_or_log(ParseError::UnexpectedTokenType(context.clone(), A2lTokenType::Identifier, text.clone(), A2lTokenType::String))?;
            text
        } else {
            let token = self.expect_token(context, A2lTokenType::String)?;
            let mut text = self.get_token_text(token);

            if text.starts_with("\"") {
                text = &text[1..text.len() - 1];
            }

            unescape_string(text)
        };
        
        Ok(text)
    }


    // get_string_maxlen()
    // Get the content of a String token as a string. Trigger an error if the string is longer than maxlen
    pub fn get_string_maxlen(&mut self, context: &ParseContext, maxlen: usize) -> Result<String, ParseError> {
        let text = self.get_string(context)?;
        if text.len() > maxlen {
            self.error_or_log(ParseError::StringTooLong(context.clone(), text.clone(), maxlen, text.len()))?
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
            Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
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
            Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
        }
    }


    // All the following get_integer_<foo> functions were supposed to be one generic function.
    // I was unable to make that work; the stumbling block is the possiblity for signed data
    // that is represented as hex to have the high bit set.
    // E.g. 0xffff is considered to be a valid signed int (i16); it is -1 in decimal notation.
    // I did not manage to decode this in a generic manner.


    pub fn get_integer_i8(&mut self, context: &ParseContext) -> Result<i8, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u8::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num as i8),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }        
        } else {
            match text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }
        }
    }


    pub fn get_integer_u8(&mut self, context: &ParseContext) -> Result<u8, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u8::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }        
        } else {
            match text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }
        }
    }


    pub fn get_integer_i16(&mut self, context: &ParseContext) -> Result<i16, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u16::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num as i16),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }        
        } else {
            match text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }
        }
    }


    pub fn get_integer_u16(&mut self, context: &ParseContext) -> Result<u16, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u16::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }        
        } else {
            match text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }
        }
    }


    pub fn get_integer_i32(&mut self, context: &ParseContext) -> Result<i32, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u32::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num as i32),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }        
        } else {
            match text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }
        }
    }


    pub fn get_integer_u32(&mut self, context: &ParseContext) -> Result<u32, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u32::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }        
        } else {
            match text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }
        }
    }
    

    pub fn get_integer_u64(&mut self, context: &ParseContext) -> Result<u64, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u64::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }        
        } else {
            match text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }
        }
    }


    pub fn get_integer_i64(&mut self, context: &ParseContext) -> Result<i64, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u64::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num as i64),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }        
        } else {
            match text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), text.to_string()))
            }
        }
    }


    // get_next_tag()
    // get the tag of the next item of a taggedstruct or taggedunion
    pub fn get_next_tag(&mut self, context: &ParseContext) -> Result<Option<(&'a A2lToken, bool)>, ParseError> {
        let mut is_block = false;
        let tokenpos = self.get_tokenpos();

        // if the next token is /begin, then set is_block and skip the token
        if let Some(A2lToken { ttype: A2lTokenType::Begin, ..}) = self.token_cursor.peek() {
            is_block = true;
            self.get_token(context)?;
        }

        let token = self.token_cursor.next();
        // get the tag or return None if the token is not an Identifier
        if let Some(A2lToken { ttype: A2lTokenType::Identifier, ..}) = token {
            Ok(Some((token.unwrap(), is_block)))
        } else {
            self.set_tokenpos(tokenpos);
            if is_block {
                if let Some(token) = token {
                    // an Identifier must follow after a /begin
                    let errtext = self.get_token_text(token);
                    Err(ParseError::UnexpectedTokenType(context.clone(), token.ttype.clone(), errtext.to_string(), A2lTokenType::Identifier))
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
    pub fn handle_unknown_taggedstruct_tag(&mut self, context: &ParseContext, item_tag: &str, item_is_block: bool, stoplist: &[&str]) -> Result<(), ParseError> {
        self.error_or_log(ParseError::UnknownSubBlock(context.clone(), item_tag.to_string()))?;
        let startpos = self.get_tokenpos();
        let text = self.get_token_text(&self.token_cursor.tokens[startpos]);
        let errcontext = ParseContext::from_token(text, &self.token_cursor.tokens[startpos], item_is_block);
    
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
                                return Err(ParseError::IncorrectEndTag(errcontext, text.to_string()));
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


    // stringify_parse_error()
    // Generate error messages for the various parse errors
    // Handling the error info this way opens up the possibility of passing ParseError data to the caller of the parser
    pub fn stringify_parse_error(&self, err: &ParseError, is_err: bool) -> String {
        let prefix = if is_err { "Error"} else {"Warning"};
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
        }
    }


    // parse_ifdata()
    // entry point for ifdata parsing.
    // Three attmpts to parse the data will be made:
    // 1) parse according to the built-in spec provided using the a2ml_specification! macro
    // 2) parse according to the spec in the A2ML block
    // 3) fallback parsing using parse_unknown_ifdata()
    pub (crate) fn parse_ifdata(&mut self, context: &ParseContext) -> Result<Option<GenericIfData>, ParseError> {
        let mut result = None;
        // is there any content in the IF_DATA?
        if let Some(token) = self.peek_token() {
            if token.ttype != A2lTokenType::End {
                // try parsing according to the spec provided by the user of the crate in the a2ml_specification! macro
                let spec = std::mem::take(&mut self.builtin_a2mlspec);
                if let Some(a2mlspec) = &spec {
                    if let Some(ifdata_items) = self.parse_ifdata_from_spec(context, a2mlspec) {
                        result = Some(ifdata_items);
                    }
                }
                self.builtin_a2mlspec = spec;

                // no built in spec, or parsing using that spec failed
                if result.is_none() {
                    // try parsing according to the spec inside the file
                    let spec = std::mem::take(&mut self.file_a2mlspec);
                    if let Some(a2mlspec) = &spec {
                        if let Some(ifdata_items) = self.parse_ifdata_from_spec(context, a2mlspec) {
                            result = Some(ifdata_items);
                        }
                    }
                    self.file_a2mlspec = spec;
                }

                if result.is_none() {
                    // this will succeed if the data format follows the basic a2l rules (e.g. matching /begin and /end)
                    // if it does not, a ParseErrror is generated
                    result = Some(self.parse_unknown_ifdata(context, true)?);
                }
            }
        }

        Ok(result)
    }


    // parse_ifdata_from_spec()
    // parse the items of an IF_DATA block according to a spec.
    // If parsing fails, the token cursor is set back to the beginning of the input so that parsing can be retried
    fn parse_ifdata_from_spec(&mut self, context: &ParseContext, spec: &A2mlTypeSpec) -> Option<GenericIfData> {
        let pos = self.get_tokenpos();
        if let Ok(ifdata) = self.parse_ifdata_item(context, spec) {
            if let Some(A2lToken {ttype: A2lTokenType::End, ..}) = self.peek_token() {
                Some(ParserState::parse_ifdata_make_block(ifdata, self.get_incfilename(context.fileid), context.line))
            } else {
                // parsed some (or maybe none if the spec allows this!), but not all elements of the IF_DATA.
                // put the token_cursor back at the beginning of the IF_DATA input
                self.set_tokenpos(pos);
                None
            }
        } else {
            // put the token_cursor back at the beginning of the IF_DATA input
            self.set_tokenpos(pos);
            None
        }
    }


    // parse_ifdata_item()
    // parse one item together with all of its dependent elements according to an A2mlTypeSpec
    fn parse_ifdata_item(&mut self, context: &ParseContext, spec: &A2mlTypeSpec) -> Result<GenericIfData, ParseError> {
        Ok(match spec {
            A2mlTypeSpec::None => { GenericIfData::None }
            A2mlTypeSpec::Char => { GenericIfData::Char(self.get_current_line(), self.get_integer_i8(context)?) }
            A2mlTypeSpec::Int => { GenericIfData::Int(self.get_current_line(), self.get_integer_i16(context)?) }
            A2mlTypeSpec::Long => { GenericIfData::Long(self.get_current_line(), self.get_integer_i32(context)?) }
            A2mlTypeSpec::Int64 => { GenericIfData::Int64(self.get_current_line(), self.get_integer_i64(context)?) }
            A2mlTypeSpec::UChar => { GenericIfData::UChar(self.get_current_line(), self.get_integer_u8(context)?) }
            A2mlTypeSpec::UInt => { GenericIfData::UInt(self.get_current_line(), self.get_integer_u16(context)?) }
            A2mlTypeSpec::ULong => { GenericIfData::ULong(self.get_current_line(), self.get_integer_u32(context)?) }
            A2mlTypeSpec::UInt64 => { GenericIfData::UInt64(self.get_current_line(), self.get_integer_u64(context)?) }
            A2mlTypeSpec::Float => { GenericIfData::Float(self.get_current_line(), self.get_float(context)?) }
            A2mlTypeSpec::Double => { GenericIfData::Double(self.get_current_line(), self.get_double(context)?) }
            A2mlTypeSpec::Array(arraytype, dim) => {
                if **arraytype == A2mlTypeSpec::Char {
                    GenericIfData::String(self.get_current_line(), self.get_string_maxlen(context, *dim)?)
                } else {
                    let mut arrayitems = Vec::new();
                    let line = self.get_current_line();
                    for _ in 0..*dim {
                        arrayitems.push(self.parse_ifdata_item(context, arraytype)?);
                    }
                    GenericIfData::Array(line, arrayitems)
                }
            }
            A2mlTypeSpec::Enum(enumspec) => {
                let pos = self.get_current_line();
                let enumitem = self.get_identifier(context)?;
                if let Some(_) = enumspec.get(&enumitem) {
                    GenericIfData::EnumItem(pos, enumitem)
                } else {
                    return Err(ParseError::InvalidEnumValue(context.clone(), enumitem));
                }
            }
            A2mlTypeSpec::Struct(structspec) => {
                let mut structitems = Vec::new();
                let line = self.get_current_line();
                for itemspec in structspec {
                    structitems.push(self.parse_ifdata_item(context, itemspec)?);
                }
                GenericIfData::Struct(self.get_incfilename(context.fileid), line, structitems)
            }
            A2mlTypeSpec::Sequence(seqspec) => {
                let mut seqitems = Vec::new();
                let mut checkpoint = self.get_tokenpos();
                while let Ok(item) = self.parse_ifdata_item(context, seqspec) {
                    seqitems.push(item);
                    checkpoint = self.get_tokenpos();
                }
                self.set_tokenpos(checkpoint);
                GenericIfData::Sequence(seqitems)
            }
            A2mlTypeSpec::TaggedStruct(tsspec) => {
                GenericIfData::TaggedStruct(self.parse_ifdata_taggedstruct(context, tsspec)?)
            }
            A2mlTypeSpec::TaggedUnion(tuspec) => {
                let mut result = HashMap::new();
                if let Some(taggeditem) = self.parse_ifdata_taggeditem(context, tuspec)? {
                    result.insert(taggeditem.tag.clone(), vec![taggeditem]);
                }
                GenericIfData::TaggedUnion(result)
            }
        })
    }


    // parse_ifdata_taggedstruct()
    // parse all the tagged items of a TaggedStruct (TaggedUnions are not handled here because no loop is needed for those)
    fn parse_ifdata_taggedstruct(&mut self, context: &ParseContext, tsspec: &HashMap<String, A2mlTaggedTypeSpec>) -> Result<HashMap<String, Vec<GenericIfDataTaggedItem>>, ParseError> {
        let mut result = HashMap::new();
        while let Some(taggeditem) = self.parse_ifdata_taggeditem(context, tsspec)? {
            if result.get(&taggeditem.tag).is_none() {
                result.insert(taggeditem.tag.clone(), Vec::new());
            }
            result.get_mut(&taggeditem.tag.clone()).unwrap().push(taggeditem);
        }

        Ok(result)
    }


    // parse_ifdata_taggeditem()
    // try to parse a TaggedItem inside a TaggedStruct or TaggedUnion
    //
    // behold the ridiculous^Wglorious return type:
    //  - Parsing can fail completely because the file is structurally broken -> Outer layer is Result to handle this
    //  - It is possible that the function succeeds but can't return a value -> middle layer of Option handles this
    //  - finally, a GenericIfDataTaggedItem value is returned
    fn parse_ifdata_taggeditem(&mut self, context: &ParseContext, spec: &HashMap<String, A2mlTaggedTypeSpec>) -> Result<Option<GenericIfDataTaggedItem>, ParseError> {
        let checkpoint = self.get_tokenpos();
        // check if there is a tag
        if let Ok(Some((token, is_block))) = self.get_next_tag(context) {
            let tag = self.get_token_text(token);

            // check if the tag is valid inside this TaggedStruct/TaggedUnion. If it is not, parsing should abort so that the caller sees the tag
            if let Some(taggedspec) = spec.get(tag) {
                if taggedspec.is_block != is_block {
                    self.set_tokenpos(checkpoint);
                    return Ok(None);
                }

                // parse the content of the tagged item
                let newcontext = ParseContext::from_token(tag, token, is_block);
                let data = self.parse_ifdata_item(&newcontext, &taggedspec.item)?;
                let parsed_item = ParserState::parse_ifdata_make_block(data, self.get_incfilename(newcontext.fileid), newcontext.line);

                // make sure that blocks that started with /begin end with /end
                if is_block {
                    self.expect_token(&newcontext, A2lTokenType::End)?;
                    let endident = self.expect_token(&newcontext, A2lTokenType::Identifier)?;
                    let endtag = self.get_token_text(endident);
                    if endtag != tag {
                        return Err(ParseError::IncorrectEndTag(newcontext, endtag.to_string()));
                    }
                }

                Ok(Some(GenericIfDataTaggedItem {
                    incfile: self.get_incfilename(newcontext.fileid),
                    line: newcontext.line,
                    tag: tag.to_string(),
                    data: parsed_item,
                    is_block
                }))
            } else {
                self.set_tokenpos(checkpoint);
                Ok(None)
            }
        } else {
            self.set_tokenpos(checkpoint);
            Ok(None)
        }
    }


    // parse_ifdata_make_block()
    // turn the GenericIfData contained in a TaggedItem into a block.
    fn parse_ifdata_make_block(data: GenericIfData, incfile: Option<String>, line: u32) -> GenericIfData {
        match data {
            GenericIfData::Struct(_, _, structitems) => GenericIfData::Block(incfile, line, structitems),
            _ => GenericIfData::Block(incfile, line, vec![data])
        }
    }


    // parse_unknown_ifdata()
    // this function provides a fallback in case the data inside of an IF_DATA block cannot be
    // parsed using either the built-in specification or the A2ML spec in the file
    // If the data is strucutrally sane (matching /begin and /end tags), then it returns a result.
    // The returned data is not intended to be useful for further processing, but only to preserve
    // it so that is can be written out to a file again later.
    pub(crate) fn parse_unknown_ifdata(&mut self, context: &ParseContext, is_block: bool) -> Result<GenericIfData, ParseError> {
        let mut items: Vec<GenericIfData> = Vec::new();
        let line = self.get_current_line();

        loop {
            let token_peek = self.token_cursor.peek();
            if token_peek.is_none() {
                return Err(ParseError::UnexpectedEOF(context.clone()));
            }
            let token = token_peek.unwrap();

            match token.ttype {
                A2lTokenType::Identifier => {
                    // an arbitrary identifier; it could be a tag of a taggedstruct, but we don't know that here. The other option is an enum value.
                    items.push(GenericIfData::EnumItem(token.line, self.get_identifier(context)?));
                }
                A2lTokenType::String => {
                    items.push(GenericIfData::String(token.line, self.get_string(context)?));
                }
                A2lTokenType::Number => {
                    match self.get_integer_i32(context) {
                        Ok(num) => { items.push(GenericIfData::Long(token.line, num)); }
                        Err(_) => {
                            // try again, looks like the number is a float instead
                            self.token_cursor.back();
                            let floatnum = self.get_float(context)?; // if this also returns an error, it is neither int nor float, which is a genuine parse error
                            items.push(GenericIfData::Float(token.line, floatnum));
                        }
                    }
                }
                A2lTokenType::Begin => {
                    // if this is directly within a block level element, then a new taggedstruct will be created to contain the new block element
                    // if it is not, this block belongs to the parent and we only need to break and exit here
                    if is_block {
                        items.push(self.parse_unknown_taggedstruct(context)?);
                    } else {
                        break;
                    }
                }
                A2lTokenType::End => {
                    // the end of this unknown block. Contained unknown blocks are handled recursively, so we don't see their /end tags in this loop
                    break;
                }
                _ => { /* A2lTokenType::Include doesn't matter here */}
            }
        }

        Ok(GenericIfData::Struct(self.get_incfilename(context.fileid), line, items))
    }


    // parse_unknown_taggedstruct()
    // A taggedstruct is constructed to contain inner blocks, because the parse tree struct(block) is not permitted, while struct (taggedstruct(block)) is
    // The function will interpret as much as possible of the following data as elements of the taggedstruct
    fn parse_unknown_taggedstruct(&mut self, context: &ParseContext) -> Result<GenericIfData, ParseError> {
        let mut tsitems: HashMap<String, Vec<GenericIfDataTaggedItem>> = HashMap::new();

        while let Ok(Some((token, is_block))) = self.get_next_tag(context) {
            let tag = self.get_token_text(token);
            let newcontext = ParseContext::from_token(tag, &token, true);
            let result = self.parse_unknown_ifdata(&newcontext, is_block)?;

            if is_block {
                self.expect_token(&newcontext, A2lTokenType::End)?;
                let endident = self.expect_token(&newcontext, A2lTokenType::Identifier)?;
                let endtag = self.get_token_text(endident);
                if endtag != tag {
                    return Err(ParseError::IncorrectEndTag(newcontext, endtag.to_string()));
                }
            }

            let taggeditem = GenericIfDataTaggedItem {
                incfile: self.get_incfilename(newcontext.fileid),
                line: newcontext.line,
                tag: tag.to_string(),
                data: result,
                is_block
            };

            if tsitems.get(tag).is_none() {
                tsitems.insert(tag.to_string(), vec![]);
            }
            tsitems.get_mut(tag).unwrap().push(taggeditem);
        }

        Ok(GenericIfData::TaggedStruct(tsitems))
    }


}


impl ParseContext {
    pub fn from_token(text: &str, token: &A2lToken, is_block: bool) -> ParseContext {
        ParseContext { element: text.to_string(), fileid: token.fileid, line: token.line, inside_block: is_block }
    }
}


fn unescape_string(text: &str) -> String {
    let input_chars: Vec<char> = text.chars().collect();
    let mut output_chars = Vec::<char>::new();

    let mut idx = 1;
    while idx < input_chars.len() {
        if input_chars[idx-1] == '"' && input_chars[idx] == '"' {
            output_chars.push('"');
            idx += 1;
        } else if input_chars[idx-1] == '\\' && input_chars[idx] == '"' {
            output_chars.push('"');
            idx += 1;
        } else if input_chars[idx-1] == '\\' && input_chars[idx] == '\'' {
            output_chars.push('\'');
            idx += 1;
        } else if input_chars[idx-1] == '\\' && input_chars[idx] == '\\' {
            output_chars.push('\\');
            idx += 1;
        } else if input_chars[idx-1] == '\\' && input_chars[idx] == 'n' {
            output_chars.push('\n');
            idx += 1;
        } else if input_chars[idx-1] == '\\' && input_chars[idx] == 'r' {
            output_chars.push('\r');
            idx += 1;
        } else if input_chars[idx-1] == '\\' && input_chars[idx] == 't' {
            output_chars.push('\t');
            idx += 1;
        } else {
            output_chars.push(input_chars[idx-1]);
        }

        idx += 1;
    }
    if idx == input_chars.len() {
        output_chars.push(input_chars[idx-1]);
    }

    output_chars.iter().collect()
}
