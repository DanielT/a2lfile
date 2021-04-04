use super::a2ltokenizer::*;

struct TokenIter<'a> {
    tokens: &'a Vec<A2lToken>,
    pos: usize
}

pub struct ParserState<'a> {
    token_cursor: TokenIter<'a>,
    currentline: u32,
    logger: &'a mut dyn super::Logger,
    strict: bool
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
    StringTooLong(ParseContext, String, usize, usize)
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
    pub fn new<'b>(tokens: &'b Vec<A2lToken>, logger: &'b mut dyn super::Logger, strict: bool) -> ParserState<'b> {
        ParserState {
            token_cursor: TokenIter{ tokens, pos: 0},
            currentline: 0,
            logger,
            strict
        }
    }


    // get_token
    // get one token from the list of tokens and unwrap it
    pub fn get_token(&mut self, context: &ParseContext) -> Result<&'a A2lToken, ParseError> {
        if let Some(token) = self.token_cursor.next() {
            self.currentline = token.line;
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


    // expect_token get a token which has to be of a particular type (hence: expect)
    // getting a token of any other type is a ParseError
    pub fn expect_token(&mut self, context: &ParseContext, token_type: A2lTokenType) -> Result<&'a A2lToken, ParseError> {
        let token = self.get_token(context)?;

        if token.ttype != token_type {
            return Err(ParseError::UnexpectedTokenType(context.clone(), token.ttype.clone(), token.text.clone(), token_type));
        }

        Ok(token)
    }


    // get_string()
    // Get the content of a String token as a string
    pub fn get_string(&mut self, context: &ParseContext) -> Result<String, ParseError> {
        let text = if let Some(A2lToken {ttype: A2lTokenType::Identifier, text, ..}) = self.peek_token() {
            // an identifier can be used in place of a string, if the parser is not strict
            self.error_or_log(ParseError::UnexpectedTokenType(context.clone(), A2lTokenType::Identifier, text.clone(), A2lTokenType::String))?;
            self.get_identifier(context)?
        } else {
            let token = self.expect_token(context, A2lTokenType::String)?;
            token.text.clone()
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
        Ok(String::from(&token.text))
    }


    // get_float()
    // Get the content of a Number token as a float
    // Since the Number token stores text internally, the text must be converted first
    pub fn get_float(&mut self, context: &ParseContext) -> Result<f32, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        match token.text.parse::<f32>() {
            Ok(num) => Ok(num),
            Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
        }
    }

    // get_double()
    // Get the content of a Number token as a double(f64)
    // Since the Number token stores text internally, the text must be converted first
    pub fn get_double(&mut self, context: &ParseContext) -> Result<f64, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        match token.text.parse::<f64>() {
            Ok(num) => Ok(num),
            Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
        }
    }


    // All the following get_integer_<foo> functions were supposed to be one generic function.
    // I was unable to make that work; the stumbling block is the possiblity for signed data
    // that is represented as hex to have the high bit set.
    // E.g. 0xffff is considered to be a valid signed int (i16); it is -1 in decimal notation.
    // I did not manage to decode this in a generic manner.


    pub fn get_integer_i8(&mut self, context: &ParseContext) -> Result<i8, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        if token.text.len() > 2 && (token.text.starts_with("0x") || token.text.starts_with("0X")) {
            match u8::from_str_radix(&token.text[2..], 16) {
                Ok(num) => Ok(num as i8),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }        
        } else {
            match token.text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }
        }
    }


    pub fn get_integer_u8(&mut self, context: &ParseContext) -> Result<u8, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        if token.text.len() > 2 && (token.text.starts_with("0x") || token.text.starts_with("0X")) {
            match u8::from_str_radix(&token.text[2..], 16) {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }        
        } else {
            match token.text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }
        }
    }


    pub fn get_integer_i16(&mut self, context: &ParseContext) -> Result<i16, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        if token.text.len() > 2 && (token.text.starts_with("0x") || token.text.starts_with("0X")) {
            match u16::from_str_radix(&token.text[2..], 16) {
                Ok(num) => Ok(num as i16),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }        
        } else {
            match token.text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }
        }
    }


    pub fn get_integer_u16(&mut self, context: &ParseContext) -> Result<u16, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        if token.text.len() > 2 && (token.text.starts_with("0x") || token.text.starts_with("0X")) {
            match u16::from_str_radix(&token.text[2..], 16) {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }        
        } else {
            match token.text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }
        }
    }


    pub fn get_integer_i32(&mut self, context: &ParseContext) -> Result<i32, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        if token.text.len() > 2 && (token.text.starts_with("0x") || token.text.starts_with("0X")) {
            match u32::from_str_radix(&token.text[2..], 16) {
                Ok(num) => Ok(num as i32),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }        
        } else {
            match token.text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }
        }
    }


    pub fn get_integer_u32(&mut self, context: &ParseContext) -> Result<u32, ParseError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        if token.text.len() > 2 && (token.text.starts_with("0x") || token.text.starts_with("0X")) {
            match u32::from_str_radix(&token.text[2..], 16) {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }        
        } else {
            match token.text.parse() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParseError::MalformedNumber(context.clone(), token.text.clone()))
            }
        }
    }


    // peek_next_tag()
    // peek ahead in the token stream, in order to get the tag of the next item of a taggedstruct or taggedunion
    pub fn peek_next_tag(&mut self, context: &ParseContext) -> Result<Option<(String, bool)>, ParseError> {
        let mut is_block = false;
        let mut peek = self.token_cursor.peek();

        // if the next token is /begin, then set is_block and skip the token
        if let Some(tok_peek) = peek {
            if tok_peek.ttype == A2lTokenType::Begin {
                is_block = true;
                self.get_token(context)?;
                peek = self.token_cursor.peek();
            }
        }

        // get the tag or return None if the token is not an Identifier
        if peek.is_none() {
            if is_block {
                self.token_cursor.back();
            }
            return Err(ParseError::UnexpectedEOF(context.clone()));
        }
        let token = peek.unwrap();
        if token.ttype != A2lTokenType::Identifier {
            return Ok(None);
        }

        Ok(Some((token.text.clone(), is_block)))
    }


    // handle_unknown_taggedstruct_tag
    // perform error recovery if an unknown tag is found inside a taggedstruct and strict parsing is off
    pub fn handle_unknown_taggedstruct_tag(&mut self, context: &ParseContext, item_tag: &str, item_is_block: bool, stoplist: &[&str]) -> Result<(), ParseError> {
        self.error_or_log(ParseError::UnknownSubBlock(context.clone(), item_tag.to_string()))?;
        let startpos = self.get_tokenpos();
        let errcontext = ParseContext::from_token(&self.token_cursor.tokens[startpos], item_is_block);
    
        let mut balance = 0;
        if item_is_block {
            balance = 1;
        }

        loop {
            let token = self.get_token(context)?;
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
                            if token.text == item_tag {
                                break;
                            } else {
                                return Err(ParseError::IncorrectEndTag(errcontext, token.text.clone()));
                            }
                        }
                    } else {
                        // this unknown item did not begin with /begin, so the end is reached when either:
                        // - the end tag of the parent block is found (balance == -1)
                        // - a tag belonging to the parent block (on the stoplist) is found (balance == 0)
                        // - the sequence /begin TAG for a tag on the stoplist is encountered (balance == 1)
                        if balance == 0 || balance == 1 {
                            let found = stoplist.iter().find(|entry| *entry == &token.text);
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
                        return Err(ParseError::IncorrectEndTag(errcontext, token.text.clone()));
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
                format!("{} on line {}: expected token of type {:?}, got {:?} (\"{}\") inside block {} starting on line {}", prefix, self.currentline, expected_ttype, actual_ttype, actual_text, context.element, context.line)
            }
            ParseError::MalformedNumber(_context, numstr) => {
                format!("{} on line {}: string \"{}\" could not be interpreted as a number", prefix, self.currentline, numstr)
            }
            ParseError::InvalidEnumValue(context, enval) => {
                format!("{} on line {}: expected an enum value, but \"{}\" is not part of the enum (located inside block {} starting on line {})", prefix, self.currentline, enval, context.element, context.line)
            }
            ParseError::InvalidMultiplicityTooMany(context, tag) => {
                format!("{} on line {}: optional element {} occurs too often within block {} starting on line {}", prefix, self.currentline, tag, context.element, context.line)
            }
            ParseError::InvalidMultiplicityNotPresent(context, tag) => {
                format!("{} on line {}: element {} is missing in block {} starting on line {}", prefix, self.currentline, tag, context.element, context.line)
            }
            ParseError::IncorrectElemType(context, tag, is_block) => {
                match is_block {
                    true => format!("{} on line {}: element {} in block {} starting on line {} must be enclosed in /begin and /end", prefix, self.currentline, tag, context.element, context.line),
                    false => format!("{} on line {}: element {} in block {} starting on line {} must not be enclosed in /begin and /end", prefix, self.currentline, tag, context.element, context.line)
                }
            }
            ParseError::UnexpectedEOF(context) => {
                format!("{} on line {}: encountered end of input while not done parsing block {} starting on line {}", prefix, self.currentline, context.element, context.line)
            }
            ParseError::UnknownSubBlock(context, tag) => {
                format!("{} on line {}: Unknown sub-block {} found inside block {} starting on line {}", prefix, self.currentline, tag, context.element, context.line)
            }
            ParseError::IncorrectEndTag(context, tag) => {
                format!("{} on line {}: Wrong end tag {} found at the end of block {} starting on line {}", prefix, self.currentline, tag, context.element, context.line)
            }
            ParseError::StringTooLong(context, text, maxlen, actual_len) => {
                format!("{} on line {}: String \"{}\" in block {} is {} bytes long, but the maximum allowed length is {}", prefix, self.currentline, text, context.element, actual_len, maxlen)
            }
        }
    }
}


impl ParseContext {
    pub fn from_token(token: &A2lToken, is_block: bool) -> ParseContext {
        ParseContext { element: token.text.clone(), fileid: token.fileid, line: token.line, inside_block: is_block }
    }
}






