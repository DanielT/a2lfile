use num_traits::{AsPrimitive, Num};
use std::fmt::Display;
use thiserror::Error;

use crate::a2ml::A2mlTypeSpec;
use crate::tokenizer::{A2lToken, A2lTokenType, TokenResult};
use crate::{A2lError, Filename, ParseableA2lObject};

const MAX_IDENT: usize = 1024;

struct TokenIter<'a> {
    tokens: &'a [A2lToken],
    pos: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum A2lVersion {
    V1_5_0,
    V1_5_1,
    V1_6_0,
    V1_6_1,
    V1_7_0,
    V1_7_1,
}

pub struct ParserState<'a> {
    token_cursor: TokenIter<'a>,
    filedata: &'a [String],
    pub(crate) filenames: &'a [Filename],
    pub(crate) last_token_position: u32,
    sequential_id: u32,
    pub(crate) log_msgs: &'a mut Vec<A2lError>,
    strict: bool,
    file_ver: A2lVersion,
    pub(crate) a2mlspec: Vec<A2mlTypeSpec>,
}

/// describes the current parser context, giving the name of the current element and its file and line number
#[derive(Debug, Clone)]
pub struct ParseContext {
    pub element: String,
    pub fileid: usize,
    pub line: u32,
}
#[derive(Debug, Error)]
#[non_exhaustive]
pub enum ParserError {
    #[error(
        "{filename}:{error_line}: expected token of type {expected_ttype:?}, got {actual_ttype:?} (\"{actual_text}\") inside block {element} starting on line {block_line}"
    )]
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

    #[error(
        "{filename}:{error_line}: expected an enum value, but \"{enumtxt}\" is not part of the enum (located inside block {block} starting on line {block_line})"
    )]
    InvalidEnumValue {
        filename: String,
        error_line: u32,
        enumtxt: String,
        block: String,
        block_line: u32,
    },

    #[error(
        "{filename}:{error_line}: optional element {tag} occurs too often within block {block} starting on line {block_line}"
    )]
    InvalidMultiplicityTooMany {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error(
        "{filename}:{error_line}: element {tag} is missing in block {block} starting on line {block_line}"
    )]
    InvalidMultiplicityNotPresent {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error(
        "{filename}:{error_line}: element {tag} in block {block} starting on line {block_line} must be enclosed in /begin and /end"
    )]
    IncorrectBlockError {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error(
        "{filename}:{error_line}: element {tag} in block {block} starting on line {block_line} may not be enclosed in /begin and /end"
    )]
    IncorrectKeywordError {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error(
        "{filename}:{error_line}: Wrong end tag {tag} found at the end of block {block} starting on line {block_line}"
    )]
    IncorrectEndTag {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error(
        "{filename}:{error_line}: Unknown sub-block {tag} found inside block {block} starting on line {block_line}"
    )]
    UnknownSubBlock {
        filename: String,
        error_line: u32,
        tag: String,
        block: String,
        block_line: u32,
    },

    #[error(
        "{filename}:{error_line}: encountered end of file while not done parsing block {block} starting on line {block_line}"
    )]
    UnexpectedEOF {
        filename: String,
        error_line: u32,
        block: String,
        block_line: u32,
    },

    #[error(
        "{filename}:{error_line}: String \"{text}\" in block {block} is {length} bytes long, but the maximum allowed length is {max_length}"
    )]
    StringTooLong {
        filename: String,
        error_line: u32,
        block: String,
        text: String,
        length: usize,
        max_length: usize,
    },

    #[error(
        "{filename}:{error_line}: Sub-block \"{tag}\" in block {block} is deprecated after version {limit_ver:.2}, but the file declares version {file_ver:.2}"
    )]
    BlockRefDeprecated {
        filename: String,
        error_line: u32,
        block: String,
        tag: String,
        limit_ver: A2lVersion,
        file_ver: A2lVersion,
    },

    #[error(
        "{filename}:{error_line}: Sub-block \"{tag}\" in block {block} is available from version {limit_ver:.2}, but the file declares version {file_ver:.2}"
    )]
    BlockRefTooNew {
        filename: String,
        error_line: u32,
        block: String,
        tag: String,
        limit_ver: A2lVersion,
        file_ver: A2lVersion,
    },

    #[error(
        "{filename}:{error_line}: Enum item \"{tag}\" in block {block} is deprecated after version {limit_ver:.2}, but the file declares version {file_ver:.2}"
    )]
    EnumRefDeprecated {
        filename: String,
        error_line: u32,
        block: String,
        tag: String,
        limit_ver: A2lVersion,
        file_ver: A2lVersion,
    },

    #[error(
        "{filename}:{error_line}: Enum item \"{tag}\" in block {block} is available from version {limit_ver:.2}, but the file declares version {file_ver:.2}"
    )]
    EnumRefTooNew {
        filename: String,
        error_line: u32,
        block: String,
        tag: String,
        limit_ver: A2lVersion,
        file_ver: A2lVersion,
    },

    #[error("{filename}:{error_line}: /begin in block {block} is not followed by a valid tag")]
    InvalidBegin {
        filename: String,
        error_line: u32,
        block: String,
    },

    #[error(
        "{filename}:{error_line}: The string '{ident}' in block {block} is not a valid identifier"
    )]
    InvalidIdentifier {
        filename: String,
        error_line: u32,
        ident: String,
        block: String,
    },

    #[error("{filename}:{error_line}: A2ML parser reports {errmsg}")]
    A2mlError {
        filename: String,
        error_line: u32,
        errmsg: String,
    },

    /// `AdditionalTokensError` parsing finished without consuming all data in the file
    #[error(
        "{filename}:{error_line}: unexpected additional data \"{text}...\" after parsed a2l file content"
    )]
    AdditionalTokensError {
        filename: String,
        error_line: u32,
        text: String,
    },

    /// `MissingVerionInfo`: no version information in the file
    #[error("File is not recognized as an a2l file. Mandatory version information is missing.")]
    MissingVersionInfo,

    /// Theversion number in the file dos not correspond to a known A2L specification
    #[error("File has invalid version {major} {minor}")]
    InvalidVersion { major: u16, minor: u16 },
}

pub(crate) enum BlockContent<'a> {
    Block(&'a A2lToken, bool, u32),
    Comment(&'a A2lToken, u32),
    None,
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
        filenames: &'b [Filename],
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
            file_ver: A2lVersion::V1_7_1,
            a2mlspec: Vec::new(),
        }
    }

    pub(crate) fn parse_file(&mut self) -> Result<crate::A2lFile, ParserError> {
        let firstline = self.token_cursor.tokens.first().map_or(1, |tok| tok.line);
        // create a context for the parser
        let context = ParseContext {
            element: "A2L_FILE".to_string(),
            fileid: 0,
            line: firstline,
        };

        // try to get the file version. Starting with 1.60, the ASAP2_VERSION element is mandatory. For
        // compatibility with old files, a missing version is only an error if strict parsing is requested
        self.file_ver = self.parse_version(&context)?;

        // parse the file
        let a2l_file = crate::A2lFile::parse(self, &context, 0)?;

        // make sure this is the end of the input, i.e. no additional data after the parsed data
        if let Some(token) = self.peek_token() {
            self.error_or_log(ParserError::AdditionalTokensError {
                filename: self.filenames[token.fileid].to_string(),
                error_line: self.last_token_position,
                text: self.get_token_text(token).to_owned(),
            })?;
        }

        Ok(a2l_file)
    }

    fn parse_version(&mut self, context: &ParseContext) -> Result<crate::A2lVersion, ParserError> {
        if let Some(token) = self.peek_token() {
            let ident = self.get_identifier(context);
            let ver_context = ParseContext::from_token("", token);
            if let Ok("ASAP2_VERSION") = ident.as_deref() {
                let version_result = crate::Asap2Version::parse(self, &ver_context, 0)
                    .map_err(|_| ParserError::MissingVersionInfo)
                    .and_then(|ver| A2lVersion::new(ver.version_no, ver.upgrade_no));

                // reset the input, the consumed tokens will be needed again by A2lFile::parse
                self.set_tokenpos(0);

                return match version_result {
                    Ok(ver) => Ok(ver),
                    Err(err) => {
                        // couldn't parse the version, but the file appears to have one
                        self.error_or_log(err)?;
                        Ok(A2lVersion::V1_7_1)
                    }
                };
            }
        }
        // for compatibility with 1.50 and earlier, also make it possible to catch the error and continue
        self.set_tokenpos(0);
        self.error_or_log(ParserError::MissingVersionInfo)?;
        Ok(A2lVersion::V1_5_1)
    }
    // get_token
    // get one token from the list of tokens and unwrap it
    pub(crate) fn get_token(
        &mut self,
        context: &ParseContext,
    ) -> Result<&'a A2lToken, ParserError> {
        if let Some(token) = self.token_cursor.next() {
            self.last_token_position = token.line;
            Ok(token)
        } else {
            Err(ParserError::unexpected_eof(self, context))
        }
    }

    pub(crate) fn undo_get_token(&mut self) {
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

    pub(crate) fn error_or_log(&mut self, err: ParserError) -> Result<(), ParserError> {
        if self.strict {
            Err(err)
        } else {
            self.log_warning(err);
            Ok(())
        }
    }

    pub(crate) fn get_tokenpos(&self) -> usize {
        self.token_cursor.pos
    }

    pub(crate) fn set_tokenpos(&mut self, newpos: usize) {
        self.token_cursor.pos = newpos;
    }

    pub(crate) fn get_token_text(&self, token: &'a A2lToken) -> &'a str {
        let data = &self.filedata[token.fileid];
        &data[token.startpos..token.endpos]
    }

    /// get the line offset of the token that was just consumed
    /// this means that seff.token_cursor.pos is already incremented
    /// - usually we're calculating token[pos-1] - token[pos-2]
    /// - for token[pos==1] it returns the line number of the first token - 1
    /// - the function shouldn't be called while pos == 0, but this case would behave like pos==1
    pub(crate) fn get_line_offset(&self) -> u32 {
        if self.token_cursor.pos > 1 && self.token_cursor.pos < self.token_cursor.tokens.len() {
            let prev_line = self.token_cursor.tokens[self.token_cursor.pos - 2].line;
            let prev_fileid = self.token_cursor.tokens[self.token_cursor.pos - 2].fileid;
            let cur_line = self.token_cursor.tokens[self.token_cursor.pos - 1].line;
            let cur_fileid = self.token_cursor.tokens[self.token_cursor.pos - 1].fileid;

            // subtracting line numbers is only sane within a file
            if prev_fileid == cur_fileid {
                cur_line - prev_line
            } else {
                // if the tokens come from different files then there is no line offset and the
                // value 2 is used for formatting: 2 newlines leave one line empty
                2
            }
        } else {
            self.token_cursor.tokens[0].line - 1
        }
    }

    pub(crate) fn get_next_id(&mut self) -> u32 {
        self.sequential_id += 1;
        self.sequential_id
    }

    pub(crate) fn get_incfilename(&self, fileid: usize) -> Option<String> {
        if fileid == 0 || fileid >= self.filenames.len() {
            None
        } else {
            Some(self.filenames[fileid].to_string())
        }
    }

    pub(crate) fn set_file_version(&mut self, version: A2lVersion) {
        self.file_ver = version;
    }

    pub(crate) fn check_block_version_lower(
        &mut self,
        context: &ParseContext,
        tag: &str,
        min_ver: A2lVersion,
    ) -> Result<(), ParserError> {
        if self.file_ver < min_ver {
            self.error_or_log(ParserError::BlockRefTooNew {
                filename: self.filenames[context.fileid].to_string(),
                error_line: self.last_token_position,
                block: context.element.clone(),
                tag: tag.to_string(),
                limit_ver: min_ver,
                file_ver: self.file_ver,
            })?;
        }
        Ok(())
    }

    pub(crate) fn check_block_version_upper(
        &mut self,
        context: &ParseContext,
        tag: &str,
        max_ver: A2lVersion,
    ) {
        if self.file_ver > max_ver {
            self.log_warning(ParserError::BlockRefDeprecated {
                filename: self.filenames[context.fileid].to_string(),
                error_line: self.last_token_position,
                block: context.element.clone(),
                tag: tag.to_string(),
                limit_ver: max_ver,
                file_ver: self.file_ver,
            });
        }
    }

    pub(crate) fn check_enumitem_version_lower(
        &mut self,
        context: &ParseContext,
        tag: &str,
        min_ver: A2lVersion,
    ) -> Result<(), ParserError> {
        if self.file_ver < min_ver {
            self.error_or_log(ParserError::EnumRefTooNew {
                filename: self.filenames[context.fileid].to_string(),
                error_line: self.last_token_position,
                block: context.element.clone(),
                tag: tag.to_string(),
                limit_ver: min_ver,
                file_ver: self.file_ver,
            })?;
        }
        Ok(())
    }

    pub(crate) fn check_enumitem_version_upper(
        &mut self,
        context: &ParseContext,
        tag: &str,
        max_ver: A2lVersion,
    ) {
        if self.file_ver > max_ver {
            self.log_warning(ParserError::EnumRefDeprecated {
                filename: self.filenames[context.fileid].to_string(),
                error_line: self.last_token_position,
                block: context.element.clone(),
                tag: tag.to_string(),
                limit_ver: max_ver,
                file_ver: self.file_ver,
            });
        }
    }

    // expect_token get a token which has to be of a particular type (hence: expect)
    // getting a token of any other type is a ParserError
    pub(crate) fn expect_token(
        &mut self,
        context: &ParseContext,
        token_type: A2lTokenType,
    ) -> Result<&'a A2lToken, ParserError> {
        let mut token = self.get_token(context)?;
        // we never "expect" a comment token, so we skip it
        while token.ttype == A2lTokenType::Comment {
            token = self.get_token(context)?;
        }

        if token.ttype != token_type {
            return Err(ParserError::unexpected_token_type(
                self, context, token, token_type,
            ));
        }

        Ok(token)
    }

    // get_string()
    // Get the content of a String token as a string
    pub(crate) fn get_string(&mut self, context: &ParseContext) -> Result<String, ParserError> {
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
    pub(crate) fn get_string_maxlen(
        &mut self,
        context: &ParseContext,
        maxlen: usize,
    ) -> Result<String, ParserError> {
        let text = self.get_string(context)?;
        if text.len() > maxlen {
            self.error_or_log(ParserError::StringTooLong {
                filename: self.filenames[context.fileid].to_string(),
                error_line: self.last_token_position,
                block: context.element.clone(),
                text: text.clone(),
                length: text.len(),
                max_length: maxlen,
            })?;
        }
        Ok(text)
    }

    // get_identifier()
    // Get the content of an Identifier token as a string
    pub(crate) fn get_identifier(&mut self, context: &ParseContext) -> Result<String, ParserError> {
        let token = self.expect_token(context, A2lTokenType::Identifier)?;
        let text = self.get_token_text(token);
        if text.as_bytes()[0].is_ascii_digit() || text.len() > MAX_IDENT {
            self.error_or_log(ParserError::InvalidIdentifier {
                filename: self.filenames[context.fileid].to_string(),
                error_line: self.last_token_position,
                block: context.element.clone(),
                ident: text.to_owned(),
            })?;
        }
        Ok(String::from(text))
    }

    // get_float()
    // Get the content of a Number token as a float
    // Since the Number token stores text internally, the text must be converted first
    pub(crate) fn get_float(&mut self, context: &ParseContext) -> Result<f32, ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        // some vendor tools are defining the characteristic UpperLimit and LowerLimit
        // (float values from specifications) using 0xNNN for characteristics that
        // are actually integers.
        if text.starts_with("0x") || text.starts_with("0X") {
            match u64::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num as f32),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse::<f32>() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    // get_double()
    // Get the content of a Number token as a double(f64)
    // Since the Number token stores text internally, the text must be converted first
    pub(crate) fn get_double(&mut self, context: &ParseContext) -> Result<f64, ParserError> {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        // some vendor tools are defining the characteristic UpperLimit and LowerLimit
        // (float values from specifications) using 0xNNN for characteristics that
        // are actually integers.
        if text.starts_with("0x") || text.starts_with("0X") {
            match u64::from_str_radix(&text[2..], 16) {
                Ok(num) => Ok(num as f64),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        } else {
            match text.parse::<f64>() {
                Ok(num) => Ok(num),
                Err(_) => Err(ParserError::malformed_number(self, context, text)),
            }
        }
    }

    pub(crate) fn get_integer<T>(
        &mut self,
        context: &ParseContext,
    ) -> Result<(T, bool), ParserError>
    where
        T: Num + std::str::FromStr + Copy + 'static,
        u64: AsPrimitive<T>,
    {
        let token = self.expect_token(context, A2lTokenType::Number)?;
        let text = self.get_token_text(token);
        if text.len() > 2 && (text.starts_with("0x") || text.starts_with("0X")) {
            match u64::from_str_radix(&text[2..], 16) {
                Ok(num_u64) => Ok((num_u64.as_(), true)),
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
    // alternatively, get the next comment
    //
    // Handling tags and comments in the same function makes sense, because the current parser
    // implementation can only handle comments where blocks are permitted. If the parser is ever
    // extended to handle comments in other locations, then this function will need to be split.
    pub(crate) fn get_next_tag_or_comment(
        &mut self,
        context: &ParseContext,
    ) -> Result<BlockContent<'a>, ParserError> {
        let mut is_block = false;
        let tokenpos = self.get_tokenpos();

        if let Some(
            tokenval @ A2lToken {
                ttype: A2lTokenType::Comment,
                ..
            },
        ) = self.token_cursor.peek()
        {
            self.token_cursor.next(); // consume the peeked token
            let start_offset = self.get_line_offset();
            Ok(BlockContent::Comment(tokenval, start_offset))
        } else {
            // if the next token is /begin, then set is_block and skip the token
            let (token_result, start_offset) = if let Some(A2lToken {
                ttype: A2lTokenType::Begin,
                ..
            }) = self.token_cursor.peek()
            {
                is_block = true;
                self.get_token(context)?;
                // for blocks, we went to set start_offset to the offset of the /begin token,
                // and then get the tag from the next token
                let start_offset = self.get_line_offset();
                (
                    self.expect_token(context, A2lTokenType::Identifier),
                    start_offset,
                )
            } else {
                // if this is not a block, then just get the tag and the offset of the tag
                (
                    self.expect_token(context, A2lTokenType::Identifier),
                    self.get_line_offset(),
                )
            };

            // get the tag or return None if the token is not an Identifier
            match token_result {
                Ok(token) => Ok(BlockContent::Block(token, is_block, start_offset)),
                Err(error) => {
                    self.set_tokenpos(tokenpos);
                    if is_block {
                        Err(error)
                    } else {
                        // no tag? no problem!
                        Ok(BlockContent::None)
                    }
                }
            }
        }
    }

    // handle_unknown_taggedstruct_tag
    // perform error recovery if an unknown tag is found inside a taggedstruct and strict parsing is off
    pub(crate) fn handle_unknown_taggedstruct_tag(
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
        let errcontext = ParseContext::from_token(text, &self.token_cursor.tokens[startpos]);

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
                        // the current unknown item started with /begin ITEM_TAG, so it must end with /end ITEM_TAG.
                        // the stoplist is not relevant
                        if balance == 0 {
                            if text == item_tag {
                                break;
                            } else {
                                return Err(ParserError::incorrect_end_tag(
                                    self,
                                    &errcontext,
                                    text,
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
                    // once balance == 0 is reached for a block, the next tag should be an Identifier
                    if item_is_block && balance == 0 {
                        return Err(ParserError::incorrect_end_tag(self, &errcontext, text));
                    }
                    // else: ignore the token
                }
            }
        }

        Ok(())
    }

    pub(crate) fn handle_multiplicity_error(
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

    pub(crate) fn require_block(
        &self,
        tag: &str,
        is_block: bool,
        context: &ParseContext,
    ) -> Result<(), ParserError> {
        if !is_block {
            Err(ParserError::IncorrectBlockError {
                filename: self.filenames[context.fileid].to_string(),
                error_line: self.last_token_position,
                tag: tag.to_string(),
                block: context.element.clone(),
                block_line: context.line,
            })
        } else {
            Ok(())
        }
    }

    pub(crate) fn require_keyword(
        &self,
        tag: &str,
        is_block: bool,
        context: &ParseContext,
    ) -> Result<(), ParserError> {
        if is_block {
            Err(ParserError::IncorrectKeywordError {
                filename: self.filenames[context.fileid].to_string(),
                error_line: self.last_token_position,
                tag: tag.to_string(),
                block: context.element.clone(),
                block_line: context.line,
            })
        } else {
            Ok(())
        }
    }
}

impl ParseContext {
    pub(crate) fn from_token(text: &str, token: &A2lToken) -> ParseContext {
        ParseContext {
            element: text.to_string(),
            fileid: token.fileid,
            line: token.line,
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
            filename: parser.filenames[context.fileid].to_string(),
            error_line: parser.last_token_position,
            block_line: context.line,
            element: context.element.clone(),
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
            filename: parser.filenames[context.fileid].to_string(),
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
            filename: parser.filenames[context.fileid].to_string(),
            error_line: parser.last_token_position,
            enumtxt: enumitem.to_owned(),
            block: context.element.clone(),
            block_line: context.line,
        }
    }

    pub(crate) fn invalid_multiplicity_too_many(
        parser: &ParserState,
        context: &ParseContext,
        tag: &str,
    ) -> Self {
        Self::InvalidMultiplicityTooMany {
            filename: parser.filenames[context.fileid].to_string(),
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
            filename: parser.filenames[context.fileid].to_string(),
            error_line: parser.last_token_position,
            tag: tag.to_owned(),
            block: context.element.clone(),
            block_line: context.line,
        }
    }

    pub(crate) fn unknown_sub_block(
        parser: &ParserState,
        context: &ParseContext,
        tag: &str,
    ) -> Self {
        Self::UnknownSubBlock {
            filename: parser.filenames[context.fileid].to_string(),
            error_line: parser.last_token_position,
            tag: tag.to_owned(),
            block: context.element.clone(),
            block_line: context.line,
        }
    }

    pub(crate) fn unexpected_eof(parser: &ParserState, context: &ParseContext) -> Self {
        Self::UnexpectedEOF {
            filename: parser.filenames[context.fileid].to_string(),
            error_line: parser.last_token_position,
            block: context.element.clone(),
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

impl A2lVersion {
    pub fn new(major: u16, minor: u16) -> Result<Self, ParserError> {
        match (major, minor) {
            (1, 50) => Ok(A2lVersion::V1_5_0),
            (1, 51) => Ok(A2lVersion::V1_5_1),
            (1, 60) => Ok(A2lVersion::V1_6_0),
            (1, 61) => Ok(A2lVersion::V1_6_1),
            (1, 70) => Ok(A2lVersion::V1_7_0),
            (1, 71) => Ok(A2lVersion::V1_7_1),
            _ => Err(ParserError::InvalidVersion { major, minor }),
        }
    }
}

impl Display for A2lVersion {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            A2lVersion::V1_5_0 => f.write_str("1.5.0"),
            A2lVersion::V1_5_1 => f.write_str("1.5.1"),
            A2lVersion::V1_6_0 => f.write_str("1.6.0"),
            A2lVersion::V1_6_1 => f.write_str("1.6.1"),
            A2lVersion::V1_7_0 => f.write_str("1.7.0"),
            A2lVersion::V1_7_1 => f.write_str("1.7.1"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Filename, load_from_string, tokenizer};

    #[test]
    fn parsing_numbers_test() {
        let input_text = r##"0 0x1 1.0e+2 1000 0 0.1 0x11 1.0e+2 0X1f 0X2F 2F F"##;
        let tokenresult = tokenizer::tokenize(&Filename::from("test_input"), 0, input_text);
        assert!(tokenresult.is_ok());

        let tokenresult = tokenresult.unwrap();
        let mut log_msgs = Vec::<A2lError>::new();
        let mut parser = ParserState::new(&tokenresult, &mut log_msgs, true);
        let context = ParseContext {
            element: "TEST".to_string(),
            fileid: 0,
            line: 0,
        };

        // uint8: 0
        let res = parser.get_integer::<u8>(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, (0, false));

        // uint8: 0x1
        let res = parser.get_integer::<u8>(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, (1, true));

        // uint8: 1.0e+2
        let res = parser.get_integer::<u8>(&context);
        assert!(res.is_err());

        // uint8: 257
        let res = parser.get_integer::<u8>(&context);
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
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, 17f32);

        // float: 1.0e+2
        let res = parser.get_float(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, 100f32);

        // float: 0X1f
        let res = parser.get_float(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, 31f32);

        // float: 0X2F
        let res = parser.get_float(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, 47f32);

        // float: 2F
        let res = parser.get_float(&context);
        assert!(res.is_err());

        // float: F
        let res = parser.get_float(&context);
        assert!(res.is_err());
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
        // \' -> '
        let result = unescape_string(r#"\'"#);
        assert_eq!(result, r#"'"#);
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
        let result = unescape_string(r#"\txx"#);
        assert_eq!(result, "\txx");
    }

    #[test]
    fn parsing_identifiers_test() {
        let input_text = r##"ident 0ident 123"##;
        let tokenresult = tokenizer::tokenize(&Filename::from("test_input"), 0, input_text);
        assert!(tokenresult.is_ok());

        let tokenresult = tokenresult.unwrap();
        let mut log_msgs = Vec::<A2lError>::new();
        let mut parser = ParserState::new(&tokenresult, &mut log_msgs, true);
        let context = ParseContext {
            element: "TEST".to_string(),
            fileid: 0,
            line: 0,
        };

        // identifier: ident
        let res = parser.get_identifier(&context);
        assert!(res.is_ok());
        let val = res.unwrap();
        assert_eq!(val, "ident");

        // bad identifier - identifiers may not start with a number
        let res = parser.get_identifier(&context);
        assert!(matches!(res, Err(ParserError::InvalidIdentifier { .. })));

        // not an identifier
        let res = parser.get_identifier(&context);
        assert!(res.is_err());
    }

    #[test]
    fn test_check_version() {
        let tokenresult = tokenizer::tokenize(&Filename::from("test_input"), 0, "").unwrap();
        let mut log_msgs = Vec::<A2lError>::new();
        let mut parser = ParserState::new(&tokenresult, &mut log_msgs, true);
        let context = ParseContext {
            element: "TEST".to_string(),
            fileid: 0,
            line: 0,
        };
        parser.file_ver = A2lVersion::V1_6_0;

        // block version v1_5_0 <= file version V1_6_0
        let result = parser.check_block_version_lower(&context, "CHECK", A2lVersion::V1_5_0);
        assert!(result.is_ok());

        // !block version v1_7_0 <= file version V1_6_0
        let result = parser.check_block_version_lower(&context, "CHECK", A2lVersion::V1_7_0);
        assert!(result.is_err());

        // !block version v1_5_0 >= file version V1_6_0
        let num_warnings = parser.log_msgs.len();
        parser.check_block_version_upper(&context, "CHECK", A2lVersion::V1_5_0);
        assert_ne!(num_warnings, parser.log_msgs.len());

        // block version v1_7_0 >= file version V1_6_0
        let num_warnings = parser.log_msgs.len();
        parser.check_block_version_upper(&context, "CHECK", A2lVersion::V1_7_0);
        assert_eq!(num_warnings, parser.log_msgs.len());

        // enum item version V1_5_0 <= file version V1_6_0
        let result: Result<(), ParserError> =
            parser.check_enumitem_version_lower(&context, "CHECK", A2lVersion::V1_5_0);
        assert!(result.is_ok());

        // !enum item version V1_7_0 <= file version V1_6_0
        let result = parser.check_enumitem_version_lower(&context, "CHECK", A2lVersion::V1_7_0);
        assert!(result.is_err());

        // !enum item version V1_5_0 >= file version V1_6_0
        let num_warnings = parser.log_msgs.len();
        parser.check_enumitem_version_upper(&context, "CHECK", A2lVersion::V1_5_0);
        assert_ne!(num_warnings, parser.log_msgs.len());

        // enum item version V1_7_0 >= file version V1_6_0
        let num_warnings = parser.log_msgs.len();
        parser.check_enumitem_version_upper(&context, "CHECK", A2lVersion::V1_7_0);
        assert_eq!(num_warnings, parser.log_msgs.len());
    }

    #[test]
    fn a2lversion() {
        let v150 = A2lVersion::new(1, 50).unwrap();
        let v151 = A2lVersion::new(1, 51).unwrap();
        let v160 = A2lVersion::new(1, 60).unwrap();
        let v161 = A2lVersion::new(1, 61).unwrap();
        let v170 = A2lVersion::new(1, 70).unwrap();
        let v171 = A2lVersion::new(1, 71).unwrap();

        assert!(v150 < v151);
        assert!(v151 < v160);
        assert!(v160 < v161);
        assert!(v161 < v170);
        assert!(v170 < v171);

        let bad_version = A2lVersion::new(1, 80);
        assert!(bad_version.is_err());

        let cpy = v171;
        assert_eq!(cpy, v171);

        assert_eq!(format!("{v150}"), "1.5.0");
        assert_eq!(format!("{v151}"), "1.5.1");
        assert_eq!(format!("{v160}"), "1.6.0");
        assert_eq!(format!("{v161}"), "1.6.1");
        assert_eq!(format!("{v170}"), "1.7.0");
        assert_eq!(format!("{v171}"), "1.7.1");
    }

    #[test]
    fn error_missing_version() {
        static DATA: &str = r#"/begin PROJECT p "" /end PROJECT"#;
        let a2l_file = load_from_string(DATA, None, true);
        assert!(a2l_file.is_err());
        assert!(matches!(
            a2l_file,
            Err(A2lError::ParserError {
                parser_error: ParserError::MissingVersionInfo
            })
        ));
    }

    #[test]
    fn error_invalid_mult_not_present() {
        static DATA: &str = r#"ASAP2_VERSION 1 71"#;
        let a2l_file = load_from_string(DATA, None, true);
        assert!(matches!(
            a2l_file,
            Err(A2lError::ParserError {
                parser_error: ParserError::InvalidMultiplicityNotPresent { .. }
            })
        ));
    }

    #[test]
    fn error_invalid_mult_too_many() {
        static DATA: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT /begin PROJECT p2 "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        let a2l_file = load_from_string(DATA, None, true);
        assert!(matches!(
            a2l_file,
            Err(A2lError::ParserError {
                parser_error: ParserError::InvalidMultiplicityTooMany { .. }
            })
        ));
    }

    #[test]
    fn error_unknown_subblock() {
        static DATA: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m "" ABCDEF /end MODULE /end PROJECT"#;
        let a2l_file = load_from_string(DATA, None, true);
        assert!(matches!(
            a2l_file,
            Err(A2lError::ParserError {
                parser_error: ParserError::UnknownSubBlock { .. }
            })
        ));
    }

    #[test]
    fn error_incorrect_end_tag() {
        static DATA: &str =
            r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m "" /end MMMMM /end PROJECT"#;
        let a2l_file = load_from_string(DATA, None, true);
        assert!(matches!(
            a2l_file,
            Err(A2lError::ParserError {
                parser_error: ParserError::IncorrectEndTag { .. }
            })
        ));
    }

    #[test]
    fn error_unexpected_eof() {
        static DATA: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT"#;
        let a2l_file = load_from_string(DATA, None, true);
        assert!(matches!(
            a2l_file,
            Err(A2lError::ParserError {
                parser_error: ParserError::UnexpectedEOF { .. }
            })
        ));
    }

    #[test]
    fn error_invalid_identifier() {
        let data = format!(
            r#"ASAP2_VERSION 1 71 /begin PROJECT {} "" /begin MODULE m "" /end MODULE /end PROJECT"#,
            ['a'; 1025].iter().collect::<String>()
        );
        let a2l_file = load_from_string(&data, None, true);
        println!("a2l_file: {:#?}", a2l_file);
        assert!(a2l_file.is_err());
        assert!(matches!(
            a2l_file,
            Err(A2lError::ParserError {
                parser_error: ParserError::InvalidIdentifier { .. }
            })
        ));
    }

    #[test]
    fn test_handle_unknown_taggedstruct_tag() {
        // balanced unknown block
        static DATA: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin UNKNOWN_TAG abc def /begin ghi /end ghi /end UNKNOWN_TAG
        /end MODULE /end PROJECT"#;
        let result = load_from_string(DATA, None, false);
        assert!(result.is_ok());
        let (a2l_file, _) = result.unwrap();
        assert_eq!(a2l_file.project.module.len(), 1);

        // unbalanced unknown block
        static DATA2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin UNKNOWN_TAG abc def
        /end MODULE /end PROJECT"#;
        let result = load_from_string(DATA2, None, false);
        assert!(matches!(
            result,
            Err(A2lError::ParserError {
                parser_error: ParserError::IncorrectEndTag { .. }
            })
        ));

        //unknown keyword
        static DATA3: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            UNKNOWN_TAG abc def /begin ghi /end ghi
            /begin GROUP group_name "" ROOT
            /end GROUP
        /end MODULE /end PROJECT"#;
        let result = load_from_string(DATA3, None, false);
        assert!(result.is_ok());
        let (a2l_file, _) = result.unwrap();
        assert_eq!(a2l_file.project.module.len(), 1);
        assert_eq!(a2l_file.project.module[0].group.len(), 1);
    }

    #[test]
    fn parse_with_comments() {
        static DATA: &str = r#"
        /begin TRANSFORMER Transformer
            "1.0.0"                          // Version info
            "TransformerDll32.dll"           // Name of the 32bit DLL
            ""                               // Name of the 64bit DLL
            1500                             // timeout in [ms]
            ON_CHANGE
            InverseTransformer
        /end TRANSFORMER"#;
        let module = crate::load_fragment(DATA, None).unwrap();
        let bi = &module.transformer[0].__block_info;
        assert_eq!(bi.item_location.1, 1); // offset of the version
        assert_eq!(bi.item_location.2, 1); // offset of the 32-bit dll name
        assert_eq!(bi.item_location.3, 1); // offset of the 64-bit dll name
        assert_eq!(bi.item_location.4, (1, false)); // offset of the timeout
        assert_eq!(bi.item_location.5, 1); // offset of the trigger [ON_CHANGE]
        assert_eq!(bi.item_location.6, 1); // offset of the inverse transformer
        assert_eq!(bi.end_offset, 1); // offset of the /end TRANSFORMER
    }
}
