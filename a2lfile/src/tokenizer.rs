use std::path::Path;
use thiserror::Error;

use super::loader;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum TokenizerError {
    #[error("{filename}:{line}: Failed to load included file {incname}")]
    IncludeFileError {
        filename: String,
        line: u32,
        incname: String,
    },

    #[error("{filename}:{line}: Include directive was not follwed by a filename")]
    IncompleteIncludeError { filename: String, line: u32 },

    #[error("{filename}:{line}: Input text \"{tokentext}...\" was not recognized as an a2l token")]
    InvalidA2lToken {
        filename: String,
        line: u32,
        tokentext: String,
    },

    #[error("{filename}:{line}: Invalid numerical constant \"{tokentext}\"")]
    InvalidNumericalConstant {
        filename: String,
        line: u32,
        tokentext: String,
    },

    #[error("{filename}:{line}: Block comment was not closed before the end of input was reached")]
    UnclosedComment { filename: String, line: u32 },

    #[error("{filename}:{line}: String was not closed before the end of input was reached")]
    UnclosedString { filename: String, line: u32 },

    #[error("{filename}:{line}: There is no whitespace separating the input tokens")]
    MissingWhitespace { filename: String, line: u32 },
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum A2lTokenType {
    Identifier,
    Begin,
    End,
    Include,
    String,
    Number,
}

#[derive(Debug, Clone)]
pub struct A2lToken {
    pub ttype: A2lTokenType,
    pub startpos: usize,
    pub endpos: usize,
    pub fileid: usize,
    pub line: u32,
}

#[derive(Debug)]
pub(crate) struct TokenResult {
    pub(crate) tokens: Vec<A2lToken>,
    pub(crate) filedata: Vec<String>,
    pub(crate) filenames: Vec<String>,
}

// tokenize()
// Runs the actual tokenizer, then ensures that any /include directives are resolved
pub(crate) fn tokenize(
    filename: String,
    fileid: usize,
    filetext: &str,
) -> Result<TokenResult, TokenizerError> {
    let mut filenames: Vec<String> = vec![filename.clone()];
    let mut filedatas: Vec<String> = vec![filetext.to_owned()];
    let filebytes = filetext.as_bytes();
    let mut next_fileid = fileid + 1;

    let input_tokens = tokenize_core(filename.clone(), fileid, filetext)?;
    let mut include_directives: Vec<usize> = input_tokens
        .iter()
        .enumerate()
        .filter_map(|(pos, A2lToken { ttype, .. })| {
            if *ttype == A2lTokenType::Include {
                Some(pos)
            } else {
                None
            }
        })
        .collect();

    let tokens = if include_directives.is_empty() {
        // default case: no include directives found
        input_tokens
    } else {
        // the file contains include directives, and the indices of the Include tokens are stored in include_directives

        // initialize the vector with all tokens up to the first include directive
        let mut tokens = input_tokens[0..include_directives[0]].to_vec();
        // extending include_directives with the last index of the input_tokens simplifies the logic, because then all blocks of tokens have a start and end index
        include_directives.push(input_tokens.len());
        for idx in 1..include_directives.len() {
            // token_subseq contains all tokens between the previous include directive and the current one
            let token_subseq =
                &input_tokens[include_directives[idx - 1] + 1..include_directives[idx]];
            if !token_subseq.is_empty()
                && (token_subseq[0].ttype == A2lTokenType::String
                    || token_subseq[0].ttype == A2lTokenType::Identifier)
            {
                let mut filename_start = token_subseq[0].startpos;
                let mut filename_end = token_subseq[0].endpos;
                if filebytes[filename_start] == b'"' && filebytes[filename_end - 1] == b'"' {
                    filename_start += 1;
                    filename_end -= 1;
                }
                // incname is the include filename from the filetext without the surrounding quotes
                let incname = &filetext[filename_start..filename_end];
                let incfilename = loader::make_include_filename(incname, &filenames[0]);

                // check if incname is an accessible file
                let incpathref = Path::new(&incfilename);
                let loadresult = loader::load(incpathref);
                if let Ok(incfiledata) = loadresult {
                    let mut tokresult = tokenize(incname.to_owned(), next_fileid, &incfiledata)?;

                    next_fileid += tokresult.filenames.len();

                    // append the tokens from the included file(s)
                    tokens.append(&mut tokresult.tokens);

                    // also save the names of the included file(s)
                    filenames.append(&mut tokresult.filenames);
                    filedatas.append(&mut tokresult.filedata);
                } else {
                    return Err(TokenizerError::IncludeFileError {
                        filename,
                        line: token_subseq[0].line,
                        incname: incname.to_owned(),
                    });
                }

                // append the tokens between the include directives to the output
                tokens.extend_from_slice(&token_subseq[1..]);
            } else {
                let line = input_tokens[include_directives[idx - 1]].line;
                return Err(TokenizerError::IncompleteIncludeError { filename, line });
            }
        }
        tokens
    };

    Ok(TokenResult {
        tokens,
        filenames,
        filedata: filedatas,
    })
}

// tokenize_core()
// Convert the text of an a2l file to tokens.
// During tokenization the text is treated as ASCII, even though it is actually UTF-8. It is
// possible to do this because characters outside of basic ASCII can actually only occur in
// strings and comments. UTF-8 in strings is directly copied to the output, while comments are discarded.
// An important extra goal of the tokenizer is to attach the source line number to each token so that error messages can give accurate location info
fn tokenize_core(
    filename: String,
    fileid: usize,
    filetext: &str,
) -> Result<Vec<A2lToken>, TokenizerError> {
    let filebytes = filetext.as_bytes();
    let datalen = filebytes.len();

    let mut tokens: Vec<A2lToken> = Vec::with_capacity(datalen / 20);
    let mut bytepos = 0;
    let mut separated = true;
    let mut line = 1;

    while bytepos < datalen {
        let startpos = bytepos;

        if filebytes[bytepos].is_ascii_whitespace() {
            // skip whitespace
            separated = true;
            while bytepos < datalen && filebytes[bytepos].is_ascii_whitespace() {
                bytepos += 1;
            }
            line += count_newlines(&filebytes[startpos..bytepos]);
            continue;
        } else if filebytes[bytepos] == b'/' && bytepos + 1 < datalen {
            bytepos += 1;
            if filebytes[bytepos] == b'*' {
                // block comment
                separated = true;
                bytepos = skip_block_comment(filebytes, bytepos + 1).map_err(|()| {
                    TokenizerError::UnclosedComment {
                        filename: filename.clone(),
                        line,
                    }
                })?;
                line += count_newlines(&filebytes[startpos..bytepos]);
            } else if filebytes[bytepos] == b'/' {
                // line comment
                separated = true;
                while bytepos < datalen && filebytes[bytepos] != b'\n' {
                    bytepos += 1;
                }
            } else if filebytes[bytepos..].starts_with(b"begin") {
                separator_check(separated, &filename, line)?;
                bytepos += 5;
                tokens.push(A2lToken {
                    ttype: A2lTokenType::Begin,
                    startpos,
                    endpos: bytepos,
                    fileid,
                    line,
                });
                separated = false;
            } else if filebytes[bytepos..].starts_with(b"end") {
                separator_check(separated, &filename, line)?;
                bytepos += 3;
                tokens.push(A2lToken {
                    ttype: A2lTokenType::End,
                    startpos,
                    endpos: bytepos,
                    fileid,
                    line,
                });
                separated = false;
            } else if filebytes[bytepos..].starts_with(b"include") {
                separator_check(separated, &filename, line)?;
                bytepos += 7;
                tokens.push(A2lToken {
                    ttype: A2lTokenType::Include,
                    startpos,
                    endpos: bytepos,
                    fileid,
                    line,
                });
                separated = false;
            } else {
                let endpos = if startpos + 10 < datalen {
                    startpos + 10
                } else {
                    datalen
                };
                return Err(TokenizerError::InvalidA2lToken {
                    filename,
                    line,
                    tokentext: String::from_utf8_lossy(&filebytes[startpos..endpos]).into(),
                });
            }
        } else if filebytes[bytepos] == b'"' {
            // a string
            separator_check(separated, &filename, line)?;
            bytepos = find_string_end(filebytes, bytepos + 1).map_err(|()| {
                TokenizerError::UnclosedString {
                    filename: filename.clone(),
                    line,
                }
            })?;
            line += count_newlines(&filebytes[startpos..bytepos]);
            tokens.push(A2lToken {
                ttype: A2lTokenType::String,
                startpos,
                endpos: bytepos,
                fileid,
                line,
            });
            separated = false;
        } else if !tokens.is_empty()
            && tokens.last().unwrap().ttype == A2lTokenType::Include
            && !(filebytes[bytepos]).is_ascii_digit()
            && is_identchar(filebytes[bytepos])
        {
            // a file path
            separator_check(separated, &filename, line)?;
            while bytepos < datalen && is_pathchar(filebytes[bytepos]) {
                bytepos += 1;
            }
            tokens.push(A2lToken {
                ttype: A2lTokenType::Identifier,
                startpos,
                endpos: bytepos,
                fileid,
                line,
            });
            separated = false;
        } else if !(filebytes[bytepos]).is_ascii_digit() && is_identchar(filebytes[bytepos]) {
            // an identifier
            separator_check(separated, &filename, line)?;
            while bytepos < datalen && is_identchar(filebytes[bytepos]) {
                bytepos += 1;
            }
            tokens.push(A2lToken {
                ttype: A2lTokenType::Identifier,
                startpos,
                endpos: bytepos,
                fileid,
                line,
            });
            separated = false;

            let (new_bytepos, new_line) = handle_a2ml(filetext, bytepos, line, fileid, &mut tokens);
            if bytepos != new_bytepos {
                separated = true;
            }
            bytepos = new_bytepos;
            line = new_line;
        } else if filebytes[bytepos] == b'-' || is_numchar(filebytes[bytepos]) {
            // a number, in any format (integer, floating point or hexadecimal)
            separator_check(separated, &filename, line)?;
            bytepos += 1;
            while bytepos < datalen && is_numchar(filebytes[bytepos]) {
                bytepos += 1;
            }
            if bytepos == datalen || !is_identchar(filebytes[bytepos]) {
                let number = &filebytes[startpos..bytepos];
                if number == b"-" {
                    return Err(TokenizerError::InvalidNumericalConstant {
                        filename,
                        line,
                        tokentext: "-".to_owned(),
                    });
                } else if number == b"0x" {
                    return Err(TokenizerError::InvalidNumericalConstant {
                        filename,
                        line,
                        tokentext: "0x".to_owned(),
                    });
                }
                tokens.push(A2lToken {
                    ttype: A2lTokenType::Number,
                    startpos,
                    endpos: bytepos,
                    fileid,
                    line,
                });
            } else if bytepos < datalen && is_identchar(filebytes[bytepos]) {
                // this is actually an identifier that starts with a number, which is not standard compliant
                // it is still worth recognizing, in oder to give better error messages and also the error can be bypassed if strict is false
                while bytepos < datalen && is_identchar(filebytes[bytepos]) {
                    bytepos += 1;
                }
                tokens.push(A2lToken {
                    ttype: A2lTokenType::Identifier,
                    startpos,
                    endpos: bytepos,
                    fileid,
                    line,
                });
            }
            separated = false;
        } else {
            let endpos = if startpos + 10 < datalen {
                startpos + 10
            } else {
                datalen
            };
            return Err(TokenizerError::InvalidA2lToken {
                filename,
                line,
                tokentext: String::from_utf8_lossy(&filebytes[startpos..endpos]).into(),
            });
        }
    }

    Ok(tokens)
}

// skip_block_comment
// finds the first byte position after the end of a block comment
fn skip_block_comment(filebytes: &[u8], mut bytepos: usize) -> Result<usize, ()> {
    let datalen = filebytes.len();

    bytepos += 1;
    while bytepos < datalen && !(filebytes[bytepos - 1] == b'*' && filebytes[bytepos] == b'/') {
        bytepos += 1;
    }

    if bytepos >= datalen {
        return Err(());
    }

    // currently filebytes[bytepos] == b'/', but bytepos should be set to the first character after the block comment
    bytepos += 1;

    Ok(bytepos)
}

// find_string_end
// finds the end of a string
fn find_string_end(filebytes: &[u8], mut bytepos: usize) -> Result<usize, ()> {
    let datalen = filebytes.len();
    let mut end_found = false;
    let mut prev_quote = false;
    let mut prev_bkslash = false;

    while bytepos < datalen && !end_found {
        if filebytes[bytepos] == b'"' {
            // the current char is a quote char
            // if either prev_quote or prev_bkslash is set, then this quote is escaped
            // otherwise it's the start of a new double quote escape sequence or the end of the string
            prev_quote = !(prev_quote || prev_bkslash);
            prev_bkslash = false;
        } else {
            /* current char is not a quote char */
            if prev_quote {
                /* the previous char was a quote though, so the end has been found */
                end_found = true;
            } else if filebytes[bytepos] == b'\\' {
                if prev_bkslash {
                    /* both this char and the previous one were '\', so this completes a \\ escape */
                    prev_bkslash = false;
                } else {
                    /* this char is the beginnning of an escape sequence (not necessarily \", but that's the only one that matters here) */
                    prev_bkslash = true;
                }
            } else {
                prev_bkslash = false;
            }
            prev_quote = false;
        }
        bytepos += 1;
    }
    /* if next is None here, then either the closing quote was the last character of the input (ok), or the string was not closed and the input ended */
    if bytepos == datalen && !end_found {
        if prev_quote {
            /* compensate for the fact that we weren't able to look one character past the end of the string */
            bytepos += 1;
        } else {
            return Err(());
        }
    }
    bytepos -= 1;

    Ok(bytepos)
}

// handle_a2ml()
// the data inside the A2ML block can't be tokenized according to the rules for A2L, because it is a completely different format
// handle_a2ml finds the end of the A2ML block and stores its content as a string
fn handle_a2ml(
    filedata: &str,
    mut bytepos: usize,
    mut line: u32,
    fileid: usize,
    tokens: &mut Vec<A2lToken>,
) -> (usize, u32) {
    let tokcount = tokens.len();
    if tokcount >= 2 && tokens[tokcount - 2].ttype == A2lTokenType::Begin {
        let startpos = bytepos;
        let filebytes = filedata.as_bytes();
        let datalen = filedata.len();
        let tag = &filedata[tokens[tokcount - 1].startpos..tokens[tokcount - 1].endpos];

        if tag == "A2ML" {
            let mut done = false;
            while !done && bytepos < datalen {
                // find the next occurrence of '/'
                // this should be the start of one of "/*", "//", or "/end"
                while bytepos < datalen && filebytes[bytepos] != b'/' {
                    bytepos += 1;
                }
                if filebytes[bytepos..].starts_with(b"//") {
                    // line comment
                    // skip the comment marker
                    bytepos += 2;
                    // skip the remaining characters on the line up to the newline
                    while bytepos < datalen && filebytes[bytepos] != b'\n' {
                        bytepos += 1;
                    }
                } else if filebytes[bytepos..].starts_with(b"/*") {
                    // block comment
                    // skip the comment marker
                    bytepos += 2;
                    // skip the remaining characters on the line up to the newline
                    while bytepos < (datalen - 1)
                        && !(filebytes[bytepos] == b'*' && filebytes[bytepos + 1] == b'/')
                    {
                        bytepos += 1;
                    }
                    bytepos += 2;
                    if bytepos > datalen {
                        bytepos = datalen;
                    }
                } else if filebytes[bytepos..].starts_with(b"/end") {
                    done = true;
                } else {
                    // solitary '/' hanging around? this will definitely be a parse error later on
                    bytepos += 1;
                }
            }

            // while bytepos < datalen && !(filebytes[bytepos] == b'/' && filedata[bytepos .. ].starts_with("/end A2ML")) {
            //     bytepos += 1;
            // }

            // trim off trailing whitespace up to and including the last newline - this newline and the
            // following indentation will be written together with /end A2ML
            while filebytes[bytepos - 1].is_ascii_whitespace()
                && filebytes[bytepos - 1] != b'\r'
                && filebytes[bytepos - 1] != b'\n'
            {
                bytepos -= 1;
            }
            if filebytes[bytepos - 1] == b'\r' && filebytes[bytepos - 1] == b'\n' {
                bytepos -= 2;
            } else if filebytes[bytepos - 1] == b'\n' {
                bytepos -= 1;
            }
        }

        if bytepos > startpos {
            tokens.push(A2lToken {
                ttype: A2lTokenType::String,
                startpos,
                endpos: bytepos,
                fileid,
                line,
            });

            line += count_newlines(&filebytes[startpos..bytepos]);
        }
    }

    (bytepos, line)
}

// separator_check
// generate an error message if there is no whitespace (or a block comment) separating two tokens
fn separator_check(separated: bool, filename: &str, line: u32) -> Result<(), TokenizerError> {
    if !separated {
        return Err(TokenizerError::MissingWhitespace {
            filename: filename.to_owned(),
            line,
        });
    }
    Ok(())
}

// count_newlines()
// count the number of newlines in a comment or string. This is needed to keep the line count accurate
fn count_newlines(text: &[u8]) -> u32 {
    text.iter().map(|c| u32::from(*c == b'\n')).sum()
}

// is_pathchar()
// is this char allowed in a file path, extension of is_identchar()
pub(crate) fn is_pathchar(c: u8) -> bool {
    is_identchar(c) || c == b'\\' || c == b'/'
}

// is_identchar()
// is this char allowed in an identifier
pub(crate) fn is_identchar(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'.' || c == b'[' || c == b']' || c == b'_'
}

// is_numchar()
// in addition to decimal format, numbers can also be written as hex, or as floats with exponents
// this expands the set of allowable characters beyond is_ascii_hexdigit()
fn is_numchar(c: u8) -> bool {
    c.is_ascii_hexdigit() || c == b'x' || c == b'X' || c == b'.' || c == b'+' || c == b'-'
}

/*************************************************************************************************/

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_a2l_comment() {
        let data = String::from("/**/");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 0);
        //assert_eq!(tok[0].ttype, A2lTokenType::BlockComment);

        let data = String::from("/*/*/");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 0);
        //assert_eq!(tok[0].ttype, A2lTokenType::BlockComment);

        let data = String::from("/***********/");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 0);
        //assert_eq!(tok[0].ttype, A2lTokenType::BlockComment);

        let data = String::from("/***********/ abcdef");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        //assert_eq!(tok[0].ttype, A2lTokenType::BlockComment);

        let data = String::from("//");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 0);
        //assert_eq!(tok[0].ttype, A2lTokenType::LineComment);

        let data = String::from("// abcdef");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 0);
        //assert_eq!(tok[0].ttype, A2lTokenType::LineComment);

        let data = String::from("// abcdef\nabcde");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        //assert_eq!(tok[0].ttype, A2lTokenType::LineComment);
        //assert_eq!(tok[1].data.line, 2);
    }

    #[test]
    fn tokenize_a2l_command() {
        let data = String::from("/begin");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::Begin);

        let data = String::from("/end");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::End);

        let data = String::from("/include");
        let tokresult = tokenize_core("test".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.len(), 1);
        assert_eq!(tokresult[0].ttype, A2lTokenType::Include);
    }

    #[test]
    fn tokenize_a2l_string() {
        /* empty string */
        let data = String::from(r#" "" "#);
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::String);

        /* string containing a single double quote escaped as two double quotes */
        let data = String::from(r#" """" "#);
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::String);

        /* string containing two instances of a single double quote escaped as two double quotes */
        let data = String::from(r#"" ""x"" ""#);
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::String);

        /* string containing a single double quote escaped with a backslash */
        let data = String::from(r#" "\"" "#);
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::String);

        /* string containing two instances of a single double quote escaped with a backslash */
        let data = String::from(r#"" \"x\" ""#);
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::String);

        /* a string containing text */
        let data = String::from("\"sdf sdf sdf\"");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::String);

        /* a string containing unicode characters */
        let data = String::from("\"\u{1234}\u{2345}\"");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::String);
    }

    #[test]
    fn tokenize_a2l_item() {
        let data = String::from("foo_bar");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::Identifier);
    }

    #[test]
    fn tokenize_a2l_number() {
        let data = String::from("0xabc1234");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::Number);

        let data = String::from("0ident");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 1);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::Identifier);
    }

    #[test]
    fn tokenize_a2l_skip_whitespace() {
        let data = String::from("");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 0);

        let data = String::from(" ");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 0);

        let data = String::from("\n\n ");
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 0);
    }

    #[test]
    fn tokenize_string_with_backslash() {
        let data = String::from(r#" ident "\\" 0 "#);
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.tokens.len(), 3);
    }

    #[test]
    fn tokenize_skip_a2ml() {
        let data = String::from(
            r##"
ASAP2_VERSION 1 60
/begin PROJECT Test "test test"
    /begin MODULE MODULE_NAME ""
        /begin A2ML
            struct Foo {
                uint;
                uint;
            }; /* trap: /end A2ML */
            / / //
        /end A2ML
    /end MODULE
/end PROJECT
"##,
        );
        let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
        println!("token count: {}", tokresult.tokens.len());
        assert_eq!(tokresult.tokens.len(), 20);
        assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::Identifier);
        assert_eq!(tokresult.tokens[13].ttype, A2lTokenType::String); // a2ml body text
    }

    #[test]
    fn tokenize_include() {
        let data = String::from(
            r##"
                /include ./tests/test.a2l
                /include .\tests\test.a2l
                /include ".\tests\test.a2l"
                /include "./tests/test.a2l"
            "##,
        );

        let tokresult = tokenize_core("test".to_string(), 0, &data).expect("Error");
        assert_eq!(tokresult.len(), 8);
        println!("{:?}", tokresult);
        assert_eq!(tokresult[0].ttype, A2lTokenType::Include);
        assert_eq!(tokresult[1].ttype, A2lTokenType::Identifier);
        assert_eq!(tokresult[2].ttype, A2lTokenType::Include);
        assert_eq!(tokresult[3].ttype, A2lTokenType::Identifier);
        assert_eq!(tokresult[4].ttype, A2lTokenType::Include);
        assert_eq!(tokresult[5].ttype, A2lTokenType::String);
        assert_eq!(tokresult[6].ttype, A2lTokenType::Include);
        assert_eq!(tokresult[7].ttype, A2lTokenType::String);
    }
}
