use std::ffi::OsString;

use super::loader;

#[derive(Debug, PartialEq, Clone)]
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
    pub line: u32
}

#[derive(Debug)]
pub(crate) struct TokenResult {
    pub(crate) tokens: Vec<A2lToken>,
    pub(crate) filedata: Vec<String>,
    pub(crate) filenames: Vec<String>
}


// tokenize()
// Convert the text of an a2l file to tokens.
// During tokenization the text is treated as ASCII, even though it is actually UTF-8. It is
// possible to do this because characters outside of basic ASCII can actually only occur in
// strings and comments. UTF-8 in strings is directly copied to the output, while comments are discarded.
// An important extra goal of the tokenizer is to attach the source line number to each token so that error messages can give accurate location info
pub(crate) fn tokenize(filename: String, fileid: usize, filedata: &str) -> Result<TokenResult, String> {
    let mut filenames: Vec<String> = vec![filename];
    let filebytes = filedata.as_bytes();
    let mut tokens: Vec<A2lToken> = Vec::with_capacity(filedata.len() / 20);
    let mut next_fileid = fileid + 1;
    let datalen = filedata.len();
    let mut bytepos = 0;
    let mut separated = true;
    let mut line = 1;
    let mut filedatas: Vec<String> = vec![];

    filedatas.push(filedata.to_string());

    while bytepos < datalen {
        let startpos = bytepos;

        if filebytes[bytepos].is_ascii_whitespace() {
            // skip whitespace
            separated = true;
            while bytepos < datalen && filebytes[bytepos].is_ascii_whitespace() {
                bytepos += 1;
            }
            line += count_newlines(&filebytes[startpos .. bytepos]);
            continue;
        } else if filebytes[bytepos] == b'/' && bytepos + 1 < datalen {
            bytepos += 1;
            if filebytes[bytepos] == b'*' {
                // block comment
                separated = true;
                bytepos = skip_block_comment(filebytes, bytepos + 1, line)?;
                line += count_newlines(&filebytes[startpos .. bytepos]);
            } else if filebytes[bytepos] == b'/' {
                // line comment
                separated = true;
                while bytepos < datalen && filebytes[bytepos] != b'\n' {
                    bytepos += 1;
                }
            } else if filedata[bytepos .. ].starts_with("begin") {
                separator_check(separated, line)?;
                bytepos += 5;
                tokens.push(A2lToken{ttype: A2lTokenType::Begin, startpos, endpos: bytepos, fileid, line});
                separated = false;
            } else if filedata[bytepos .. ].starts_with("end") {
                separator_check(separated, line)?;
                bytepos += 3;
                tokens.push(A2lToken{ttype: A2lTokenType::End, startpos, endpos: bytepos, fileid, line});
                separated = false;
            } else if filedata[bytepos .. ].starts_with("include") {
                separator_check(separated, line)?;
                bytepos += 7;
                tokens.push(A2lToken{ttype: A2lTokenType::Include, startpos, endpos: bytepos, fileid, line});
                separated = false;
            } else {
                let endpos = if startpos + 10 < datalen {startpos + 10} else {datalen};
                return Err(format!("Error: input text \"{}...\" on line {} was not recognized as an a2l token", String::from_utf8_lossy(&filebytes[startpos..endpos]), line));
            }
        } else if filebytes[bytepos] == b'"' {
            // a string
            separator_check(separated, line)?;
            bytepos = find_string_end(filebytes, bytepos + 1, line)?;
            line += count_newlines(&filebytes[startpos .. bytepos]);
            tokens.push(A2lToken{ttype: A2lTokenType::String, startpos, endpos: bytepos, fileid, line});
            separated = false;
        } else if !(filebytes[bytepos]).is_ascii_digit() && is_identchar(filebytes[bytepos]) {
            // an identifier
            separator_check(separated, line)?;
            while bytepos < datalen && is_identchar(filebytes[bytepos]) {
                bytepos += 1;
            }
            tokens.push(A2lToken{ttype: A2lTokenType::Identifier, startpos, endpos: bytepos, fileid, line});
            separated = false;

            let (new_bytepos, new_line) = handle_a2ml(filedata, bytepos, line, fileid, &mut tokens);
            if bytepos != new_bytepos {
                separated = true;
            }
            bytepos = new_bytepos;
            line = new_line;
        } else if filebytes[bytepos] == b'-' || is_numchar(filebytes[bytepos]) {
            // a number, in any format (integer, floating point or hexadecimal)
            separator_check(separated, line)?;
            bytepos += 1;
            while bytepos < datalen && is_numchar(filebytes[bytepos]) {
                bytepos += 1;
            }
            let number = &filedata[startpos..bytepos];
            if number == "-" {
                return Err(format!("Error: Invalid numerical constant consisting of only \"-\" found on line {}", line));
            } else if number == "0x" {
                return Err(format!("Error: Invalid numerical constant consisting of only \"0x\" found on line {}", line));
            }
            tokens.push(A2lToken{ttype: A2lTokenType::Number, startpos, endpos: bytepos, fileid, line});
            separated = false;
        } else {
            let endpos = if startpos + 10 < datalen {startpos + 10} else {datalen};
            return Err(format!("failed to tokenize characters \"{}...\" on line {}", String::from_utf8_lossy(&filebytes[startpos..endpos]), line));
        }

        if tokens.len() >= 2 {
            // process /include statements
            // /include foo.a2l and /include "foo.a2l" are both valid
            // /include is not permitted inside <A2ML> blocks
            if tokens[tokens.len() - 2].ttype == A2lTokenType::Include &&
              (tokens[tokens.len() - 1].ttype == A2lTokenType::String || tokens[tokens.len() - 1].ttype == A2lTokenType::Identifier) {
                let prevtok = &tokens[tokens.len() - 1];
                let mut filename_start = prevtok.startpos;
                let mut filename_end = prevtok.endpos;
                if filebytes[filename_start] == b'"' && filebytes[filename_end-1] == b'"' {
                    filename_start += 1;
                    filename_end -= 1;
                }
                let incname = &filedata[filename_start .. filename_end];

                let incfilename = make_include_filename(&incname, &filenames[0]);

                // check if incname is an accessible file
                let loadresult = loader::load(&incfilename);
                if let Ok(incfiledata) = loadresult {
                    let mut tokresult = tokenize(incname.to_string(), next_fileid, &incfiledata)?;

                    next_fileid += tokresult.filenames.len();

                    // remove the include directive
                    tokens.remove(tokens.len() - 1);
                    tokens.remove(tokens.len() - 1);
                    // and append the tokens from the included file(s)
                    tokens.append(&mut tokresult.tokens);

                    // also save the names of the included file(s)
                    filenames.append(&mut tokresult.filenames);
                    filedatas.append(&mut tokresult.filedata);
                }
                else {
                    return Err(format!("Error: Failed to load included file {}", incname));
                }
            }
        }
    }

    Ok(TokenResult { tokens, filenames, filedata: filedatas })
}


// skip_block_comment
// finds the first byte position after the end of a block comment
fn skip_block_comment(filebytes: &[u8], mut bytepos: usize, line: u32) -> Result<usize, String> {
    let datalen = filebytes.len();

    bytepos += 1;
    while bytepos < datalen && !(filebytes[bytepos - 1] == b'*' && filebytes[bytepos] == b'/') {
        bytepos += 1;
    }

    if bytepos >= datalen {
        return Err(format!("Error: end of input reached before */ was found to close the block comment that started on line {}", line));
    }

    // currently filebytes[bytepos] == b'/', but bytepos should be set to the first character after the block comment
    bytepos += 1;

    Ok(bytepos)
}


// find_string_end
// finds the end of a string
fn find_string_end(filebytes: &[u8],  mut bytepos: usize, line: u32) -> Result<usize, String> {
    let datalen = filebytes.len();
    let mut end_found = false;
    let mut prev_quote = false;
    let mut prev_bkslash = false;

    while bytepos < datalen && !end_found {
        if filebytes[bytepos] == b'"' {
            /* the current char is a quote char */
            if prev_quote || prev_bkslash {
                /* this quote char has been escaped as either "" or \" */
            } else {
                /* this quote has not been escaped, so it's the first of a new escape sequence or the end of the string */
                prev_quote = true;
            }
            prev_bkslash = false;
        } else {
            /* current char is not a quote char */
            if prev_quote {
                /* the previous char was a quote though, so the end has been found */
                end_found = true;
            } else if filebytes[bytepos] == b'\\' {
                /* this char is the beginnning of an escape sequence (not necessarily \", but that's the only one that matters here) */
                prev_bkslash = true;
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
            return Err(format!("Error: end of file found in unclosed string starting on line {}", line));
        }
    }
    bytepos -= 1;

    Ok(bytepos)
}


// handle_a2ml()
// the data inside the A2ML block can't be tokenized according to the rules for A2L, because it is a completely different format
// handle_a2ml finds the end of the A2ML block and stores its content as a string
fn handle_a2ml(filedata: &str, mut bytepos: usize, mut line: u32, fileid: usize, tokens: &mut Vec<A2lToken>) -> (usize, u32) {
    let tokcount = tokens.len();
    if tokcount >= 2 {
        if tokens[tokcount-2].ttype == A2lTokenType::Begin {
            let startpos = bytepos;
            let filebytes = filedata.as_bytes();
            let datalen = filedata.len();
            let tag = &filedata[tokens[tokcount-1].startpos .. tokens[tokcount-1].endpos];

            if tag == "A2ML" {
                while bytepos < datalen && !(filebytes[bytepos] == b'/' && filedata[bytepos .. ].starts_with("/end A2ML")) {
                    bytepos += 1;
                }

                // trim off trailing whitespace up to and including the last newline - this newline and the
                // following indentation will be written together with /end A2ML
                while filebytes[bytepos-1].is_ascii_whitespace() && filebytes[bytepos-1] != b'\r' && filebytes[bytepos-1] != b'\n' {
                    bytepos -= 1;
                }
                if filebytes[bytepos-1] == b'\r' && filebytes[bytepos-1] == b'\n' {
                    bytepos -= 2;
                } else if filebytes[bytepos-1] == b'\n' {
                    bytepos -= 1;
                }
            }

            if bytepos > startpos {
                tokens.push(A2lToken{ttype: A2lTokenType::String, startpos, endpos: bytepos, fileid, line});

                line += count_newlines(&filebytes[startpos .. bytepos]);
            }
        }
    }

    (bytepos, line)
}


// separator_check
// generate an error message if there is no whitespace (or a block comment) separating two tokens
fn separator_check(separated: bool, line: u32) -> Result<(), String> {
    if !separated {
        return Err(format!("Error: There is no whitespace separating the input tokens on line {} ", line))
    }
    Ok(())
}


// count_newlines()
// count the number of newlines in a comment or string. This is needed to keep the line count accurate
fn count_newlines(text: &[u8]) -> u32 {
    text.iter().map(|c| if *c == b'\n' {1} else {0}).sum()
}


// is_identchar()
// is this char allowed in an identifier
fn is_identchar(c: u8) -> bool {
    c.is_ascii_alphanumeric() || c == b'.' || c == b'[' || c == b']' || c == b'_'
}


// is_numchar()
// in addition to decimal format, numbers can also be written as hex, or as floats with exponents
// this expands the set of allowable characters beyond is_ascii_hexdigit()
fn is_numchar(c: u8) -> bool {
    c.is_ascii_hexdigit() || c == b'x' || c == b'X' || c == b'.' || c == b'+' || c == b'-'
}


fn make_include_filename(incname: &str, base_filename: &str) -> OsString {
    let base = std::path::Path::new(base_filename);
    if let Some(basedir) = base.parent() {
        let joined = basedir.join(incname);
        if joined.exists() {
            return OsString::from(joined);
        }
    }

    OsString::from(incname)
}


/*************************************************************************************************/




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
    let tokresult = tokenize("testcase".to_string(), 0, &data).expect("Error");
    assert_eq!(tokresult.tokens.len(), 1);
    assert_eq!(tokresult.tokens[0].ttype, A2lTokenType::Include);
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

    /* string containing a single double quote escaped with a backslash */
    let data = String::from(r#" "\"" "#);
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
