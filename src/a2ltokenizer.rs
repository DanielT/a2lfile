use super::a2lloader;

#[derive(Debug, PartialEq, Clone)]
pub enum A2lTokenType {
    Identifier,
    Begin,
    End,
    Include,
    String,
    Symbol,
    Number,
    Text,
    // LineComment,
    // BlockComment,
    Eof,
}

#[derive(Debug, Clone)]
pub struct A2lToken {
    pub ttype: A2lTokenType,
    pub text: String,
    pub fileid: usize,
    pub line: u32
}

#[derive(Debug)]
pub(crate) struct A2lTokenResult {
    pub(crate) tokens: Vec<A2lToken>,
    pub(crate) filenames: Vec<String>
}


#[derive(Debug, PartialEq)]
enum SpecialBlockType {
    None,
    A2ml,
    IfData
}


// tokenize()
// Convert the text of an a2l file to tokens.
// An important extra goal of the tokenizer is to attach the source line number to each token so that error messages can give accurate location info
pub(crate) fn tokenize(filename: String, fileid: usize, filedata: &str) -> Result<A2lTokenResult, String> {
    let mut tokens: Vec<A2lToken> = Vec::with_capacity(filedata.len() / 20);
    let mut filenames: Vec<String> = vec![filename];
    let mut offset: usize = 0;
    let mut remaining = filedata.as_bytes();
    let mut line = 1;
    let mut raw_starttoken = 0;
    let mut raw_startoffset = 0;
    let mut next_fileid = fileid + 1;
    let mut special_block = SpecialBlockType::None;
    let mut balance = 0;

    while remaining.len() > 0 {
        let mut chars = remaining.iter().enumerate();
        let (mut idx, mut c) = chars.next().unwrap();
        if c.is_ascii_whitespace() {
            /* skip whitepace */
            while c.is_ascii_whitespace() {
                if *c == b'\n' {
                    line += 1;
                }

                /* this should be a destructuring assignment, but the RFC for this feature has not been accepted as of Rust 1.46 */
                let pair = chars.next().unwrap_or_else(|| (idx+1, &b'\0'));
                idx = pair.0;
                c = pair.1;
            }
            offset += idx;
            remaining = &remaining[idx..];
            continue;
        } else if remaining.starts_with(b"/*") {
            /* get a block comment */
            let mut pos = if remaining.len() < 4 { remaining.len() } else { 4 };
            while pos <= remaining.len() && (remaining[pos - 2] != b'*' || remaining[pos - 1] != b'/') {
                pos += 1;
            }
            let comment = &filedata[offset..offset+pos];
            line += count_newlines(comment);
            // tokens.push(A2lToken{ttype: A2lTokenType::BlockComment, data: Itemdata{text: String::from(comment), line}});
            offset += pos;
            remaining = &remaining[pos..];

        } else if remaining.starts_with(b"//") {
            /* get a line comment */
            let mut pos = 2;
            while pos < remaining.len() && remaining[pos] != b'\n' {
                pos += 1;
            }
            //let mut comment = &filedata[offset..offset+pos];
            //let endpos = comment.rfind(|c: char| {!c.is_whitespace()}).unwrap();
            //comment = &comment[0..endpos];
            //tokens.push(A2lToken{ttype: A2lTokenType::LineComment, data: Itemdata{text: String::from(comment), line}});
            offset += pos;
            remaining = &remaining[pos..];
            
        } else if remaining.starts_with(b"\"") {
            /* string */
            let mut end_found = false;
            let mut next = chars.next();
            let mut prev_quote = false;
            let mut prev_bkslash = false;
            while next.is_some() && !end_found {
                let pair = next.unwrap();
                idx = pair.0;
                c = pair.1;
                if *c == b'"' {
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
                    } else if *c == b'\\' {
                        /* this char is the beginnning of an escape sequence (not necessarily \", but that's the only one that matters here) */
                        prev_bkslash = true;
                    } else {
                        prev_bkslash = false;
                    }
                    prev_quote = false;
                }
                next = chars.next();
            }
            /* if next is None here, then either the closing quote was the last character of the input (ok), or the string was not closed and the input ended (should be an error) */
            if next.is_none() {
                /* compensate for the fact that we weren't able to look one character past the end of the string */
                idx += 1;
            }
            let quotedstring = &filedata[offset+1..offset+idx-1];
            line += count_newlines(quotedstring);
            tokens.push(A2lToken{ttype: A2lTokenType::String, text: String::from(quotedstring), fileid, line});
            offset += idx;
            remaining = &remaining[idx..];
        } else if remaining.starts_with(b"/begin") {
            tokens.push(A2lToken{ttype: A2lTokenType::Begin, text: String::from(""), fileid, line});
            offset += 6;
            remaining = &remaining[6..];
            let next = remaining.iter().next();
            if next.is_some() && !next.unwrap().is_ascii_whitespace() {
                return Err(format!("missing whitespace after /begin on line {}", line));
            }
        } else if remaining.starts_with(b"/end") {
            tokens.push(A2lToken{ttype: A2lTokenType::End, text: String::from(""), fileid, line});
            offset += 4;
            remaining = &remaining[4..];
            let next = remaining.iter().next();
            if next.is_some() && !next.unwrap().is_ascii_whitespace() {
                return Err(format!("missing whitespace after /end on line {}", line));
            }
        } else if remaining.starts_with(b"/include") {
            tokens.push(A2lToken{ttype: A2lTokenType::Include, text: String::from(""), fileid, line});
            offset += 8;
            remaining = &remaining[8..];
            let next = remaining.iter().next();
            if next.is_some() && !next.unwrap().is_ascii_whitespace() {
                return Err(format!("missing whitespace after /include on line {}", line));
            }
        } else if !(*c).is_ascii_digit() && is_identchar(*c) {
            while is_identchar(*c) {
                /* this should be a destructuring assignment, but the RFC for this feature has not been accepted as of Rust 1.46 */
                let pair = chars.next().unwrap_or_else(|| (idx+1, &b'\0'));
                idx = pair.0;
                c = pair.1;
            }
            let item = &filedata[offset..offset+idx];
            tokens.push(A2lToken{ttype: A2lTokenType::Identifier, text: String::from(item), fileid, line});
            offset += idx;
            remaining = &remaining[idx..];
        } else if *c == b'-' || is_numchar(*c) {
            while is_numchar(*c) {
                let pair = chars.next().unwrap_or_else(|| (idx+1, &b'\0'));
                idx = pair.0;
                c = pair.1;
            }
            let number = &filedata[offset..offset+idx];
            tokens.push(A2lToken{ttype: A2lTokenType::Number, text: String::from(number), fileid, line});
            offset += idx;
            remaining = &remaining[idx..];
        } else {
            if !c.is_ascii_graphic() {
                return Err(format!("non-printable character 0x{:02x} on line {}", *c, line));
            }
            let symbol = &filedata[offset..offset+1];
            tokens.push(A2lToken{ttype: A2lTokenType::Symbol, text: String::from(symbol), fileid, line});
            offset += 1;
            remaining = &remaining[1..];
        }

        if tokens.len() >= 2 {
            // Handle the two special block types: A2ML and IF_DATA
            // A2ML is special because various A2ML reserved words (e.g. block, struct, taggedstruct) are not reserved in A2L.
            // This means the A2L tokenizer is only useful in order to correctly skip over comments and find the end of the A2ML section.
            // Then the A2ml tokens are removed from the token list and the originaly A2ml text is sotred instead so that it can be processed later.
            // IF_DATA is special because the parser does not necessarily understand the content of this section.
            // In particular, this is a problem when writing files containing unknown IF_DATA, because it is not possible to format the tokens nicely.
            // Having the original input text available is useful, because it can be directly copied into the output file
            if special_block == SpecialBlockType::None {
                // A2ML and IF_DATA cannot be nested, so special block handling can only begin if there is none in progress yet.
                if tokens[tokens.len() - 2].ttype == A2lTokenType::Begin && tokens[tokens.len() - 1].text == "A2ML" {
                    raw_starttoken = tokens.len();
                    raw_startoffset = offset;
                    special_block = SpecialBlockType::A2ml;
                    balance = 1;
                } else if tokens[tokens.len() - 2].ttype == A2lTokenType::Begin && tokens[tokens.len() - 1].text == "IF_DATA" {
                    raw_starttoken = tokens.len();
                    raw_startoffset = offset;
                    special_block = SpecialBlockType::IfData;
                    balance = 1;
                }
            } else {
                // special block handling is in progress
                if tokens[tokens.len() - 1].ttype == A2lTokenType::Begin {
                    balance += 1;
                } else if tokens[tokens.len() - 1].ttype == A2lTokenType::End {
                    balance -= 1;
                } else if balance == 0 {
                    if (special_block == SpecialBlockType::A2ml && tokens[tokens.len() - 1].text == "A2ML") ||
                       (special_block == SpecialBlockType::IfData && tokens[tokens.len() - 1].text == "IF_DATA") {
                        // only add a text token if there was anything other than whitespace between /begin X and /end X
                        if raw_starttoken < tokens.len() - 2 {
                            for _ in raw_starttoken..tokens.len()-2 {
                                tokens.remove(raw_starttoken);
                            }
                            let mut endoffset = offset - 4;
                            while &filedata[endoffset..endoffset+4] != "/end" {
                                endoffset -= 1;
                            }
                            let raw_text = &filedata[raw_startoffset..endoffset];
                            tokens.insert(raw_starttoken, A2lToken{ttype: A2lTokenType::Text, text: String::from(raw_text), fileid, line});
                            tokens.insert(raw_starttoken+1, A2lToken{ttype: A2lTokenType::String, text: filenames[0].clone(), fileid, line});
                        }
                        raw_starttoken = 0;
                        special_block = SpecialBlockType::None;
                    }
                }
            }

            // process /include statements
            // /include foo.a2l and /include "foo.a2l" are both valid
            // /include is not permitted inside <A2ML> blocks
            if raw_starttoken == 0 &&
                tokens[tokens.len() - 2].ttype == A2lTokenType::Include &&
                (tokens[tokens.len() - 1].ttype == A2lTokenType::String || tokens[tokens.len() - 1].ttype == A2lTokenType::Identifier) {
                let incname = tokens[tokens.len() - 1].text.clone();

                let incfilename = make_include_filename(&incname, &filenames[0]);

                // check if incname is an accessible file
                let loadresult = a2lloader::load(&incfilename);
                if let Ok(incfiledata) = loadresult {
                    let mut tokresult = tokenize(incname, next_fileid, &incfiledata)?;

                    next_fileid += tokresult.filenames.len();

                    // remove the include directive
                    tokens.remove(tokens.len() - 1);
                    tokens.remove(tokens.len() - 1);
                    // and append the tokens from the included file(s)
                    tokens.append(&mut tokresult.tokens);

                    // also save the names of the included file(s)
                    filenames.append(&mut tokresult.filenames);
                }
                else {
                    return Err(format!("Error: Failed to load included file {}", incname));
                }
            }
        }
    }

    Ok(A2lTokenResult { tokens, filenames })
}


// count_newlines()
// count the number of newlines in a comment or string. This is needed to keep the line count accurate
fn count_newlines(text: &str) -> u32 {
    text.chars().map(|c| if c == '\n' {1} else {0}).sum()
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


fn make_include_filename(incname: &str, base_filename: &str) -> String {
    let base = std::path::Path::new(base_filename);
    if let Some(basedir) = base.parent() {
        let joined = basedir.join(incname);
        if joined.exists() {
            return joined.to_str().unwrap().to_owned();
        }
    }

    incname.to_string()
}


impl A2lTokenResult {
    pub fn finalize(&mut self) {
        let final_line = self.tokens[self.tokens.len() - 1].line;

        // in the parser, parse_block_elements() expects each block to end with (End) (Blockname).
        // In order to use the same function at the top level, the ending sequence needs to be faked.
        // This is done by inserting (End) (FILE_ROOT) before the (Eof) marker at the end of the token sequence */
        self.tokens.push(A2lToken {ttype: A2lTokenType::End, text: "".to_string(), line: final_line, fileid: 0});
        self.tokens.push(A2lToken {ttype: A2lTokenType::Identifier, text: "A2L_FILE".to_string(), line: final_line, fileid: 0});

        // add an end of file token to signal that there is no more input during parsing
        self.tokens.push(A2lToken{ttype: A2lTokenType::Eof, text: String::from(""), line: final_line, fileid: 0});
    }
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
