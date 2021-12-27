//! a2lfile is a library that allows you to read, modify and write a2l files.
//!
//! It is fast, preserves the formatting of the input, and has support for files using standard version 1.71.

mod loader;
mod tokenizer;
mod parser;
mod a2ml;
mod ifdata;
mod writer;
mod specification;
mod namemap;
mod merge;
mod checker;
mod sort;

use std::ffi::OsStr;
use std::fmt::Write;
// used internally
use tokenizer::{A2lToken, A2lTokenType};
use parser::{ParseContext, ParserState};

// re-export for the crate user
pub use a2lmacros::a2ml_specification;
pub use a2ml::GenericIfData;
pub use a2ml::GenericIfDataTaggedItem;
pub use specification::*;


/**
Load an a2l file

a2ml_spec is optional and contains a String that is valid A2ML that can be used while parsing the file in addition to the A2ML that might be contained inside the A2ML block in the file.
If a definition is provided here and there is also an A2ML block in the file, then the definition provided here will be tried first during parsing.

log_msgs is a reference to a Vec<String> which will receive all warning messages generated during parsing

strict_parsing toggles strict parsing: If strict parsing is enabled, most warnings become errors.

```
    let mut log_msgs = Vec::<String>::new();
    match a2lfile::load(&std::ffi::OsString::from("example.a2l"), None, &mut log_msgs, true) {
        Ok(a2l_file) => {/* do something with it*/},
        Err(error_message) => println!("{}", error_message)
    }
```
 */
pub fn load(filename: &OsStr, a2ml_spec: Option<String>, log_msgs: &mut Vec<String>, strict_parsing: bool) -> Result<A2lFile, String> {
    let filedata = loader::load(filename)?;
    load_impl(filename, &filedata, log_msgs, strict_parsing, a2ml_spec)
}


/// load a2l data stored in a string
pub fn load_from_string(a2ldata: &str, a2ml_spec: Option<String>, log_msgs: &mut Vec<String>, strict_parsing: bool) -> Result<A2lFile, String> {
    load_impl(OsStr::new(""), a2ldata, log_msgs, strict_parsing, a2ml_spec)
}


fn load_impl(filename: &OsStr, filedata: &str, log_msgs: &mut Vec<String>, strict_parsing: bool, a2ml_spec: Option<String>) -> Result<A2lFile, String> {
    // tokenize the input data
    let tokenresult = tokenizer::tokenize(filename.to_string_lossy().to_string(), 0, filedata)?;

    if tokenresult.tokens.is_empty() {
        return Err("Error: File contains no a2l data".to_string());
    }

    // create a context for the parser. Ensure that the current line of the context is set to the first line that actually contains a token
    let mut fake_token = A2lToken {ttype: A2lTokenType::Identifier, startpos: 0, endpos: 0, fileid: 0, line: 1};
    let firstline = tokenresult.tokens.get(0).unwrap_or(&fake_token).line;
    fake_token.line = firstline;
    let context = &ParseContext::from_token("A2L_FILE", &fake_token, false);

    // create the parser state object
    let mut parser = ParserState::new(&tokenresult.tokens, &tokenresult.filedata, &tokenresult.filenames, log_msgs, strict_parsing);

    // if a built-in A2ml specification was passed as a string, then it is parsed here
    if let Some(spec) = a2ml_spec {
        let ret = a2ml::parse_a2ml(&spec);
        if let Ok(parsed_spec) = ret {
            parser.builtin_a2mlspec = Some(parsed_spec);
        } else {
            // this shouldn't happen; if it does then there is a bug in the a2ml_specification! macro
            return Err(format!("Error: Failed to load built-in specification: {}", ret.unwrap_err()));
        }
    }

    // try to get the file version. Starting with 1.60, the ASAP2_VERSION element is mandatory. For
    // compatibility with old files, a missing version is only an error if strict parsing is requested
    if let Err(version_error) = get_version(&mut parser, context) {
        if !strict_parsing {
            parser.log_msgs.push(version_error);
        } else {
            return Err(version_error)
        }
    }
    // build the a2l data structures from the tokens
    let a2l_file = A2lFile::parse(&mut parser, context, 0);
    if let Err(parse_error) = a2l_file {
        return Err(parser.stringify_parse_error(&parse_error, true));
    }

    // make sure this is the end of the input, i.e. no additional data after the parsed data
    if let Some(token) = parser.peek_token() {
        if !strict_parsing {
            parser.log_msgs.push(
                format!("Warning on line {}: unexpected additional data \"{}...\" after parsed a2l file content", token.line, parser.get_token_text(token))
            );
        } else {
            return Err(
                format!("Error on line {}: unexpected additional data \"{}...\" after parsed a2l file content", token.line, parser.get_token_text(token))
            );
        }
    }

    Ok(a2l_file.unwrap())
}


fn get_version(parser: &mut ParserState, context: &ParseContext) -> Result<Asap2Version, String> {
    if let Some(token) = parser.peek_token() {
        let ident = parser.get_identifier(context);
        let ver_context = ParseContext::from_token("", token, false);
        if let Ok(tag) = ident {
            if tag == "ASAP2_VERSION" {
                let version = Asap2Version::parse(parser, &ver_context, 0);
                if let Ok(version) = version {
                    parser.set_tokenpos(0);
                    parser.set_file_version(version.version_no, version.upgrade_no)?;
                    return Ok(version);
                }
            }
        }
    }
    // for compatibility with 1.50 and earlier, also make it possible to catch the error and continue
    parser.set_tokenpos(0);
    parser.set_file_version(1, 50)?;
    Err("File is not recognized as an a2l file. Mandatory version information is missing.".to_string())
}


impl A2lFile {
    /// construct a string containing the whole a2l data of this A2lFile object
    pub fn write_to_string(&self) -> String {
        self.stringify(0)
    }

    /// write this A2lFile object to the given file
    /// the banner will be placed inside a comment at the beginning of the file; "/*" an "*/" should not be part of the banner string
    pub fn write(&self, filename: &OsStr, banner: Option<&str>) -> Result<(), String> {
        let mut outstr = "".to_string();

        let file_text = self.write_to_string();

        if let Some(banner_text) = banner {
            outstr = format!("/* {} */", banner_text);
            // if the first line is empty (first charachter is \n), then the banner is placed on the empty line
            // otherwise a newline is added
            if !file_text.starts_with('\n') {
                outstr.write_char('\n').unwrap();
            }
        }
        outstr.write_str(&file_text).unwrap();

        if let Err(err) = std::fs::write(filename, outstr) {
            return Err(format!("Error while writing output {}: {}\n", filename.to_string_lossy(), err.to_string()))
        }

        Ok(())
    }


    /// Merge another a2l file on the MODULE level.
    ///
    /// The input file and the merge file must each contain exactly one MODULE.
    /// The contents will be merged so that there is one merged MODULE in the output.
    pub fn merge_modules(&mut self, merge_file: &mut A2lFile) {
        merge::merge_modules(&mut self.project.module[0], &mut merge_file.project.module[0]);

        // if the merge file uses a newer file version, then the file version is upgraded by the merge
        if let Some(file_ver) = &mut self.asap2_version {
            if let Some(merge_ver) = &merge_file.asap2_version {
                if file_ver.version_no < merge_ver.version_no ||
                    ((file_ver.version_no == merge_ver.version_no) && (file_ver.upgrade_no < merge_ver.upgrade_no)) {
                    file_ver.version_no = merge_ver.version_no;
                    file_ver.upgrade_no = merge_ver.upgrade_no;
                }
            }
        } else {
            // ASAP2_VERSION is required in newer revisions of the standard, but old files might not have it
            self.asap2_version = std::mem::take(&mut merge_file.asap2_version);
        }
    }


    /// perform a consistency check on the data.
    pub fn check(&self, log_msgs: &mut Vec<String>) {
        checker::check(self, log_msgs);
    }


    /// sort the data in the a2l file.
    /// This changes the order in which the blocks will be written to an output file
    pub fn sort(&mut self) {
        sort::sort(self)
    }


    /// sort newly added or merged blocks into sensible locations between the existing blocks
    pub fn sort_new_items(&mut self) {
        sort::sort_new_items(self)
    }


    /// cleanup IF_DATA: remove any IF_DATA blocks that could not be parsed using either the
    /// specification provided during load or the specification in the A2ML block in the file
    pub fn ifdata_cleanup(&mut self) {
        ifdata::remove_unknown_ifdata(self);
    }
}


