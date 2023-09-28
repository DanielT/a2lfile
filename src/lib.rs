//! a2lfile is a library that allows you to read, modify and write a2l files.
//!
//! It is fast, preserves the formatting of the input, and has support for files using standard version 1.71.

mod a2ml;
mod checker;
mod cleanup;
mod ifdata;
mod loader;
mod merge;
mod namemap;
mod parser;
mod sort;
mod specification;
mod tokenizer;
mod writer;

use std::convert::AsRef;
use std::fmt::Write;
use std::path::Path;
pub use namemap::ModuleNameMap;
// used internally
use parser::{ParseContext, ParserState};
use tokenizer::{A2lToken, A2lTokenType};

// re-export for the crate user
pub use a2lmacros::a2ml_specification;
pub use a2ml::{GenericIfData, GenericIfDataTaggedItem};
pub use specification::*;

/**
Create a new a2l file

```rust
let new_a2l = a2lfile::new();
assert_eq!(new_a2l.project.module.len(), 1);
```

The created file is equivalent to loading a string containing
```text
ASAP2_VERSION 1 71
/begin PROJECT new_project ""
  /begin MODULE new_module ""
  /end MODULE
/end PROJECT
```
 */
pub fn new() -> A2lFile {
    // a minimal a2l file needs only a PROJECT containing a MODULE
    let mut project = Project::new("new_project".to_string(), "".to_string());
    project.module = vec![Module::new("new_module".to_string(), "".to_string())];
    let mut a2l_file = A2lFile::new(project);
    // only one line break for PROJECT (after ASAP2_VERSION) instead of the default 2
    a2l_file.project.get_layout_mut().start_offset = 1;
    // only one line break for MODULE [0] instead of the default 2
    a2l_file.project.module[0].get_layout_mut().start_offset = 1;
    // also set ASAP2_VERSION 1.71
    a2l_file.asap2_version = Some(Asap2Version::new(1, 71));

    a2l_file
}

/**
Load an a2l file

`a2ml_spec` is optional and contains a String that is valid A2ML that can be used while parsing the file in addition to the A2ML that might be contained inside the A2ML block in the file.
If a definition is provided here and there is also an A2ML block in the file, then the definition provided here will be tried first during parsing.

`log_msgs` is a reference to a `Vec<String>` which will receive all warning messages generated during parsing

`strict_parsing` toggles strict parsing: If strict parsing is enabled, most warnings become errors.

```
let mut log_msgs = Vec::<String>::new();
match a2lfile::load("example.a2l", None, &mut log_msgs, true) {
    Ok(a2l_file) => {/* do something with it*/},
    Err(error_message) => println!("{}", error_message)
}
```
 */
pub fn load<P: AsRef<Path>>(
    path: P,
    a2ml_spec: Option<String>,
    log_msgs: &mut Vec<String>,
    strict_parsing: bool,
) -> Result<A2lFile, String> {
    let pathref = path.as_ref();
    let filedata = loader::load(pathref)?;
    load_impl(pathref, &filedata, log_msgs, strict_parsing, a2ml_spec)
}

/**
load a2l data stored in a string

`a2ldata` contains the text of an a2l file.

`a2ml_spec` is optional and contains a String that is valid A2ML that can be used while parsing the file in addition to the A2ML that might be contained inside the A2ML block in the file.
If a definition is provided here and there is also an A2ML block in the file, then the definition provided here will be tried first during parsing.

`log_msgs` is a reference to a `Vec<String>` which will receive all warning messages generated during parsing

`strict_parsing` toggles strict parsing: If strict parsing is enabled, most warnings become errors.

```rust
let text = r#"
ASAP2_VERSION 1 71
/begin PROJECT new_project ""
  /begin MODULE new_module ""
  /end MODULE
/end PROJECT
"#;

let mut log_msgs = Vec::<String>::new();
let a2l = a2lfile::load_from_string(&text, None, &mut log_msgs, true).unwrap();
assert_eq!(a2l.project.module[0].name, "new_module");
```
 */
pub fn load_from_string(
    a2ldata: &str,
    a2ml_spec: Option<String>,
    log_msgs: &mut Vec<String>,
    strict_parsing: bool,
) -> Result<A2lFile, String> {
    let pathref = Path::new("");
    load_impl(pathref, a2ldata, log_msgs, strict_parsing, a2ml_spec)
}

fn load_impl(
    path: &Path,
    filedata: &str,
    log_msgs: &mut Vec<String>,
    strict_parsing: bool,
    a2ml_spec: Option<String>,
) -> Result<A2lFile, String> {
    // tokenize the input data
    let tokenresult = tokenizer::tokenize(path.to_string_lossy().to_string(), 0, filedata)?;

    if tokenresult.tokens.is_empty() {
        return Err("Error: File contains no a2l data".to_string());
    }

    // create a context for the parser. Ensure that the current line of the context is set to the first line that actually contains a token
    let mut fake_token = A2lToken {
        ttype: A2lTokenType::Identifier,
        startpos: 0,
        endpos: 0,
        fileid: 0,
        line: 1,
    };
    let firstline = tokenresult.tokens.get(0).unwrap_or(&fake_token).line;
    fake_token.line = firstline;
    let context = &ParseContext::from_token("A2L_FILE", &fake_token, false);

    // create the parser state object
    let mut parser = ParserState::new(
        &tokenresult.tokens,
        &tokenresult.filedata,
        &tokenresult.filenames,
        log_msgs,
        strict_parsing,
    );

    // if a built-in A2ml specification was passed as a string, then it is parsed here
    if let Some(spec) = a2ml_spec {
        let ret = a2ml::parse_a2ml(&spec);
        if let Ok(parsed_spec) = ret {
            parser.builtin_a2mlspec = Some(parsed_spec);
        } else {
            // this shouldn't happen; if it does then there is a bug in the a2ml_specification! macro
            return Err(format!(
                "Error: Failed to load built-in specification: {}",
                ret.unwrap_err()
            ));
        }
    }

    // try to get the file version. Starting with 1.60, the ASAP2_VERSION element is mandatory. For
    // compatibility with old files, a missing version is only an error if strict parsing is requested
    if let Err(version_error) = get_version(&mut parser, context) {
        if !strict_parsing {
            parser.log_msgs.push(version_error);
        } else {
            return Err(version_error);
        }
    }
    // build the a2l data structures from the tokens
    let a2l_file = match A2lFile::parse(&mut parser, context, 0) {
        Ok(data) => data,
        Err(parse_error) => return Err(parser.stringify_parse_error(&parse_error, true)),
    };

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

    Ok(a2l_file)
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
    Err(
        "File is not recognized as an a2l file. Mandatory version information is missing."
            .to_string(),
    )
}

impl A2lFile {
    /// construct a string containing the whole a2l data of this A2lFile object
    pub fn write_to_string(&self) -> String {
        self.stringify(0)
    }

    /// write this `A2lFile` object to the given file
    /// the banner will be placed inside a comment at the beginning of the file; "/*" an "*/" should not be part of the banner string
    pub fn write<P: AsRef<Path>>(&self, path: P, banner: Option<&str>) -> Result<(), String> {
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

        if let Err(err) = std::fs::write(&path, outstr) {
            return Err(format!(
                "Error while writing output {}: {}\n",
                path.as_ref().display(),
                err
            ));
        }

        Ok(())
    }

    /// Merge another a2l file on the MODULE level.
    ///
    /// The input file and the merge file must each contain exactly one MODULE.
    /// The contents will be merged so that there is one merged MODULE in the output.
    pub fn merge_modules(&mut self, merge_file: &mut A2lFile) {
        merge::merge_modules(
            &mut self.project.module[0],
            &mut merge_file.project.module[0],
        );

        // if the merge file uses a newer file version, then the file version is upgraded by the merge
        if let Some(file_ver) = &mut self.asap2_version {
            if let Some(merge_ver) = &merge_file.asap2_version {
                if file_ver.version_no < merge_ver.version_no
                    || ((file_ver.version_no == merge_ver.version_no)
                        && (file_ver.upgrade_no < merge_ver.upgrade_no))
                {
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

    /// cleanup: remove unused GROUPs, RECORD_LAYOUTs, COMPU_METHODs, COMPU_(V)TABs and UNITs
    pub fn cleanup(&mut self) {
        cleanup::cleanup(self);
    }

    /// cleanup IF_DATA: remove any IF_DATA blocks that could not be parsed using either the
    /// specification provided during load or the specification in the A2ML block in the file
    pub fn ifdata_cleanup(&mut self) {
        ifdata::remove_unknown_ifdata(self);
    }
}


impl Module {
    pub fn build_namemap(&self) -> ModuleNameMap {
        let mut log_msgs = vec![];
        ModuleNameMap::build(&self, &mut log_msgs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_empty_file() {
        let mut log_msgs = Vec::new();
        let result = load_from_string("", None, &mut log_msgs, false);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(error, "Error: File contains no a2l data");
    }

    #[test]
    fn bad_a2ml_data() {
        let mut log_msgs = Vec::new();
        let result = load_from_string(
            r#"/begin PROJECT x "" /begin MODULE y "" /end MODULE /end PROJECT"#,
            Some("x".to_string()),
            &mut log_msgs,
            false,
        );
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.starts_with("Error: Failed to load built-in specification"))
    }

    #[test]
    fn strict_parsing_version_error() {
        // version is missing completely
        let mut log_msgs = Vec::new();
        let result = load_from_string(
            r#"/begin PROJECT x "" /begin MODULE y "" /end MODULE /end PROJECT"#,
            None,
            &mut log_msgs,
            true,
        );
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(
            error,
            "File is not recognized as an a2l file. Mandatory version information is missing."
        );

        // version is damaged
        let mut log_msgs = Vec::new();
        let result = load_from_string(
            r#"ASAP2_VERSION 1 /begin PROJECT"#,
            None,
            &mut log_msgs,
            true,
        );
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert_eq!(
            error,
            "File is not recognized as an a2l file. Mandatory version information is missing."
        );
    }

    #[test]
    fn additional_tokens() {
        // strict parsin off - no error
        let mut log_msgs = Vec::new();
        let result = load_from_string(
            r#"ASAP2_VERSION 1 71 /begin PROJECT x "" /begin MODULE y "" /end MODULE /end PROJECT abcdef"#,
            None,
            &mut log_msgs,
            false,
        );
        assert!(result.is_ok());
        assert_eq!(log_msgs.len(), 1);

        // strict parsing on - error
        let mut log_msgs = Vec::new();
        let result = load_from_string(
            r#"ASAP2_VERSION 1 71 /begin PROJECT x "" /begin MODULE y "" /end MODULE /end PROJECT abcdef"#,
            None,
            &mut log_msgs,
            true,
        );
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.ends_with("after parsed a2l file content"));
    }

    #[test]
    fn write_nonexistent_file() {
        let a2l = new();
        let result = a2l.write(
            "__NONEXISTENT__/__FILE__/__PATH__/test.a2l",
            Some("test case write_nonexistent_file()"),
        );
        assert!(result.is_err());
    }

    #[test]
    fn write_with_banner() {
        let mut a2l = new();
        a2l.asap2_version
            .as_mut()
            .unwrap()
            .get_layout_mut()
            .start_offset = 0;
        let result = a2l.write("test.a2l", Some("test case write_nonexistent_file()"));
        assert!(result.is_ok());
        let file_text = String::from_utf8(std::fs::read("test.a2l").unwrap()).unwrap();
        assert!(file_text.starts_with("/* test case write_nonexistent_file() */"));
        std::fs::remove_file("test.a2l").unwrap();

        a2l.asap2_version
            .as_mut()
            .unwrap()
            .get_layout_mut()
            .start_offset = 1;
        let result = a2l.write("test.a2l", Some("test case write_nonexistent_file()"));
        assert!(result.is_ok());
        let file_text = String::from_utf8(std::fs::read("test.a2l").unwrap()).unwrap();
        assert!(file_text.starts_with("/* test case write_nonexistent_file() */"));
        std::fs::remove_file("test.a2l").unwrap();
    }

    #[test]
    fn merge() {
        // version is copied if non exists
        let mut a2l = new();
        let mut a2l_2 = new();
        a2l.asap2_version = None;
        a2l.merge_modules(&mut a2l_2);
        assert!(a2l.asap2_version.is_some());

        // version is updated if the merged file has a higher version
        let mut a2l = new();
        let mut a2l_2 = new();
        a2l.asap2_version = Some(Asap2Version::new(1, 50));
        a2l.merge_modules(&mut a2l_2);
        assert!(a2l.asap2_version.is_some());
        assert!(matches!(
            a2l.asap2_version,
            Some(Asap2Version {
                version_no: 1,
                upgrade_no: 71,
                ..
            })
        ));
    }
}
