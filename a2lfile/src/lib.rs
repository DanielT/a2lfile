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

pub use namemap::{ModuleNameMap, NameMapCompuTab, NameMapObject, NameMapTypedef};
pub use parser::ParserError;
use std::convert::AsRef;
use std::ffi::OsString;
use std::fmt::Display;
use std::path::Path;
use std::path::PathBuf;
use thiserror::Error;
pub use tokenizer::TokenizerError;
// used internally
use parser::A2lVersion;
use parser::{ParseContext, ParserState};

// re-export for the crate user
pub use a2lmacros::a2ml_specification;
pub use a2ml::{GenericIfData, GenericIfDataTaggedItem};
pub use specification::*;

#[derive(Debug, Error)]
#[non_exhaustive]
pub enum A2lError {
    /// `FileOpenError`: An `IoError` that occurred while loading a file
    #[error("Failed to load {filename}: {ioerror}")]
    FileOpenError {
        filename: PathBuf,
        ioerror: std::io::Error,
    },

    /// `FileReadError`: An `IoError` that occurred while reading from a file
    #[error("Could not read from {filename}: {ioerror}")]
    FileReadError {
        filename: PathBuf,
        ioerror: std::io::Error,
    },

    /// `EmptyFileError`: No `A2lTokens` found in the file
    #[error("File \"{filename}\" contains no a2l data")]
    EmptyFileError { filename: PathBuf },

    /// `InvalidBuiltinA2mlSpec`: Parse error while processing a built-in a2ml specification
    #[error("Failed to load built-in a2ml specification: {parse_err}")]
    InvalidBuiltinA2mlSpec { parse_err: String },

    /// `TokenizerError`: Failed to tokenize the input
    #[error("Tokenizer error: {tokenizer_error}")]
    TokenizerError { tokenizer_error: TokenizerError },

    /// `ParserError`: Invalid data, the file could not be parsed
    #[error("Parser error: {parser_error}")]
    ParserError { parser_error: ParserError },

    /// `FileWriteError`: An `IoError` that occurred while writing from a file
    #[error("Could not write to {filename}: {ioerror}")]
    FileWriteError {
        filename: PathBuf,
        ioerror: std::io::Error,
    },
}

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
#[must_use]
pub fn new() -> A2lFile {
    // a minimal a2l file needs only a PROJECT containing a MODULE
    let mut project = Project::new("new_project".to_string(), String::new());
    project.module = vec![Module::new("new_module".to_string(), String::new())];
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

`log_msgs` is a reference to a `Vec<A2LError>` which will receive all warnings generated during parsing

`strict_parsing` toggles strict parsing: If strict parsing is enabled, most warnings become errors.

# Example
```
# use a2lfile::A2lError;
let mut log_msgs = Vec::<A2lError>::new();
match a2lfile::load("example.a2l", None, &mut log_msgs, true) {
    Ok(a2l_file) => {/* do something with it*/},
    Err(error_message) => println!("{}", error_message)
}
```

# Errors

An `A2lError` provides details information if loading the file fails.
 */
pub fn load<P: AsRef<Path>>(
    path: P,
    a2ml_spec: Option<String>,
    log_msgs: &mut Vec<A2lError>,
    strict_parsing: bool,
) -> Result<A2lFile, A2lError> {
    let pathref = path.as_ref();
    let filedata = loader::load(pathref)?;
    load_impl(pathref, &filedata, log_msgs, strict_parsing, a2ml_spec)
}

/**
load a2l data stored in a string

`a2ldata` contains the text of an a2l file.

`a2ml_spec` is optional and contains a String that is valid A2ML that can be used while parsing the file in addition to the A2ML that might be contained inside the A2ML block in the file.
If a definition is provided here and there is also an A2ML block in the file, then the definition provided here will be tried first during parsing.

`log_msgs` is a reference to a `Vec<A2LError>` which will receive all warnings generated during parsing

`strict_parsing` toggles strict parsing: If strict parsing is enabled, most warnings become errors.

# Example

```rust
# use a2lfile::A2lError;
let text = r#"
ASAP2_VERSION 1 71
/begin PROJECT new_project ""
  /begin MODULE new_module ""
  /end MODULE
/end PROJECT
"#;

let mut log_msgs = Vec::<A2lError>::new();
let a2l = a2lfile::load_from_string(&text, None, &mut log_msgs, true).unwrap();
assert_eq!(a2l.project.module[0].name, "new_module");
```

# Errors

An `A2lError` provides details information if loading the data fails.
 */
pub fn load_from_string(
    a2ldata: &str,
    a2ml_spec: Option<String>,
    log_msgs: &mut Vec<A2lError>,
    strict_parsing: bool,
) -> Result<A2lFile, A2lError> {
    let pathref = Path::new("");
    load_impl(pathref, a2ldata, log_msgs, strict_parsing, a2ml_spec)
}

fn load_impl(
    path: &Path,
    filedata: &str,
    log_msgs: &mut Vec<A2lError>,
    strict_parsing: bool,
    a2ml_spec: Option<String>,
) -> Result<A2lFile, A2lError> {
    // tokenize the input data
    let tokenresult = tokenizer::tokenize(&Filename::from(path), 0, filedata)
        .map_err(|tokenizer_error| A2lError::TokenizerError { tokenizer_error })?;

    if tokenresult.tokens.is_empty() {
        return Err(A2lError::EmptyFileError {
            filename: path.to_path_buf(),
        });
    }

    // create the parser state object
    let mut parser = ParserState::new(&tokenresult, log_msgs, strict_parsing);

    // if a built-in A2ml specification was passed as a string, then it is parsed here
    if let Some(spec) = a2ml_spec {
        parser.builtin_a2mlspec = Some(
            a2ml::parse_a2ml(&Filename::from(path), &spec)
                .map_err(|parse_err| A2lError::InvalidBuiltinA2mlSpec { parse_err })?
                .0,
        );
    }

    // build the a2l data structures from the tokens
    let a2l_file = parser
        .parse_file()
        .map_err(|parser_error| A2lError::ParserError { parser_error })?;

    Ok(a2l_file)
}

/// load an a2l fragment
///
/// An a2l fragment is just the bare content of a module, without the enclosing PROJECT and MODULE.
/// Because the fragment cannot specify a version, strict parsing is not available.
///
/// # Errors
///
/// If reading or parsing of the file fails, the `A2lError` will give details about the problem.
pub fn load_fragment(a2ldata: &str) -> Result<Module, A2lError> {
    let fixed_a2ldata = format!(r#"fragment "" {a2ldata} /end MODULE"#);
    // tokenize the input data
    let tokenresult = tokenizer::tokenize(&Filename::from("(fragment)"), 0, &fixed_a2ldata)
        .map_err(|tokenizer_error| A2lError::TokenizerError { tokenizer_error })?;
    let firstline = tokenresult.tokens.first().map_or(1, |tok| tok.line);
    let context = ParseContext {
        element: "MODULE".to_string(),
        fileid: 0,
        line: firstline,
        inside_block: true,
    };

    // create the parser state object
    let mut log_msgs = Vec::<A2lError>::new();
    let mut parser = ParserState::new(&tokenresult, &mut log_msgs, false);
    parser.set_file_version(A2lVersion::V1_7_1); // doesn't really matter with strict = false

    // build the a2l data structures from the tokens
    Module::parse(&mut parser, &context, 0)
        .map_err(|parser_error| A2lError::ParserError { parser_error })
}

/// load an a2l fragment from a file
///
/// # Errors
///
/// If reading or parsing of the file fails, the `A2lError` will give details about the problem.
pub fn load_fragment_file<P: AsRef<Path>>(path: P) -> Result<Module, A2lError> {
    let pathref = path.as_ref();
    let filedata = loader::load(pathref)?;
    load_fragment(&filedata)
}

impl A2lFile {
    /// construct a string containing the whole a2l data of this `A2lFile` object
    #[must_use]
    pub fn write_to_string(&self) -> String {
        self.stringify(0)
    }

    /// write this `A2lFile` object to the given file
    /// the banner will be placed inside a comment at the beginning of the file; "/*" an "*/" should not be part of the banner string
    ///
    /// # Errors
    ///
    /// [`A2lError::FileWriteError`] if writing the file fails.
    pub fn write<P: AsRef<Path>>(&self, path: P, banner: Option<&str>) -> Result<(), A2lError> {
        let mut outstr = String::new();

        let file_text = self.write_to_string();

        if let Some(banner_text) = banner {
            outstr = format!("/* {banner_text} */");
            // if the first line is empty (first charachter is \n), then the banner is placed on the empty line
            // otherwise a newline is added
            if !file_text.starts_with('\n') {
                outstr.push('\n');
            }
        }
        outstr.push_str(&file_text);

        std::fs::write(&path, outstr).map_err(|ioerror| A2lError::FileWriteError {
            filename: path.as_ref().to_path_buf(),
            ioerror,
        })?;

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
        sort::sort(self);
    }

    /// sort newly added or merged blocks into sensible locations between the existing blocks
    pub fn sort_new_items(&mut self) {
        sort::sort_new_items(self);
    }

    /// cleanup: remove unused GROUPs, `RECORD_LAYOUTs`, `COMPU_METHODs`, COMPU_(V)TABs and UNITs
    pub fn cleanup(&mut self) {
        cleanup::cleanup(self);
    }

    /// cleanup `IF_DATA`: remove any `IF_DATA` blocks that could not be parsed using either the
    /// specification provided during load or the specification in the A2ML block in the file
    pub fn ifdata_cleanup(&mut self) {
        ifdata::remove_unknown_ifdata(self);
    }
}

impl Module {
    /// build a map of all named elements inside the module
    #[must_use]
    pub fn build_namemap(&self) -> ModuleNameMap {
        let mut log_msgs = vec![];
        ModuleNameMap::build(self, &mut log_msgs)
    }

    /// merge another module with this module
    ///
    /// Any elements in other that are not present in this module will be moved over. The other module will typically be empty at the end of the merge.
    pub fn merge(&mut self, other: &mut Module) {
        merge::merge_modules(self, other);
    }
}

#[derive(Debug, Clone)]
struct Filename {
    // the full filename, which has been extended with a base path relative to the working directory
    full: OsString,
    // the "display" name, i.e. the name that appears in an /include directive or an error message
    display: String,
}

impl Filename {
    pub(crate) fn new(full: OsString, display: &str) -> Self {
        Self {
            full,
            display: display.to_string(),
        }
    }
}

impl From<&str> for Filename {
    fn from(value: &str) -> Self {
        Self {
            full: OsString::from(value),
            display: String::from(value),
        }
    }
}

impl From<&Path> for Filename {
    fn from(value: &Path) -> Self {
        Self {
            display: value.to_string_lossy().to_string(),
            full: OsString::from(value),
        }
    }
}

impl From<OsString> for Filename {
    fn from(value: OsString) -> Self {
        Self {
            display: value.to_string_lossy().to_string(),
            full: value,
        }
    }
}

impl Display for Filename {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.display)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn load_empty_file() {
        let mut log_msgs = Vec::new();
        let result = load_from_string("", None, &mut log_msgs, false);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(matches!(error, A2lError::EmptyFileError { .. }));
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
        assert!(matches!(error, A2lError::InvalidBuiltinA2mlSpec { .. }));
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
        assert!(matches!(
            error,
            A2lError::ParserError {
                parser_error: ParserError::MissingVersionInfo
            }
        ));

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
        assert!(matches!(
            error,
            A2lError::ParserError {
                parser_error: ParserError::MissingVersionInfo
            }
        ));
    }

    #[test]
    fn additional_tokens() {
        // strict parsing off - no error
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
        assert!(matches!(
            error,
            A2lError::ParserError {
                parser_error: ParserError::AdditionalTokensError { .. }
            }
        ));
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
        // set the current working directory to a temp dir
        let dir = tempdir().unwrap();
        std::env::set_current_dir(&dir.path()).unwrap();

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

    #[test]
    fn test_load_fagment() {
        // an empty string is a valid fragment
        let result = load_fragment("");
        assert!(result.is_ok());

        // load a fragment with some data
        let result = load_fragment(
            r#"
    /begin MEASUREMENT ASAM.M.SCALAR.UBYTE.IDENTICAL
        "Scalar measurement"
        UBYTE CM.IDENTICAL 0 0 0 255
        ECU_ADDRESS 0x13A00
        FORMAT "%5.0"    /* Note: Overwrites the format stated in the computation method */
        DISPLAY_IDENTIFIER DI.ASAM.M.SCALAR.UBYTE.IDENTICAL    /* optional display identifier */
        /begin IF_DATA ETK  KP_BLOB 0x13A00 INTERN 1 RASTER 2 /end IF_DATA
    /end MEASUREMENT"#,
        );
        assert!(result.is_ok());

        // random data is not a valid fragment
        let result = load_fragment("12345");
        assert!(result.is_err());
    }
}
