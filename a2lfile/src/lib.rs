//! a2lfile is a library that allows you to read, modify and write a2l files.
//!
//! It is fast, preserves the formatting of the input, and has support for files using standard version 1.71.
//!
//! # Features
//!
//! - `check`: perform a consistency check on the data
//! - `cleanup`: remove unused `GROUP`s, `RECORD_LAYOUTs`, `COMPU_METHOD`s, `COMPU_(V)TAB`s and `UNIT`s
//! - `ifdata_cleanup`: remove any `IF_DATA` blocks that could not be parsed using either the specification provided during load or the specification in the A2ML block in the file
//! - `merge`: merge two a2l files on the `MODULE` level
//! - `sort`: sort the data in the a2l file

mod a2ml;
#[cfg(feature = "check")]
mod checker;
#[cfg(feature = "cleanup")]
mod cleanup;
mod ifdata;
mod itemlist;
mod loader;
#[cfg(feature = "merge")]
mod merge;
mod module;
mod parser;
#[cfg(feature = "sort")]
mod sort;
mod specification;
mod tokenizer;
mod writer;

pub use itemlist::ItemList;
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

    /// `NameCollisionError`: A name collision occurred bateween two blocks of the same type
    #[error(
        "Name collision: {blockname} blocks on line {line_1} and {line_2} both use the name \"{item_name}\""
    )]
    NameCollisionError {
        item_name: String,
        blockname: String,
        line_1: u32,
        line_2: u32,
    },

    /// `NameCollisionError2`: A name collision occurred bateween two different blocks which share the same namespace
    #[error(
        "Name collision: {blockname_1} on line {line_1} and {blockname_2} on line {line_2} both use the name \"{item_name}\""
    )]
    NameCollisionError2 {
        item_name: String,
        blockname_1: String,
        line_1: u32,
        blockname_2: String,
        line_2: u32,
    },

    /// `CrossReferenceError`: A reference to a non-existent item was found
    #[error(
        "Cross-reference error: {source_type} {source_name} on line {source_line} references a non-existent {target_type} {target_name}"
    )]
    CrossReferenceError {
        source_type: String,
        source_name: String,
        source_line: u32,
        target_type: String,
        target_name: String,
    },

    /// `LimitCheckError`: The given limits are outside of the calculated limits
    #[error(
        "Limit check error: {blockname} {item_name} on line {line} has limits {lower_limit} .. {upper_limit}, but the calculated limits are {calculated_lower_limit} .. {calculated_upper_limit}"
    )]
    LimitCheckError {
        item_name: String,
        blockname: String,
        line: u32,
        lower_limit: f64,
        upper_limit: f64,
        calculated_lower_limit: f64,
        calculated_upper_limit: f64,
    },

    /// `GroupStructureError`: A GROUP block cannot be both a ROOT and a sub group at the same time. it also cannot be a sub group of multiple groups
    #[error("Group structure error: GROUP {group_name} on line {line} {description}")]
    GroupStructureError {
        group_name: String,
        line: u32,
        description: String,
    },

    /// `ContentError`: A block contains invalid content of some description
    #[error("Content error: {blockname} {item_name} on line {line}: {description}")]
    ContentError {
        item_name: String,
        blockname: String,
        line: u32,
        description: String,
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
    project.module = ItemList::default();
    project
        .module
        .push(Module::new("new_module".to_string(), String::new()));
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
match a2lfile::load("example.a2l", None, true) {
    Ok((a2l_file, log_messages)) => {/* do something with it*/},
    Err(error_message) => println!("{error_message}")
}
```

# Errors

An `A2lError` provides details information if loading the file fails.
 */
pub fn load<P: AsRef<Path>>(
    path: P,
    a2ml_spec: Option<String>,
    strict_parsing: bool,
) -> Result<(A2lFile, Vec<A2lError>), A2lError> {
    let pathref = path.as_ref();
    let filedata = loader::load(pathref)?;
    load_impl(pathref, &filedata, strict_parsing, a2ml_spec)
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
# use crate::a2lfile::A2lObjectName;
# fn main() -> Result<(), A2lError> {
let text = r#"
ASAP2_VERSION 1 71
/begin PROJECT new_project ""
  /begin MODULE new_module ""
  /end MODULE
/end PROJECT
"#;
let (a2l, log_msgs) = a2lfile::load_from_string(&text, None, true).unwrap();
assert_eq!(a2l.project.module[0].get_name(), "new_module");
# Ok(())
# }
```

# Errors

An `A2lError` provides details information if loading the data fails.
 */
pub fn load_from_string(
    a2ldata: &str,
    a2ml_spec: Option<String>,
    strict_parsing: bool,
) -> Result<(A2lFile, Vec<A2lError>), A2lError> {
    let pathref = Path::new("");
    load_impl(pathref, a2ldata, strict_parsing, a2ml_spec)
}

fn load_impl(
    path: &Path,
    filedata: &str,
    strict_parsing: bool,
    a2ml_spec: Option<String>,
) -> Result<(A2lFile, Vec<A2lError>), A2lError> {
    let mut log_msgs = Vec::<A2lError>::new();
    // tokenize the input data
    let tokenresult = tokenizer::tokenize(&Filename::from(path), 0, filedata)
        .map_err(|tokenizer_error| A2lError::TokenizerError { tokenizer_error })?;

    if tokenresult.tokens.is_empty() {
        return Err(A2lError::EmptyFileError {
            filename: path.to_path_buf(),
        });
    }

    // create the parser state object
    let mut parser = ParserState::new(&tokenresult, &mut log_msgs, strict_parsing);

    // if a built-in A2ml specification was passed as a string, then it is parsed here
    if let Some(spec) = a2ml_spec {
        parser.a2mlspec.push(
            a2ml::parse_a2ml(&Filename::from(path), &spec)
                .map_err(|parse_err| A2lError::InvalidBuiltinA2mlSpec { parse_err })?
                .0,
        );
    }

    // build the a2l data structures from the tokens
    let a2l_file = parser
        .parse_file()
        .map_err(|parser_error| A2lError::ParserError { parser_error })?;

    Ok((a2l_file, log_msgs))
}

/// load an a2l fragment
///
/// An a2l fragment is just the bare content of a module, without the enclosing PROJECT and MODULE.
/// Because the fragment cannot specify a version, strict parsing is not available.
///
/// # Errors
///
/// If reading or parsing of the file fails, the `A2lError` will give details about the problem.
pub fn load_fragment(a2ldata: &str, a2ml_spec: Option<String>) -> Result<Module, A2lError> {
    let fixed_a2ldata = format!(r#"fragment "" {a2ldata} /end MODULE"#);
    // tokenize the input data
    let tokenresult = tokenizer::tokenize(&Filename::from("(fragment)"), 0, &fixed_a2ldata)
        .map_err(|tokenizer_error| A2lError::TokenizerError { tokenizer_error })?;
    let firstline = tokenresult.tokens.first().map_or(1, |tok| tok.line);
    let context = ParseContext {
        element: "MODULE".to_string(),
        fileid: 0,
        line: firstline,
    };

    // create the parser state object
    let mut log_msgs = Vec::<A2lError>::new();
    let mut parser = ParserState::new(&tokenresult, &mut log_msgs, false);
    parser.set_file_version(A2lVersion::V1_7_1); // doesn't really matter with strict = false

    // if a built-in A2ml specification was passed as a string, then it is parsed here
    if let Some(spec) = a2ml_spec {
        parser.a2mlspec.push(
            a2ml::parse_a2ml(&Filename::from("(built-in)"), &spec)
                .map_err(|parse_err| A2lError::InvalidBuiltinA2mlSpec { parse_err })?
                .0,
        );
    }
    // build the a2l data structures from the tokens
    Module::parse(&mut parser, &context, 0)
        .map_err(|parser_error| A2lError::ParserError { parser_error })
}

/// load an a2l fragment from a file
///
/// # Errors
///
/// If reading or parsing of the file fails, the `A2lError` will give details about the problem.
pub fn load_fragment_file<P: AsRef<Path>>(
    path: P,
    a2ml_spec: Option<String>,
) -> Result<Module, A2lError> {
    let pathref = path.as_ref();
    let filedata = loader::load(pathref)?;
    load_fragment(&filedata, a2ml_spec)
}

impl A2lFile {
    /// construct a string containing the whole a2l data of this `A2lFile` object
    #[must_use]
    pub fn write_to_string(&self) -> String {
        self.stringify(0)
    }

    /// write this `A2lFile` object to the given file
    /// the banner will be placed inside a comment at the beginning of the file; `/*` and `*/` should not be part of the banner string
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

    #[cfg(feature = "merge")]
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

    #[cfg(feature = "check")]
    /// perform a consistency check on the data.
    #[must_use]
    pub fn check(&self) -> Vec<A2lError> {
        checker::check(self)
    }

    #[cfg(feature = "sort")]
    /// sort the data in the a2l file.
    /// This changes the order in which the blocks will be written to an output file
    pub fn sort(&mut self) {
        sort::sort(self);
    }

    #[cfg(feature = "sort")]
    /// sort newly added or merged blocks into sensible locations between the existing blocks
    pub fn sort_new_items(&mut self) {
        sort::sort_new_items(self);
    }

    #[cfg(feature = "cleanup")]
    /// cleanup: remove unused GROUPs, `RECORD_LAYOUTs`, `COMPU_METHODs`, COMPU_(V)TABs and UNITs
    pub fn cleanup(&mut self) {
        cleanup::cleanup(self);
    }

    #[cfg(feature = "ifdata_cleanup")]
    /// cleanup `IF_DATA`: remove any `IF_DATA` blocks that could not be parsed using either the
    /// specification provided during load or the specification in the A2ML block in the file
    pub fn ifdata_cleanup(&mut self) {
        ifdata::remove_unknown_ifdata(self);
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
        let result = load_from_string("", None, false);
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(matches!(error, A2lError::EmptyFileError { .. }));
    }

    #[test]
    fn test_load_file() {
        let dir = tempdir().unwrap();

        // create a file in a temp directory and load it
        let path = dir.path().join("test.a2l");
        let path = path.to_str().unwrap();

        let text = r#"
            ASAP2_VERSION 1 71
            /begin PROJECT new_project ""
                /begin MODULE new_module ""
                /end MODULE
            /end PROJECT
        "#;
        std::fs::write(path, text).unwrap();

        let (a2l, _) = load(path, None, false).unwrap();
        assert_eq!(a2l.project.module[0].name, "new_module");

        // try to load a file that does not exist
        let nonexistent_path = dir.path().join("nonexistent.a2l");
        let nonexistent_path = nonexistent_path.to_str().unwrap();
        let result = load(nonexistent_path, None, false);
        assert!(matches!(result, Err(A2lError::FileOpenError { .. })));
    }

    #[test]
    fn bad_a2ml_data() {
        let result = load_from_string(
            r#"/begin PROJECT x "" /begin MODULE y "" /end MODULE /end PROJECT"#,
            Some("x".to_string()),
            false,
        );
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(matches!(error, A2lError::InvalidBuiltinA2mlSpec { .. }));
    }

    #[test]
    fn strict_parsing_version_error() {
        // version is missing completely
        let result = load_from_string(
            r#"/begin PROJECT x "" /begin MODULE y "" /end MODULE /end PROJECT"#,
            None,
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
        let result = load_from_string(r#"ASAP2_VERSION 1 /begin PROJECT"#, None, true);
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
        let result = load_from_string(
            r#"ASAP2_VERSION 1 71 /begin PROJECT x "" /begin MODULE y "" /end MODULE /end PROJECT abcdef"#,
            None,
            false,
        );
        assert!(result.is_ok());
        let (_a2l, log_msgs) = result.unwrap();
        assert_eq!(log_msgs.len(), 1);

        // strict parsing on - error
        let result = load_from_string(
            r#"ASAP2_VERSION 1 71 /begin PROJECT x "" /begin MODULE y "" /end MODULE /end PROJECT abcdef"#,
            None,
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
        let path = dir.path().join("test.a2l");
        let path = path.to_str().unwrap();

        let mut a2l = new();
        a2l.asap2_version
            .as_mut()
            .unwrap()
            .get_layout_mut()
            .start_offset = 0;
        let result = a2l.write(path, Some("test case write_nonexistent_file()"));
        assert!(result.is_ok());
        let file_text = String::from_utf8(std::fs::read(path).unwrap()).unwrap();
        assert!(file_text.starts_with("/* test case write_nonexistent_file() */"));
        std::fs::remove_file(path).unwrap();

        a2l.asap2_version
            .as_mut()
            .unwrap()
            .get_layout_mut()
            .start_offset = 1;
        let result = a2l.write(path, Some("test case write_nonexistent_file()"));
        assert!(result.is_ok());
        let file_text = String::from_utf8(std::fs::read(path).unwrap()).unwrap();
        assert!(file_text.starts_with("/* test case write_nonexistent_file() */"));
        std::fs::remove_file(path).unwrap();
    }

    #[cfg(feature = "merge")]
    #[test]
    fn merge() {
        // version is copied if none exists
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

        // merge modules
        let mut a2l = new();
        let mut a2l_2 = new();
        // create an item in the module of a2l_2
        a2l_2.project.module[0].compu_tab.push(CompuTab::new(
            "compu_tab".to_string(),
            String::new(),
            ConversionType::Identical,
            0,
        ));
        a2l.project.module[0].merge(&mut a2l_2.project.module[0]);
        // verify that the item was merged into the module of a2l
        assert_eq!(a2l.project.module[0].compu_tab.len(), 1);
    }

    #[test]
    fn test_load_fagment() {
        // an empty string is a valid fragment
        let result = load_fragment("", None);
        assert!(result.is_ok());

        // load a fragment with some data
        let result = load_fragment(
            r#"
            /begin MEASUREMENT measurement_name ""
                UBYTE CM.IDENTICAL 0 0 0 255
                ECU_ADDRESS 0x13A00
                FORMAT "%5.0"    /* Note: Overwrites the format stated in the computation method */
                DISPLAY_IDENTIFIER DI.ASAM.M.SCALAR.UBYTE.IDENTICAL    /* optional display identifier */
                /begin IF_DATA ETK  KP_BLOB 0x13A00 INTERN 1 RASTER 2 /end IF_DATA
            /end MEASUREMENT"#,
            None,
        );
        assert!(result.is_ok());

        // load a fragment with some data and a specification
        let result = load_fragment(
            r#"
            /begin MEASUREMENT measurement_name ""
                UBYTE CM.IDENTICAL 0 0 0 255
                ECU_ADDRESS 0x13A00
                /begin IF_DATA ETK  KP_BLOB 0x13A00 INTERN 1 RASTER 2 /end IF_DATA
            /end MEASUREMENT"#,
            Some(r#"block "IF_DATA" long;"#.to_string()),
        );
        assert!(result.is_ok());

        // load a fragment with some data and an invalid specification
        let result = load_fragment(
            r#"
            /begin MEASUREMENT measurement_name ""
                UBYTE CM.IDENTICAL 0 0 0 255
                ECU_ADDRESS 0x13A00
                /begin IF_DATA ETK  KP_BLOB 0x13A00 INTERN 1 RASTER 2 /end IF_DATA
            /end MEASUREMENT"#,
            Some(r#"lorem ipsum"#.to_string()),
        );
        assert!(matches!(
            result,
            Err(A2lError::InvalidBuiltinA2mlSpec { .. })
        ));

        // random data is not a valid fragment
        let result = load_fragment("12345", None);
        assert!(matches!(result, Err(A2lError::ParserError { .. })));

        let result = load_fragment(",,,", None);
        println!("{:?}", result);
        assert!(matches!(result, Err(A2lError::TokenizerError { .. })));
    }

    #[test]
    fn test_load_fagment_file() {
        let dir = tempdir().unwrap();
        let path = dir.path().join("fragment.a2l");
        let path = path.to_str().unwrap();

        // load a fragment with some data
        std::fs::write(
            path,
            r#"
            /begin MEASUREMENT measurement_name ""
                UBYTE CM.IDENTICAL 0 0 0 255
            /end MEASUREMENT"#,
        )
        .unwrap();
        let result = load_fragment_file(path, None);
        assert!(result.is_ok());

        // try to load a nonexistent file
        let nonexistent_path = dir.path().join("nonexistent.a2l");
        let nonexistent_path = nonexistent_path.to_str().unwrap();
        let result = load_fragment_file(nonexistent_path, None);
        assert!(matches!(result, Err(A2lError::FileOpenError { .. })));
    }

    #[test]
    fn test_filename() {
        let filename = Filename::from("test.a2l");
        assert_eq!(filename.to_string(), "test.a2l");

        let filename = Filename::from(OsString::from("test.a2l"));
        assert_eq!(filename.to_string(), "test.a2l");

        let filename = Filename::from(Path::new("test.a2l"));
        assert_eq!(filename.to_string(), "test.a2l");
    }
}
