mod loader;
mod tokenizer;
mod parser;
mod a2ml;
mod writer;
mod specification;
mod namemap;
mod merge;
mod checker;

// used internally
use tokenizer::{A2lToken, A2lTokenType};
use parser::{ParseContext, ParserState};

// re-export for the crate user
pub use a2lmacros::a2ml_specification;
pub use a2ml::GenericIfData;
pub use a2ml::GenericIfDataTaggedItem;
pub use specification::*;


pub trait Logger {
    fn log_message(&mut self, msg: String);
}



pub fn load(filename: &str, a2ml_spec: Option<String>, logger: &mut dyn Logger, strict_parsing: bool) -> Result<A2lFile, String> {
    let filedata = loader::load(filename)?;
    load_impl(filename, filedata, logger, strict_parsing, a2ml_spec)
}


pub fn load_from_string(a2ldata: &str, a2ml_spec: Option<String>, logger: &mut dyn Logger, strict_parsing: bool) -> Result<A2lFile, String> {
    load_impl("", a2ldata.to_string(), logger, strict_parsing, a2ml_spec)
}


fn load_impl(filename: &str, filedata: String, logger: &mut dyn Logger, strict_parsing: bool, a2ml_spec: Option<String>) -> Result<A2lFile, String> {
    // tokenize the input data
    let tokenresult = tokenizer::tokenize(String::from(filename), 0, &filedata)?;

    // create a context for the parser. Ensure that the current line of the context is set to the first line that actually contains a token
    let mut fake_token = A2lToken {ttype: A2lTokenType::Identifier, startpos: 0, endpos: 0, fileid: 0, line: 1};
    let firstline = tokenresult.tokens.get(0).unwrap_or_else(|| &fake_token).line;
    fake_token.line = firstline;
    let context = &ParseContext::from_token("", &fake_token, false);

    // create the parser state object
    let mut parser = ParserState::new(&tokenresult.tokens, &tokenresult.filedata, &tokenresult.filenames, logger, strict_parsing);

    // if a built-in A2ml specification was passed as a string, then it is parsed here
    if let Some(spec) = a2ml_spec {
        let ret = a2ml::parse_a2ml(&spec);
        if let Ok(parsed_spec) = ret {
            parser.builtin_a2mlspec = Some(parsed_spec);
        } else {
            // this shouldn't happen; if it does then there is a bug in the a2ml_specification! macro
            return Err(format!("Failed to load built-in specification: {}", ret.unwrap_err()));
        }
    }

    // try to get the file version. Starting with 1.60, the ASAP2_VERSION element is mandatory. For
    // compatibility with old files, a missing version is only an error if strict parsing is requested
    if let Err(version_error) = get_version(&mut parser, &context) {
        if !strict_parsing {
            parser.logger.log_message(version_error);
        } else {
            return Err(version_error)
        }
    }
    // build the a2l data structures from the tokens
    let a2lfile = A2lFile::parse(&mut parser, &context);
    if let Err(parse_error) = a2lfile {
        return Err(parser.stringify_parse_error(&parse_error, true));
    }
    let a2lfile = a2lfile.unwrap();
    Ok(a2lfile)
}


fn get_version(parser: &mut ParserState, context: &ParseContext) -> Result<Asap2Version, String> {
    if let Some(token) = parser.peek_token() {
        let ident = parser.get_identifier(context);
        let ver_context = ParseContext::from_token("", token, false);
        if let Ok(tag) = ident {
            if tag == "ASAP2_VERSION" {
                let version = Asap2Version::parse(parser, &ver_context);
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


pub fn write(a2lstruct: &A2lFile, filename: &str) -> Result<(), String> {
    let write_string = write_to_string(a2lstruct);

    if let Err(err) = std::fs::write(filename, write_string) {
        return Err(format!("Error while writing output {}: {}\n", filename, err.to_string()))
    }

    Ok(())
}


pub fn write_to_string(a2lstruct: &A2lFile) -> String {
    let file_text = a2lstruct.write().finish();
    // add a banner on the first line, but only if it is empty
    format!("/* written by a2ltool */\n{}", file_text)
}


pub fn merge_includes(a2lstruct: &mut A2lFile) {
    a2lstruct.merge_includes();
}


pub fn merge_modules(a2l_file: &mut A2lFile, merge_file: A2lFile) {
    merge::merge_modules(a2l_file, merge_file);
}


pub fn check(a2l_file: &A2lFile, logger: &mut dyn Logger) {
    checker::check(a2l_file, logger);
}
