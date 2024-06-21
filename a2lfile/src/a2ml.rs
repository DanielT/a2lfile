use super::writer::{TaggedItemInfo, Writer};
use super::{loader, tokenizer};
use std::borrow::Cow;
use std::collections::HashMap;
use std::path::Path;

// tokenizer types
#[derive(Debug, PartialEq)]
enum TokenType {
    Semicolon,
    Comma,
    OpenCurlyBracket,
    ClosedCurlyBracket,
    OpenSquareBracket,
    ClosedSquareBracket,
    OpenRoundBracket,
    ClosedRoundBracket,
    Repeat,
    Equals,
    Char,
    Int,
    Long,
    Int64,
    Uchar,
    Uint,
    Ulong,
    Uint64,
    Double,
    Float,
    Block,
    Enum,
    Struct,
    Taggedstruct,
    Taggedunion,
    Constant(i32),
    Identifier(String),
    Tag(String),
}

// parser representation types
#[derive(Debug, PartialEq, Clone)]
pub(crate) struct A2mlTaggedTypeSpec {
    pub(crate) tag: String,
    pub(crate) item: A2mlTypeSpec,
    pub(crate) is_block: bool,
    pub(crate) repeat: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) enum A2mlTypeSpec {
    None,
    Char,
    Int,
    Long,
    Int64,
    UChar,
    UInt,
    ULong,
    UInt64,
    Float,
    Double,
    Array(Box<A2mlTypeSpec>, usize),
    Enum(HashMap<String, Option<i32>>),
    Struct(Vec<A2mlTypeSpec>),
    Sequence(Box<A2mlTypeSpec>),
    TaggedStruct(HashMap<String, A2mlTaggedTypeSpec>),
    TaggedUnion(HashMap<String, A2mlTaggedTypeSpec>),
}

struct TypeSet {
    enums: HashMap<String, A2mlTypeSpec>,
    structs: HashMap<String, A2mlTypeSpec>,
    taggedstructs: HashMap<String, A2mlTypeSpec>,
    taggedunions: HashMap<String, A2mlTypeSpec>,
}

type A2mlTokenIter<'a> = std::iter::Peekable<std::slice::Iter<'a, TokenType>>;

// parser output types (generic IF_DATA)

/// A tagged item (of taggedstruct or taggedunion) in the generic `IfData` representation
#[derive(Debug, Clone)]
pub struct GenericIfDataTaggedItem {
    pub incfile: Option<String>,
    pub line: u32,
    pub uid: u32,
    pub start_offset: u32,
    pub end_offset: u32,
    pub tag: String,
    pub data: GenericIfData,
    pub is_block: bool,
}

/// generic representation of data inside an `IF_DATA` block that can be loaded into application-specific data structures
#[derive(Debug, Clone)]
pub enum GenericIfData {
    None,
    Char(u32, (i8, bool)),
    Int(u32, (i16, bool)),
    Long(u32, (i32, bool)),
    Int64(u32, (i64, bool)),
    UChar(u32, (u8, bool)),
    UInt(u32, (u16, bool)),
    ULong(u32, (u32, bool)),
    UInt64(u32, (u64, bool)),
    Float(u32, f32),
    Double(u32, f64),
    String(u32, String),
    Array(Vec<GenericIfData>),
    EnumItem(u32, String),
    Sequence(Vec<GenericIfData>),
    TaggedStruct(HashMap<String, Vec<GenericIfDataTaggedItem>>),
    TaggedUnion(HashMap<String, Vec<GenericIfDataTaggedItem>>),
    Struct(Option<String>, u32, Vec<GenericIfData>),
    Block {
        incfile: Option<String>,
        line: u32,
        items: Vec<GenericIfData>,
    },
}

// tokenize()
// Tokenize the text of the a2ml section
fn tokenize_a2ml(filename: &str, input: &str) -> Result<(Vec<TokenType>, String), String> {
    let mut amltokens = Vec::<TokenType>::new();
    let input_bytes = input.as_bytes();
    let datalen = input_bytes.len();
    let mut bytepos = 0;
    let mut complete_string = String::with_capacity(datalen);
    let mut copypos = 0;

    while bytepos < datalen {
        let startpos = bytepos;
        let c = input_bytes[bytepos];

        if input_bytes[bytepos].is_ascii_whitespace() {
            /* skip whitespace */
            while bytepos < datalen && input_bytes[bytepos].is_ascii_whitespace() {
                bytepos += 1;
            }
        } else if input_bytes[bytepos..].starts_with(b"/*") {
            /* skip a block comment */
            bytepos += 2; // just past the initial "/*"
            while bytepos < datalen && !input_bytes[bytepos..].starts_with(b"*/") {
                bytepos += 1;
            }

            if bytepos >= datalen {
                let errtxt = make_errtxt(startpos, input_bytes);
                return Err(format!("unclosed block quote starting with \"{errtxt}\""));
            }

            // chomp the closing "*/"
            bytepos += 2;
        } else if input_bytes[bytepos..].starts_with(b"//") {
            /* skip a line comment */
            while bytepos < datalen && input_bytes[bytepos] != b'\n' {
                bytepos += 1;
            }
            if bytepos < datalen {
                // skip the final '\n'
                bytepos += 1;
            }
        } else if input_bytes[bytepos..].starts_with(b"/include") {
            // copy any uncopied text before the include token
            complete_string.push_str(&input[copypos..startpos]);
            let (mut tokresult, incfile_text) = tokenize_include(filename, input, &mut bytepos)?;
            complete_string.push_str(&incfile_text);
            copypos = bytepos;

            // append the tokens from the included file(s)
            amltokens.append(&mut tokresult);
        } else if c == b'"' {
            let token = tokenize_tag(input, &mut bytepos)?;
            amltokens.push(token);
        } else if c == b';' {
            amltokens.push(TokenType::Semicolon);
            bytepos += 1;
        } else if c == b',' {
            amltokens.push(TokenType::Comma);
            bytepos += 1;
        } else if c == b'{' {
            amltokens.push(TokenType::OpenCurlyBracket);
            bytepos += 1;
        } else if c == b'}' {
            amltokens.push(TokenType::ClosedCurlyBracket);
            bytepos += 1;
        } else if c == b'[' {
            amltokens.push(TokenType::OpenSquareBracket);
            bytepos += 1;
        } else if c == b']' {
            amltokens.push(TokenType::ClosedSquareBracket);
            bytepos += 1;
        } else if c == b'(' {
            amltokens.push(TokenType::OpenRoundBracket);
            bytepos += 1;
        } else if c == b')' {
            amltokens.push(TokenType::ClosedRoundBracket);
            bytepos += 1;
        } else if c == b'*' {
            amltokens.push(TokenType::Repeat);
            bytepos += 1;
        } else if c == b'=' {
            amltokens.push(TokenType::Equals);
            bytepos += 1;
        } else if c.is_ascii_digit() {
            // tokenize a number, either decimal or hexadecimal
            let token = tokenize_number(input, &mut bytepos)?;
            amltokens.push(token);
        } else if c.is_ascii_alphabetic() || c == b'_' {
            // tokenize a keyword (int, long, etc.) or an identifier, both of which are non-quoted text
            let token = tokenize_keyword_ident(input, &mut bytepos);
            amltokens.push(token);
        } else {
            let errtxt = make_errtxt(startpos, input_bytes);
            return Err(format!("Unable to tokenize: {errtxt}..."));
        }
    }
    complete_string.push_str(&input[copypos..datalen]);

    Ok((amltokens, complete_string))
}

fn tokenize_tag(input: &str, bytepos: &mut usize) -> Result<TokenType, String> {
    let input_bytes = input.as_bytes();
    let datalen = input_bytes.len();
    let startpos = *bytepos;

    *bytepos += 1;
    let mut c = input_bytes[*bytepos];
    while *bytepos < datalen {
        c = input_bytes[*bytepos];
        if c == b'"' {
            break;
        }
        *bytepos += 1;
    }
    /* tag - it is enclosed in double quotes, but contains neither spaces nor escape characters */
    if c == b'"' {
        let tag = &input[(startpos + 1)..*bytepos];
        *bytepos += 1;
        Ok(TokenType::Tag(tag.to_string()))
    } else {
        let errtxt = make_errtxt(startpos, input_bytes);
        Err(format!("unclosed tag string starting with {errtxt}"))
    }
}

fn tokenize_include(
    filename: &str,
    input: &str,
    bytepos: &mut usize,
) -> Result<(Vec<TokenType>, String), String> {
    let input_bytes = input.as_bytes();
    let datalen = input_bytes.len();
    let startpos = *bytepos;

    *bytepos += 8;
    let mut state = 0;
    let mut fname_idx_start = 0;
    let fname_idx_end;
    loop {
        let c = if *bytepos < datalen {
            input_bytes[*bytepos]
        } else {
            b'\0'
        };

        if state == 0 && c.is_ascii_whitespace() {
            // just skip whitespaces
        } else if state == 0 && tokenizer::is_pathchar(c) {
            // start a non quoted filename
            state = 1;
            fname_idx_start = *bytepos;
        } else if state == 1 && tokenizer::is_pathchar(c) {
            // in non quoted filename
        } else if state == 1 && (c.is_ascii_whitespace() || c == b'\0') {
            // end of non quoted filename
            fname_idx_end = *bytepos;
            break;
        } else if state == 0 && c == b'"' {
            // start a quoted filename
            state = 2;
        } else if state == 2 && tokenizer::is_pathchar(c) {
            // first byte of a quoted filename
            state = 3;
            fname_idx_start = *bytepos;
        } else if state == 3 && tokenizer::is_pathchar(c) {
            // in a quoted filename
        } else if state == 3 && c == b'"' {
            // end of non quoted filename
            fname_idx_end = *bytepos;
            // chomp the '"'
            *bytepos += 1;
            break;
        } else {
            let errtxt = make_errtxt(startpos, input_bytes);
            return Err(format!("failed parsing a2ml include filename in {errtxt}"));
        }
        *bytepos += 1;
    }

    let incname = &input[fname_idx_start..fname_idx_end];
    let incfilename = loader::make_include_filename(incname, filename);

    // check if incname is an accessible file
    let incpathref = Path::new(&incfilename);
    let loadresult = loader::load(incpathref);
    if let Ok(incfiledata) = loadresult {
        tokenize_a2ml(incpathref.to_string_lossy().as_ref(), &incfiledata)
    } else {
        Err(format!("failed reading {}", incpathref.display()))
    }
}

fn tokenize_number(input: &str, bytepos: &mut usize) -> Result<TokenType, String> {
    let input_bytes = input.as_bytes();
    let datalen = input_bytes.len();
    let startpos = *bytepos;

    while *bytepos < datalen {
        let c = input_bytes[*bytepos];
        if !c.is_ascii_alphanumeric() && c != b'_' {
            break;
        }
        *bytepos += 1;
    }
    let num_text = &input[startpos..*bytepos];
    if let Some(hexval) = num_text.strip_prefix("0x") {
        // hex constant
        if let Ok(number) = i32::from_str_radix(hexval, 16) {
            Ok(TokenType::Constant(number))
        } else {
            Err(format!("Invalid sequence in AML: {num_text}"))
        }
    } else {
        // not hex format -> must be decimal
        if let Ok(number) = num_text.parse::<i32>() {
            Ok(TokenType::Constant(number))
        } else {
            Err(format!("Invalid sequence in AML: {num_text}"))
        }
    }
}

fn tokenize_keyword_ident(input: &str, bytepos: &mut usize) -> TokenType {
    let input_bytes = input.as_bytes();
    let datalen = input_bytes.len();
    let startpos = *bytepos;
    while *bytepos < datalen {
        let c = input_bytes[*bytepos];
        if !c.is_ascii_alphanumeric() && c != b'_' {
            break;
        }
        *bytepos += 1;
    }
    let kw_or_ident = &input[startpos..*bytepos];
    match kw_or_ident {
        "char" => TokenType::Char,
        "int" => TokenType::Int,
        "long" => TokenType::Long,
        "int64" => TokenType::Int64,
        "uint" => TokenType::Uint,
        "uchar" => TokenType::Uchar,
        "ulong" => TokenType::Ulong,
        "uint64" => TokenType::Uint64,
        "double" => TokenType::Double,
        "float" => TokenType::Float,
        "block" => TokenType::Block,
        "enum" => TokenType::Enum,
        "struct" => TokenType::Struct,
        "taggedstruct" => TokenType::Taggedstruct,
        "taggedunion" => TokenType::Taggedunion,
        _ => TokenType::Identifier(kw_or_ident.to_string()),
    }
}

fn make_errtxt(pos: usize, input_bytes: &[u8]) -> Cow<str> {
    let datalen = input_bytes.len();
    let endpos = if pos + 16 < datalen {
        pos + 16
    } else {
        datalen
    };
    // slicing remaining in arbitrary ways is not safe, the end might be in the middle of a utf-8 sequence, so from_utf8_lossy is needed
    String::from_utf8_lossy(&input_bytes[pos..endpos])
}

// parse an a2ml fragment in an a2l file
// The target data structure is the parsing definition used by the a2l parser, so that the
// a2ml can control the parsing of IF_DATA blocks
pub(crate) fn parse_a2ml(filename: &str, input: &str) -> Result<(A2mlTypeSpec, String), String> {
    let (tok_result, complete_string) = tokenize_a2ml(filename, input)?;
    let mut tok_iter = tok_result.iter().peekable();

    let mut ifdata_block: Option<A2mlTypeSpec> = None;
    // complex data types can be defined at the beginning and then referenced by name later on.
    let mut types = TypeSet {
        enums: HashMap::<String, A2mlTypeSpec>::new(),
        structs: HashMap::<String, A2mlTypeSpec>::new(),
        taggedstructs: HashMap::<String, A2mlTypeSpec>::new(),
        taggedunions: HashMap::<String, A2mlTypeSpec>::new(),
    };

    // at the top level the applicable grammar rule is
    //    declaration = type_definition ";" | block_definition ";"
    while let Some(tok) = tok_iter.next() {
        match tok {
            TokenType::Block => {
                // the top level only _needs_ exactly one block.
                let tag = require_tag(&mut tok_iter)?;
                let blk = parse_aml_tagged_def(&mut tok_iter, &types)?;
                if tag == "IF_DATA" {
                    ifdata_block = Some(blk);
                }
            }

            TokenType::Taggedstruct => {
                let (optname, typ) = parse_aml_type(&mut tok_iter, &types, tok)?;
                if let Some(name) = optname {
                    types.taggedstructs.insert(name, typ);
                }
            }
            TokenType::Taggedunion => {
                let (optname, typ) = parse_aml_type(&mut tok_iter, &types, tok)?;
                if let Some(name) = optname {
                    types.taggedunions.insert(name, typ);
                }
            }
            TokenType::Enum => {
                let (optname, typ) = parse_aml_type(&mut tok_iter, &types, tok)?;
                if let Some(name) = optname {
                    types.enums.insert(name, typ);
                }
            }
            TokenType::Struct => {
                let (optname, typ) = parse_aml_type(&mut tok_iter, &types, tok)?;
                if let Some(name) = optname {
                    types.structs.insert(name, typ);
                }
            }

            // the grammar allows any type to be defined at the top level, even basic types.
            // however these do not have names, and even if they did, storing them would not help in any way
            TokenType::Char
            | TokenType::Int
            | TokenType::Long
            | TokenType::Int64
            | TokenType::Uchar
            | TokenType::Uint
            | TokenType::Ulong
            | TokenType::Uint64
            | TokenType::Double
            | TokenType::Float => {
                parse_aml_type(&mut tok_iter, &types, tok)?;
            }
            _ => {
                return Err(format!("found unexpected token {tok:?}"));
            }
        }
        require_token_type(&mut tok_iter, &TokenType::Semicolon)?;
    }

    // The integration point between the custom blocks in Aml and the A2l file is the IF_DATA block.
    if let Some(ifdata_block) = ifdata_block {
        Ok((ifdata_block, complete_string))
    } else {
        Err("The A2ML declaration was fully parsed. However it does not contain an IF_DATA block, so it is not usable.".to_string())
    }
}

// parse_aml_type()
// Implements the grammar rules
//    type_name = predefined_type_name | struct_type_name | taggedstruct_type_name | taggedunion_type_name | enum_type_name
//    predefined_type_name = "char" | "int" | "long" | "uchar" | "uint" | "ulong" | "double" | "float"
fn parse_aml_type(
    tok_iter: &mut A2mlTokenIter,
    types: &TypeSet,
    tok_start: &TokenType,
) -> Result<(Option<String>, A2mlTypeSpec), String> {
    match tok_start {
        TokenType::Char => Ok((None, A2mlTypeSpec::Char)),
        TokenType::Int => Ok((None, A2mlTypeSpec::Int)),
        TokenType::Long => Ok((None, A2mlTypeSpec::Long)),
        TokenType::Int64 => Ok((None, A2mlTypeSpec::Int64)),
        TokenType::Uchar => Ok((None, A2mlTypeSpec::UChar)),
        TokenType::Uint => Ok((None, A2mlTypeSpec::UInt)),
        TokenType::Ulong => Ok((None, A2mlTypeSpec::ULong)),
        TokenType::Uint64 => Ok((None, A2mlTypeSpec::UInt64)),
        TokenType::Float => Ok((None, A2mlTypeSpec::Float)),
        TokenType::Double => Ok((None, A2mlTypeSpec::Double)),
        TokenType::Enum => parse_aml_type_enum(tok_iter, types),
        TokenType::Struct => parse_aml_type_struct(tok_iter, types),
        TokenType::Taggedstruct => parse_aml_type_taggedstruct(tok_iter, types),
        TokenType::Taggedunion => parse_aml_type_taggedunion(tok_iter, types),
        _ => Err(format!(
            "unexpected token {tok_start:?} in type declaration"
        )),
    }
}

// parse_aml_type_enum()
// Parses enum definitions according to the grammar:
//    enum_type_name = "enum" [ identifier ] "{" enumerator_list "}" | "enum" identifier
//    enumerator_list = enumerator | enumerator "," enumerator_list
//    enumerator = keyword [ "=" constant ]
//
// If the short form "enum identifier;" is found, then the type is looked up in the hashmap of previously defined enums
// If not, a new enum definition is expected
fn parse_aml_type_enum(
    tok_iter: &mut A2mlTokenIter,
    types: &TypeSet,
) -> Result<(Option<String>, A2mlTypeSpec), String> {
    let name: Option<String> = parse_optional_name(tok_iter);

    // check if this is a reference to a previous declaration or if there is also a list of items in {}
    match tok_iter.peek() {
        Some(TokenType::OpenCurlyBracket) => {
            // nothing to do here, a group definition follows
        }
        _ => {
            // no group with content follows, must be a reference to an existing type
            if let Some(name) = name {
                if let Some(A2mlTypeSpec::Enum(items)) = types.enums.get(&name) {
                    return Ok((Some(name), A2mlTypeSpec::Enum(items.clone())));
                } else {
                    return Err(format!("enum {name} was referenced but not defined"));
                }
            } else {
                return Err(String::from(
                    "expected either an identifier or an opening bracket after keyword enum.",
                ));
            }
        }
    }

    // parse the list of enum items
    require_token_type(tok_iter, &TokenType::OpenCurlyBracket)?; // guaranteed to succeed
    let mut enumvalues = HashMap::new();
    loop {
        let tag = require_tag(tok_iter)?;
        let mut token = nexttoken(tok_iter)?;
        /* optionally each enum item may include a constant. */
        let mut con = None;
        if *token == TokenType::Equals {
            con = Some(require_constant(tok_iter)?);
            token = nexttoken(tok_iter)?;
        }
        match token {
            TokenType::Comma => {
                enumvalues.insert(String::from(tag), con);
            }
            TokenType::ClosedCurlyBracket => {
                enumvalues.insert(String::from(tag), con);
                break;
            }
            _ => {
                return Err(format!("unexpected token type {token:?} in enum list"));
            }
        }
    }

    Ok((name, A2mlTypeSpec::Enum(enumvalues)))
}

// parse_aml_type_struct()
// Parses struct definitions according to the grammar:
//    struct_type_name = "struct" [ identifier ] "{" [struct_member_list ] "}" | "struct" identifier
//    struct_member_list = struct_member | struct_member struct_member_list
//    struct_member = member ";"
fn parse_aml_type_struct(
    tok_iter: &mut A2mlTokenIter,
    types: &TypeSet,
) -> Result<(Option<String>, A2mlTypeSpec), String> {
    let name: Option<String> = parse_optional_name(tok_iter);

    // check if this is a reference to a previous declaration or if there is also a definition of the struct enclosed in {}
    match tok_iter.peek() {
        Some(TokenType::OpenCurlyBracket) => {
            // nothing to do here, a group definition follows
        }
        _ => {
            // no group with content follows, must be a reference to an existing type
            if let Some(name) = name {
                if let Some(A2mlTypeSpec::Struct(structitems)) = types.structs.get(&name) {
                    return Ok((Some(name), A2mlTypeSpec::Struct(structitems.clone())));
                } else {
                    return Err(format!("struct {name} was referenced but not defined"));
                }
            } else {
                return Err(String::from(
                    "expected either an identifier or an opening bracket after keyword struct.",
                ));
            }
        }
    }

    // parse the struct elements
    require_token_type(tok_iter, &TokenType::OpenCurlyBracket)?; // guaranteed to succeed
    let mut structdata = Vec::new();

    loop {
        structdata.push(parse_aml_member(tok_iter, types)?);
        require_token_type(tok_iter, &TokenType::Semicolon)?;

        if let Some(TokenType::ClosedCurlyBracket) = tok_iter.peek() {
            break;
        }
    }
    require_token_type(tok_iter, &TokenType::ClosedCurlyBracket)?;

    Ok((name, A2mlTypeSpec::Struct(structdata)))
}

// parse_aml_type_taggedstruct()
// Parses taggedstructs according to the grammar:
//    taggedstruct_type_name = "taggedstruct" [ identifier ] "{" [taggedstruct_member_list ] "}" | "taggedstruct" identifier
//    taggedstruct_member_list = taggedstruct_member | taggedstruct_member taggedstruct_member_list
fn parse_aml_type_taggedstruct(
    tok_iter: &mut A2mlTokenIter,
    types: &TypeSet,
) -> Result<(Option<String>, A2mlTypeSpec), String> {
    let name: Option<String> = parse_optional_name(tok_iter);

    // check if this is a reference to a previous declaration or if there is also a definition of the taggedstruct enclosed in {}
    match tok_iter.peek() {
        Some(TokenType::OpenCurlyBracket) => {
            // nothing to do here, a group definition follows
        }
        _ => {
            // no group with content follows, must be a reference to an existing type
            if let Some(name) = name {
                if let Some(A2mlTypeSpec::TaggedStruct(tsitems)) = types.taggedstructs.get(&name) {
                    return Ok((Some(name), A2mlTypeSpec::TaggedStruct(tsitems.clone())));
                } else {
                    return Err(format!(
                        "taggedstruct {name} was referenced but not defined"
                    ));
                }
            } else {
                return Err(String::from("expected either an identifier or an opening bracket after keyword taggedstruct."));
            }
        }
    }

    // parse the taggedstruct elements
    require_token_type(tok_iter, &TokenType::OpenCurlyBracket)?; // guaranteed to succeed
    let mut taggedstructdata = HashMap::new();
    loop {
        let (itemname, itemdef) = parse_aml_taggedmember(tok_iter, types, true)?;
        taggedstructdata.insert(itemname, itemdef);
        require_token_type(tok_iter, &TokenType::Semicolon)?;

        if let Some(TokenType::ClosedCurlyBracket) = tok_iter.peek() {
            break;
        }
    }
    require_token_type(tok_iter, &TokenType::ClosedCurlyBracket)?;

    Ok((name, A2mlTypeSpec::TaggedStruct(taggedstructdata)))
}

// parse_aml_type_taggedunion()
//    taggedunion_type_name = "taggedunion" [ identifier ] "{" [taggedunion_member_list ] "}" | "taggedunion" identifier
//    taggedunion_member_list = tagged_union_member | tagged_union_member taggedunion_member_list
fn parse_aml_type_taggedunion(
    tok_iter: &mut A2mlTokenIter,
    types: &TypeSet,
) -> Result<(Option<String>, A2mlTypeSpec), String> {
    let name: Option<String> = parse_optional_name(tok_iter);

    // check if this is a reference to a previous declaration or if there is also a definition of the taggedunion enclosed in {}
    match tok_iter.peek() {
        Some(TokenType::OpenCurlyBracket) => {
            // nothing to do here, a group definition follows
        }
        _ => {
            // no group with content follows, must be a reference to an existing type
            if let Some(name) = name {
                if let Some(A2mlTypeSpec::TaggedUnion(tsitems)) = types.taggedunions.get(&name) {
                    return Ok((Some(name), A2mlTypeSpec::TaggedUnion(tsitems.clone())));
                } else {
                    return Err(format!("taggedunion {name} was referenced but not defined"));
                }
            } else {
                return Err(String::from("A2ML error: expected either an identifier or an opening bracket after keyword taggedunion."));
            }
        }
    }

    /* parse the taggedunion elements */
    require_token_type(tok_iter, &TokenType::OpenCurlyBracket)?; // guaranteed to succeed
    let mut taggeduniondata = HashMap::new();
    loop {
        let (itemname, itemdef) = parse_aml_taggedmember(tok_iter, types, false)?;
        taggeduniondata.insert(itemname, itemdef);
        require_token_type(tok_iter, &TokenType::Semicolon)?;

        if let Some(TokenType::ClosedCurlyBracket) = tok_iter.peek() {
            break;
        }
    }
    require_token_type(tok_iter, &TokenType::ClosedCurlyBracket)?;

    Ok((name, A2mlTypeSpec::TaggedUnion(taggeduniondata)))
}

// parse_aml_taggedstructmember()
// Parses taggedstruct members according to the grammar:
//    taggedstruct_member = taggedstruct_definition ";" | "(" taggedstruct_definition ")*;" | block_definition ";" | "(" block_definition ")*;"
fn parse_aml_taggedmember(
    tok_iter: &mut A2mlTokenIter,
    types: &TypeSet,
    allow_repeat: bool,
) -> Result<(String, A2mlTaggedTypeSpec), String> {
    let mut tok = nexttoken(tok_iter)?;

    let mut repeat = false;
    if allow_repeat && *tok == TokenType::OpenRoundBracket {
        repeat = true;
        tok = nexttoken(tok_iter)?;
    }

    let mut is_block = false;
    if let TokenType::Block = tok {
        is_block = true;
        tok = nexttoken(tok_iter)?;
    }

    let taggedmember = if let TokenType::Tag(tag) = tok {
        let tok_peek = tok_iter.peek();
        let item = if let Some(TokenType::Semicolon) = tok_peek {
            A2mlTypeSpec::None
        } else {
            parse_aml_tagged_def(tok_iter, types)?
        };
        (
            (*tag).to_string(),
            A2mlTaggedTypeSpec {
                tag: (*tag).to_string(),
                item,
                is_block,
                repeat,
            },
        )
    } else {
        return Err(format!(
            "invalid token type {tok:#?} while attempting to parse taggedstruct member"
        ));
    };

    if repeat {
        require_token_type(tok_iter, &TokenType::ClosedRoundBracket)?;
        require_token_type(tok_iter, &TokenType::Repeat)?;
    }

    Ok(taggedmember)
}

// parse_aml_tagged_def()
// Parses taggedstruct definitions according to the grammar:
//    taggedstruct_definition = tag [ member ] | tag "(" member ")*;"
fn parse_aml_tagged_def(
    tok_iter: &mut A2mlTokenIter,
    types: &TypeSet,
) -> Result<A2mlTypeSpec, String> {
    let mut inner_repeat = false;
    if let Some(TokenType::OpenRoundBracket) = tok_iter.peek() {
        inner_repeat = true;
        tok_iter.next();
    }

    let mut member = parse_aml_member(tok_iter, types)?;

    if inner_repeat {
        require_token_type(tok_iter, &TokenType::ClosedRoundBracket)?;
        require_token_type(tok_iter, &TokenType::Repeat)?;
        member = A2mlTypeSpec::Sequence(Box::new(member));
    }

    Ok(member)
}

// parse_aml_member()
// Parse a member of some other data structure. Each member could potentially have an arbitrary number of array dimensions
//    member = type_name [ array_specifier ]
//    array_specifier = "[" constant "]" | "[" constant "]" array_specifier
fn parse_aml_member(tok_iter: &mut A2mlTokenIter, types: &TypeSet) -> Result<A2mlTypeSpec, String> {
    let tok_start = nexttoken(tok_iter)?;
    let (_, mut base_type) = parse_aml_type(tok_iter, types, tok_start)?;

    while let Some(TokenType::OpenSquareBracket) = tok_iter.peek() {
        /* get the array dim */
        require_token_type(tok_iter, &TokenType::OpenSquareBracket)?;
        let dim = require_constant(tok_iter)?;
        require_token_type(tok_iter, &TokenType::ClosedSquareBracket)?;

        base_type = A2mlTypeSpec::Array(Box::new(base_type), dim as usize);
    }

    Ok(base_type)
}

// parse_optional_name()
// For enums, structs, taggedstructs and taggedunions the typename is optional.
// Called at the beginning of parsing one of these data strucutres, this function checks if the next token is a type name and returns it
fn parse_optional_name(tok_iter: &mut A2mlTokenIter) -> Option<String> {
    if let Some(TokenType::Identifier(ident)) = tok_iter.peek() {
        tok_iter.next(); // consume the token. no need to do anything with it since we already have the content
        Some(ident.to_string())
    } else {
        None
    }
}

// require_token_type()
// get the next token, which is required to be of the provided type
fn require_token_type(tok_iter: &mut A2mlTokenIter, reference: &TokenType) -> Result<(), String> {
    let token = nexttoken(tok_iter)?;
    if *token != *reference {
        return Err(format!(
            "A2ML Error: expected token of type {reference:?}, got {token:?}"
        ));
    }
    Ok(())
}

// require_tag()
// get the content of the next token, which is required to be a tag
fn require_tag<'a>(tok_iter: &mut A2mlTokenIter<'a>) -> Result<&'a str, String> {
    match tok_iter.next() {
        Some(TokenType::Tag(tag)) => Ok(tag),
        Some(tok) => Err(format!(
            "A2ML Error: incorrect token type {tok:?} where tag was expected"
        )),
        None => Err(String::from(
            "A2ML Error: unexpected end of input where token type tag was expected",
        )),
    }
}

// require_constant()
// get the content of the next token, which is required to be a constant
fn require_constant(tok_iter: &mut A2mlTokenIter) -> Result<i32, String> {
    match tok_iter.next() {
        Some(TokenType::Constant(c)) => Ok(*c),
        Some(tok) => Err(format!(
            "A2ML Error: incorrect token type {tok:?} where a constant was expected"
        )),
        None => Err(String::from(
            "A2ML Error: unexpected end of input where token type constant was expected",
        )),
    }
}

// nexttoken
// get the next token from the iterator and centralize the handling of potential None-values
fn nexttoken<'a>(tok_iter: &mut A2mlTokenIter<'a>) -> Result<&'a TokenType, String> {
    match tok_iter.next() {
        Some(tok) => Ok(tok),
        None => Err(String::from("A2ML Error: unexpected end of input")),
    }
}

impl GenericIfData {
    pub fn get_block_items(
        &self,
    ) -> Result<(Option<String>, u32, &Vec<GenericIfData>), &'static str> {
        match self {
            GenericIfData::Block {
                incfile,
                line,
                items,
            } => Ok((incfile.clone(), *line, items)),
            _ => {
                Err("structural mismatch: get_block_items called on something that is not a Block")
            }
        }
    }

    pub fn get_struct_items(
        &self,
    ) -> Result<(Option<String>, u32, &Vec<GenericIfData>), &'static str> {
        match self {
            GenericIfData::Struct(file, line, blockitems) => Ok((file.clone(), *line, blockitems)),
            _ => Err(
                "structural mismatch: get_struct_items called on something that is not a Struct",
            ),
        }
    }

    pub fn get_int_is_hex(&self) -> Result<bool, &'static str> {
        match self {
            GenericIfData::UChar(_, (_, is_hex))
            | GenericIfData::UInt(_, (_, is_hex))
            | GenericIfData::ULong(_, (_, is_hex))
            | GenericIfData::UInt64(_, (_, is_hex))
            | GenericIfData::Char(_, (_, is_hex))
            | GenericIfData::Int(_, (_, is_hex))
            | GenericIfData::Long(_, (_, is_hex))
            | GenericIfData::Int64(_, (_, is_hex)) => Ok(*is_hex),
            _ => Err(
                "structural mismatch: get_int_is_hex called on something that is not an integer",
            ),
        }
    }

    pub fn get_integer_u8(&self) -> Result<u8, &'static str> {
        if let GenericIfData::UChar(_, (val, _)) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_integer_u8 called on something that is not a UChar")
        }
    }

    pub fn get_integer_u16(&self) -> Result<u16, &'static str> {
        if let GenericIfData::UInt(_, (val, _)) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_integer_u16 called on something that is not a UInt")
        }
    }

    pub fn get_integer_u32(&self) -> Result<u32, &'static str> {
        if let GenericIfData::ULong(_, (val, _)) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_integer_u32 called on something that is not a ULong")
        }
    }

    pub fn get_integer_u64(&self) -> Result<u64, &'static str> {
        if let GenericIfData::UInt64(_, (val, _)) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_integer_u64 called on something that is not a UInt64")
        }
    }

    pub fn get_integer_i8(&self) -> Result<i8, &'static str> {
        if let GenericIfData::Char(_, (val, _)) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_integer_i8 called on something that is not a Char")
        }
    }

    pub fn get_integer_i16(&self) -> Result<i16, &'static str> {
        if let GenericIfData::Int(_, (val, _)) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_integer_i16 called on something that is not an Int")
        }
    }

    pub fn get_integer_i32(&self) -> Result<i32, &'static str> {
        if let GenericIfData::Long(_, (val, _)) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_integer_i32 called on something that is not a Long")
        }
    }

    pub fn get_integer_i64(&self) -> Result<i64, &'static str> {
        if let GenericIfData::Int64(_, (val, _)) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_integer_i64 called on something that is not an Int64")
        }
    }

    pub fn get_float(&self) -> Result<f32, &'static str> {
        if let GenericIfData::Float(_, val) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_float called on something that is not a Float")
        }
    }

    pub fn get_double(&self) -> Result<f64, &'static str> {
        if let GenericIfData::Double(_, val) = self {
            Ok(*val)
        } else {
            Err("structural mismatch: get_double called on something that is not a Double")
        }
    }

    pub fn get_stringval(&self) -> Result<String, &'static str> {
        if let GenericIfData::String(_, val) = self {
            Ok(val.to_owned())
        } else {
            Err("structural mismatch: get_stringval called on something that is not a String")
        }
    }

    pub fn get_array(&self) -> Result<&Vec<GenericIfData>, &'static str> {
        if let GenericIfData::Array(arrayitems) = self {
            Ok(arrayitems)
        } else {
            Err("structural mismatch: get_array called on something that is not an Array")
        }
    }

    pub fn get_sequence(&self) -> Result<&Vec<GenericIfData>, &'static str> {
        if let GenericIfData::Sequence(seqitems) = self {
            Ok(seqitems)
        } else {
            Err("structural mismatch: get_sequence called on something that is not a Sequence")
        }
    }

    pub fn get_line(&self) -> Result<u32, &'static str> {
        match self {
            GenericIfData::Char(line, _)
            | GenericIfData::Int(line, _)
            | GenericIfData::Long(line, _)
            | GenericIfData::Int64(line, _)
            | GenericIfData::UChar(line, _)
            | GenericIfData::UInt(line, _)
            | GenericIfData::ULong(line, _)
            | GenericIfData::UInt64(line, _)
            | GenericIfData::Float(line, _)
            | GenericIfData::Double(line, _)
            | GenericIfData::String(line, _)
            | GenericIfData::EnumItem(line, _)
            | GenericIfData::Struct(_, line, _)
            | GenericIfData::Block { line, .. } => Ok(*line),
            _ => Err("structural mismatch: get_line called on something that has no line info"),
        }
    }

    pub fn get_single_optitem<T>(
        &self,
        tag: &str,
        func: fn(&GenericIfData, u32, u32, u32) -> Result<T, &'static str>,
    ) -> Result<Option<T>, &'static str> {
        match self {
            GenericIfData::TaggedStruct(taggeditems) | GenericIfData::TaggedUnion(taggeditems) => {
                if let Some(itemlist) = taggeditems.get(tag) {
                    Ok(Some(func(
                        &itemlist[0].data,
                        itemlist[0].uid,
                        itemlist[0].start_offset,
                        itemlist[0].end_offset,
                    )?))
                } else {
                    Ok(None)
                }
            }
            _ => Err("structural mismatch: get_single_optitem called on unsuitable element"),
        }
    }

    pub fn get_multiple_optitems<T>(
        &self,
        tag: &str,
        func: fn(&GenericIfData, u32, u32, u32) -> Result<T, &'static str>,
    ) -> Result<Vec<T>, &'static str> {
        match self {
            GenericIfData::TaggedStruct(taggeditems) | GenericIfData::TaggedUnion(taggeditems) => {
                let mut resultvec = Vec::new();
                if let Some(itemlist) = taggeditems.get(tag) {
                    for item in itemlist {
                        resultvec.push(func(
                            &item.data,
                            item.uid,
                            item.start_offset,
                            item.end_offset,
                        )?);
                    }
                }
                Ok(resultvec)
            }
            _ => Err("structural mismatch: get_multiple_optitems called on unsuitable element"),
        }
    }
}

impl GenericIfData {
    pub(crate) fn write(&self, indent: usize) -> String {
        match self {
            GenericIfData::Struct(_, _, items) | GenericIfData::Block { items, .. } => {
                let mut writer = Writer::new(indent);
                for item in items {
                    item.write_item(&mut writer, indent);
                }
                writer.finish()
            }
            _ => {
                let mut writer = Writer::new(indent);
                self.write_item(&mut writer, indent);
                writer.finish()
            }
        }
    }

    fn write_item(&self, writer: &mut Writer, indent: usize) {
        match self {
            Self::Char(offset, value) => {
                writer.add_integer(value.0, value.1, *offset);
            }
            Self::Int(offset, value) => {
                writer.add_integer(value.0, value.1, *offset);
            }
            Self::Long(offset, value) => {
                writer.add_integer(value.0, value.1, *offset);
            }
            Self::Int64(offset, value) => {
                writer.add_integer(value.0, value.1, *offset);
            }
            Self::UChar(offset, value) => {
                writer.add_integer(value.0, value.1, *offset);
            }
            Self::UInt(offset, value) => {
                writer.add_integer(value.0, value.1, *offset);
            }
            Self::ULong(offset, value) => {
                writer.add_integer(value.0, value.1, *offset);
            }
            Self::UInt64(offset, value) => {
                writer.add_integer(value.0, value.1, *offset);
            }
            Self::Float(offset, value) => {
                writer.add_float(*value, *offset);
            }
            Self::Double(offset, value) => {
                writer.add_float(*value, *offset);
            }
            Self::String(offset, text) => {
                writer.add_quoted_string(text, *offset);
            }
            Self::EnumItem(offset, enitem) => {
                writer.add_str(enitem, *offset);
            }
            Self::Array(items) | Self::Sequence(items) | Self::Struct(_, _, items) => {
                for item in items {
                    item.write_item(writer, indent);
                }
            }
            Self::TaggedStruct(taggeditems) | Self::TaggedUnion(taggeditems) => {
                let mut tgroup = Vec::new();
                for tgitemlist in taggeditems.values() {
                    for tgitem in tgitemlist {
                        tgroup.push(TaggedItemInfo {
                            tag: &tgitem.tag,
                            incfile: &tgitem.incfile,
                            uid: tgitem.uid,
                            line: tgitem.line,
                            start_offset: tgitem.start_offset,
                            end_offset: tgitem.end_offset,
                            is_block: tgitem.is_block,
                            item_text: tgitem.data.write(indent + 1),
                            position_restriction: None,
                        });
                    }
                }
                writer.add_group(tgroup);
            }
            _ => { /* no need to do anything for Self::None and Self::Block */ }
        }
    }

    // merge_includes()
    // items that originate in an included file will have theit incfile member set to Some("filename")
    // this function strips that information, which will cause these included items to be written with all
    // the elements that were part of this file originally
    pub(crate) fn merge_includes(&mut self) {
        match self {
            Self::Block { incfile, items, .. } | Self::Struct(incfile, _, items) => {
                *incfile = None;
                for item in items {
                    item.merge_includes();
                }
            }
            Self::TaggedStruct(taggeditems) | Self::TaggedUnion(taggeditems) => {
                for tgitemlist in taggeditems.values_mut() {
                    for tgitem in tgitemlist {
                        tgitem.incfile = None;
                        tgitem.data.merge_includes();
                    }
                }
            }
            _ => {}
        }
    }
}

// an implementation of PartialEq that ignores the line numbers stored in the elements.
// Two elements are equal if their contained values match, regardless of the line numbers.
impl PartialEq for GenericIfData {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Self::None => {
                matches!(other, Self::None)
            }
            Self::Char(_, val) => {
                if let Self::Char(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::Int(_, val) => {
                if let Self::Int(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::Long(_, val) => {
                if let Self::Long(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::Int64(_, val) => {
                if let Self::Int64(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::UChar(_, val) => {
                if let Self::UChar(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::UInt(_, val) => {
                if let Self::UInt(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::ULong(_, val) => {
                if let Self::ULong(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::UInt64(_, val) => {
                if let Self::UInt64(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::Float(_, val) => {
                if let Self::Float(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::Double(_, val) => {
                if let Self::Double(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::String(_, val) => {
                if let Self::String(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::Array(arr) => {
                if let Self::Array(otherarr) = other {
                    arr == otherarr
                } else {
                    false
                }
            }
            Self::EnumItem(_, val) => {
                if let Self::EnumItem(_, otherval) = other {
                    val == otherval
                } else {
                    false
                }
            }
            Self::Struct(_, _, items) => {
                if let Self::Struct(_, _, otheritems) = other {
                    items == otheritems
                } else {
                    false
                }
            }
            Self::Sequence(seq) => {
                if let Self::Sequence(otherseq) = other {
                    seq == otherseq
                } else {
                    false
                }
            }
            Self::TaggedStruct(tgitems) => {
                if let Self::TaggedStruct(othertgi) = other {
                    tgitems == othertgi
                } else {
                    false
                }
            }
            Self::TaggedUnion(tgitems) => {
                if let Self::TaggedUnion(othertgi) = other {
                    tgitems == othertgi
                } else {
                    false
                }
            }
            Self::Block { items, .. } => {
                if let Self::Block {
                    items: otheritems, ..
                } = other
                {
                    items == otheritems
                } else {
                    false
                }
            }
        }
    }
}

impl PartialEq for GenericIfDataTaggedItem {
    fn eq(&self, other: &Self) -> bool {
        self.tag == other.tag && self.data == other.data && self.is_block == other.is_block
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn tokenize() {
        let (tokenvec, _) = tokenize_a2ml("test", "       ").unwrap();
        assert!(tokenvec.is_empty());

        let (tokenvec, _) = tokenize_a2ml("test", "/* // */").unwrap();
        assert!(tokenvec.is_empty());
        let (tokenvec, _) = tokenize_a2ml("test", "/*/*/").unwrap();
        assert!(tokenvec.is_empty());
        let (tokenvec, _) = tokenize_a2ml("test", "/***/").unwrap();
        assert!(tokenvec.is_empty());
        let tokenvec_err = tokenize_a2ml("test", "/* ");
        assert!(tokenvec_err.is_err());
        let (tokenvec, _) = tokenize_a2ml("test", "//*/").unwrap();
        assert!(tokenvec.is_empty());

        let (tokenvec, _) = tokenize_a2ml("test", r#""TAG""#).unwrap();
        assert_eq!(tokenvec.len(), 1);
        let _tag = TokenType::Tag("TAG".to_string());
        assert!(matches!(&tokenvec[0], _tag));

        let (tokenvec, _) = tokenize_a2ml("test", ";").unwrap();
        assert_eq!(tokenvec.len(), 1);
        assert!(matches!(tokenvec[0], TokenType::Semicolon));

        let (tokenvec, _) = tokenize_a2ml("test", "0").unwrap();
        assert_eq!(tokenvec.len(), 1);
        assert!(matches!(tokenvec[0], TokenType::Constant(0)));

        let (tokenvec, _) = tokenize_a2ml("test", "0x03").unwrap();
        assert_eq!(tokenvec.len(), 1);
        assert!(matches!(tokenvec[0], TokenType::Constant(3)));

        let (tokenvec, _) = tokenize_a2ml("test", "123456").unwrap();
        assert_eq!(tokenvec.len(), 1);
        assert!(matches!(tokenvec[0], TokenType::Constant(123456)));

        // set the current working directory to a temp dir
        let dir = tempdir().unwrap();
        std::env::set_current_dir(&dir.path()).unwrap();

        // create the empty "testfile" so that it can be included
        std::fs::File::create_new("testfile").unwrap();

        let (tokenvec, _) = tokenize_a2ml("test", r#"/include "testfile""#).unwrap();
        assert_eq!(tokenvec.len(), 0);

        let (tokenvec, _) = tokenize_a2ml("test", r#"/include"testfile""#).unwrap();
        assert_eq!(tokenvec.len(), 0);

        let (tokenvec, _) = tokenize_a2ml("test", r#"/include testfile"#).unwrap();
        assert_eq!(tokenvec.len(), 0);

        let err_result = tokenize_a2ml("test", r#"/include "testfile_unclosed_quote"#);
        assert!(err_result.is_err());

        let err_result = tokenize_a2ml("test", r#" "unclosed "#);
        assert!(err_result.is_err());
    }

    #[test]
    fn parse() {
        static TEST_INPUT: &str = r#"
        /* comment */
        struct SomeStruct {
            uchar; // comment
            uint;
            ulong;
            char;
            int;
            long;
            float;
            double;
            uint[3];
            enum {
                "ANON_ENUM_A" = 0,
                "ANON_ENUM_B" = 1
            };
            enum xyz {
                "SOME_ENUM_A" = 1,
                "SOME_ENUM_B" = 0x2,
                "SOME_EMUM_C" = 3
            };
            taggedunion {
                "FOO" uint;
                "BAR" uchar;
            };
            taggedstruct {
                ("REP_ITEM" uint)*;
                "NORMAL_ITEM" struct {
                    char[128];
                };
                "REP_ITEM_INNER" (struct InnerRepStruct {
                    uint;
                })*;
            };
        };

        block "IF_DATA" struct SomeStruct;
"#;
        let mut enum_hashmap_1 = HashMap::new();
        enum_hashmap_1.insert("ANON_ENUM_B".to_string(), Some(1));
        enum_hashmap_1.insert("ANON_ENUM_A".to_string(), Some(0));
        let mut enum_hashmap_2 = HashMap::new();
        enum_hashmap_2.insert("SOME_ENUM_A".to_string(), Some(1));
        enum_hashmap_2.insert("SOME_ENUM_B".to_string(), Some(2));
        enum_hashmap_2.insert("SOME_EMUM_C".to_string(), Some(3));
        let mut taggedunion_hashmap = HashMap::new();
        taggedunion_hashmap.insert(
            "FOO".to_string(),
            A2mlTaggedTypeSpec {
                tag: "FOO".to_string(),
                item: A2mlTypeSpec::UInt,
                is_block: false,
                repeat: false,
            },
        );
        taggedunion_hashmap.insert(
            "BAR".to_string(),
            A2mlTaggedTypeSpec {
                tag: "BAR".to_string(),
                item: A2mlTypeSpec::UChar,
                is_block: false,
                repeat: false,
            },
        );
        let mut taggedstruct_hashmap = HashMap::new();
        taggedstruct_hashmap.insert(
            "NORMAL_ITEM".to_string(),
            A2mlTaggedTypeSpec {
                tag: "NORMAL_ITEM".to_string(),
                item: A2mlTypeSpec::Struct(vec![A2mlTypeSpec::Array(
                    Box::new(A2mlTypeSpec::Char),
                    128,
                )]),
                is_block: false,
                repeat: false,
            },
        );
        taggedstruct_hashmap.insert(
            "REP_ITEM_INNER".to_string(),
            A2mlTaggedTypeSpec {
                tag: "REP_ITEM_INNER".to_string(),
                item: A2mlTypeSpec::Sequence(Box::new(A2mlTypeSpec::Struct(vec![
                    A2mlTypeSpec::UInt,
                ]))),
                is_block: false,
                repeat: false,
            },
        );
        taggedstruct_hashmap.insert(
            "REP_ITEM".to_string(),
            A2mlTaggedTypeSpec {
                tag: "REP_ITEM".to_string(),
                item: A2mlTypeSpec::UInt,
                is_block: false,
                repeat: true,
            },
        );
        let expected_parse_result = A2mlTypeSpec::Struct(vec![
            A2mlTypeSpec::UChar,
            A2mlTypeSpec::UInt,
            A2mlTypeSpec::ULong,
            A2mlTypeSpec::Char,
            A2mlTypeSpec::Int,
            A2mlTypeSpec::Long,
            A2mlTypeSpec::Float,
            A2mlTypeSpec::Double,
            A2mlTypeSpec::Array(Box::new(A2mlTypeSpec::UInt), 3),
            A2mlTypeSpec::Enum(enum_hashmap_1),
            A2mlTypeSpec::Enum(enum_hashmap_2),
            A2mlTypeSpec::TaggedUnion(taggedunion_hashmap),
            A2mlTypeSpec::TaggedStruct(taggedstruct_hashmap),
        ]);

        let parse_result = parse_a2ml("test", TEST_INPUT);
        assert!(parse_result.is_ok());
        let (a2ml_spec, _complete_string) = parse_result.unwrap();
        println!("{:?}", a2ml_spec);
        assert_eq!(a2ml_spec, expected_parse_result);
    }
}
