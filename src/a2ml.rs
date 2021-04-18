use std::collections::HashMap;
use super::a2lwriter;
use super::a2lwriter::A2lWriter;


// tokenizer types
#[derive(Debug, PartialEq)]
pub(crate) enum A2MlTokenType<'a> {
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
    Identifier(&'a str),
    Tag(&'a str)
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
    String,
    Array(Box<A2mlTypeSpec>, usize),
    Enum(HashMap<String, Option<i32>>),
    Struct(Vec<A2mlTypeSpec>),
    Sequence(Box<A2mlTypeSpec>),
    TaggedStruct(HashMap<String, A2mlTaggedTypeSpec>),
    TaggedUnion(HashMap<String, A2mlTaggedTypeSpec>),
}

struct TypeSet {
    enums: HashMap::<String, A2mlTypeSpec>,
    structs: HashMap::<String, A2mlTypeSpec>,
    taggedstructs: HashMap::<String, A2mlTypeSpec>,
    taggedunions: HashMap::<String, A2mlTypeSpec>
}


type AmlTokenIter<'a> = std::iter::Peekable<std::slice::Iter<'a, A2MlTokenType<'a>>>;


// parser output types (generic IF_DATA)
#[derive(Debug, PartialEq)]
pub struct GenericIfDataTaggedItem {
    pub incfile: Option<String>,
    pub line: u32,
    pub tag: String,
    pub data: GenericIfData,
    pub is_block: bool
}

#[derive(Debug, PartialEq)]
pub enum GenericIfData {
    None,
    Char(u32, i8),
    Int(u32, i16),
    Long(u32, i32),
    Int64(u32, i64),
    UChar(u32, u8),
    UInt(u32, u16),
    ULong(u32, u32),
    UInt64(u32, u64),
    Float(u32, f32),
    Double(u32, f64),
    String(u32, String),
    Array(u32, Vec<GenericIfData>),
    EnumItem(u32, String),
    Struct(Option<String>, u32, Vec<GenericIfData>),
    Sequence(Vec<GenericIfData>),
    TaggedStruct(HashMap<String, Vec<GenericIfDataTaggedItem>>),
    TaggedUnion(HashMap<String, Vec<GenericIfDataTaggedItem>>),
    Block(Option<String>, u32, Vec<GenericIfData>)
}




// tokenize()
// Tokenize the text of the a2ml section
fn tokenize_a2ml(input: &str) -> Result<Vec<A2MlTokenType>, String> {
    let mut amltokens = Vec::<A2MlTokenType>::new();
    let mut remaining = input;

    while remaining.len() > 0 {
        let mut chars = remaining.char_indices();
        let (mut idx, mut c) = chars.next().unwrap();

        if c.is_ascii_whitespace() {
            /* skip whitepace */
            while c.is_ascii_whitespace() {
                let pair = chars.next().unwrap_or_else(|| (idx+1, '\0'));
                idx = pair.0;
                c = pair.1;
            }
            remaining = &remaining[idx..];
            continue;

        } else if remaining.starts_with("/*") {
            /* get a block comment */
            chars.next(); /* skip over the '*' char of the opening sequence */
            let mut done = false;
            let mut star = false;
            while !done {
                let pair = chars.next().unwrap_or_else(|| (idx+1, '\0'));
                idx = pair.0;
                c = pair.1;
                if c == '*' {
                    star = true;
                } else if (c == '/' && star == true) || c == '\0' {
                    done = true;
                } else {
                    star = false;
                }
            }
            remaining = &remaining[idx+1..];

        } else if remaining.starts_with("//") {
            /* get a line comment */
            loop {
                let pair = chars.next().unwrap_or_else(|| (idx+1, '\0'));
                idx = pair.0;
                c = pair.1;
                if c == '\n' || c == '\0' {
                    break;
                }
            }
            remaining = &remaining[idx+1..];
            
        } else if c == '"' {
            /* tag - it is enclosed in double quotes, but contains neither spaces nor escape characters */
            loop {
                let pair = chars.next().unwrap_or_else(|| (idx+1, '\0'));
                idx = pair.0;
                c = pair.1;
                if c == '"' || c == '\0' {
                    break;
                }
            }
            if c == '"' {
                let tag = &remaining[1..idx];
                amltokens.push(A2MlTokenType::Tag(tag));
                remaining = &remaining[idx+1..];
            } else {
                let displaylen = if remaining.len() > 16 { 16 } else { remaining.len() };
                return Err(format!("unclosed tag string starting with {}", &remaining[..displaylen]))
            }

        } else if c == ';' {
            amltokens.push(A2MlTokenType::Semicolon);
            remaining = &remaining[1..];
        } else if c == ',' {
            amltokens.push(A2MlTokenType::Comma);
            remaining = &remaining[1..];
        } else if c == '{' {
            amltokens.push(A2MlTokenType::OpenCurlyBracket);
            remaining = &remaining[1..];
        } else if c == '}' {
            amltokens.push(A2MlTokenType::ClosedCurlyBracket);
            remaining = &remaining[1..];
        } else if c == '[' {
            amltokens.push(A2MlTokenType::OpenSquareBracket);
            remaining = &remaining[1..];
        } else if c == ']' {
            amltokens.push(A2MlTokenType::ClosedSquareBracket);
            remaining = &remaining[1..];
        } else if c == '(' {
            amltokens.push(A2MlTokenType::OpenRoundBracket);
            remaining = &remaining[1..];
        } else if c == ')' {
            amltokens.push(A2MlTokenType::ClosedRoundBracket);
            remaining = &remaining[1..];
        } else if c == '*' {
            amltokens.push(A2MlTokenType::Repeat);
            remaining = &remaining[1..];
        } else if c == '=' {
            amltokens.push(A2MlTokenType::Equals);
            remaining = &remaining[1..];
        } else if c.is_ascii_digit() {
            loop {
                let pair = chars.next().unwrap_or_else(|| (idx+1, '\0'));
                idx = pair.0;
                c = pair.1;
                if !c.is_ascii_alphanumeric() && c != '_' {
                    break;
                }
            }
            let num_text = &remaining[0..idx];
            if let Ok(number) = num_text.parse::<i32>() {
                amltokens.push(A2MlTokenType::Constant(number));
            } else {
                return Err(format!("Invalid sequence in AML: {}", num_text));
            }
            remaining = &remaining[idx..];
        } else if c.is_ascii_alphabetic() || c == '_' {
            loop {
                let pair = chars.next().unwrap_or_else(|| (idx+1, '\0'));
                idx = pair.0;
                c = pair.1;
                if !c.is_ascii_alphanumeric() && c != '_' {
                    break;
                }
            }
            let kw_or_ident = &remaining[..idx];
            match kw_or_ident {
                "char" => { amltokens.push(A2MlTokenType::Char); }
                "int" => { amltokens.push(A2MlTokenType::Int); }
                "long" => { amltokens.push(A2MlTokenType::Long); }
                "int64" => { amltokens.push(A2MlTokenType::Int64); }
                "uint" => { amltokens.push(A2MlTokenType::Uint); }
                "uchar" => { amltokens.push(A2MlTokenType::Uchar); }
                "ulong" => { amltokens.push(A2MlTokenType::Ulong); }
                "iunt64" => { amltokens.push(A2MlTokenType::Uint64); }
                "double" => { amltokens.push(A2MlTokenType::Double); }
                "float" => { amltokens.push(A2MlTokenType::Float); }
                "block" => { amltokens.push(A2MlTokenType::Block); }
                "enum" => { amltokens.push(A2MlTokenType::Enum); }
                "struct" => { amltokens.push(A2MlTokenType::Struct); }
                "taggedstruct" => { amltokens.push(A2MlTokenType::Taggedstruct); }
                "taggedunion" => { amltokens.push(A2MlTokenType::Taggedunion); }
                _ => {amltokens.push(A2MlTokenType::Identifier(kw_or_ident));}
            }
            remaining = &remaining[idx..];
        } else {
            let displaylen = if remaining.len() > 16 { 16 } else { remaining.len() };
            return Err(format!("Unable to tokenize: {}...", &remaining[..displaylen]));
        }
    }

    Ok(amltokens)
}



// parse an a2ml fragment in an a2l file
// The target data structure is the parsing definition used by the a2l parser, so that the
// a2ml can control the parsing of IF_DATA blocks
pub(crate) fn parse_a2ml(input: &str) -> Result<A2mlTypeSpec, String> {
    let tok_result = tokenize_a2ml(input)?;
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
    while tok_iter.peek().is_some() {
        let tok = tok_iter.next().unwrap();
        match tok {
            A2MlTokenType::Block => {
                // the top level only _needs_ exactly one block.
                let tag = require_tag(&mut tok_iter)?;
                let blk = parse_aml_tagged_def(&mut tok_iter, &types)?;
                if tag == "IF_DATA" {
                    ifdata_block = Some(blk);
                }
            }

            A2MlTokenType::Taggedstruct => {
                let (optname, typ) = parse_aml_type(&mut tok_iter, &types, tok)?;
                if let Some(name) = optname {
                    types.taggedstructs.insert(name, typ);
                }
            }
            A2MlTokenType::Taggedunion => {
                let (optname, typ) = parse_aml_type(&mut tok_iter, &types, tok)?;
                if let Some(name) = optname {
                    types.taggedunions.insert(name, typ);
                }
            }
            A2MlTokenType::Enum => {
                let (optname, typ) = parse_aml_type(&mut tok_iter, &types, tok)?;
                if let Some(name) = optname {
                    types.enums.insert(name, typ);
                }
            }
            A2MlTokenType::Struct => {
                let (optname, typ) = parse_aml_type(&mut tok_iter, &types, tok)?;
                if let Some(name) = optname {
                    types.structs.insert(name, typ);
                }
            }

            // the grammar allows any type to be defined at the top level, even basic types.
            // however these do not have names, and even if they did, storing them would not help in any way
            A2MlTokenType::Char | 
            A2MlTokenType::Int | 
            A2MlTokenType::Long |
            A2MlTokenType::Int64 |
            A2MlTokenType::Uchar |
            A2MlTokenType::Uint |
            A2MlTokenType::Ulong |
            A2MlTokenType::Uint64 |
            A2MlTokenType::Double |
            A2MlTokenType::Float => {
                parse_aml_type(&mut tok_iter, &types, tok)?;
            }
            _ => {
                return Err(format!("found unexpected token {:?}", tok));
            }
        }
        require_token_type(&mut tok_iter, A2MlTokenType::Semicolon)?;
    }

    // The integration point between the custom blocks in Aml and the A2l file is the IF_DATA block.
    if let Some(ifdata_block) = ifdata_block {
        Ok(ifdata_block)
    } else {
        Err(format!("The A2ML declaration was fully parsed. However it does not contain an IF_DATA block, so it is not usable."))
    }
}


// parse_aml_type()
// Implements the grammar rules
//    type_name = predefined_type_name | struct_type_name | taggedstruct_type_name | taggedunion_type_name | enum_type_name
//    predefined_type_name = "char" | "int" | "long" | "uchar" | "uint" | "ulong" | "double" | "float"
fn parse_aml_type(tok_iter: &mut AmlTokenIter, types: &TypeSet, tok_start: &A2MlTokenType) -> Result<(Option<String>, A2mlTypeSpec), String> {
    match tok_start {
        A2MlTokenType::Char => Ok((None, A2mlTypeSpec::Char)),
        A2MlTokenType::Int => Ok((None, A2mlTypeSpec::Int)),
        A2MlTokenType::Long => Ok((None, A2mlTypeSpec::Long)),
        A2MlTokenType::Int64 => Ok((None, A2mlTypeSpec::Int64)),
        A2MlTokenType::Uchar => Ok((None, A2mlTypeSpec::UChar)),
        A2MlTokenType::Uint => Ok((None, A2mlTypeSpec::UInt)),
        A2MlTokenType::Ulong => Ok((None, A2mlTypeSpec::ULong)),
        A2MlTokenType::Uint64 => Ok((None, A2mlTypeSpec::UInt64)),
        A2MlTokenType::Float => Ok((None, A2mlTypeSpec::Float)),
        A2MlTokenType::Double => Ok((None, A2mlTypeSpec::Double)),
        A2MlTokenType::Enum => parse_aml_type_enum(tok_iter, &types),
        A2MlTokenType::Struct => parse_aml_type_struct(tok_iter, &types),
        A2MlTokenType::Taggedstruct => parse_aml_type_taggedstruct(tok_iter, &types),
        A2MlTokenType::Taggedunion => parse_aml_type_taggedunion(tok_iter, &types),
        _ => Err(format!("unexpected token {:?} in type declaration", tok_start)),
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
fn parse_aml_type_enum(tok_iter: &mut AmlTokenIter, types: &TypeSet) -> Result<(Option<String>, A2mlTypeSpec), String> {
    let name: Option<String> = parse_optional_name(tok_iter);

    // check if this is a reference to a previous declaration or if there is also a list of items in {}
    let tok_peek = tok_iter.peek();
    if tok_peek.is_none() || **tok_peek.unwrap() != A2MlTokenType::OpenCurlyBracket {
        if name.is_some() {
            let name = String::from(name.unwrap());
            if let Some(A2mlTypeSpec::Enum(items)) = types.enums.get(&name) {
                return Ok( (Some(name), A2mlTypeSpec::Enum(items.clone())) );
            } else {
                return Err(format!("enum {} was referenced but not defined", name));
            }
            
        } else {
            return Err(String::from("expected either an identifier or an opening bracket after keyword enum."));
        }
    }

    // parse the list of enum items
    require_token_type(tok_iter, A2MlTokenType::OpenCurlyBracket)?; // guaranteed to succeed
    let mut enumvalues = HashMap::new();
    loop {
        let tag = require_tag(tok_iter)?;
        let mut token = nexttoken(tok_iter)?;
        /* optionally each enum item may include a constant. */
        let mut con = None;
        if *token == A2MlTokenType::Equals {
            con = Some(require_constant(tok_iter)?);
            token = nexttoken(tok_iter)?;
        }
        match token {
            A2MlTokenType::Comma => {
                enumvalues.insert(String::from(tag), con);
            }
            A2MlTokenType::ClosedCurlyBracket => {
                enumvalues.insert(String::from(tag), con);
                break;
            }
            _ => { return Err(format!("unexpected token type {:?} in enum list", token)); }
        }
    }

    Ok((name, A2mlTypeSpec::Enum(enumvalues)))
}


// parse_aml_type_struct()
// Parses struct definitions according to the grammar: 
//    struct_type_name = "struct" [ identifier ] "{" [struct_member_list ] "}" | "struct" identifier
//    struct_member_list = struct_member | struct_member struct_member_list
//    struct_member = member ";" 
fn parse_aml_type_struct(tok_iter: &mut AmlTokenIter, types: &TypeSet) -> Result<(Option<String>, A2mlTypeSpec), String> {
    let name: Option<String> = parse_optional_name(tok_iter);

    // check if this is a reference to a previous declaration or if there is also a definition of the struct enclosed in {}
    let tok_peek = tok_iter.peek();
    if tok_peek.is_none() || **tok_peek.unwrap() != A2MlTokenType::OpenCurlyBracket {
        if name.is_some() {
            let name = String::from(name.unwrap());
            if let Some(A2mlTypeSpec::Struct(structitems)) = types.structs.get(&name) {
                return Ok( (Some(name), A2mlTypeSpec::Struct(structitems.clone())) );
            } else {
                return Err(format!("struct {} was referenced but not defined", name));
            }
        } else {
            return Err(String::from("expected either an identifier or an opening bracket after keyword struct."));
        }
    }

    // parse the struct elements
    require_token_type(tok_iter, A2MlTokenType::OpenCurlyBracket)?; // guaranteed to succeed
    let mut structdata = Vec::new();

    loop {
        structdata.push(parse_aml_member(tok_iter, &types)?);
        require_token_type(tok_iter, A2MlTokenType::Semicolon)?;

        if let Some(A2MlTokenType::ClosedCurlyBracket) = tok_iter.peek() {
            break;
        }
    }
    require_token_type(tok_iter, A2MlTokenType::ClosedCurlyBracket)?;

    Ok((name, A2mlTypeSpec::Struct(structdata)))
}


// parse_aml_type_taggedstruct()
// Parses taggedstructs according to the grammar:
//    taggedstruct_type_name = "taggedstruct" [ identifier ] "{" [taggedstruct_member_list ] "}" | "taggedstruct" identifier
//    taggedstruct_member_list = taggedstruct_member | taggedstruct_member taggedstruct_member_list
fn parse_aml_type_taggedstruct(tok_iter: &mut AmlTokenIter, types: &TypeSet) -> Result<(Option<String>, A2mlTypeSpec), String> {
    let name: Option<String> = parse_optional_name(tok_iter);

    // check if this is a reference to a previous declaration or if there is also a definition of the taggedstruct enclosed in {}
    let tok_peek = tok_iter.peek();
    if tok_peek.is_none() || **tok_peek.unwrap() != A2MlTokenType::OpenCurlyBracket {
        if name.is_some() {
            let name = String::from(name.unwrap());
            if let Some(A2mlTypeSpec::TaggedStruct(tsitems)) = types.taggedstructs.get(&name) {
                return Ok( (Some(name), A2mlTypeSpec::TaggedStruct(tsitems.clone())) );
            } else {
                return Err(format!("taggedstruct {} was referenced but not defined", name));
            }
        } else {
            return Err(String::from("expected either an identifier or an opening bracket after keyword taggedstruct."));
        }
    }

    // parse the taggedstruct elements
    require_token_type(tok_iter, A2MlTokenType::OpenCurlyBracket)?; // guaranteed to succeed
    let mut taggedstructdata = HashMap::new();
    loop {
        let (itemname, itemdef) = parse_aml_taggedmember(tok_iter, &types, true)?;
        taggedstructdata.insert(itemname, itemdef);
        require_token_type(tok_iter, A2MlTokenType::Semicolon)?;

        if let Some(A2MlTokenType::ClosedCurlyBracket) = tok_iter.peek() {
            break;
        }
    }
    require_token_type(tok_iter, A2MlTokenType::ClosedCurlyBracket)?;

    Ok((name, A2mlTypeSpec::TaggedStruct(taggedstructdata)))
}


// parse_aml_type_taggedunion()
//    taggedunion_type_name = "taggedunion" [ identifier ] "{" [taggedunion_member_list ] "}" | "taggedunion" identifier
//    taggedunion_member_list = tagged_union_member | tagged_union_member taggedunion_member_list
fn parse_aml_type_taggedunion(tok_iter: &mut AmlTokenIter, types: &TypeSet) -> Result<(Option<String>, A2mlTypeSpec), String> {
    let name: Option<String> = parse_optional_name(tok_iter);

    /* check if this is a reference to a previous declaration or if there is also a definition of the taggedunion enclosed in {} */
    let tok_peek = tok_iter.peek();
    if tok_peek.is_none() || **tok_peek.unwrap() != A2MlTokenType::OpenCurlyBracket {
        if name.is_some() {
            let name = String::from(name.unwrap());
            if let Some(A2mlTypeSpec::TaggedUnion(tsitems)) = types.taggedunions.get(&name) {
                return Ok( (Some(name), A2mlTypeSpec::TaggedUnion(tsitems.clone())) );
            } else {
                return Err(format!("taggedunion {} was referenced but not defined", name));
            }
        } else {
            return Err(String::from("A2ML error: expected either an identifier or an opening bracket after keyword taggedunion."));
        }
    }

    /* parse the taggedunion elements */
    require_token_type(tok_iter, A2MlTokenType::OpenCurlyBracket)?; // guaranteed to succeed
    let mut taggeduniondata = HashMap::new();
    loop {
        let (itemname, itemdef) = parse_aml_taggedmember(tok_iter, &types, false)?;
        taggeduniondata.insert(itemname, itemdef);
        require_token_type(tok_iter, A2MlTokenType::Semicolon)?;

        if let Some(A2MlTokenType::ClosedCurlyBracket) = tok_iter.peek() {
            break;
        }
    }
    require_token_type(tok_iter, A2MlTokenType::ClosedCurlyBracket)?;

    Ok((name, A2mlTypeSpec::TaggedUnion(taggeduniondata)))
}


// parse_aml_taggedstructmember()
// Parses taggedstruct members according to the grammar:
//    taggedstruct_member = taggedstruct_definition ";" | "(" taggedstruct_definition ")*;" | block_definition ";" | "(" block_definition ")*;"
fn parse_aml_taggedmember(tok_iter: &mut AmlTokenIter, types: &TypeSet, allow_repeat: bool) -> Result<(String, A2mlTaggedTypeSpec), String> {
    let mut tok = nexttoken(tok_iter)?;

    let mut repeat = false;
    if allow_repeat && *tok == A2MlTokenType::OpenRoundBracket {
        repeat = true;
        tok = nexttoken(tok_iter)?;
    }

    let mut is_block = false;
    if let A2MlTokenType::Block = tok {
        is_block = true;
        tok = nexttoken(tok_iter)?;
    }

    let taggedmember = 
        if let A2MlTokenType::Tag(tag) = tok {
            let tok_peek = tok_iter.peek();
            let item = if let Some(A2MlTokenType::Semicolon) = tok_peek {
                A2mlTypeSpec::None
            } else {
                parse_aml_tagged_def(tok_iter, &types)?
            };
            (tag.to_string(), A2mlTaggedTypeSpec { tag: tag.to_string(), item, is_block, repeat })
        } else {
            return Err(format!("invalid token type {:#?} while attempting to parse taggedstruct member", tok));
        };

    if repeat {
        require_token_type(tok_iter, A2MlTokenType::ClosedRoundBracket)?;
        require_token_type(tok_iter, A2MlTokenType::Repeat)?;
    }

    Ok(taggedmember)
}


// parse_aml_tagged_def()
// Parses taggedstruct definitions according to the grammar:
//    taggedstruct_definition = tag [ member ] | tag "(" member ")*;"
fn parse_aml_tagged_def(tok_iter: &mut AmlTokenIter, types: &TypeSet) -> Result<A2mlTypeSpec, String> {
    let mut inner_repeat = false;
    if let Some(A2MlTokenType::OpenRoundBracket) = tok_iter.peek() {
        inner_repeat = true;
        tok_iter.next();
    }

    let mut member = parse_aml_member(tok_iter, &types)?;

    if inner_repeat {
        require_token_type(tok_iter, A2MlTokenType::ClosedRoundBracket)?;
        require_token_type(tok_iter, A2MlTokenType::Repeat)?;
        member = A2mlTypeSpec::Sequence(Box::new(member));
    }

    Ok(member)
}


// parse_aml_member()
// Parse a member of some other data structure. Each member could potentially have an arbitrary number of array dimensions
//    member = type_name [ array_specifier ]
//    array_specifier = "[" constant "]" | "[" constant "]" array_specifier
fn parse_aml_member(tok_iter: &mut AmlTokenIter, types: &TypeSet) -> Result<A2mlTypeSpec, String> {
    let tok_start = nexttoken(tok_iter)?;
    let (_, mut base_type) = parse_aml_type(tok_iter, &types, tok_start)?;

    while let Some(A2MlTokenType::OpenSquareBracket) = tok_iter.peek() {
        /* get the array dim */
        require_token_type(tok_iter, A2MlTokenType::OpenSquareBracket)?;
        let dim = require_constant(tok_iter)?;
        require_token_type(tok_iter, A2MlTokenType::ClosedSquareBracket)?;

        /* special case: char[x] -> string */
        match base_type {
            A2mlTypeSpec::Char => {
                base_type = A2mlTypeSpec::String;
            }
            _ => {
                base_type = A2mlTypeSpec::Array(Box::new(base_type), dim as usize);
            }
        }
    }

    Ok(base_type)
}


// parse_optional_name()
// For enums, structs, taggedstructs and taggedunions the typename is optional.
// Called at the beginning of parsing one of these data strucutres, this function checks if the next token is a type name and returns it
fn parse_optional_name(tok_iter: &mut AmlTokenIter) -> Option<String> {
    if let Some(A2MlTokenType::Identifier(ident)) = tok_iter.peek() {
        tok_iter.next(); // consume the token. no need to do anything with it since we already have the content
        Some(String::from(*ident))
    } else {
        None
    }
}


// require_token_type()
// get the next token, which is required to be of the provided type
fn require_token_type(tok_iter: &mut AmlTokenIter, reference: A2MlTokenType) -> Result<(), String> {
    let token = nexttoken(tok_iter)?;
    if *token != reference {
        return Err(format!("A2ML Error: expected token of type {:?}, got {:?}", reference, token));
    }
    Ok(())
}


// require_tag()
// get the content of the next token, which is required to be a tag
fn require_tag<'a>(tok_iter: &mut AmlTokenIter<'a>) -> Result<&'a str, String> {
    match tok_iter.next() {
        Some(A2MlTokenType::Tag(tag)) => Ok(tag),
        Some(tok) => Err(format!("A2ML Error: incorrect token type {:?} where tag was expected", tok)),
        None => Err(String::from("A2ML Error: unexpected end of input where token type tag was expected"))
    }
}


// require_constant()
// get the content of the next token, which is required to be a constant
fn require_constant(tok_iter: &mut AmlTokenIter) -> Result<i32, String> {
    match tok_iter.next() {
        Some(A2MlTokenType::Constant(c)) => Ok(*c),
        Some(tok) => Err(format!("A2ML Error: incorrect token type {:?} where a constant was expected", tok)),
        None => Err(String::from("A2ML Error: unexpected end of input where token type constant was expected"))
    }
}


// nexttoken
// get the next token from the iterator and centralize the handling of potential None-values
fn nexttoken<'a>(tok_iter: &mut AmlTokenIter<'a>) -> Result<&'a A2MlTokenType<'a>, String> {
    match tok_iter.next() {
        Some(tok) => Ok(tok),
        None => Err(String::from("A2ML Error: unexpected end of input"))
    }
}


impl GenericIfData {
    pub fn get_block_items(&self) -> Result<(Option<String>, u32, &Vec<GenericIfData>), ()> {
        match self {
            GenericIfData::Block(file, line, blockitems) => Ok((file.clone(), *line, blockitems)),
            _ => Err(())
        }
    }

    pub fn get_struct_items(&self) -> Result<(Option<String>, u32, &Vec<GenericIfData>), ()> {
        match self {
            GenericIfData::Struct(file, line, blockitems) => Ok((file.clone(), *line, blockitems)),
            _ => Err(())
        }
    }

    pub fn get_integer_u8(&self) -> Result<u8, ()> {
        if let GenericIfData::UChar(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_integer_u16(&self) -> Result<u16, ()> {
        if let GenericIfData::UInt(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_integer_u32(&self) -> Result<u32, ()> {
        if let GenericIfData::ULong(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_integer_u64(&self) -> Result<u64, ()> {
        if let GenericIfData::UInt64(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_integer_i8(&self) -> Result<i8, ()> {
        if let GenericIfData::Char(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_integer_i16(&self) -> Result<i16, ()> {
        if let GenericIfData::Int(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_integer_i32(&self) -> Result<i32, ()> {
        if let GenericIfData::Long(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_integer_i64(&self) -> Result<i64, ()> {
        if let GenericIfData::Int64(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_float(&self) -> Result<f32, ()> {
        if let GenericIfData::Float(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_double(&self) -> Result<f64, ()> {
        if let GenericIfData::Double(_, val) = self {
            Ok(*val)
        } else {
            Err(())
        }
    }

    pub fn get_stringval(&self) -> Result<String, ()> {
        if let GenericIfData::String(_, val) = self {
            Ok(val.to_owned())
        } else {
            Err(())
        }
    }

    pub fn get_array(&self) -> Result<&Vec<GenericIfData>, ()> {
        if let GenericIfData::Array(_, arrayitems) = self {
            Ok(arrayitems)
        } else {
            Err(())
        }
    }

    pub fn get_sequence(&self) -> Result<&Vec<GenericIfData>, ()> {
        if let GenericIfData::Sequence(seqitems) = self {
            Ok(seqitems)
        } else {
            Err(())
        }
    }

    pub fn get_line(&self) -> Result<u32, ()> {
        match self {
            GenericIfData::Char(line, _) |
            GenericIfData::Int(line, _) |
            GenericIfData::Long(line, _) |
            GenericIfData::Int64(line, _) |
            GenericIfData::UChar(line, _) |
            GenericIfData::UInt(line, _) |
            GenericIfData::ULong(line, _) |
            GenericIfData::UInt64(line, _) |
            GenericIfData::Float(line, _) |
            GenericIfData::Double(line, _) |
            GenericIfData::String(line, _) |
            GenericIfData::Array(line, _) |
            GenericIfData::EnumItem(line, _) |
            GenericIfData::Struct(_, line, _) |
            GenericIfData::Block(_, line, _) => Ok(*line),
            _ => Err(())
        }
    }

    pub fn get_single_optitem<T>(&self, tag: &str, func: fn (&GenericIfData) -> Result<T, ()>) -> Result<Option<T>, ()> {
        match self {
            GenericIfData::TaggedStruct(taggeditems) |
            GenericIfData::TaggedUnion(taggeditems) => {
                if let Some(itemlist) = taggeditems.get(tag) {
                    Ok(Some(func(&itemlist[0].data)?))
                } else {
                    Ok(None)
                }
            }
            _ => {
                Err(())
            }
        }
    }

    pub fn get_multiple_optitems<T>(&self, tag: &str, func: fn (&GenericIfData) -> Result<T, ()>) -> Result<Vec<T>, ()> {
        match self {
            GenericIfData::TaggedStruct(taggeditems) |
            GenericIfData::TaggedUnion(taggeditems) => {
                let mut resultvec = Vec::new();
                if let Some(itemlist) = taggeditems.get(tag) {
                    for item in itemlist {
                        resultvec.push(func(&item.data)?);
                    }
                }
                Ok(resultvec)
            }
            _ => {
                Err(())
            }
        }
    }
}


impl GenericIfData {
    pub(crate) fn write(&self, file: &Option<String>, line: u32) -> A2lWriter {
        match self {
            GenericIfData::Struct(file, line, items) |
            GenericIfData::Block(file, line, items) => {
                let mut writer = A2lWriter::new(file, *line);
                for item in items {
                    item.write_item(&mut writer);
                }
                writer
            }
            _ => {
                let mut writer = A2lWriter::new(file, line);
                self.write_item(&mut writer);
                writer
            }
        }
    }

    fn write_item(&self, writer: &mut A2lWriter) {
        match self {
            Self::Char(line, value) => {
                writer.add_fixed_item(a2lwriter::format_i8(*value), *line);
            }
            Self::Int(line, value) => {
                writer.add_fixed_item(a2lwriter::format_i16(*value), *line);
            }
            Self::Long(line, value) => {
                writer.add_fixed_item(a2lwriter::format_i32(*value), *line);
            }
            Self::Int64(line, value) => {
                writer.add_fixed_item(a2lwriter::format_i64(*value), *line);
            }
            Self::UChar(line, value) => {
                writer.add_fixed_item(a2lwriter::format_u8(*value), *line);
            }
            Self::UInt(line, value) => {
                writer.add_fixed_item(a2lwriter::format_u16(*value), *line);
            }
            Self::ULong(line, value) => {
                writer.add_fixed_item(a2lwriter::format_u32(*value), *line);
            }
            Self::UInt64(line, value) => {
                writer.add_fixed_item(a2lwriter::format_u64(*value), *line);
            }
            Self::Float(line, value) => {
                writer.add_fixed_item(a2lwriter::format_float(*value), *line);
            }
            Self::Double(line, value) => {
                writer.add_fixed_item(a2lwriter::format_double(*value), *line);
            }
            Self::String(line, value) => {
                writer.add_fixed_item(format!("\"{}\"", a2lwriter::escape_string(value)), *line);
            }
            Self::EnumItem(line, enitem) => {
                writer.add_fixed_item(enitem.to_owned(), *line);
            }
            Self::Array(_, items) |
            Self::Sequence(items) |
            Self::Struct(_, _, items) => {
                for item in items {
                    item.write_item(writer);
                }
            }
            Self::TaggedStruct(taggeditems) |
            Self::TaggedUnion(taggeditems) => {
                let mut tgroup = writer.add_tagged_group();
                for (tag, tgitemlist) in taggeditems {
                    for tgitem in tgitemlist {
                        tgroup.add_tagged_item(tag, tgitem.data.write(&tgitem.incfile, tgitem.line), tgitem.is_block);
                    }
                }
            }
            _ => {
                /* no need to do anything for Self::None and Self::Block */
            }
        }
    }
}