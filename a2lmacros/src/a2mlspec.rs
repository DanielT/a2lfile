use std::{collections::{HashMap, HashSet}, fmt::Write};

use proc_macro::TokenStream;
use proc_macro::TokenTree;
use proc_macro::Delimiter;
use quote::{format_ident, quote};
// use quote::format_ident;
use super::util::*;


#[derive(Debug)]
struct A2mlSpec {
    name: String,
    types: A2mlTypeList,
}



#[derive(Debug, PartialEq, Clone)]
struct EnumItem {
    item_name: String,
    value: Option<i32>,
    comment: Option<String>
}

#[derive(Debug)]
struct DataItem {
    typename: Option<String>,
    item_type: A2mlType,
    varname: Option<String>,
    comment: Option<String>
}


#[derive(Debug, PartialEq)]
struct TaggedItem {
    tag: String,
    item: DataItem,
    repeat: bool
}


#[derive(Debug, PartialEq)]
enum A2mlType {
    None,
    Char,
    Int,
    Long,
    Uchar,
    Uint,
    Ulong,
    Double,
    Float,
    Array(Box<A2mlType>, usize),
    Sequence(Box<A2mlType>),
    Enum(Vec<EnumItem>),
    EnumRef(String),
    Struct(Vec<DataItem>),
    StructRef(String),
    TaggedUnion(Vec<TaggedItem>),
    TaggedUnionRef(String),
    TaggedStruct(Vec<TaggedItem>),
    TaggedStructRef(String),
    Block(String, Box<DataItem>)
}


#[derive(Debug)]
struct A2mlTypeList {
    list: Vec<(String, A2mlType)>
}


pub(crate) fn a2ml_specification(tokens: TokenStream) -> TokenStream {
    let mut iter: TokenStreamIter = tokens.into_iter().peekable();
    let spec = parse_specification(&mut iter);

    let mut result = quote!{};
    result.extend(generate_a2ml_constant(&spec));
    let outtypes = fixup_output_datatypes(&spec);

    result.extend(generate_data_structures(&outtypes));

    result.extend(generate_parser(&outtypes));

    result.into()
}

/*
Grammar for A2ML:

    declaration = type_definition ";" | block_definition ";"

    block_definition = "block" tag type_name | "block" tag "(" type_name ")*"
    type_definition = type_name
    type_name = predefined_type_name | struct_type_name | taggedstruct_type_name | taggedunion_type_name | enum_type_name
    predefined_type_name = "char" | "int" | "long" | "uchar" | "uint" | "ulong" | "double" | "float"

    enum_type_name = "enum" [ identifier ] "{" enumerator_list "}" | "enum" identifier
    enumerator_list = enumerator | enumerator "," enumerator_list
    enumerator = keyword [ "=" constant ] 

    member = type_name [ array_specifier ] 
    array_specifier = "[" constant "]" | "[" constant "]" array_specifier

    struct_type_name = "struct" [ identifier ] "{" [struct_member_list ] "}" | "struct" identifier
    struct_member_list = struct_member | struct_member struct_member_list
    struct_member = member ";" 

    taggedstruct_type_name = "taggedstruct" [ identifier ] "{" [taggedstruct_member_list ] "}" | "taggedstruct" identifier
    taggedstruct_member_list = taggedstruct_member | taggedstruct_member taggedstruct_member_list
    taggedstruct_member = taggedstruct_definition ";" | "(" taggedstruct_definition ")*;" | block_definition ";" | "(" block_definition ")*;"
    taggedstruct_definition = tag [ member ] | tag "(" member ")*;"

    taggedunion_type_name = "taggedunion" [ identifier ] "{" [taggedunion_member_list ] "}" | "taggedunion" identifier
    taggedunion_member_list = tagged_union_member | tagged_union_member taggedunion_member_list
    tagged_union_member = tag [ member ] ";" | block_definition ";"

    constant = for enums this is defined as a signed 32 bit int
*/

fn parse_specification(token_iter: &mut TokenStreamIter) -> A2mlSpec {
    require_punct(token_iter, '<');
    let name = get_ident(token_iter);
    require_punct(token_iter, '>');

    let mut types = A2mlTypeList {
        list: Vec::new()
    };

    while token_iter.peek().is_some() {
        let ident = get_ident(token_iter);
        // no need to keep types without names, we wouldn't be able to do anything with those anyway
        if let (Some(typename), itemtype) = parse_a2ml_type(token_iter, &types, &ident) {
            types.list.push((typename, itemtype));
        }

        require_punct(token_iter, ';');
        // don't error out if a comment appears even though there is no data structure to attach it to in this context
        parse_optional_comment(token_iter);
    }

    A2mlSpec {
        name,
        types,
    }
}


// parse_a2ml_type()
// Implements the grammar rules
//    type_name = predefined_type_name | struct_type_name | taggedstruct_type_name | taggedunion_type_name | enum_type_name
//    predefined_type_name = "char" | "int" | "long" | "uchar" | "uint" | "ulong" | "double" | "float"
fn parse_a2ml_type(token_iter: &mut TokenStreamIter, types: &A2mlTypeList, tok_start: &str) -> (Option<String>, A2mlType) {
    match tok_start {
        "block" => {
            let (tag, item) = parse_a2ml_block(token_iter, &types);
            (Some(tag), item)
        }
        "char" => (None, A2mlType::Char),
        "int" => (None, A2mlType::Int),
        "long" => (None, A2mlType::Long),
        "uchar" => (None, A2mlType::Uchar),
        "uint" => (None, A2mlType::Uint),
        "ulong" => (None, A2mlType::Ulong),
        "float" => (None, A2mlType::Float),
        "double" => (None, A2mlType::Double),
        "enum" => parse_a2ml_type_enum(token_iter, &types),
        "struct" => parse_a2ml_type_struct(token_iter, &types),
        "taggedstruct" => parse_a2ml_type_taggedstruct(token_iter, &types),
        "taggedunion" => parse_a2ml_type_taggedunion(token_iter, &types),
        _ => panic!("unexpected token {:?} in type declaration", tok_start),
    }
}


// parse_a2ml_block()
// This function implements the grammar rule
//    block_definition = "block" tag type_name | "block" tag "(" type_name ")*"
// The block token has already been used and cused this function to be called so now a tag and
// a type name (possibly with repetition) are expected
fn parse_a2ml_block(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (String, A2mlType) {
    let tag = get_string(token_iter);

    let mut has_repetition = false;
    let mut block_token_iter = token_iter;
    let mut block_token_iter_base;
    let block_tokens;
    if let Some(TokenTree::Group(g)) = block_token_iter.peek() {
        if g.delimiter() == Delimiter::Parenthesis {
            block_tokens = get_group(block_token_iter, Delimiter::Parenthesis);
            require_punct(block_token_iter, '*');
            block_token_iter_base = block_tokens.into_iter().peekable();
            block_token_iter = &mut block_token_iter_base;
            has_repetition = true;
        }
    }

    let (typename, mut btype, varname) = parse_a2ml_member(block_token_iter, &types);
    if has_repetition {
        btype = A2mlType::Sequence(Box::new(btype))
    }

    (tag.clone(), A2mlType::Block(tag, Box::new(DataItem {typename, item_type: btype, varname, comment: None})))
}


// parse_a2ml_type_enum()
// Parses enum definitions according to the grammar:
//    enum_type_name = "enum" [ identifier ] "{" enumerator_list "}" | "enum" identifier
//    enumerator_list = enumerator | enumerator "," enumerator_list
//    enumerator = keyword [ "=" constant ]
//
// If the short form "enum identifier;" is found, then the type is looked up in the hashmap of previously defined enums
// If not, a new enum definition is expected
fn parse_a2ml_type_enum(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, A2mlType) {
    let name: Option<String> = parse_optional_name(token_iter);

    if let Some(TokenTree::Group(_)) = token_iter.peek() {
        // parse the list of enum items
        let enum_tokens = get_group(token_iter, Delimiter::Brace);
        let enum_token_iter = &mut enum_tokens.into_iter().peekable();
        let mut enumvalues = Vec::new();
        while enum_token_iter.peek().is_some() {
            let tag = get_string(enum_token_iter);
            let mut value = None;
            // each enum item may specify an assiciated value e.g. "= 123"
            if let Some(TokenTree::Punct(p)) = enum_token_iter.peek() {
                if p.as_char() == '=' {
                    get_punct(enum_token_iter);
                    value = Some(get_integer(enum_token_iter));
                }
            }
            // a comment is required after every enum value except the last one
            let mut lastvalue = true;
            if let Some(TokenTree::Punct(p)) = enum_token_iter.peek() {
                if p.as_char() == ',' {
                    get_punct(enum_token_iter);
                    lastvalue = false;
                }
            }
            let comment = parse_optional_comment(enum_token_iter);
            // if there was no comma, but there are stil items remaining, then that is a parsing error
            if lastvalue && enum_token_iter.peek().is_some() {
                panic!("additional items ({:#?}, ...) found in enum; missing ','?", enum_token_iter.peek().unwrap());
            }
            enumvalues.push(EnumItem {item_name: tag, value, comment});
        }
        (name, A2mlType::Enum(enumvalues))
    } else {
        // there is no list of items, so this can only be a reference to a previous declaration
        if name.is_some() {
            let name = String::from(name.unwrap());
            let refenum = types.get_enum(&name);
            if refenum.is_none() {
                panic!("enum {} was referenced but not defined", name);
            }
            (Some(name.clone()), A2mlType::EnumRef(name))
        } else {
            panic!("expected either an identifier or an opening bracket after keyword enum.");
        }
    }
}


// parse_a2ml_type_struct()
// Parses struct definitions according to the grammar: 
//    struct_type_name = "struct" [ identifier ] "{" [struct_member_list ] "}" | "struct" identifier
//    struct_member_list = struct_member | struct_member struct_member_list
//    struct_member = member ";" 
fn parse_a2ml_type_struct(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, A2mlType) {
    let struct_typename: Option<String> = parse_optional_name(token_iter);

    // check if there is definition of the struct enclosed in {} or if is a reference to a previous declaration
    if let Some(TokenTree::Group(_)) = token_iter.peek() {
        // parse the struct elements
        let struct_tokens = get_group(token_iter, Delimiter::Brace);
        let struct_token_iter = &mut struct_tokens.into_iter().peekable();
        let mut structdata = Vec::new();
        while struct_token_iter.peek().is_some() {
            let (item_typename, struct_item, item_varname) = parse_a2ml_member(struct_token_iter, &types);
            require_punct(struct_token_iter, ';');
            let comment = parse_optional_comment(struct_token_iter);
            structdata.push(DataItem {typename: item_typename, item_type: struct_item, varname: item_varname, comment});
        }

        (struct_typename, A2mlType::Struct(structdata))
    } else {
        // no definition of the struct, so it must be a reference
        if struct_typename.is_some() {
            let name = String::from(struct_typename.unwrap());
            let refstruct = types.get_struct(&name);
            if refstruct.is_none() {
                panic!("struct {} was referenced but not defined", name);
            }
            (Some(name.clone()), A2mlType::StructRef(name))
        } else {
            panic!("expected either an identifier or an opening bracket after keyword struct.");
        }
    }
}


// parse_a2ml_type_taggedstruct()
// Parses taggedstructs according to the grammar:
//    taggedstruct_type_name = "taggedstruct" [ identifier ] "{" [taggedstruct_member_list ] "}" | "taggedstruct" identifier
//    taggedstruct_member_list = taggedstruct_member | taggedstruct_member taggedstruct_member_list
fn parse_a2ml_type_taggedstruct(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, A2mlType) {
    let typename: Option<String> = parse_optional_name(token_iter);

    // check if there is definition of the taggedstruct enclosed in {} or if is a reference to a previous declaration
    if let Some(TokenTree::Group(_)) = token_iter.peek() {
        // parse the struct elements
        let ts_tokens = get_group(token_iter, Delimiter::Brace);
        let ts_token_iter = &mut ts_tokens.into_iter().peekable();
        let mut ts_members = Vec::<TaggedItem>::new();

        while ts_token_iter.peek().is_some() {
            let mut tsitem = parse_a2ml_taggedstructmember(ts_token_iter, &types);
            require_punct(ts_token_iter, ';');
            tsitem.item.comment = parse_optional_comment(ts_token_iter);
            ts_members.push(tsitem);
        }

        (typename, A2mlType::TaggedStruct(ts_members))
    } else {
        // no definition of the taggedstruct, so it must be a reference
        if typename.is_some() {
            let name = String::from(typename.unwrap());
            let refts = types.get_taggedstruct(&name);
            if refts.is_none() {
                panic!("taggedstruct {} was referenced but not defined", name);
            }
            (Some(name.clone()), A2mlType::TaggedStructRef(name))
        } else {
            panic!("expected either an identifier or an opening bracket after keyword taggedstruct.");
        }
    }
}


// parse_a2ml_taggedstructmember()
// Parses taggedstruct members according to the grammar:
//    taggedstruct_member = taggedstruct_definition ";" | "(" taggedstruct_definition ")*;" | block_definition ";" | "(" block_definition ")*;"
fn parse_a2ml_taggedstructmember(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> TaggedItem {
    let mut ts_item_iter = token_iter;

    let mut multi = false;
    let ts_item;
    let mut ts_item_iter_base;
    if let Some(TokenTree::Group(_)) = ts_item_iter.peek() {
        ts_item = get_group(ts_item_iter, Delimiter::Parenthesis);
        require_punct(ts_item_iter, '*');
        ts_item_iter_base = ts_item.into_iter().peekable();
        ts_item_iter = &mut ts_item_iter_base;
        multi = true;
    }

    match ts_item_iter.peek() {
        Some(TokenTree::Ident(proc_macro::Ident {..})) => {
            let ident = get_ident(ts_item_iter);
            if &*ident == "block" {
                let (tag, taggedmember) = parse_a2ml_block(ts_item_iter, &types);
                TaggedItem {tag, item: DataItem {item_type: taggedmember, typename: None, varname: None, comment: None}, repeat: multi}
            } else {
                panic!("Identifier {} is not allowed in the definition of a taggedstruct", ident);
            }
        }
        Some(TokenTree::Literal(_)) => {
            let tag = get_string(ts_item_iter);
            let dataitem;
            if multi && ts_item_iter.peek().is_none() {
                // case 1: repeating, i.e. enclosed in ( ... )*
                // In this case the ts_item_iter only covers the items inside the parenthesis and there are no more items
                dataitem = DataItem {typename: None, item_type: A2mlType::None, varname: None, comment: None};
            } else if let Some(TokenTree::Punct(_)) = ts_item_iter.peek() {
                // case 2: not repeating, so ts_item_iter refers to some larger group of items.
                // In this case the taggeditem contains None if punctuation (i.e. ';') is seen
                dataitem = DataItem {typename: None, item_type: A2mlType::None, varname: None, comment: None};
            } else {
                // case 3: not at the end of the iterator and not followed by ';' immediately, so there is an item to parse
                dataitem = parse_a2ml_tagged_def(ts_item_iter, &types);
            };
            TaggedItem { tag, item: dataitem, repeat: multi }
        }
        tok => {
            panic!("got {:#?} while attempting to parse taggedstruct member", tok);
        }
    }
}


// parse_aml_tagged_def()
// Parses taggedstruct definitions according to the grammar:
//    taggedstruct_definition = tag [ member ] | tag "(" member ")*;"
fn parse_a2ml_tagged_def(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> DataItem {
    let mut tagged_def_iter = token_iter;

    let mut inner_repeat = false;
    let mut tagged_def_iter_base;
    if let Some(TokenTree::Group(_)) = tagged_def_iter.peek() {
        let tagged_def_items = get_group(tagged_def_iter, Delimiter::Parenthesis);
        tagged_def_iter_base = tagged_def_items.into_iter().peekable();
        tagged_def_iter = &mut tagged_def_iter_base;
        inner_repeat = true;
    }

    let (typename, mut member, varname) = parse_a2ml_member(tagged_def_iter, &types);

    if inner_repeat {
        member = A2mlType::Sequence(Box::new(member));
    }

    DataItem { typename, item_type: member, varname, comment: None}
}


// parse_aml_type_taggedunion()
//    taggedunion_type_name = "taggedunion" [ identifier ] "{" [taggedunion_member_list ] "}" | "taggedunion" identifier
//    taggedunion_member_list = tagged_union_member | tagged_union_member taggedunion_member_list
fn parse_a2ml_type_taggedunion(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, A2mlType) {
    let typename: Option<String> = parse_optional_name(token_iter);

    // check if there is definition of the taggedunion enclosed in {} or if is a reference to a previous declaration
    if let Some(TokenTree::Group(_)) = token_iter.peek() {
        // parse the union elements
        let tu_tokens = get_group(token_iter, Delimiter::Brace);
        let tu_token_iter = &mut tu_tokens.into_iter().peekable();
        let mut taggeduniondata = Vec::<TaggedItem>::new();
        while tu_token_iter.peek().is_some() {
            let mut item = parse_a2ml_taggedunionmember(tu_token_iter, &types);
            require_punct(tu_token_iter, ';');
            item.item.comment = parse_optional_comment(tu_token_iter);
            taggeduniondata.push(item);
        }
        (typename, A2mlType::TaggedUnion(taggeduniondata))
    } else {
        // no definition of the taggedunion, so it must be a reference
        if typename.is_some() {
            let name = String::from(typename.unwrap());
            let reftu = types.get_taggedunion(&name);
            if reftu.is_none() {
                panic!("taggedunion {} was referenced but not defined", name);
            }
            (Some(name.clone()), A2mlType::TaggedUnionRef(name))
        } else {
            panic!("expected either an identifier or an opening bracket after keyword taggedunion.");
        }
    }
}


// parse_aml_taggedunionmember()
//    tagged_union_member = tag [ member ] ";" | block_definition ";"
fn parse_a2ml_taggedunionmember(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> TaggedItem {
    match token_iter.peek() {
        Some(TokenTree::Ident(proc_macro::Ident {..})) => {
            let ident = get_ident(token_iter);
            if &*ident == "block" {
                let (tag, tuitem) = parse_a2ml_block(token_iter, &types);
                TaggedItem {tag, item: DataItem {item_type: tuitem, typename: None, varname: None, comment: None}, repeat: false}
            } else {
                panic!("Identifier {} is not allowed in the definition of a taggedunion", ident);
            }
        }
        Some(TokenTree::Literal(_)) => {
            let tag = get_string(token_iter);
            let dataitem = if let Some(TokenTree::Punct(_)) = token_iter.peek() {
                DataItem { typename: None, item_type: A2mlType::None, varname: None, comment: None }
            } else {
                parse_a2ml_tagged_def(token_iter, &types)
            };
            TaggedItem {tag, item: dataitem, repeat: false}
        }
        tok => {
            panic!("got {:#?} while attempting to parse taggedstruct member", tok);
        }
    }
}


// parse_aml_member()
// Parse a member of some other data structure. Each member could potentially have an arbitrary number of array dimensions
//    member = type_name [ array_specifier ]
//    array_specifier = "[" constant "]" | "[" constant "]" array_specifier
fn parse_a2ml_member(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, A2mlType, Option<String>) {
    let tok_start = get_ident(token_iter);
    let (typename, mut base_type) = parse_a2ml_type(token_iter, &types, &tok_start);
    let varname = parse_optional_name(token_iter);

    while let Some(TokenTree::Group(_)) = token_iter.peek() {
        let array_dim_tokens = get_group(token_iter, Delimiter::Bracket);
        let array_dim_iter = &mut array_dim_tokens.into_iter().peekable();
        let dim = get_integer(array_dim_iter);

        /* special case: char[x] -> string */
        // match base_type {
        //     A2mlType::Char => {
        //         base_type = A2mlType::String;
        //     }
        //     _ => {
                base_type = A2mlType::Array(Box::new(base_type), dim as usize);
        //     }
        // }
    }

    (typename, base_type, varname)
}


// parse_optional_name()
// For enums, structs, taggedstructs and taggedunions the typename is optional.
// Called at the beginning of parsing one of these data strucutres, this function checks if the next token is a type name and returns it
fn parse_optional_name(token_iter: &mut TokenStreamIter) -> Option<String> {
    if let Some(TokenTree::Ident(_)) = token_iter.peek() {
        let ident = get_ident(token_iter);
        Some( ident.to_string() )
    } else {
        None
    }
}


// Extension: parse_optional_comment()
// check if there is a rust-style doc comment in the input and parse it
fn parse_optional_comment(token_iter: &mut TokenStreamIter) -> Option<String> {
    if let Some(TokenTree::Punct(_)) = token_iter.peek() {
        require_punct(token_iter, '#');
        let doc_tokens = get_group(token_iter, Delimiter::Bracket);
        let doc_iter = &mut doc_tokens.into_iter().peekable();
        let doc_ident = get_ident(doc_iter);
        if doc_ident == "doc" {
            require_punct(doc_iter, '=');
            Some(get_string(doc_iter))
        } else {
            None
        }
    } else {
        None
    }
}


impl A2mlTypeList {
    fn get_item(&self, name: &str, kind: std::mem::Discriminant<A2mlType>) -> Option<&A2mlType> {
        let findresult = self.list.iter().find(|(itemname, item)| {
            itemname == name && kind == std::mem::discriminant(item)
        });

        if let Some((_, item)) = findresult {
            Some(item)
        } else {
            None
        }
    }

    fn get_enum(&self, name: &str) -> Option<&A2mlType> {
        self.get_item(name, std::mem::discriminant(&A2mlType::Enum(Vec::<EnumItem>::new())))
    }

    fn get_struct(&self, name: &str) -> Option<&A2mlType> {
        self.get_item(name, std::mem::discriminant(&A2mlType::Struct(Vec::<DataItem>::new())))
    }

    fn get_taggedstruct(&self, name: &str) -> Option<&A2mlType> {
        self.get_item(name, std::mem::discriminant(&A2mlType::TaggedStruct(Vec::<TaggedItem>::new())))
    }

    fn get_taggedunion(&self, name: &str) -> Option<&A2mlType> {
        self.get_item(name, std::mem::discriminant(&A2mlType::TaggedUnion(Vec::<TaggedItem>::new())))
    }

    fn get_block(&self, name: &str) -> Option<&A2mlType> {
        self.get_item(name, std::mem::discriminant(&A2mlType::Block(name.to_string(), Box::new(DataItem{typename:None, item_type:A2mlType::None, varname:None, comment:None}))))
    }
}


//-----------------------------------------------------------------------------

const A2ML_INDENT_WIDTH: usize = 2; // number of spaces per indent level
const A2ML_INITIAL_INDENT_LEVEL: usize = 3; // typically a2ml will be indented by 3 levels within the A2l file

fn generate_a2ml_constant(spec: &A2mlSpec) -> proc_macro2::TokenStream {
    let constname = format_ident!("{}_TEXT", spec.name.to_ascii_uppercase());
    let mut result = quote!{};

    let mut definition = "".to_string();
    for (name, item) in &spec.types.list {
        generate_a2ml_constant_of_item(&mut definition, &Some(name.clone()), item, A2ML_INITIAL_INDENT_LEVEL, true);
        write!(definition, ";\n\n").unwrap();
    }

    let def_literal = proc_macro2::Literal::string(&definition);
    result.extend(quote!{const #constname: &str = #def_literal;});

    result
}


fn generate_a2ml_constant_of_item(outstring: &mut String, name: &Option<String>, item: &A2mlType, indent_level: usize, indent_first: bool) {

    if indent_first {
        write!(outstring, "{:1$}", "", (indent_level * A2ML_INDENT_WIDTH)).unwrap();
    }

    match item {
        A2mlType::None => { }
        A2mlType::Char => { write!(outstring, "char").unwrap(); }
        A2mlType::Int => { write!(outstring, "int").unwrap(); }
        A2mlType::Long => { write!(outstring, "long").unwrap(); }
        A2mlType::Uchar => { write!(outstring, "uchar").unwrap(); }
        A2mlType::Uint => { write!(outstring, "uint").unwrap(); }
        A2mlType::Ulong => { write!(outstring, "ulong").unwrap(); }
        A2mlType::Double => { write!(outstring, "double").unwrap(); }
        A2mlType::Float => { write!(outstring, "float").unwrap(); }
        A2mlType::Array(itemtype, dim) => {
            generate_a2ml_constant_of_item(outstring, name, &**itemtype, indent_level, false);
            write!(outstring, "[{}]", dim).unwrap();
        }
        A2mlType::Sequence(seqtype) => {
            write!(outstring, "(").unwrap();
            generate_a2ml_constant_of_item(outstring, name, &**seqtype, indent_level, false);
            write!(outstring, ")*").unwrap();
        }
        A2mlType::Enum(enumdef) => {
            generate_a2ml_constant_of_enum(outstring, name, enumdef, indent_level);
        }
        A2mlType::EnumRef(refname) => {
            write!(outstring, "enum {}", refname).unwrap();
        }
        A2mlType::Struct(structdef) => {
            generate_a2ml_constant_of_struct(outstring, name, structdef, indent_level);
        }
        A2mlType::StructRef(refname) => {
            write!(outstring, "struct {}", refname).unwrap();
        }
        A2mlType::TaggedUnion(tudef) => {
            generate_a2ml_constant_of_taggedunion(outstring, name, tudef, indent_level);
        }
        A2mlType::TaggedUnionRef(refname) => {
            write!(outstring, "taggedunion {}", refname).unwrap();
        }
        A2mlType::TaggedStruct(tsdef) => {
            generate_a2ml_constant_of_taggedstruct(outstring, name, tsdef, indent_level);
        }
        A2mlType::TaggedStructRef(refname) => {
            write!(outstring, "taggedstruct {}", refname).unwrap();
        }
        A2mlType::Block(blkname, item) => {
            generate_a2ml_constant_of_block(outstring, blkname, &item.item_type, indent_level);
        }
    }
}


fn generate_a2ml_constant_of_enum(outstring: &mut String, name: &Option<String>, enumdef: &Vec<EnumItem>, indent_level: usize) {
    write!(outstring, "enum").unwrap();
    if let Some(name_unwrapped) = name {
        write!(outstring, " {}", name_unwrapped).unwrap();
    }
    write!(outstring, " {{\n").unwrap();
    let itemcount = enumdef.len();
    for (idx, item) in enumdef.iter().enumerate() {
        write!(outstring, "{:2$}\"{}\"", "", item.item_name, (indent_level+1) * A2ML_INDENT_WIDTH).unwrap();
        if let Some(val) = item.value {
            write!(outstring, " = {}", val).unwrap();
        }
        if idx < itemcount-1 {
            write!(outstring, ",").unwrap();
        }
        if let Some(comment) = &item.comment {
            write!(outstring, "//{}", comment).unwrap();
        }
        write!(outstring, "\n").unwrap();
    }
    write!(outstring, "{:1$}}}", "", indent_level * A2ML_INDENT_WIDTH).unwrap();
}


fn generate_a2ml_constant_of_struct(outstring: &mut String, name: &Option<String>, structdef: &Vec<DataItem>, indent_level: usize) {
    write!(outstring, "struct").unwrap();
    if let Some(name_unwrapped) = name {
        write!(outstring, " {}", name_unwrapped).unwrap();
    }
    write!(outstring, " {{\n").unwrap();
    for structitem in structdef {
        generate_a2ml_constant_of_item(outstring, &structitem.typename, &structitem.item_type, indent_level + 1, true);
        write!(outstring, ";").unwrap();
        if let Some(comment) = &structitem.comment {
            write!(outstring, " //{}", comment).unwrap();
        }
        write!(outstring, "\n").unwrap();
    }
    write!(outstring, "{:1$}}}", "", indent_level * A2ML_INDENT_WIDTH).unwrap();
}


fn generate_a2ml_constant_of_taggedunion(outstring: &mut String, name: &Option<String>, tudef: &Vec<TaggedItem>, indent_level: usize) {
    write!(outstring, "taggedunion").unwrap();
    if let Some(name_unwrapped) = name {
        write!(outstring, " {}", name_unwrapped).unwrap();
    }
    write!(outstring, " {{\n").unwrap();
    for tuitem in tudef.iter() {
        if let A2mlType::Block(_,_) = tuitem.item.item_type {
            generate_a2ml_constant_of_item(outstring, &tuitem.item.typename, &tuitem.item.item_type, indent_level + 1, true);
        } else {
            write!(outstring, "{:2$}\"{}\" ", "", tuitem.tag, (indent_level+1) * A2ML_INDENT_WIDTH).unwrap();
            generate_a2ml_constant_of_item(outstring, &tuitem.item.typename, &tuitem.item.item_type, indent_level + 1, false);
        }
        write!(outstring, ";").unwrap();
        if let Some(comment) = &tuitem.item.comment {
            write!(outstring, " //{}", comment).unwrap();
        }
        write!(outstring, "\n").unwrap();
    }
    write!(outstring, "{:1$}}}", "", indent_level * A2ML_INDENT_WIDTH).unwrap();
}


fn generate_a2ml_constant_of_taggedstruct(outstring: &mut String, name: &Option<String>, tsdef: &Vec<TaggedItem>, indent_level: usize) {
    write!(outstring, "taggedstruct").unwrap();
    if let Some(name_unwrapped) = name {
        write!(outstring, " {}", name_unwrapped).unwrap();
    }
    write!(outstring, " {{\n").unwrap();
    for tsitem in tsdef.iter() {
        write!(outstring, "{:1$}", "", (indent_level+1) * A2ML_INDENT_WIDTH).unwrap();
        if tsitem.repeat {
            write!(outstring, "(").unwrap();
        }
        if let A2mlType::Block(_,_) = tsitem.item.item_type {
            generate_a2ml_constant_of_item(outstring, &tsitem.item.typename, &tsitem.item.item_type, indent_level + 1, false);
        } else {
            write!(outstring, "\"{}\" ", tsitem.tag).unwrap();
            generate_a2ml_constant_of_item(outstring, &tsitem.item.typename, &tsitem.item.item_type, indent_level + 1, false);
        }
        if tsitem.repeat {
            write!(outstring, ")*").unwrap();
        }
        write!(outstring, ";").unwrap();
        if let Some(comment) = &tsitem.item.comment {
            write!(outstring, " //{}", comment).unwrap();
        }
        write!(outstring, "\n").unwrap();
    }
    write!(outstring, "{:1$}}}", "", indent_level * A2ML_INDENT_WIDTH).unwrap();
}


fn generate_a2ml_constant_of_block(outstring: &mut String, name: &String, blocktype: &A2mlType, indent_level: usize) {
    write!(outstring, "block \"{}\" ", name).unwrap();
    generate_a2ml_constant_of_item(outstring, &None, blocktype, indent_level, false);
}


//-----------------------------------------------------------------------------
//
// The datatypes derived from the input a2ml specification are not directly suited for code generation
// Problems taht need to be fixed:
// - items are not required to have names
// - there is nothing that prevents the spec from using one type name with different meanings in different places
// - a2ml is often excessivly complex, e.g. struct {a, b, struct {c, d}} can be flattened to struct {a, b, c, d}

fn fixup_output_datatypes(spec: &A2mlSpec) -> HashMap<String, A2mlType> {
    if let Some(A2mlType::Block(_, item)) = spec.types.get_block("IF_DATA") {
        let mut datatypes = HashMap::<String, A2mlType>::new();
        fixup_output_block(spec, &spec.name, item, &mut datatypes);
        datatypes
    } else {
        panic!("the specification must define the IF_DATA block, which is the root of all defined elements");
    }
}


fn fixup_output_block(spec: &A2mlSpec, tag: &str, item: &DataItem, defined_types: &mut HashMap<String, A2mlType>) -> String {
    let structname = ucname_to_typename(tag);
    let mut structitems = fixup_add_data_to_struct(spec, item, defined_types);
    fixup_make_varnames_unique(&mut structitems);

    let newstruct = A2mlType::Struct(structitems);
    fixup_add_type(structname.clone(), newstruct, defined_types)
}


fn fixup_add_data_to_struct(spec: &A2mlSpec, item: &DataItem, defined_types: &mut HashMap<String, A2mlType>) -> Vec<DataItem> {
    let mut new_structitems= Vec::<DataItem>::new();

    match &item.item_type {
        A2mlType::Struct(structitems) => {
            for sitem in structitems {
                new_structitems.extend(fixup_add_data_to_struct(spec, sitem, defined_types));
            }
        }
        A2mlType::StructRef(refname) => {
            let refstruct = spec.types.get_struct(refname);
                if let Some(A2mlType::Struct(structitems)) = refstruct {
                    for sitem in structitems {
                        new_structitems.extend(fixup_add_data_to_struct(spec, sitem, defined_types));
                    }
                } else {
                panic!("invalid struct reference {} in a2ml spec", refname);
            }
        }
        A2mlType::None => {
            // items with the type None shouldn't be added to the output struct
        }
        _ => {
            let varname = make_itemname(&item.varname, &item.typename);
            new_structitems.push(DataItem {
                typename: item.typename.clone(),
                item_type: fixup_data_type(spec, &item.typename, &item.item_type, &item.comment, defined_types),
                varname: Some(varname),
                comment: item.comment.clone()
            });
        }
    }

    new_structitems
}


fn fixup_data_type(spec: &A2mlSpec, itemname: &Option<String>, itemtype: &A2mlType, itemcomment: &Option<String>, defined_types: &mut HashMap<String, A2mlType>) -> A2mlType {
    match itemtype {
        A2mlType::Array(arraytype, dim) => {
            A2mlType::Array(Box::new(fixup_data_type(spec, itemname, arraytype, itemcomment, defined_types)), *dim)
        }
        A2mlType::Sequence(seqtype) => {
            match &**seqtype {
                A2mlType::Sequence(inner_seqtype) => {
                    // seqence(seqence(x)) makes no sense, flatten it to just sequence(x)
                    A2mlType::Sequence(Box::new(fixup_data_type(spec, itemname, inner_seqtype, itemcomment, defined_types)))
                }
                A2mlType::TaggedStruct(_) |
                A2mlType::TaggedStructRef(_) => {
                    panic!("A repeating taggedstruct cannot be parsed properly, because there is no separator between each instance of the taggedstruct. Suggestion: use a taggdstruct with repeating elements instead.")
                }
                A2mlType::TaggedUnion(_) |
                A2mlType::TaggedUnionRef(_) => {
                    panic!("A repeating taggedunion does not make sense. Use a taggedstruct instead")
                }
                _ => {
                    A2mlType::Sequence(Box::new(fixup_data_type(spec, itemname, seqtype, itemcomment, defined_types)))
                }
            }
        }
        A2mlType::Enum(enumitems) => {
            let newenum = A2mlType::Enum(enumitems.clone());
            let enumname = make_enum_name(itemname, &enumitems);
            let final_enumname = fixup_add_type(enumname.clone(), newenum, defined_types);
            A2mlType::EnumRef(final_enumname)
        }
        A2mlType::EnumRef(refname) => {
            if let Some(A2mlType::Enum(enumitems)) = spec.types.get_enum(refname) {
                let newenum = A2mlType::Enum(enumitems.clone());
                let final_enumname = fixup_add_type(refname.to_owned(), newenum, defined_types);
                A2mlType::EnumRef(final_enumname)
            } else {
                panic!("failed to resolve enum reference {}", refname);
            }
        }
        A2mlType::Struct(structitems) => {
            if let Some(name) = itemname {
                fixup_struct(name.to_owned(), structitems, spec, defined_types)
            } else {
                panic!("unnamed struct {:#?} found in a position where no name can be derived from context.", itemtype);
            }
        }
        A2mlType::StructRef(refname) => {
            if let Some(A2mlType::Struct(structitems)) = spec.types.get_struct(refname) {
                fixup_struct(refname.to_owned(), structitems, spec, defined_types)
            } else {
                panic!("failed to resolve taggedunion reference {}", refname)
            }
        }
        A2mlType::TaggedUnion(tuitems) => {
            A2mlType::TaggedUnion(fixup_taggeditems(tuitems, spec, defined_types))
        }
        A2mlType::TaggedUnionRef(refname) => {
            if let Some(A2mlType::TaggedUnion(tuitems)) = spec.types.get_taggedunion(refname) {
                A2mlType::TaggedUnion(fixup_taggeditems(tuitems, spec, defined_types))
            } else {
                panic!("failed to resolve taggedunion reference {}", refname)
            }
        }
        A2mlType::TaggedStruct(tsitems) => {
            A2mlType::TaggedStruct(fixup_taggeditems(tsitems, spec, defined_types))
        }
        A2mlType::TaggedStructRef(refname) => {
            if let Some(A2mlType::TaggedStruct(tsitems)) = spec.types.get_taggedstruct(refname) {
                A2mlType::TaggedStruct(fixup_taggeditems(tsitems, spec, defined_types))
            } else {
                panic!("failed to resolve taggedstruct reference {}", refname)
            }
        }
        A2mlType::Block(_, _) => {
            panic!("Type block should never be seen in this context")
        }
        A2mlType::None => A2mlType::None,
        A2mlType::Char => A2mlType::Char,
        A2mlType::Int => A2mlType::Int,
        A2mlType::Long => A2mlType::Long,
        A2mlType::Uchar => A2mlType::Uchar,
        A2mlType::Uint => A2mlType::Uint,
        A2mlType::Ulong => A2mlType::Ulong,
        A2mlType::Double => A2mlType::Double,
        A2mlType::Float => A2mlType::Float,
    }
}


fn fixup_struct(structname: String, structitems: &Vec<DataItem>, spec: &A2mlSpec, defined_types: &mut HashMap<String, A2mlType>) -> A2mlType {
    let mut new_structitems = Vec::<DataItem>::new();
    for item in structitems {
        new_structitems.extend(fixup_add_data_to_struct(spec, &item, defined_types));
    }
    fixup_make_varnames_unique(&mut new_structitems);

    let new_structname = fixup_add_type(structname, A2mlType::Struct(new_structitems), defined_types);

    A2mlType::StructRef(new_structname)
}


fn fixup_taggeditems(taggeditems: &Vec<TaggedItem>, spec: &A2mlSpec, defined_types: &mut HashMap<String, A2mlType>) -> Vec<TaggedItem> {
    let mut new_tuitems = Vec::<TaggedItem>::new();
    for tgitem in taggeditems {
        if let A2mlType::Block(tag, item) = &tgitem.item.item_type {
            let new_typename = fixup_output_block(spec, tag, item, defined_types);
            let newblock = A2mlType::Block(tag.clone(), Box::new(
                DataItem {
                    typename: Some(new_typename.clone()),
                    item_type: A2mlType::StructRef(new_typename),
                    varname: None,
                    comment: None
                }
            ));
            new_tuitems.push(TaggedItem {
                tag: tgitem.tag.clone(),
                item: DataItem { typename: None, item_type: newblock, varname: None, comment: None },
                repeat: tgitem.repeat
            });
        } else {
            let new_typename = fixup_output_block(spec, &tgitem.tag, &tgitem.item, defined_types);
            new_tuitems.push(TaggedItem {
                tag: tgitem.tag.clone(),
                item: DataItem { typename: Some(new_typename.clone()), item_type: A2mlType::StructRef(new_typename), varname: None, comment: None },
                repeat: tgitem.repeat
            });
        }
    }

    new_tuitems
}


fn fixup_add_type(itemname: String, newitem: A2mlType, defined_types: &mut HashMap<String, A2mlType>) -> String {
    let mut new_itemname = itemname.clone();
    let mut suffix = 1;
    let type_exists;

    loop {
        if let Some(item) = defined_types.get(&new_itemname) {
            if *item == newitem {
                type_exists = true;
                break;
            } else {
                suffix += 1;
                new_itemname = format!("{}{}", itemname, suffix);
            }
        } else {
            type_exists = false;
            break;
        }
    }

    if !type_exists {
        defined_types.insert(new_itemname.clone(), newitem);
    }

    new_itemname
}


fn make_itemname(varname: &Option<String>, typename: &Option<String>) -> String {
    if let Some(name) = varname {
        // there is a name for the item in the spec
        name.to_owned()
    } else {
        if let Some(name) = typename {
            typename_to_varname(name)
        } else {
            // no suggested name and no way to derive a name from the type name. The item will be named "item" and likely be renamed to item_<n> later
            "item".to_string()
        }
    }
}


fn fixup_make_varnames_unique(structitems: &mut Vec<DataItem>) {
    let mut names = HashSet::<String>::new();

    for item in structitems {
        if let Some(varname) = &item.varname {
            let mut tmp_varname = varname.clone();
            let mut idx = 1;
            while names.get(&tmp_varname).is_some() {
                idx += 1;
                tmp_varname = format!("{}_{}", varname, idx);
            }
            if &tmp_varname != varname {
                item.varname = Some(tmp_varname.clone());
            }
            names.insert(tmp_varname);
        }
    }
}


fn make_enum_name(itemname: &Option<String>, enumitems: &Vec<EnumItem>) -> String {
    if let Some(name) = itemname {
        // a name already exists
        name.clone()
    } else {
        // try to find a common prefix across all enumitems. For example the enum
        //     FOO_BAR_ONE
        //     FOO_BAR_TWO
        // has the common prefix FOO_BAR_
        let namechars: Vec<Vec<char>> = enumitems.iter().map(|item| { item.item_name.chars().collect() }).collect();
        let minlen = namechars.iter().map(|vec| vec.len()).min().unwrap();
        let mut prefixlen = minlen;
        'outer_loop: for pos in 0..minlen {
            let nextchar = namechars[0][pos];
            for enumidx in 1..enumitems.len() {
                if namechars[enumidx][pos] != nextchar {
                    prefixlen = pos;
                    break 'outer_loop;
                }
            }
        }

        if prefixlen > 3 {
            // only bother with nontrivial prefixes
            let prefixname: String = namechars[0][0..prefixlen].iter().collect();
            ucname_to_typename(&prefixname)
        } else {
            // no provided name and no usable prefix either; generate a name of the form AnonEnum<x>
            String::from("AnonEnum")
        }
    }
}


fn typename_to_varname(varname: &str) -> String {
    let inchars: Vec<char> = varname.chars().collect();
    let mut outchars = Vec::<char>::new();

    outchars.push(inchars[0].to_ascii_lowercase());
    for idx in 1..inchars.len() {
        if inchars[idx].is_ascii_alphabetic() && inchars[idx].is_ascii_uppercase() {
            outchars.push('_');
            outchars.push(inchars[idx].to_ascii_lowercase());
        } else {
            outchars.push(inchars[idx]);
        }
    }

    outchars.iter().collect()
}


//-----------------------------------------------------------------------------

fn generate_data_structures(types: &HashMap<String, A2mlType>) -> proc_macro2::TokenStream {
    let mut result = quote!{};
    let mut typesvec: Vec<(&String, &A2mlType)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());

    for (typename, a2mltype) in typesvec {
        match a2mltype {
            A2mlType::Enum(enumitems) => {
                result.extend(generate_enum_data_structure(typename, enumitems));
            }
            A2mlType::Struct(structitems) => {
                result.extend(generate_struct_data_structure(typename, structitems));
            }
            _ => {
                panic!("only struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, a2mltype);
            }
        }
    }

    result
}


fn generate_enum_data_structure(typename: &str, enumitems: &Vec<EnumItem>) -> proc_macro2::TokenStream {
    let typeident = format_ident!("{}", typename);
    let enumidents: Vec<proc_macro2::Ident> = enumitems.iter().map(|enumitem| { format_ident!("{}", ucname_to_typename(&enumitem.item_name)) } ).collect();
    quote!{
        #[derive(Debug, PartialEq)]
        pub(crate) enum #typeident {
            #(#enumidents),*
        }
    }
}


fn generate_struct_data_structure(typename: &str, structitems: &Vec<DataItem>) ->  proc_macro2::TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut definitions = Vec::new();

    for item in structitems {
        definitions.push(generate_struct_item_definition(item));
    }

    quote!{
        #[derive(Debug)]
        pub(crate) struct #typeident {
            fileid: usize,
            line: u32,

            #(#definitions),*
        }
    }
}


fn generate_struct_item_definition(item: &DataItem) -> proc_macro2::TokenStream {
    let mut def = quote!{};

    if let Some(comment) = &item.comment {
        def.extend(quote!{#[doc=#comment]});
    }
    if item.varname.is_none() {
        panic!("bad typename of {:#?}", item);
    }
    let itemname = format_ident!("{}", item.varname.to_owned().unwrap());

    match &item.item_type {
        A2mlType::None => { panic!("type None is not permitted for struct items"); }
        A2mlType::Block(_, _) => { panic!("type Block is not permitted for struct items"); }
        A2mlType::Enum(_) => { panic!("type Enum is not permitted at this point and should have been transformed to an EnumRef"); }
        A2mlType::Struct(_) => { panic!("type Struct is not permitted at this point and should have been transformed to a StructRef"); }
        A2mlType::TaggedUnionRef(_) => { panic!("TaggedUnionRef should have been resolved in the data structure fixup phase"); }
        A2mlType::TaggedStructRef(_) => { panic!("TaggedStructRef should have been resolved in the data structure fixup phase"); }
        A2mlType::TaggedUnion(tuitems) => {
            let mut tudefs = Vec::new();
            for tuitem in tuitems {
                let mut curr_def = quote!{};
                if let Some(comment) = &tuitem.item.comment {
                    curr_def.extend(quote!{#[doc=#comment]});
                }
                let tuitemname = format_ident!("{}", make_varname(&tuitem.tag));
                let typename = generate_bare_typename(&tuitem.item.item_type);
                curr_def.extend(quote!{#tuitemname: Option<#typename>});

                tudefs.push(curr_def);
            }
            def.extend(quote!{#(#tudefs),*});
        }
        A2mlType::TaggedStruct(tsitems) => {
            let mut tsdefs = Vec::new();
            for tsitem in tsitems {
                let mut curr_def = quote!{};
                if let Some(comment) = &tsitem.item.comment {
                    curr_def.extend(quote!{#[doc=#comment]});
                }
                let tuitemname = format_ident!("{}", make_varname(&tsitem.tag));
                let typename = generate_bare_typename(&tsitem.item.item_type);
                if tsitem.repeat {
                    curr_def.extend(quote!{#tuitemname: Vec<#typename>});
                } else {
                    curr_def.extend(quote!{#tuitemname: Option<#typename>});
                }

                tsdefs.push(curr_def);
            }
            def.extend(quote!{#(#tsdefs),*});
        }
        _ => {
            let typename = generate_bare_typename(&item.item_type);
            def.extend(quote!{#itemname: #typename});
        }
    }
    def
}


fn generate_bare_typename(item: &A2mlType) -> proc_macro2::TokenStream {
    match item {
        A2mlType::Char => { quote!{i8} }
        A2mlType::Int => { quote!{i16} }
        A2mlType::Long => { quote!{i32} }
        A2mlType::Uchar => { quote!{u8} }
        A2mlType::Uint => { quote!{u16} }
        A2mlType::Ulong => { quote!{u32} }
        A2mlType::Double => { quote!{f64} }
        A2mlType::Float => { quote!{f32} }
        A2mlType::Array(arraytype, dim) => {
            // a2ml specifies strings like C does: as arrays of char
            // if this pattern is found, then represent it as a String in Rust
            if **arraytype == A2mlType::Char {
                quote!{String}
            } else {
                let typename = generate_bare_typename(arraytype);
                quote!{[#typename; #dim]}
            }
        }
        A2mlType::Sequence(seqtype) => {
            let typename = generate_bare_typename(seqtype);
            quote!{Vec<#typename>}
        }
        A2mlType::EnumRef(refname) => {
            let typename = format_ident!("{}", refname);
            quote!{#typename}
        }
        A2mlType::StructRef(refname) => {
            let typename = format_ident!("{}", refname);
            quote!{#typename}
        }
        A2mlType::Block(_, item) => {
            let typename = generate_bare_typename(&item.item_type);
            quote!{#typename}
        }
        _ => {
            // None is not allowed at all once generation begins (previously it is a placeholder that is dropped during the fixup phase)
            // Enum, Struct, TaggedUnionRef, TaggedStructRef have been transformed in the fixup phase and can't occur here
            panic!("impossible type - unreachable");
        }
    }
}



//-----------------------------------------------------------------------------


fn generate_parser(types: &HashMap<String, A2mlType>) -> proc_macro2::TokenStream {
    let mut result = quote!{};
    let mut typesvec: Vec<(&String, &A2mlType)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());

    for (typename, a2mltype) in typesvec {
        match a2mltype {
            A2mlType::Enum(enumitems) => {
                result.extend(generate_enum_parser(typename, enumitems));
            }
            A2mlType::Struct(structitems) => {
                result.extend(generate_struct_parser(typename, structitems));
            }
            _ => {
                panic!("only struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, a2mltype);
            }
        }
    }
    result
}


fn generate_enum_parser(typename: &str, enumitems: &Vec<EnumItem>) -> proc_macro2::TokenStream {
    let name = format_ident!("{}", typename);

    let mut match_branches = Vec::new();
    for enitem in enumitems {
        let enident = format_ident!("{}", ucname_to_typename(&enitem.item_name));
        let entag = &enitem.item_name;
        match_branches.push(quote!{#entag => Ok(Self::#enident),});
    }

    quote!{
        impl #name {
            fn parse(parser: &mut a2lfile::ParserState, context: &a2lfile::ParseContext) -> Result<Self, a2lfile::ParseError> {
                let enumname = parser.get_identifier(context)?;
                match &*enumname {
                    #(#match_branches)*
                    _ => Err(a2lfile::ParseError::InvalidEnumValue(context.copy(), enumname))
                }
            }
        }
    }
}


fn generate_struct_parser(typename: &str, structitems: &Vec<DataItem>) -> proc_macro2::TokenStream {
    let name = format_ident!("{}", typename);

    let mut itemparsers = Vec::<proc_macro2::TokenStream>::new();
    let mut itemnames = Vec::<proc_macro2::Ident>::new();
    for sitem in structitems {
        match &sitem.item_type {
            A2mlType::TaggedStruct(tg_items) => {
                itemparsers.push(generate_taggeditem_parser(tg_items, false));
                itemnames.extend(generate_tagged_item_names(tg_items));
            }
            A2mlType::TaggedUnion(tg_items) => {
                itemparsers.push(generate_taggeditem_parser(tg_items, true));
                itemnames.extend(generate_tagged_item_names(tg_items));
            }
            A2mlType::Sequence(seqitem) => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                itemparsers.push(generate_sequence_parser(&itemname, seqitem));
                itemnames.push(itemname);
            }
            _ => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                let itemparser = generate_item_parser(&sitem.item_type);
                itemparsers.push(quote!{let #itemname = #itemparser?;});
                itemnames.push(itemname);
            }
        }
    }

    quote! {
        impl #name {
            fn parse(parser: &mut a2lfile::ParserState, context: &a2lfile::ParseContext) -> Result<Self, a2lfile::ParseError> {
                let fileid = context.fileid;
                let line = context.line;
                #(#itemparsers)*
                let blk = Self {
                    fileid,
                    line,
                    #(#itemnames),*
                };
                
                if context.inside_block {
                    parser.expect_token(context, a2lfile::A2lTokenType::End)?;
                    let ident = parser.get_identifier(context)?;
                    if ident != context.element {
                        parser.error_or_log(a2lfile::ParseError::IncorrectEndTag(context.clone(), ident))?;
                    }
                }

                Ok(blk)
            }
        }
    }
}


fn generate_item_parser(item: &A2mlType) ->proc_macro2::TokenStream {
    match item {
        A2mlType::Char => { quote!{parser.get_integer_i8(context)} }
        A2mlType::Int => { quote!{parser.get_integer_i16(context)} }
        A2mlType::Long => { quote!{parser.get_integer_i32(context)} }
        A2mlType::Uchar => { quote!{parser.get_integer_u8(context)} }
        A2mlType::Uint => { quote!{parser.get_integer_u16(context)} }
        A2mlType::Ulong => { quote!{parser.get_integer_u32(context)} }
        A2mlType::Double => { quote!{parser.get_double(context)} }
        A2mlType::Float => { quote!{parser.get_float(context)} }
        A2mlType::Array(arraytype, dim) => {
            if let A2mlType::Char = **arraytype {
                quote!{parser.get_string(context)}
            } else {
                let itemparser = generate_item_parser(arraytype);
                let parsercalls = (0..(*dim)).into_iter().map(|_| quote!{#itemparser?});
                // this looks complicated, but is actually the simplest solution I could come up with
                // The intent is to propagate the Err from any singe array element. Since this is a code
                // fragment that will be inserted elsewhere, using the '?' operator directly does not work as intended.
                // Wrapping it all in the closure and calling that immediately works.
                quote!{
                    {
                        |parser: &mut a2lfile::ParserState, context: &a2lfile::ParseContext| { Ok([ #(#parsercalls),*]) }
                    }(parser, context)
                }
            }
        }
        A2mlType::EnumRef(refname) => {
            let typename = format_ident!("{}", refname);
            quote!{#typename::parse(parser, context)}
        }
        A2mlType::StructRef(refname) => {
            let typename = format_ident!("{}", refname);
            quote!{#typename::parse(parser, context)}
        }
        _ => { panic!("forbidden type: {:#?}", item); }
    }
}


fn generate_sequence_parser(itemname: &proc_macro2::Ident, seqitem: &A2mlType) -> proc_macro2::TokenStream {
    let parserfunc = generate_item_parser(seqitem);
    quote!{
        let mut #itemname = Vec::new();
        let mut done = false;
        while done == false {
            let current_token = parser.get_tokenpos();
            let sequence_item = #parserfunc;
            if sequence_item.is_err() {
                parser.set_tokenpos(current_token);
                done = true;
            }
            else {
                #itemname.push(sequence_item?);
            }
        }
    }
}


fn generate_taggeditem_parser(tg_items: &Vec<TaggedItem>, is_taggedunion: bool) -> proc_macro2::TokenStream {
    let mut result = quote!{};
    let mut item_match_arms = Vec::new();
    for item in tg_items {
        let itemname = format_ident!("{}", make_varname(&item.tag));
        let typename = generate_bare_typename(&item.item.item_type);
        let store_item;
        if item.repeat {
            result.extend(quote!{let mut #itemname: Vec<#typename> = Vec::new();});
            store_item = quote!{
                #itemname.push(newitem);
            };
        } else {
            result.extend(quote!{let mut #itemname: Option<#typename> = None;});
            store_item = quote!{
                if #itemname.is_none() {
                    #itemname = Some(newitem);
                } else {
                    parser.error_or_log(a2lfile::ParseError::InvalidMultiplicityTooMany(context.clone(), tag.clone()))?;
                }
            };
        }

        let mut is_block_item = false;
        if let A2mlType::Block(_, _) = item.item.item_type {
            is_block_item = true;
        }

        let keyword = &item.tag;
        item_match_arms.push(
            quote!{
                #keyword => {
                    let newitem = #typename::parse(parser, &newcontext)?;
                    #store_item
                    if (#is_block_item != is_block) {
                        parser.error_or_log(a2lfile::ParseError::IncorrectElemType(context.clone(), #keyword.to_string(), #is_block_item))?;
                    }
                }
            }
        );

    }

    let parser_core = quote!{
        let (tag, is_block) = tag_peek.unwrap();
        let token = parser.get_token(context)?;
        let newcontext = a2lfile::ParseContext::from_token(token, is_block);
        match &*tag {
            #(#item_match_arms)*
            _ => {
                if is_block {
                    parser.undo_get_token();
                }
                parser.undo_get_token();
                break;
            }
        }
    };

    if is_taggedunion {
        result.extend(quote!{
            let mut tag_peek = parser.peek_next_tag(context)?;
            while tag_peek.is_some() { // must use while instead of if here, because the match arms of parser_core can use break
                #parser_core
                tag_peek = None; // never loop
            }
        });
    } else {
        result.extend(quote!{
            let mut tag_peek = parser.peek_next_tag(context)?;
            while tag_peek.is_some() {
                #parser_core
                tag_peek = parser.peek_next_tag(context)?;
            }
        });
    }

    result
}



fn generate_tagged_item_names(tg_items: &Vec<TaggedItem>) -> Vec<proc_macro2::Ident> {
    let mut names = Vec::new();

    for item in tg_items {
        names.push(format_ident!("{}", make_varname(&item.tag)));
    }

    names
}


//-----------------------------------------------------------------------------


// manual implementation of PartialEq to ignore comments when comparing for equality
impl PartialEq for DataItem {
    fn eq(&self, other: &Self) -> bool {
        self.typename == other.typename && self.item_type == other.item_type && self.varname == other.varname
    }
}
