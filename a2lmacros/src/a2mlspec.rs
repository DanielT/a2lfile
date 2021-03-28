use std::{collections::{HashMap, HashSet}, fmt::Write};

use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use proc_macro2::Delimiter;
use proc_macro2::Ident;
use quote::quote;
use quote::format_ident;
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
    basetype: BaseType,
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
enum BaseType {
    None,
    Char,
    Int,
    Long,
    Uchar,
    Uint,
    Ulong,
    Double,
    Float,
    Array(Box<DataItem>, usize),
    Sequence(Box<BaseType>),
    Enum(Vec<EnumItem>),
    EnumRef,
    Struct(Vec<DataItem>),
    StructRef,
    TaggedUnion(Vec<TaggedItem>),
    TaggedUnionRef,
    TaggedStruct(Vec<TaggedItem>),
    TaggedStructRef,
    Block(String, Box<DataItem>)
}


#[derive(Debug)]
struct A2mlTypeList {
    list: Vec<(String, BaseType)>
}


/// a2ml_specification - entry point for the proc_macro a2ml_specification!
/// the function processes the provided specification and generates appropriate data structures and parser functions from it
pub(crate) fn a2ml_specification(tokens: TokenStream) -> TokenStream {
    let mut iter: TokenStreamIter = tokens.into_iter().peekable();
    let spec = parse_specification(&mut iter);

    let mut result = quote!{};
    result.extend(generate_a2ml_constant(&spec));
    let outtypes = fixup_output_datatypes(&spec);

    result.extend(generate_data_structures(&outtypes));

    result.extend(generate_parser(&outtypes));

    result
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


/// parse_specification - parses the tokens from the compiler into a data structure
/// the format of the specification is basically A2ML.
/// There are two extensions to the A2ML grammar: 
/// Firstly, all variables can be named, e.g.
///     uint some_name;
/// instead of just
///     uint;
/// Secondly, comments starting with /// are preserved and copied into the resulting code
fn parse_specification(token_iter: &mut TokenStreamIter) -> A2mlSpec {
    // the specifiaction must begin with <SomeName> in order to name the top-level type that will be generated
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
// returns a type name in addition to the BaseType; this matters for enums, structs, etc.
fn parse_a2ml_type(token_iter: &mut TokenStreamIter, types: &A2mlTypeList, tok_start: &str) -> (Option<String>, BaseType) {
    match tok_start {
        "block" => {
            let (tag, item) = parse_a2ml_block(token_iter, &types);
            (Some(tag), item)
        }
        "char" => (None, BaseType::Char),
        "int" => (None, BaseType::Int),
        "long" => (None, BaseType::Long),
        "uchar" => (None, BaseType::Uchar),
        "uint" => (None, BaseType::Uint),
        "ulong" => (None, BaseType::Ulong),
        "float" => (None, BaseType::Float),
        "double" => (None, BaseType::Double),
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
fn parse_a2ml_block(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (String, BaseType) {
    let tag = get_string(token_iter);

    let mut has_repetition = false;
    let mut block_token_iter = token_iter;
    let mut block_token_iter_base;
    let block_tokens;
    // check if this block has a repeating data element
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
        btype = BaseType::Sequence(Box::new(btype))
    }

    let dataitem = DataItem {
        typename,
        basetype: btype,
        varname,
        comment: None
    };
    let blocktype = BaseType::Block(tag.clone(), Box::new(dataitem));

    (tag, blocktype)
}


// parse_a2ml_type_enum()
// Parses enum definitions according to the grammar:
//    enum_type_name = "enum" [ identifier ] "{" enumerator_list "}" | "enum" identifier
//    enumerator_list = enumerator | enumerator "," enumerator_list
//    enumerator = keyword [ "=" constant ]
//
// If the short form "enum identifier;" is found, then the type is looked up in the hashmap of previously defined enums
// If not, a new enum definition is expected
fn parse_a2ml_type_enum(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, BaseType) {
    let name: Option<String> = parse_optional_name(token_iter);

    if let Some(TokenTree::Group(_)) = token_iter.peek() {
        // parse the list of enum items
        let enum_tokens = get_group(token_iter, Delimiter::Brace);
        let enum_token_iter = &mut enum_tokens.into_iter().peekable();
        let mut enumvalues = Vec::new();
        while enum_token_iter.peek().is_some() {
            let tag = get_string(enum_token_iter);
            let mut value = None;
            // each enum item may specify an associated value e.g. "= 123"
            if let Some(TokenTree::Punct(p)) = enum_token_iter.peek() {
                if p.as_char() == '=' {
                    get_punct(enum_token_iter);
                    value = Some(get_integer(enum_token_iter));
                }
            }
            // a comma is required after every enum value except the last one
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
        (name, BaseType::Enum(enumvalues))
    } else {
        // there is no list of items, so this can only be a reference to a previous declaration
        if name.is_some() {
            let name = String::from(name.unwrap());
            let refenum = types.get_enum(&name);
            if refenum.is_none() {
                panic!("enum {} was referenced but not defined", name);
            }
            (Some(name), BaseType::EnumRef)
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
fn parse_a2ml_type_struct(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, BaseType) {
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
            structdata.push(DataItem {typename: item_typename, basetype: struct_item, varname: item_varname, comment});
        }

        (struct_typename, BaseType::Struct(structdata))
    } else {
        // no definition of the struct, so it must be a reference
        if struct_typename.is_some() {
            let name = String::from(struct_typename.unwrap());
            let refstruct = types.get_struct(&name);
            if refstruct.is_none() {
                panic!("struct {} was referenced but not defined", name);
            }
            (Some(name), BaseType::StructRef)
        } else {
            panic!("expected either an identifier or an opening bracket after keyword struct.");
        }
    }
}


// parse_a2ml_type_taggedstruct()
// Parses taggedstructs according to the grammar:
//    taggedstruct_type_name = "taggedstruct" [ identifier ] "{" [taggedstruct_member_list ] "}" | "taggedstruct" identifier
//    taggedstruct_member_list = taggedstruct_member | taggedstruct_member taggedstruct_member_list
fn parse_a2ml_type_taggedstruct(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, BaseType) {
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

        (typename, BaseType::TaggedStruct(ts_members))
    } else {
        // no definition of the taggedstruct, so it must be a reference
        if typename.is_some() {
            let name = String::from(typename.unwrap());
            let refts = types.get_taggedstruct(&name);
            if refts.is_none() {
                panic!("taggedstruct {} was referenced but not defined", name);
            }
            (Some(name), BaseType::TaggedStructRef)
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
        Some(TokenTree::Ident(Ident {..})) => {
            let ident = get_ident(ts_item_iter);
            if &*ident == "block" {
                let (tag, taggedmember) = parse_a2ml_block(ts_item_iter, &types);
                TaggedItem {tag, item: DataItem {basetype: taggedmember, typename: None, varname: None, comment: None}, repeat: multi}
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
                dataitem = DataItem {typename: None, basetype: BaseType::None, varname: None, comment: None};
            } else if let Some(TokenTree::Punct(_)) = ts_item_iter.peek() {
                // case 2: not repeating, so ts_item_iter refers to some larger group of items.
                // In this case the taggeditem contains None if punctuation (i.e. ';') is seen
                dataitem = DataItem {typename: None, basetype: BaseType::None, varname: None, comment: None};
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


// parse_a2ml_tagged_def()
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
        member = BaseType::Sequence(Box::new(member));
    }

    DataItem { typename, basetype: member, varname, comment: None}
}


// parse_a2ml_type_taggedunion()
//    taggedunion_type_name = "taggedunion" [ identifier ] "{" [taggedunion_member_list ] "}" | "taggedunion" identifier
//    taggedunion_member_list = tagged_union_member | tagged_union_member taggedunion_member_list
fn parse_a2ml_type_taggedunion(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, BaseType) {
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
        (typename, BaseType::TaggedUnion(taggeduniondata))
    } else {
        // no definition of the taggedunion, so it must be a reference
        if typename.is_some() {
            let name = String::from(typename.unwrap());
            let reftu = types.get_taggedunion(&name);
            if reftu.is_none() {
                panic!("taggedunion {} was referenced but not defined", name);
            }
            (Some(name), BaseType::TaggedUnionRef)
        } else {
            panic!("expected either an identifier or an opening bracket after keyword taggedunion.");
        }
    }
}


// parse_a2ml_taggedunionmember()
//    tagged_union_member = tag [ member ] ";" | block_definition ";"
fn parse_a2ml_taggedunionmember(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> TaggedItem {
    match token_iter.peek() {
        Some(TokenTree::Ident(Ident {..})) => {
            let ident = get_ident(token_iter);
            if &*ident == "block" {
                let (tag, tuitem) = parse_a2ml_block(token_iter, &types);
                TaggedItem {tag, item: DataItem {basetype: tuitem, typename: None, varname: None, comment: None}, repeat: false}
            } else {
                panic!("Identifier {} is not allowed in the definition of a taggedunion", ident);
            }
        }
        Some(TokenTree::Literal(_)) => {
            let tag = get_string(token_iter);
            let dataitem = if let Some(TokenTree::Punct(_)) = token_iter.peek() {
                DataItem { typename: None, basetype: BaseType::None, varname: None, comment: None }
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


// parse_a2ml_member()
// Parse a member of some other data structure. Each member could potentially have an arbitrary number of array dimensions
//    member = type_name [ array_specifier ]
//    array_specifier = "[" constant "]" | "[" constant "]" array_specifier
fn parse_a2ml_member(token_iter: &mut TokenStreamIter, types: &A2mlTypeList) -> (Option<String>, BaseType, Option<String>) {
    let tok_start = get_ident(token_iter);
    let (mut typename, mut base_type) = parse_a2ml_type(token_iter, &types, &tok_start);
    let varname = parse_optional_name(token_iter);

    while let Some(TokenTree::Group(_)) = token_iter.peek() {
        let array_dim_tokens = get_group(token_iter, Delimiter::Bracket);
        let array_dim_iter = &mut array_dim_tokens.into_iter().peekable();
        let dim = get_integer(array_dim_iter);

        base_type = BaseType::Array(Box::new(DataItem { typename, basetype: base_type, varname: None, comment: None }), dim as usize);
        typename = None;
    }

    (typename, base_type, varname)
}


// parse_optional_name()
// In some cases a name is allowed optionally; this can be either a type name, or a variable name depending on the calling context.
fn parse_optional_name(token_iter: &mut TokenStreamIter) -> Option<String> {
    if let Some(TokenTree::Ident(_)) = token_iter.peek() {
        let ident = get_ident(token_iter);
        Some( ident.to_string() )
    } else {
        None
    }
}


// parse_optional_comment()
// check if there is a rust-style doc comment in the input and parse it
fn parse_optional_comment(token_iter: &mut TokenStreamIter) -> Option<String> {
    if let Some(TokenTree::Punct(_)) = token_iter.peek() {
        // comments beginning with "///"" in the input are converted to rust's doc-comment form: #[doc="comment string here"]
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


/// A2mlTypeList is a wrapper around Vec<(name, BaseType)>
/// It provides functions to get varios entries from the list.
/// Unlike a HashMap, it preserves the ordering of the elements, which is needed when generating the a2ml-constant
impl A2mlTypeList {
    fn get_item(&self, name: &str, kind: std::mem::Discriminant<BaseType>) -> Option<&BaseType> {
        let findresult = self.list.iter().find(|(itemname, item)| {
            itemname == name && kind == std::mem::discriminant(item)
        });

        if let Some((_, item)) = findresult {
            Some(item)
        } else {
            None
        }
    }

    fn get_enum(&self, name: &str) -> Option<&BaseType> {
        self.get_item(name, std::mem::discriminant(&BaseType::Enum(Vec::<EnumItem>::new())))
    }

    fn get_struct(&self, name: &str) -> Option<&BaseType> {
        self.get_item(name, std::mem::discriminant(&BaseType::Struct(Vec::<DataItem>::new())))
    }

    fn get_taggedstruct(&self, name: &str) -> Option<&BaseType> {
        self.get_item(name, std::mem::discriminant(&BaseType::TaggedStruct(Vec::<TaggedItem>::new())))
    }

    fn get_taggedunion(&self, name: &str) -> Option<&BaseType> {
        self.get_item(name, std::mem::discriminant(&BaseType::TaggedUnion(Vec::<TaggedItem>::new())))
    }

    fn get_block(&self, name: &str) -> Option<&BaseType> {
        self.get_item(name, std::mem::discriminant(&BaseType::Block(name.to_string(), Box::new(DataItem{typename:None, basetype:BaseType::None, varname:None, comment:None}))))
    }
}


//-----------------------------------------------------------------------------

// Generate a constant containing the a2ml specification as a text string, e.g.
// const SECIFICATION_NAME_TEXT: &str = " ... "
//
// This constant is neede in order to be able to implement the update_a2ml() function, which
// updates the A2ML block of the loaded a2l file


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


fn generate_a2ml_constant_of_item(outstring: &mut String, typename: &Option<String>, item: &BaseType, indent_level: usize, indent_first: bool) {

    if indent_first {
        write!(outstring, "{:1$}", "", (indent_level * A2ML_INDENT_WIDTH)).unwrap();
    }

    match item {
        BaseType::None => { }
        BaseType::Char => { write!(outstring, "char").unwrap(); }
        BaseType::Int => { write!(outstring, "int").unwrap(); }
        BaseType::Long => { write!(outstring, "long").unwrap(); }
        BaseType::Uchar => { write!(outstring, "uchar").unwrap(); }
        BaseType::Uint => { write!(outstring, "uint").unwrap(); }
        BaseType::Ulong => { write!(outstring, "ulong").unwrap(); }
        BaseType::Double => { write!(outstring, "double").unwrap(); }
        BaseType::Float => { write!(outstring, "float").unwrap(); }
        BaseType::Array(arrayitem, dim) => {
            generate_a2ml_constant_of_item(outstring, typename, &arrayitem.basetype, indent_level, false);
            write!(outstring, "[{}]", dim).unwrap();
        }
        BaseType::Sequence(seqtype) => {
            write!(outstring, "(").unwrap();
            generate_a2ml_constant_of_item(outstring, typename, &**seqtype, indent_level, false);
            write!(outstring, ")*").unwrap();
        }
        BaseType::Enum(enumdef) => {
            generate_a2ml_constant_of_enum(outstring, typename, enumdef, indent_level);
        }
        BaseType::EnumRef => {
            let typename = typename.as_ref().unwrap();
            write!(outstring, "enum {}", typename).unwrap();
        }
        BaseType::Struct(structdef) => {
            generate_a2ml_constant_of_struct(outstring, typename, structdef, indent_level);
        }
        BaseType::StructRef => {
            let typename = typename.as_ref().unwrap();
            write!(outstring, "struct {}", typename).unwrap();
        }
        BaseType::TaggedUnion(tudef) => {
            generate_a2ml_constant_of_taggedunion(outstring, typename, tudef, indent_level);
        }
        BaseType::TaggedUnionRef => {
            let typename = typename.as_ref().unwrap();
            write!(outstring, "taggedunion {}", typename).unwrap();
        }
        BaseType::TaggedStruct(tsdef) => {
            generate_a2ml_constant_of_taggedstruct(outstring, typename, tsdef, indent_level);
        }
        BaseType::TaggedStructRef => {
            let typename = typename.as_ref().unwrap();
            write!(outstring, "taggedstruct {}", typename).unwrap();
        }
        BaseType::Block(blkname, item) => {
            generate_a2ml_constant_of_block(outstring, blkname, item, indent_level);
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
        generate_a2ml_constant_of_item(outstring, &structitem.typename, &structitem.basetype, indent_level + 1, true);
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
        if let BaseType::Block(_,_) = tuitem.item.basetype {
            generate_a2ml_constant_of_item(outstring, &tuitem.item.typename, &tuitem.item.basetype, indent_level + 1, true);
        } else {
            write!(outstring, "{:2$}\"{}\" ", "", tuitem.tag, (indent_level+1) * A2ML_INDENT_WIDTH).unwrap();
            generate_a2ml_constant_of_item(outstring, &tuitem.item.typename, &tuitem.item.basetype, indent_level + 1, false);
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
        if let BaseType::Block(_,_) = tsitem.item.basetype {
            generate_a2ml_constant_of_item(outstring, &tsitem.item.typename, &tsitem.item.basetype, indent_level + 1, false);
        } else {
            write!(outstring, "\"{}\" ", tsitem.tag).unwrap();
            generate_a2ml_constant_of_item(outstring, &tsitem.item.typename, &tsitem.item.basetype, indent_level + 1, false);
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


fn generate_a2ml_constant_of_block(outstring: &mut String, name: &String, blockitem: &DataItem, indent_level: usize) {
    write!(outstring, "block \"{}\" ", name).unwrap();
    generate_a2ml_constant_of_item(outstring, &blockitem.typename, &blockitem.basetype, indent_level, false);
}


//-----------------------------------------------------------------------------
//
// The datatypes derived from the input a2ml specification are not directly suited for code generation
// Problems taht need to be fixed:
// - items are not required to have names
// - there is nothing that prevents the spec from using one type name with different meanings in different places
// - a2ml is often excessivly complex, e.g. struct {a, b, struct {c, d}} can be flattened to struct {a, b, c, d}

fn fixup_output_datatypes(spec: &A2mlSpec) -> HashMap<String, BaseType> {
    if let Some(BaseType::Block(_, item)) = spec.types.get_block("IF_DATA") {
        let mut datatypes = HashMap::<String, BaseType>::new();
        fixup_output_block(spec, &spec.name, item, &mut datatypes);
        datatypes
    } else {
        panic!("the specification must define the IF_DATA block, which is the root of all defined elements");
    }
}


fn fixup_output_block(spec: &A2mlSpec, tag: &str, item: &DataItem, defined_types: &mut HashMap<String, BaseType>) -> String {
    let structname = ucname_to_typename(tag);
    let mut structitems = fixup_add_data_to_struct(spec, item, defined_types);
    fixup_make_varnames_unique(&mut structitems);

    let newstruct = BaseType::Struct(structitems);
    fixup_add_type(structname.clone(), newstruct, defined_types)
}


fn fixup_add_data_to_struct(spec: &A2mlSpec, item: &DataItem, defined_types: &mut HashMap<String, BaseType>) -> Vec<DataItem> {
    let mut new_structitems= Vec::<DataItem>::new();

    match &item.basetype {
        BaseType::Struct(structitems) => {
            for sitem in structitems {
                new_structitems.extend(fixup_add_data_to_struct(spec, sitem, defined_types));
            }
        }
        BaseType::StructRef => {
            let typename = item.typename.as_ref().unwrap();
            let refstruct = spec.types.get_struct(typename);
                if let Some(BaseType::Struct(structitems)) = refstruct {
                    for sitem in structitems {
                        new_structitems.extend(fixup_add_data_to_struct(spec, sitem, defined_types));
                    }
                } else {
                panic!("invalid struct reference {} in a2ml spec", typename);
            }
        }
        BaseType::None => {
            // items with the type None shouldn't be added to the output struct
        }
        _ => {
            let varname = make_itemname(&item.varname, &item.typename);
            let (new_typename, new_basetype) = fixup_data_type(spec, &item.typename, &item.basetype, &item.comment, defined_types);
            new_structitems.push(DataItem {
                typename: new_typename,
                basetype: new_basetype,
                varname: Some(varname),
                comment: item.comment.clone()
            });
        }
    }

    new_structitems
}


fn fixup_data_type(spec: &A2mlSpec, typename: &Option<String>, basetype: &BaseType, comment: &Option<String>, defined_types: &mut HashMap<String, BaseType>) -> (Option<String>, BaseType) {
    match basetype {
        BaseType::Array(arraytype, dim) => {
            let (newname, new_arraytype) = fixup_data_type(spec, &arraytype.typename, &arraytype.basetype, comment, defined_types);
            let new_basetype = BaseType::Array(Box::new(DataItem { typename: arraytype.typename.clone(), basetype: new_arraytype, varname: None, comment: None }), *dim);
            (newname, new_basetype)
        }
        BaseType::Sequence(seqtype) => {
            match &**seqtype {
                BaseType::Sequence(inner_seqtype) => {
                    // seqence(seqence(x)) makes no sense, flatten it to just sequence(x)
                    let (new_typename, new_basetype) = fixup_data_type(spec, typename, inner_seqtype, comment, defined_types);
                    (new_typename, BaseType::Sequence(Box::new(new_basetype)))
                }
                BaseType::TaggedStruct(_) |
                BaseType::TaggedStructRef => {
                    panic!("A repeating taggedstruct cannot be parsed properly, because there is no separator between each instance of the taggedstruct. Suggestion: use a taggdstruct with repeating elements instead.")
                }
                BaseType::TaggedUnion(_) |
                BaseType::TaggedUnionRef => {
                    panic!("A repeating taggedunion does not make sense. Use a taggedstruct instead")
                }
                _ => {
                    let (new_typename, new_basetype) = fixup_data_type(spec, typename, seqtype, comment, defined_types);
                    (new_typename, BaseType::Sequence(Box::new(new_basetype)))
                }
            }
        }
        BaseType::Enum(enumitems) => {
            let newenum = BaseType::Enum(enumitems.clone());
            let enumname = make_enum_name(typename, &enumitems);
            let new_enumname = fixup_add_type(enumname.clone(), newenum, defined_types);
            (Some(new_enumname), BaseType::EnumRef)
        }
        BaseType::EnumRef => {
            let typename = typename.as_ref().unwrap();
            if let Some(BaseType::Enum(enumitems)) = spec.types.get_enum(typename) {
                let newenum = BaseType::Enum(enumitems.clone());
                let new_enumname = fixup_add_type(typename.to_owned(), newenum, defined_types);
                (Some(new_enumname), BaseType::EnumRef)
            } else {
                panic!("failed to resolve enum reference {}", typename);
            }
        }
        BaseType::Struct(structitems) => {
            if let Some(name) = typename {
                fixup_struct(name.to_owned(), structitems, spec, defined_types)
            } else {
                panic!("unnamed struct {:#?} found in a position where no name can be derived from context.", basetype);
            }
        }
        BaseType::StructRef => {
            let typename = typename.as_ref().unwrap();
            if let Some(BaseType::Struct(structitems)) = spec.types.get_struct(typename) {
                fixup_struct(typename.to_owned(), structitems, spec, defined_types)
            } else {
                panic!("failed to resolve taggedunion reference {}", typename)
            }
        }
        BaseType::TaggedUnion(tuitems) => {
            (None, BaseType::TaggedUnion(fixup_taggeditems(tuitems, spec, defined_types)))
        }
        BaseType::TaggedUnionRef => {
            let typename = typename.as_ref().unwrap();
            if let Some(BaseType::TaggedUnion(tuitems)) = spec.types.get_taggedunion(typename) {
                (None, BaseType::TaggedUnion(fixup_taggeditems(tuitems, spec, defined_types)))
            } else {
                panic!("failed to resolve taggedunion reference {}", typename)
            }
        }
        BaseType::TaggedStruct(tsitems) => {
            (None, BaseType::TaggedStruct(fixup_taggeditems(tsitems, spec, defined_types)))
        }
        BaseType::TaggedStructRef => {
            let typename = typename.as_ref().unwrap();
            if let Some(BaseType::TaggedStruct(tsitems)) = spec.types.get_taggedstruct(typename) {
                (None, BaseType::TaggedStruct(fixup_taggeditems(tsitems, spec, defined_types)))
            } else {
                panic!("failed to resolve taggedstruct reference {}", typename)
            }
        }
        BaseType::Block(_, _) => {
            panic!("Type block should never be seen in this context")
        }
        BaseType::None => (None, BaseType::None),
        BaseType::Char => (None, BaseType::Char),
        BaseType::Int => (None, BaseType::Int),
        BaseType::Long => (None, BaseType::Long),
        BaseType::Uchar => (None, BaseType::Uchar),
        BaseType::Uint => (None, BaseType::Uint),
        BaseType::Ulong => (None, BaseType::Ulong),
        BaseType::Double => (None, BaseType::Double),
        BaseType::Float => (None, BaseType::Float),
    }
}


fn fixup_struct(structname: String, structitems: &Vec<DataItem>, spec: &A2mlSpec, defined_types: &mut HashMap<String, BaseType>) -> (Option<String>, BaseType) {
    let mut new_structitems = Vec::<DataItem>::new();
    for item in structitems {
        new_structitems.extend(fixup_add_data_to_struct(spec, &item, defined_types));
    }
    fixup_make_varnames_unique(&mut new_structitems);

    let new_structname = fixup_add_type(structname, BaseType::Struct(new_structitems), defined_types);

    (Some(new_structname), BaseType::StructRef)
}


fn fixup_taggeditems(taggeditems: &Vec<TaggedItem>, spec: &A2mlSpec, defined_types: &mut HashMap<String, BaseType>) -> Vec<TaggedItem> {
    let mut new_tuitems = Vec::<TaggedItem>::new();
    for tgitem in taggeditems {
        if let BaseType::Block(tag, item) = &tgitem.item.basetype {
            let new_typename = fixup_output_block(spec, tag, item, defined_types);
            let newblock = BaseType::Block(tag.clone(), Box::new(
                DataItem {
                    typename: Some(new_typename.clone()),
                    basetype: BaseType::StructRef,
                    varname: None,
                    comment: None
                }
            ));
            new_tuitems.push(TaggedItem {
                tag: tgitem.tag.clone(),
                item: DataItem { typename: None, basetype: newblock, varname: None, comment: None },
                repeat: tgitem.repeat
            });
        } else {
            let new_typename = fixup_output_block(spec, &tgitem.tag, &tgitem.item, defined_types);
            new_tuitems.push(TaggedItem {
                tag: tgitem.tag.clone(),
                item: DataItem { typename: Some(new_typename.clone()), basetype: BaseType::StructRef, varname: None, comment: None },
                repeat: tgitem.repeat
            });
        }
    }

    new_tuitems
}


fn fixup_add_type(itemname: String, newitem: BaseType, defined_types: &mut HashMap<String, BaseType>) -> String {
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

fn generate_data_structures(types: &HashMap<String, BaseType>) -> proc_macro2::TokenStream {
    let mut result = quote!{};
    let mut typesvec: Vec<(&String, &BaseType)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());

    for (typename, a2mltype) in typesvec {
        match a2mltype {
            BaseType::Enum(enumitems) => {
                result.extend(generate_enum_data_structure(typename, enumitems));
            }
            BaseType::Struct(structitems) => {
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

    match &item.basetype {
        BaseType::None => { panic!("type None is not permitted for struct items"); }
        BaseType::Block(_, _) => { panic!("type Block is not permitted for struct items"); }
        BaseType::Enum(_) => { panic!("type Enum is not permitted at this point and should have been transformed to an EnumRef"); }
        BaseType::Struct(_) => { panic!("type Struct is not permitted at this point and should have been transformed to a StructRef"); }
        BaseType::TaggedUnionRef => { panic!("TaggedUnionRef should have been resolved in the data structure fixup phase"); }
        BaseType::TaggedStructRef => { panic!("TaggedStructRef should have been resolved in the data structure fixup phase"); }
        BaseType::TaggedUnion(tuitems) => {
            let mut tudefs = Vec::new();
            for tuitem in tuitems {
                let mut curr_def = quote!{};
                if let Some(comment) = &tuitem.item.comment {
                    curr_def.extend(quote!{#[doc=#comment]});
                }
                let tuitemname = format_ident!("{}", make_varname(&tuitem.tag));
                let typename = generate_bare_typename(&tuitem.item.typename, &tuitem.item.basetype);
                curr_def.extend(quote!{#tuitemname: Option<#typename>});

                tudefs.push(curr_def);
            }
            def.extend(quote!{#(#tudefs),*});
        }
        BaseType::TaggedStruct(tsitems) => {
            let mut tsdefs = Vec::new();
            for tsitem in tsitems {
                let mut curr_def = quote!{};
                if let Some(comment) = &tsitem.item.comment {
                    curr_def.extend(quote!{#[doc=#comment]});
                }
                let tuitemname = format_ident!("{}", make_varname(&tsitem.tag));
                let typename = generate_bare_typename(&tsitem.item.typename, &tsitem.item.basetype);
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
            let typename = generate_bare_typename(&item.typename, &item.basetype);
            def.extend(quote!{#itemname: #typename});
        }
    }
    def
}


fn generate_bare_typename(typename: &Option<String>, item: &BaseType) -> proc_macro2::TokenStream {
    match item {
        BaseType::Char => { quote!{i8} }
        BaseType::Int => { quote!{i16} }
        BaseType::Long => { quote!{i32} }
        BaseType::Uchar => { quote!{u8} }
        BaseType::Uint => { quote!{u16} }
        BaseType::Ulong => { quote!{u32} }
        BaseType::Double => { quote!{f64} }
        BaseType::Float => { quote!{f32} }
        BaseType::Array(arraytype, dim) => {
            // a2ml specifies strings like C does: as arrays of char
            // if this pattern is found, then represent it as a String in Rust
            if arraytype.basetype == BaseType::Char {
                quote!{String}
            } else {
                let name = generate_bare_typename(&arraytype.typename, &arraytype.basetype);
                quote!{[#name; #dim]}
            }
        }
        BaseType::Sequence(seqtype) => {
            let name = generate_bare_typename(typename, seqtype);
            quote!{Vec<#name>}
        }
        BaseType::EnumRef => {
            let typename = typename.as_ref().unwrap();
            let name = format_ident!("{}", typename);
            quote!{#name}
        }
        BaseType::StructRef => {
            let typename = typename.as_ref().unwrap();
            let name = format_ident!("{}", typename);
            quote!{#name}
        }
        BaseType::Block(_, item) => {
            let name = generate_bare_typename(&item.typename, &item.basetype);
            quote!{#name}
        }
        _ => {
            // None is not allowed at all once generation begins (previously it is a placeholder that is dropped during the fixup phase)
            // Enum, Struct, TaggedUnionRef, TaggedStructRef have been transformed in the fixup phase and can't occur here
            panic!("impossible type - unreachable");
        }
    }
}



//-----------------------------------------------------------------------------


fn generate_parser(types: &HashMap<String, BaseType>) -> proc_macro2::TokenStream {
    let mut result = quote!{};
    let mut typesvec: Vec<(&String, &BaseType)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());

    for (typename, a2mltype) in typesvec {
        match a2mltype {
            BaseType::Enum(enumitems) => {
                result.extend(generate_enum_parser(typename, enumitems));
            }
            BaseType::Struct(structitems) => {
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
        match &sitem.basetype {
            BaseType::TaggedStruct(tg_items) => {
                itemparsers.push(generate_taggeditem_parser(tg_items, false));
                itemnames.extend(generate_tagged_item_names(tg_items));
            }
            BaseType::TaggedUnion(tg_items) => {
                itemparsers.push(generate_taggeditem_parser(tg_items, true));
                itemnames.extend(generate_tagged_item_names(tg_items));
            }
            BaseType::Sequence(seqitem) => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                itemparsers.push(generate_sequence_parser(&itemname, &sitem.typename, seqitem));
                itemnames.push(itemname);
            }
            _ => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                let itemparser = generate_item_parser(&sitem.typename, &sitem.basetype);
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


fn generate_item_parser(typename: &Option<String>, item: &BaseType) ->proc_macro2::TokenStream {
    match item {
        BaseType::Char => { quote!{parser.get_integer_i8(context)} }
        BaseType::Int => { quote!{parser.get_integer_i16(context)} }
        BaseType::Long => { quote!{parser.get_integer_i32(context)} }
        BaseType::Uchar => { quote!{parser.get_integer_u8(context)} }
        BaseType::Uint => { quote!{parser.get_integer_u16(context)} }
        BaseType::Ulong => { quote!{parser.get_integer_u32(context)} }
        BaseType::Double => { quote!{parser.get_double(context)} }
        BaseType::Float => { quote!{parser.get_float(context)} }
        BaseType::Array(arraytype, dim) => {
            if let BaseType::Char = arraytype.basetype {
                quote!{parser.get_string(context)}
            } else {
                let itemparser = generate_item_parser(&arraytype.typename, &arraytype.basetype);
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
        BaseType::EnumRef => {
            let typename = typename.as_ref().unwrap();
            let name = format_ident!("{}", typename);
            quote!{#name::parse(parser, context)}
        }
        BaseType::StructRef => {
            let typename = typename.as_ref().unwrap();
            let name = format_ident!("{}", typename);
            quote!{#name::parse(parser, context)}
        }
        _ => { panic!("forbidden type: {:#?}", item); }
    }
}


fn generate_sequence_parser(itemname: &proc_macro2::Ident, typename: &Option<String>, seqitem: &BaseType) -> proc_macro2::TokenStream {
    let parserfunc = generate_item_parser(typename, seqitem);
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
        let typename = generate_bare_typename(&item.item.typename, &item.item.basetype);
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
        if let BaseType::Block(_, _) = item.item.basetype {
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
        self.typename == other.typename && self.basetype == other.basetype && self.varname == other.varname
    }
}




#[cfg(test)]
mod test {
    use crate::a2mlspec::*;

    fn get_test_tokenstream() -> TokenStream {
        let result = quote::quote!{
            <Testspec>

            struct SomeStruct {
                uchar my_uchar; /// comment 1
                uint my_uint;  /// comment 2
                ulong my_ulong;  /// comment 3
                char my_char;  /// comment 4
                int my_int;  /// comment 5
                long my_long;  /// comment 6
                float my_float;  /// comment 7
                double my_double;  /// comment 8
                uint my_array[3];  /// comment 9
                enum {
                    "ANON_ENUM_A" = 0,
                    "ANUN_ENUM_B" = 1
                };
                enum SomeEnum {
                    "SOME_ENUM_A" = 1,
                    "SOME_ENUM_B" = 2,
                } my_enum; // comment 10
                taggedunion {
                    "FOO" uint ;
                    "BAR" uchar ;
                };
                taggedstruct {
                    ("REP_ITEM" uint rep_item)*;
                    "NORMAL_ITEM" struct {
                        char my_string[128];
                    };
                };
            };

            block "IF_DATA" struct SomeStruct;

        };
        result
    }

    #[test]
    fn parse_a2mlspec() {
        let ts = get_test_tokenstream();
        let mut iter: TokenStreamIter = ts.into_iter().peekable();
        let _spec = parse_specification(&mut iter);
    }

    #[test]
    fn make_a2mlconst() {
        let ts = get_test_tokenstream();
        let mut iter: TokenStreamIter = ts.into_iter().peekable();
        let spec = parse_specification(&mut iter);
        let _const_tokens = generate_a2ml_constant(&spec);
    }

    #[test]
    fn fixup_data_structures() {
        let ts = get_test_tokenstream();
        let mut iter: TokenStreamIter = ts.into_iter().peekable();
        let spec = parse_specification(&mut iter);
        let _outtypes = fixup_output_datatypes(&spec);
    }

    #[test]
    fn generate_output_structures() {
        let ts = get_test_tokenstream();
        let mut iter: TokenStreamIter = ts.into_iter().peekable();
        let spec = parse_specification(&mut iter);
        let outtypes = fixup_output_datatypes(&spec);
        generate_data_structures(&outtypes);
    }

    #[test]
    fn generate_output_parsers() {
        let ts = get_test_tokenstream();
        let mut iter: TokenStreamIter = ts.into_iter().peekable();
        let spec = parse_specification(&mut iter);
        let outtypes = fixup_output_datatypes(&spec);
        generate_data_structures(&outtypes);
        generate_parser(&outtypes);
    }

}