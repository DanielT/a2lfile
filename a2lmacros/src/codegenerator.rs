use std::collections::HashMap;
use proc_macro2::TokenStream;
use proc_macro2::Ident;
use quote::format_ident;
use quote::quote;

use super::util::*;


#[derive(Debug, PartialEq, Clone)]
pub(crate) struct EnumItem {
    pub(crate) name: String,
    pub(crate) value: Option<i32>,
    pub(crate) comment: Option<String>
}

#[derive(Debug)]
pub(crate) struct DataItem {
    pub(crate) typename: Option<String>,
    pub(crate) basetype: BaseType,
    pub(crate) varname: Option<String>,
    pub(crate) comment: Option<String>
}


#[derive(Debug, PartialEq)]
pub(crate) struct TaggedItem {
    pub(crate) tag: String,
    pub(crate) item: DataItem,
    pub(crate) is_block: bool,
    pub(crate) repeat: bool,
    pub(crate) required: bool
}


#[derive(Debug, PartialEq)]
pub(crate) enum BaseType {
    None,
    Char,
    Int,
    Long,
    Uchar,
    Uint,
    Ulong,
    Double,
    Float,
    Ident,  // text without double quotes, obeying the rules for identifiers (no spaces, etc)
    String, // text inside double qoutes
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
    Block(Vec<DataItem>)
}



pub(crate) fn generate_data_structures(types: &HashMap<String, BaseType>) -> TokenStream {
    let mut result = quote!{};

    // Convert the hashmap of types to a vec and sort it. This gives stable output ordering
    let mut typesvec: Vec<(&String, &BaseType)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());

    // generate each type in the list
    for (typename, a2mltype) in typesvec {
        match a2mltype {
            BaseType::Enum(enumitems) => {
                result.extend(generate_enum_data_structure(typename, enumitems));
            }
            BaseType::Struct(structitems) => {
                result.extend(generate_struct_data_structure(typename, structitems));
            }
            BaseType::Block(structitems) => {
                result.extend(generate_block_data_structure(typename, structitems));
            }
            _ => {
                panic!("only block, struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, a2mltype);
            }
        }
    }

    result
}


fn generate_enum_data_structure(typename: &str, enumitems: &Vec<EnumItem>) -> TokenStream {
    let typeident = format_ident!("{}", typename);

    let enumidents: Vec<proc_macro2::Ident> = enumitems.iter().map(
        |enumitem| { format_ident!("{}", ucname_to_typename(&enumitem.name)) }
    ).collect();

    quote!{
        #[derive(Debug, PartialEq, Clone)]
        pub enum #typeident {
            #(#enumidents),*
        }
    }
}


fn generate_struct_data_structure(typename: &str, structitems: &Vec<DataItem>) ->  TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut definitions = Vec::new();

    for item in structitems {
        definitions.push(generate_struct_item_definition(item));
    }

    quote!{
        #[derive(Debug)]
        pub struct #typeident {
            #(#definitions),*
        }
    }
}


fn generate_block_data_structure(typename: &str, structitems: &Vec<DataItem>) ->  TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut definitions = Vec::new();

    for item in structitems {
        definitions.push(generate_struct_item_definition(item));
    }

    quote!{
        #[derive(Debug)]
        pub struct #typeident {
            pub fileid: usize,
            pub line: u32,

            #(#definitions),*
        }
    }
}


fn generate_struct_item_definition(item: &DataItem) -> TokenStream {
    let mut def = quote!{};

    if let Some(comment) = &item.comment {
        def.extend(quote!{#[doc=#comment]});
    }

    match &item.basetype {
        BaseType::None => { panic!("type None is not permitted for struct items"); }
        BaseType::Enum(_) => { panic!("type Enum is not permitted at this point and should have been transformed to an EnumRef"); }
        BaseType::Struct(_) => { panic!("type Struct is not permitted at this point and should have been transformed to a StructRef"); }
        BaseType::TaggedUnionRef => { panic!("TaggedUnionRef should have been resolved in the data structure fixup phase"); }
        BaseType::TaggedStructRef => { panic!("TaggedStructRef should have been resolved in the data structure fixup phase"); }
        BaseType::TaggedUnion(tgitems) |
        BaseType::TaggedStruct(tgitems) => {
            let mut tgdefs = Vec::new();
            for tgitem in tgitems {
                let mut curr_def = quote!{};
                if let Some(comment) = &tgitem.item.comment {
                    curr_def.extend(quote!{#[doc=#comment]});
                }
                let tuitemname = format_ident!("{}", make_varname(&tgitem.tag));
                let typename = generate_bare_typename(&tgitem.item.typename, &tgitem.item.basetype);
                if tgitem.repeat {
                    curr_def.extend(quote!{pub #tuitemname: Vec<#typename>});
                } else {
                    if tgitem.required {
                        curr_def.extend(quote!{pub #tuitemname: #typename});
                    } else {
                        curr_def.extend(quote!{pub #tuitemname: Option<#typename>});
                    }
                }

                tgdefs.push(curr_def);
            }
            def.extend(quote!{#(#tgdefs),*});
        }
        _ => {
            if item.varname.is_none() {
                panic!("bad varname of {:#?}", item);
            }
            let itemname = format_ident!("{}", item.varname.to_owned().unwrap());
            let typename = generate_bare_typename(&item.typename, &item.basetype);
            def.extend(quote!{pub #itemname: #typename});
        }
    }
    def
}


fn generate_bare_typename(typename: &Option<String>, item: &BaseType) -> TokenStream {
    match item {
        BaseType::Char => { quote!{i8} }
        BaseType::Int => { quote!{i16} }
        BaseType::Long => { quote!{i32} }
        BaseType::Uchar => { quote!{u8} }
        BaseType::Uint => { quote!{u16} }
        BaseType::Ulong => { quote!{u32} }
        BaseType::Double => { quote!{f64} }
        BaseType::Float => { quote!{f32} }
        BaseType::Ident => { quote!{String} }
        BaseType::String => { quote!{String} }
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
        _ => {
            // None is not allowed at all once generation begins (previously it is a placeholder that is dropped during the fixup phase)
            // Enum, Struct, TaggedUnionRef, TaggedStructRef have been transformed in the fixup phase and can't occur here
            panic!("impossible type - unreachable");
        }
    }
}



//-----------------------------------------------------------------------------

// generate_parser
// entry point for externe use (by users of the a2ml_specification! macro) of the parser generator
// functions will be prefixed with "a2lfile::"
pub(crate) fn generate_parser(types: &HashMap<String, BaseType>) -> TokenStream {
    let cratename = format_ident!("a2lfile");
    generate_parser_impl(&cratename, types)
}


// generate_parser_internal
// entry point for internal use (within the crate) of the parser generator
// functions will be prefixed with "crate::"
pub(crate) fn generate_parser_internal(types: &HashMap<String, BaseType>) -> TokenStream {
    let cratename = format_ident!("crate");
    generate_parser_impl(&cratename, types)
}


// generate_parser_impl
// generate a full set of parser function implementations for the set of types
fn generate_parser_impl(cratename: &Ident, types: &HashMap<String, BaseType>) -> TokenStream {
    let mut result = quote!{};
    let mut typesvec: Vec<(&String, &BaseType)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.partial_cmp(b.0).unwrap());

    for (typename, a2mltype) in typesvec {
        match a2mltype {
            BaseType::Enum(enumitems) => {
                result.extend(generate_enum_parser(cratename, typename, enumitems));
            }
            BaseType::Struct(structitems) => {
                result.extend(generate_struct_parser(cratename, typename, structitems));
            }
            BaseType::Block(structitems) => {
                result.extend(generate_block_parser(cratename, typename, structitems));
            }
            _ => {
                panic!("only block, struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, a2mltype);
            }
        }
    }
    result
}


// generate_enum_parser
// generates a parser function that returns the enum variant matching the text of the current input token
fn generate_enum_parser(cratename: &Ident, typename: &str, enumitems: &Vec<EnumItem>) -> TokenStream {
    let name = format_ident!("{}", typename);

    let mut match_branches = Vec::new();
    for enitem in enumitems {
        let enident = format_ident!("{}", ucname_to_typename(&enitem.name));
        let entag = &enitem.name;
        match_branches.push(quote!{#entag => Ok(Self::#enident),});
    }

    quote!{
        impl #name {
            fn parse(parser: &mut #cratename::ParserState, context: &#cratename::ParseContext) -> Result<Self, #cratename::ParseError> {
                let enumname = parser.get_identifier(context)?;
                match &*enumname {
                    #(#match_branches)*
                    _ => Err(#cratename::ParseError::InvalidEnumValue(context.copy(), enumname))
                }
            }
        }
    }
}


// generate_struct_parser
// generates the full parser function implementation for a struct.
// Most data structures are actually blocks; a struct is usually only seen as the wrapped type of a sequence
// e.g.
//    Sequence(Struct(strictitems))
// but
//    TaggedStruct( [ TaggedItem(Block(structitems)), ...])
fn generate_struct_parser(cratename: &Ident, typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    let name = format_ident!("{}", typename);

    let (itemnames, itemparsers) = generate_struct_item_fragments(structitems, cratename);

    quote! {
        impl #name {
            fn parse(parser: &mut #cratename::ParserState, context: &#cratename::ParseContext) -> Result<Self, #cratename::ParseError> {
                #(#itemparsers)*
                Ok(Self {
                    #(#itemnames),*
                })
            }
        }
    }
}


// generate_block_parser
// generates the full parser function for a block
// blocks are structs which occur after a tag in a TaggedUnion or TaggedStruc.
// This means that they also need the fileid and line elements, so that the position within the file can be preserved when writing the data structure
fn generate_block_parser(cratename: &Ident, typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    let name = format_ident!("{}", typename);

    let (itemnames, itemparsers) = generate_struct_item_fragments(structitems, cratename);

    quote! {
        impl #name {
            fn parse(parser: &mut #cratename::ParserState, context: &#cratename::ParseContext) -> Result<Self, #cratename::ParseError> {
                let fileid = context.fileid;
                let line = context.line;
                #(#itemparsers)*
                let blk = Self {
                    fileid,
                    line,
                    #(#itemnames),*
                };
                
                if context.inside_block {
                    parser.expect_token(context, #cratename::A2lTokenType::End)?;
                    let ident = parser.get_identifier(context)?;
                    if ident != context.element {
                        parser.error_or_log(#cratename::ParseError::IncorrectEndTag(context.clone(), ident))?;
                    }
                }

                Ok(blk)
            }
        }
    }
}


// generate_struct_item_fragments
// generate a list of struct elements as well as TokenStreams with code to parse these elements
fn generate_struct_item_fragments(structitems: &Vec<DataItem>, cratename: &Ident) -> (Vec<Ident>, Vec<TokenStream>) {
    let mut itemparsers = Vec::<TokenStream>::new();
    let mut itemnames = Vec::<Ident>::new();
    for sitem in structitems {
        match &sitem.basetype {
            BaseType::TaggedStruct(tg_items) => {
                itemparsers.push(generate_taggeditem_parser(cratename, tg_items, false));
                itemnames.extend(generate_tagged_item_names(tg_items));
            }
            BaseType::TaggedUnion(tg_items) => {
                itemparsers.push(generate_taggeditem_parser(cratename, tg_items, true));
                itemnames.extend(generate_tagged_item_names(tg_items));
            }
            BaseType::Sequence(seqitem) => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                itemparsers.push(generate_sequence_parser(cratename, &itemname, &sitem.typename, seqitem));
                itemnames.push(itemname);
            }
            _ => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                let itemparser = generate_item_parser_call(cratename, &sitem.typename, &sitem.basetype);
                itemparsers.push(quote!{let #itemname = #itemparser?;});
                itemnames.push(itemname);
            }
        }
    }

    (itemnames, itemparsers)
}


// generate_item_parser_call
// generates code to call an existing item parser function
fn generate_item_parser_call(cratename: &Ident, typename: &Option<String>, item: &BaseType) -> TokenStream {
    match item {
        BaseType::Char => { quote!{parser.get_integer_i8(context)} }
        BaseType::Int => { quote!{parser.get_integer_i16(context)} }
        BaseType::Long => { quote!{parser.get_integer_i32(context)} }
        BaseType::Uchar => { quote!{parser.get_integer_u8(context)} }
        BaseType::Uint => { quote!{parser.get_integer_u16(context)} }
        BaseType::Ulong => { quote!{parser.get_integer_u32(context)} }
        BaseType::Double => { quote!{parser.get_double(context)} }
        BaseType::Float => { quote!{parser.get_float(context)} }
        BaseType::Ident => { quote!{parser.get_identifier(context)} }
        BaseType::String => { quote!{parser.get_string(context)} }
        BaseType::Array(arraytype, dim) => {
            if let BaseType::Char = arraytype.basetype {
                quote!{parser.get_string_maxlen(context, #dim)}
            } else {
                let itemparser = generate_item_parser_call(cratename, &arraytype.typename, &arraytype.basetype);
                let parsercalls = (0..(*dim)).into_iter().map(|_| quote!{#itemparser?});
                // this looks complicated, but is actually the simplest solution I could come up with
                // The intent is to propagate the Err from any single array element. Since this is a code
                // fragment that will be inserted elsewhere, using the '?' operator directly does not work as intended.
                // Wrapping it all in the closure and calling that immediately works.
                quote!{
                    {
                        |parser: &mut #cratename::ParserState, context: &#cratename::ParseContext| { Ok([ #(#parsercalls),*]) }
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


// generate_sequence_parser
// Generates a TokenStream with code to greedily parse elements of a sequence
// Parsing of sequence items continues until the parser function for the current sequence item returns an error
fn generate_sequence_parser(cratename: &Ident, itemname: &Ident, typename: &Option<String>, seqitem: &BaseType) -> TokenStream {
    let parserfunc = generate_item_parser_call(cratename, typename, seqitem);
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


// generate_taggeditem_parser
// Generate a TokenStream representing code to parse all the tagged items of a TaggedStruct or TaggedUnion
fn generate_taggeditem_parser(cratename: &Ident, tg_items: &Vec<TaggedItem>, is_taggedunion: bool) -> TokenStream {
    // result: the TokenStream that ultimately collcts all the code fragements in this function
    let mut result = quote!{};
    // item_match_arms: the match arms of the while loop that passes each set of input tokens to the appropriate item parser
    let mut item_match_arms = Vec::new();
    // multiplicity_check: code fragemnts that check if references marked as required are present
    let mut multiplicity_check = quote!{};

    // generate for each tagged item
    for item in tg_items {
        let tmp_itemname = format_ident!("tmp_required__{}", make_varname(&item.tag));
        let itemname = format_ident!("{}", make_varname(&item.tag));
        let typename = generate_bare_typename(&item.item.typename, &item.item.basetype);
        let store_item; // a code fragment that stores the parsed item into an Option<T> or a Vec<T>
        let tag_string = item.tag.clone();

        if item.repeat {
            // repeated items are represented as Vec<TypeName>
            result.extend(quote!{let mut #itemname: Vec<#typename> = Vec::new();});
            store_item = quote!{
                #itemname.push(newitem);
            };
            if item.required {
                multiplicity_check.extend(quote!{
                    if #itemname.len() == 0 {
                        parser.error_or_log(#cratename::ParseError::InvalidMultiplicityNotPresent(context.clone(), #tag_string.to_string()))?;
                    }
                });
            }
        } else {
            // non-repeated items are represented as Option<Typename> if they are not required
            // they are represented directly as Typename if they are required
            if item.required {
                // required items are first stored into a temporary variable of type Option<T>
                result.extend(quote!{let mut #tmp_itemname: Option<#typename> = None;});
                store_item = quote!{
                    if #tmp_itemname.is_none() {
                        #tmp_itemname = Some(newitem);
                    } else {
                        parser.error_or_log(#cratename::ParseError::InvalidMultiplicityTooMany(context.clone(), tag.clone()))?;
                    }
                };
                // during the mutliplicity check the required item can be unwrapped from the Option
                multiplicity_check.extend(quote!{
                    let #itemname = if let Some(value) = #tmp_itemname {
                        value
                    } else {
                        return Err(#cratename::ParseError::InvalidMultiplicityNotPresent(context.clone(), #tag_string.to_string()));
                    };
                });
            } else {
                // an non-repeating item that is not required
                result.extend(quote!{let mut #itemname: Option<#typename> = None;});
                store_item = quote!{
                    if #itemname.is_none() {
                        #itemname = Some(newitem);
                    } else {
                        parser.error_or_log(#cratename::ParseError::InvalidMultiplicityTooMany(context.clone(), tag.clone()))?;
                    }
                };
            }
        }

        let is_block_item = item.is_block;
        let keyword = &item.tag;
        item_match_arms.push(
            quote!{
                #keyword => {
                    let newitem = #typename::parse(parser, &newcontext)?;
                    #store_item
                    if #is_block_item != is_block {
                        parser.error_or_log(#cratename::ParseError::IncorrectElemType(context.clone(), #keyword.to_string(), #is_block_item))?;
                    }
                }
            }
        );

    }

    // collect all the match arms of the separate statements into the full match statement
    let parser_core = quote!{
        let (tag, is_block) = tag_peek.unwrap();
        let token = parser.get_token(context)?;
        let newcontext = #cratename::ParseContext::from_token(token, is_block);
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


    // wrap the match statement inside a while loop
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

    // now that all items have been parsed, the check if all required items are present can be performed
    result.extend(quote!{
        #multiplicity_check
    });

    result
}


// generate_tagged_item_names()
// generate variable names for all of the items in a TggedStruct or TaggedUnion
fn generate_tagged_item_names(tg_items: &Vec<TaggedItem>) -> Vec<Ident> {
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
