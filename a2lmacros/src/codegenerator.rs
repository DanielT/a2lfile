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
    pub(crate) repeat: bool
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
            _ => {
                panic!("only struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, a2mltype);
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
        #[derive(Debug, PartialEq)]
        pub(crate) enum #typeident {
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
        pub(crate) struct #typeident {
            fileid: usize,
            line: u32,

            #(#definitions),*
        }
    }
}


fn generate_struct_item_definition(item: &DataItem) -> TokenStream {
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


pub(crate) fn generate_parser(types: &HashMap<String, BaseType>) -> TokenStream {
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


fn generate_enum_parser(typename: &str, enumitems: &Vec<EnumItem>) -> TokenStream {
    let name = format_ident!("{}", typename);

    let mut match_branches = Vec::new();
    for enitem in enumitems {
        let enident = format_ident!("{}", ucname_to_typename(&enitem.name));
        let entag = &enitem.name;
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


fn generate_struct_parser(typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    let name = format_ident!("{}", typename);

    let mut itemparsers = Vec::<TokenStream>::new();
    let mut itemnames = Vec::<Ident>::new();
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


fn generate_item_parser(typename: &Option<String>, item: &BaseType) -> TokenStream {
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
                // The intent is to propagate the Err from any single array element. Since this is a code
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


fn generate_sequence_parser(itemname: &Ident, typename: &Option<String>, seqitem: &BaseType) -> TokenStream {
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


fn generate_taggeditem_parser(tg_items: &Vec<TaggedItem>, is_taggedunion: bool) -> TokenStream {
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
