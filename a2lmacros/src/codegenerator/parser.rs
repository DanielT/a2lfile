use proc_macro2::Ident;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;

use super::{generate_bare_typename, BaseType, DataItem, EnumItem, TaggedItem};
use crate::util::*;

// generate
// generate parser function implementations for the set of types of A2l
// A2ml does not use this code, since it parses its data from intermediate GenericIfData structures
pub(crate) fn generate(typename: &str, dataitem: &DataItem) -> TokenStream {
    let mut result = quote! {};

    match &dataitem.basetype {
        BaseType::Enum { enumitems } => {
            result.extend(generate_enum_parser(typename, enumitems));
        }
        BaseType::Struct { structitems } => {
            result.extend(generate_block_parser_generic(typename, structitems, false));
        }
        BaseType::Block {
            blockitems,
            is_block,
        } => {
            result.extend(generate_block_parser(typename, blockitems, *is_block));
        }
        _ => {
            panic!("only block, struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, dataitem);
        }
    }
    result
}

// generate_enum_parser()
// generates a parser function that returns the enum variant matching the text of the current input token
fn generate_enum_parser(typename: &str, enumitems: &[EnumItem]) -> TokenStream {
    let name = format_ident!("{}", typename);

    let mut match_branches = Vec::new();
    for enitem in enumitems {
        let enident = format_ident!("{}", ucname_to_typename(&enitem.name));
        let entag = &enitem.name;

        let mut version_check = quote! {};
        if let Some((min_ver, max_ver)) = enitem.version_range {
            version_check = quote! {
                parser.check_enumitem_version(context, #entag, #min_ver, #max_ver)?;
            };
        }

        match_branches.push(quote! {#entag => {
            #version_check
            Ok(Self::#enident)
        }});
    }

    quote! {
        impl #name {
            pub(crate) fn parse(parser: &mut ParserState, context: &ParseContext) -> Result<Self, ParseError> {
                let enumname = parser.get_identifier(context)?;
                match &*enumname {
                    #(#match_branches)*
                    _ => Err(ParseError::InvalidEnumValue(context.clone(), enumname))
                }
            }
        }
    }
}

// generate_block_parser
// generates the full parser function for a block or keyword
// for elements derived from an a2ml spec, blocks are structs which occur after a tag in a TaggedUnion or TaggedStruct.
fn generate_block_parser(typename: &str, structitems: &[DataItem], is_block: bool) -> TokenStream {
    match typename {
        "A2ml" | "IfData" => {
            quote! {}
        }
        _ => generate_block_parser_generic(typename, structitems, is_block),
    }
}

// generate_block_parser_generic()
// generate a parser function for a block, keyword or struct
fn generate_block_parser_generic(
    typename: &str,
    structitems: &[DataItem],
    is_block: bool,
) -> TokenStream {
    let name = format_ident!("{}", typename);
    let (itemnames, itemparsers, location_names) = generate_struct_item_fragments(structitems);

    // check the block /end tag - blocks only, not for keywords or structs
    let blockcheck = if is_block {
        quote! {
            let __end_offset = parser.get_current_line_offset();
            parser.expect_token(context, A2lTokenType::End)?;
            let ident = parser.get_identifier(context)?;
            if ident != context.element {
                parser.error_or_log(ParseError::IncorrectEndTag(context.clone(), ident))?;
            }
        }
    } else {
        quote! {
            let __end_offset: u32 = 0;
        }
    };

    quote! {
        impl #name {
            pub(crate) fn parse(parser: &mut ParserState, context: &ParseContext, __start_offset: u32) -> Result<Self, ParseError> {
                let __location_incfile = parser.get_incfilename(context.fileid);
                let __location_line = context.line;
                let __uid = parser.get_next_id();
                #(#itemparsers)*
                #blockcheck
                Ok(Self {
                    __block_info: BlockInfo {
                        incfile: __location_incfile,
                        line: __location_line,
                        uid: __uid,
                        start_offset: __start_offset,
                        end_offset: __end_offset,
                        item_location:( #(#location_names),* )
                    },
                    #(#itemnames),*
                })
            }
        }
    }
}

// generate_struct_item_fragments
// generate a list of struct elements as well as TokenStreams with code to parse these elements
fn generate_struct_item_fragments(
    structitems: &[DataItem],
) -> (Vec<Ident>, Vec<TokenStream>, Vec<Ident>) {
    let mut itemparsers = Vec::<TokenStream>::new();
    let mut itemnames = Vec::<Ident>::new();
    let mut location_names = Vec::<Ident>::new();
    for (idx, sitem) in structitems.iter().enumerate() {
        let is_last = idx == (structitems.len() - 1);
        match &sitem.basetype {
            BaseType::TaggedStruct { tsitems } => {
                itemparsers.push(generate_taggeditem_parser(tsitems, false, is_last));
                itemnames.extend(generate_tagged_item_names(tsitems));
            }
            BaseType::TaggedUnion { tuitems } => {
                itemparsers.push(generate_taggeditem_parser(tuitems, true, is_last));
                itemnames.extend(generate_tagged_item_names(tuitems));
            }
            BaseType::Sequence { seqtype } => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                itemparsers.push(generate_sequence_parser(
                    &itemname,
                    &sitem.typename,
                    seqtype,
                ));
                location_names.push(format_ident!("__{}_location", itemname));
                itemnames.push(itemname);
            }
            _ => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                let itemname_location = format_ident!("__{}_location", itemname);
                let itemparser = generate_item_parser_call(&sitem.typename, &sitem.basetype);
                itemparsers.push(quote! {let (#itemname_location, #itemname) = #itemparser;});
                location_names.push(itemname_location);
                itemnames.push(itemname);
            }
        }
    }

    if location_names.len() == 1 {
        itemparsers.push(quote! { let __dummy = (); });
        location_names.push(format_ident!("__dummy"));
    }

    (itemnames, itemparsers, location_names)
}

// generate_item_parser_call
// generates code to call an existing item parser function
// each item parser fragment evaluates as a tuple (locationinfo, value)
fn generate_item_parser_call(typename: &Option<String>, item: &BaseType) -> TokenStream {
    match item {
        BaseType::Char
        | BaseType::Int
        | BaseType::Long
        | BaseType::Int64
        | BaseType::Uchar
        | BaseType::Uint
        | BaseType::Ulong
        | BaseType::Uint64 => {
            let intparser = get_int_parser(item);
            quote! {{
                let line = parser.get_current_line_offset();
                let (value, is_hex) = #intparser(context)?;
                ((line, is_hex), value)
            }}
        }
        BaseType::Double => {
            quote! {(parser.get_current_line_offset(), parser.get_double(context)?)}
        }
        BaseType::Float => {
            quote! {(parser.get_current_line_offset(), parser.get_float(context)?)}
        }
        BaseType::Ident => {
            quote! {(parser.get_current_line_offset(), parser.get_identifier(context)?)}
        }
        BaseType::String => {
            quote! {(parser.get_current_line_offset(), parser.get_string(context)?)}
        }
        BaseType::Array { arraytype, dim } => {
            if let BaseType::Char = arraytype.basetype {
                quote! {parser.get_string_maxlen(context, #dim)?}
            } else {
                let itemparser =
                    generate_item_parser_call(&arraytype.typename, &arraytype.basetype);
                let names: Vec<Ident> = (0..(*dim))
                    .into_iter()
                    .map(|x| format_ident!("__arrayitem_{}", x))
                    .collect();
                let parsercalls = names.iter().map(|name| quote! {let #name = #itemparser;});
                quote! {{
                    #(#parsercalls)*
                    ([ #(#names.0),* ], [ #(#names.1),*])
                }}
            }
        }
        BaseType::EnumRef => {
            let typename = typename.as_ref().unwrap();
            let name = format_ident!("{}", typename);
            quote! { (parser.get_current_line_offset(), #name::parse(parser, context)?) }
        }
        BaseType::StructRef => {
            let typename = typename.as_ref().unwrap();
            let name = format_ident!("{}", typename);
            quote! { (parser.get_current_line_offset(), #name::parse(parser, context, 0)?) }
        }
        _ => panic!("forbidden type: {:#?}", item),
    }
}

// get_int_parser()
// simplify the handling of the integer case in generate_item_parser_call
fn get_int_parser(item: &BaseType) -> TokenStream {
    match item {
        BaseType::Char => {
            quote! {parser.get_integer_i8}
        }
        BaseType::Int => {
            quote! {parser.get_integer_i16}
        }
        BaseType::Long => {
            quote! {parser.get_integer_i32}
        }
        BaseType::Int64 => {
            quote! {parser.get_integer_i64}
        }
        BaseType::Uchar => {
            quote! {parser.get_integer_u8}
        }
        BaseType::Uint => {
            quote! {parser.get_integer_u16}
        }
        BaseType::Ulong => {
            quote! {parser.get_integer_u32}
        }
        BaseType::Uint64 => {
            quote! {parser.get_integer_u64}
        }
        _ => panic!("call of get_int_parser only allowed for integer types"),
    }
}

// generate_sequence_parser
// Generates a TokenStream with code to greedily parse elements of a sequence
// Parsing of sequence items continues until the parser function for the current sequence item returns an error
fn generate_sequence_parser(
    itemname: &Ident,
    typename: &Option<String>,
    seqitem: &BaseType,
) -> TokenStream {
    let parserfunc = generate_item_parser_call(typename, seqitem);
    let itemname_location = format_ident!("__{}_location", itemname);
    quote! {
        let mut #itemname = Vec::new();
        let mut #itemname_location = Vec::new();
        let mut done = false;
        while done == false {
            let current_token = parser.get_tokenpos();
            let sequence_item = {|parser: &mut ParserState, context: &ParseContext| {Ok(#parserfunc)}}(parser, context);
            if sequence_item.is_err() {
                parser.set_tokenpos(current_token);
                done = true;
            }
            else {
                let (location, value) = sequence_item?;
                #itemname.push(value);
                #itemname_location.push(location);
            }
        }
    }
}

// generate_taggeditem_parser
// Generate a TokenStream representing code to parse all the tagged items of a TaggedStruct or TaggedUnion
fn generate_taggeditem_parser(
    tg_items: &[TaggedItem],
    is_taggedunion: bool,
    is_last: bool,
) -> TokenStream {
    // result: the TokenStream that ultimately collcts all the code fragements in this function
    let mut result = quote! {};

    // item_match_arms: the match arms of the while loop that passes each set of input tokens to the appropriate item parser
    // multiplicity_check: code fragemnts that check if references marked as required are present
    let (var_definitions, item_match_arms, multiplicity_check) =
        generate_taggeditem_match_arms(tg_items);
    result.extend(var_definitions);

    // generate the full match statement that has one arm for each tgitem
    let parser_core =
        generate_taggeditem_parser_core(tg_items, is_taggedunion, is_last, &item_match_arms);

    // wrap the match statement inside an if or a while loop
    if is_taggedunion {
        result.extend(quote! {
            let mut next_tag = parser.get_next_tag(context)?;
            if next_tag.is_some() {
                #parser_core
            }
        });
    } else {
        result.extend(quote! {
            let mut next_tag = parser.get_next_tag(context)?;
            while next_tag.is_some() {
                #parser_core
                next_tag = parser.get_next_tag(context)?;
            }
        });
    }

    // now that all items have been parsed, the check if all required items are present can be performed
    result.extend(quote! {
        #multiplicity_check
    });

    result
}

// generate_taggeditem_match_arms()
// a match statement is used in order to parse the taggeditems of a TaggedStruct / TaggedUnion
// This function generates all of the match arms of the match expression
// In order to use the result of the match arms, we also need the definition of the generated variables.
fn generate_taggeditem_match_arms(
    tg_items: &[TaggedItem],
) -> (TokenStream, Vec<TokenStream>, TokenStream) {
    let mut var_definitions = quote! {};
    // item_match_arms: the match arms of the while loop that passes each set of input tokens to the appropriate item parser
    let mut item_match_arms = Vec::new();
    // multiplicity_check: code fragemnts that check if references marked as required are present
    let mut multiplicity_check = quote! {};

    for item in tg_items {
        let tmp_itemname = format_ident!("__tmp_required_{}", make_varname(&item.tag));
        let itemname = format_ident!("{}", make_varname(&item.tag));
        let typename = generate_bare_typename(&item.item.typename, &item.item.basetype);
        let store_item; // a code fragment that stores the parsed item into an Option<T> or a Vec<T>
        let tag_string = &item.tag;

        if item.repeat {
            // repeated items are represented as Vec<TypeName>
            var_definitions.extend(quote! {let mut #itemname: Vec<#typename> = Vec::new();});
            store_item = quote! {
                #itemname.push(newitem);
            };
            if item.required {
                multiplicity_check.extend(quote! {
                    if #itemname.len() == 0 {
                        parser.error_or_log(ParseError::InvalidMultiplicityNotPresent(context.clone(), #tag_string.to_string()))?;
                    }
                });
            }
        } else {
            // non-repeated items are represented as Option<Typename> if they are not required
            // they are represented directly as Typename if they are required
            if item.required {
                // required items are first stored into a temporary variable of type Option<T>
                var_definitions.extend(quote! {let mut #tmp_itemname: Option<#typename> = None;});
                store_item = quote! {
                    parser.handle_multiplicity_error(context, tag, #tmp_itemname.is_some())?;
                    #tmp_itemname = Some(newitem);
                };
                // during the mutliplicity check the required item can be unwrapped from the Option
                multiplicity_check.extend(quote! {
                    let #itemname = if let Some(value) = #tmp_itemname {
                        value
                    } else {
                        return Err(ParseError::InvalidMultiplicityNotPresent(context.clone(), #tag_string.to_string()));
                    };
                });
            } else {
                // an non-repeating item that is not required
                var_definitions.extend(quote! {let mut #itemname: Option<#typename> = None;});
                store_item = quote! {
                    parser.handle_multiplicity_error(context, tag, #itemname.is_some())?;
                    #itemname = Some(newitem);
                };
            }
        }

        let mut version_check = quote! {};
        if let Some((min_ver, max_ver)) = item.version_range {
            version_check.extend(quote! {
                parser.check_block_version(context, #tag_string, #min_ver, #max_ver)?;
            });
        }

        let is_block_item = item.is_block;
        item_match_arms.push(quote! {
            #tag_string => {
                #version_check
                let newitem = #typename::parse(parser, &newcontext, line_offset)?;
                #store_item
                expect_block = #is_block_item;
            }
        });
    }

    (var_definitions, item_match_arms, multiplicity_check)
}

// generate_taggeditem_parser_core()
// generate the match statement for parsing TaggedStructs and TaggedUnions
fn generate_taggeditem_parser_core(
    tg_items: &[TaggedItem],
    is_taggedunion: bool,
    is_last: bool,
    item_match_arms: &[TokenStream],
) -> TokenStream {
    // default action if a tag is not recognized: step back in the tokenstream and let it be handled somewhere else
    let mut default_match_arm = quote! {
        if is_block {
            parser.undo_get_token();
        }
        parser.undo_get_token();
    };

    if !is_taggedunion {
        // taggedstructs use a while loop; if parsing fails the loop needs to end
        default_match_arm.extend(quote! {break;});
    }

    // if this taggedstruct / taggedunion is the last element in the block
    // and this block (at runtime) is actually inside /begin ...  /end, then there is no way to let the unknown tag to be handled somewhere else
    if is_last {
        default_match_arm = quote! {
            if context.inside_block {
                parser.handle_unknown_taggedstruct_tag(context, tag, is_block, &TAG_LIST)?;
            } else {
                #default_match_arm
            }
            expect_block = is_block;
        };
    }

    let taglist: Vec<String> = tg_items.iter().map(|item| item.tag.clone()).collect();
    let taglist_len = taglist.len();
    // generate the full match statement
    quote! {
        let (token, is_block, line_offset) = next_tag.unwrap();
        let tag = parser.get_token_text(token);
        let newcontext = ParseContext::from_token(tag, token, is_block);
        let expect_block: bool;
        const TAG_LIST: [&str; #taglist_len] = [#(#taglist),*];
        match tag {
            #(#item_match_arms)*
            _ => {
                #default_match_arm
            }
        }
        if expect_block != is_block {
            parser.error_or_log(ParseError::IncorrectElemType(context.clone(), tag.to_string(), expect_block))?;
        }
    }
}

// generate_tagged_item_names()
// generate variable names for all of the items in a TggedStruct or TaggedUnion
fn generate_tagged_item_names(tg_items: &[TaggedItem]) -> Vec<Ident> {
    let mut names = Vec::new();

    for item in tg_items {
        names.push(format_ident!("{}", make_varname(&item.tag)));
    }

    names
}
