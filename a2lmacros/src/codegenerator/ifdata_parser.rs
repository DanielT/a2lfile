use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;

use crate::codegenerator::{BaseType, DataItem, EnumItem};
use crate::util::{make_varname, ucname_to_typename};

//-----------------------------------------------------------------------------
// "indirect" parsing of IF_DATA -> parsing generic IF_DATA in GenericIfData structures into application-specific data structures

pub(crate) fn generate(typename: &str, dataitem: &DataItem) -> TokenStream {
    let mut result = TokenStream::new();

    match &dataitem.basetype {
        BaseType::Enum { enumitems } => {
            result.extend(generate_indirect_enum_parser(typename, enumitems));
        }
        BaseType::Struct { structitems } => {
            result.extend(generate_indirect_struct_parser(typename, structitems));
        }
        BaseType::Block { blockitems, .. } => {
            result.extend(generate_indirect_block_parser(typename, blockitems));
        }
        _ => {
            panic!(
                "only block, struct and enum are allowed as top-level types, but {typename} = {dataitem:#?} was encountered"
            );
        }
    }

    result
}

fn generate_indirect_enum_parser(typename: &str, enumitems: &[EnumItem]) -> TokenStream {
    let name = format_ident!("{}", typename);

    let mut match_branches = Vec::new();
    for enitem in enumitems {
        let enident = format_ident!("{}", ucname_to_typename(&enitem.name));
        let entag = &enitem.name;

        match_branches.push(quote! {#entag => {
            Ok(Self::#enident)
        }});
    }

    quote! {
        impl #name {
            pub(crate) fn parse(data: &a2lfile::GenericIfData) -> Result<Self, &'static str> {
                if let a2lfile::GenericIfData::EnumItem(_, item) = data {
                    match &**item {
                        #(#match_branches)*
                        _ => Err("failed to match enumeration value")
                    }
                } else {
                    Err("element is not an EnumItem")
                }
            }
        }
    }
}

fn generate_indirect_struct_parser(typename: &str, structitems: &[DataItem]) -> TokenStream {
    let name = format_ident!("{}", typename);
    let structfields = generate_struct_field_intializers(structitems);

    quote! {
        impl #name {
            pub(crate) fn parse(data: &a2lfile::GenericIfData) -> Result<Self, &'static str> {
                let (incfile, line, input_items) = data.get_struct_items()?;
                let __uid: u32 = 0;
                let __start_offset: u32 = 0;
                let __end_offset: u32 = 0;

                Ok(#name {
                    #(#structfields),*
                })
            }
        }
    }
}

fn generate_indirect_block_parser(typename: &str, blockitems: &[DataItem]) -> TokenStream {
    let name = format_ident!("{}", typename);
    let structfields = generate_struct_field_intializers(blockitems);

    quote! {
        impl #name {
            pub(crate) fn parse(data: &a2lfile::GenericIfData, __uid: u32, __start_offset: u32, __end_offset: u32) -> Result<Self, &'static str> {
                let (incfile, line, input_items) = data.get_block_items()?;

                Ok(#name {
                    #(#structfields),*
                })
            }
        }
    }
}

fn generate_struct_field_intializers(items: &[DataItem]) -> Vec<TokenStream> {
    let mut parsers = Vec::new();
    let mut location_info = Vec::new();
    for (idx, item) in items.iter().enumerate() {
        let item_getter =
            quote! {input_items.get(#idx).unwrap_or_else(|| &a2lfile::GenericIfData::None)};
        match &item.basetype {
            BaseType::Sequence { seqtype } => {
                let itemname = format_ident!("{}", item.varname.as_ref().unwrap());
                let itemparser =
                    generate_item_parser_call(&quote! {seqitem}, &item.typename, seqtype);
                let itemlocation = generate_item_location(&quote! {seqitem}, seqtype);
                parsers.push(quote! {
                    #itemname: {
                        let seqitems = #item_getter.get_sequence()?;
                        let mut outitems = Vec::new();
                        for seqitem in seqitems {
                            outitems.push(#itemparser);
                        }
                        outitems
                    }
                });
                location_info.push(quote! {
                    {
                        let seqitems = #item_getter.get_sequence()?;
                        let mut lines = Vec::new();
                        for seqitem in seqitems {
                            lines.push(#itemlocation);
                        }
                        lines
                    }
                });
            }
            BaseType::TaggedUnion {
                tuitems: taggeditems,
            }
            | BaseType::TaggedStruct {
                tsitems: taggeditems,
            } => {
                for tgitem in taggeditems {
                    let tag = &tgitem.tag;
                    let tgitemname = format_ident!("{}", make_varname(tag));
                    let typename = format_ident!("{}", tgitem.item.typename.as_ref().unwrap());
                    if tgitem.repeat {
                        parsers.push(quote! {#tgitemname: #item_getter.get_multiple_optitems(#tag, #typename::parse)?});
                    } else {
                        parsers.push(quote! {#tgitemname: #item_getter.get_single_optitem(#tag, #typename::parse)?});
                    }
                }
            }
            _ => {
                let itemname = format_ident!("{}", item.varname.as_ref().unwrap());
                let itemparser = generate_item_parser_call(
                    &quote! {#item_getter},
                    &item.typename,
                    &item.basetype,
                );
                let itemlocation = generate_item_location(&quote! {#item_getter}, &item.basetype);
                parsers.push(quote! {#itemname: #itemparser});
                location_info.push(itemlocation);
            }
        }
    }

    if location_info.len() == 1 {
        location_info.push(quote! { () });
    }

    parsers.push(quote! {__block_info: BlockInfo {
        incfile,
        line,
        uid: __uid,
        start_offset: __start_offset,
        end_offset: __end_offset,
        item_location: ( #(#location_info),* )
    }});

    parsers
}

fn generate_item_parser_call(
    item_ident: &TokenStream,
    typename: &Option<String>,
    basetype: &BaseType,
) -> TokenStream {
    match basetype {
        BaseType::Char => quote! {#item_ident.get_integer_i8()?},
        BaseType::Int => quote! {#item_ident.get_integer_i16()?},
        BaseType::Long => quote! {#item_ident.get_integer_i32()?},
        BaseType::Int64 => quote! {#item_ident.get_integer_i64()?},
        BaseType::Uchar => quote! {#item_ident.get_integer_u8()?},
        BaseType::Uint => quote! {#item_ident.get_integer_u16()?},
        BaseType::Ulong => quote! {#item_ident.get_integer_u32()?},
        BaseType::Uint64 => quote! {#item_ident.get_integer_u64()?},
        BaseType::Double => quote! {#item_ident.get_double()?},
        BaseType::Float => quote! {#item_ident.get_float()?},
        BaseType::Ident => quote! {#item_ident.get_ident()?},
        BaseType::String => quote! {#item_ident.get_stringval()?},
        BaseType::Array { arraytype, dim } => {
            if let BaseType::Char = arraytype.basetype {
                quote! {#item_ident.get_stringval()?}
            } else {
                let mut arrayelements = Vec::new();
                for arrayidx in 0..*dim {
                    arrayelements.push(generate_item_parser_call(
                        &quote! {arrayitems[#arrayidx]},
                        &arraytype.typename,
                        &arraytype.basetype,
                    ));
                }
                quote! { {
                    let arrayitems = #item_ident.get_array()?;
                    [ #(#arrayelements),* ]
                }}
                //quote! {foo}
            }
        }
        BaseType::EnumRef | BaseType::StructRef => {
            let typename = format_ident!("{}", typename.as_ref().unwrap());
            quote! {#typename::parse(&#item_ident)?}
        }
        _ => panic!("impossible type {basetype:?} in generate_item_parser_call"),
    }
}

fn generate_item_location(item_ident: &TokenStream, basetype: &BaseType) -> TokenStream {
    match basetype {
        BaseType::Char
        | BaseType::Int
        | BaseType::Long
        | BaseType::Int64
        | BaseType::Uchar
        | BaseType::Uint
        | BaseType::Ulong
        | BaseType::Uint64 => quote! {(#item_ident.get_line()?, #item_ident.get_int_is_hex()?)},
        BaseType::Array { arraytype, dim } => {
            if arraytype.basetype == BaseType::Char {
                quote! { #item_ident.get_line()? }
            } else {
                let mut arraylocations = Vec::new();
                for arrayidx in 0..*dim {
                    arraylocations.push(generate_item_location(
                        &quote! {arrayitems[#arrayidx]},
                        &arraytype.basetype,
                    ));
                }
                quote! { {
                    let arrayitems = #item_ident.get_array()?;
                    [ #(#arraylocations),* ]
                }}
            }
        }
        _ => quote! { #item_ident.get_line()? },
    }
}
