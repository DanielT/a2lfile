use proc_macro2::Literal;
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;

use crate::codegenerator::{BaseType, DataItem, EnumItem, TaggedItem};
use crate::util::{make_varname, ucname_to_typename};

pub(crate) fn generate(typename: &str, dataitem: &DataItem) -> TokenStream {
    let mut result = TokenStream::new();

    match &dataitem.basetype {
        BaseType::Enum { enumitems } => {
            result.extend(generate_indirect_enum_writer(typename, enumitems));
        }
        BaseType::Struct { structitems } => {
            result.extend(generate_indirect_struct_writer(typename, structitems));
        }
        BaseType::Block { blockitems, .. } => {
            result.extend(generate_indirect_block_writer(typename, blockitems));
        }
        _ => {
            panic!("only block, struct and enum are allowed as top-level types, but {typename} = {dataitem:#?} was encountered");
        }
    }

    result
}

fn generate_indirect_enum_writer(typename: &str, enumitems: &[EnumItem]) -> TokenStream {
    let name = format_ident!("{}", typename);

    let mut match_branches = Vec::new();
    for enitem in enumitems {
        let enident = format_ident!("{}", ucname_to_typename(&enitem.name));
        let entag = &enitem.name;

        match_branches.push(quote! {Self::#enident => #entag});
    }

    quote! {
        impl #name {
            pub(crate) fn store(&self, line: u32) -> a2lfile::GenericIfData {
                a2lfile::GenericIfData::EnumItem(line, match self {
                    #(#match_branches),*
                }.to_string())
            }
        }
    }
}

fn generate_indirect_struct_writer(typename: &str, structitems: &[DataItem]) -> TokenStream {
    let name = format_ident!("{}", typename);

    let stored_structitems = generate_indirect_store_item(structitems);

    quote! {
        impl #name {
            pub(crate) fn store(&self) -> a2lfile::GenericIfData {
                a2lfile::GenericIfData::Struct(self.__block_info.incfile.clone(), self.__block_info.line, vec![ #(#stored_structitems),* ])
            }
        }
    }
}

fn generate_indirect_block_writer(typename: &str, blockitems: &[DataItem]) -> TokenStream {
    let name = format_ident!("{}", typename);

    let stored_blockitems = generate_indirect_store_item(blockitems);

    quote! {
        impl #name {
            pub(crate) fn store(&self) -> a2lfile::GenericIfData {
                a2lfile::GenericIfData::Block {
                    incfile: self.__block_info.incfile.clone(),
                    line: self.__block_info.line,
                    items: vec![ #(#stored_blockitems),* ]
                }
            }
        }
    }
}

fn generate_indirect_store_item(structitems: &[DataItem]) -> Vec<TokenStream> {
    let mut stored_structitems = Vec::new();
    let mut storageidx: usize = 0;
    for item in structitems {
        let sidx_literal = Literal::usize_unsuffixed(storageidx);
        let location = quote! {self.__block_info.item_location.#sidx_literal};
        stored_structitems.push(match &item.basetype {
            BaseType::Sequence { seqtype } => {
                let itemname = format_ident!("{}", item.varname.as_ref().unwrap());
                let locationinfo = quote! {(*#location.get(idx).unwrap_or_else(|| &0))};
                let parsercall =
                    generate_indirect_store_simple_item(&quote! {item}, &locationinfo, seqtype);
                storageidx += 1;
                quote! {a2lfile::GenericIfData::Sequence({
                    let mut sequence_content = Vec::new();
                    for (idx, item) in self.#itemname.iter().enumerate() {
                        sequence_content.push(#parsercall);
                    }
                    sequence_content})
                }
            }
            BaseType::TaggedUnion { tuitems } => {
                let taggeditem_generator = generate_indirect_store_taggeditems(tuitems);
                quote! {a2lfile::GenericIfData::TaggedUnion({#taggeditem_generator})}
            }
            BaseType::TaggedStruct { tsitems } => {
                let taggeditem_generator = generate_indirect_store_taggeditems(tsitems);
                quote! {a2lfile::GenericIfData::TaggedStruct({#taggeditem_generator})}
            }
            _ => {
                let itemname = format_ident!("{}", item.varname.as_ref().unwrap());
                storageidx += 1;
                generate_indirect_store_simple_item(
                    &quote! {self.#itemname},
                    &location,
                    &item.basetype,
                )
            }
        });
    }
    stored_structitems
}

fn generate_indirect_store_simple_item(
    itemname: &TokenStream,
    locationinfo: &TokenStream,
    basetype: &BaseType,
) -> TokenStream {
    match basetype {
        BaseType::None => quote! {a2lfile::GenericIfData::None},
        BaseType::Char => {
            quote! {a2lfile::GenericIfData::Char(#locationinfo.0, (#itemname, #locationinfo.1))}
        }
        BaseType::Int => {
            quote! {a2lfile::GenericIfData::Int(#locationinfo.0, (#itemname, #locationinfo.1))}
        }
        BaseType::Long => {
            quote! {a2lfile::GenericIfData::Long(#locationinfo.0, (#itemname, #locationinfo.1))}
        }
        BaseType::Int64 => {
            quote! {a2lfile::GenericIfData::Int64(#locationinfo.0, (#itemname, #locationinfo.1))}
        }
        BaseType::Uchar => {
            quote! {a2lfile::GenericIfData::UChar(#locationinfo.0, (#itemname, #locationinfo.1))}
        }
        BaseType::Uint => {
            quote! {a2lfile::GenericIfData::UInt(#locationinfo.0, (#itemname, #locationinfo.1))}
        }
        BaseType::Ulong => {
            quote! {a2lfile::GenericIfData::ULong(#locationinfo.0, (#itemname, #locationinfo.1))}
        }
        BaseType::Uint64 => {
            quote! {a2lfile::GenericIfData::UInt64(#locationinfo.0, (#itemname, #locationinfo.1))}
        }
        BaseType::Double => quote! {a2lfile::GenericIfData::Double(#locationinfo, #itemname)},
        BaseType::Float => quote! {a2lfile::GenericIfData::Float(#locationinfo, #itemname)},
        BaseType::String => {
            quote! {a2lfile::GenericIfData::String(#locationinfo, #itemname.to_owned())}
        }
        BaseType::Array { arraytype, .. } => {
            if arraytype.basetype == BaseType::Char {
                quote! {a2lfile::GenericIfData::String(#locationinfo, #itemname.to_owned())}
            } else {
                let arrayitem_locinfo = quote! {#locationinfo[idx]};
                let parsercall = generate_indirect_store_simple_item(
                    &quote! {*item},
                    &arrayitem_locinfo,
                    &arraytype.basetype,
                );
                quote! {a2lfile::GenericIfData::Array({
                    let mut arraycontent = Vec::new();
                    for (idx, item) in #itemname.iter().enumerate() {
                        arraycontent.push(#parsercall);
                    }
                    arraycontent})
                }
            }
        }
        BaseType::EnumRef => {
            quote! {#itemname.store(#locationinfo)}
        }
        BaseType::StructRef => {
            quote! {#itemname.store()}
        }
        BaseType::Sequence { .. }
        | BaseType::TaggedUnion { .. }
        | BaseType::TaggedStruct { .. } => {
            panic!("should no be able to reach this function with type {basetype:?}")
        }
        _ => quote! {},
    }
}

fn generate_indirect_store_taggeditems(tgitems: &[TaggedItem]) -> TokenStream {
    let mut insert_items = Vec::new();
    for tgitem in tgitems {
        let tgname = format_ident!("{}", make_varname(&tgitem.tag));
        let tag = &tgitem.tag;
        let is_block = tgitem.is_block;

        if tgitem.repeat {
            insert_items.push(quote! {
                let mut tgvec = Vec::new();
                for taggeditem in &self.#tgname {
                    tgvec.push(a2lfile::GenericIfDataTaggedItem {
                        tag: #tag.to_string(),
                        data: taggeditem.store(),
                        is_block: #is_block,
                        incfile: taggeditem.__block_info.incfile.clone(),
                        line: taggeditem.__block_info.line,
                        uid: taggeditem.__block_info.uid,
                        start_offset: taggeditem.__block_info.start_offset,
                        end_offset: taggeditem.__block_info.end_offset,
                    });
                }
                if tgvec.len() > 0 {
                    output.insert(#tag.to_string(), tgvec);
                }
            });
        } else {
            insert_items.push(quote! {
                if let Some(taggeditem) = &self.#tgname {
                    let outitem = a2lfile::GenericIfDataTaggedItem{
                        tag: #tag.to_string(),
                        data: taggeditem.store(),
                        is_block: #is_block,
                        incfile: taggeditem.__block_info.incfile.clone(),
                        line: taggeditem.__block_info.line,
                        uid: taggeditem.__block_info.uid,
                        start_offset: taggeditem.__block_info.start_offset,
                        end_offset: taggeditem.__block_info.end_offset,
                    };
                    output.insert(#tag.to_string(), vec![outitem]);
                }
            });
        }
    }

    quote! {
        let mut output = std::collections::HashMap::new();
        #(#insert_items)*
        output
    }
}
