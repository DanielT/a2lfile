use proc_macro2::{Literal, TokenStream};
use quote::format_ident;
use quote::quote;

use super::*;

// generate()
// Generate a pub fn stringify() function for all data types in the specification
pub(crate) fn generate(typename: &str, dataitem: &DataItem) -> TokenStream {
    let mut result = TokenStream::new();

    match &dataitem.basetype {
        BaseType::Enum { enumitems } => {
            result.extend(generate_enum_writer(typename, enumitems));
        }
        BaseType::Struct { structitems } => {
            result.extend(generate_struct_writer(typename, structitems));
        }
        BaseType::Block { blockitems, .. } => {
            result.extend(generate_block_writer(typename, blockitems));
        }
        _ => {
            panic!("only block, struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, dataitem);
        }
    }

    result
}


// generate_enum_writer()
// For enums it actually makes more sense to implement the trait std::fmt::Display, than to have a non-standard stringify() function
fn generate_enum_writer(typename: &str, enumitems: &[EnumItem]) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut match_arms = Vec::new();
    for item in enumitems {
        let enident = format_ident!("{}", ucname_to_typename(&item.name));
        let entag = &item.name;
        // each match arm is something like:  Self::WhateverValue => "WHATEVER_VALUE",
        match_arms.push(quote! {Self::#enident => #entag});
    }

    quote! {
        impl std::fmt::Display for #typeident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let tag = match &self {
                    #(#match_arms),*
                };
                write!(f, "{}", tag)
            }
        }
    }
}


// generate_block_writer()
// Choose between custom handling for A2ml and IfData and generic handling for every other block.
fn generate_block_writer(typename: &str, blockitems: &[DataItem]) -> TokenStream {
    match typename {
        "A2ml" | "IfData" => quote! {},
        _ => generate_block_writer_generic(typename, blockitems)
    }
}


// generate_block_writer_generic()
// Generate a stringify() function for a block
// Each block instantiates its own Writer and uses it to write all of its child elements
fn generate_block_writer_generic(typename: &str, structitems: &[DataItem]) -> TokenStream {
    let typeident = format_ident!("{}", typename);

    let write_items = generate_block_item_writers(structitems);

    quote! {
        impl #typeident {
            pub(crate) fn stringify(&self, indent: usize) -> String {
                let mut writer = writer::Writer::new(indent);

                #(#write_items)*

                writer.finish()
            }
        }
    }
}


// generate_struct_writer()
// Generate a write function for a struct which occurs inside of a block.
// The struct is written using the parent block's Writer
fn generate_struct_writer(typename: &str, structitems: &[DataItem]) -> TokenStream {
    let typeident = format_ident!("{}", typename);

    let write_items = generate_block_item_writers(structitems);

    quote! {
        impl #typeident {
            pub(crate) fn stringify(&self, writer: &mut writer::Writer) {
                #(#write_items)*
            }
        }
    }
}


// generate_block_item_writers()
// Generate a write command for every item in the block.
fn generate_block_item_writers(structitems: &[DataItem]) -> Vec<TokenStream> {
    let mut write_items = Vec::<TokenStream>::new();
    let mut posidx: usize = 0;

    for item in structitems {
        let posliteral = Literal::usize_unsuffixed(posidx);
        let location = quote! {self.__block_info.item_location.#posliteral};
        match &item.basetype {
            BaseType::TaggedUnion { tuitems: taggeditems } |
            BaseType::TaggedStruct { tsitems: taggeditems } => {
                let mut tgwriters = Vec::new();
                for tgitem in taggeditems {
                    let tag = &tgitem.tag;
                    let tgname = format_ident!("{}", make_varname(&tgitem.tag));
                    let tgname_out = format_ident!("{}_out", tgname);
                    let is_block = tgitem.is_block;
                    if tgitem.repeat {
                        tgwriters.push(quote! {
                            for #tgname in &self.#tgname {
                                let #tgname_out = #tgname.stringify(indent + 1);
                                tgroup.push(writer::TaggedItemInfo::build(#tag, #tgname_out, #is_block, &#tgname.__block_info));
                            }
                        });
                    } else if tgitem.required {
                        tgwriters.push(quote! {
                            let #tgname_out = self.#tgname.stringify(indent + 1);
                            tgroup.push(writer::TaggedItemInfo::build(#tag, #tgname_out, #is_block, &self.#tgname.__block_info));
                        });
                    } else {
                        tgwriters.push(quote! {
                            if let Some(#tgname) = &self.#tgname {
                                let #tgname_out = #tgname.stringify(indent + 1);
                                tgroup.push(writer::TaggedItemInfo::build(#tag, #tgname_out, #is_block, &#tgname.__block_info));
                            }
                        });
                    }
                }
                write_items.push(quote! {
                    let mut tgroup = Vec::<writer::TaggedItemInfo>::new();
                    #(#tgwriters)*
                    writer.add_group(tgroup);
                });
            }
            _ => {
                let itemident = format_ident!("{}", item.varname.as_ref().unwrap() );
                let write_cmd = generate_block_item_write_cmd(&item.basetype, quote! {self.#itemident}, location, 0);
                write_items.push(write_cmd);
                posidx += 1;
            }
        }
    }

    write_items
}

// generate_block_item_write_cmd()
// Generate the command to write a single item of a block based on its data type
// The generated code fragments assume that a Writer called writer is available in the enclosing code
fn generate_block_item_write_cmd(basetype: &BaseType, itemname: TokenStream, location: TokenStream, calldepth: usize) -> TokenStream {
    match basetype {
        BaseType::Uchar |
        BaseType::Char |
        BaseType::Uint |
        BaseType::Int |
        BaseType::Ulong |
        BaseType::Long |
        BaseType::Uint64 |
        BaseType::Int64 => { quote! { writer.add_integer(#itemname, #location.1, #location.0); } }
        BaseType::Double |
        BaseType::Float => { quote! { writer.add_float(#itemname, #location); } }
        BaseType::Ident => { quote! {
            writer.add_str(&#itemname, #location);
        } }
        BaseType::String => { quote! {
            writer.add_quoted_string(&#itemname, #location);
        } }
        BaseType::EnumRef => { quote! {
            writer.add_str(&#itemname.to_string(), #location);
        } }
        BaseType::StructRef => { quote! {
            #itemname.stringify(&mut writer);
        } }
        BaseType::Array {arraytype, dim} => {
            if let BaseType::Char = arraytype.basetype {
                generate_block_item_write_cmd(&BaseType::String, itemname, location, calldepth)
            } else {
                // assign an individual idxident based on the calldepth. This enables nested arrays.
                let idxident = format_ident!("idx{}", calldepth);
                let arrayelemname = quote! { #itemname[#idxident] };
                let locationname = quote! { #location[#idxident] };
                let write_cmd = generate_block_item_write_cmd(
                    &arraytype.basetype,
                    arrayelemname,
                    locationname,
                    calldepth + 1
                );
                quote! {
                    for #idxident in 0..#dim {
                        // the #write_cmd inside of this loop uses #idxident to index into the array
                        #write_cmd
                    }
                }
            }
        }
        BaseType::Sequence { seqtype } => {
            let seqitemident = format_ident!("seqitem{}", calldepth);
            let seqidxident = format_ident!("seqidx{}", calldepth);
            let default_location = generate_default_location(seqtype);
            let seqlocation = quote! {(*#location.get(#seqidxident).unwrap_or_else(|| &#default_location))};

            // in the write_cmd all the integer basetypes need an additional dereference, because .iter().enumerate() gives us references
            let write_cmd = match **seqtype {
                BaseType::Double |
                BaseType::Float |
                BaseType::Char |
                BaseType::Uchar |
                BaseType::Int |
                BaseType::Uint |
                BaseType::Long |
                BaseType::Ulong |
                BaseType::Int64 |
                BaseType::Uint64 => generate_block_item_write_cmd(seqtype, quote! {*#seqitemident}, seqlocation, calldepth + 1),
                _                => generate_block_item_write_cmd(seqtype, quote! {#seqitemident}, seqlocation, calldepth + 1),
            };
            quote! {
                for (#seqidxident, #seqitemident) in #itemname.iter().enumerate() {
                    #write_cmd
                }
            }
        }
        _ => {
            panic!("generate_block_item_to_string_cmd can't be called for type {:#?}", basetype);
        }
    }
}


fn generate_default_location(basetype: &BaseType) -> TokenStream {
    match basetype {
        BaseType::Char |
        BaseType::Uchar |
        BaseType::Int |
        BaseType::Uint |
        BaseType::Long |
        BaseType::Ulong |
        BaseType::Int64 |
        BaseType::Uint64 => quote! {(0, false)},
        _ => quote! {0}
    }
}

