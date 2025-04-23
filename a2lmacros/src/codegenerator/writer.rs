use proc_macro2::{Literal, TokenStream};
use quote::format_ident;
use quote::quote;

use crate::codegenerator::{BaseType, DataItem, EnumItem, make_varname, ucname_to_typename};

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
        BaseType::Block {
            blockitems,
            is_block,
            ..
        } => {
            result.extend(generate_block_writer(typename, blockitems, *is_block));
        }
        _ => {
            panic!(
                "only block, struct and enum are allowed as top-level types, but {typename} = {dataitem:#?} was encountered"
            );
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
                f.write_str(tag)
            }
        }
    }
}

// generate_block_writer()
// Choose between custom handling for A2ml and IfData and generic handling for every other block.
fn generate_block_writer(typename: &str, blockitems: &[DataItem], is_block: bool) -> TokenStream {
    match typename {
        "A2ml" | "IfData" => quote! {},
        _ => generate_block_writer_generic(typename, blockitems, is_block),
    }
}

// generate_block_writer_generic()
// Generate a stringify() function for a block
// Each block instantiates its own Writer and uses it to write all of its child elements
fn generate_block_writer_generic(
    typename: &str,
    structitems: &[DataItem],
    is_block: bool,
) -> TokenStream {
    let typeident = format_ident!("{}", typename);

    let write_items = generate_block_item_writers(structitems, is_block);

    if write_items.is_empty() {
        quote! {
            impl #typeident {
                pub(crate) fn stringify(&self, indent: usize) -> String {
                    let writer = writer::Writer::new(indent);
                    writer.finish()
                }
            }
        }
    } else {
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
}

// generate_struct_writer()
// Generate a write function for a struct which occurs inside of a block.
// The struct is written using the parent block's Writer
fn generate_struct_writer(typename: &str, structitems: &[DataItem]) -> TokenStream {
    let typeident = format_ident!("{}", typename);

    let write_items = generate_block_item_writers(structitems, false);

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
fn generate_block_item_writers(structitems: &[DataItem], allow_comments: bool) -> Vec<TokenStream> {
    let mut write_items = Vec::<TokenStream>::new();
    let mut posidx: usize = 0;

    for item in structitems {
        let posliteral = Literal::usize_unsuffixed(posidx);
        let location = quote! {self.__block_info.item_location.#posliteral};
        match &item.basetype {
            BaseType::TaggedUnion {
                tuitems: taggeditems,
            }
            | BaseType::TaggedStruct {
                tsitems: taggeditems,
            } => {
                let mut tgwriters = Vec::new();
                for tgitem in taggeditems {
                    let tag = &tgitem.tag;
                    let tgname = format_ident!("{}", make_varname(&tgitem.tag));
                    let tgname_out = format_ident!("{}_out", tgname);
                    let is_block = tgitem.is_block;
                    if tgitem.repeat {
                        tgwriters.push(quote! {
                            for #tgname in &self.#tgname {
                                // only stringify the block if it is in the current file
                                let #tgname_out = if #tgname.__block_info.incfile.is_none() { #tgname.stringify(indent + 1) } else { String::new() };
                                tgroup.push(writer::TaggedItemInfo::Tag {
                                    tag: #tag,
                                    item_text: #tgname_out,
                                    is_block: #is_block,
                                    incfile: &#tgname.__block_info.incfile,
                                    uid: #tgname.__block_info.uid,
                                    line: #tgname.__block_info.line,
                                    start_offset: #tgname.__block_info.start_offset,
                                    end_offset: #tgname.__block_info.end_offset,
                                    position_restriction: #tgname.pos_restrict(),
                                });
                            }
                        });
                    } else if tgitem.required {
                        tgwriters.push(quote! {
                            // only stringify the block if it is in the current file
                            let #tgname_out = if self.#tgname.__block_info.incfile.is_none() { self.#tgname.stringify(indent + 1) } else { String::new() };
                            tgroup.push(writer::TaggedItemInfo::Tag {
                                tag: #tag,
                                item_text: #tgname_out,
                                is_block: #is_block,
                                incfile: &self.#tgname.__block_info.incfile,
                                uid: self.#tgname.__block_info.uid,
                                line: self.#tgname.__block_info.line,
                                start_offset: self.#tgname.__block_info.start_offset,
                                end_offset: self.#tgname.__block_info.end_offset,
                                position_restriction: self.#tgname.pos_restrict(),
                            });
                        });
                    } else {
                        tgwriters.push(quote! {
                            if let Some(#tgname) = &self.#tgname {
                                // only stringify the block if it is in the current file
                                let #tgname_out = if #tgname.__block_info.incfile.is_none() { #tgname.stringify(indent + 1) } else { String::new() };
                                tgroup.push(writer::TaggedItemInfo::Tag {
                                    tag: #tag,
                                    item_text: #tgname_out,
                                    is_block: #is_block,
                                    incfile: &#tgname.__block_info.incfile,
                                    uid: #tgname.__block_info.uid,
                                    line: #tgname.__block_info.line,
                                    start_offset: #tgname.__block_info.start_offset,
                                    end_offset: #tgname.__block_info.end_offset,
                                    position_restriction: #tgname.pos_restrict(),
                                });
                            }
                        });
                    }
                }
                let comment_writer = if allow_comments {
                    quote! { writer::add_comments_to_group(&mut tgroup, &self.a2lcomment); }
                } else {
                    quote! {}
                };
                write_items.push(quote! {
                    let mut tgroup = Vec::<writer::TaggedItemInfo>::new();
                    #(#tgwriters)*
                    #comment_writer
                    writer.add_group(tgroup);
                });
            }
            _ => {
                let itemident = format_ident!("{}", item.varname.as_ref().unwrap());
                let write_cmd = generate_block_item_write_cmd(
                    &item.basetype,
                    quote! {self.#itemident},
                    location,
                    0,
                );
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
fn generate_block_item_write_cmd(
    basetype: &BaseType,
    itemname: TokenStream,
    location: TokenStream,
    calldepth: usize,
) -> TokenStream {
    match basetype {
        BaseType::Uchar
        | BaseType::Char
        | BaseType::Uint
        | BaseType::Int
        | BaseType::Ulong
        | BaseType::Long
        | BaseType::Uint64
        | BaseType::Int64 => {
            quote! { writer.add_integer(#itemname, #location.1, #location.0); }
        }
        BaseType::Double | BaseType::Float => {
            quote! { writer.add_float(#itemname, #location); }
        }
        BaseType::Ident => {
            quote! {
                writer.add_str(&#itemname, #location);
            }
        }
        BaseType::String => {
            quote! {
                writer.add_quoted_string(&#itemname, #location);
            }
        }
        BaseType::EnumRef => {
            quote! {
                writer.add_str(&#itemname.to_string(), #location);
            }
        }
        BaseType::StructRef => {
            quote! {
                #itemname.stringify(&mut writer);
            }
        }
        BaseType::Array { arraytype, dim } => {
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
                    calldepth + 1,
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
            let seqlocation = quote! {(*#location.get(#seqidxident).unwrap_or(&#default_location))};

            // in the write_cmd all the integer basetypes need an additional dereference, because .iter().enumerate() gives us references
            let write_cmd = match **seqtype {
                BaseType::Double
                | BaseType::Float
                | BaseType::Char
                | BaseType::Uchar
                | BaseType::Int
                | BaseType::Uint
                | BaseType::Long
                | BaseType::Ulong
                | BaseType::Int64
                | BaseType::Uint64 => generate_block_item_write_cmd(
                    seqtype,
                    quote! {*#seqitemident},
                    seqlocation,
                    calldepth + 1,
                ),
                _ => generate_block_item_write_cmd(
                    seqtype,
                    quote! {#seqitemident},
                    seqlocation,
                    calldepth + 1,
                ),
            };
            quote! {
                for (#seqidxident, #seqitemident) in #itemname.iter().enumerate() {
                    #write_cmd
                }
            }
        }
        _ => {
            panic!("generate_block_item_to_string_cmd can't be called for type {basetype:#?}");
        }
    }
}

fn generate_default_location(basetype: &BaseType) -> TokenStream {
    match basetype {
        BaseType::Char
        | BaseType::Uchar
        | BaseType::Int
        | BaseType::Uint
        | BaseType::Long
        | BaseType::Ulong
        | BaseType::Int64
        | BaseType::Uint64 => quote! {(0, false)},
        _ => quote! {0},
    }
}
