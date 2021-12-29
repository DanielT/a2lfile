use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;

pub(crate) mod data_structure;
pub(crate) mod ifdata_parser;
pub(crate) mod ifdata_writer;
pub(crate) mod parser;
pub(crate) mod writer;

use super::util::*;


#[derive(Debug, PartialEq, Clone)]
pub(crate) struct EnumItem {
    pub(crate) name: String,
    pub(crate) value: Option<i32>,
    pub(crate) comment: Option<String>,
    pub(crate) version_range: Option<(f32, f32)>
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
    pub(crate) required: bool,
    pub(crate) version_range: Option<(f32, f32)>
}


#[derive(Debug, PartialEq)]
pub(crate) enum BaseType {
    None,
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
    Ident,  // text without double quotes, obeying the rules for identifiers (no spaces, etc)
    String, // text inside double qoutes
    Array {
        arraytype: Box<DataItem>,
        dim: usize
    },
    Sequence {
        seqtype: Box<BaseType>
    },
    Enum {
        enumitems: Vec<EnumItem>
    },
    EnumRef,
    Struct {
        structitems: Vec<DataItem>
    },
    StructRef,
    TaggedUnion {
        tuitems: Vec<TaggedItem>
    },
    TaggedUnionRef,
    TaggedStruct {
        tsitems: Vec<TaggedItem>
    },
    TaggedStructRef,
    Block {
        blockitems: Vec<DataItem>,
        is_block: bool
    }
}


//-----------------------------------------------------------------------------

// generate_bare_typename()
// generates the typename without other tokens (i.e. "u32" instead of "foo: u32") for a given struct item
fn generate_bare_typename(typename: &Option<String>, item: &BaseType) -> TokenStream {
    match item {
        BaseType::Char => quote! {i8},
        BaseType::Int => quote! {i16},
        BaseType::Long => quote! {i32},
        BaseType::Int64 => quote! {i64},
        BaseType::Uchar => quote! {u8},
        BaseType::Uint => quote! {u16},
        BaseType::Ulong => quote! {u32},
        BaseType::Uint64 => quote! {u64},
        BaseType::Double => quote! {f64},
        BaseType::Float => quote! {f32},
        BaseType::Ident => quote! {String},
        BaseType::String => quote! {String},
        BaseType::Array { arraytype, dim } => {
            // a2ml specifies strings like C does: as arrays of char
            // if this pattern is found, then represent it as a String in Rust
            if arraytype.basetype == BaseType::Char {
                quote! {String}
            } else {
                let name = generate_bare_typename(&arraytype.typename, &arraytype.basetype);
                quote! {[#name; #dim]}
            }
        }
        BaseType::Sequence { seqtype } => {
            let name = generate_bare_typename(typename, seqtype);
            quote! {Vec<#name>}
        }
        BaseType::EnumRef |
        BaseType::StructRef => {
            let typename = typename.as_ref().unwrap();
            let name = format_ident!("{}", typename);
            quote! {#name}
        }
        _ => {
            // None is not allowed at all once generation begins (previously it is a placeholder that is dropped during the fixup phase)
            // Enum, Struct, TaggedUnionRef, TaggedStructRef have been transformed in the fixup phase and can't occur here
            panic!("impossible type - unreachable");
        }
    }
}


// manual implementation of PartialEq to ignore comments when comparing for equality
impl PartialEq for DataItem {
    fn eq(&self, other: &Self) -> bool {
        self.typename == other.typename && self.basetype == other.basetype && self.varname == other.varname
    }
}
