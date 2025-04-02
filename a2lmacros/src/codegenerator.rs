use crate::util::{make_varname, ucname_to_typename};
use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;
use quote::ToTokens;
use std::str::FromStr;

pub(crate) mod data_structure;
pub(crate) mod ifdata_parser;
pub(crate) mod ifdata_writer;
pub(crate) mod parser;
pub(crate) mod writer;

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum A2lVersion {
    V1_5_0,
    V1_5_1,
    V1_6_0,
    V1_6_1,
    V1_7_0,
    V1_7_1,
}

#[derive(Debug, PartialEq, Clone)]
pub(crate) struct EnumItem {
    pub(crate) name: String,
    pub(crate) value: Option<i32>,
    pub(crate) comment: Option<String>,
    pub(crate) version_upper: Option<A2lVersion>,
    pub(crate) version_lower: Option<A2lVersion>,
}

#[derive(Debug)]
pub(crate) struct DataItem {
    pub(crate) typename: Option<String>,
    pub(crate) basetype: BaseType,
    pub(crate) varname: Option<String>,
    pub(crate) comment: Option<String>,
}

#[derive(Debug, PartialEq)]
pub(crate) struct TaggedItem {
    pub(crate) tag: String,
    pub(crate) item: DataItem,
    pub(crate) is_block: bool,
    pub(crate) repeat: bool,
    pub(crate) required: bool,
    pub(crate) is_named: bool,
    pub(crate) version_upper: Option<A2lVersion>,
    pub(crate) version_lower: Option<A2lVersion>,
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
        dim: usize,
    },
    Sequence {
        seqtype: Box<BaseType>,
    },
    Enum {
        enumitems: Vec<EnumItem>,
    },
    EnumRef,
    Struct {
        structitems: Vec<DataItem>,
    },
    StructRef,
    TaggedUnion {
        tuitems: Vec<TaggedItem>,
    },
    TaggedUnionRef,
    TaggedStruct {
        tsitems: Vec<TaggedItem>,
    },
    TaggedStructRef,
    Block {
        blockitems: Vec<DataItem>,
        is_block: bool,
        used_in_list: bool,
    },
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
        BaseType::EnumRef | BaseType::StructRef => {
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
        self.typename == other.typename
            && self.basetype == other.basetype
            && self.varname == other.varname
    }
}

impl FromStr for A2lVersion {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "1.50" => Ok(A2lVersion::V1_5_0),
            "1.51" => Ok(A2lVersion::V1_5_1),
            "1.60" => Ok(A2lVersion::V1_6_0),
            "1.61" => Ok(A2lVersion::V1_6_1),
            "1.70" => Ok(A2lVersion::V1_7_0),
            "1.71" => Ok(A2lVersion::V1_7_1),
            _ => Err(format!("{s} is not a valid A2L version")),
        }
    }
}

impl ToTokens for A2lVersion {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            A2lVersion::V1_5_0 => tokens.extend(quote! {A2lVersion::V1_5_0}),
            A2lVersion::V1_5_1 => tokens.extend(quote! {A2lVersion::V1_5_1}),
            A2lVersion::V1_6_0 => tokens.extend(quote! {A2lVersion::V1_6_0}),
            A2lVersion::V1_6_1 => tokens.extend(quote! {A2lVersion::V1_6_1}),
            A2lVersion::V1_7_0 => tokens.extend(quote! {A2lVersion::V1_7_0}),
            A2lVersion::V1_7_1 => tokens.extend(quote! {A2lVersion::V1_7_1}),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_data_item_eq() {
        let item1 = DataItem {
            typename: Some("foo".to_string()),
            basetype: BaseType::Int,
            varname: Some("foo".to_string()),
            comment: None,
        };
        let item2 = DataItem {
            typename: Some("bar".to_string()),
            basetype: BaseType::Int,
            varname: Some("bar".to_string()),
            comment: None,
        };
        assert_eq!(item1, item1);
        assert_ne!(item1, item2);
    }

    #[test]
    fn test_a2lversion() {
        let v1_5_0: A2lVersion = "1.50".parse().unwrap();
        let v1_5_1: A2lVersion = "1.51".parse().unwrap();
        let v1_6_0: A2lVersion = "1.60".parse().unwrap();
        let v1_6_1: A2lVersion = "1.61".parse().unwrap();
        let v1_7_0: A2lVersion = "1.70".parse().unwrap();
        let v1_7_1: A2lVersion = "1.71".parse().unwrap();
        assert_eq!(v1_5_0, A2lVersion::V1_5_0);
        assert_eq!(v1_5_1, A2lVersion::V1_5_1);
        assert_eq!(v1_6_0, A2lVersion::V1_6_0);
        assert_eq!(v1_6_1, A2lVersion::V1_6_1);
        assert_eq!(v1_7_0, A2lVersion::V1_7_0);
        assert_eq!(v1_7_1, A2lVersion::V1_7_1);

        let bad_version = "bad".parse::<A2lVersion>();
        assert!(bad_version.is_err());
    }
}
