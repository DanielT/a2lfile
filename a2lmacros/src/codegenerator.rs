use std::collections::HashMap;
use proc_macro2::{Literal, TokenStream};
use proc_macro2::Ident;
use quote::format_ident;
use quote::quote;

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
    Block(Vec<DataItem>, bool)
}



// generate_data_structures()
// generate the struct and enum definitions for all inputs in types
// Also generate a new() for each type as well as functions for impl Debug and impl PartialEq
pub(crate) fn generate_data_structures(types: &HashMap<String, DataItem>) -> TokenStream {
    let mut result = quote!{};

    // Convert the hashmap of types to a vec and sort it. This gives stable output ordering
    let mut typesvec: Vec<(&String, &DataItem)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.cmp(b.0));

    // generate each type in the list
    for (typename, a2mltype) in typesvec {
        if let Some(comment) = &a2mltype.comment {
            result.extend(quote!{#[doc=#comment]});
        }

        match &a2mltype.basetype {
            BaseType::Enum(enumitems) => {
                result.extend(generate_enum_data_structure(typename, enumitems));
            }
            BaseType::Struct(structitems) => {
                result.extend(generate_block_data_structure_generic(typename, structitems, false));
            }
            BaseType::Block(structitems, is_block) => {
                result.extend(generate_block_data_structure(typename, structitems, *is_block));
            }
            _ => {
                panic!("only block, struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, a2mltype);
            }
        }
    }

    result
}


// generate_enum_data_structure()
// generate an enum with the given name and items
fn generate_enum_data_structure(typename: &str, enumitems: &Vec<EnumItem>) -> TokenStream {
    let typeident = format_ident!("{}", typename);

    let enumidents: Vec<proc_macro2::Ident> = enumitems.iter().map(
        |enumitem| { format_ident!("{}", ucname_to_typename(&enumitem.name)) }
    ).collect();

    quote!{
        #[derive(Debug, PartialEq, Eq, Clone)]
        pub enum #typeident {
            #(#enumidents),*
        }
    }
}


// generate_block_data_structure()
// Generate the data structure of a block (if is_block is false it is a keyword instead, but still almost the same)
// The A2ml and IfData blocks are special in the specification and this is reflected here
fn generate_block_data_structure(typename: &str, structitems: &Vec<DataItem>, is_block: bool) ->  TokenStream {
    match typename {
        "A2ml" => {
            generate_block_data_structure_a2ml()
        }
        "IfData" => {
            generate_block_data_structure_ifdata()
        }
        _ => {
            generate_block_data_structure_generic(typename, structitems, is_block)
        }
    }
}


// generate_block_data_structure_generic()
// generate the data structure and associated functions for all except A2ml and IfData
fn generate_block_data_structure_generic(typename: &str, structitems: &Vec<DataItem>, is_block: bool) ->  TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut definitions = Vec::new();

    for item in structitems {
        definitions.push(generate_struct_item_definition(item));
    }
    definitions.push(generate_struct_location_info(structitems));

    // generate all of the utility functions together with the data structure
    // only write and parse are excluded here, because they are not shared between A2l and A2ml
    let debug = generate_block_data_structure_debug(typename, structitems);
    let constructor = generate_block_data_structure_constructor(typename, structitems, is_block);
    let partialeq = generate_block_data_structure_partialeq(typename, structitems);
    let mergeinc = generate_block_data_structure_mergeincludes(typename, structitems);
    let getline = generate_block_data_structure_get_line(typename);
    let resetlocation = generate_block_data_structure_reset_location(typename, is_block);

    quote!{
        pub struct #typeident {
            #(#definitions),*
        }

        #debug
        #constructor
        #partialeq
        #mergeinc
        #getline
        #resetlocation
    }
}


// generate_block_data_structure_a2ml
// generate the definition of the structure for the A2ML block
fn generate_block_data_structure_a2ml() ->  TokenStream {
    quote!{
        pub struct A2ml {
            pub a2ml_text: String,
            pub __location_info: (Option<String>, u32, u32)
        }

        impl std::fmt::Debug for A2ml {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("A2ml")
                .field("a2ml_text", &self.a2ml_text)
                .finish()
            }
        }

        impl A2ml {
            pub fn new(a2ml_text: String) -> Self {
                Self {
                    a2ml_text,
                    __location_info: (None, 0, 0)
                }
            }

            pub fn merge_includes(&mut self) {
                self.__location_info.0 = None;
            }

            pub fn get_line(&self) -> u32 {
                self.__location_info.1
            }

            pub fn reset_location(&mut self) {
                self.merge_includes();
                self.__location_info.1 = 0;
            }
        }

        impl PartialEq for A2ml {
            fn eq(&self, other: &Self) -> bool {
                self.a2ml_text == other.a2ml_text
            }
        }
    }
}


// generate_block_data_structure_ifdata
// generate the definition of the structure for the IF_DATA block
fn generate_block_data_structure_ifdata() ->  TokenStream {
    quote!{
        pub struct IfData {
            pub ifdata_items: Option<a2ml::GenericIfData>,
            pub __location_info: (Option<String>, u32)
        }

        impl std::fmt::Debug for IfData {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct("IfData")
                .field("ifdata_items", &self.ifdata_items)
                .finish()
            }
        }

        impl IfData {
            pub fn new() -> Self {
                Self {
                    ifdata_items: None,
                    __location_info: (None, 0)
                }
            }

            pub fn merge_includes(&mut self) {
                self.__location_info.0 = None;
                if let Some(ifdata_items) = &mut self.ifdata_items {
                    // ifdata_items is an un-decoded GenericIfData. It can directly handle merge_includes()
                    ifdata_items.merge_includes();
                }
            }

            pub fn get_line(&self) -> u32 {
                self.__location_info.1
            }

            pub fn reset_location(&mut self) {
                self.merge_includes();
                self.__location_info.1 = 0;
            }
        }

        impl PartialEq for IfData {
            fn eq(&self, other: &Self) -> bool {
                self.ifdata_items == other.ifdata_items
            }
        }
    }
}


// generate_struct_item_definition()
// Generate the full definition of an input struct item, e.g. "pub foo: u32"
// In the case of TaggedStructs/TaggedUnions, the output TokenStream consists
// of several definitions - one for each TaggedItem
fn generate_struct_item_definition(item: &DataItem) -> TokenStream {
    let mut def = quote!{};

    // preserve documentation comments from the specification into the output
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
            // output each TaggedItem
            for tgitem in tgitems {
                let mut curr_def = quote!{};
                // documentation comments are also possible on individual TaggedItems and are also output per item
                if let Some(comment) = &tgitem.item.comment {
                    curr_def.extend(quote!{#[doc=#comment]});
                }
                let tgitemname = format_ident!("{}", make_varname(&tgitem.tag));
                let typename = generate_bare_typename(&tgitem.item.typename, &tgitem.item.basetype);
                // The container type for the TaggedItems varies depending on the options
                if tgitem.repeat {
                    curr_def.extend(quote!{pub #tgitemname: Vec<#typename>});
                } else {
                    if tgitem.required {
                        curr_def.extend(quote!{pub #tgitemname: #typename});
                    } else {
                        curr_def.extend(quote!{pub #tgitemname: Option<#typename>});
                    }
                }

                tgdefs.push(curr_def);
            }
            def.extend(quote!{#(#tgdefs),*});
        }
        _ => {
            if item.varname.is_none() {
                panic!("bad varname for struct item {:#?}", item);
            }
            let itemname = format_ident!("{}", item.varname.as_ref().unwrap());
            let typename = generate_bare_typename(&item.typename, &item.basetype);
            def.extend(quote!{pub #itemname: #typename});
        }
    }
    def
}


// generate_bare_typename()
// generates the typename without other tokens (i.e. "u32" instead of "foo: u32") for a given struct item
fn generate_bare_typename(typename: &Option<String>, item: &BaseType) -> TokenStream {
    match item {
        BaseType::Char => { quote!{i8} }
        BaseType::Int => { quote!{i16} }
        BaseType::Long => { quote!{i32} }
        BaseType::Int64 => { quote!{i64} }
        BaseType::Uchar => { quote!{u8} }
        BaseType::Uint => { quote!{u16} }
        BaseType::Ulong => { quote!{u32} }
        BaseType::Uint64 => { quote!{u64} }
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
        BaseType::EnumRef |
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


// generate_struct_location_info()
// the location of each item needs to be tracked in order to be able to write elements on an
// a2l file in the same order they were in the input file.
// This function generates the definition of the __location_info tuple that stores these locations
// For example a struct with 3 items of type (enum, String, int) the generated tuple would look like this:
//    pub __location_info: ( Option<String>, u32, u32, u32, (u32, bool) )
fn generate_struct_location_info(structitems: &Vec<DataItem>) -> TokenStream {
    let mut locationtypes = Vec::new();
    locationtypes.push(quote!{Option<String>, u32});
    for item in structitems {
        match item.basetype {
            BaseType::Char |
            BaseType::Int |
            BaseType::Long |
            BaseType::Int64 |
            BaseType::Uchar |
            BaseType::Uint |
            BaseType::Ulong |
            BaseType::Uint64 |
            BaseType::Double |
            BaseType::Float |
            BaseType::Ident |
            BaseType::StructRef |
            BaseType::EnumRef |
            BaseType::Array(_, _) |
            BaseType::String |
            BaseType::Sequence(_) => {
                let item_location_info = generate_item_location_info(&item.basetype);
                locationtypes.push(item_location_info);
            }
            _ => {} // nothing to do for the other variants
        }
    }

    quote!{
        pub __location_info: ( #(#locationtypes),* )
    }
}


// generate_item_location_info()
// generate the part of the location info tuple that corresponds to one struct item
fn generate_item_location_info(item_basetype: &BaseType) -> TokenStream {
    match item_basetype {
        BaseType::Char |
        BaseType::Int |
        BaseType::Long |
        BaseType::Int64 |
        BaseType::Uchar |
        BaseType::Uint |
        BaseType::Ulong |
        BaseType::Uint64 => {
            // for integers the location info tracks a boolean flag that indicates if
            // the value should be displayed in hex format in addition to the line number
            quote!{ (u32, bool) }
        }
        BaseType::Double |
        BaseType::Float |
        BaseType::Ident |
        BaseType::StructRef |
        BaseType::EnumRef |
        BaseType::String => {
            quote!{u32}
        }
        BaseType::Array(arraytype, dim) => {
            if arraytype.basetype == BaseType::Char {
                // as usual, array of char is just treated as String
                quote!{u32}
            } else {
                let item_loc_info = generate_item_location_info(&arraytype.basetype);
                quote!{ [#item_loc_info; #dim] }
            }
        }
        BaseType::Sequence(seqtype) => {
            let item_loc_info = generate_item_location_info(seqtype);
            quote!{ Vec<#item_loc_info> }
        }
        _ => {
            // other variants have no location info
            quote!{}
        }
    }
}



// generate an impl of the Debug trait for each generated struct
// unfortunately this cannot be derived automatically because of the __location_info tuple
fn generate_block_data_structure_debug(typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut membernames = Vec::new();
    let mut structmembers = Vec::new();

    for item in structitems {
        match &item.basetype {
            BaseType::TaggedUnion(taggeditems) |
            BaseType::TaggedStruct(taggeditems) => {
                for tgitem in taggeditems {
                    let membername = make_varname(&tgitem.tag);
                    structmembers.push(format_ident!("{}", membername));
                    membernames.push(membername);
                }
            }
            _ => {
                let membername = item.varname.as_ref().unwrap();
                structmembers.push(format_ident!("{}", membername));
                membernames.push(membername.to_owned());
            }
        }
    }

    quote!{
        impl std::fmt::Debug for #typeident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#typename)
                #(.field(#membernames, &self.#structmembers))*
                .finish()
            }
        }
    }
}


// generate_block_data_structure_constructor()
// Generate a constructor new(...) for a struct.
// All of the required elements become arguments of new(), while the
// optional ones are automatically set to None or Vec::new()
fn generate_block_data_structure_constructor(typename: &str, structitems: &Vec<DataItem>, is_block: bool) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut newargs = Vec::<TokenStream>::new();
    let mut fieldinit = Vec::<TokenStream>::new();

    // initialize the line numbers of all elements with 0 for blocks, but with u32::MAX for keywords and structs
    // both values can't occur for elements loaded from a file and have special menings in the writer code
    let locationinfo_line = if is_block {
        // initialize line numbers as 0: place each element on a separate line
        0
    } else {
        // initialize line numbers as u32::MAX: place all elements on one line
        u32::MAX
    };
    let mut locationinfo = vec![quote!{None}, quote!{#locationinfo_line}];

    for item in structitems {
        match &item.basetype {
            BaseType::Sequence(_) => {
                let membername = item.varname.as_ref().unwrap();
                let memberident = format_ident!("{}", membername);
                fieldinit.push(quote!{#memberident: Vec::new()});
                locationinfo.push(generate_item_locationinfo_init(&item.basetype, locationinfo_line));
            }
            BaseType::TaggedUnion(taggeditems) |
            BaseType::TaggedStruct(taggeditems) => {
                // TaggedItems are optional items and have no initializers in the argument list of the new() function
                for tgitem in taggeditems {
                    let tgitemname = format_ident!("{}", make_varname(&tgitem.tag));
                    let typename = generate_bare_typename(&tgitem.item.typename, &tgitem.item.basetype);
                    if tgitem.repeat {
                        fieldinit.push(quote!{#tgitemname: Vec::new()});
                    } else {
                        if tgitem.required {
                            newargs.push(quote!{#tgitemname: #typename});
                            fieldinit.push(quote!{#tgitemname});
                        } else {
                            fieldinit.push(quote!{#tgitemname: None});
                        }
                    }
                }
            }
            _ => {
                // all required struct fields are initialized from arguments of new()
                // to enable that, the name of the field is pushed to the list of function arguments (e.g. "addess: u32")
                // then the struct can be initialized using the compact form (e.g. Self { .., address, ..})
                let membername = item.varname.as_ref().unwrap();
                let memberident = format_ident!("{}", membername);
                let membertype = generate_bare_typename(&item.typename, &item.basetype);
                newargs.push(quote!{#memberident: #membertype});
                fieldinit.push(quote!{#memberident});
                locationinfo.push(generate_item_locationinfo_init(&item.basetype, locationinfo_line));
            }
        }
    }
    fieldinit.push(quote!{__location_info: ( #(#locationinfo),* )});

    quote!{
        impl #typeident {
            pub fn new(#(#newargs),*) -> Self {
                Self {
                    #(#fieldinit),*
                }
            }
        }
    }
}


// generate_item_locationinfo_init()
// generate the initializer for one struct item in the location_info tuple of the new() function
fn generate_item_locationinfo_init(item_basetype: &BaseType, initline: u32) -> TokenStream {
    match item_basetype {
        BaseType::Char |
        BaseType::Int |
        BaseType::Long |
        BaseType::Int64 |
        BaseType::Uchar |
        BaseType::Uint |
        BaseType::Ulong |
        BaseType::Uint64 => {
            // for integers we track both the current line and the formatting of the number (hex or not)
            // by default no number will be output as hex
            quote!{ (#initline, false) }
        }
        BaseType::Double |
        BaseType::Float |
        BaseType::Ident |
        BaseType::StructRef |
        BaseType::EnumRef |
        BaseType::String => {
            // all other single items only need to track the line
            quote!{#initline}
        }
        BaseType::Array(arraytype, dim) => {
            if arraytype.basetype == BaseType::Char {
                quote!{#initline}
            } else {
                let item_loc_info_init = generate_item_locationinfo_init(&arraytype.basetype, initline);
                let loc_info_init_list: Vec<TokenStream> = (0..(*dim)).into_iter().map(|_| quote!{ #item_loc_info_init }).collect();
                quote!{ [#(#loc_info_init_list),*] }
            }
        }
        BaseType::Sequence(seqtype) => {
            let item_loc_info = generate_item_location_info(seqtype);
            quote!{ Vec::<#item_loc_info>::new() }
        }
        _ => {
            // other variants don't get here (panic?) and have no location info
            quote!{}
        }
    }
}


// generate_block_data_structure_partialeq()
// generate an impl of partialeq which compares all the regular struct members but ignores
// the line number information in self.__location_info
fn generate_block_data_structure_partialeq(typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut comparisons = Vec::<TokenStream>::new();

    for item in structitems {
        match &item.basetype {
            BaseType::TaggedUnion(taggeditems) |
            BaseType::TaggedStruct(taggeditems) => {
                for tgitem in taggeditems {
                    let tgitemname = format_ident!("{}", make_varname(&tgitem.tag));
                    comparisons.push(quote!{
                        (self.#tgitemname == other.#tgitemname)
                    });
                }
            }
            _ => {
                let itemname = format_ident!("{}", item.varname.as_ref().unwrap());
                comparisons.push(quote!{
                    (self.#itemname == other.#itemname)
                });
            }
        }
    }

    // some structs, e.g. DISCRETE and READ_ONLY have no content. They are always equal.
    if comparisons.len() == 0 {
        comparisons.push(quote!{true});
    }

    quote!{
        impl PartialEq for #typeident {
            fn eq(&self, other: &Self) -> bool {
                #(#comparisons)&&*
            }
        }
    }
}


// generate_block_data_structure_mergeincludes()
// generate fn merge_includes to merge included data into the main file
// merging included data is simple: set the origin of the block and all of it's optional
// children to None, as an origin that is Some(filename) indicates that the data comes from an include
fn generate_block_data_structure_mergeincludes(typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    let merge_commands = make_merge_commands(quote!{self}, structitems);

    quote!{
        impl #typeident {
            pub fn merge_includes(&mut self) {
                self.__location_info.0 = None;
                #(#merge_commands)*
            }
        }
    }
}


// make_merge_commands()
// generate the function calls inside a structs merge_includes() function that also merge the child elements
// this is only a concern for structs and optional elements, because other items do not track an include location
// and are always assumed to be inside the same file
fn make_merge_commands(name_prefix: TokenStream, structitems: &Vec<DataItem>) -> Vec<TokenStream> {
    let mut merge_commands = Vec::<TokenStream>::new();
    for item in structitems {
        match &item.basetype {
            BaseType::TaggedUnion(taggeditems) |
            BaseType::TaggedStruct(taggeditems) => {
                for tgitem in taggeditems {
                    let tgitemname = format_ident!("{}", make_varname(&tgitem.tag));
                    if tgitem.repeat {
                        merge_commands.push(quote!{
                            for #tgitemname in &mut #name_prefix.#tgitemname {
                                #tgitemname.merge_includes();
                            }
                        });
                    } else {
                        if tgitem.required {
                            merge_commands.push(quote!{
                                #name_prefix.#tgitemname.merge_includes();
                            });
                        } else {
                            merge_commands.push(quote!{
                                if let Some(#tgitemname) = &mut #name_prefix.#tgitemname {
                                    #tgitemname.merge_includes();
                                }
                            });
                        }
                    }
                }
            }
            BaseType::Block(structitems, _) |
            BaseType::Struct(structitems) => {
                let itemname = format_ident!("{}", item.varname.as_ref().unwrap());
                let newprefix = quote!{#name_prefix.#itemname};
                merge_commands.push(quote!{
                    #newprefix.__location_info.0 = None;
                });
                merge_commands.extend(make_merge_commands(newprefix, structitems));
            }
            _ => {}
        }
    }

    merge_commands
}


// generate the function to get the line number of the object
fn generate_block_data_structure_get_line(typename: &str) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    quote!{
        impl #typeident {
            pub fn get_line(&self) -> u32 {
                self.__location_info.1
            }
        }
    }
}


// generate the function to clear the location info (include filename and line number) of an object
// unlike merge_includes() this function does not operate recursively
fn generate_block_data_structure_reset_location(typename: &str, is_block: bool) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    let locationinfo_line = if is_block {
        // initialize line numbers as 0: place each element on a separate line
        0
    } else {
        // initialize line numbers as u32::MAX: place all elements on one line
        u32::MAX
    };

    quote!{
        impl #typeident {
            pub fn reset_location(&mut self) {
                self.merge_includes();
                self.__location_info.1 = #locationinfo_line;
            }
        }
    }
}

//-----------------------------------------------------------------------------

// generate_parser_impl
// generate parser function implementations for the set of types of A2l
// A2ml does not use this code, since it parses its data from intermediate GenericIfData structures
pub(crate) fn generate_parser(types: &HashMap<String, DataItem>) -> TokenStream {
    let mut result = quote!{};
    let mut typesvec: Vec<(&String, &DataItem)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.cmp(b.0));

    for (typename, a2mltype) in typesvec {
        match &a2mltype.basetype {
            BaseType::Enum(enumitems) => {
                result.extend(generate_enum_parser(typename, enumitems));
            }
            BaseType::Struct(structitems) => {
                result.extend(generate_block_parser(typename, structitems, false));
            }
            BaseType::Block(structitems, is_block) => {
                result.extend(generate_block_parser(typename, structitems, *is_block));
            }
            _ => {
                panic!("only block, struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, a2mltype);
            }
        }
    }
    result
}


// generate_enum_parser()
// generates a parser function that returns the enum variant matching the text of the current input token
fn generate_enum_parser(typename: &str, enumitems: &Vec<EnumItem>) -> TokenStream {
    let name = format_ident!("{}", typename);

    let mut match_branches = Vec::new();
    for enitem in enumitems {
        let enident = format_ident!("{}", ucname_to_typename(&enitem.name));
        let entag = &enitem.name;

        let mut version_check = quote! {};
        if let Some((min_ver, max_ver)) = enitem.version_range {
            version_check = quote!{
                parser.check_enumitem_version(context, #entag, #min_ver, #max_ver)?;
            };
        }

        match_branches.push(quote!{#entag => {
            #version_check
            Ok(Self::#enident)
        }});
    }

    quote!{
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
fn generate_block_parser(typename: &str, structitems: &Vec<DataItem>, is_block: bool) -> TokenStream {
    match typename {
        "A2ml" => {
            generate_block_parser_a2ml()
        }
        "IfData" => {
            generate_block_parser_ifdata()
        }
        _ => {
            generate_block_parser_generic(typename, structitems, is_block)
        }

    }
}


// generate_block_parser_a2ml()
// generate a parser function for the A2ml block
fn generate_block_parser_a2ml() -> TokenStream {
    quote! {
        impl A2ml {
            pub(crate) fn parse(parser: &mut ParserState, context: &ParseContext) -> Result<Self, ParseError> {
                let fileid = parser.get_incfilename(context.fileid);
                let line = context.line;

                let token = parser.expect_token(context, A2lTokenType::String)?;
                let __a2ml_text_location = token.line;
                let a2ml_text = parser.get_token_text(token).to_string();

                if let Ok(a2mlspec) = a2ml::parse_a2ml(&a2ml_text) {
                    parser.file_a2mlspec = Some(a2mlspec);
                }

                let blk = A2ml {
                    a2ml_text,
                    __location_info: (fileid, line, __a2ml_text_location)
                };

                parser.expect_token(context, A2lTokenType::End)?;
                let ident = parser.get_identifier(context)?;
                if ident != "A2ML" {
                    parser.error_or_log(ParseError::IncorrectEndTag(context.clone(), ident))?;
                }

                Ok(blk)
            }
        }
    }
}


// generate_block_parser_ifdata()
// generate a parser function for the IfData block
fn generate_block_parser_ifdata() -> TokenStream {
    quote! {
        impl IfData {
            pub(crate) fn parse(parser: &mut ParserState, context: &ParseContext) -> Result<Self, ParseError> {
                let fileid = parser.get_incfilename(context.fileid);
                let line = context.line;

                let ifdata_items = parser.parse_ifdata(context)?;

                let blk = IfData {
                    ifdata_items,
                    __location_info: (fileid, line)
                };

                parser.expect_token(context, A2lTokenType::End)?;
                let ident = parser.get_identifier(context)?;
                if ident != "IF_DATA" {
                    parser.error_or_log(ParseError::IncorrectEndTag(context.clone(), ident))?;
                }

                Ok(blk)
            }
        }
    }
}


// generate_block_parser_generic()
// generate a parser function for a block, keyword or struct
fn generate_block_parser_generic(typename: &str, structitems: &Vec<DataItem>, is_block: bool) -> TokenStream {
    let name = format_ident!("{}", typename);
    let (itemnames, itemparsers) = generate_struct_item_fragments(structitems);

    // check the block /end tag - blocks only, not for keywords or structs
    let blockcheck = if is_block {
        quote! {
            parser.expect_token(context, A2lTokenType::End)?;
            let ident = parser.get_identifier(context)?;
            if ident != context.element {
                parser.error_or_log(ParseError::IncorrectEndTag(context.clone(), ident))?;
            }
        }
    } else {
        quote!{}
    };

    quote! {
        impl #name {
            pub(crate) fn parse(parser: &mut ParserState, context: &ParseContext) -> Result<Self, ParseError> {
                let (__location_incfile, __location_line) = (parser.get_incfilename(context.fileid), context.line);
                #(#itemparsers)*
                let blk = Self {
                    #(#itemnames),*
                };

                #blockcheck

                Ok(blk)
            }
        }
    }
}


// generate_struct_item_fragments
// generate a list of struct elements as well as TokenStreams with code to parse these elements
fn generate_struct_item_fragments(structitems: &Vec<DataItem>) -> (Vec<Ident>, Vec<TokenStream>) {
    let mut itemparsers = Vec::<TokenStream>::new();
    let mut itemnames = Vec::<Ident>::new();
    let mut location_names = vec![format_ident!("__location_incfile"), format_ident!("__location_line")];
    for (idx, sitem) in structitems.iter().enumerate() {
        let is_last = idx == (structitems.len() - 1);
        match &sitem.basetype {
            BaseType::TaggedStruct(tg_items) => {
                itemparsers.push(generate_taggeditem_parser(tg_items, false, is_last));
                itemnames.extend(generate_tagged_item_names(tg_items));
            }
            BaseType::TaggedUnion(tg_items) => {
                itemparsers.push(generate_taggeditem_parser(tg_items, true, is_last));
                itemnames.extend(generate_tagged_item_names(tg_items));
            }
            BaseType::Sequence(seqitem) => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                itemparsers.push(generate_sequence_parser(&itemname, &sitem.typename, seqitem));
                location_names.push(format_ident!("__{}_location", itemname));
                itemnames.push(itemname);
            }
            _ => {
                let itemname = format_ident!("{}", sitem.varname.clone().unwrap());
                let itemname_location = format_ident!("__{}_location", itemname);
                let itemparser = generate_item_parser_call(&sitem.typename, &sitem.basetype);
                itemparsers.push(quote!{let (#itemname_location, #itemname) = #itemparser;});
                location_names.push(itemname_location);
                itemnames.push(itemname);
            }
        }
    }

    itemnames.push(format_ident!("__location_info"));
    itemparsers.push(quote!{
        let __location_info = ( #(#location_names),* );
    });

    (itemnames, itemparsers)
}


// generate_item_parser_call
// generates code to call an existing item parser function
// each item parser fragment evaluates as a tuple (locationinfo, value)
fn generate_item_parser_call(typename: &Option<String>, item: &BaseType) -> TokenStream {
    match item {
        BaseType::Char |
        BaseType::Int |
        BaseType::Long |
        BaseType::Int64 |
        BaseType::Uchar |
        BaseType::Uint |
        BaseType::Ulong |
        BaseType::Uint64 => {
            let intparser = get_int_parser(item);
            quote!{{
                let line = parser.get_current_line();
                let (value, is_hex) = #intparser(context)?;
                ((line, is_hex), value)
            }}
        }
        BaseType::Double => { quote!{(parser.get_current_line(), parser.get_double(context)?)} }
        BaseType::Float => { quote!{(parser.get_current_line(), parser.get_float(context)?)} }
        BaseType::Ident => { quote!{(parser.get_current_line(), parser.get_identifier(context)?)} }
        BaseType::String => { quote!{(parser.get_current_line(), parser.get_string(context)?)} }
        BaseType::Array(arraytype, dim) => {
            if let BaseType::Char = arraytype.basetype {
                quote!{parser.get_string_maxlen(context, #dim)?}
            } else {
                let itemparser = generate_item_parser_call(&arraytype.typename, &arraytype.basetype);
                let names: Vec<Ident> = (0..(*dim)).into_iter().map(
                    |x| format_ident!("__arrayitem_{}", x)
                ).collect();
                let parsercalls = names.iter().map(
                    |name| quote!{let #name = #itemparser;}
                );
                quote!{{
                    #(#parsercalls)*
                    ([ #(#names.0),* ], [ #(#names.1),*])
                }}
            }
        }
        BaseType::EnumRef |
        BaseType::StructRef => {
            let typename = typename.as_ref().unwrap();
            let name = format_ident!("{}", typename);
            quote!{ (parser.get_current_line(), #name::parse(parser, context)?) }
        }
        _ => { panic!("forbidden type: {:#?}", item); }
    }
}


// get_int_parser()
// simplify the handling of the integer case in generate_item_parser_call
fn get_int_parser(item: &BaseType) -> TokenStream {
    match item {
        BaseType::Char => { quote!{parser.get_integer_i8} }
        BaseType::Int => { quote!{parser.get_integer_i16} }
        BaseType::Long => { quote!{parser.get_integer_i32} }
        BaseType::Int64 => { quote!{parser.get_integer_i64} }
        BaseType::Uchar => { quote!{parser.get_integer_u8} }
        BaseType::Uint => { quote!{parser.get_integer_u16} }
        BaseType::Ulong => { quote!{parser.get_integer_u32} }
        BaseType::Uint64 => { quote!{parser.get_integer_u64} }
        _ => panic!("call of get_int_parser only allowed for integer types")
    }
}


// generate_sequence_parser
// Generates a TokenStream with code to greedily parse elements of a sequence
// Parsing of sequence items continues until the parser function for the current sequence item returns an error
fn generate_sequence_parser(itemname: &Ident, typename: &Option<String>, seqitem: &BaseType) -> TokenStream {
    let parserfunc = generate_item_parser_call(typename, seqitem);
    let itemname_location = format_ident!("__{}_location", itemname);
    quote!{
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
fn generate_taggeditem_parser(tg_items: &Vec<TaggedItem>, is_taggedunion: bool, is_last: bool) -> TokenStream {
    // result: the TokenStream that ultimately collcts all the code fragements in this function
    let mut result = quote!{};

    // item_match_arms: the match arms of the while loop that passes each set of input tokens to the appropriate item parser
    // multiplicity_check: code fragemnts that check if references marked as required are present
    let (var_definitions, item_match_arms, multiplicity_check) = generate_taggeditem_match_arms(tg_items);
    result.extend(var_definitions);


    // generate the full match statement that has one arm for each tgitem
    let parser_core = generate_taggeditem_parser_core(tg_items, is_taggedunion, is_last, &item_match_arms);

    // wrap the match statement inside an if or a while loop
    if is_taggedunion {
        result.extend(quote!{
            let mut next_tag = parser.get_next_tag(context)?;
            if next_tag.is_some() {
                #parser_core
            }
        });
    } else {
        result.extend(quote!{
            let mut next_tag = parser.get_next_tag(context)?;
            while next_tag.is_some() {
                #parser_core
                next_tag = parser.get_next_tag(context)?;
            }
        });
    }

    // now that all items have been parsed, the check if all required items are present can be performed
    result.extend(quote!{
        #multiplicity_check
    });

    result
}


// generate_taggeditem_match_arms()
// a match statement is used in order to parse the taggeditems of a TaggedStruct / TaggedUnion
// This function generates all of the match arms of the match expression
// In order to use the result of the match arms, we also need the definition of the generated variables.
fn generate_taggeditem_match_arms(tg_items: &Vec<TaggedItem>) -> (TokenStream, Vec<TokenStream>, TokenStream) {
    let mut var_definitions = quote!{};
    // item_match_arms: the match arms of the while loop that passes each set of input tokens to the appropriate item parser
    let mut item_match_arms = Vec::new();
    // multiplicity_check: code fragemnts that check if references marked as required are present
    let mut multiplicity_check = quote!{};

    for item in tg_items {
        let tmp_itemname = format_ident!("tmp_required__{}", make_varname(&item.tag));
        let itemname = format_ident!("{}", make_varname(&item.tag));
        let typename = generate_bare_typename(&item.item.typename, &item.item.basetype);
        let store_item; // a code fragment that stores the parsed item into an Option<T> or a Vec<T>
        let tag_string = &item.tag;

        if item.repeat {
            // repeated items are represented as Vec<TypeName>
            var_definitions.extend(quote!{let mut #itemname: Vec<#typename> = Vec::new();});
            store_item = quote!{
                #itemname.push(newitem);
            };
            if item.required {
                multiplicity_check.extend(quote!{
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
                var_definitions.extend(quote!{let mut #tmp_itemname: Option<#typename> = None;});
                store_item = quote!{
                    if #tmp_itemname.is_none() {
                        #tmp_itemname = Some(newitem);
                    } else {
                        parser.error_or_log(ParseError::InvalidMultiplicityTooMany(context.clone(), #tag_string.to_string()))?;
                    }
                };
                // during the mutliplicity check the required item can be unwrapped from the Option
                multiplicity_check.extend(quote!{
                    let #itemname = if let Some(value) = #tmp_itemname {
                        value
                    } else {
                        return Err(ParseError::InvalidMultiplicityNotPresent(context.clone(), #tag_string.to_string()));
                    };
                });
            } else {
                // an non-repeating item that is not required
                var_definitions.extend(quote!{let mut #itemname: Option<#typename> = None;});
                store_item = quote!{
                    if #itemname.is_none() {
                        #itemname = Some(newitem);
                    } else {
                        parser.error_or_log(ParseError::InvalidMultiplicityTooMany(context.clone(), #tag_string.to_string()))?;
                    }
                };
            }
        }

        let mut version_check = quote!{};
        if let Some((min_ver, max_ver)) = item.version_range {
            version_check.extend(quote!{
                parser.check_block_version(context, #tag_string, #min_ver, #max_ver)?;
            });
        }

        let is_block_item = item.is_block;
        item_match_arms.push(
            quote!{
                #tag_string => {
                    #version_check
                    let newitem = #typename::parse(parser, &newcontext)?;
                    #store_item
                    if #is_block_item != is_block {
                        parser.error_or_log(ParseError::IncorrectElemType(context.clone(), #tag_string.to_string(), #is_block_item))?;
                    }
                }
            }
        );
    }

    (var_definitions, item_match_arms, multiplicity_check)
}


// generate_taggeditem_parser_core()
// generate the match statement for parsing TaggedStructs and TaggedUnions
fn generate_taggeditem_parser_core(tg_items: &Vec<TaggedItem>, is_taggedunion: bool, is_last: bool, item_match_arms: &Vec<TokenStream>) -> TokenStream {
    // default action if a tag is not recognized: step back in the tokenstream and let it be handled somewhere else
    let mut default_match_arm = quote!{
        if is_block {
            parser.undo_get_token();
        }
        parser.undo_get_token();
    };

    if !is_taggedunion {
        // taggedstructs use a while loop; if parsing fails the loop needs to end
        default_match_arm.extend(quote!{break;});
    }

    // if this taggedstruct / taggedunion is the last element in the block
    // and this block (at runtime) is actually inside /begin ...  /end, then there is no way to let the unknown tag to be handled somewhere else
    if is_last {
        default_match_arm = quote!{
            if context.inside_block {
                parser.handle_unknown_taggedstruct_tag(context, tag, is_block, &TAG_LIST)?;
            } else {
                #default_match_arm
            }
        };
    }

    let taglist: Vec<String> = tg_items.iter().map(|item| item.tag.clone() ).collect();
    let taglist_len = taglist.len();
    // generate the full match statement
    quote!{
        let (token, is_block) = next_tag.unwrap();
        let tag = parser.get_token_text(token);
        let newcontext = ParseContext::from_token(tag, token, is_block);
        const TAG_LIST: [&str; #taglist_len] = [#(#taglist),*];
        match tag {
            #(#item_match_arms)*
            _ => {
                #default_match_arm
            }
        }
    }
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


// generate_writer()
// Generate a pub fn write() function for all data types in the specification
pub(crate) fn generate_writer(types: &HashMap<String, DataItem>) -> TokenStream {
    let mut result = quote!{};
    let mut typesvec: Vec<(&String, &DataItem)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.cmp(b.0));

    for (typename, a2mltype) in typesvec {
        match &a2mltype.basetype {
            BaseType::Enum(enumitems) => {
                result.extend(generate_enum_writer(typename, enumitems));
            }
            BaseType::Struct(structitems) => {
                result.extend(generate_struct_writer(typename, structitems));
            }
            BaseType::Block(structitems, _) => {
                result.extend(generate_block_writer(typename, structitems));
            }
            _ => {
                panic!("only block, struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, a2mltype);
            }
        }
    }
    result
}


// generate_enum_writer()
// For enums it actually makes more sense to implement the trait std::fmt::Display, than to have a non-standard write() function
fn generate_enum_writer(typename: &str, enumitems: &Vec<EnumItem>) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut match_arms = Vec::new();
    for item in enumitems {
        let enident = format_ident!("{}", ucname_to_typename(&item.name));
        let entag = &item.name;
        // each match arm is something like:  Self::WhateverValue => "WHATEVER_VALUE",
        match_arms.push(quote!{Self::#enident => #entag});
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
fn generate_block_writer(typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    match typename {
        "A2ml" => {
            generate_block_writer_a2ml()
        }
        "IfData" => {
            generate_block_writer_ifdata()
        }
        _ => {
            generate_block_writer_generic(typename, structitems)
        }
    }
}


// generate_block_writer_a2ml()
// The a2ml contains the full A2ML specification, which can be written as one string
fn generate_block_writer_a2ml() -> TokenStream {
    quote!{
        impl A2ml {
            pub(crate) fn write(&self) -> writer::Writer {
                let mut writer = writer::Writer::new(&self.__location_info.0, self.__location_info.1);
                writer.add_fixed_item(self.a2ml_text.to_owned(), self.__location_info.2);
                writer
            }
        }
    }
}


// generate_block_writer_ifdata()
// Gnerate the write() function for an IfData block, which can contain a GenericIfData item.
fn generate_block_writer_ifdata() -> TokenStream {
    quote!{
        impl IfData {
            pub(crate) fn write(&self) -> writer::Writer {
                if let Some(ifdata_items) = &self.ifdata_items {
                    let outval = ifdata_items.write(&self.__location_info.0, self.__location_info.1);
                    outval
                } else {
                    writer::Writer::new(&self.__location_info.0, self.__location_info.1)
                }
            }
        }
    }
}


// generate_block_writer_generic()
// Generate a write() function for a block
// Each block instantiates its own Writer and uses it to write all of its child elements
fn generate_block_writer_generic(typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    let typeident = format_ident!("{}", typename);

    let write_items = generate_block_item_writers(structitems);

    quote! {
        impl #typeident {
            pub(crate) fn write(&self) -> writer::Writer {
                let mut writer = writer::Writer::new(&self.__location_info.0, self.__location_info.1);

                #(#write_items)*

                writer
            }
        }
    }
}


// generate_struct_writer()
// Generate a write function for a struct which occurs inside of a block.
// The struct is written using the parent block's Writer
fn generate_struct_writer(typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    let typeident = format_ident!("{}", typename);

    let write_items = generate_block_item_writers(structitems);

    quote! {
        impl #typeident {
            pub(crate) fn write(&self, writer: &mut writer::Writer) {
                // if this struct is part of a sequence of structs (e.g. inside of COMPU_VTAB), then
                // the writer needs additional info in order to insert sane line breaks for new items
                writer.add_break_if_new_item(self.__location_info.1);
                #(#write_items)*
            }
        }
    }
}


// generate_block_item_writers()
// Generate a write command for every item in the block.
fn generate_block_item_writers(structitems: &Vec<DataItem>) -> Vec<TokenStream> {
    let mut write_items = Vec::<TokenStream>::new();
    let mut posidx: usize = 2;

    for item in structitems {
        let posliteral = Literal::usize_unsuffixed(posidx);
        let location = quote! {self.__location_info.#posliteral};
        match &item.basetype {
            BaseType::TaggedUnion(taggeditems) |
            BaseType::TaggedStruct(taggeditems) => {
                let mut tgwriters = Vec::new();
                let mut tgcount = Vec::new();
                for tgitem in taggeditems {
                    let tag = &tgitem.tag;
                    let tgname = format_ident!("{}", make_varname(&tgitem.tag));
                    let tgname_out = format_ident!("{}_out", tgname);
                    let is_block = tgitem.is_block;
                    if tgitem.repeat {
                        tgwriters.push(quote!{
                            for #tgname in &self.#tgname {
                                let #tgname_out = #tgname.write();
                                tgroup.add_tagged_item(#tag, #tgname_out, #is_block);
                            }
                        });
                        tgcount.push(quote!{self.#tgname.len()});
                    } else {
                        if tgitem.required {
                            tgwriters.push(quote!{
                                let #tgname_out = self.#tgname.write();
                                tgroup.add_tagged_item(#tag, #tgname_out, #is_block);
                            });
                        } else {
                            tgwriters.push(quote!{
                                if let Some(#tgname) = &self.#tgname {
                                    let #tgname_out = #tgname.write();
                                    tgroup.add_tagged_item(#tag, #tgname_out, #is_block);
                                }
                            });
                        }
                        tgcount.push(quote!{1});
                    }
                   
                }
                write_items.push(quote!{
                    let mut tgroup = writer.add_tagged_group(#(#tgcount)+*);
                    #(#tgwriters)*
                });
            }
            _ => {
                let itemident = format_ident!("{}", item.varname.as_ref().unwrap() );
                let write_cmd = generate_block_item_write_cmd(&item.basetype, quote!{self.#itemident}, location, 0);
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
        BaseType::Uchar => { quote!{ writer.add_fixed_item(writer::format_u8((#itemname, #location.1)), #location.0); } }
        BaseType::Char => { quote!{ writer.add_fixed_item(writer::format_i8((#itemname, #location.1)), #location.0); } }
        BaseType::Uint => { quote!{ writer.add_fixed_item(writer::format_u16((#itemname, #location.1)), #location.0); } }
        BaseType::Int => { quote!{ writer.add_fixed_item(writer::format_i16((#itemname, #location.1)), #location.0); } }
        BaseType::Ulong => { quote!{ writer.add_fixed_item(writer::format_u32((#itemname, #location.1)), #location.0); } }
        BaseType::Long => { quote!{ writer.add_fixed_item(writer::format_i32((#itemname, #location.1)), #location.0); } }
        BaseType::Uint64 => { quote!{ writer.add_fixed_item(writer::format_u64((#itemname, #location.1)), #location.0); } }
        BaseType::Int64 => { quote!{ writer.add_fixed_item(writer::format_i64((#itemname, #location.1)), #location.0); } }
        BaseType::Double => { quote!{ writer.add_fixed_item(writer::format_double(#itemname), #location); } }
        BaseType::Float => { quote!{ writer.add_fixed_item(writer::format_float(#itemname), #location); } }
        BaseType::Ident => { quote!{
            writer.add_fixed_item(#itemname.to_owned(), #location);
        } }
        BaseType::String => { quote!{
            writer.add_fixed_item(format!("\"{}\"", writer::escape_string(&#itemname)), #location);
        } }
        BaseType::EnumRef => { quote!{
            writer.add_fixed_item(#itemname.to_string(), #location);
        } }
        BaseType::StructRef => { quote!{
            #itemname.write(&mut writer);
        } }
        BaseType::Array(arraytype, dim) => {
            if let BaseType::Char = arraytype.basetype {
                generate_block_item_write_cmd(&BaseType::String, itemname, location, calldepth)
            } else {
                // assign an individual idxident based on the calldepth. This enables nested arrays.
                let idxident = format_ident!("idx{}", calldepth);
                let arrayelemname = quote!{ #itemname[#idxident] };
                let locationname = quote!{ #location[#idxident] };
                let write_cmd = generate_block_item_write_cmd(&arraytype.basetype, arrayelemname, locationname, calldepth + 1);
                quote!{
                    for #idxident in 0..#dim {
                        // the #write_cmd inside of this loop uses #idxident to index into the array
                        #write_cmd
                    }
                }
            }
        }
        BaseType::Sequence(seqtype) => {
            let seqitemident = format_ident!("seqitem{}", calldepth);
            let seqidxident = format_ident!("seqidx{}", calldepth);
            let seqlocation = quote!{#location[#seqidxident]};

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
                BaseType::Uint64 => generate_block_item_write_cmd(&seqtype, quote!{*#seqitemident}, seqlocation, calldepth + 1),
                _                => generate_block_item_write_cmd(&seqtype, quote!{#seqitemident}, seqlocation, calldepth + 1),
            };
            quote!{
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


//-----------------------------------------------------------------------------

// manual implementation of PartialEq to ignore comments when comparing for equality
impl PartialEq for DataItem {
    fn eq(&self, other: &Self) -> bool {
        self.typename == other.typename && self.basetype == other.basetype && self.varname == other.varname
    }
}
