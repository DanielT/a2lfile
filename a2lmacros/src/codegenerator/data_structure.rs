use proc_macro2::TokenStream;
use quote::format_ident;
use quote::quote;

use super::*;

// generate_data_structures()
// generate the struct and enum definitions for all inputs in types
// Also generate a new() for each type as well as functions for impl Debug and impl PartialEq
pub(crate) fn generate(typename: &String, dataitem: &DataItem) -> TokenStream {
    let mut result = quote!{};

    if let Some(comment) = &dataitem.comment {
        result.extend(quote!{#[doc=#comment]});
    }

    match &dataitem.basetype {
        BaseType::Enum(enumitems) => {
            result.extend(generate_enum_data_structure(typename, enumitems));
        }
        BaseType::Struct(structitems) => {
            result.extend(generate_block_data_structure_generic(typename, structitems, true, false));
        }
        BaseType::Block(structitems, is_block) => {
            result.extend(generate_block_data_structure(typename, structitems, *is_block));
        }
        _ => {
            panic!("only block, struct and enum are allowed as top-level types, but {} = {:#?} was encountered", typename, dataitem);
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
        #[derive(Debug, PartialEq, Eq, Copy, Clone)]
        pub enum #typeident {
            #(#enumidents),*
        }
    }
}


// generate_block_data_structure()
// Generate the data structure of a block (if is_block is false it is a keyword instead, but still almost the same)
// The A2ml and IfData blocks are special in the specification and are implemented by hand
fn generate_block_data_structure(typename: &str, structitems: &Vec<DataItem>, is_block: bool) ->  TokenStream {
    match typename {
        "A2ml" | "IfData" => { quote!{} }
        _ => {
            generate_block_data_structure_generic(typename, structitems, false, is_block)
        }
    }
}


// generate_block_data_structure_generic()
// generate the data structure and associated functions for all except A2ml and IfData
fn generate_block_data_structure_generic(typename: &str, structitems: &Vec<DataItem>, is_struct: bool, is_block: bool) ->  TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut definitions = Vec::new();

    for item in structitems {
        definitions.push(generate_struct_item_definition(item));
    }
    let location_spec = generate_struct_block_location(structitems);
    definitions.push(quote!{pub(crate) __block_info: BlockInfo<#location_spec>});


    // generate all of the utility functions together with the data structure
    // only write and parse are excluded here, because they are not shared between A2l and A2ml
    let debug = generate_block_data_structure_debug(typename, structitems);
    let constructor = generate_block_data_structure_constructor(typename, structitems, is_struct, is_block);
    let partialeq = generate_block_data_structure_partialeq(typename, structitems);
    let trait_location = generate_block_data_structure_trait_location(typename, structitems, location_spec);
    let trait_name = generate_block_data_structure_trait_name(typename, structitems);

    quote!{
        #[derive(Clone)]
        pub struct #typeident {
            #(#definitions),*
        }

        #debug
        #constructor
        #partialeq
        #trait_location
        #trait_name
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


// generate_struct_block_location()
// the location of each item needs to be tracked in order to be able to write elements on an
// a2l file in the same order they were in the input file.
// This function generates the definition of the __block_info tuple that stores these locations
// For example a struct with 3 items of type (enum, String, int) the generated tuple would look like this:
//    ( u32, u32, (u32, bool) )
fn generate_struct_block_location(structitems: &Vec<DataItem>) -> TokenStream {
    let mut locationtypes = Vec::new();
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

    // make sure we don't try to make a tuple of one item. Add () as an extra tuple item if required
    if locationtypes.len() == 1 {
        locationtypes.push(quote!{ () });
    }

    quote!{
        ( #(#locationtypes),* )
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
// unfortunately this cannot be derived automatically because of the __block_info
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
fn generate_block_data_structure_constructor(typename: &str, structitems: &Vec<DataItem>, is_struct: bool, is_block: bool) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    let mut newargs = Vec::<TokenStream>::new();
    let mut fieldinit = Vec::<TokenStream>::new();

    let mut locationinfo = Vec::new();

    let named_block = if is_block && structitems.len() > 1 && structitems[0].basetype == BaseType::Ident && structitems[1].basetype == BaseType::String {
        true
    } else {
        false
    };

    for item in structitems {
        let initline = if named_block && locationinfo.len() == 2 {
            // if it's a named block, then the first two items go on the same line as /begin FOO, followed by a line break
            1
        } else if is_struct && locationinfo.len() == 0 {
            // structs have a line break before the first element
            1
        } else if !named_block && is_block && locationinfo.len() == 0 {
            // any other random block has the line break right after /begin FOO
            1
        } else {
            0
        };

        match &item.basetype {
            BaseType::Sequence(_) => {
                let membername = item.varname.as_ref().unwrap();
                let memberident = format_ident!("{}", membername);
                fieldinit.push(quote!{#memberident: Vec::new()});
                // a sequence should always start on a separate line (initline = 1)
                locationinfo.push(generate_item_locationinfo_init(&item.basetype, 1));
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
                locationinfo.push(generate_item_locationinfo_init(&item.basetype, initline));
            }
        }
    }

    // if there is only one item in the location info tuple, an extra () is added
    if locationinfo.len() == 1 {
        locationinfo.push(quote!{ () });
    }

    let start_offset: u32 = if is_block { 2 } else { 1 };
    fieldinit.push(quote!{__block_info: BlockInfo {
            incfile: None,
            line: 0,
            uid: 0,
            start_offset: #start_offset,
            end_offset: 1,
            item_location: ( #(#locationinfo),* )
        }
    });

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
                let item_loc_info_init = generate_item_locationinfo_init(&arraytype.basetype, 0);
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
// the layout information in self.__block_info
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
                    #newprefix.__block_info.incfile = None;
                });
                merge_commands.extend(make_merge_commands(newprefix, structitems));
            }
            _ => {}
        }
    }

    merge_commands
}


fn generate_block_data_structure_trait_location(typename: &str, structitems: &Vec<DataItem>, location_spec: TokenStream) -> TokenStream {
    let typeident = format_ident!("{}", typename);
    let merge_commands = make_merge_commands(quote!{self}, structitems);
 
    quote!{
        impl A2lObject<#location_spec> for #typeident {
            fn get_layout(&self) -> &BlockInfo<#location_spec> {
                &self.__block_info
            }

            fn get_layout_mut(&mut self) -> &mut BlockInfo<#location_spec> {
                &mut self.__block_info
            }

            // clear the location info (include filename and uid) of an object
            // unlike merge_includes() this function does not operate recursively
            fn reset_location(&mut self) {
                self.merge_includes();
                self.__block_info.uid = 0;
            }

            fn merge_includes(&mut self) {
                self.__block_info.incfile = None;
                #(#merge_commands)*
            }

            fn get_line(&self) -> u32 {
                self.__block_info.line
            }
        }
    }
}


fn generate_block_data_structure_trait_name(typename: &str, structitems: &Vec<DataItem>) -> TokenStream {
    let typeident = format_ident!("{}", typename);

    if structitems.len() > 0 && structitems[0].basetype == BaseType::Ident && structitems[0].varname == Some("name".to_string()) {
        quote!{
            impl A2lObjectName for #typeident {
                fn get_name(&self) -> &str {
                    &self.name
                }
            }
        }
    } else {
        quote!{}
    }
}
