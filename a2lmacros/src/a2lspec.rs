use proc_macro::TokenStream;
use proc_macro::TokenTree;
use proc_macro::Delimiter;
use std::collections::HashSet;
use std::collections::HashMap;
use quote::quote;
use quote::format_ident;
use super::util::*;

#[derive(Debug)]
struct Block {
    name: String,
    is_keyword: bool,
    items: Vec<BlockItem>,
    references: Vec<BlockRef>,
    contains_a2ml: bool,
    contains_ifdata: bool
}


#[derive(Debug, Clone)]
enum BlockItemData {
    Single(String),
    Group(Vec<String>)
}


#[derive(Debug, Clone)]
struct BlockItem {
    datatype: BlockItemData,
    name: String,
    arraydim: u32
}

#[derive(Debug, Clone, PartialEq)]
enum Multiplicity {
    One,
    ZeroOrOne,
    Many,
    Any
}

#[derive(Debug, Clone)]
struct BlockRef {
    reference_name: String, // e.g. AXIS_PTS_X
    ref_typename: String, // e.g. AxisPts
    ref_varname: String, // e.g. axis_pts_x
    multiplicity: Multiplicity
}

#[derive(Debug)]
struct Enum {
    name: String,
    items: Vec<String>
}

#[derive(Debug)]
struct Specification {
    blocks: Vec<Block>,
    enums: Vec<Enum>
}


//----------------------------------------------------------------------------------------------------


pub(crate) fn a2l_specification(tokens: TokenStream) -> TokenStream {
    let mut iter: TokenStreamIter = tokens.into_iter().peekable();
    let spec = parse_specification(&mut iter);

    consistency_check(&spec);

    let mut result = generate_data_structures(&spec);
    result.extend(generate_parser_functions(&spec));
    result.into()
}

//----------------------------------------------------------------------------------------------------


fn parse_specification(token_iter: &mut TokenStreamIter) -> Specification {
    let mut spec = Specification {
        blocks: Vec::new(),
        enums: Vec::new()
    };

    while token_iter.peek().is_some() {
        let ident = get_ident(token_iter);
        match &*ident {
            "block" => {
                spec.blocks.push(parse_block(token_iter, false));
            },
            "keyword" => {
                spec.blocks.push(parse_block(token_iter, true));
            },
            "enum" => {
                spec.enums.push(parse_enum(token_iter))
            },
            _ => { panic!("expected a keyword of the a2l specification in this position, got {}", ident); }
        }
    }

    spec
}


fn parse_block(token_iter: &mut TokenStreamIter, is_keyword: bool) -> Block {
    let mut items = Vec::new();
    let mut references = Vec::new();
    let mut contains_a2ml = false;
    let mut contains_ifdata = false;

    let namelist = parse_blockname(token_iter);
    let name = typename_from_names(&namelist);

    let block_tokens = get_group(token_iter, Delimiter::Brace);
    let mut block_token_iter = block_tokens.into_iter().peekable();

    while block_token_iter.peek().is_some() {
        let nexttok = block_token_iter.peek().unwrap();
        match nexttok {
            TokenTree::Ident(_) => {
                // a single item (uint foo)
                items.push(parse_blockitem_single(&mut block_token_iter));
            }
            TokenTree::Group(grp) => {
                match grp.delimiter() {
                    Delimiter::Brace => {
                        // a group of repeated items ({string, uint}* foo)
                        items.push(parse_blockitem_group(&mut block_token_iter));
                    }
                    Delimiter::Bracket => {
                        // a reference to another block ([-> BLOCK_NAME])
                        references.append(&mut parse_optitem(&mut block_token_iter));
                    }
                    _ => {}
                }
            }
            TokenTree::Punct(_) => {
                let p = get_punct(&mut block_token_iter);
                if p == '#' {
                    // an attribute. #[A2ML] marks the block containing the a2ml specification; #[IFDATA] marks a block that is allowed to use elelemts defined in a2ml
                    let attribute_tokens = get_group(&mut block_token_iter, Delimiter::Bracket);
                    let mut attr_token_iter = attribute_tokens.into_iter().peekable();
                    let attr = get_ident(&mut attr_token_iter);
                    match &*attr {
                        "A2ML" => contains_a2ml = true,
                        "IFDATA" => contains_ifdata = true,
                        "doc" => {/* doc comment, ignore */},
                        _ => panic!("unknown attribute {} inside block {}",attr, namelist[0])
                    }
                } else {
                    panic!("Punctuation character {} is not allowed in this position while parsing the definition of {}", p, namelist[0]);
                }
            }
            _ => panic!("{:#?} is not allowed in this position while parsing the definition of {}", nexttok.to_string(), namelist[0])
        }
    }

    Block {
        name,
        is_keyword,
        items,
        references,
        contains_a2ml,
        contains_ifdata
    }
}


// it is possible to specify that one block is referenced under multiple names, e.g.  AXIS_PTS_X / _Y / _Z / _4 / _5
// this function constructs all the full names from the pattern and returns them in a Vec
fn parse_blockname(token_iter: &mut TokenStreamIter) -> Vec<String> {
    let mut names = Vec::new();
    names.push(get_ident(token_iter));
    let name0chars: Vec<char> = names[0].chars().collect();

    loop {
        let nextitem = token_iter.peek();
        if let Some(TokenTree::Punct(_)) = nextitem {
            require_punct(token_iter, '/');
            let suffix = get_ident(token_iter);
            let mut suffixchars: Vec<char> = suffix.chars().collect();
            let mut newname = name0chars[0..(name0chars.len()-suffixchars.len())].to_vec();
            newname.append(&mut suffixchars);
            names.push(newname.iter().collect());
        } else {
            break;
        }
    }

    names
}


fn parse_blockitem_single(block_token_iter: &mut TokenStreamIter) -> BlockItem {
    let mut arraydim = 1;
    let datatype = BlockItemData::Single(get_ident(block_token_iter));

    if let Some(TokenTree::Group(g)) = block_token_iter.peek() {
        if g.delimiter() == Delimiter::Bracket {
            let arrspec_tokens = get_group(block_token_iter, Delimiter::Bracket);
            if let Some(TokenTree::Literal(lit)) = arrspec_tokens.into_iter().next() {
                arraydim = lit.to_string().parse().unwrap();
            }
        }
    }

    BlockItem {
        datatype,
        name: get_ident(block_token_iter),
        arraydim
    }
}


fn parse_blockitem_group(block_token_iter: &mut TokenStreamIter) -> BlockItem {
    let grp_tokens = get_group(block_token_iter, Delimiter::Brace);
    let mut grp_token_iter = grp_tokens.into_iter().peekable();
    require_punct(block_token_iter, '*');
    let name = get_ident(block_token_iter);
    let mut groupitems = Vec::new();
    loop {
        if grp_token_iter.peek().is_none() {
            break;
        }
        groupitems.push(get_ident(&mut grp_token_iter));
        /* if thee are further items in the enum, there must be a ',' as a separator */
        if grp_token_iter.peek().is_some() {
            require_punct(&mut grp_token_iter, ',');
        }
    }
    BlockItem {
        datatype: BlockItemData::Group(groupitems),
        name,
        arraydim: 1
    }
}


fn parse_optitem(block_token_iter: &mut TokenStreamIter) -> Vec<BlockRef> {
    let mut blocks: Vec<BlockRef> = Vec::new();

    let ref_tokens = get_group(block_token_iter, Delimiter::Bracket);
    let mut ref_token_iter = ref_tokens.into_iter().peekable();
    require_punct(&mut ref_token_iter, '-');
    require_punct(&mut ref_token_iter, '>');
    let blocknames = parse_blockname(&mut ref_token_iter);
    let mut refnames: Vec<String> = blocknames.iter().map(|bn| bn.to_ascii_lowercase()).collect();

    let mut multiplicity = Multiplicity::ZeroOrOne;
    if block_token_iter.peek().is_some() {
        let nextitem = block_token_iter.peek().unwrap().clone();
        if let TokenTree::Punct(punctitem) = nextitem {
            block_token_iter.next();
            multiplicity = match punctitem.as_char() {
                '!' => Multiplicity::One,
                '+' => Multiplicity::Many,
                '*' => Multiplicity::Any,
                _ => panic!("multiplicity constraints must be one of !+*")
            }
        }
    }

    // support renaming the autogenerated var name; e.g. VIRTUAL can now use virtual_measurement instead of failing to use the reserved word virtual
    // this mechanism doe not support references with mutliple variants (_X / _Y / _Z, etc.) because that is currently not needed anywhere
    if let Some(TokenTree::Ident(id)) = block_token_iter.peek() {
        if id.to_string() == "as" {
            block_token_iter.next();
            refnames[0] = get_ident(block_token_iter);
        }
    }

    let ref_typename = typename_from_names(&blocknames);
    for (idx, blockname) in blocknames.iter().enumerate() {
        blocks.push(BlockRef {
            reference_name: blockname.to_owned(),
            ref_typename: ref_typename.to_owned(),
            ref_varname: refnames[idx].to_owned(),
            multiplicity: multiplicity.clone()
        });
    }

    blocks
}


fn parse_enum(token_iter: &mut TokenStreamIter) -> Enum {
    let mut a2lenum = Enum {
        name: get_ident(token_iter),
        items: Vec::new()
    };

    let enum_tokens = get_group(token_iter, Delimiter::Brace);
    let mut enum_token_iter = enum_tokens.into_iter().peekable();

    loop {
        if enum_token_iter.peek().is_none() {
            break;
        }
        a2lenum.items.push(get_ident(&mut enum_token_iter));
        /* if thee are further items in the enum, there must be a ',' as a separator */
        if enum_token_iter.peek().is_some() {
            require_punct(&mut enum_token_iter, ',');
        }
    }

    a2lenum
}


fn typename_from_names(names: &Vec<String>) -> String {
    if names.len() == 1 {
        ucname_to_typename(&names[0])
    }
    else {
        let mut namechars: Vec<char> = names[0].chars().collect();
        // Replace the varying pat of the name with "DIM"
        namechars.pop();
        namechars.push('D');
        namechars.push('I');
        namechars.push('M');

        let prefixstr: String = namechars.iter().collect();
        ucname_to_typename(&prefixstr)
    }
}


fn consistency_check(spec: &Specification) {
    let mut set = HashSet::<String>::new();
    let mut typeset: HashSet<String>;

    // add the default types
    const BUILTIN_TYPES: [&str; 8] = ["ident", "string", "float", "int", "uint", "long", "ulong", "enum"];
    

    typeset = BUILTIN_TYPES.iter().map(|&x| x.to_string()).collect();
    for e in spec.enums.iter() {
        if typeset.insert(e.name.clone()) == false {
            panic!("duplicate enum name {} in a2l specification", e.name);
        }
    }

    for b in spec.blocks.iter() {
        // make sure all blocknames are unique
        if set.insert(b.name.clone()) == false {
            // block name aleady present -> duplicate
            panic!("duplicate block name {} in a2l specification", b.name);
        }

        if b.contains_ifdata && (b.items.len() > 0 || b.references.len() > 0) {
            panic!("an IF_DATA block may not contain any other items");
        }

        if b.contains_a2ml && (b.items.len() > 0 || b.references.len() > 0) {
            panic!("an A2ML block may not contain any other items")
        }

        if b.is_keyword && b.references.len() > 0 {
            panic!("optional references are not allowed in keyword {}", b.name);
        }

        let mut itemset = HashSet::<String>::new();
        for item in b.items.iter() {
            match &item.datatype {
                BlockItemData::Group(grp) => {
                    for datatype in grp {
                        if typeset.get(datatype).is_none() {
                            panic!("data item {} in block {} uses the unknown type {}", item.name, b.name, datatype);
                        }
                    }
                }
                BlockItemData::Single(datatype) => {
                    if typeset.get(datatype).is_none() {
                        panic!("data item {} in block {} uses the unknown type {}", item.name, b.name, datatype);
                    }
                }
            }
            if itemset.insert(item.name.clone()) == false {
                panic!("duplicate data item name {} in block {}", item.name, b.name);
            }
        }
    }
}

//----------------------------------------------------------------------------------------------------

fn generate_data_structures(spec: &Specification) -> proc_macro2::TokenStream {
    let mut result = quote!{};

    for e in &spec.enums {
        result.extend(generate_enum(e));
    }

    for blk in &spec.blocks {
        let name = format_ident!("{}", blk.name);
        if blk.contains_ifdata {
            result.extend(quote! {
                #[derive(Debug)]
                pub struct #name {
                    pub fileid: usize,
                    pub line: u32,
                    pub ifdata_text: String,
                    pub ifdata_filename: String
                }
            });
        } else if blk.contains_a2ml {
            result.extend(quote! {
                #[derive(Debug)]
                pub struct #name {
                    pub fileid: usize,
                    pub line: u32,
                    pub a2ml_text: String
                }
            });
        } else {
            let mut items = Vec::new();
            for item in &blk.items {
                items.push(generate_blkitem(item));
            }

            for blkref in &blk.references {
                items.push(generate_blkref(blkref));
            }

            result.extend(quote! {
                #[derive(Debug)]
                pub struct #name {
                    pub fileid: usize,
                    pub line: u32,

                    #(#items),*
                }
            });
        }
    }

    result.into()
}


fn generate_enum(e: &Enum) -> proc_macro2::TokenStream {
    let name = format_ident!("{}", &e.name);
    let items = e.items.iter().map(|it| format_ident!("{}", ucname_to_typename(it)));
    quote! {
        #[derive(Debug, PartialEq)]
        pub enum #name {
            #(#items),*
        }
    }
}


fn generate_blkitem(item: &BlockItem) -> proc_macro2::TokenStream {
    let itemname = format_ident!("{}", item.name);
    let itemtype;
    let blkitem;
    match &item.datatype {
        BlockItemData::Single(datatype) => {
            if item.arraydim == 1 {
                itemtype = format_ident!("{}", datatype_lookup(datatype));
                blkitem = quote!{ pub #itemname: #itemtype };
            } else {
                itemtype = format_ident!("{}", datatype_lookup(datatype));
                let arraydim: usize = item.arraydim as usize;
                blkitem = quote!{ pub #itemname: [#itemtype; #arraydim] };
            }
        }
        // a group in the specification is generated as a Vec of tuples
        // e.g. {ulong, string}* foo -> foo: Vec<(u32, String)>
        BlockItemData::Group(grpitems) => {
            let itemtypes = grpitems.iter().map(|i| format_ident!("{}", datatype_lookup(i)));
            blkitem = quote!{ pub #itemname: Vec<(#(#itemtypes),*)> };
        }
    }

    blkitem
}


fn generate_blkref(blkref: &BlockRef) -> proc_macro2::TokenStream {
    let varname = format_ident!("{}", blkref.ref_varname);
    let typename = format_ident!("{}", blkref.ref_typename);
    match blkref.multiplicity {
        Multiplicity::One |
        Multiplicity::ZeroOrOne => {
            quote!{pub #varname: Option<#typename>}
        }
        Multiplicity::Many |
        Multiplicity::Any => {
            quote!{pub #varname: Vec<#typename>}
        }
    }
}


// convert the predefined datatypes of the a2l specification to equivalent rust types
fn datatype_lookup(datatype_in: &str) -> &str {
    match datatype_in {
        "ident" => "String",
        "string" => "String",
        "float" => "f32",
        "int" => "i16",
        "uint" => "u16",
        "long" => "i32",
        "ulong" => "u32",
        "enum" => panic!("enums should be declard with the actual enum type name"),
        _ => datatype_in
    }
}


//----------------------------------------------------------------------------------------------------


fn generate_parser_functions(spec: &Specification) -> proc_macro2::TokenStream {
    let mut result = quote!{};

    for e in &spec.enums {
        result.extend(generate_enum_parser_function(e));
    }

    let blktypes: HashMap<String, bool> = spec.blocks.iter().map(|blk| (blk.name.clone(), blk.is_keyword)).collect();
    for blk in &spec.blocks {
        if blk.contains_ifdata {
            result.extend(generate_ifdata_parser_function(blk));
        } else if blk.contains_a2ml {
            result.extend(generate_a2ml_parser_function(blk));
        } else {
            result.extend(generate_block_parser_function(blk, &blktypes));
        } 
    }

    result
}


fn generate_enum_parser_function(e: &Enum) -> proc_macro2::TokenStream {
    let name = format_ident!("{}", &e.name);

    let mut match_branches = Vec::new();
    for enval in &e.items {
        let enident = format_ident!("{}", ucname_to_typename(enval));
        match_branches.push(quote!{#enval => Ok(#name::#enident),});
    }
    
    quote! {
        impl #name {
            fn parse(parser: &mut a2lparser::ParserState, context: &ParseContext) -> Result<#name, ParseError> {
                let enumname = parser.get_identifier(context)?;
                match &*enumname {
                    #(#match_branches)*
                    _ => Err(ParseError::InvalidEnumValue(context.copy(), enumname))
                }
            }
        }
    }
}


fn generate_block_parser_function(blk: &Block, blk_is_kw: &HashMap<String, bool>) -> proc_macro2::TokenStream {
    let name = format_ident!("{}", &blk.name);

    let mut item_parser_fragments = Vec::new();
    let mut blkitems = Vec::new();
    for item in &blk.items {
        // create the parser fragment to get the item data
        item_parser_fragments.push(generate_item_parser(item));

        // add all item names to this list, so that they can be used in the block constructor
        let blkitem = format_ident!("{}", item.name);
        blkitems.push(blkitem);
    }

    let mut refitem_init = Vec::new();
    let mut blkref_matches = Vec::new();
    let mut multiplicity_check = Vec::new();
    for blkref in &blk.references {
        let ident = format_ident!("{}", &blkref.ref_varname);
        let store_item;
        // generate code to store each parsed item
        match blkref.multiplicity {
            Multiplicity::One |
            Multiplicity::ZeroOrOne => {
                // generate the initializer for an item that uses an Option as the underlying storage
                refitem_init.push(quote!{
                    let mut #ident = None;
                });

                // for the match arm, geneate the code to store the newly parsed item
                store_item = quote!{
                    if #ident.is_none() {
                        #ident = Some(newitem);
                    }
                    else {
                        parser.error_or_log(ParseError::InvalidMultiplicityTooMany(context.clone(), tag.clone()))?;
                    }
                }
                
            }
            Multiplicity::Any |
            Multiplicity::Many => {
                // generate the initializer for an item that uses a Vec as the underlying storage
                refitem_init.push(quote!{
                    let mut #ident = Vec::new();
                });

                // for the match arm, geneate the code to store the newly parsed item
                store_item = quote!{
                    #ident.push(newitem);
                }
            }
        }

        // generate code to parse each optional item. The code to store the items was generated above and is inserted here.
        // These are the match arms inside the optional item parsing loop
        let keyword = &blkref.reference_name;
        let refitem_type = format_ident!("{}", &blkref.ref_typename);
        let is_blk_spec = !*blk_is_kw.get(&blkref.ref_typename).unwrap_or_else(|| &true);
        blkref_matches.push(
            quote!{
                #keyword => {
                    let newitem = #refitem_type::parse(parser, &newcontext)?;
                    #store_item
                    if (#is_blk_spec != is_block) {
                        parser.error_or_log(ParseError::IncorrectElemType(context.clone(), #keyword.to_string(), #is_blk_spec))?;
                    }
                }
            }
        );

        // generate code to check that mandatory items exist
        if blkref.multiplicity == Multiplicity::Many {
            multiplicity_check.push(quote!{
                if blkvar.#ident.len() == 0 {
                    parser.error_or_log(ParseError::InvalidMultiplicityNotPresent(context.clone(), #keyword.to_string()))?;
                }
            });
        } else if blkref.multiplicity == Multiplicity::One {
            multiplicity_check.push(quote!{
                if blkvar.#ident.is_none() {
                    parser.error_or_log(ParseError::InvalidMultiplicityNotPresent(context.clone(), #keyword.to_string()))?;
                }
            });
        }

        blkitems.push(ident);
    }


    // generate the loop that controls the parsing of optional items
    let mut refitem_parse = quote!{};
    if blk.references.len() > 0 {
        refitem_parse.extend(quote! {
            let mut tag_peek = parser.peek_next_tag(context)?;
            while tag_peek.is_some() {
                let (tag, is_block) = tag_peek.unwrap();
                let token = parser.get_token(context)?;
                let newcontext = a2lparser::ParseContext::from_token(token, is_block);

                match &*tag {
                    #(#blkref_matches)*
                    _ => {
                        parser.error_or_log(ParseError::UnknownSubBlock(context.clone(), tag.clone()))?;
                    }
                }
                tag_peek = parser.peek_next_tag(context)?;
            }
        });
    }

    // generate the parse() function for the current block, using all of the code fragments generated above
    quote! {
        impl #name {
            fn parse(parser: &mut a2lparser::ParserState, context: &ParseContext) -> Result<#name, ParseError> {
                #(#item_parser_fragments)*
                #(#refitem_init)*
                #refitem_parse

                let blkvar = #name {
                    fileid: context.fileid,
                    line: context.line,
                    #(#blkitems: #blkitems),*
                };

                #(#multiplicity_check)*

                if context.inside_block {
                    parser.expect_token(context, A2lTokenType::End)?;
                    let ident = parser.get_identifier(context)?;
                    if ident != context.element {
                        parser.error_or_log(ParseError::IncorrectEndTag(context.clone(), ident))?;
                    }
                }

                Ok(blkvar)
            }
        }
    }
}


fn generate_item_parser(item: &BlockItem) -> proc_macro2::TokenStream {
    match &item.datatype {
        BlockItemData::Single(single_data) => {
            if item.arraydim == 1 {
                let itemname = format_ident!("{}", item.name);
                let parserfunc = get_element_parser_function(single_data);
                quote!{let #itemname = #parserfunc?;}
            }
            else {
                let itemname = format_ident!("{}", item.name);
                let parserfunc = get_element_parser_function(single_data);
                let parsercalls = (0..item.arraydim).into_iter().map(|_| quote!{#parserfunc?});
                quote!{let #itemname = [ #(#parsercalls),* ];}
            }
        }
        BlockItemData::Group(group_data) => {
            let itemname = format_ident!("{}", item.name);
            let groupitems: Vec<proc_macro2::Ident> = (0..(group_data.len() )).into_iter().map(|idx| format_ident!("item_{}", idx)).collect();
            let parserfunc = group_data.iter().map(|data| get_element_parser_function(data));
            quote!{
                let mut #itemname = Vec::new();
                let mut done = false;
                while done == false {
                    let current_token = parser.get_tokenpos();
                    #(let #groupitems = #parserfunc;)*
                    if (#(#groupitems.is_err() )||*) {
                        parser.set_tokenpos(current_token);
                        done = true;
                    }
                    else {
                        let grp = ( #(#groupitems?),* );
                        #itemname.push(grp);
                    }
                }
            }
        }
    }
}


fn get_element_parser_function(datatype: &str) -> proc_macro2::TokenStream {
    match datatype {
        "ident" => {
            let funcname = format_ident!("get_identifier");
            quote!{parser.#funcname(context)}
        }
        "string" => {
            let funcname = format_ident!("get_string");
            quote!{parser.#funcname(context)}
        }
        "float" => {
            let funcname = format_ident!("get_float");
            quote!{parser.#funcname(context)}
        }
        "int" => {
            let funcname = format_ident!("get_integer_i16");
            quote!{parser.#funcname(context)}
        }
        "uint" => {
            let funcname = format_ident!("get_integer_u16");
            quote!{parser.#funcname(context)}
        }
        "long" => {
            let funcname = format_ident!("get_integer_i32");
            quote!{parser.#funcname(context)}
        }
        "ulong" => {
            let funcname = format_ident!("get_integer_u32");
            quote!{parser.#funcname(context)}
        }
        "enum" => panic!("enums should be declard with the actual enum type name"),
        _ => {
            let typename = format_ident!("{}", datatype);
            quote!{ #typename::parse(parser, context) }
        }
    }
}


fn generate_ifdata_parser_function(blk: &Block) -> proc_macro2::TokenStream {
    let name = format_ident!("{}", &blk.name);
    quote! {
        impl #name {
            fn parse(parser: &mut ParserState, context: &ParseContext) -> Result<#name, ParseError> {
                let token_peek = parser.peek_token();
                let ifdata_text;
                let ifdata_filename;
                if token_peek.is_none() || token_peek.unwrap().ttype != A2lTokenType::Text {
                    ifdata_text = String::from("");
                    ifdata_filename = String::from("");
                } else {
                    // there is an IF_DATA text token
                    let token = parser.get_token(context)?; // remove the IF_DATA token from the input
                    ifdata_text = token.text.clone();
                    ifdata_filename = parser.get_string(context)?;
                };

                let blkvar = #name {
                    fileid: context.fileid,
                    line: context.line,
                    ifdata_text,
                    ifdata_filename
                };

                parser.expect_token(context, A2lTokenType::End)?;
                let ident = parser.get_identifier(context)?;
                if ident != context.element {
                    parser.error_or_log(ParseError::IncorrectEndTag(context.clone(), ident))?;
                }            

                Ok(blkvar)
            }
        }
    }
}


fn generate_a2ml_parser_function(blk: &Block) -> proc_macro2::TokenStream {
    let name = format_ident!("{}", &blk.name);
    quote! {
        impl #name {
            fn parse(parser: &mut ParserState, context: &ParseContext) -> Result<#name, ParseError> {
                let token_peek = parser.peek_token();
                let a2ml_text = if token_peek.is_none() || token_peek.unwrap().ttype != A2lTokenType::Text {
                    String::from("")
                } else {
                    // there is an a2ml text token
                    let token = parser.get_token(context)?; // remove the A2ml token from the input
                    parser.get_string(context)?;
                    token.text.clone()
                };
                let blkvar = #name {
                    fileid: context.fileid,
                    line: context.line,
                    a2ml_text
                };

                parser.expect_token(context, A2lTokenType::End)?;
                let ident = parser.get_identifier(context)?;
                if ident != context.element {
                    parser.error_or_log(ParseError::IncorrectEndTag(context.clone(), ident))?;
                }            

                Ok(blkvar)
            }
        }
    }
}