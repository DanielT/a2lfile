use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use proc_macro2::Delimiter;
use std::collections::HashMap;
use crate::util::*;
use crate::codegenerator;
use crate::codegenerator::*;



struct StructInfo {
    taglist: Vec<String>,
    dataitem: DataItem,
    is_block: bool
}


//----------------------------------------------------------------------------------------------------


pub(crate) fn a2l_specification(tokens: TokenStream) -> TokenStream {
    let mut iter: TokenStreamIter = tokens.into_iter().peekable();
    let (structs, enums) = parse_input(&mut iter);

    let types = build_typelist(structs, enums);

    let mut typesvec: Vec<(&String, &DataItem)> = types.iter().map(|(key, val)| (key, val)).collect();
    typesvec.sort_by(|a, b| a.0.cmp(b.0));

    let mut result = TokenStream::new();
    for (typename, a2ltype) in typesvec {
        result.extend(codegenerator::data_structure::generate(typename, a2ltype));
        result.extend(codegenerator::parser::generate(typename, a2ltype));
        result.extend(codegenerator::writer::generate(typename, a2ltype));
    }

    result
}

//----------------------------------------------------------------------------------------------------


fn parse_input(token_iter: &mut TokenStreamIter) -> (Vec<StructInfo>, Vec<DataItem>) {
    let mut structs = Vec::new();
    let mut enums = Vec::new();

    while token_iter.peek().is_some() {

        let comment = parse_doc_comments(token_iter);

        let ident = get_ident(token_iter);
        match &*ident {
            "block" => {
                structs.push(parse_struct(token_iter, comment, false));
            },
            "keyword" => {
                structs.push(parse_struct(token_iter, comment, true));
            },
            "enum" => {
                enums.push(parse_enum(token_iter, comment))
            },
            _ => { panic!("expected a keyword of the a2l specification in this position, got {}", ident); }
        }
    }

    (structs, enums)
}

fn parse_doc_comments(token_iter: &mut TokenStreamIter) -> Option<String> {
    let mut comment = "".to_string();
    while let Some(comment_line) = parse_optional_comment(token_iter) {
        if !comment.is_empty() {
            comment = format!("{}\n{}", comment, comment_line);
        } else {
            comment = comment_line;
        }
    }

    if !comment.is_empty() {
        Some(comment)
    } else {
        None
    }
}


fn parse_struct(token_iter: &mut TokenStreamIter, comment: Option<String>, is_keyword: bool) -> StructInfo {
    let mut items = Vec::new();
    let mut references = Vec::new();

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
                        // a group of repeated items ({string x, uint y}* foo)
                        items.push(parse_blockitem_group(&mut block_token_iter));
                    }
                    Delimiter::Bracket => {
                        // a reference to another block ([-> BLOCK_NAME])
                        references.append(&mut parse_optitem(&mut block_token_iter));
                    }
                    other => {
                        panic!("unknown token group inside delimiter '{:?}' in A2l block {}", other, namelist[0]);
                    }
                }
            }
            _ => panic!("{:#?} is not allowed in this position while parsing the definition of {}", nexttok.to_string(), namelist[0])
        }
    }

    if !references.is_empty() {
        // the references to other blocks are represented as a taggedstruct.
        // This is always the last element in the struct, as a2l does not have required items after optional items
        items.push(
            DataItem {
                typename: None,
                basetype: BaseType::TaggedStruct(references),
                varname: None,
                comment: None
            }
        );
    }

    let dataitem = DataItem {
        typename: Some(name),
        basetype: BaseType::Struct(items),
        varname: None,
        comment
    };
    StructInfo {
        taglist: namelist,
        dataitem,
        is_block: !is_keyword
    }
}


// it is possible to specify that one block is referenced under multiple names, e.g.  AXIS_PTS_X / _Y / _Z / _4 / _5
// this function constructs all the full names from the pattern and returns them in a Vec
fn parse_blockname(token_iter: &mut TokenStreamIter) -> Vec<String> {
    let mut names = vec![ get_ident(token_iter) ];
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


fn parse_blockitem_single(block_token_iter: &mut TokenStreamIter) -> DataItem {
    let typename = get_ident(block_token_iter);
    let basetype = get_basetype(&typename);

    let mut dataitem = DataItem {
        typename: Some(typename),
        basetype,
        varname: None,
        comment: None
    };

    if let Some(TokenTree::Group(g)) = block_token_iter.peek() {
        if g.delimiter() == Delimiter::Bracket {
            let arrspec_tokens = get_group(block_token_iter, Delimiter::Bracket);
            if let Some(TokenTree::Literal(lit)) = arrspec_tokens.into_iter().next() {
                let arraydim = lit.to_string().parse().unwrap();

                dataitem = DataItem{
                    typename: None,
                    basetype: BaseType::Array(Box::new(dataitem), arraydim),
                    varname: None,
                    comment: None
                };
            }
        }
    }

    dataitem.varname = Some(get_ident(block_token_iter));

    dataitem
}


fn parse_blockitem_group(block_token_iter: &mut TokenStreamIter) -> DataItem {
    let grp_tokens = get_group(block_token_iter, Delimiter::Brace);
    let mut grp_token_iter = grp_tokens.into_iter().peekable();
    require_punct(block_token_iter, '*');
    let varname = get_ident(block_token_iter);
    let typename = format!("{}Struct", ucname_to_typename(&varname.to_ascii_uppercase()));

    let mut structitems = Vec::new();
    while grp_token_iter.peek().is_some() {
        structitems.push(parse_blockitem_single(&mut grp_token_iter));
    }

    if structitems.len() == 1 {
        // there is actually only one item in the sequence; no need to create a struct for it
        let item = structitems.pop().unwrap();
        DataItem {
            typename: Some(typename),
            basetype: BaseType::Sequence(Box::new(item.basetype)),
            varname: Some(varname),
            comment: None
        }
    } else {
        // since there are multiple items in the sequence, they are wrapped in a struct
        DataItem {
            typename: Some(typename),
            basetype: BaseType::Sequence(Box::new(BaseType::Struct(structitems))),
            varname: Some(varname),
            comment: None
        }
    }
}


fn parse_optitem(block_token_iter: &mut TokenStreamIter) -> Vec<TaggedItem> {
    let mut blocks: Vec<TaggedItem> = Vec::new();

    let ref_tokens = get_group(block_token_iter, Delimiter::Bracket);
    let mut ref_token_iter = ref_tokens.into_iter().peekable();
    require_punct(&mut ref_token_iter, '-');
    require_punct(&mut ref_token_iter, '>');
    let blocknames = parse_blockname(&mut ref_token_iter);
    let varnames: Vec<String> = blocknames.iter().map(
        |bn| make_varname(&bn.to_ascii_lowercase())
    ).collect();

    // check if there is an additional *, +, or ! to mark this reference as repeating or required
    let mut required = false;
    let mut repeat = false;
    if let Some(TokenTree::Punct(_)) = block_token_iter.peek() {
        match get_punct(block_token_iter) {
            '!' => { required = true; }
            '+' => { required = true; repeat = true; }
            '*' => { repeat = true; },
            c => panic!("multiplicity constraints must be one of !+*, got '{}'", c)
        }
    }

    // finally, there can also be a version range, e.g (1.0 .. 1.61), or (.. 1.50), or (1.70 ..)
    let version_range = get_optional_version_range(block_token_iter);
    
    let ref_typename = typename_from_names(&blocknames);
    for (idx, blockname) in blocknames.iter().enumerate() {
        blocks.push(TaggedItem {
            tag: blockname.to_owned(),
            item: DataItem {
                typename: Some(ref_typename.to_owned()),
                basetype: BaseType::StructRef,
                varname: Some(varnames[idx].to_owned()),
                comment: None
            },
            is_block: false, // don't know that yet, it will be fixed later
            repeat,
            required,
            version_range
        });
    }

    blocks
}


fn get_optional_version_range(token_iter: &mut TokenStreamIter) -> Option<(f32, f32)> {
    let mut version_range = None;
    if let Some(TokenTree::Group(g)) = token_iter.peek() {
        if g.delimiter() == Delimiter::Parenthesis {
            let range_tokens = get_group(token_iter, Delimiter::Parenthesis);
            let mut range_token_iter = range_tokens.into_iter().peekable();

            // get the minimum version
            let mut min_ver = 0.0;
            if let Some(TokenTree::Literal(_)) = range_token_iter.peek() {
                min_ver = get_float(&mut range_token_iter);
            }

            // min and max versions are separated by ".."
            require_punct(&mut range_token_iter, '.');
            require_punct(&mut range_token_iter, '.');

            // get the maximum version
            let mut max_ver = std::f32::MAX;
            if let Some(TokenTree::Literal(_)) = range_token_iter.peek() {
                max_ver = get_float(&mut range_token_iter);
            }
            version_range = Some((min_ver, max_ver));
        }
    }

    version_range
}


fn parse_enum(token_iter: &mut TokenStreamIter, comment: Option<String>) -> DataItem {
    let name = get_ident(token_iter);
    let mut items = Vec::new();

    let enum_tokens = get_group(token_iter, Delimiter::Brace);
    let mut enum_token_iter = enum_tokens.into_iter().peekable();

    while enum_token_iter.peek().is_some() {
        let name = get_ident(&mut enum_token_iter);

        // there can also be a version range, e.g (1.0 .. 1.61), or (.. 1.50), or (1.70 ..)
        let version_range = get_optional_version_range(&mut enum_token_iter);

        items.push(EnumItem {
            name,
            value: None,
            comment: None,
            version_range
        });

        /* if there are further items in the enum, there must be a ',' as a separator */
        if enum_token_iter.peek().is_some() {
            require_punct(&mut enum_token_iter, ',');
        }
    }

    DataItem {
        typename: Some(name),
        basetype: BaseType::Enum(items),
        varname: None,
        comment
    }
}


fn typename_from_names(names: &[String]) -> String {
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


fn get_basetype(typename: &str) -> BaseType {
    match typename {
        "char" => BaseType::Char,
        "int" => BaseType::Int,
        "long" => BaseType::Long,
        "uchar" => BaseType::Uchar,
        "uint" => BaseType::Uint,
        "ulong" => BaseType::Ulong,
        "double" => BaseType::Double,
        "float" => BaseType::Double, /* use double for greater precision even when float is requested by the spec. Other programs also do this */
        "ident" => BaseType::Ident,
        "string" => BaseType::String,
        _ => BaseType::EnumRef,
    }
}


//-------------------------------------------------------------------------------------------------


fn build_typelist(structs: Vec<StructInfo>, enums: Vec<DataItem>) -> HashMap<String, DataItem> {
    let mut typelist: HashMap<String, DataItem> = HashMap::new();
    let mut tagmap: HashMap<String, bool> = HashMap::new();

    // enums can be copied directly to the output type list
    for e in enums {
        let typename = e.typename.as_ref().unwrap().to_owned();
        let oldval = typelist.insert(typename.clone(), e);
        assert!(oldval.is_none(), "duplicate enum name {} in a2l specification", typename);
    }

    // for all tags of all structs: store if this tag refers to a block or not
    for StructInfo{taglist, is_block, ..} in &structs {
        for tag in taglist {
            tagmap.insert(tag.clone(), *is_block);
        }
    }

    for StructInfo{taglist, dataitem, is_block} in structs {
        // the structs on the list can't be passed directly to the code generator:
        // where there are repeated sequences of the form
        // {
        //     ...
        // }* sequence_name
        // in the input a nested struct to represent the items is generated. These nested structs
        // need to be moved to the typelist, and are replaced by StructRefs at their original locations

        let mut output_structitems = Vec::new();
        // everything in the structs array is a Struct, so the condition is always true. It's just a convenient way to unwrap the structitems
        if let BaseType::Struct(blockitems) = dataitem.basetype {
            for si in blockitems {
                output_structitems.push(unwrap_nested_structs(&taglist[0], si, &mut typelist));
            }

            // If the struct contains a TaggedStruct with optional elements, these need to updatd with the info if the referent is a block or not
            // Check if there are any elements in the struct at all. Some structs are empty.
            let si_count = output_structitems.len();
            if si_count > 0 {
                // the struct is not empty, so it is safe to get the last item (the TaggedStruct is always last if it exists)
                let lastitem = &mut output_structitems[si_count-1];
                if let BaseType::TaggedStruct(taggeditems) = &mut lastitem.basetype {
                    // The taggedstruct exists, loop over the taggeditems and update them
                    for tgitem in taggeditems {
                        if let Some(is_block) = tagmap.get(&tgitem.tag) {
                            tgitem.is_block = *is_block;
                        } else {
                            panic!("referenced block {} is missing", tgitem.tag);
                        }
                    }
                }
            }
    
            let typename = dataitem.typename.unwrap();
            let oldval = typelist.insert(typename.clone(), DataItem {typename: Some(typename.clone()), basetype: BaseType::Block(output_structitems, is_block), varname: None, comment: dataitem.comment });
            assert!(oldval.is_none(), "type {} for block {} altready exists", typename, taglist[0]);
        }
    }

    typelist
}


// unwrap_nested_structs
// if the current item is a Sequence and contains a struct rather than a single item, then the struct needs to be moved out of the sequence
fn unwrap_nested_structs(tag: &str, si: DataItem, typelist: &mut HashMap<String, DataItem>) -> DataItem {
    if let BaseType::Sequence(seqitem) = si.basetype {
        if let BaseType::Struct(_) = *seqitem {
            // the sequence contains a struct, which needs to be moved out of the sequence
            let typename = si.typename.unwrap();
            if let Some(existing_type) = typelist.get(&typename) {
                // the struct type already exists in the output types. Some sequences (identifier_list) occur frequently
                assert!((existing_type.basetype == *seqitem), "type {} has multiple incompatible definitions", typename);
                // no need to insert the type, because it exists and is compatible
            } else {
                let comment = if let Some(comment) = si.comment {
                    Some(comment)
                } else {
                    Some(format!("Auto generated for repeating sequence {} in block {}", si.varname.as_ref().unwrap().to_owned(), tag))
                };
                typelist.insert(typename.clone(), DataItem {typename: Some(typename.clone()), basetype: *seqitem, varname: None, comment});
            }

            DataItem {
                typename: Some(typename),
                basetype: BaseType::Sequence(Box::new(BaseType::StructRef)),
                varname: si.varname,
                comment: None
            }
        } else {
            // the sequence does not contain a struct, so the type can be added unchanged to the output struct
            DataItem {
                typename: si.typename,
                basetype: BaseType::Sequence(seqitem),
                varname: si.varname,
                comment: si.comment
            }
        }
    } else {
        // anything that is not a sequence remains unchanged and is directly added to the output struct
        si
    }
}




#[cfg(test)]
mod test {
    use proc_macro2::TokenStream;
    use crate::a2lspec::*;

    fn get_test_spec_tokens() -> TokenStream {
        quote::quote!{
            // Specification: predefined data types
            enum enumTest {
                VARIANT1,
                VARIANT2
            }

            // Specification: 3.5.2
            block BLK1 {
                enumTest bla
                ident filename
                {
                    long a
                    int b
                }* sequence
                [-> BLK2]
            }

            block BLK2 {
                long[33] it
            }
        }
    }

    #[test]
    fn test_parse_specification() {
        let tokens = get_test_spec_tokens();

        a2l_specification(tokens);
    }

}