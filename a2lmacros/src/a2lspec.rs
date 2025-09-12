use crate::codegenerator;
use crate::codegenerator::{A2lVersion, BaseType, DataItem, EnumItem, TaggedItem};
use crate::util::*;
use proc_macro2::Delimiter;
use proc_macro2::TokenStream;
use proc_macro2::TokenTree;
use std::collections::HashMap;
use std::fmt::Write;

struct StructInfo {
    taglist: Vec<String>,
    dataitem: DataItem,
    is_block: bool,
}

//----------------------------------------------------------------------------------------------------

pub(crate) fn a2l_specification(tokens: TokenStream) -> TokenStream {
    let mut iter: TokenStreamIter = tokens.into_iter().peekable();
    let (structs, enums) = parse_input(&mut iter);

    let types = build_typelist(structs, enums);

    let mut typesvec: Vec<(&String, &DataItem)> = types.iter().collect();
    typesvec.sort_by(|a, b| a.0.cmp(b.0));

    let mut result = TokenStream::new();
    for (typename, a2ltype) in typesvec {
        result.extend(codegenerator::data_structure::generate(
            typename, a2ltype, true,
        ));
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
            }
            "keyword" => {
                structs.push(parse_struct(token_iter, comment, true));
            }
            "enum" => enums.push(parse_enum(token_iter, comment)),
            _ => {
                panic!("expected a keyword of the a2l specification in this position, got {ident}");
            }
        }
    }

    (structs, enums)
}

fn parse_doc_comments(token_iter: &mut TokenStreamIter) -> Option<String> {
    let mut comment = String::new();
    while let Some(comment_line) = parse_optional_comment(token_iter) {
        if comment.is_empty() {
            comment = comment_line;
        } else {
            comment = format!("{comment}\n{comment_line}");
        }
    }

    if comment.is_empty() {
        None
    } else {
        Some(comment)
    }
}

fn parse_struct(
    token_iter: &mut TokenStreamIter,
    comment: Option<String>,
    is_keyword: bool,
) -> StructInfo {
    let mut items = Vec::new();
    let mut references = Vec::new();

    let namelist = parse_blockname(token_iter);
    let name = typename_from_names(&namelist);

    let block_tokens = get_group(token_iter, Delimiter::Brace);
    let mut block_token_iter = block_tokens.into_iter().peekable();

    while let Some(nexttok) = block_token_iter.peek() {
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
                        panic!(
                            "unknown token group inside delimiter '{:?}' in A2l block {}",
                            other, namelist[0]
                        );
                    }
                }
            }
            _ => panic!(
                "{:#?} is not allowed in this position while parsing the definition of {}",
                nexttok.to_string(),
                namelist[0]
            ),
        }
    }

    if !references.is_empty() {
        // the references to other blocks are represented as a taggedstruct.
        // This is always the last element in the struct, as a2l does not have required items after optional items
        items.push(DataItem {
            typename: None,
            basetype: BaseType::TaggedStruct {
                tsitems: references,
            },
            varname: None,
            comment: None,
        });
    }

    let dataitem = DataItem {
        typename: Some(name),
        basetype: BaseType::Struct { structitems: items },
        varname: None,
        comment,
    };
    StructInfo {
        taglist: namelist,
        dataitem,
        is_block: !is_keyword,
    }
}

// it is possible to specify that one block is referenced under multiple names, e.g.  AXIS_PTS_X / _Y / _Z / _4 / _5
// this function constructs all the full names from the pattern and returns them in a Vec
fn parse_blockname(token_iter: &mut TokenStreamIter) -> Vec<String> {
    let mut names = vec![get_ident(token_iter)];
    let mut suffixlist = Vec::<String>::new();

    // collect the suffixes (if any)
    while let Some(TokenTree::Punct(_)) = token_iter.peek() {
        require_punct(token_iter, '/');
        let suffix = get_ident(token_iter);
        if !suffix.is_empty() {
            suffixlist.push(suffix);
        }
    }

    if !suffixlist.is_empty() {
        // strip the suffix from the first name, and append all the other suffixes, e.g. AXIS_PTS_X -> AXIS_PTS_Y
        let suffixlen = suffixlist[0].len();
        assert!(suffixlist.iter().all(|suffix| suffix.len() == suffixlen));
        let endidx = names[0].len() - suffixlen;
        let basename = String::from(&names[0][0..endidx]);

        for suffix in &suffixlist {
            let mut newname = basename.clone();
            newname.write_str(suffix).unwrap();
            names.push(newname);
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
        comment: None,
    };

    if let Some(TokenTree::Group(g)) = block_token_iter.peek()
        && g.delimiter() == Delimiter::Bracket
    {
        let arrspec_tokens = get_group(block_token_iter, Delimiter::Bracket);
        if let Some(TokenTree::Literal(lit)) = arrspec_tokens.into_iter().next() {
            let arraydim = match lit.to_string().parse() {
                Ok(val) => val,
                Err(error) => panic!("{lit} is not a valid array index: {error}"),
            };

            dataitem = DataItem {
                typename: None,
                basetype: BaseType::Array {
                    arraytype: Box::new(dataitem),
                    dim: arraydim,
                },
                varname: None,
                comment: None,
            };
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
    let typename = format!(
        "{}Struct",
        ucname_to_typename(&varname.to_ascii_uppercase())
    );

    let mut structitems = Vec::new();
    while grp_token_iter.peek().is_some() {
        structitems.push(parse_blockitem_single(&mut grp_token_iter));
    }

    if structitems.len() == 1 {
        // there is actually only one item in the sequence; no need to create a struct for it
        let item = structitems.pop().unwrap();
        DataItem {
            typename: Some(typename),
            basetype: BaseType::Sequence {
                seqtype: Box::new(item.basetype),
            },
            varname: Some(varname),
            comment: None,
        }
    } else {
        // since there are multiple items in the sequence, they are wrapped in a struct
        DataItem {
            typename: Some(typename),
            basetype: BaseType::Sequence {
                seqtype: Box::new(BaseType::Struct { structitems }),
            },
            varname: Some(varname),
            comment: None,
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
    let varnames: Vec<String> = blocknames
        .iter()
        .map(|bn| make_varname(&bn.to_ascii_lowercase()))
        .collect();

    // check if there is an additional *, +, or ! to mark this reference as repeating or required
    let mut required = false;
    let mut repeat = false;
    if let Some(TokenTree::Punct(_)) = block_token_iter.peek() {
        match get_punct(block_token_iter) {
            '!' => {
                required = true;
            }
            '+' => {
                required = true;
                repeat = true;
            }
            '*' => {
                repeat = true;
            }
            c => panic!("multiplicity constraints must be one of !+*, got '{c}'"),
        }
    }

    // finally, there can also be a version range, e.g (1.0 .. 1.61), or (.. 1.50), or (1.70 ..)
    let (version_lower, version_upper) = get_optional_version_range(block_token_iter);

    let ref_typename = typename_from_names(&blocknames);
    for (idx, blockname) in blocknames.iter().enumerate() {
        blocks.push(TaggedItem {
            tag: blockname.to_owned(),
            item: DataItem {
                typename: Some(ref_typename.clone()),
                basetype: BaseType::StructRef,
                varname: Some(varnames[idx].clone()),
                comment: None,
            },
            is_block: false, // don't know that yet, it will be fixed later
            is_named: false, // likewise, this will be fixed later
            repeat,
            required,
            version_lower,
            version_upper,
        });
    }

    blocks
}

fn get_optional_version_range(
    token_iter: &mut TokenStreamIter,
) -> (Option<A2lVersion>, Option<A2lVersion>) {
    if let Some(TokenTree::Group(g)) = token_iter.peek()
        && g.delimiter() == Delimiter::Parenthesis
    {
        let range_tokens = get_group(token_iter, Delimiter::Parenthesis);
        let mut range_token_iter = range_tokens.into_iter().peekable();

        // get the minimum version
        let mut min_ver = None;
        if let Some(TokenTree::Literal(_)) = range_token_iter.peek() {
            min_ver = Some(get_version(&mut range_token_iter));
        }

        // min and max versions are separated by ".."
        require_punct(&mut range_token_iter, '.');
        require_punct(&mut range_token_iter, '.');

        // get the maximum version
        let mut max_ver = None;
        if let Some(TokenTree::Literal(_)) = range_token_iter.peek() {
            max_ver = Some(get_version(&mut range_token_iter));
        }

        return (min_ver, max_ver);
    }

    (None, None)
}

fn parse_enum(token_iter: &mut TokenStreamIter, comment: Option<String>) -> DataItem {
    let name = get_ident(token_iter);
    let mut items = Vec::new();

    let enum_tokens = get_group(token_iter, Delimiter::Brace);
    let mut enum_token_iter = enum_tokens.into_iter().peekable();

    while enum_token_iter.peek().is_some() {
        let name = get_ident(&mut enum_token_iter);

        // there can also be a version range, e.g (1.0 .. 1.61), or (.. 1.50), or (1.70 ..)
        let (version_lower, version_upper) = get_optional_version_range(&mut enum_token_iter);

        items.push(EnumItem {
            name,
            value: None,
            comment: None,
            version_lower,
            version_upper,
        });

        /* if there are further items in the enum, there must be a ',' as a separator */
        if enum_token_iter.peek().is_some() {
            require_punct(&mut enum_token_iter, ',');
        }
    }

    DataItem {
        typename: Some(name),
        basetype: BaseType::Enum { enumitems: items },
        varname: None,
        comment,
    }
}

fn typename_from_names(names: &[String]) -> String {
    if names.len() == 1 {
        ucname_to_typename(&names[0])
    } else {
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
        "int64" => BaseType::Int64,
        "uchar" => BaseType::Uchar,
        "uint" => BaseType::Uint,
        "ulong" => BaseType::Ulong,
        "uint64" => BaseType::Uint64,
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
    let mut tagmap: HashMap<String, (bool, bool)> = HashMap::new();
    let mut used_in_list: HashMap<String, bool> = HashMap::new();

    // enums can be copied directly to the output type list
    for e in enums {
        // an enum in the a2lspec always has a type name
        let typename = e.typename.as_ref().unwrap().to_owned();
        let oldval = typelist.insert(typename.clone(), e);
        assert!(
            oldval.is_none(),
            "duplicate enum name {typename} in a2l specification"
        );
    }

    // for all tags of all structs: store if this tag refers to a block or not
    for StructInfo {
        taglist,
        is_block,
        dataitem,
    } in &structs
    {
        let is_named = if let BaseType::Struct { structitems } = &dataitem.basetype {
            structitems.len() > 1
                && structitems[0].basetype == BaseType::Ident
                && structitems[0]
                    .varname
                    .as_ref()
                    .is_some_and(|name| name == "name")
        } else {
            false
        };

        for tag in taglist {
            tagmap.insert(tag.clone(), (*is_block, is_named));
        }
    }

    // find out which blocks are used in lists before processing structs into the typelist
    // this makes it possible to set the used_in_list flag for all blocks
    for StructInfo { dataitem, .. } in &structs {
        let BaseType::Struct { structitems } = &dataitem.basetype else {
            unreachable!();
        };

        if let Some(lastitem) = structitems.last()
            && let BaseType::TaggedStruct { tsitems } = &lastitem.basetype
        {
            for tgitem in tsitems {
                if tgitem.repeat {
                    let name = ucname_to_typename(&tgitem.tag);
                    used_in_list.insert(name, true);
                }
            }
        }
    }

    for StructInfo {
        taglist,
        dataitem,
        is_block,
    } in structs
    {
        // the structs on the list can't be passed directly to the code generator:
        // where there are repeated sequences of the form
        // {
        //     ...
        // }* sequence_name
        // in the input a nested struct to represent the items is generated. These nested structs
        // need to be moved to the typelist, and are replaced by StructRefs at their original locations

        let mut output_structitems = Vec::new();
        let BaseType::Struct { structitems } = dataitem.basetype else {
            unreachable!();
        };

        for si in structitems {
            output_structitems.push(unwrap_nested_structs(&taglist[0], si, &mut typelist));
        }

        // If the struct contains a TaggedStruct with optional elements, these need to updatd with the info if the referent is a block or not
        // Check if there are any elements in the struct at all. Some structs are empty.
        let si_count = output_structitems.len();
        if si_count > 0 {
            // the struct is not empty, so it is safe to get the last item (the TaggedStruct is always last if it exists)
            let lastitem = &mut output_structitems[si_count - 1];
            if let BaseType::TaggedStruct {
                tsitems: taggeditems,
            } = &mut lastitem.basetype
            {
                // The taggedstruct exists, loop over the taggeditems and update them
                for tgitem in taggeditems {
                    if let Some((is_block, is_named)) = tagmap.get(&tgitem.tag) {
                        tgitem.is_block = *is_block;
                        tgitem.is_named = *is_named;
                    } else {
                        panic!("referenced block {} is missing", tgitem.tag);
                    }
                }
            }
        }

        // a struct always has a type name in the a2l spec, so unwrap() is safe here
        let typename: String = dataitem.typename.unwrap();
        let used_in_list = used_in_list.remove(&typename).unwrap_or(false); // if the typename is not in the list, it is not used in a list
        let oldval = typelist.insert(
            typename.clone(),
            DataItem {
                typename: Some(typename.clone()),
                basetype: BaseType::Block {
                    blockitems: output_structitems,
                    is_block,
                    used_in_list,
                },
                varname: None,
                comment: dataitem.comment,
            },
        );
        assert!(
            oldval.is_none(),
            "type {typename} for block {} altready exists",
            taglist[0]
        );
    }

    typelist
}

// unwrap_nested_structs
// if the current item is a Sequence and contains a struct rather than a single item, then the struct needs to be moved out of the sequence
fn unwrap_nested_structs(
    tag: &str,
    si: DataItem,
    typelist: &mut HashMap<String, DataItem>,
) -> DataItem {
    if let BaseType::Sequence { seqtype } = si.basetype {
        if let BaseType::Struct { .. } = *seqtype {
            // the sequence contains a struct, which needs to be moved out of the sequence
            let typename = si.typename.unwrap();
            if let Some(existing_type) = typelist.get(&typename) {
                // the struct type already exists in the output types. Some sequences (identifier_list) occur frequently
                assert_eq!(
                    existing_type.basetype, *seqtype,
                    "type {typename} has multiple incompatible definitions"
                );
                // no need to insert the type, because it exists and is compatible
            } else {
                let comment = if let Some(comment) = si.comment {
                    Some(comment)
                } else {
                    Some(format!(
                        "Auto generated for repeating sequence {} in block {}",
                        si.varname.as_ref().unwrap(),
                        tag
                    ))
                };
                typelist.insert(
                    typename.clone(),
                    DataItem {
                        typename: Some(typename.clone()),
                        basetype: *seqtype,
                        varname: None,
                        comment,
                    },
                );
            }

            DataItem {
                typename: Some(typename),
                basetype: BaseType::Sequence {
                    seqtype: Box::new(BaseType::StructRef),
                },
                varname: si.varname,
                comment: None,
            }
        } else {
            // the sequence does not contain a struct, so the type can be added unchanged to the output struct
            DataItem {
                typename: si.typename,
                basetype: BaseType::Sequence { seqtype },
                varname: si.varname,
                comment: si.comment,
            }
        }
    } else {
        // anything that is not a sequence remains unchanged and is directly added to the output struct
        si
    }
}

#[cfg(test)]
mod test {
    use crate::a2lspec::*;
    use proc_macro2::TokenStream;

    fn get_test_spec_tokens() -> TokenStream {
        quote::quote! {
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
