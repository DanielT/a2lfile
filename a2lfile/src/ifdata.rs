use crate::a2ml::{A2mlTaggedTypeSpec, A2mlTypeSpec, GenericIfData, GenericIfDataTaggedItem};
use crate::parser::{ParseContext, ParserError, ParserState};
use crate::specification::{A2lFile, IfData};
use crate::tokenizer::{A2lToken, A2lTokenType};
use std::collections::HashMap;

// parse_ifdata()
// entry point for ifdata parsing.
// Three attmpts to parse the data will be made:
// 1) parse according to the built-in spec provided using the a2ml_specification! macro
// 2) parse according to the spec in the A2ML block
// 3) fallback parsing using parse_unknown_ifdata()
pub(crate) fn parse_ifdata(
    parser: &mut ParserState,
    context: &ParseContext,
) -> Result<(Option<GenericIfData>, bool), ParserError> {
    let mut result = None;
    let mut valid = false;
    // is there any content in the IF_DATA?
    if let Some(token) = parser.peek_token() {
        if token.ttype != A2lTokenType::End {
            // try parsing according to the spec provided by the user of the crate in the a2ml_specification! macro
            let spec = std::mem::take(&mut parser.builtin_a2mlspec);
            if let Some(a2mlspec) = &spec {
                if let Some(ifdata_items) = parse_ifdata_from_spec(parser, context, a2mlspec) {
                    result = Some(ifdata_items);
                    valid = true;
                }
            }
            parser.builtin_a2mlspec = spec;

            // no built in spec, or parsing using that spec failed
            if result.is_none() {
                // try parsing according to the spec inside the file
                let spec = std::mem::take(&mut parser.file_a2mlspec);
                if let Some(a2mlspec) = &spec {
                    if let Some(ifdata_items) = parse_ifdata_from_spec(parser, context, a2mlspec) {
                        result = Some(ifdata_items);
                        valid = true;
                    }
                }
                parser.file_a2mlspec = spec;
            }

            if result.is_none() {
                // this will succeed if the data format follows the basic a2l rules (e.g. matching /begin and /end)
                // if it does not, a ParseErrror is generated
                result = Some(parse_unknown_ifdata_start(parser, context)?);
                valid = false;
            }
        }
    }

    Ok((result, valid))
}

// parse_ifdata_from_spec()
// parse the items of an IF_DATA block according to a spec.
// If parsing fails, the token cursor is set back to the beginning of the input so that parsing can be retried
fn parse_ifdata_from_spec(
    parser: &mut ParserState,
    context: &ParseContext,
    spec: &A2mlTypeSpec,
) -> Option<GenericIfData> {
    let pos = parser.get_tokenpos();
    if let Ok(ifdata) = parse_ifdata_item(parser, context, spec) {
        if let Some(A2lToken {
            ttype: A2lTokenType::End,
            ..
        }) = parser.peek_token()
        {
            Some(parse_ifdata_make_block(
                ifdata,
                parser.get_incfilename(context.fileid),
                context.line,
            ))
        } else {
            // parsed some (or maybe none if the spec allows this!), but not all elements of the IF_DATA.
            // put the token_cursor back at the beginning of the IF_DATA input
            parser.set_tokenpos(pos);
            None
        }
    } else {
        // put the token_cursor back at the beginning of the IF_DATA input
        parser.set_tokenpos(pos);
        None
    }
}

// parse_ifdata_item()
// parse one item together with all of its dependent elements according to an A2mlTypeSpec
fn parse_ifdata_item(
    parser: &mut ParserState,
    context: &ParseContext,
    spec: &A2mlTypeSpec,
) -> Result<GenericIfData, ParserError> {
    Ok(match spec {
        A2mlTypeSpec::None => GenericIfData::None,
        A2mlTypeSpec::Char => GenericIfData::Char(
            parser.get_current_line_offset(),
            parser.get_integer_i8(context)?,
        ),
        A2mlTypeSpec::Int => GenericIfData::Int(
            parser.get_current_line_offset(),
            parser.get_integer_i16(context)?,
        ),
        A2mlTypeSpec::Long => GenericIfData::Long(
            parser.get_current_line_offset(),
            parser.get_integer_i32(context)?,
        ),
        A2mlTypeSpec::Int64 => GenericIfData::Int64(
            parser.get_current_line_offset(),
            parser.get_integer_i64(context)?,
        ),
        A2mlTypeSpec::UChar => GenericIfData::UChar(
            parser.get_current_line_offset(),
            parser.get_integer_u8(context)?,
        ),
        A2mlTypeSpec::UInt => GenericIfData::UInt(
            parser.get_current_line_offset(),
            parser.get_integer_u16(context)?,
        ),
        A2mlTypeSpec::ULong => GenericIfData::ULong(
            parser.get_current_line_offset(),
            parser.get_integer_u32(context)?,
        ),
        A2mlTypeSpec::UInt64 => GenericIfData::UInt64(
            parser.get_current_line_offset(),
            parser.get_integer_u64(context)?,
        ),
        A2mlTypeSpec::Float => {
            GenericIfData::Float(parser.get_current_line_offset(), parser.get_float(context)?)
        }
        A2mlTypeSpec::Double => GenericIfData::Double(
            parser.get_current_line_offset(),
            parser.get_double(context)?,
        ),
        A2mlTypeSpec::Array(arraytype, dim) => {
            if **arraytype == A2mlTypeSpec::Char {
                GenericIfData::String(
                    parser.get_current_line_offset(),
                    parser.get_string_maxlen(context, *dim)?,
                )
            } else {
                let mut arrayitems = Vec::new();
                for _ in 0..*dim {
                    arrayitems.push(parse_ifdata_item(parser, context, arraytype)?);
                }
                GenericIfData::Array(arrayitems)
            }
        }
        A2mlTypeSpec::Enum(enumspec) => {
            let pos = parser.get_current_line_offset();
            let enumitem = parser.get_identifier(context)?;
            if enumspec.get(&enumitem).is_some() {
                GenericIfData::EnumItem(pos, enumitem)
            } else {
                return Err(ParserError::invalid_enum_value(parser, context, &enumitem));
            }
        }
        A2mlTypeSpec::Struct(structspec) => {
            let mut structitems = Vec::with_capacity(structspec.len());
            let line = parser.get_current_line_offset();
            for itemspec in structspec {
                structitems.push(parse_ifdata_item(parser, context, itemspec)?);
            }
            GenericIfData::Struct(parser.get_incfilename(context.fileid), line, structitems)
        }
        A2mlTypeSpec::Sequence(seqspec) => {
            let mut seqitems = Vec::new();
            let mut checkpoint = parser.get_tokenpos();
            while let Ok(item) = parse_ifdata_item(parser, context, seqspec) {
                seqitems.push(item);
                checkpoint = parser.get_tokenpos();
            }
            parser.set_tokenpos(checkpoint);
            GenericIfData::Sequence(seqitems)
        }
        A2mlTypeSpec::TaggedStruct(tsspec) => {
            GenericIfData::TaggedStruct(parse_ifdata_taggedstruct(parser, context, tsspec)?)
        }
        A2mlTypeSpec::TaggedUnion(tuspec) => {
            let mut result = HashMap::new();
            if let Some(taggeditem) = parse_ifdata_taggeditem(parser, context, tuspec)? {
                result.insert(taggeditem.tag.clone(), vec![taggeditem]);
            }
            GenericIfData::TaggedUnion(result)
        }
    })
}

// parse_ifdata_taggedstruct()
// parse all the tagged items of a TaggedStruct (TaggedUnions are not handled here because no loop is needed for those)
fn parse_ifdata_taggedstruct(
    parser: &mut ParserState,
    context: &ParseContext,
    tsspec: &HashMap<String, A2mlTaggedTypeSpec>,
) -> Result<HashMap<String, Vec<GenericIfDataTaggedItem>>, ParserError> {
    let mut result = HashMap::<String, Vec<GenericIfDataTaggedItem>>::new();
    while let Some(taggeditem) = parse_ifdata_taggeditem(parser, context, tsspec)? {
        if let Some(itemvec) = result.get_mut(&taggeditem.tag) {
            itemvec.push(taggeditem);
        } else {
            result.insert(taggeditem.tag.clone(), vec![taggeditem]);
        }
    }

    Ok(result)
}

// parse_ifdata_taggeditem()
// try to parse a TaggedItem inside a TaggedStruct or TaggedUnion
//
// behold the ridiculous^Wglorious return type:
//  - Parsing can fail completely because the file is structurally broken -> Outer layer is Result to handle this
//  - It is possible that the function succeeds but can't return a value -> middle layer of Option handles this
//  - finally, a GenericIfDataTaggedItem value is returned
fn parse_ifdata_taggeditem(
    parser: &mut ParserState,
    context: &ParseContext,
    spec: &HashMap<String, A2mlTaggedTypeSpec>,
) -> Result<Option<GenericIfDataTaggedItem>, ParserError> {
    let checkpoint = parser.get_tokenpos();
    // check if there is a tag
    if let Ok(Some((token, is_block, start_offset))) = parser.get_next_tag(context) {
        let tag = parser.get_token_text(token);

        // check if the tag is valid inside this TaggedStruct/TaggedUnion. If it is not, parsing should abort so that the caller sees the tag
        if let Some(taggedspec) = spec.get(tag) {
            if taggedspec.is_block != is_block {
                parser.set_tokenpos(checkpoint);
                return Ok(None);
            }
            let uid = parser.get_next_id();

            // parse the content of the tagged item
            let newcontext = ParseContext::from_token(tag, token, is_block);
            let data = parse_ifdata_item(parser, &newcontext, &taggedspec.item)?;
            let parsed_item = parse_ifdata_make_block(
                data,
                parser.get_incfilename(newcontext.fileid),
                newcontext.line,
            );

            let end_offset = parser.get_current_line_offset();
            // make sure that blocks that started with /begin end with /end
            if is_block {
                parser.expect_token(&newcontext, A2lTokenType::End)?;
                let endident = parser.expect_token(&newcontext, A2lTokenType::Identifier)?;
                let endtag = parser.get_token_text(endident);
                if endtag != tag {
                    return Err(ParserError::IncorrectEndTag {
                        filename: parser.filenames[context.fileid].to_string(),
                        error_line: parser.last_token_position,
                        tag: endtag.to_owned(),
                        block: newcontext.element.clone(),
                        block_line: newcontext.line,
                    });
                }
            }

            Ok(Some(GenericIfDataTaggedItem {
                incfile: parser.get_incfilename(newcontext.fileid),
                line: newcontext.line,
                uid,
                start_offset,
                end_offset,
                tag: tag.to_string(),
                data: parsed_item,
                is_block,
            }))
        } else {
            parser.set_tokenpos(checkpoint);
            Ok(None)
        }
    } else {
        parser.set_tokenpos(checkpoint);
        Ok(None)
    }
}

// parse_ifdata_make_block()
// turn the GenericIfData contained in a TaggedItem into a block.
fn parse_ifdata_make_block(
    data: GenericIfData,
    incfile: Option<String>,
    line: u32,
) -> GenericIfData {
    match data {
        GenericIfData::Struct(_, _, structitems) => GenericIfData::Block {
            incfile,
            line,
            items: structitems,
        },
        _ => GenericIfData::Block {
            incfile,
            line,
            items: vec![data],
        },
    }
}

pub(crate) fn parse_unknown_ifdata_start(
    parser: &mut ParserState,
    context: &ParseContext,
) -> Result<GenericIfData, ParserError> {
    let token_peek = parser.peek_token();
    // by convention, the elements inside of IF_DATA are wrapped in a taggedunion
    if let Some(A2lToken {
        ttype: A2lTokenType::Identifier,
        ..
    }) = token_peek
    {
        // first token is an identifier, so it could be a tag of a tagegdunion
        let start_offset = parser.get_current_line_offset();
        let token = parser.get_token(context)?;
        let tag = parser.get_token_text(token);
        let uid = parser.get_next_id();
        let newcontext = ParseContext::from_token(tag, token, false);
        let result = parse_unknown_ifdata(parser, &newcontext, true)?;
        let end_offset = parser.get_current_line_offset();
        let taggeditem = GenericIfDataTaggedItem {
            incfile: parser.get_incfilename(newcontext.fileid),
            line: newcontext.line,
            uid,
            start_offset,
            end_offset,
            tag: tag.to_string(),
            data: result,
            is_block: false,
        };
        let mut tuitem = HashMap::new();
        tuitem.insert(tag.to_string(), vec![taggeditem]);

        let items: Vec<GenericIfData> = vec![GenericIfData::TaggedUnion(tuitem)];

        Ok(GenericIfData::Block {
            incfile: parser.get_incfilename(context.fileid),
            line: start_offset,
            items,
        })
    } else {
        // first token cannot be a tag, so the format is totally unknown
        parse_unknown_ifdata(parser, context, true)
    }
}

// parse_unknown_ifdata()
// this function provides a fallback in case the data inside of an IF_DATA block cannot be
// parsed using either the built-in specification or the A2ML spec in the file
// If the data is strucutrally sane (matching /begin and /end tags), then it returns a result.
// The returned data is not intended to be useful for further processing, but only to preserve
// it so that is can be written out to a file again later.
pub(crate) fn parse_unknown_ifdata(
    parser: &mut ParserState,
    context: &ParseContext,
    is_block: bool,
) -> Result<GenericIfData, ParserError> {
    let mut items: Vec<GenericIfData> = Vec::new();
    let offset = parser.get_current_line_offset();

    loop {
        let token_peek = parser.peek_token();
        if token_peek.is_none() {
            return Err(ParserError::unexpected_eof(parser, context));
        }
        let token = token_peek.unwrap();

        match token.ttype {
            A2lTokenType::Identifier => {
                // an arbitrary identifier; it could be a tag of a taggedstruct, but we don't know that here. The other option is an enum value.
                items.push(GenericIfData::EnumItem(
                    parser.get_current_line_offset(),
                    parser.get_identifier(context)?,
                ));
            }
            A2lTokenType::String => {
                items.push(GenericIfData::String(
                    parser.get_current_line_offset(),
                    parser.get_string(context)?,
                ));
            }
            A2lTokenType::Number => {
                let line_offset = parser.get_current_line_offset();
                if let Ok(num) = parser.get_integer_i32(context) {
                    items.push(GenericIfData::Long(line_offset, num));
                } else {
                    // try again, looks like the number is a float instead
                    parser.undo_get_token();
                    let floatnum = parser.get_float(context)?; // if this also returns an error, it is neither int nor float, which is a genuine parse error
                    items.push(GenericIfData::Float(line_offset, floatnum));
                }
            }
            A2lTokenType::Begin => {
                // if this is directly within a block level element, then a new taggedstruct will be created to contain the new block element
                // if it is not, this block belongs to the parent and we only need to break and exit here
                if is_block {
                    items.push(parse_unknown_taggedstruct(parser, context)?);
                } else {
                    break;
                }
            }
            A2lTokenType::End => {
                // the end of this unknown block. Contained unknown blocks are handled recursively, so we don't see their /end tags in this loop
                break;
            }
            A2lTokenType::Include => { /* A2lTokenType::Include doesn't matter here */ }
        }
    }

    Ok(GenericIfData::Struct(
        parser.get_incfilename(context.fileid),
        offset,
        items,
    ))
}

// parse_unknown_taggedstruct()
// A taggedstruct is constructed to contain inner blocks, because the parse tree struct(block) is not permitted, while struct (taggedstruct(block)) is
// The function will interpret as much as possible of the following data as elements of the taggedstruct
fn parse_unknown_taggedstruct(
    parser: &mut ParserState,
    context: &ParseContext,
) -> Result<GenericIfData, ParserError> {
    let mut tsitems: HashMap<String, Vec<GenericIfDataTaggedItem>> = HashMap::new();

    while let Ok(Some((token, is_block, start_offset))) = parser.get_next_tag(context) {
        let uid = parser.get_next_id();
        let tag = parser.get_token_text(token);
        let newcontext = ParseContext::from_token(tag, token, true);
        let result = parse_unknown_ifdata(parser, &newcontext, is_block)?;

        let end_offset = parser.get_current_line_offset();
        if is_block {
            parser.expect_token(&newcontext, A2lTokenType::End)?;
            let endident = parser.expect_token(&newcontext, A2lTokenType::Identifier)?;
            let endtag = parser.get_token_text(endident);
            if endtag != tag {
                return Err(ParserError::IncorrectEndTag {
                    filename: parser.filenames[newcontext.fileid].to_string(),
                    error_line: parser.last_token_position,
                    tag: endtag.to_owned(),
                    block: newcontext.element.clone(),
                    block_line: newcontext.line,
                });
            }
        }

        let taggeditem = GenericIfDataTaggedItem {
            incfile: parser.get_incfilename(newcontext.fileid),
            line: newcontext.line,
            uid,
            start_offset,
            end_offset,
            tag: tag.to_string(),
            data: result,
            is_block,
        };

        if !tsitems.contains_key(tag) {
            tsitems.insert(tag.to_string(), vec![]);
        }
        tsitems.get_mut(tag).unwrap().push(taggeditem);
    }

    // There shouldn't be an unused /begin token after the loop has run. If there is, then this indicates that the file is damaged
    if let Some(A2lToken {
        ttype: A2lTokenType::Begin,
        ..
    }) = parser.peek_token()
    {
        return Err(ParserError::InvalidBegin {
            filename: parser.filenames[context.fileid].to_string(),
            error_line: parser.last_token_position,
            block: context.element.clone(),
        });
    }

    Ok(GenericIfData::TaggedStruct(tsitems))
}

pub(crate) fn remove_unknown_ifdata(a2l_file: &mut A2lFile) {
    for module in &mut a2l_file.project.module {
        remove_unknown_ifdata_from_list(&mut module.if_data);

        if let Some(mod_par) = &mut module.mod_par {
            for memory_layout in &mut mod_par.memory_layout {
                remove_unknown_ifdata_from_list(&mut memory_layout.if_data);
            }

            for memory_segment in &mut mod_par.memory_segment {
                remove_unknown_ifdata_from_list(&mut memory_segment.if_data);
            }
        }

        for axis_pts in &mut module.axis_pts {
            remove_unknown_ifdata_from_list(&mut axis_pts.if_data);
        }

        for blob in &mut module.blob {
            remove_unknown_ifdata_from_list(&mut blob.if_data);
        }

        for characteristic in &mut module.characteristic {
            remove_unknown_ifdata_from_list(&mut characteristic.if_data);
        }

        for frame in &mut module.frame {
            remove_unknown_ifdata_from_list(&mut frame.if_data);
        }

        for function in &mut module.function {
            remove_unknown_ifdata_from_list(&mut function.if_data);
        }

        for group in &mut module.group {
            remove_unknown_ifdata_from_list(&mut group.if_data);
        }

        for instance in &mut module.instance {
            remove_unknown_ifdata_from_list(&mut instance.if_data);
        }

        for measurement in &mut module.measurement {
            remove_unknown_ifdata_from_list(&mut measurement.if_data);
        }
    }
}

fn remove_unknown_ifdata_from_list(ifdata_list: &mut Vec<IfData>) {
    let mut new_ifdata_list = Vec::new();
    std::mem::swap(ifdata_list, &mut new_ifdata_list);
    for if_data in new_ifdata_list {
        if if_data.ifdata_valid {
            ifdata_list.push(if_data);
        }
    }
}

#[cfg(test)]
mod ifdata_test {
    use super::*;
    use crate::{self as a2lfile, Filename, IfData};

    crate::a2ml_specification! {
        <A2mlTest>

        block "IF_DATA" taggedunion if_data {
            "CHAR" char a;
            "INT" int b;
            "LONG" long c;
            "INT64" int64 d;
            "UCHAR" uchar e;
            "UINT" uint64 f;
            "ULONG" ulong g;
            "UINT64" uint64 h;
            "DOUBLE" double i;
            "FLOAT" float j;
            "STRUCT" struct structname {
                char[256];
                int;
            };
            block "BLOCK" taggedstruct tagged_struct {
                "TAG1" int intval;
            };
            "ENUM" enum EnumTest {
                "ENUMVAL1" = 1,
                "ENUMVAL2"
            } named_enum;
            "ARRAY" uint arr[3];
            block "SEQUENCE" (char[256] name)*;
            "NONE";
        };
    }

    #[test]
    fn parse_ifdata() {
        let result = parse_helper(r##"CHAR 5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.char, Some(Char { a: 5, .. })));

        let result = parse_helper(r##"CHAR xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"INT 5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.int, Some(Int { b: 5, .. })));

        let result = parse_helper(r##"INT xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"LONG 5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.long, Some(Long { c: 5, .. })));

        let result = parse_helper(r##"LONG xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"INT64 5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.int64, Some(Int64 { d: 5, .. })));

        let result = parse_helper(r##"INT64 xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"UCHAR 5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.uchar, Some(Uchar { e: 5, .. })));

        let result = parse_helper(r##"UCHAR xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"UINT 5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.uint, Some(Uint { f: 5, .. })));

        let result = parse_helper(r##"UINT xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"ULONG 5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.ulong, Some(Ulong { g: 5, .. })));

        let result = parse_helper(r##"ULONG xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"UINT64 5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.uint64, Some(Uint64 { h: 5, .. })));

        let result = parse_helper(r##"UINT64 xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"DOUBLE 5.5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.double, Some(Double { .. })));

        let result = parse_helper(r##"DOUBLE xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"FLOAT 5.5 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(matches!(decoded_ifdata.float, Some(Float { .. })));

        let result = parse_helper(r##"FLOAT xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"STRUCT "text value" 3 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        let var_struct = decoded_ifdata.var_struct.unwrap();
        assert_eq!(var_struct.item, "text value");
        assert_eq!(var_struct.item_2, 3);

        let result = parse_helper(r##"STRUCT 5.5 xyz /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"/begin BLOCK TAG1 3 /end BLOCK /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert_eq!(decoded_ifdata.block.unwrap().tag1.unwrap().intval, 3);

        let result = parse_helper(r##"ENUM ENUMVAL2 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert_eq!(
            decoded_ifdata.var_enum.unwrap().named_enum,
            EnumTest::Enumval2
        );

        let result = parse_helper(r##"ENUM NOTVALID /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result = parse_helper(r##"ARRAY 7 8 9 /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert_eq!(decoded_ifdata.array.unwrap().arr, [7, 8, 9]);

        let result = parse_helper(r##"ARRAY 7 8 "bad" /end IFDATA"##);
        assert!(result.is_ok());
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, false);

        let result =
            parse_helper(r##"/begin SEQUENCE "name 1" "name 2" /end SEQUENCE /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert_eq!(decoded_ifdata.sequence.unwrap().item, ["name 1", "name 2"]);

        let result = parse_helper(r##"NONE /end IFDATA"##);
        assert!(result.is_ok());
        let decoded_ifdata = check_and_decode(result);
        assert!(decoded_ifdata.none.is_some());
    }

    fn parse_helper(ifdata: &str) -> Result<(Option<GenericIfData>, bool), ParserError> {
        let token_result =
            a2lfile::tokenizer::tokenize(&Filename::from("test"), 0, ifdata).unwrap();
        let mut log_msgs = Vec::new();
        let ifdatas = [ifdata.to_string()];
        let filenames = [Filename::from("test")];
        let mut parser = ParserState::new_internal(
            &token_result.tokens,
            &ifdatas,
            &filenames,
            &mut log_msgs,
            false,
        );
        parser.builtin_a2mlspec = Some(
            a2lfile::a2ml::parse_a2ml(&Filename::from("test"), A2MLTEST_TEXT)
                .unwrap()
                .0,
        );
        super::parse_ifdata(
            &mut parser,
            &a2lfile::ParseContext {
                fileid: 0,
                line: 0,
                element: "IFDATA".to_string(),
                inside_block: true,
            },
        )
    }

    fn check_and_decode(result: Result<(Option<GenericIfData>, bool), ParserError>) -> A2mlTest {
        let (data, valid) = result.unwrap();
        assert!(data.is_some());
        assert_eq!(valid, true);
        let mut if_data = IfData::new();
        if_data.ifdata_items = data;
        if_data.ifdata_valid = valid;
        A2mlTest::load_from_ifdata(&if_data).unwrap()
    }
}
