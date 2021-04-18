use std::cmp::Ordering;
use std::fmt::Write;
use std::collections::HashSet;

#[derive(Debug)]
enum A2lWriterItem{
    StaticItem(u32, String),
    TaggedGroup(Vec<(String, A2lWriter, bool)>),
}

#[derive(Debug)]
pub(crate) struct A2lWriter {
    file: Option<String>,
    pub(crate) line: u32,
    items: Vec<A2lWriterItem>,
}

#[derive(Debug)]
pub struct TaggedGroupHandle<'a> {
    owner: &'a mut A2lWriter,
    tagged_group_idx: usize
}


const INDENT_WIDTH: usize = 2;

impl A2lWriter {
    pub(crate) fn new(file: &Option<String>, line: u32) -> Self {
        Self {
            file: file.clone(),
            line,
            items: Vec::new(),
        }
    }

    pub(crate) fn add_fixed_item(&mut self, item: String, position: u32) -> &mut Self {
        self.items.push(A2lWriterItem::StaticItem(position, item));

        self
    }

    pub(crate) fn add_tagged_group(&mut self) -> TaggedGroupHandle {
        self.items.push(A2lWriterItem::TaggedGroup(Vec::new()));
        let idx = self.items.len() - 1;
        TaggedGroupHandle {
            owner: self,
            tagged_group_idx: idx
        }
    }


    // finish()
    // consumes self to build a String of the file
    pub(crate) fn finish(self) -> String {
        let (_, text) = self.finish_internal(0);

        // there is an extra space at the beginning of the output - in all other blocks
        // a separator between the tag and the content is needed. It is easier to remove
        // this space here than to add a special case to prevent it from being generated
        text[1..].to_owned()
    }


    // finish_internal()
    // recursively construct each indentation level of the file
    fn finish_internal(self, indent: usize) -> (u32, String) {
        let mut outstring = "".to_string();
        let mut current_line = self.line;
        let mut empty_block = true;
        for item in self.items {
            match item {
                A2lWriterItem::StaticItem(item_line, text) => {
                    // add the text of static items, while inserting linebreaks with indentation as needed
                    write!(outstring, "{}{}", make_whitespace(current_line, item_line, indent), text).unwrap();
                    current_line = item_line;
                }
                A2lWriterItem::TaggedGroup(mut group) => {
                    // sort the items in this group according to the sorting function
                    group.sort_by(Self::sort_function);
                    // build the text containing all of the group items and append it to the string for this block
                    let (last_line, text) = Self::finish_group(current_line, group, indent, empty_block);
                    write!(outstring, "{}", text).unwrap();
                    current_line = last_line;
                }
            }
            empty_block = false;
        }

        (current_line, outstring)
    }


    fn finish_group(start_line: u32, group: Vec<(String, A2lWriter, bool)>, indent: usize, empty_block: bool) -> (u32, String) {
        let mut outstring = "".to_string();
        let mut current_line = start_line;
        let mut included_files = HashSet::<String>::new();
        let grouplen = group.len();
        for (tag, item, is_block) in group {
            // check if the element should be written to this file, or if an /include statement should be generated instead
            if item.file.is_none() {
                // if needed add whitespace
                write!(outstring, "{}", make_whitespace(current_line, item.line, indent)).unwrap();

                // if the opening block tag shares the line with the previous opening block tag, then (probably) no additional indentation is needed
                // this is a hacky heuristic that fixes the indentation of IF_DATA blocks
                let newindent = if empty_block && current_line == item.line && grouplen == 1 {
                    indent
                } else {
                    indent + 1
                };
                // build the text for this block item
                let (endline, text) = item.finish_internal(newindent);
                current_line = endline;
                if is_block {
                    write!(outstring, "/begin ").unwrap();
                }
                write!(outstring, "{}{}", tag, text).unwrap();
                if is_block {
                    write!(outstring, "{}/end {}", make_whitespace(current_line, current_line + 1, indent), tag).unwrap();
                    current_line += 1;
                }
            } else {
                // this element came from an include file
                let incfile = item.file.as_ref().unwrap();
                // check if the /include has been added yet and add it if needed
                if included_files.get(incfile).is_none() {
                    write!(outstring, "{}/include \"{}\"", make_whitespace(current_line, current_line + 1, indent), incfile).unwrap();
                    current_line += 1;

                    included_files.insert(incfile.to_owned());
                }
            }
        }
        (current_line, outstring)
    }


    fn sort_function(a: &(String, A2lWriter, bool), b: &(String, A2lWriter, bool)) -> Ordering {
        let (tag_a, item_a, _) = a;
        let (tag_b, item_b, _) = b;

        // hard-code a little bit of odering of blocks: at the top level, ASAP2_VERSION must be the first block
        // within MODULE, A2ML must be present before there are any IF_DATA blocks so that these can be decoded
        if tag_a == "ASAP2_VERSION" || tag_a == "A2ML" {
            Ordering::Less
        } else if tag_b == "ASAP2_VERSION" || tag_b == "A2ML" {
            Ordering::Greater
        } else {
            // handle included elements
            if item_a.file.is_some() && item_b.file.is_some() {
                // both items are included
                let incname_a = item_a.file.as_ref().unwrap();
                let incname_b = item_b.file.as_ref().unwrap();

                // sort included elements alphabetically by the name of the file they were included from
                incname_a.cmp(incname_b)
            } else if item_a.file.is_some() && item_b.file.is_none() {
                // a included, b is not: put a first to group all included items at the beginnning
                Ordering::Less
            } else if item_a.file.is_none() && item_b.file.is_some() {
                // a not included, b is included: put b first to group all included items at the beginnning
                Ordering::Greater
            } else {
                // no special cases basd on the tag or include status
                // items that have a line number (i. they were loaded from an input file) come first
                // items without a line number (created at runtime) are placed at the end
                if item_a.line != 0 && item_b.line != 0 {
                    item_a.line.cmp(&item_b.line)
                } else if item_a.line != 0 && item_b.line == 0 {
                    Ordering::Less
                } else if item_a.line == 0 && item_b.line != 0 {
                    Ordering::Greater
                } else {
                    // neither item has a line number, sort them alphabetically by tag
                    tag_a.cmp(tag_b)
                }
            }
        }
    }
}


impl<'a> TaggedGroupHandle<'a> {
    pub(crate) fn add_tagged_item(&mut self, tag: &str, item: A2lWriter, is_block: bool) -> &mut Self {
        if let A2lWriterItem::TaggedGroup(tagmap) = &mut self.owner.items[self.tagged_group_idx] {
            // if item.line != 0 && self.owner.first_line < item.line {
            //     self.owner.first_line = item.line;
            // }

            tagmap.push((tag.to_string(), item, is_block));
        }
        self
    }
}


pub fn escape_string(value: &str) -> String {
    // todo
    value.to_string()
}


// make_whitespace()
// create whitespace between two elements, depending on which lines they are to be placed on:
// - same line, and both elements actually have line numbers: separate with a space
// - one line apart, or neither element has a line number, i.e. newly inserted: add a newline and indent
// - any other case: add two newlines and indent
fn make_whitespace(current_line: u32, item_line: u32, indent: usize) -> String {
    if current_line != 0 && current_line == item_line {
        " ".to_string()
    } else if current_line + 1 == item_line || (current_line == 0 && item_line == 0) {
        format!("\n{:1$}", "", indent * INDENT_WIDTH)
    } else {
        format!("\n\n{:1$}", "", indent * INDENT_WIDTH)
    }
}


pub(crate) fn format_u8(value: u8) -> String {
    if value > 100 {
        format!("0x{:02X}", value)
    } else {
        format!("{}", value)
    }
}

pub(crate) fn format_u16(value: u16) -> String {
    if value > 0xFF {
        format!("0x{:04X}", value)
    } else if value > 100 {
        format!("0x{:02X}", value)
    } else {
        format!("{}", value)
    }
}

pub(crate) fn format_u32(value: u32) -> String {
    if value > 0xFFFFFF {
        format!("0x{:08X}", value)
    } else if value > 0xFFFF {
        format!("0x{:06X}", value)
    } else if value > 0xFF {
        format!("0x{:04X}", value)
    } else if value > 100 {
        format!("0x{:02X}", value)
    } else {
        format!("{}", value)
    }
}

pub(crate) fn format_u64(value: u64) -> String {
    if value > 0xFFFFFFFFFFFFFF {
        format!("0x{:016X}", value)
    } else if value > 0xFFFFFFFFFFFF {
        format!("0x{:014X}", value)
    } else if value > 0xFFFFFFFFFF {
        format!("0x{:012X}", value)
    } else if value > 0xFFFFFFFF {
        format!("0x{:010X}", value)
    } else if value > 0xFFFFFF {
        format!("0x{:08X}", value)
    } else if value > 0xFFFF {
        format!("0x{:06X}", value)
    } else if value > 0xFF {
        format!("0x{:04X}", value)
    } else if value > 100 {
        format!("0x{:02X}", value)
    } else {
        format!("{}", value)
    }
}


pub(crate) fn format_i8(value: i8) -> String {
    if value < -100 || value > 100 {
        format!("0x{:02X}", value)
    } else {
        format!("{}", value)
    }
}

pub(crate) fn format_i16(value: i16) -> String {
    if value < -0x7f || value > 0x7F {
        format!("0x{:04X}", value)
    } else if value < -100 || value > 100 {
        format!("0x{:02X}", value)
    } else {
        format!("{}", value)
    }
}

pub(crate) fn format_i32(value: i32) -> String {
    if value < -0x7FFFFF || value > 0x7FFFFF {
        format!("0x{:08X}", value)
    } else if value < -0x7FFF || value > 0x7FFF {
        format!("0x{:06X}", value)
    } else if value < -0x7F || value > 0x7F {
        format!("0x{:04X}", value)
    } else if value < -100 || value > 100 {
        format!("0x{:02X}", value)
    } else {
        format!("{}", value)
    }
}

pub(crate) fn format_i64(value: i64) -> String {
    if value < -0x7FFFFFFFFFFFFF || value > 0x7FFFFFFFFFFFFF {
        format!("0x{:016X}", value)
    } else if value < -0x7FFFFFFFFFFF || value > 0x7FFFFFFFFFFF {
        format!("0x{:014X}", value)
    } else if value < -0x7FFFFFFFFF || value > 0x7FFFFFFFFF {
        format!("0x{:012X}", value)
    } else if value < -0x7FFFFFFF || value > 0x7FFFFFFF {
        format!("0x{:010X}", value)
    } else if value < -0x7FFFFF || value > 0x7FFFFF {
        format!("0x{:08X}", value)
    } else if value < -0x7FFF || value > 0x7FFF {
        format!("0x{:06X}", value)
    } else if value < -0x7F || value > 0x7F {
        format!("0x{:04X}", value)
    } else if value < -100 || value > 100 {
        format!("0x{:02X}", value)
    } else {
        format!("{}", value)
    }
}


pub(crate) fn format_float(value: f32) -> String {
    if value == 0f32 {
        "0".to_string()
    } else if value < -1e+10 || (-0.0001 < value && value < 0.0001) || 1e+10 < value {
        format!("{:e}", value)
    } else {
        format!("{}", value)
    }
}


pub(crate) fn format_double(value: f64) -> String {
    if value == 0f64 {
        "0".to_string()
    } else if value < -1e+10 || (-0.0001 < value && value < 0.0001) || 1e+10 < value {
        format!("{:e}", value)
    } else {
        format!("{}", value)
    }
}

