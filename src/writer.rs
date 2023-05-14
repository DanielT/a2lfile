use std::cmp::Ordering;
use std::collections::HashSet;
use std::convert::TryInto;
use std::fmt::Write;

use crate::specification::BlockInfo;
use crate::GenericIfDataTaggedItem;

#[derive(Debug)]
pub(crate) struct Writer {
    indent: usize,
    outstring: String,
}

pub(crate) struct TaggedItemInfo<'a> {
    tag: &'a str,
    incfile: &'a Option<String>,
    uid: u32,
    line: u32,
    start_offset: u32,
    end_offset: u32,
    is_block: bool,
    item_text: String,
}

impl Writer {
    pub(crate) fn new(indent: usize) -> Self {
        Self {
            indent,
            /* using an initial capacity of 1024 means that usually only MODULE will have to
            reallocate while adding elements. This is a measurable speed improvement. */
            outstring: String::with_capacity(1024),
        }
    }

    // add a string to the outout and prefix it with whitespace
    pub(crate) fn add_str(&mut self, text: &str, offset: u32) {
        self.add_whitespace(offset);
        self.outstring.push_str(text);
    }

    // add a string to the output, prefixed with whitespace only if the string does not already contain any
    // this is used to write A2ML blocks
    pub(crate) fn add_str_raw(&mut self, text: &str, offset: u32) {
        if !text.starts_with(|c: char| c.is_whitespace()) {
            self.add_whitespace(offset);
        }
        self.outstring.push_str(text);
    }

    pub(crate) fn add_quoted_string(&mut self, value: &str, offset: u32) {
        self.add_whitespace(offset);
        self.outstring.push('"');

        // escaping lots of strings is an expensive operation, so check if anything needs to be done first
        if value.contains(|c| c == '\'' || c == '"' || c == '\\' || c == '\n' || c == '\t') {
            let input_chars: Vec<char> = value.chars().collect();

            for c in input_chars {
                if c == '\'' || c == '"' || c == '\\' || c == '\n' || c == '\t' {
                    self.outstring.push('\\');
                }
                self.outstring.push(c);
            }
        } else {
            self.outstring.push_str(value);
        }
        self.outstring.push('"');
    }

    pub(crate) fn add_integer<T>(&mut self, value: T, is_hex: bool, offset: u32)
    where
        T: std::fmt::Display + std::fmt::UpperHex,
    {
        self.add_whitespace(offset);
        if !is_hex {
            write!(self.outstring, "{}", value).unwrap();
        } else {
            write!(self.outstring, "0x{:0X}", value).unwrap();
        }
    }

    pub(crate) fn add_float<T>(&mut self, value: T, offset: u32)
    where
        T: std::convert::Into<f64>,
    {
        let value_conv = value.try_into().unwrap();
        self.add_whitespace(offset);
        if value_conv == 0f64 {
            self.outstring.push('0');
        } else if value_conv < -1e+10
            || (-0.0001 < value_conv && value_conv < 0.0001)
            || 1e+10 < value_conv
        {
            write!(self.outstring, "{:e}", value_conv).unwrap();
        } else {
            write!(self.outstring, "{}", value_conv).unwrap();
        }
    }

    pub(crate) fn add_group(&mut self, mut group: Vec<TaggedItemInfo>) {
        group.sort_by(Self::sort_function);
        let mut included_files = HashSet::<String>::new();

        for item in group {
            if let Some(incname) = item.incfile {
                if included_files.get(incname).is_none() {
                    self.add_whitespace(item.start_offset);
                    self.outstring.push_str("/include \"");
                    self.outstring.push_str(incname);
                    self.outstring.push('"');

                    included_files.insert(incname.to_owned());
                }
            } else {
                self.add_whitespace(item.start_offset);
                if item.is_block {
                    self.outstring.push_str("/begin ");
                }
                self.outstring.push_str(item.tag);
                self.outstring.push_str(&item.item_text);
                if item.is_block {
                    self.add_whitespace(item.end_offset);
                    self.outstring.push_str("/end ");
                    self.outstring.push_str(item.tag);
                }
            }
        }
    }

    fn add_whitespace(&mut self, offset: u32) {
        match offset {
            0 => {
                self.outstring.push(' ');
            }
            _ => {
                for _ in 0..offset {
                    self.outstring.push('\n');
                }
                for _ in 0..self.indent {
                    self.outstring.push_str("  ");
                }
            }
        }
    }

    fn sort_function(a: &TaggedItemInfo, b: &TaggedItemInfo) -> Ordering {
        if a.uid == 0 && b.uid != 0 {
            Ordering::Greater
        } else if b.uid == 0 && a.uid != 0 {
            Ordering::Less
        } else if a.uid == b.uid {
            // probably both uids are zero
            if a.line == b.line {
                // both uid and line are equal - newly created elements
                a.tag.cmp(b.tag)
            } else {
                a.line.cmp(&b.line)
            }
        } else {
            a.uid.cmp(&b.uid)
        }
    }

    pub(crate) fn finish(self) -> String {
        self.outstring
    }
}

impl<'a> TaggedItemInfo<'a> {
    // build a TaggedItemInfo from a normal block which has BlockInfo
    pub(crate) fn build<T>(
        tag: &'a str,
        item_text: String,
        is_block: bool,
        block_info: &'a BlockInfo<T>,
    ) -> Self {
        Self {
            tag,
            incfile: &block_info.incfile,
            uid: block_info.uid,
            line: block_info.line,
            start_offset: block_info.start_offset,
            end_offset: block_info.end_offset,
            is_block,
            item_text,
        }
    }

    // build a TaggedItemInfo from a generic IF_DATA block which has GenericIfDataTaggedItem information
    pub(crate) fn build_generic(item_text: String, gti: &'a GenericIfDataTaggedItem) -> Self {
        Self {
            tag: &gti.tag,
            incfile: &gti.incfile,
            line: gti.line,
            uid: gti.uid,
            start_offset: gti.start_offset,
            end_offset: gti.end_offset,
            is_block: gti.is_block,
            item_text,
        }
    }
}
