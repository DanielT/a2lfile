use std::cmp::Ordering;
use std::collections::HashSet;
use std::fmt::Write;

use crate::Comment;

#[derive(Debug)]
pub(crate) struct Writer {
    indent: usize,
    outstring: String,
}

#[derive(Debug, Clone)]
pub(crate) enum TaggedItemInfo<'a> {
    // IncludedTag {
    //     tag: &'a str,
    //     incfile: &'a Option<String>,
    //     uid: u32,
    //     line: u32,
    //     start_offset: u32,
    //     end_offset: u32,
    //     is_block: bool,
    //     item_text: String,
    //     position_restriction: Option<u16>,
    // },
    Tag {
        tag: &'a str,
        incfile: &'a Option<String>,
        uid: u32,
        line: u32,
        start_offset: u32,
        end_offset: u32,
        is_block: bool,
        item_text: String,
        position_restriction: Option<u16>,
    },
    Comment {
        comment: &'a str,
        is_included: bool,
        uid: u32,
        line: u32,
        start_offset: u32,
    },
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

    // add a string to the output and prefix it with whitespace
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
        if value.contains(['\'', '"', '\\', '\r', '\n', '\t']) {
            let input_chars: Vec<char> = value.chars().collect();

            for c in input_chars {
                match c {
                    '\'' | '"' | '\\' => {
                        self.outstring.push('\\');
                        self.outstring.push(c);
                    }
                    '\r' => {
                        // non-standard in a2l files
                        self.outstring.push('\\');
                        self.outstring.push('r');
                    }
                    '\n' => {
                        self.outstring.push('\\');
                        self.outstring.push('n');
                    }
                    '\t' => {
                        self.outstring.push('\\');
                        self.outstring.push('t');
                    }
                    _ => self.outstring.push(c),
                }
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
        if is_hex {
            write!(self.outstring, "0x{value:0X}").unwrap();
        } else {
            write!(self.outstring, "{value}").unwrap();
        }
    }

    pub(crate) fn add_float<T>(&mut self, value: T, offset: u32)
    where
        T: std::convert::Into<f64>,
    {
        let value_conv = value.into();
        self.add_whitespace(offset);
        if value_conv == 0f64 {
            self.outstring.push('0');
        } else if value_conv < -1e+10
            || (-0.0001 < value_conv && value_conv < 0.0001)
            || 1e+10 < value_conv
        {
            write!(self.outstring, "{value_conv:e}").unwrap();
        } else {
            write!(self.outstring, "{value_conv}").unwrap();
        }
    }

    pub(crate) fn add_group(&mut self, mut group: Vec<TaggedItemInfo>) {
        // intially sort the group items by their id / name / etc
        group.sort_by(Self::sort_function);
        // apply position restrictions if there are any restricted items, e.g. at the top level and inside RECORD_LAYOUT
        apply_position_restrictions(&mut group);

        let mut included_files = HashSet::<String>::new();

        for item in group {
            match item {
                TaggedItemInfo::Tag {
                    tag,
                    incfile,
                    start_offset,
                    end_offset,
                    is_block,
                    item_text,
                    ..
                } => {
                    if let Some(incname) = incfile {
                        if !included_files.contains(incname) {
                            self.add_whitespace(start_offset);
                            self.outstring.push_str("/include \"");
                            self.outstring.push_str(incname);
                            self.outstring.push('"');

                            included_files.insert(incname.to_owned());
                        }
                    } else {
                        self.add_whitespace(start_offset);
                        if is_block {
                            self.outstring.push_str("/begin ");
                        }
                        self.outstring.push_str(tag);
                        self.outstring.push_str(&item_text);
                        if is_block {
                            self.add_whitespace(end_offset);
                            self.outstring.push_str("/end ");
                            self.outstring.push_str(tag);
                        }
                    }
                }
                TaggedItemInfo::Comment {
                    comment,
                    is_included,
                    start_offset,
                    ..
                } => {
                    if !is_included {
                        // don't use self.add_whitespace() here, because comments don't follow indentation rules
                        // if the comment was indented when it was parsed, then the indentation is preserved in the comment
                        for _ in 0..start_offset {
                            self.outstring.push('\n');
                        }
                        self.outstring.push_str(comment);
                    }
                }
            }
        }
    }

    fn add_whitespace(&mut self, offset: u32) {
        if offset == 0 {
            self.outstring.push(' ');
        } else {
            for _ in 0..offset {
                self.outstring.push('\n');
            }
            for _ in 0..self.indent {
                self.outstring.push_str("  ");
            }
        }
    }

    fn sort_function(a: &TaggedItemInfo, b: &TaggedItemInfo) -> Ordering {
        if a.uid() == 0 && b.uid() != 0 {
            Ordering::Greater
        } else if b.uid() == 0 && a.uid() != 0 {
            Ordering::Less
        } else if a.uid() == b.uid() {
            // probably both uids are zero
            if a.line() == b.line() {
                // both uid and line are equal - newly created elements
                a.tag().cmp(b.tag())
            } else {
                a.line().cmp(&b.line())
            }
        } else {
            a.uid().cmp(&b.uid())
        }
    }

    pub(crate) fn finish(self) -> String {
        self.outstring
    }
}

impl TaggedItemInfo<'_> {
    fn uid(&self) -> u32 {
        match self {
            TaggedItemInfo::Tag { uid, .. } | TaggedItemInfo::Comment { uid, .. } => *uid,
        }
    }

    fn line(&self) -> u32 {
        match self {
            TaggedItemInfo::Tag { line, .. } | TaggedItemInfo::Comment { line, .. } => *line,
        }
    }

    fn tag(&self) -> &str {
        match self {
            TaggedItemInfo::Tag { tag, .. } => tag,
            TaggedItemInfo::Comment { .. } => "", // no tag for comments
        }
    }

    fn position_restriction(&self) -> Option<u16> {
        match self {
            TaggedItemInfo::Tag {
                position_restriction,
                ..
            } => *position_restriction,
            TaggedItemInfo::Comment { .. } => None, // no position restriction for comments
        }
    }
}

fn apply_position_restrictions(group: &mut [TaggedItemInfo]) {
    let positions: Vec<usize> = group
        .iter()
        .enumerate()
        .filter(|(_, item)| item.position_restriction().is_some())
        .map(|(idx, _)| idx)
        .collect();
    let len = positions.len();
    if len > 1 {
        let mut items: Vec<TaggedItemInfo> = group
            .iter()
            .filter(|item| item.position_restriction().is_some())
            .cloned()
            .collect();
        items.sort_by_key(TaggedItemInfo::position_restriction);

        for idx in 0..len {
            group[positions[idx]] = items[idx].clone();
        }
    }
}

pub(crate) fn add_comments_to_group<'a>(
    group: &mut Vec<TaggedItemInfo<'a>>,
    comment_list: &'a [Comment],
) {
    // add the comments to the group
    for item in comment_list {
        group.push(TaggedItemInfo::Comment {
            comment: &item.comment,
            is_included: item.is_included,
            uid: item.uid,
            line: item.line,
            start_offset: item.start_offset,
        });
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn write_str() {
        let mut writer = Writer::new(2);
        writer.add_str("test", 0);
        // test is not a block, so no indentation, but one space is added for separation
        assert_eq!(writer.finish(), " test");
    }

    #[test]
    fn write_str_raw() {
        let mut writer = Writer::new(2);
        writer.add_str_raw(" has leading whitespace", 0);
        assert_eq!(writer.finish(), " has leading whitespace");

        let mut writer = Writer::new(2);
        writer.add_str_raw("no leading whitspace", 0);
        assert_eq!(writer.finish(), " no leading whitspace");
    }

    #[test]
    fn write_quoted_string() {
        let mut writer = Writer::new(2);
        writer.add_quoted_string("test:\rabc\ndef\tghi\'jkl\"nmo\\pqr", 0);
        // test is not a block, so no indentation, but one space is added for separation
        assert_eq!(writer.finish(), r#" "test:\rabc\ndef\tghi\'jkl\"nmo\\pqr""#);
    }

    #[test]
    fn write_integer() {
        let mut writer = Writer::new(2);
        writer.add_integer(123, false, 0);
        assert_eq!(writer.finish(), " 123");

        let mut writer = Writer::new(2);
        writer.add_integer(123, true, 0);
        assert_eq!(writer.finish(), " 0x7B");
    }

    #[test]
    fn write_float() {
        // test with default precision
        let mut writer = Writer::new(2);
        writer.add_float(123.456, 0);
        assert_eq!(writer.finish(), " 123.456");

        // very small value -> scientific notation
        let mut writer = Writer::new(2);
        writer.add_float(0.0000123456, 0);
        assert_eq!(writer.finish(), " 1.23456e-5");

        // very large value -> scientific notation
        let mut writer = Writer::new(2);
        writer.add_float(123456000000.0, 0);
        assert_eq!(writer.finish(), " 1.23456e11");

        // zero value
        let mut writer = Writer::new(2);
        writer.add_float(0.0, 0);
        assert_eq!(writer.finish(), " 0");
    }

    #[test]
    fn write_group() {
        let mut writer = Writer::new(0);
        let group = vec![
            TaggedItemInfo::Tag {
                tag: "MEASUREMENT",
                incfile: &None,
                uid: 0,
                line: 1,
                start_offset: 1,
                end_offset: 1,
                is_block: true,
                item_text: "".to_string(),
                position_restriction: None,
            },
            TaggedItemInfo::Tag {
                tag: "CHARACTERISTIC",
                incfile: &None,
                uid: 0,
                line: 0,
                start_offset: 0,
                end_offset: 1,
                is_block: true,
                item_text: "".to_string(),
                position_restriction: None,
            },
        ];
        // the group gets sorted; by line number the CHARACTERISTIC comes first
        writer.add_group(group);
        assert_eq!(
            writer.finish(),
            " /begin CHARACTERISTIC\n/end CHARACTERISTIC\n/begin MEASUREMENT\n/end MEASUREMENT"
        );
    }
}
