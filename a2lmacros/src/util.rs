use proc_macro2::*;

pub(crate) type TokenStreamIter = std::iter::Peekable<proc_macro2::token_stream::IntoIter>;

pub(crate) fn get_ident(token_iter: &mut TokenStreamIter) -> String {
    match token_iter.next() {
        Some(TokenTree::Ident(ident)) => ident.to_string(),
        Some(tok) => panic!("Expected an Ident, got: {:#?}", tok.to_string()),
        None => panic!("Expected an Ident, but reached the end of input"),
    }
}

pub(crate) fn get_string(token_iter: &mut TokenStreamIter) -> String {
    match token_iter.next() {
        Some(TokenTree::Literal(literal)) => {
            let raw_string = literal.to_string();

            let stripped = if raw_string.starts_with('"') {
                // "string" -> string
                raw_string.strip_prefix('"')
            } else {
                // r"string" -> string
                raw_string.strip_prefix("r\"")
            }
            .and_then(|no_prefix_str| no_prefix_str.strip_suffix('"'));

            match stripped {
                Some(val) => val.to_owned(),
                None => panic!("expected a string in double-quotes, got {}", raw_string),
            }
        }
        Some(tok) => panic!("Expected a string, got: {:#?}", tok.to_string()),
        None => panic!("Expected a string, but reached the end of input"),
    }
}

pub(crate) fn get_integer(token_iter: &mut TokenStreamIter) -> i32 {
    match token_iter.next() {
        Some(TokenTree::Literal(literal)) => {
            let strval = literal.to_string();
            if let Some(suffix) = strval.strip_prefix("0x") {
                i32::from_str_radix(suffix, 16).unwrap()
            } else {
                strval.parse().unwrap()
            }
        }
        Some(tok) => panic!("Expected an int literal, got: {:#?}", tok.to_string()),
        None => panic!("Expected an int literal, but reached the end of input"),
    }
}

pub(crate) fn get_float(token_iter: &mut TokenStreamIter) -> f32 {
    match token_iter.next() {
        Some(TokenTree::Literal(literal)) => literal.to_string().parse().unwrap(),
        Some(tok) => panic!("Expected a float literal, got: {:#?}", tok.to_string()),
        None => panic!("Expected a float literal, but reached the end of input"),
    }
}

pub(crate) fn get_group(token_iter: &mut TokenStreamIter, delim: Delimiter) -> TokenStream {
    match token_iter.next() {
        Some(TokenTree::Group(grp)) => {
            assert_eq!(
                grp.delimiter(),
                delim,
                "Expected a group inside of {:#?}, but got a group inside {:#?}",
                delim,
                grp.delimiter()
            );
            grp.stream()
        }
        Some(tok) => panic!("Expected a group, got {:#?}", tok.to_string()),
        None => panic!("Expected a group, but reached the end of input"),
    }
}

pub(crate) fn get_punct(token_iter: &mut TokenStreamIter) -> char {
    match token_iter.next() {
        Some(TokenTree::Punct(p)) => p.as_char(),
        Some(TokenTree::Group(_)) => panic!("Expected Punct, got a Group(...)"),
        Some(tok) => panic!("Expected Punct, got: {:#?}", tok.to_string()),
        None => panic!("Expected Punct, but reached the end of input"),
    }
}

pub(crate) fn require_punct(token_iter: &mut TokenStreamIter, reqchar: char) {
    let cur_pkchar = get_punct(token_iter);
    assert_eq!(
        cur_pkchar, reqchar,
        "Punctuation character '{}' is required in this position, but '{}' was found instead",
        reqchar, cur_pkchar
    );
}

// parse_optional_comment()
// check if there is a rust-style doc comment and parse it
pub(crate) fn parse_optional_comment(token_iter: &mut TokenStreamIter) -> Option<String> {
    if let Some(TokenTree::Punct(_)) = token_iter.peek() {
        // comments beginning with "///"" in the input are converted to rust's doc-comment form: #[doc="comment string here"]
        require_punct(token_iter, '#');
        let doc_tokens = get_group(token_iter, Delimiter::Bracket);
        let doc_iter = &mut doc_tokens.into_iter().peekable();
        let doc_ident = get_ident(doc_iter);
        if doc_ident == "doc" {
            require_punct(doc_iter, '=');
            Some(get_string(doc_iter))
        } else {
            None
        }
    } else {
        None
    }
}

// convert an uppercase name with underscores to a CamelCase type name (e.g. COMPU_METHOD -> CompuMethod)
// it the name contains any lowercase characters, it is not considered to be an uppercase name and is passed through unchanged
pub(crate) fn ucname_to_typename(blkname: &str) -> String {
    let namechars: Vec<char> = blkname.chars().collect();
    let mut outchars: Vec<char> = Vec::new();
    let mut capitalise_next = true;
    let mut is_ucname = true;

    for c in namechars {
        if c.is_ascii_lowercase() {
            is_ucname = false;
        }
        if c == '_' {
            capitalise_next = true;
            continue;
        }

        if capitalise_next {
            outchars.push(c); // c is expected to already be uppercase
        } else {
            outchars.push(c.to_lowercase().next().unwrap());
        }
        capitalise_next = false;
    }

    if is_ucname {
        outchars.iter().collect()
    } else {
        blkname.to_owned()
    }
}

const RUST_RESERVED_KEYWORDS: [&str; 51] = [
    "abstract", "as", "async", "await", "become", "box", "break", "const", "continue", "crate",
    "do", "dyn", "else", "enum", "extern", "false", "final", "fn", "for", "if", "impl", "in",
    "let", "loop", "macro", "match", "mod", "move", "mut", "override", "priv", "pub", "ref",
    "return", "Self", "self", "static", "struct", "super", "trait", "true", "try", "type",
    "typeof", "unsafe", "unsized", "use", "virtual", "where", "while", "yield",
];

// convert an uppercase name to lowercase, then check that the varname does not collide with a Rust keyword
pub(crate) fn make_varname(blkname: &str) -> String {
    let mut lcname = blkname.to_ascii_lowercase();
    if RUST_RESERVED_KEYWORDS.iter().any(|kw| kw == &lcname) {
        lcname = format!("var_{}", lcname);
    }
    lcname
}
