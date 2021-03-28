use proc_macro2::*;

pub(crate) type TokenStreamIter = std::iter::Peekable<proc_macro2::token_stream::IntoIter>;


pub(crate) fn get_ident(token_iter: &mut TokenStreamIter) -> String {
    let tok: TokenTree = token_iter.next().unwrap();
    match tok {
        TokenTree::Ident(ident) => ident.to_string(),
        _ => panic!("Expected an Ident in this position, got: {:#?}", tok.to_string())
    }
}


pub(crate) fn get_string(token_iter: &mut TokenStreamIter) -> String {
    let tok: TokenTree = token_iter.next().unwrap();
    match tok {
        TokenTree::Literal(literal) => {
            let raw_string = literal.to_string();
            let chars: Vec<char> = raw_string.chars().collect();
            let startoffset = if chars[0] == '"' {
                1
            } else if chars[0] == 'r' && chars[1] == '"' {
                2
            } else {
                panic!("expected a string in double-quotes, got {}", raw_string);
            };
            let stripped_string = chars[startoffset..chars.len()-1].iter().collect();
            stripped_string
        }
        _ => panic!("Expected a string in this position, got: {:#?}", tok.to_string())
    }
}


pub(crate) fn get_integer(token_iter: &mut TokenStreamIter) -> i32 {
    let tok: TokenTree = token_iter.next().unwrap();
    match tok {
        TokenTree::Literal(literal) => {
            literal.to_string().parse().unwrap()
        }
        _ => panic!("Expected a string in this position, got: {:#?}", tok.to_string())
    }
}


pub(crate) fn get_group(token_iter: &mut TokenStreamIter, delim: Delimiter) -> TokenStream {
    let tok: TokenTree = token_iter.next().unwrap();
    match tok {
        TokenTree::Group(grp) => {
            if grp.delimiter() != delim {
                panic!("Expected a group inside of {:#?}, but got a group inside {:#?}", delim, grp.delimiter());
            }
            grp.stream()
        }
        _ => {
            panic!("expected a group, got {:#?}", tok.to_string());
        }
    }
}


pub(crate) fn get_punct(token_iter: &mut TokenStreamIter) -> char {
    let tok: TokenTree = token_iter.next().unwrap();
    match tok {
        TokenTree::Punct(p) => p.as_char(),
        TokenTree::Group(_) => panic!("Expected Punct in this position, got a Group(...)"),
        _ => panic!("Expected Punct in this position, got: {:#?}", tok.to_string())
    }
}


pub(crate) fn require_punct(token_iter: &mut TokenStreamIter, reqchar: char) {
    let cur_pkchar = get_punct(token_iter);
    if cur_pkchar != reqchar {
        panic!("Punctuation character '{}' is required in this position, but '{}' was found instead", reqchar, cur_pkchar);
    }
}




// convert an uppercase name with underscores to a CamelCase type name (e.g. COMPU_METHOD -> CompuMethod)
// it the name contains any lowercase characters, it is not considered to be an uppercase name and is passed through unchanged
pub(crate) fn ucname_to_typename(blkname: &str) -> String {
    let namechars: Vec<char> = blkname.chars().collect();
    let mut outchars:Vec<char> = Vec::new();
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
    "abstract", "as", "async", "await", "become", "box", "break", "const", "continue", "crate", "do", "dyn",
    "else", "enum", "extern", "false", "final", "fn", "for", "if", "impl", "in", "let", "loop", "macro",
    "match", "mod", "move", "mut", "override", "priv", "pub", "ref", "return", "Self", "self", "static",
    "struct", "super", "trait", "true", "try", "type", "typeof", "unsafe", "unsized", "use", "virtual",
    "where", "while", "yield"];

// convert an uppercase name to lowercase, then check that the varname does not collide with a Rust keyword
pub(crate) fn make_varname(blkname: &str) -> String {
    let mut lcname = blkname.to_ascii_lowercase();
    if RUST_RESERVED_KEYWORDS.iter().find(|kw| kw == &&&lcname).is_some() {
        lcname = format!("var_{}", lcname);
    }
    lcname
}
