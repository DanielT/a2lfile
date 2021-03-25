extern crate proc_macro;
use proc_macro::TokenStream;

mod a2lspec;
mod a2mlspec;
mod util;


#[proc_macro]
pub fn a2l_specification(tokens: TokenStream) -> TokenStream {
    a2lspec::a2l_specification(tokens)
}


#[proc_macro]
pub fn a2ml_specification(tokens: TokenStream) -> TokenStream {
    a2mlspec::a2ml_specification(tokens)
}
