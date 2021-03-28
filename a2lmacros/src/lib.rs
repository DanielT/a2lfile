extern crate proc_macro;
use proc_macro::TokenStream;

pub(crate) mod a2lspec;
pub(crate) mod a2mlspec;
mod util;


#[proc_macro]
pub fn a2l_specification(tokens: TokenStream) -> TokenStream {
    let tokens2: proc_macro2::TokenStream = tokens.into();
    a2lspec::a2l_specification(tokens2).into()
}


#[proc_macro]
pub fn a2ml_specification(tokens: TokenStream) -> TokenStream {
    let tokens2: proc_macro2::TokenStream = tokens.into();
    a2mlspec::a2ml_specification(tokens2).into()
}
