//! a2lmacros is a crate for internal use by the a2lfile crate. It contains the proc macros used by a2lfile.

extern crate proc_macro;
use proc_macro::TokenStream;

pub(crate) mod a2lspec;
pub(crate) mod a2mlspec;
pub(crate) mod codegenerator;
pub(crate) mod util;


#[proc_macro]
pub fn a2l_specification(tokens: TokenStream) -> TokenStream {
    let tokens2: proc_macro2::TokenStream = tokens.into();
    a2lspec::a2l_specification(tokens2).into()
}


/**
The a2ml_specification! macro enables application to conveniently decode and use IF_DATA defined though A2ML.
The macro uses an "enhanced A2ML" language to define the items.
Rust data structures and associated code will be generated for items defined inside of the macro.

For example a file might have the following definition inside its A2ML block:
```
block "IF_DATA" taggedunion {
    "SOME_DATA" struct {
        uint;
        uint;
    }
}
```
The a2ml_specification! can process this form directly, but the struct and its members have no names.
The "enhanced A2ML" form of this would be:

```
block "IF_DATA" taggedunion {
    "SOME_DATA" struct SomeData {
        uint month;
        uint day;
    }
}
```
Finally, the A2ml inside the macro must be preceded by a header that names the content; all together it looks like this:
```
a2ml_specification! {
    <MyA2mlSpec>
    block "IF_DATA" taggedunion {
        "SOME_DATA" struct SomeData {
            uint month; /// a doc comment for month that will be preserved
            uint day; /// a doc comment for day
        }
    }
}
```
The macro will also generate a text constant containing standard (basic) A2ML matching the enhanced definition in the macro; it is called uppercase(macroname)_text.
For the example it is MYA2MLSPEC_TEXT.

Now you can load IfData from an a2l file:
```
let a2l_file: A2lFile = a2lfile::load(the_filename, Some(MYA2MLSPEC_TEXT.to_string()), &mut logger, false);
let my_data = MyA2mlSpec::load_from_ifdata(&a2l_file.project.modules[0].if_data[0]);
```

*/
#[proc_macro]
pub fn a2ml_specification(tokens: TokenStream) -> TokenStream {
    let tokens2: proc_macro2::TokenStream = tokens.into();
    a2mlspec::a2ml_specification(tokens2).into()
}
