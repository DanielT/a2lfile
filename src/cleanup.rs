use crate::specification::*;

mod compu_methods;
mod functions;
mod groups;
mod record_layouts;

pub(crate) fn cleanup(a2l_file: &mut A2lFile) {
    for module in &mut a2l_file.project.module {
        groups::cleanup(module);
        functions::cleanup(module);
        compu_methods::cleanup(module);
        record_layouts::cleanup(module);
    }
}

