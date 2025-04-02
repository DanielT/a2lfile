use std::collections::HashMap;
use std::collections::HashSet;

use crate::specification::{Function, FunctionList, Module};

pub(crate) fn cleanup(module: &mut Module) {
    // remove any references to non-existent functions. Usually there shouldn't be any.
    remove_broken_func_refs(module);

    // remove references to non-existent objects from functions
    remove_broken_object_refs(module);

    // Function can refer to objects (characteristics, etc.) and objects can refer to functions.
    // This is super ugly, because ther is no clear reason to prefer one over the other.
    // A Function can be removed if nothing refers to it, and it doesn't refer to anything
    let used_functions = get_used_functions(module);

    let mut name2idx = HashMap::<String, usize>::new();
    for (idx, func) in module.function.iter().enumerate() {
        name2idx.insert(func.name.clone(), idx);
    }

    let mut user_of = vec![Vec::<usize>::new(); module.function.len()];
    let mut delete_queue: Vec<usize> = vec![];
    for (idx, func) in module.function.iter_mut().enumerate() {
        // build up a reverse reference list, i.e. for each function, which other functions list it as a sub-function
        if let Some(sub_function) = &func.sub_function {
            for name in &sub_function.identifier_list {
                if let Some(subidx) = name2idx.get(name) {
                    user_of[*subidx].push(idx);
                }
            }
        }

        if !used_functions.contains(&func.name) && is_function_empty(func) {
            delete_queue.push(idx);
        }
    }

    let mut to_delete = vec![false; module.function.len()];
    while let Some(del_idx) = delete_queue.pop() {
        let name = module.function[del_idx].name.clone();
        to_delete[del_idx] = true;

        // for all functions that have a sub_function reference to this to-be-deleted function
        for refidx in &user_of[del_idx] {
            if let Some(sg) = &mut module.function[*refidx].sub_function {
                // remove the reference to the deleted function from the sub-function list of the referencing function
                sg.identifier_list.retain(|item| *item != name);
                if sg.identifier_list.is_empty() {
                    module.function[*refidx].sub_function = None;
                }
            }
            // if the function referencing the current function became empty after the
            // removal of the reference, then it is also queued for deletion
            if !used_functions.contains(&module.function[*refidx].name)
                && is_function_empty(&module.function[*refidx])
            {
                delete_queue.push(*refidx);
            }
        }
    }

    // retain only those items in module.function where the matching to_delete flag is false
    let mut del_iter = to_delete.iter();
    module.function.retain(|_| !del_iter.next().unwrap());
}

fn get_used_functions(module: &Module) -> HashSet<String> {
    let mut used_funcs = HashSet::new();
    for item in &module.axis_pts {
        insert_func_names(&item.function_list, &mut used_funcs);
    }
    for item in &module.characteristic {
        insert_func_names(&item.function_list, &mut used_funcs);
    }
    for item in &module.measurement {
        insert_func_names(&item.function_list, &mut used_funcs);
    }
    for item in &module.group {
        insert_func_names(&item.function_list, &mut used_funcs);
    }

    used_funcs
}

fn insert_func_names(function_list: &Option<FunctionList>, used_funcs: &mut HashSet<String>) {
    if let Some(func_list) = function_list {
        for func in &func_list.name_list {
            used_funcs.insert(func.to_owned());
        }
    }
}

fn is_function_empty(func: &Function) -> bool {
    let ref_c_empty = if let Some(ref_c) = &func.ref_characteristic {
        ref_c.identifier_list.is_empty()
    } else {
        true
    };
    let def_c_empty = if let Some(def_c) = &func.def_characteristic {
        def_c.identifier_list.is_empty()
    } else {
        true
    };
    let in_meas_empty = if let Some(in_meas) = &func.in_measurement {
        in_meas.identifier_list.is_empty()
    } else {
        true
    };
    let loc_meas_empty = if let Some(loc_meas) = &func.loc_measurement {
        loc_meas.identifier_list.is_empty()
    } else {
        true
    };
    let out_meas_empty = if let Some(out_meas) = &func.out_measurement {
        out_meas.identifier_list.is_empty()
    } else {
        true
    };
    let sub_func_empty = if let Some(sub_func) = &func.sub_function {
        sub_func.identifier_list.is_empty()
    } else {
        true
    };

    ref_c_empty
        && def_c_empty
        && in_meas_empty
        && loc_meas_empty
        && out_meas_empty
        && sub_func_empty
}

// remove references to nonexistent functions from all places where function references are possible
fn remove_broken_func_refs(module: &mut Module) {
    // get the set of all names of existing functions
    let existing_functions: HashSet<String> = module.function.keys().cloned().collect();

    // keep only references to existing functions, dropping any that don't exist
    for axispts in &mut module.axis_pts {
        if let Some(function_list) = &mut axispts.function_list {
            function_list
                .name_list
                .retain(|name| existing_functions.contains(name));
        }
    }

    for chara in &mut module.characteristic {
        if let Some(function_list) = &mut chara.function_list {
            function_list
                .name_list
                .retain(|name| existing_functions.contains(name));
        }
    }

    for meas in &mut module.measurement {
        if let Some(function_list) = &mut meas.function_list {
            function_list
                .name_list
                .retain(|name| existing_functions.contains(name));
        }
    }

    for group in &mut module.group {
        if let Some(function_list) = &mut group.function_list {
            function_list
                .name_list
                .retain(|name| existing_functions.contains(name));
        }
    }

    for function in &mut module.function {
        if let Some(sub_functions) = &mut function.sub_function {
            sub_functions
                .identifier_list
                .retain(|name| existing_functions.contains(name));
        }
    }
}

fn remove_broken_object_refs(module: &mut Module) {
    let mut function = std::mem::take(&mut module.function);
    let objects = module.objects();

    // retain only references to existing objects
    for func in &mut function {
        if let Some(ref_characteristic) = &mut func.ref_characteristic {
            ref_characteristic
                .identifier_list
                .retain(|ident| objects.contains_key(ident));
        }
        if let Some(def_characteristic) = &mut func.def_characteristic {
            def_characteristic
                .identifier_list
                .retain(|ident| objects.contains_key(ident));
        }
        if let Some(in_measurement) = &mut func.in_measurement {
            in_measurement
                .identifier_list
                .retain(|ident| objects.contains_key(ident));
        }
        if let Some(loc_measurement) = &mut func.loc_measurement {
            loc_measurement
                .identifier_list
                .retain(|ident| objects.contains_key(ident));
        }
        if let Some(out_measurement) = &mut func.out_measurement {
            out_measurement
                .identifier_list
                .retain(|ident| objects.contains_key(ident));
        }
    }
    module.function = function;
}
