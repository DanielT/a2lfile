use std::cmp::Ordering;
use crate::specification::*;


pub(crate) fn sort_new_items(a2l_file: &mut A2lFile) {
    // if there is a newly inserted ASAP2_VERSION then reorder the top-level elements
    if let Some(asap2version) = &mut a2l_file.asap2_version {
        if asap2version.__block_info.uid == 0 {
            asap2version.__block_info.uid = 1;

            if let Some(a2ml_version) = &mut a2l_file.a2ml_version {
                a2ml_version.__block_info.uid = 2;
            }

            a2l_file.project.__block_info.uid = 3;
        }
    }

    // inside of PROJECT the HEADER is placed as the first element
    if let Some(header) = &mut a2l_file.project.header {
        if header.__block_info.uid == 0 {
            header.__block_info.uid = 1;
            header.__block_info.start_offset = 1;
        }
    }

    /* how it works:
     * The writing code orders the items in the file by their uids. Items with uid = 0 are placed at the end.
     * New items have uid = 0, while merged items are reset to have uid = 0 during merging.
     * To insert new objects at some position, a uid must be assigned.
     * All existing uids of items inside of MODULE are multiplied by 2. This doesn't change the ordering,
     * but now existing uids are all even, and odd uids can be used to insert items.
     * Then the max uid of each type of item is found, and new items of that type are assigned uid = max_uid + 1
     */
    for module in &mut a2l_file.project.module {
        let next_uid = sort_optional_item(&mut module.a2ml, 1);
        let next_uid = sort_optional_item(&mut module.mod_common, next_uid);
        sort_optional_item(&mut module.mod_par, next_uid);

        sort_objectlist_new(&mut module.axis_pts);
        sort_objectlist_new(&mut module.blob);
        sort_objectlist_new(&mut module.characteristic);
        sort_objectlist_new(&mut module.compu_method);
        sort_objectlist_new(&mut module.compu_tab);
        sort_objectlist_new(&mut module.compu_vtab);
        sort_objectlist_new(&mut module.compu_vtab_range);
        sort_objectlist_new(&mut module.frame);
        sort_objectlist_new(&mut module.function);
        sort_objectlist_new(&mut module.group);
        sort_objectlist_new(&mut module.instance);
        sort_objectlist_new(&mut module.measurement);
        sort_objectlist_new(&mut module.record_layout);
        sort_objectlist_new(&mut module.transformer);
        sort_objectlist_new(&mut module.typedef_axis);
        sort_objectlist_new(&mut module.typedef_blob);
        sort_objectlist_new(&mut module.typedef_characteristic);
        sort_objectlist_new(&mut module.typedef_measurement);
        sort_objectlist_new(&mut module.typedef_structure);
        sort_objectlist_new(&mut module.unit);


        if let Some(maxid) = module.if_data.iter().map(|item| item.get_layout().uid).max() {
            if maxid > 0 {
                module.if_data.iter_mut().for_each(|item| {
                    if item.get_layout_mut().uid != 0 {
                        item.get_layout_mut().uid *= 2;
                    } else {
                        item.get_layout_mut().uid = maxid * 2 + 1;
                    }
                })
            }
        }

        if let Some(maxid) = module.user_rights.iter().map(|item| item.get_layout().uid).max() {
            if maxid > 0 {
                module.user_rights.iter_mut().for_each(|item| {
                    if item.get_layout_mut().uid != 0 {
                        item.get_layout_mut().uid *= 2;
                    } else {
                        item.get_layout_mut().uid = maxid * 2 + 1;
                    }
                })
            }
        }

        sort_optional_item(&mut module.variant_coding, 0);
    }
}


fn sort_optional_item<T, U>(a2lobject_option: &mut Option<T>, new_uid: u32) -> u32
where T: A2lObjectLayout<U> {
    let mut next_uid = new_uid;
    if let Some(a2lobject) = a2lobject_option {
        let layout = a2lobject.get_layout_mut();
        if layout.uid == 0 {
            layout.uid = new_uid;
        } else {
            layout.uid *= 2;
            next_uid = layout.uid + 1;
        }
    }
    next_uid
}


fn sort_objectlist_new<T, U>(a2lobject_list: &mut Vec<T>)
where T: A2lObjectLayout<U> + A2lObjectName {
    a2lobject_list.sort_by(cmp_named_a2lobject);
    let mut last_uid = 0;
    for a2lobject in a2lobject_list {
        let layout = a2lobject.get_layout_mut();
        if layout.uid != 0 {
            layout.uid *= 2;
            last_uid = layout.uid + 1;
        } else {
            layout.uid = last_uid;
            layout.start_offset = 2;
            layout.end_offset = 1;
        }
    }
}


pub(crate) fn sort(a2l_file: &mut A2lFile) {
    // top level elements
    // ASAP2_VERSION
    if let Some(asap2_version) = &mut a2l_file.asap2_version {
        asap2_version.__block_info.uid = 1;
        asap2_version.__block_info.start_offset = 1;
    }

    // A2ML_VERSION
    if let Some(a2ml_version) = &mut a2l_file.a2ml_version {
        a2ml_version.__block_info.uid = 2;
        a2ml_version.__block_info.start_offset = 2;
    }

    // PROJECT
    a2l_file.project.__block_info.uid = 3;
    a2l_file.project.__block_info.start_offset = 2;
    a2l_file.project.__block_info.end_offset = 1;

    // PROJECT.HEADER
    if let Some(header) = &mut a2l_file.project.header {
        header.__block_info.uid = 1;
        header.__block_info.start_offset = 1;
    }

    // PROJECT.MODULE
    sort_objectlist_full(&mut a2l_file.project.module, 2);

    // PROJECT.MODULE.*
    for module in &mut a2l_file.project.module {
        // a2ml must come first in order to allow following IF_DATA to be parsed
        if let Some(a2ml) = &mut module.a2ml {
            a2ml.__block_info.uid = 1;
            a2ml.__block_info.start_offset = 1;
        }

        // it also makes sense to put module information at the top
        if let Some(mod_common) = &mut module.mod_common {
            mod_common.__block_info.uid = 2;
            mod_common.__block_info.start_offset = 2;
        }

        if let Some(mod_par) = &mut module.mod_par {
            mod_par.__block_info.uid = 3;
            mod_par.__block_info.start_offset = 2;
        }

        let mut uid = 4;
        for if_data in &mut module.if_data {
            if_data.__block_info.uid = uid;
            uid += 1;
            if_data.__block_info.start_offset = 2;
        }

        // everything else could be in any order, this ordering goes by (subjective) importance
        // measurement / calibration objects
        uid = sort_objectlist_full(&mut module.characteristic, uid);
        uid = sort_objectlist_full(&mut module.measurement, uid);
        uid = sort_objectlist_full(&mut module.axis_pts, uid);
        uid = sort_objectlist_full(&mut module.instance, uid);
        uid = sort_objectlist_full(&mut module.blob, uid);

        // conversions
        uid = sort_objectlist_full(&mut module.compu_method, uid);
        uid = sort_objectlist_full(&mut module.compu_tab, uid);
        uid = sort_objectlist_full(&mut module.compu_vtab, uid);
        uid = sort_objectlist_full(&mut module.compu_vtab_range, uid);

        // data type typedefs
        uid = sort_objectlist_full(&mut module.typedef_structure, uid);
        uid = sort_objectlist_full(&mut module.typedef_characteristic, uid);
        uid = sort_objectlist_full(&mut module.typedef_measurement, uid);
        uid = sort_objectlist_full(&mut module.typedef_axis, uid);
        uid = sort_objectlist_full(&mut module.typedef_blob, uid);

        // everything else
        uid = sort_objectlist_full(&mut module.frame, uid);
        uid = sort_objectlist_full(&mut module.function, uid);
        uid = sort_objectlist_full(&mut module.group, uid);
        uid = sort_objectlist_full(&mut module.record_layout, uid);
        uid = sort_objectlist_full(&mut module.transformer, uid);
        uid = sort_objectlist_full(&mut module.unit, uid);

        module.user_rights.sort_by(|a, b| a.user_level_id.cmp(&b.user_level_id));
        for user_rights in &mut module.user_rights {
            user_rights.__block_info.uid = uid;
            uid += 1;
            user_rights.__block_info.start_offset = 2;
        }

        if let Some(variant_coding) = &mut module.variant_coding {
            variant_coding.__block_info.uid = uid;
            variant_coding.__block_info.start_offset = 2;
        }
    }
}



fn sort_objectlist_full<T, U>(a2lobject_list: &mut Vec<T>, start_uid: u32) -> u32
where T: A2lObjectLayout<U> + A2lObjectName {
    let mut current_uid = start_uid;
    a2lobject_list.sort_by(|a, b| a.get_name().cmp(b.get_name()));
    for a2lobject in a2lobject_list {
        a2lobject.get_layout_mut().uid = current_uid;
        a2lobject.get_layout_mut().start_offset = 2;
        a2lobject.get_layout_mut().end_offset = 1;
        current_uid += 1;
    }
    current_uid
}


fn cmp_named_a2lobject<T, U>(a: &T, b: &T) -> Ordering
where T: A2lObjectLayout<U> + A2lObjectName {
    let la = a.get_layout();
    let lb = b.get_layout();

    if la.uid == 0 && lb.uid != 0 {
        Ordering::Greater
    } else if lb.uid == 0 && la.uid != 0 {
        Ordering::Less
    } else if la.uid == lb.uid {
        // probably both uids are zero
        if la.line == lb.line {
            // both uid and line are equal - newly created elements
            a.get_name().cmp(b.get_name())
        } else {
            la.line.cmp(&lb.line)
        }
    } else {
        la.uid.cmp(&lb.uid)
    }
}
