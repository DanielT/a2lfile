use std::collections::HashMap;
use std::collections::HashSet;

use crate::specification::*;

pub(crate) fn cleanup(module: &mut Module) {
    // in all groups, remove references to non-existent CHARACTERISTICs, MEASUREMENTs, etc.
    remove_invalid_object_references(module);

    // remove all empty groups
    delete_empty_groups(module);
}

fn remove_invalid_object_references(module: &mut Module) {
    // a set of the names that a group could potentially refer to
    let refnames = build_refname_set(module);

    for grp in &mut module.group {
        if let Some(ref_characteristic) = &mut grp.ref_characteristic {
            // retain only references to existing characteristics
            ref_characteristic
                .identifier_list
                .retain(|item| refnames.get(item).is_some());
            if ref_characteristic.identifier_list.is_empty() {
                grp.ref_characteristic = None;
            }
        }
        if let Some(ref_measurement) = &mut grp.ref_measurement {
            // retain only references to existing measurements
            ref_measurement
                .identifier_list
                .retain(|item| refnames.get(item).is_some());
            if ref_measurement.identifier_list.is_empty() {
                grp.ref_measurement = None;
            }
        }
    }
}

fn build_refname_set(module: &Module) -> HashSet<String> {
    let mut refnames = HashSet::new();
    for item in &module.characteristic {
        refnames.insert(item.get_name().to_owned());
    }
    for item in &module.measurement {
        refnames.insert(item.get_name().to_owned());
    }
    for item in &module.blob {
        refnames.insert(item.get_name().to_owned());
    }
    for item in &module.instance {
        refnames.insert(item.get_name().to_owned());
    }

    refnames
}

fn delete_empty_groups(module: &mut Module) {
    let mut name2idx = HashMap::<String, usize>::new();
    for (idx, grp) in module.group.iter().enumerate() {
        name2idx.insert(grp.name.to_owned(), idx);
    }
    let used_groups = get_used_groups(module);
    let mut user_of = vec![Vec::<usize>::new(); module.group.len()];
    let mut delete_queue: Vec<usize> = vec![];
    for (idx, grp) in module.group.iter_mut().enumerate() {
        // build up a reverse reference list, i.e. for each group, which other groups list it as a sub-group
        if let Some(sub_group) = &grp.sub_group {
            for name in &sub_group.identifier_list {
                if let Some(subidx) = name2idx.get(name) {
                    user_of[*subidx].push(idx);
                }
            }
        }

        // detect which groups are empty
        if used_groups.get(&grp.name).is_none() && is_group_empty(grp) {
            delete_queue.push(idx);
        }
    }
    let mut to_delete = vec![false; module.group.len()];
    while let Some(del_idx) = delete_queue.pop() {
        let name = module.group[del_idx].name.clone();
        to_delete[del_idx] = true;

        // for all groups that have a sub-group reference to this to-be-deleted group
        for refidx in &user_of[del_idx] {
            if let Some(sg) = &mut module.group[*refidx].sub_group {
                // remove the reference to the deleted group from the sub-group list of the referencing group
                sg.identifier_list.retain(|item| *item != name);
                if sg.identifier_list.is_empty() {
                    module.group[*refidx].sub_group = None;
                }
            }
            // if the group referencing the current group became empty after the
            // removal of the reference, then it is also queued for deletion
            if used_groups.get(&module.group[*refidx].name).is_none()
                && is_group_empty(&module.group[*refidx])
            {
                delete_queue.push(*refidx);
            }
        }
    }
    let mut del_iter = to_delete.iter();
    module.group.retain(|_| !del_iter.next().unwrap());
}

fn get_used_groups(module: &Module) -> HashSet<String> {
    let mut used_groups = HashSet::<String>::new();
    for user_rights in &module.user_rights {
        for ref_group in &user_rights.ref_group {
            for groupname in &ref_group.identifier_list {
                used_groups.insert(groupname.to_owned());
            }
        }
    }

    used_groups
}

fn is_group_empty(group: &Group) -> bool {
    let sub_group_empty = if let Some(sg) = &group.sub_group {
        sg.identifier_list.is_empty()
    } else {
        true
    };

    let ref_measurement_empty = if let Some(rm) = &group.ref_measurement {
        rm.identifier_list.is_empty()
    } else {
        true
    };

    let ref_characteristic_empty = if let Some(rc) = &group.ref_characteristic {
        rc.identifier_list.is_empty()
    } else {
        true
    };

    // no need to look at ANNOTATION or FUNCTION_LIST - if there are no characteristics,
    // measurements, or sub groups, then the group is no longer useful

    sub_group_empty && ref_characteristic_empty && ref_measurement_empty
}
