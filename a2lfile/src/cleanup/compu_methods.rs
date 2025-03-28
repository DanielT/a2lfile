use std::collections::HashSet;

use crate::specification::Module;

pub(crate) fn cleanup(module: &mut Module) {
    // remove references to non-existent COMPU_METHODs
    remove_invalid_compumethod_refs(module);

    // remove all unused COMPU_METHODs
    remove_unused_compumethods(module);

    // remove all unused COMPU_TABs, COMPU_VTABs, COMPU_VTAB_RANGEs and UNITs
    remove_unused_sub_elements(module);

    // remove references to non-existent COMPU_TABs, COMPU_VTABs, COMPU_VTAB_RANGEs and UNITs
    remove_invalid_sub_element_refs(module);
}

fn remove_invalid_compumethod_refs(module: &mut Module) {
    for axis_pts in module.axis_pts.values_mut() {
        if !module.compu_method.contains_key(&axis_pts.conversion) {
            axis_pts.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for characteristic in module.characteristic.values_mut() {
        for axis_descr in &mut characteristic.axis_descr {
            if !module.compu_method.contains_key(&axis_descr.conversion) {
                axis_descr.conversion = "NO_COMPU_METHOD".to_string();
            }
        }
        if !module.compu_method.contains_key(&characteristic.conversion) {
            characteristic.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for measurement in module.measurement.values_mut() {
        if !module.compu_method.contains_key(&measurement.conversion) {
            measurement.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for typedef_axis in module.typedef_axis.values_mut() {
        if !module.compu_method.contains_key(&typedef_axis.conversion) {
            typedef_axis.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for typedef_characteristic in module.typedef_characteristic.values_mut() {
        if !module
            .compu_method
            .contains_key(&typedef_characteristic.conversion)
        {
            typedef_characteristic.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for typedef_measurement in module.typedef_measurement.values_mut() {
        if !module
            .compu_method
            .contains_key(&typedef_measurement.conversion)
        {
            typedef_measurement.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
}

fn remove_unused_compumethods(module: &mut Module) {
    let mut used_compumethods = HashSet::<String>::new();
    for axis_pts in module.axis_pts.values_mut() {
        used_compumethods.insert(axis_pts.conversion.clone());
    }
    for characteristic in module.characteristic.values_mut() {
        for axis_descr in &characteristic.axis_descr {
            used_compumethods.insert(axis_descr.conversion.clone());
        }
        used_compumethods.insert(characteristic.conversion.clone());
    }
    for measurement in module.measurement.values_mut() {
        used_compumethods.insert(measurement.conversion.clone());
    }
    for typedef_axis in module.typedef_axis.values_mut() {
        used_compumethods.insert(typedef_axis.conversion.clone());
    }
    for typedef_characteristic in module.typedef_characteristic.values_mut() {
        used_compumethods.insert(typedef_characteristic.conversion.clone());
    }
    for typedef_measurement in module.typedef_measurement.values_mut() {
        used_compumethods.insert(typedef_measurement.conversion.clone());
    }
    for compu_method in module.compu_method.values_mut() {
        if let Some(ssr) = compu_method.status_string_ref.as_ref() {
            used_compumethods.insert(ssr.conversion_table.clone());
        }
    }

    module
        .compu_method
        .retain(|_, item| used_compumethods.contains(&item.name));
}

fn remove_unused_sub_elements(module: &mut Module) {
    let mut used_compu_tabs = HashSet::<String>::new();
    let mut used_units = HashSet::<String>::new();

    // remove all unused COMPU_TABs, COMPU_VTABs and COMPU_VTAB_RANGEs
    for compu_method in module.compu_method.values() {
        if let Some(compu_tab_ref) = &compu_method.compu_tab_ref {
            used_compu_tabs.insert(compu_tab_ref.conversion_table.clone());
        }
        if let Some(ref_unit) = &compu_method.ref_unit {
            used_units.insert(ref_unit.unit.clone());
        }
    }

    module
        .compu_tab
        .retain(|_, item| used_compu_tabs.contains(&item.name));
    module
        .compu_vtab
        .retain(|_, item| used_compu_tabs.contains(&item.name));
    module
        .compu_vtab_range
        .retain(|_, item| used_compu_tabs.contains(&item.name));

    // remove all unused UNITs
    for unit in module.unit.values() {
        if let Some(ref_unit) = &unit.ref_unit {
            used_units.insert(ref_unit.unit.clone());
        }
    }

    module
        .unit
        .retain(|_, item| used_units.contains(&item.name));
}

fn remove_invalid_sub_element_refs(module: &mut Module) {
    // build the set of all COMPU_TAB* names
    let existing_compu_tabs = module
        .compu_tab
        .keys()
        .chain(module.compu_vtab.keys())
        .chain(module.compu_vtab_range.keys())
        .cloned()
        .collect::<HashSet<String>>();

    for compu_method in module.compu_method.values_mut() {
        // if a reference to a non-existent COMPU_TAB exists, delete it
        if let Some(compu_tab_ref) = &mut compu_method.compu_tab_ref {
            if !existing_compu_tabs.contains(&compu_tab_ref.conversion_table) {
                compu_method.compu_tab_ref = None;
            }
        }
        // if a reference to a non-existent UNIT exists, delete it
        if let Some(ref_unit) = &mut compu_method.ref_unit {
            if !module.unit.contains_key(&ref_unit.unit) {
                compu_method.ref_unit = None;
            }
        }
    }
}
