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
    let mut existing_compumethods: HashSet<String> = module
        .compu_method
        .iter()
        .map(|item| item.name.clone())
        .collect();
    existing_compumethods.insert("NO_COMPU_METHOD".to_string());
    for axis_pts in &mut module.axis_pts {
        if !existing_compumethods.contains(&axis_pts.conversion) {
            axis_pts.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for characteristic in &mut module.characteristic {
        for axis_descr in &mut characteristic.axis_descr {
            if !existing_compumethods.contains(&axis_descr.conversion) {
                axis_descr.conversion = "NO_COMPU_METHOD".to_string();
            }
        }
        if !existing_compumethods.contains(&characteristic.conversion) {
            characteristic.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for measurement in &mut module.measurement {
        if !existing_compumethods.contains(&measurement.conversion) {
            measurement.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for typedef_axis in &mut module.typedef_axis {
        if !existing_compumethods.contains(&typedef_axis.conversion) {
            typedef_axis.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for typedef_characteristic in &mut module.typedef_characteristic {
        if !existing_compumethods.contains(&typedef_characteristic.conversion) {
            typedef_characteristic.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
    for typedef_measurement in &mut module.typedef_measurement {
        if !existing_compumethods.contains(&typedef_measurement.conversion) {
            typedef_measurement.conversion = "NO_COMPU_METHOD".to_string();
        }
    }
}

fn remove_unused_compumethods(module: &mut Module) {
    let mut used_compumethods = HashSet::<String>::new();
    for axis_pts in &module.axis_pts {
        used_compumethods.insert(axis_pts.conversion.clone());
    }
    for characteristic in &module.characteristic {
        for axis_descr in &characteristic.axis_descr {
            used_compumethods.insert(axis_descr.conversion.clone());
        }
        used_compumethods.insert(characteristic.conversion.clone());
    }
    for measurement in &module.measurement {
        used_compumethods.insert(measurement.conversion.clone());
    }
    for typedef_axis in &module.typedef_axis {
        used_compumethods.insert(typedef_axis.conversion.clone());
    }
    for typedef_characteristic in &module.typedef_characteristic {
        used_compumethods.insert(typedef_characteristic.conversion.clone());
    }
    for typedef_measurement in &module.typedef_measurement {
        used_compumethods.insert(typedef_measurement.conversion.clone());
    }
    for compu_method in &module.compu_method {
        if let Some(ssr) = compu_method.status_string_ref.as_ref() {
            used_compumethods.insert(ssr.conversion_table.clone());
        }
    }

    module
        .compu_method
        .retain(|item| used_compumethods.contains(&item.name));
}

fn remove_unused_sub_elements(module: &mut Module) {
    let mut used_compu_tabs = HashSet::<String>::new();
    let mut used_units = HashSet::<String>::new();

    // remove all unused COMPU_TABs, COMPU_VTABs and COMPU_VTAB_RANGEs
    for compu_method in &module.compu_method {
        if let Some(compu_tab_ref) = &compu_method.compu_tab_ref {
            used_compu_tabs.insert(compu_tab_ref.conversion_table.clone());
        }
        if let Some(ref_unit) = &compu_method.ref_unit {
            used_units.insert(ref_unit.unit.clone());
        }
    }

    module
        .compu_tab
        .retain(|item| used_compu_tabs.contains(&item.name));
    module
        .compu_vtab
        .retain(|item| used_compu_tabs.contains(&item.name));
    module
        .compu_vtab_range
        .retain(|item| used_compu_tabs.contains(&item.name));

    // remove all unused UNITs
    for unit in &module.unit {
        if let Some(ref_unit) = &unit.ref_unit {
            used_units.insert(ref_unit.unit.clone());
        }
    }

    module.unit.retain(|item| used_units.contains(&item.name));
}

fn remove_invalid_sub_element_refs(module: &mut Module) {
    // build the set of all COMPU_TAB* names
    let mut existing_compu_tabs = HashSet::<String>::new();
    for compu_tab in &module.compu_tab {
        existing_compu_tabs.insert(compu_tab.name.clone());
    }
    for compu_vtab in &module.compu_vtab {
        existing_compu_tabs.insert(compu_vtab.name.clone());
    }
    for compu_vtab_range in &module.compu_vtab_range {
        existing_compu_tabs.insert(compu_vtab_range.name.clone());
    }

    // build the set of all UNIT names
    let existing_units: HashSet<String> =
        module.unit.iter().map(|unit| unit.name.clone()).collect();

    for compu_method in &mut module.compu_method {
        // if a reference to a non-existent COMPU_TAB exists, delete it
        if let Some(compu_tab_ref) = &mut compu_method.compu_tab_ref {
            if !existing_compu_tabs.contains(&compu_tab_ref.conversion_table) {
                compu_method.compu_tab_ref = None;
            }
        }
        // if a reference to a non-existent UNIT exists, delete it
        if let Some(ref_unit) = &mut compu_method.ref_unit {
            if !existing_units.contains(&ref_unit.unit) {
                compu_method.ref_unit = None;
            }
        }
    }
}
