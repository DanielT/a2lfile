use crate::specification::{A2lObject, Module};
use crate::{A2lObjectName, ItemList};
use std::collections::HashMap;
use std::collections::HashSet;

/// Merges the contents of `merge_module` into `orig_module`.
/// The output contains all items from both modules, renaming items from `merge_module` as needed to avoid name conflicts.
/// Identical items are not duplicated.
pub(crate) fn merge_modules(orig_module: &mut Module, merge_module: &mut Module) {
    // merge A2ML - no dependencies
    merge_a2ml(orig_module, merge_module);

    // merge MOD_PAR and MEMORY_LAYOUT and MEMORY_SEGMENT inside it do not depend on anything else and can be merged first
    merge_mod_par(orig_module, merge_module);

    // merge IF_DATA - no dependencies
    merge_if_data(orig_module, merge_module);

    // merge UNIT - no dependencies
    merge_unit(orig_module, merge_module);

    // merge COMPU_TAB / COMPU_VTAB / COMPU_VTAB_RANGE - no dependencies
    merge_compu_tab(orig_module, merge_module);

    // merge COMPU_METHOD - depends on COMPU_TAB and UNIT
    merge_compu_method(orig_module, merge_module);

    // merge RECORD_LAYOUT - no dependencies
    merge_record_layout(orig_module, merge_module);

    // merge MOD_COMMON - depends on RECORD_LAYOUT
    merge_mod_common(orig_module, merge_module);

    // merge AXIS_PTS, CHARACTERISTIC, MEASUREMENT, INSTANCE, BLOB, TYPEDEF_*
    // depends on each other, as well as COMPU_METHOD, TYPEDEF_* and MOD_COMMON.MEMORY_SEGMENT
    merge_objects(orig_module, merge_module);

    // merge FUNCTION - depends on AXIS_PTS, CHARACTERISTIC, MEASUREMENT, INSTANCE, BLOB
    merge_function(orig_module, merge_module);

    // merge GROUP - depends on FUNCTION, CHARACTERISTIC and MEASUREMENT
    merge_group(orig_module, merge_module);

    // merge FRAME - depends on MEASUREMENT
    merge_frame(orig_module, merge_module);

    // merge TRANSFORMER - no dependencies
    merge_transformer(orig_module, merge_module);

    // merge USER_RIGHTS - depends on GROUP
    merge_user_rights(orig_module, merge_module);

    // merge VARIANT_CODING - depends on MEASUREMENT and CHARACTERISTIC
    merge_variant_coding(orig_module, merge_module);
}

// ------------------------ MOD_COMMON ------------------------

fn merge_a2ml(orig_module: &mut Module, merge_module: &mut Module) {
    if merge_module.a2ml.is_some()
        && orig_module.a2ml.is_none()
        && let Some(mut a2ml) = std::mem::take(&mut merge_module.a2ml)
    {
        a2ml.reset_location();
        orig_module.a2ml = Some(a2ml);
    }
}

// ------------------------ MOD_PAR ------------------------

fn merge_mod_par(orig_module: &mut Module, merge_module: &mut Module) {
    if merge_module.mod_par.is_some() {
        if orig_module.mod_par.is_some() {
            // MOD_PAR exists on both sides. In this case, only merge the MEMORY_LAYOUT and MEMORY_SEGMENT elements
            merge_memory_layout(orig_module, merge_module);
            merge_memory_segment(orig_module, merge_module);
            merge_system_constant(orig_module, merge_module);
        } else {
            // no MOD_PAR in the destination: move it over completely
            orig_module.mod_par = std::mem::take(&mut merge_module.mod_par);
            orig_module.mod_par.as_mut().unwrap().reset_location();
        }
    }
}

// ------------------------ MEMORY_LAYOUT ------------------------

fn merge_memory_layout(orig_module: &mut Module, merge_module: &mut Module) {
    let orig_memory_layout = &mut orig_module.mod_par.as_mut().unwrap().memory_layout;
    let merge_memory_layout =
        std::mem::take(&mut merge_module.mod_par.as_mut().unwrap().memory_layout);

    for mut merge_ml in merge_memory_layout {
        let mut is_equal = false;
        // compare with every existing MEMORY_LAYOUT, and only take new ones
        for orig_ml in orig_memory_layout.iter() {
            if merge_ml == *orig_ml {
                is_equal = true;
                break;
            }
        }
        if !is_equal {
            merge_ml.reset_location();
            orig_memory_layout.push(merge_ml);
        }
    }
}

// ------------------------ MEMORY_SEGMENT ------------------------

fn merge_memory_segment(orig_module: &mut Module, merge_module: &mut Module) {
    let orig_mod_par = orig_module.mod_par.as_ref().unwrap();
    let merge_mod_par = merge_module.mod_par.as_ref().unwrap();
    let (merge_action, mut rename_table) =
        calculate_item_actions(&orig_mod_par.memory_segment, &merge_mod_par.memory_segment);

    rename_memory_segment_refs(merge_module, &rename_table);

    let orig_mod_par = orig_module.mod_par.as_mut().unwrap();
    let merge_mod_par = merge_module.mod_par.as_mut().unwrap();
    let memory_segment_list = std::mem::take(&mut merge_mod_par.memory_segment);
    for mut memory_segment in memory_segment_list {
        if let Some(true) = merge_action.get(&memory_segment.name) {
            memory_segment.reset_location();
            if let Some(newname) = rename_table.remove(&memory_segment.name) {
                memory_segment.name = newname.clone();
            }
            orig_mod_par.memory_segment.push(memory_segment);
        }
    }
}

fn rename_memory_segment_refs(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for axis_pts in &mut merge_module.axis_pts {
        if let Some(ref_memory_segment) = &mut axis_pts.ref_memory_segment
            && let Some(newname) = rename_table.get(&ref_memory_segment.name)
        {
            ref_memory_segment.name = newname.to_owned();
        }
    }

    for characteristic in &mut merge_module.characteristic {
        if let Some(ref_memory_segment) = &mut characteristic.ref_memory_segment
            && let Some(newname) = rename_table.get(&ref_memory_segment.name)
        {
            ref_memory_segment.name = newname.to_owned();
        }
    }

    for measurement in &mut merge_module.measurement {
        if let Some(ref_memory_segment) = &mut measurement.ref_memory_segment
            && let Some(newname) = rename_table.get(&ref_memory_segment.name)
        {
            ref_memory_segment.name = newname.to_owned();
        }
    }
}

// ------------------------ SYSTEM_CONSTANT ------------------------

fn merge_system_constant(orig_module: &mut Module, merge_module: &mut Module) {
    let orig_system_constant = &mut orig_module.mod_par.as_mut().unwrap().system_constant;
    let merge_system_constant =
        std::mem::take(&mut merge_module.mod_par.as_mut().unwrap().system_constant);

    let orig_sc_names: HashSet<String> = orig_system_constant
        .iter()
        .map(|sc| sc.name.clone())
        .collect();

    for mut merge_sysconst in merge_system_constant {
        if !orig_sc_names.contains(&merge_sysconst.name) {
            merge_sysconst.reset_location();
            orig_system_constant.push(merge_sysconst);
        }
    }
}

// ------------------------ IF_DATA ------------------------

fn merge_if_data(orig_module: &mut Module, merge_module: &mut Module) {
    // merging the IF_DATA in a way that keeps as much info as possible from both sides really makes no sense at all
    // this could result in files that two conflicting XCP connection definitions, or which have
    // both XCP as well as ASAP1B_CCP definitions.
    // For this reason, IF_DATA will ony be merged if there is no IF_DATA at all
    if orig_module.if_data.is_empty() {
        let mut if_data = std::mem::take(&mut merge_module.if_data);
        for item in &mut if_data {
            item.reset_location();
        }
        orig_module.if_data = if_data;
    }
}

// ------------------------ UNIT ------------------------

fn merge_unit(orig_module: &mut Module, merge_module: &mut Module) {
    let (merge_action, mut rename_table) =
        calculate_item_actions(&orig_module.unit, &merge_module.unit);

    rename_unit_refs(merge_module, &rename_table);

    let merge_unit_list = std::mem::take(&mut merge_module.unit);
    for mut unit in merge_unit_list {
        if let Some(true) = merge_action.get(&unit.name) {
            unit.reset_location();
            if let Some(newname) = rename_table.remove(&unit.name) {
                unit.name = newname.clone();
            }
            orig_module.unit.push(unit);
        }
    }
}

fn rename_unit_refs(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for compu_method in &mut merge_module.compu_method {
        if let Some(ref_unit) = &mut compu_method.ref_unit
            && let Some(newname) = rename_table.get(&ref_unit.unit)
        {
            ref_unit.unit = newname.to_owned();
        }
    }
}

// ------------------------ COMPU_TAB / COMPU_VTAB / COMPU_VTAB_RANGE ------------------------

fn merge_compu_tab(orig_module: &mut Module, merge_module: &mut Module) {
    let orig_compu_tab = orig_module.compu_tabs();
    let merge_compu_tab = merge_module.compu_tabs();
    let (merge_action, mut rename_table) =
        calculate_item_actions(&orig_compu_tab, &merge_compu_tab);

    rename_compu_tabs(merge_module, &rename_table);

    let merge_compu_tab_list = std::mem::take(&mut merge_module.compu_tab);
    for mut compu_tab in merge_compu_tab_list {
        if let Some(true) = merge_action.get(&compu_tab.name) {
            compu_tab.reset_location();
            if let Some(newname) = rename_table.remove(&compu_tab.name) {
                compu_tab.name = newname.clone();
            }
            orig_module.compu_tab.push(compu_tab);
        }
    }
    let merge_compu_vtab_list = std::mem::take(&mut merge_module.compu_vtab);
    for mut compu_vtab in merge_compu_vtab_list {
        if let Some(true) = merge_action.get(&compu_vtab.name) {
            compu_vtab.reset_location();
            if let Some(newname) = rename_table.remove(&compu_vtab.name) {
                compu_vtab.name = newname.clone();
            }
            orig_module.compu_vtab.push(compu_vtab);
        }
    }
    let merge_compu_vtab_range_list = std::mem::take(&mut merge_module.compu_vtab_range);
    for mut compu_vtab_range in merge_compu_vtab_range_list {
        if let Some(true) = merge_action.get(&compu_vtab_range.name) {
            compu_vtab_range.reset_location();
            if let Some(newname) = rename_table.remove(&compu_vtab_range.name) {
                compu_vtab_range.name = newname.clone();
            }
            orig_module.compu_vtab_range.push(compu_vtab_range);
        }
    }
}

fn rename_compu_tabs(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    // COMPU_METHODs can refer to any of COMPU_TAB / COMPU_VTAB / COMPU_VTAB_RANGE via a COMPU_TAB_REF
    for compu_method in &mut merge_module.compu_method {
        if let Some(compu_tab_ref) = &mut compu_method.compu_tab_ref
            && let Some(newname) = rename_table.get(&compu_tab_ref.conversion_table)
        {
            compu_tab_ref.conversion_table = newname.to_owned();
        }
        if let Some(status_string_ref) = &mut compu_method.status_string_ref
            && let Some(newname) = rename_table.get(&status_string_ref.conversion_table)
        {
            status_string_ref.conversion_table = newname.to_owned();
        }
    }
}

// ------------------------ COMPU_METHOD ------------------------

fn merge_compu_method(orig_module: &mut Module, merge_module: &mut Module) {
    let (merge_action, mut rename_table) =
        calculate_item_actions(&orig_module.compu_method, &merge_module.compu_method);

    rename_compu_method_refs(merge_module, &rename_table);

    let merge_compu_method_list = std::mem::take(&mut merge_module.compu_method);
    for mut compu_method in merge_compu_method_list {
        if let Some(true) = merge_action.get(&compu_method.name) {
            compu_method.reset_location();
            if let Some(newname) = rename_table.remove(&compu_method.name) {
                compu_method.name = newname.clone();
            }
            orig_module.compu_method.push(compu_method);
        }
    }
}

fn rename_compu_method_refs(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for axis_pts in &mut merge_module.axis_pts {
        if let Some(newname) = rename_table.get(&axis_pts.conversion) {
            axis_pts.conversion = newname.to_owned();
        }
    }

    for characteristic in &mut merge_module.characteristic {
        if let Some(newname) = rename_table.get(&characteristic.conversion) {
            characteristic.conversion = newname.to_owned();
        }
        for axis_descr in &mut characteristic.axis_descr {
            if let Some(newname) = rename_table.get(&axis_descr.conversion) {
                axis_descr.conversion = newname.to_owned();
            }
        }
    }

    for measurement in &mut merge_module.measurement {
        if let Some(newname) = rename_table.get(&measurement.conversion) {
            measurement.conversion = newname.to_owned();
        }
    }

    for typedef_axis in &mut merge_module.typedef_axis {
        if let Some(newname) = rename_table.get(&typedef_axis.conversion) {
            typedef_axis.conversion = newname.to_owned();
        }
    }

    for typedef_characteristic in &mut merge_module.typedef_characteristic {
        if let Some(newname) = rename_table.get(&typedef_characteristic.conversion) {
            typedef_characteristic.conversion = newname.to_owned();
        }
        for axis_descr in &mut typedef_characteristic.axis_descr {
            if let Some(newname) = rename_table.get(&axis_descr.conversion) {
                axis_descr.conversion = newname.to_owned();
            }
        }
    }

    for typedef_measurement in &mut merge_module.typedef_measurement {
        if let Some(newname) = rename_table.get(&typedef_measurement.conversion) {
            typedef_measurement.conversion = newname.to_owned();
        }
    }
}

// ------------------------ RECORD_LAYOUT ------------------------

fn merge_record_layout(orig_module: &mut Module, merge_module: &mut Module) {
    let (merge_action, mut rename_table) =
        calculate_item_actions(&orig_module.record_layout, &merge_module.record_layout);

    rename_record_layouts(merge_module, &rename_table);

    let merge_record_layout_list = std::mem::take(&mut merge_module.record_layout);
    for mut record_layout in merge_record_layout_list {
        if let Some(true) = merge_action.get(&record_layout.name) {
            record_layout.reset_location();
            if let Some(newname) = rename_table.remove(&record_layout.name) {
                record_layout.name = newname.clone();
            }
            orig_module.record_layout.push(record_layout);
        }
    }
}

fn rename_record_layouts(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for axis_pts in &mut merge_module.axis_pts {
        if let Some(newname) = rename_table.get(&axis_pts.deposit_record) {
            axis_pts.deposit_record = newname.to_owned();
        }
    }

    for characteristic in &mut merge_module.characteristic {
        if let Some(newname) = rename_table.get(&characteristic.deposit) {
            characteristic.deposit = newname.to_owned();
        }
    }

    for typedef_axis in &mut merge_module.typedef_axis {
        if let Some(newname) = rename_table.get(&typedef_axis.record_layout) {
            typedef_axis.record_layout = newname.to_owned();
        }
    }

    for typedef_characteristic in &mut merge_module.typedef_characteristic {
        if let Some(newname) = rename_table.get(&typedef_characteristic.record_layout) {
            typedef_characteristic.record_layout = newname.to_owned();
        }
    }

    if let Some(mod_common) = &mut merge_module.mod_common
        && let Some(s_rec_layout) = &mut mod_common.s_rec_layout
        && let Some(newname) = rename_table.get(&s_rec_layout.name)
    {
        s_rec_layout.name = newname.to_owned();
    }
}

// ------------------------ MOD_COMMON ------------------------

fn merge_mod_common(orig_module: &mut Module, merge_module: &mut Module) {
    if merge_module.mod_common.is_some()
        && orig_module.mod_common.is_none()
        && let Some(mut mod_common) = std::mem::take(&mut merge_module.mod_common)
    {
        mod_common.reset_location();
        orig_module.mod_common = Some(mod_common);
    }
}

// ----------- AXIS_PTS, CHARACTERISTIC, MEASUREMENT, INSTANCE, BLOB, TYPEDEF_* -----------

fn merge_objects(orig_module: &mut Module, merge_module: &mut Module) {
    // objects and typedefs depend on each other.
    // Specifically, the INSTANCE object may reference any TYPEDEF_*, while TYPDEFE_CHARACTERISTIC may reference any MEASUREMENT
    // As a result, all renaming needs to be done first, and then the items can be merged.
    let orig_objects = orig_module.objects();
    let merge_objects = merge_module.objects();
    let (object_merge_action, mut object_rename_table) =
        calculate_item_actions(&orig_objects, &merge_objects);

    rename_objects(merge_module, &object_rename_table);

    let orig_typedefs = orig_module.typedefs();
    let merge_typedefs = merge_module.typedefs();
    let (typedef_merge_action, mut typedef_rename_table) =
        calculate_item_actions(&orig_typedefs, &merge_typedefs);

    rename_typedef_refs(merge_module, &typedef_rename_table);

    // merge all objects
    let merge_axis_pts_list = std::mem::take(&mut merge_module.axis_pts);
    for mut axis_pts in merge_axis_pts_list {
        if let Some(true) = object_merge_action.get(&axis_pts.name) {
            axis_pts.reset_location();
            if let Some(newname) = object_rename_table.remove(&axis_pts.name) {
                axis_pts.name = newname.clone();
            }
            orig_module.axis_pts.push(axis_pts);
        }
    }
    let merge_blob_list = std::mem::take(&mut merge_module.blob);
    for mut blob in merge_blob_list {
        if let Some(true) = object_merge_action.get(&blob.name) {
            blob.reset_location();
            if let Some(newname) = object_rename_table.remove(&blob.name) {
                blob.name = newname.clone();
            }
            orig_module.blob.push(blob);
        }
    }
    let merge_characteristic_list = std::mem::take(&mut merge_module.characteristic);
    for mut characteristic in merge_characteristic_list {
        if let Some(true) = object_merge_action.get(&characteristic.name) {
            characteristic.reset_location();
            if let Some(newname) = object_rename_table.remove(&characteristic.name) {
                characteristic.name = newname.clone();
            }
            orig_module.characteristic.push(characteristic);
        }
    }
    let merge_instance_list = std::mem::take(&mut merge_module.instance);
    for mut instance in merge_instance_list {
        if let Some(true) = object_merge_action.get(&instance.name) {
            instance.reset_location();
            if let Some(newname) = object_rename_table.remove(&instance.name) {
                instance.name = newname.clone();
            }
            orig_module.instance.push(instance);
        }
    }
    let merge_measurement_list = std::mem::take(&mut merge_module.measurement);
    for mut measurement in merge_measurement_list {
        if let Some(true) = object_merge_action.get(&measurement.name) {
            measurement.reset_location();
            if let Some(newname) = object_rename_table.remove(&measurement.name) {
                measurement.name = newname.clone();
            }
            orig_module.measurement.push(measurement);
        }
    }

    // merge all TYPEDEF_*
    let merge_typedef_axis_list = std::mem::take(&mut merge_module.typedef_axis);
    for mut typedef_axis in merge_typedef_axis_list {
        if let Some(true) = typedef_merge_action.get(&typedef_axis.name) {
            typedef_axis.reset_location();
            if let Some(newname) = typedef_rename_table.remove(&typedef_axis.name) {
                typedef_axis.name = newname.clone();
            }
            orig_module.typedef_axis.push(typedef_axis);
        }
    }
    let merge_typedef_blob_list = std::mem::take(&mut merge_module.typedef_blob);
    for mut typedef_blob in merge_typedef_blob_list {
        if let Some(true) = typedef_merge_action.get(&typedef_blob.name) {
            typedef_blob.reset_location();
            if let Some(newname) = typedef_rename_table.remove(&typedef_blob.name) {
                typedef_blob.name = newname.clone();
            }
            orig_module.typedef_blob.push(typedef_blob);
        }
    }
    let merge_typedef_characteristic_list =
        std::mem::take(&mut merge_module.typedef_characteristic);
    for mut typedef_characteristic in merge_typedef_characteristic_list {
        if let Some(true) = typedef_merge_action.get(&typedef_characteristic.name) {
            typedef_characteristic.reset_location();
            if let Some(newname) = typedef_rename_table.remove(&typedef_characteristic.name) {
                typedef_characteristic.name = newname.clone();
            }
            orig_module
                .typedef_characteristic
                .push(typedef_characteristic);
        }
    }
    let merge_typedef_measurement_list = std::mem::take(&mut merge_module.typedef_measurement);
    for mut typedef_measurement in merge_typedef_measurement_list {
        if let Some(true) = typedef_merge_action.get(&typedef_measurement.name) {
            typedef_measurement.reset_location();
            if let Some(newname) = typedef_rename_table.remove(&typedef_measurement.name) {
                typedef_measurement.name = newname.clone();
            }
            orig_module.typedef_measurement.push(typedef_measurement);
        }
    }
    let merge_typedef_struct_list = std::mem::take(&mut merge_module.typedef_structure);
    for mut typedef_structure in merge_typedef_struct_list {
        if let Some(true) = typedef_merge_action.get(&typedef_structure.name) {
            typedef_structure.reset_location();
            if let Some(newname) = typedef_rename_table.remove(&typedef_structure.name) {
                typedef_structure.name = newname.clone();
            }
            orig_module.typedef_structure.push(typedef_structure);
        }
    }
}

fn rename_objects(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    // MODULE.AXIS_PTS
    for axis_pts in &mut merge_module.axis_pts {
        // MODULE.AXIS_PTS.input_quantity
        if let Some(newname) = rename_table.get(&axis_pts.input_quantity) {
            axis_pts.input_quantity = newname.to_owned();
        }
    }
    // MODULE.CHARACTERISTIC
    for characteristic in &mut merge_module.characteristic {
        // MODULE.CHARACTERISTIC.AXIS_DESCR
        for axis_descr in &mut characteristic.axis_descr {
            // MODULE.CHARACTERISTIC.AXIS_DESCR.input_quantity
            if let Some(newname) = rename_table.get(&axis_descr.input_quantity) {
                axis_descr.input_quantity = newname.to_owned();
            }
            // MODULE.CHARACTERISTIC.AXIS_DESCR.AXIS_PTS_REF
            if let Some(axis_pts_ref) = &mut axis_descr.axis_pts_ref
                && let Some(newname) = rename_table.get(&axis_pts_ref.axis_points)
            {
                axis_pts_ref.axis_points = newname.to_owned();
            }
            // MODULE.CHARACTERISTIC.AXIS_DESCR.CURVE_AXIS_REF
            if let Some(curve_axis_ref) = &mut axis_descr.curve_axis_ref
                && let Some(newname) = rename_table.get(&curve_axis_ref.curve_axis)
            {
                curve_axis_ref.curve_axis = newname.to_owned();
            }
        }
        // MODULE.CHARACTERISTIC.DEPENDENT_CHARACTERISTIC
        if let Some(dependent_characteristic) = &mut characteristic.dependent_characteristic {
            rename_item_list(
                &mut dependent_characteristic.characteristic_list,
                rename_table,
            );
        }
    }
    // MODULE.TYPEDEF_CHARACTERISTIC
    for typedef_characteristic in &mut merge_module.typedef_characteristic {
        // MODULE.TYPEDEF_CHARACTERISTIC.AXIS_DESCR
        for axis_descr in &mut typedef_characteristic.axis_descr {
            // MODULE.TYPEDEF_CHARACTERISTIC.AXIS_DESCR.input_quantity
            if let Some(newname) = rename_table.get(&axis_descr.input_quantity) {
                axis_descr.input_quantity = newname.to_owned();
            }
            // MODULE.TYPEDEF_CHARACTERISTIC.AXIS_DESCR.AXIS_PTS_REF
            if let Some(axis_pts_ref) = &mut axis_descr.axis_pts_ref
                && let Some(newname) = rename_table.get(&axis_pts_ref.axis_points)
            {
                axis_pts_ref.axis_points = newname.to_owned();
            }
            // MODULE.TYPEDEF_CHARACTERISTIC.AXIS_DESCR.CURVE_AXIS_REF
            if let Some(curve_axis_ref) = &mut axis_descr.curve_axis_ref
                && let Some(newname) = rename_table.get(&curve_axis_ref.curve_axis)
            {
                curve_axis_ref.curve_axis = newname.to_owned();
            }
        }
    }

    // MODULE.FRAME
    for frame in &mut merge_module.frame {
        // MODULE.FRAME.FRAME_MEASUREMENT
        if let Some(frame_measurement) = &mut frame.frame_measurement {
            rename_item_list(&mut frame_measurement.identifier_list, rename_table);
        }
    }

    // MODULE.FUNCTION
    for function in &mut merge_module.function {
        // MODULE.FUNCTION.IN_MEASUREMENT
        if let Some(in_measurement) = &mut function.in_measurement {
            rename_item_list(&mut in_measurement.identifier_list, rename_table);
        }
        // MODULE.FUNCTION.LOC_MEASUREMENT
        if let Some(loc_measurement) = &mut function.loc_measurement {
            rename_item_list(&mut loc_measurement.identifier_list, rename_table);
        }
        // MODULE.FUNCTION.OUT_MEASUREMENT
        if let Some(out_measurement) = &mut function.out_measurement {
            rename_item_list(&mut out_measurement.identifier_list, rename_table);
        }
        // MODULE.FUNCTION.DEF_CHARACTERISTIC
        if let Some(def_characteristic) = &mut function.def_characteristic {
            rename_item_list(&mut def_characteristic.identifier_list, rename_table);
        }
        // MODULE.FUNCTION.REF_CHARACTERISTIC
        if let Some(ref_characteristic) = &mut function.ref_characteristic {
            rename_item_list(&mut ref_characteristic.identifier_list, rename_table);
        }
    }

    // MODULE.GROUP
    for group in &mut merge_module.group {
        // MODULE.GROUP.REF_CHARACTERISTIC
        if let Some(ref_characteristic) = &mut group.ref_characteristic {
            rename_item_list(&mut ref_characteristic.identifier_list, rename_table);
        }
        // MODULE.GROUP.REF_MEASUREMENT
        if let Some(ref_measurement) = &mut group.ref_measurement {
            rename_item_list(&mut ref_measurement.identifier_list, rename_table);
        }
    }

    // MODULE.TRANSFORMER
    for transformer in &mut merge_module.transformer {
        // MODULE.TRANSFORMER.TRANSFORMER_IN_OBJECTS
        if let Some(transformer_in_objects) = &mut transformer.transformer_in_objects {
            rename_item_list(&mut transformer_in_objects.identifier_list, rename_table);
        }
        // MODULE.TRANSFORMER.TRANSFORMER_OUT_OBJECTS
        if let Some(transformer_out_objects) = &mut transformer.transformer_out_objects {
            rename_item_list(&mut transformer_out_objects.identifier_list, rename_table);
        }
    }

    // MODULE.VARIANT_CODING
    if let Some(variant_coding) = &mut merge_module.variant_coding {
        // MODULE.VARIANT_CODING.VAR_CRITERION
        for var_criterion in &mut variant_coding.var_criterion {
            // MODULE.VARIANT_CODING.VAR_CRITERION.VAR_MEASUREMENT
            if let Some(var_measurement) = &mut var_criterion.var_measurement
                && let Some(newname) = rename_table.get(&var_measurement.name)
            {
                var_measurement.name = newname.to_owned();
            }
            // MODULE.VARIANT_CODING.VAR_CRITERION.VAR_SELECTION
            if let Some(var_selection_characteristic) =
                &mut var_criterion.var_selection_characteristic
                && let Some(newname) = rename_table.get(&var_selection_characteristic.name)
            {
                var_selection_characteristic.name = newname.to_owned();
            }
        }
        // MODULE.VARIANT_CODING.VAR_CHARACTERISTIC
        for var_characteristic in &mut variant_coding.var_characteristic {
            rename_item_list(&mut var_characteristic.criterion_name_list, rename_table);
        }
    }
}

// ------------------------ FUNCTION ------------------------
fn merge_function(orig_module: &mut Module, merge_module: &mut Module) {
    // special handling for FUNCTIONs: merge these based only on the name, not the content
    let merge_function_list = std::mem::take(&mut merge_module.function);
    for mut function in merge_function_list {
        if let Some(orig_function) = orig_module.function.get_mut(&function.name) {
            // a function with this name already exists in the original module
            //let orig_function = &mut orig_module.function[*idx];
            if *orig_function == function {
                // the function in the original module is identical to the one in the merge module
                // so we can just skip this one
                continue;
            } else {
                // merge the sub functions
                match (&mut orig_function.sub_function, function.sub_function) {
                    (Some(orig_sub_function), Some(merge_sub_function)) => {
                        // merge the sub_functions
                        for item in merge_sub_function.identifier_list {
                            if !orig_sub_function.identifier_list.contains(&item) {
                                orig_sub_function.identifier_list.push(item);
                            }
                        }
                    }
                    (None, Some(merge_sub_function)) => {
                        // the original function has no sub_function, but the merge function has one
                        orig_function.sub_function = Some(merge_sub_function);
                    }
                    _ => {}
                }

                // merge the IN_MEASUREMENT
                match (&mut orig_function.in_measurement, function.in_measurement) {
                    (Some(orig_in_measurement), Some(merge_in_measurement)) => {
                        // merge the sub_functions
                        for item in merge_in_measurement.identifier_list {
                            if !orig_in_measurement.identifier_list.contains(&item) {
                                orig_in_measurement.identifier_list.push(item);
                            }
                        }
                    }
                    (None, Some(merge_in_measurement)) => {
                        // the original function has no in_measurement, but the merge function has one
                        orig_function.in_measurement = Some(merge_in_measurement);
                    }
                    _ => {}
                }

                // merge the LOC_MEASUREMENT
                match (&mut orig_function.loc_measurement, function.loc_measurement) {
                    (Some(orig_loc_measurement), Some(merge_loc_measurement)) => {
                        // merge the sub_functions
                        for item in merge_loc_measurement.identifier_list {
                            if !orig_loc_measurement.identifier_list.contains(&item) {
                                orig_loc_measurement.identifier_list.push(item);
                            }
                        }
                    }
                    (None, Some(merge_loc_measurement)) => {
                        // the original function has no loc_measurement, but the merge function has one
                        orig_function.loc_measurement = Some(merge_loc_measurement);
                    }
                    _ => {}
                }

                // merge the OUT_MEASUREMENT
                match (&mut orig_function.out_measurement, function.out_measurement) {
                    (Some(orig_out_measurement), Some(merge_out_measurement)) => {
                        // merge the sub_functions
                        for item in merge_out_measurement.identifier_list {
                            if !orig_out_measurement.identifier_list.contains(&item) {
                                orig_out_measurement.identifier_list.push(item);
                            }
                        }
                    }
                    (None, Some(merge_out_measurement)) => {
                        // the original function has no out_measurement, but the merge function has one
                        orig_function.out_measurement = Some(merge_out_measurement);
                    }
                    _ => {}
                }
            }
        } else {
            // no function with this name exists in the original module
            function.reset_location();
            orig_module.function.push(function);
        }
    }
}

// ------------------------ GROUP ------------------------

fn merge_group(orig_module: &mut Module, merge_module: &mut Module) {
    // special handling for GROUPs: merge these based only on the name, not the content
    let merge_group_list = std::mem::take(&mut merge_module.group);
    for mut group in merge_group_list {
        if let Some(orig_group) = orig_module.group.get_mut(&group.name) {
            // a group with this name already exists in the original module
            if *orig_group == group {
                // the group in the original module is identical to the one in the merge module
                // so we can just skip this one
                continue;
            } else {
                // merge the sub groups
                match (&mut orig_group.sub_group, group.sub_group) {
                    (Some(orig_sub_group), Some(merge_sub_group)) => {
                        // merge the sub_groups
                        for item in merge_sub_group.identifier_list {
                            if !orig_sub_group.identifier_list.contains(&item) {
                                orig_sub_group.identifier_list.push(item);
                            }
                        }
                    }
                    (None, Some(merge_sub_group)) => {
                        // the original group has no sub_group, but the merge group has one
                        orig_group.sub_group = Some(merge_sub_group);
                    }
                    _ => {}
                }

                // merge the function lists
                match (&mut orig_group.function_list, group.function_list) {
                    (Some(orig_function_list), Some(merge_function_list)) => {
                        // merge the sub_functions
                        for item in merge_function_list.name_list {
                            if !orig_function_list.name_list.contains(&item) {
                                orig_function_list.name_list.push(item);
                            }
                        }
                    }
                    (None, Some(merge_function_list)) => {
                        // the original group has no function_list, but the merge group has one
                        orig_group.function_list = Some(merge_function_list);
                    }
                    _ => {}
                }

                // merge the ref_characteristics
                match (&mut orig_group.ref_characteristic, group.ref_characteristic) {
                    (Some(orig_ref_characteristic), Some(merge_ref_characteristic)) => {
                        // merge the sub_functions
                        for item in merge_ref_characteristic.identifier_list {
                            if !orig_ref_characteristic.identifier_list.contains(&item) {
                                orig_ref_characteristic.identifier_list.push(item);
                            }
                        }
                    }
                    (None, Some(merge_ref_characteristic)) => {
                        // the original group has no ref_characteristic, but the merge group has one
                        orig_group.ref_characteristic = Some(merge_ref_characteristic);
                    }
                    _ => {}
                }

                // merge the ref_measurements
                match (&mut orig_group.ref_measurement, group.ref_measurement) {
                    (Some(orig_ref_measurement), Some(merge_ref_measurement)) => {
                        // merge the sub_functions
                        for item in merge_ref_measurement.identifier_list {
                            if !orig_ref_measurement.identifier_list.contains(&item) {
                                orig_ref_measurement.identifier_list.push(item);
                            }
                        }
                    }
                    (None, Some(merge_ref_measurement)) => {
                        // the original group has no ref_measurement, but the merge group has one
                        orig_group.ref_measurement = Some(merge_ref_measurement);
                    }
                    _ => {}
                }

                // not merged: the ROOT attribute. We keep the original one. ANNOTATIONs are also not merged
            }
        } else {
            // no group with this name exists in the original module
            group.reset_location();
            orig_module.group.push(group);
        }
    }
}

// ------------------------ FRAME ------------------------

fn merge_frame(orig_module: &mut Module, merge_module: &mut Module) {
    let (merge_action, mut rename_table) =
        calculate_item_actions(&orig_module.frame, &merge_module.frame);

    let merge_frame_list = std::mem::take(&mut merge_module.frame);
    for mut frame in merge_frame_list {
        if let Some(true) = merge_action.get(&frame.name) {
            frame.reset_location();
            if let Some(newname) = rename_table.remove(&frame.name) {
                frame.name = newname.clone();
            }
            orig_module.frame.push(frame);
        }
    }
}

// ------------------------ TRANSFORMER ------------------------

fn merge_transformer(orig_module: &mut Module, merge_module: &mut Module) {
    let (merge_action, mut rename_table) =
        calculate_item_actions(&orig_module.transformer, &merge_module.transformer);

    rename_transformer_refs(merge_module, &rename_table);

    let merge_transformer_list = std::mem::take(&mut merge_module.transformer);
    for mut transformer in merge_transformer_list {
        if let Some(true) = merge_action.get(&transformer.name) {
            transformer.reset_location();
            if let Some(newname) = rename_table.remove(&transformer.name) {
                transformer.name = newname.clone();
            }
            orig_module.transformer.push(transformer);
        }
    }
}

fn rename_transformer_refs(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for transformer in &mut merge_module.transformer {
        if let Some(newname) = rename_table.get(&transformer.inverse_transformer) {
            transformer.inverse_transformer = newname.to_owned();
        }
    }
}

// ------------------------ TYPEDEF_* ------------------------

fn rename_typedef_refs(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    // MODULE.TYPEDEF_STRUCTURE
    for typedef_structure in &mut merge_module.typedef_structure {
        // MODULE.TYPEDEF_STRUCTURE.STRUCTURE_COMPONENT
        for structure_component in &mut typedef_structure.structure_component {
            if let Some(newname) = rename_table.get(&structure_component.component_type) {
                structure_component.component_type = newname.to_owned();
            }
        }
    }
}

// ------------------------ USER_RIGHTS ------------------------

fn merge_user_rights(orig_module: &mut Module, merge_module: &mut Module) {
    // the specification only allows one user rights block per user id
    // If a merged user rights block already exists in the original module, it will be ignored
    let merge_user_rights_list = std::mem::take(&mut merge_module.user_rights);
    for mut merge_user_rights in merge_user_rights_list {
        merge_user_rights.reset_location();
        // make sure not to create any duplicates
        if !orig_module
            .user_rights
            .iter()
            .any(|user_rights| user_rights.user_level_id == merge_user_rights.user_level_id)
        {
            orig_module.user_rights.push(merge_user_rights);
        }
    }
}

// ------------------------ VARIANT_CODING ------------------------

fn merge_variant_coding(orig_module: &mut Module, merge_module: &mut Module) {
    // "merging" the variant coding is an all or nothing affair: if the element
    // does not exist on the orig side, it will be taken from the merge side
    if orig_module.variant_coding.is_none() && merge_module.variant_coding.is_some() {
        let mut variant_coding = std::mem::take(&mut merge_module.variant_coding).unwrap();
        variant_coding.reset_location();
        orig_module.variant_coding = Some(variant_coding);
    }
}

// ------------------------------------------------

fn calculate_item_actions<T>(
    orig_map: &ItemList<T>,
    merge_map: &ItemList<T>,
) -> (HashMap<String, bool>, HashMap<String, String>)
where
    T: PartialEq + A2lObjectName,
{
    let mut rename_table = HashMap::<String, String>::new();
    let mut merge_action = HashMap::<String, bool>::new();

    for merge_object in merge_map {
        let name = merge_object.get_name();
        if let Some(orig_object) = orig_map.get(name) {
            if *orig_object == *merge_object {
                // identical items exists on both sides, no need to merge
                merge_action.insert(name.to_owned(), false);
            } else {
                // items with the same name but with different content exist on both sides. Rename the new ones before merging
                let newname = make_unique_name(name, orig_map, merge_map);
                rename_table.insert(name.to_string(), newname);
                merge_action.insert(name.to_string(), true);
            }
        } else {
            // no item with this name exists on the orig side
            merge_action.insert(name.to_owned(), true);
        }
    }

    (merge_action, rename_table)
}

fn rename_item_list(items: &mut Vec<String>, rename_table: &HashMap<String, String>) {
    for item in items {
        if let Some(newname) = rename_table.get(item) {
            *item = newname.to_owned();
        }
    }
}

fn make_unique_name<T: A2lObjectName>(
    current_name: &str,
    orig_map: &ItemList<T>,
    merge_map: &ItemList<T>,
) -> String {
    let mut newname = format!("{current_name}.MERGE");
    let mut idx = 1;

    while merge_map.get(&newname).is_some() || orig_map.get(&newname).is_some() {
        idx += 1;
        newname = format!("{current_name}.MERGE{idx}");
    }

    newname
}

/// Merge the items of `merge_module` into `orig_module`.
/// This is a limited form of merge, that only imports "new" items from `merge_module` into `orig_module`.
/// If an item of the same name already exists in `orig_module`, it will be kept as is, and the item from `merge_module` will be ignored.
/// Essentially, the orig_module is preferred, and merge_module is only used to fill in missing items.
///
/// For groups only, the content is merged, so that subgroups, functions, and references are combined.
pub(crate) fn import_new_module_items(orig: &mut Module, merge: &mut Module) {
    if orig.a2ml.is_none() {
        orig.a2ml = merge.a2ml.take();
    }
    if orig.mod_par.is_none() {
        orig.mod_par = merge.mod_par.take();
    }
    if orig.if_data.is_empty() && !merge.if_data.is_empty() {
        let if_data = std::mem::take(&mut merge.if_data);
        orig.if_data = if_data;
    }
    copy_missing(&mut orig.unit, &mut merge.unit);
    copy_missing(&mut orig.compu_tab, &mut merge.compu_tab);
    copy_missing(&mut orig.compu_vtab, &mut merge.compu_vtab);
    copy_missing(&mut orig.compu_vtab_range, &mut merge.compu_vtab_range);
    copy_missing(&mut orig.compu_method, &mut merge.compu_method);
    copy_missing(&mut orig.record_layout, &mut merge.record_layout);
    if orig.mod_common.is_none() {
        orig.mod_common = merge.mod_common.take();
    }
    copy_missing(&mut orig.axis_pts, &mut merge.axis_pts);
    copy_missing(&mut orig.blob, &mut merge.blob);
    copy_missing(&mut orig.characteristic, &mut merge.characteristic);
    copy_missing(&mut orig.measurement, &mut merge.measurement);
    copy_missing(&mut orig.instance, &mut merge.instance);
    copy_missing(&mut orig.typedef_axis, &mut merge.typedef_axis);
    copy_missing(&mut orig.typedef_blob, &mut merge.typedef_blob);
    copy_missing(
        &mut orig.typedef_characteristic,
        &mut merge.typedef_characteristic,
    );
    copy_missing(
        &mut orig.typedef_measurement,
        &mut merge.typedef_measurement,
    );
    copy_missing(&mut orig.typedef_structure, &mut merge.typedef_structure);
    copy_missing(&mut orig.frame, &mut merge.frame);
    copy_missing(&mut orig.transformer, &mut merge.transformer);
    copy_missing(&mut orig.function, &mut merge.function);

    merge_groups_simple(orig, merge);
    let merge_user_rights = std::mem::take(&mut merge.user_rights);
    for mut user_rights in merge_user_rights {
        if !orig.user_rights.contains(&user_rights) {
            user_rights.reset_location();
            orig.user_rights.push(user_rights);
        }
    }

    if orig.variant_coding.is_none() {
        // copy the whole VARIANT_CODING from the merged module if it doesn't exist in the original
        orig.variant_coding = merge.variant_coding.take();
    } else if let Some(existing_variant) = &mut orig.variant_coding
        && let Some(merge_variant) = &mut merge.variant_coding
    {
        copy_missing(
            &mut existing_variant.var_criterion,
            &mut merge_variant.var_criterion,
        );
        copy_missing(
            &mut existing_variant.var_characteristic,
            &mut merge_variant.var_characteristic,
        );
        if existing_variant.var_naming.is_none() {
            existing_variant.var_naming = merge_variant.var_naming.take();
        }
        if existing_variant.var_separator.is_none() {
            existing_variant.var_separator = merge_variant.var_separator.take();
        }
        let var_forbidden_comb = std::mem::take(&mut merge_variant.var_forbidden_comb);
        for var_forbidden_comb in var_forbidden_comb {
            if !existing_variant
                .var_forbidden_comb
                .contains(&var_forbidden_comb)
            {
                existing_variant.var_forbidden_comb.push(var_forbidden_comb);
            }
        }
    }
}

/// A simplified merge for groups, that only merges the content of existing groups,
/// but does not rename or skip any groups.
fn merge_groups_simple(orig: &mut Module, merge: &mut Module) {
    let merge_groups = std::mem::take(&mut merge.group);
    for mut merge_group in merge_groups {
        if let Some(existing_group) = orig.group.get_mut(merge_group.get_name()) {
            if existing_group.sub_group.is_none() {
                existing_group.sub_group = merge_group.sub_group;
            } else if let Some(existing_sub_group) = &mut existing_group.sub_group
                && let Some(mut merge_sub_group) = merge_group.sub_group
            {
                merge_lists(
                    &mut existing_sub_group.identifier_list,
                    &mut merge_sub_group.identifier_list,
                );
            }
            if existing_group.function_list.is_none() {
                existing_group.function_list = merge_group.function_list;
            } else if let Some(existing_function_list) = &mut existing_group.function_list
                && let Some(mut merge_function_list) = merge_group.function_list
            {
                merge_lists(
                    &mut existing_function_list.name_list,
                    &mut merge_function_list.name_list,
                );
            }
            if existing_group.ref_characteristic.is_none() {
                existing_group.ref_characteristic = merge_group.ref_characteristic;
            } else if let Some(existing_ref_characteristic) = &mut existing_group.ref_characteristic
                && let Some(mut merge_ref_characteristic) = merge_group.ref_characteristic
            {
                merge_lists(
                    &mut existing_ref_characteristic.identifier_list,
                    &mut merge_ref_characteristic.identifier_list,
                );
            }
            if existing_group.ref_measurement.is_none() {
                existing_group.ref_measurement = merge_group.ref_measurement;
            } else if let Some(existing_ref_measurement) = &mut existing_group.ref_measurement
                && let Some(mut merge_ref_measurement) = merge_group.ref_measurement
            {
                merge_lists(
                    &mut existing_ref_measurement.identifier_list,
                    &mut merge_ref_measurement.identifier_list,
                );
            }
            // not merged: the ROOT attribute. We keep the original one
        } else {
            merge_group.reset_location();
            orig.group.push(merge_group);
        }
    }
}

fn copy_missing<U, T: A2lObjectName + A2lObject<U>>(
    existing_items: &mut ItemList<T>,
    new_items: &mut ItemList<T>,
) {
    let new_items = std::mem::take(new_items);
    for mut new_item in new_items {
        if !existing_items.contains_key(new_item.get_name()) {
            new_item.reset_location();
            existing_items.push(new_item);
        }
    }
}

fn merge_lists<T: PartialEq>(existing: &mut Vec<T>, merge: &mut Vec<T>) {
    let merge_list = std::mem::take(merge);
    for item in merge_list {
        if !existing.contains(&item) {
            existing.push(item);
        }
    }
}

/// Merge the items of `merge_module` into `orig_module`.
/// This is a limited form of merge, that imports all items from `merge_module` into `orig_module`.
/// If an item of the same name already exists in `orig_module`, it will be overwritten by the item from `merge_module`.
/// Here, the merge_module is preferred, so this function is the reverse of `import_new_module_items`.
///
/// For groups only, the content is merged, so that subgroups, functions, and references are combined.
pub(crate) fn import_all_module_items(orig: &mut Module, merge: &mut Module) {
    if merge.a2ml.is_some() {
        orig.a2ml = merge.a2ml.take();
    }
    if merge.mod_par.is_some() {
        orig.mod_par = merge.mod_par.take();
    }
    if !merge.if_data.is_empty() {
        let if_data = std::mem::take(&mut merge.if_data);
        orig.if_data = if_data;
    }
    copy_replace_items(&mut orig.unit, &mut merge.unit);
    copy_replace_items(&mut orig.compu_tab, &mut merge.compu_tab);
    copy_replace_items(&mut orig.compu_vtab, &mut merge.compu_vtab);
    copy_replace_items(&mut orig.compu_vtab_range, &mut merge.compu_vtab_range);
    copy_replace_items(&mut orig.compu_method, &mut merge.compu_method);
    copy_replace_items(&mut orig.record_layout, &mut merge.record_layout);
    if merge.mod_common.is_some() {
        orig.mod_common = merge.mod_common.take();
    }
    copy_replace_items(&mut orig.axis_pts, &mut merge.axis_pts);
    copy_replace_items(&mut orig.blob, &mut merge.blob);
    copy_replace_items(&mut orig.characteristic, &mut merge.characteristic);
    copy_replace_items(&mut orig.measurement, &mut merge.measurement);
    copy_replace_items(&mut orig.instance, &mut merge.instance);
    copy_replace_items(&mut orig.typedef_axis, &mut merge.typedef_axis);
    copy_replace_items(&mut orig.typedef_blob, &mut merge.typedef_blob);
    copy_replace_items(
        &mut orig.typedef_characteristic,
        &mut merge.typedef_characteristic,
    );
    copy_replace_items(
        &mut orig.typedef_measurement,
        &mut merge.typedef_measurement,
    );
    copy_replace_items(&mut orig.typedef_structure, &mut merge.typedef_structure);
    copy_replace_items(&mut orig.frame, &mut merge.frame);
    copy_replace_items(&mut orig.transformer, &mut merge.transformer);
    copy_replace_items(&mut orig.function, &mut merge.function);

    merge_groups_simple(orig, merge);
    let merge_user_rights = std::mem::take(&mut merge.user_rights);
    for mut user_rights in merge_user_rights {
        if !orig.user_rights.contains(&user_rights) {
            user_rights.reset_location();
            orig.user_rights.push(user_rights);
        }
    }

    if orig.variant_coding.is_none() {
        // copy the whole VARIANT_CODING from the merged module if it doesn't exist in the original
        orig.variant_coding = merge.variant_coding.take();
    } else if let Some(existing_variant) = &mut orig.variant_coding
        && let Some(merge_variant) = &mut merge.variant_coding
    {
        copy_replace_items(
            &mut existing_variant.var_criterion,
            &mut merge_variant.var_criterion,
        );
        copy_replace_items(
            &mut existing_variant.var_characteristic,
            &mut merge_variant.var_characteristic,
        );
        if merge_variant.var_naming.is_some() {
            existing_variant.var_naming = merge_variant.var_naming.take();
        }
        if merge_variant.var_separator.is_some() {
            existing_variant.var_separator = merge_variant.var_separator.take();
        }
        let var_forbidden_comb = std::mem::take(&mut merge_variant.var_forbidden_comb);
        for var_forbidden_comb in var_forbidden_comb {
            if !existing_variant
                .var_forbidden_comb
                .contains(&var_forbidden_comb)
            {
                existing_variant.var_forbidden_comb.push(var_forbidden_comb);
            }
        }
    }
}

fn copy_replace_items<U, T: A2lObjectName + A2lObject<U>>(
    existing_items: &mut ItemList<T>,
    new_items: &mut ItemList<T>,
) {
    let new_items = std::mem::take(new_items);
    for mut new_item in new_items {
        if let Some(existing_item) = existing_items.get(new_item.get_name()) {
            // replace the existing item with the new one, but keep the UID of the existing item
            // the uid is used for layout tracking, so by keeping it the new item takes the place of the old one
            new_item.get_layout_mut().uid = existing_item.get_layout().uid;
            existing_items.push_unique(new_item);
        } else {
            new_item.reset_location();
            existing_items.push(new_item);
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn test_merge_a2ml() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p "" /begin MODULE m "" /begin A2ML block "IF_DATA" struct { int; }; /end A2ML /end MODULE /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p "" /begin MODULE m "" /begin A2ML block "IF_DATA" struct { float; }; /end A2ML /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();

        // merging B into A: A2ML is copied
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert!(a2l_file_a.project.module[0].a2ml.is_some());

        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        let orig_txt = a2l_file_b.project.module[0]
            .a2ml
            .as_ref()
            .unwrap()
            .a2ml_text
            .clone();
        assert_ne!(
            a2l_file_c.project.module[0]
                .a2ml
                .as_ref()
                .unwrap()
                .a2ml_text,
            orig_txt
        );
        a2l_file_b.merge_modules(&mut a2l_file_c);
        // merging C into B: A2ML is unchanged
        assert_eq!(
            a2l_file_b.project.module[0]
                .a2ml
                .as_ref()
                .unwrap()
                .a2ml_text,
            orig_txt
        );
    }

    #[test]
    fn test_merge_measurement() {
        static FILE_A: &str =
            r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p ""
            /begin MODULE m ""
                /begin MEASUREMENT measurement1 "" FLOAT32_IEEE conversion 1 1.0 0 100
                /end MEASUREMENT
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p ""
            /begin MODULE m ""
                /begin MEASUREMENT measurement1 "" SWORD conversion 1 1.0 0 100
                /end MEASUREMENT
                /begin MEASUREMENT measurement2 "" FLOAT32_IEEE conversion 1 1.0 0 100
                /end MEASUREMENT

                /begin AXIS_PTS axispts1 "" 0x1234 measurement1 deposit_record 0 conversion 3 0.0 10.0
                /end AXIS_PTS
                /begin AXIS_PTS axispts2 "" 0x1234 measurement2 deposit_record 0 conversion 3 0.0 10.0
                /end AXIS_PTS
                /begin CHARACTERISTIC characteristic1 "" CURVE 0x1234 record_layout_name 0 compu_method 0.0 1.0
                    /begin AXIS_DESCR COM_AXIS measurement1 compu_method 1 0 100
                    /end AXIS_DESCR
                /end CHARACTERISTIC
                /begin CHARACTERISTIC characteristic2 "" CURVE 0x1234 record_layout_name 0 compu_method 0.0 1.0
                    /begin AXIS_DESCR COM_AXIS measurement2 compu_method 1 0 100
                    /end AXIS_DESCR
                /end CHARACTERISTIC
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic1 "" VALUE record_layout_name 0 compu_method 0 100
                    /begin AXIS_DESCR COM_AXIS measurement1 compu_method 1 0 100
                    /end AXIS_DESCR
                /end TYPEDEF_CHARACTERISTIC
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic2 "" VALUE record_layout_name 0 compu_method 0 100
                    /begin AXIS_DESCR COM_AXIS measurement2 compu_method 1 0 100
                    /end AXIS_DESCR
                /end TYPEDEF_CHARACTERISTIC
                /begin FRAME frame1 "" 1 2
                    FRAME_MEASUREMENT measurement1 measurement2
                /end FRAME
                /begin VARIANT_CODING
                    /begin VAR_CRITERION criterion_name1 ""
                        VAR_MEASUREMENT measurement1
                    /end VAR_CRITERION
                    /begin VAR_CRITERION criterion_name2 ""
                        VAR_MEASUREMENT measurement2
                    /end VAR_CRITERION
                /end VARIANT_CODING
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no MEASUREMENT, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        assert!(a2l_file_a.project.module[0].measurement.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].measurement.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, true).unwrap();
        assert_eq!(a2l_file_b.project.module[0].measurement.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].measurement.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].measurement.len(), 3);

        let module = &a2l_file_b.project.module[0];

        // check that references to measurement1 in file C have been renamed to measurement1.MERGE
        let axispts1 = &module.axis_pts[0];
        assert_eq!(axispts1.input_quantity, "measurement1.MERGE");
        let axispts2 = &module.axis_pts[1];
        assert_eq!(axispts2.input_quantity, "measurement2");

        let characteristic1 = &module.characteristic[0];
        assert_eq!(
            characteristic1.axis_descr[0].input_quantity,
            "measurement1.MERGE"
        );
        let characteristic2 = &module.characteristic[1];
        assert_eq!(characteristic2.axis_descr[0].input_quantity, "measurement2");

        let typedef_characteristic1 = &module.typedef_characteristic[0];
        assert_eq!(
            typedef_characteristic1.axis_descr[0].input_quantity,
            "measurement1.MERGE"
        );
        let typedef_characteristic2 = &module.typedef_characteristic[1];
        assert_eq!(
            typedef_characteristic2.axis_descr[0].input_quantity,
            "measurement2"
        );

        let frame = &module.frame[0];
        let frame_measurement = frame.frame_measurement.as_ref().unwrap();
        assert_eq!(frame_measurement.identifier_list.len(), 2);
        assert_eq!(frame_measurement.identifier_list[0], "measurement1.MERGE");
        assert_eq!(frame_measurement.identifier_list[1], "measurement2");

        let variant_coding = &module.variant_coding.as_ref().unwrap();
        let var_criterion1 = &variant_coding.var_criterion[0];
        assert_eq!(
            var_criterion1.var_measurement.as_ref().unwrap().name,
            "measurement1.MERGE"
        );
        let var_criterion2 = &variant_coding.var_criterion[1];
        assert_eq!(
            var_criterion2.var_measurement.as_ref().unwrap().name,
            "measurement2"
        );
    }

    #[test]
    fn test_typedef_measurement() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_MEASUREMENT td_measurement1 "" FLOAT32_IEEE conversion 1 1.0 0 100
                /end TYPEDEF_MEASUREMENT
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_MEASUREMENT td_measurement1 "" SWORD conversion 1 1.0 0 100
                /end TYPEDEF_MEASUREMENT
                /begin TYPEDEF_MEASUREMENT td_measurement2 "" FLOAT32_IEEE conversion 1 1.0 0 100
                /end TYPEDEF_MEASUREMENT
                
                /begin TYPEDEF_STRUCTURE typedef_structure1 "" 0
                    /begin STRUCTURE_COMPONENT component1 td_measurement1 0
                    /end STRUCTURE_COMPONENT
                    /begin STRUCTURE_COMPONENT component2 td_measurement2 1
                    /end STRUCTURE_COMPONENT
                /end TYPEDEF_STRUCTURE
                /begin INSTANCE instance1 "" td_measurement1 0x0
                /end INSTANCE
                /begin INSTANCE instance2 "" td_measurement2 0x0
                /end INSTANCE
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no MEASUREMENT, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].measurement.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].typedef_measurement.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].typedef_measurement.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].typedef_measurement.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].typedef_measurement.len(), 3);

        let typedef_structure = &a2l_file_b.project.module[0].typedef_structure[0];
        assert_eq!(typedef_structure.structure_component.len(), 2);
        assert_eq!(
            typedef_structure.structure_component[0].component_type,
            "td_measurement1.MERGE"
        );
        assert_eq!(
            typedef_structure.structure_component[1].component_type,
            "td_measurement2"
        );
    }

    #[test]
    fn test_merge_characteristic() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p ""
            /begin MODULE m ""
                /begin CHARACTERISTIC characteristic1 "" VALUE 0x1234 deposit_ident 0 conversion 0.0 10.0
                /end CHARACTERISTIC
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p ""
            /begin MODULE m ""
                /begin CHARACTERISTIC characteristic1 "" VALUE 0x1234 deposit_ident 0 foo 0.0 10.0
                    /begin AXIS_DESCR COM_AXIS measurement_name compu_method 1 0 100
                        CURVE_AXIS_REF characteristic1
                    /end AXIS_DESCR
                    /begin DEPENDENT_CHARACTERISTIC "x" characteristic1 /end DEPENDENT_CHARACTERISTIC
                /end CHARACTERISTIC
                /begin CHARACTERISTIC characteristic2 "" VALUE 0x1235 deposit_ident 0 conversion 0.0 10.0
                    /begin AXIS_DESCR COM_AXIS measurement_name compu_method 1 0 100
                        CURVE_AXIS_REF characteristic2
                    /end AXIS_DESCR
                    /begin DEPENDENT_CHARACTERISTIC "x" characteristic2 /end DEPENDENT_CHARACTERISTIC
                /end CHARACTERISTIC

                /begin FUNCTION function_name ""
                    /begin DEF_CHARACTERISTIC characteristic1 characteristic2
                    /end DEF_CHARACTERISTIC
                    /begin REF_CHARACTERISTIC characteristic1 characteristic2
                    /end REF_CHARACTERISTIC
                /end FUNCTION
                /begin GROUP group_name "" ROOT
                    /begin REF_CHARACTERISTIC characteristic1 characteristic2
                    /end REF_CHARACTERISTIC
                /end GROUP
                /begin VARIANT_CODING
                    /begin VAR_CRITERION criterion_name1 ""
                        VAR_SELECTION_CHARACTERISTIC characteristic1
                    /end VAR_CRITERION
                    /begin VAR_CRITERION criterion_name2 ""
                        VAR_SELECTION_CHARACTERISTIC characteristic2
                    /end VAR_CRITERION
                    /begin VAR_CHARACTERISTIC name
                        characteristic1 characteristic2
                    /end VAR_CHARACTERISTIC
                /end VARIANT_CODING
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no CHARACTERISTIC, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].characteristic.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].characteristic.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, true).unwrap();
        assert_eq!(a2l_file_b.project.module[0].characteristic.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].characteristic.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].characteristic.len(), 3);

        let module = &a2l_file_b.project.module[0];
        let characteristic1 = &module.characteristic[0];
        assert_eq!(characteristic1.name, "characteristic1");

        let characteristic1_merge = &module.characteristic[1];
        let curve_axis_ref1 = &characteristic1_merge.axis_descr[0].curve_axis_ref;
        assert_eq!(
            curve_axis_ref1.as_ref().unwrap().curve_axis,
            "characteristic1.MERGE"
        );
        let dep_characteristic1 = &characteristic1_merge
            .dependent_characteristic
            .as_ref()
            .unwrap();
        assert_eq!(
            dep_characteristic1.characteristic_list[0],
            "characteristic1.MERGE"
        );
        let characteristic2 = &module.characteristic[2];
        let curve_axis_ref2 = &characteristic2.axis_descr[0].curve_axis_ref;
        assert_eq!(
            curve_axis_ref2.as_ref().unwrap().curve_axis,
            "characteristic2"
        );
        let dep_characteristic2 = &characteristic2.dependent_characteristic.as_ref().unwrap();
        assert_eq!(
            dep_characteristic2.characteristic_list[0],
            "characteristic2"
        );

        let function = &a2l_file_b.project.module[0].function[0];
        let def_characteristic = &function.def_characteristic.as_ref().unwrap();
        assert_eq!(
            def_characteristic.identifier_list[0],
            "characteristic1.MERGE"
        );
        assert_eq!(def_characteristic.identifier_list[1], "characteristic2");

        let ref_characteristic = &function.ref_characteristic.as_ref().unwrap();
        assert_eq!(
            ref_characteristic.identifier_list[0],
            "characteristic1.MERGE"
        );
        assert_eq!(ref_characteristic.identifier_list[1], "characteristic2");

        let group = &a2l_file_b.project.module[0].group[0];
        let ref_characteristic = &group.ref_characteristic.as_ref().unwrap();
        assert_eq!(
            ref_characteristic.identifier_list[0],
            "characteristic1.MERGE"
        );
        assert_eq!(ref_characteristic.identifier_list[1], "characteristic2");
    }

    #[test]
    fn test_merge_typedef_characteristic() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic1 "" VALUE record_layout_name 0 compu_method_name 0 100
                /end TYPEDEF_CHARACTERISTIC
                /begin INSTANCE instance1 "" typedef_characteristic1 0x100
                /end INSTANCE
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic1 "" VAL_BLK record_layout_name 0 compu_method_name 0 100
                    /begin AXIS_DESCR COM_AXIS measurement_name compu_method 1 0 100
                        CURVE_AXIS_REF instance1
                    /end AXIS_DESCR
                /end TYPEDEF_CHARACTERISTIC
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic2 "" VALUE record_layout_name 0 compu_method_name 0 100
                    /begin AXIS_DESCR COM_AXIS measurement_name compu_method 1 0 100
                        CURVE_AXIS_REF instance2
                    /end AXIS_DESCR
                /end TYPEDEF_CHARACTERISTIC

                /begin INSTANCE instance1 "" typedef_characteristic1 0x0
                /end INSTANCE
                /begin INSTANCE instance2 "" typedef_characteristic2 0x0
                /end INSTANCE
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no CHARACTERISTIC, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(
            a2l_file_a.project.module[0]
                .typedef_characteristic
                .is_empty()
        );
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].typedef_characteristic.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].typedef_characteristic.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].typedef_characteristic.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].typedef_characteristic.len(), 3);

        let module = &a2l_file_b.project.module[0];
        let td_chara1 = &module.typedef_characteristic[0];
        assert_eq!(td_chara1.name, "typedef_characteristic1");
        let td_chara1_merge = &module.typedef_characteristic[1];
        let curve_axis_ref1 = &td_chara1_merge.axis_descr[0].curve_axis_ref;
        assert_eq!(
            curve_axis_ref1.as_ref().unwrap().curve_axis,
            "instance1.MERGE"
        );
        let td_chara2 = &module.typedef_characteristic[2];
        let curve_axis_ref2 = &td_chara2.axis_descr[0].curve_axis_ref;
        assert_eq!(curve_axis_ref2.as_ref().unwrap().curve_axis, "instance2");
    }

    #[test]
    fn test_merge_axis_pts() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin AXIS_PTS axispts1 "" 0x1234 input_qty deposit_record 0 conversion 3 0.0 10.0
                /end AXIS_PTS
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin AXIS_PTS axispts1 "" 0x1234 input_qty deposit_record 0 foo 3 0.0 10.0
                /end AXIS_PTS
                /begin AXIS_PTS axispts2 "" 0x1235 input_qty deposit_record 0 conversion 3 0.0 10.0
                /end AXIS_PTS

                /begin CHARACTERISTIC characteristic1 "" CURVE 0x1234 record_layout_name 0 compu_method 0.0 1.0
                    /begin AXIS_DESCR COM_AXIS measurement_name compu_method 1 0 100
                        AXIS_PTS_REF axispts1
                    /end AXIS_DESCR
                    /begin AXIS_DESCR COM_AXIS measurement_name compu_method 1 0 100
                        AXIS_PTS_REF axispts2
                    /end AXIS_DESCR
                /end CHARACTERISTIC
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic1 "" VAL_BLK record_layout_name 0 compu_method_name 0 100
                    /begin AXIS_DESCR COM_AXIS measurement_name compu_method 1 0 100
                        AXIS_PTS_REF axispts1
                    /end AXIS_DESCR
                    /begin AXIS_DESCR COM_AXIS measurement_name compu_method 1 0 100
                        AXIS_PTS_REF axispts2
                    /end AXIS_DESCR
                /end TYPEDEF_CHARACTERISTIC
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no AXIS_PTS, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].axis_pts.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].axis_pts.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].axis_pts.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].axis_pts.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].axis_pts.len(), 3);

        let characteristic = &a2l_file_b.project.module[0].characteristic[0];
        assert_eq!(characteristic.axis_descr.len(), 2);
        let axis_pts_ref1 = &characteristic.axis_descr[0].axis_pts_ref.as_ref().unwrap();
        assert_eq!(axis_pts_ref1.axis_points, "axispts1.MERGE");
        let axis_pts_ref2 = &characteristic.axis_descr[1].axis_pts_ref.as_ref().unwrap();
        assert_eq!(axis_pts_ref2.axis_points, "axispts2");

        let td_characteristic = &a2l_file_b.project.module[0].typedef_characteristic[0];
        assert_eq!(td_characteristic.axis_descr.len(), 2);
        let axis_pts_ref1 = &td_characteristic.axis_descr[0]
            .axis_pts_ref
            .as_ref()
            .unwrap();
        assert_eq!(axis_pts_ref1.axis_points, "axispts1.MERGE");
        let axis_pts_ref2 = &td_characteristic.axis_descr[1]
            .axis_pts_ref
            .as_ref()
            .unwrap();
        assert_eq!(axis_pts_ref2.axis_points, "axispts2");
    }

    #[test]
    fn test_merge_typedef_axis() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_AXIS typedef_axis1 "" measurement_name record_layout_name 0 compu_method_name 1 0 100
                /end TYPEDEF_AXIS
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_AXIS typedef_axis1 "" xxxxx record_layout_name 0 compu_method_name 1 0 100
                /end TYPEDEF_AXIS
                /begin TYPEDEF_AXIS typedef_axis2 "" measurement_name record_layout_name 0 compu_method_name 1 0 100
                /end TYPEDEF_AXIS

                /begin INSTANCE instance1 "" typedef_axis1 0x0
                /end INSTANCE
                /begin INSTANCE instance2 "" typedef_axis2 0x0
                /end INSTANCE
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no AXIS_PTS, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].typedef_axis.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].typedef_axis.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].typedef_axis.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].typedef_axis.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].typedef_axis.len(), 3);
    }

    #[test]
    fn test_merge_blob() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin BLOB blob1 "" 0x1234 0x5678
                /end BLOB
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin BLOB blob1 "" 0x567 0x5678
                /end BLOB
                /begin BLOB blob2 "" 0x1235 0x5679
                /end BLOB

                /begin TRANSFORMER transformer_name "version string" "dll32" "dll64" 1 ON_CHANGE NO_INVERSE_TRANSFORMER
                    /begin TRANSFORMER_IN_OBJECTS blob1 blob2
                    /end TRANSFORMER_IN_OBJECTS
                    /begin TRANSFORMER_OUT_OBJECTS blob1 blob2
                    /end TRANSFORMER_OUT_OBJECTS
                /end TRANSFORMER
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no BLOB, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].blob.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].blob.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].blob.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].blob.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].blob.len(), 3);

        let transformer = &a2l_file_b.project.module[0].transformer[0];
        let in_objects = transformer.transformer_in_objects.as_ref().unwrap();
        assert_eq!(in_objects.identifier_list.len(), 2);
        assert_eq!(in_objects.identifier_list[0], "blob1.MERGE");
        assert_eq!(in_objects.identifier_list[1], "blob2");
        let out_objects = transformer.transformer_out_objects.as_ref().unwrap();
        assert_eq!(out_objects.identifier_list.len(), 2);
        assert_eq!(out_objects.identifier_list[0], "blob1.MERGE");
        assert_eq!(out_objects.identifier_list[1], "blob2");
    }

    #[test]
    fn test_merge_typedef_blob() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_BLOB typedef_blob1 "" 1
                /end TYPEDEF_BLOB
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_BLOB typedef_blob1 "" 100
                /end BLOB
                /begin TYPEDEF_BLOB typedef_blob2 "" 1
                /end BLOB

                /begin INSTANCE instance1 "" typedef_blob1 0x0
                /end INSTANCE
                /begin INSTANCE instance2 "" typedef_blob2 0x0
                /end INSTANCE
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no BLOB, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].typedef_blob.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].typedef_blob.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].typedef_blob.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].typedef_blob.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].typedef_blob.len(), 3);
    }

    #[test]
    fn test_merge_typedef_structure() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_STRUCTURE typedef_structure1 "" 0
                /end TYPEDEF_STRUCTURE
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin TYPEDEF_STRUCTURE typedef_structure1 "foo" 100
                /end TYPEDEF_STRUCTURE
                /begin TYPEDEF_STRUCTURE typedef_structure2 "" 0
                /end TYPEDEF_STRUCTURE

                /begin INSTANCE instance1 "" typedef_structure1 0x0
                /end INSTANCE
                /begin INSTANCE instance2 "" typedef_structure2 0x0
                /end INSTANCE
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no BLOB, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].typedef_structure.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].typedef_structure.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].typedef_structure.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].typedef_structure.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);

        let module = &a2l_file_b.project.module[0];
        let td_structure1 = &module.typedef_structure[0];
        assert_eq!(td_structure1.name, "typedef_structure1");

        let td_structure1_merge = &module.typedef_structure[1];
        assert_eq!(td_structure1_merge.name, "typedef_structure1.MERGE");

        let td_structure2 = &module.typedef_structure[2];
        assert_eq!(td_structure2.name, "typedef_structure2");
    }

    #[test]
    fn test_merge_instance() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin INSTANCE instance1 "" type_ref 0x1234
                /end INSTANCE
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin INSTANCE instance1 "" other_type_ref 0x0
                /end INSTANCE
                /begin INSTANCE instance2 "" type_ref 0x1111
                /end INSTANCE
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no INSTANCE, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].instance.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].instance.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].instance.len(), 1);
        assert_eq!(a2l_file_c.project.module[0].instance.len(), 2);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].instance.len(), 3);
    }

    #[test]
    fn test_merge_mod_par() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin MOD_PAR ""
                    /begin MEMORY_LAYOUT PRG_DATA 0x1000 1 0 0 0 0 0 /end MEMORY_LAYOUT
                    /begin MEMORY_LAYOUT PRG_DATA 0x1234 1 0 0 0 0 0 /end MEMORY_LAYOUT
                    /begin MEMORY_SEGMENT equal_seg "" DATA FLASH INTERN 0 0x1000 -1 -1 -1 -1 -1  /end MEMORY_SEGMENT
                    /begin MEMORY_SEGMENT seg_b "" DATA FLASH INTERN 0 0x1234 -1 -1 -1 -1 -1  /end MEMORY_SEGMENT
                    SYSTEM_CONSTANT "System_Constant_1" "1"
                    SYSTEM_CONSTANT "System_Constant_2" "2"
                /end MOD_PAR
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin MOD_PAR ""
                    /begin MEMORY_LAYOUT PRG_DATA 0x1000 1 0 0 0 0 0 /end MEMORY_LAYOUT
                    /begin MEMORY_LAYOUT PRG_DATA 0xFFFFFF 1 0 0 0 0 0 /end MEMORY_LAYOUT
                    /begin MEMORY_SEGMENT equal_seg "" DATA FLASH INTERN 0 0x1000 -1 -1 -1 -1 -1  /end MEMORY_SEGMENT
                    /begin MEMORY_SEGMENT seg_b "" DATA FLASH INTERN 0 0x1235 -1 -1 -1 -1 -1  /end MEMORY_SEGMENT
                    /begin MEMORY_SEGMENT seg_c "" DATA FLASH INTERN 0 0x1234 -1 -1 -1 -1 -1  /end MEMORY_SEGMENT
                    SYSTEM_CONSTANT "System_Constant_1" "1"
                    SYSTEM_CONSTANT "System_Constant_3" "333"
                /end MOD_PAR
                /begin AXIS_PTS axispts1 "long_identifier" 0x1234 input_qty deposit_record 0 conversion 3 0.0 10.0
                    REF_MEMORY_SEGMENT seg_b
                /end AXIS_PTS
                /begin AXIS_PTS axispts2 "long_identifier" 0x1234 input_qty deposit_record 0 conversion 3 0.0 10.0
                /end AXIS_PTS
                /begin CHARACTERISTIC characteristic1 "long_identifier" VALUE 0x1234 deposit_ident 0 conversion 0.0 10.0
                    REF_MEMORY_SEGMENT seg_b
                /end CHARACTERISTIC
                /begin CHARACTERISTIC characteristic2 "long_identifier" VALUE 0x1234 deposit_ident 0 conversion 0.0 10.0
                /end CHARACTERISTIC
                /begin MEASUREMENT measurement1 "long_identifier" FLOAT32_IEEE conversion 1 1.0 0 100
                    REF_MEMORY_SEGMENT seg_b
                /end MEASUREMENT
                /begin MEASUREMENT measurement2 "long_identifier" FLOAT32_IEEE conversion 1 1.0 0 100
                /end MEASUREMENT
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no MOD_PAR, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].mod_par.is_none());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert!(a2l_file_a.project.module[0].mod_par.is_some());

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert!(a2l_file_b.project.module[0].mod_par.is_some());
        assert!(a2l_file_c.project.module[0].mod_par.is_some());
        a2l_file_b.merge_modules(&mut a2l_file_c);
        let mod_par_b = a2l_file_b.project.module[0].mod_par.as_ref().unwrap();
        assert_eq!(mod_par_b.memory_layout.len(), 3);
        assert_eq!(mod_par_b.memory_segment.len(), 4);
        assert_eq!(mod_par_b.system_constant.len(), 3);

        let module = &a2l_file_b.project.module[0];
        // the references to MEMORY_SGMENT seg_b should be renamed
        let ref_memory_segment = &module.axis_pts[0].ref_memory_segment.as_ref();
        assert_ne!(ref_memory_segment.unwrap().name, "seg_b");
        let ref_memory_segment = &module.characteristic[0].ref_memory_segment.as_ref();
        assert_ne!(ref_memory_segment.unwrap().name, "seg_b");
        let ref_memory_segment = &module.measurement[0].ref_memory_segment.as_ref();
        assert_ne!(ref_memory_segment.unwrap().name, "seg_b");
    }

    #[test]
    fn test_merge_compu_method() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin COMPU_METHOD m1 "" TAB_NOINTP "%6.3" ""
                /end COMPU_METHOD
             /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin COMPU_METHOD m1 "" TAB_NOINTP "%6.3" ""
                    COEFFS 1 2 3 4 5 6
                /end COMPU_METHOD
                /begin COMPU_METHOD m2 "" TAB_NOINTP "%6.3" ""
                /end COMPU_METHOD

                // various items with references to m1 and m2

                /begin AXIS_PTS axispts1 "" 0x1234 NO_INPUT_QUANTITY record_layout_name 0 m1 3 0.0 10.0
                /end AXIS_PTS
                /begin AXIS_PTS axispts2 "" 0x1234 NO_INPUT_QUANTITY record_layout_name 0 m2 3 0.0 10.0
                /end AXIS_PTS
                /begin CHARACTERISTIC characteristic1 "" CURVE 0x1234 record_layout_name 0 m1 0.0 1.0
                    /begin AXIS_DESCR COM_AXIS measurement_name m1 1 0 100
                    /end AXIS_DESCR
                /end CHARACTERISTIC
                /begin CHARACTERISTIC characteristic2 "" CURVE 0x1234 record_layout_name 0 m2 0.0 1.0
                    /begin AXIS_DESCR COM_AXIS measurement_name m2 1 0 100
                    /end AXIS_DESCR
                /end CHARACTERISTIC
                /begin MEASUREMENT measurement1 "" FLOAT32_IEEE m1 1 1.0 0 100
                /end MEASUREMENT
                /begin MEASUREMENT measurement2 "" FLOAT32_IEEE m2 1 1.0 0 100
                /end MEASUREMENT
                /begin TYPEDEF_AXIS typedef_axis1 "" measurement_name record_layout_name 0 m1 1 0 100
                /end TYPEDEF_AXIS
                /begin TYPEDEF_AXIS typedef_axis2 "" measurement_name record_layout_name 0 m2 1 0 100
                /end TYPEDEF_AXIS
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic1 "" VALUE record_layout_name 0 m1 0 100
                    /begin AXIS_DESCR COM_AXIS measurement_name m1 1 0 100
                    /end AXIS_DESCR
                /end TYPEDEF_CHARACTERISTIC
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic2 "" VALUE record_layout_name 0 m2 0 100
                    /begin AXIS_DESCR COM_AXIS measurement_name m2 1 0 100
                    /end AXIS_DESCR
                /end TYPEDEF_CHARACTERISTIC
                /begin TYPEDEF_MEASUREMENT typedef_measurement1 "" UBYTE m1 1 1 0 100
                /end TYPEDEF_MEASUREMENT
                /begin TYPEDEF_MEASUREMENT typedef_measurement2 "" UBYTE m2 1 1 0 100
                /end TYPEDEF_MEASUREMENT
             /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no COMPU_METHODs, so they are all taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].compu_method.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].compu_method.len(), 1);

        // merging C into B: m1 is present in both, but with different content -> m1 is renamed
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].compu_method.len(), 1);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].compu_method.len(), 3);

        let module = &a2l_file_b.project.module[0];

        // check that the references in file C to m1 have been renamed
        let axispts1 = &module.axis_pts[0];
        assert_eq!(axispts1.conversion, "m1.MERGE");

        let characteristic1 = &module.characteristic[0];
        assert_eq!(characteristic1.conversion, "m1.MERGE");
        assert_eq!(characteristic1.axis_descr[0].conversion, "m1.MERGE");

        let measurement1 = &module.measurement[0];
        assert_eq!(measurement1.conversion, "m1.MERGE");

        let typedef_axis1 = &module.typedef_axis[0];
        assert_eq!(typedef_axis1.conversion, "m1.MERGE");

        let typedef_characteristic1 = &module.typedef_characteristic[0];
        assert_eq!(typedef_characteristic1.conversion, "m1.MERGE");
        assert_eq!(typedef_characteristic1.axis_descr[0].conversion, "m1.MERGE");

        let typedef_measurement1 = &module.typedef_measurement[0];
        assert_eq!(typedef_measurement1.conversion, "m1.MERGE");

        // check that the references in file C to m2 have not been renamed
        let axispts2 = &module.axis_pts[1];
        assert_eq!(axispts2.conversion, "m2");

        let characteristic2 = &module.characteristic[1];
        assert_eq!(characteristic2.conversion, "m2");
        assert_eq!(characteristic2.axis_descr[0].conversion, "m2");

        let measurement2 = &module.measurement[1];
        assert_eq!(measurement2.conversion, "m2");

        let typedef_axis2 = &module.typedef_axis[1];
        assert_eq!(typedef_axis2.conversion, "m2");

        let typedef_characteristic2 = &module.typedef_characteristic[1];
        assert_eq!(typedef_characteristic2.conversion, "m2");
        assert_eq!(typedef_characteristic2.axis_descr[0].conversion, "m2");

        let typedef_measurement2 = &module.typedef_measurement[1];
        assert_eq!(typedef_measurement2.conversion, "m2");
    }

    #[test]
    fn test_merge_compu_tab() {
        static FILE_A: &str =
            r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT p ""
            /begin MODULE m ""
                /begin COMPU_TAB ct1 "" TAB_NOINTP 1
                    1 1
                /end COMPU_TAB
                /begin COMPU_VTAB cvt1 "" TAB_VERB 1
                    1 "v"
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE cvtr1 "" 1
                    1 2 "v"
                /end COMPU_VTAB_RANGE
                /begin COMPU_TAB ct2 "" TAB_NOINTP 1
                    1 1
                /end COMPU_TAB
                /begin COMPU_VTAB cvt2 "" TAB_VERB 1
                    1 "v"
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE cvtr2 "" 1
                    1 2 "v"
                /end COMPU_VTAB_RANGE
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT p ""
            /begin MODULE m ""
                /begin COMPU_TAB ct1 "" TAB_NOINTP 1
                    1 1
                /end COMPU_TAB
                /begin COMPU_VTAB cvt1 "" TAB_VERB 1
                    1 "v"
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE cvtr1 "" 1
                    1 2 "v"
                /end COMPU_VTAB_RANGE
                /begin COMPU_TAB ct2 "" TAB_NOINTP 1
                    2 2
                /end COMPU_TAB
                /begin COMPU_VTAB cvt2 "" TAB_VERB 1
                    2 "v"
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE cvtr2 "" 1
                    2 3 "v"
                /end COMPU_VTAB_RANGE
                /begin COMPU_METHOD m1 "" TAB_NOINTP "%6.3" ""
                    COMPU_TAB_REF ct2
                /end COMPU_METHOD
                /begin COMPU_METHOD m2 "" TAB_VERB "%6.3" ""
                    COMPU_TAB_REF cvt2
                /end COMPU_METHOD
                /begin COMPU_METHOD m3 "" TAB_VERB "%6.3" ""
                    COMPU_TAB_REF cvtr2
                /end COMPU_METHOD
                /begin COMPU_METHOD m4 "" TAB_NOINTP "%6.3" ""
                    STATUS_STRING_REF ct1
                /end COMPU_METHOD
                /begin COMPU_METHOD m5 "" TAB_NOINTP "%6.3" ""
                    STATUS_STRING_REF cvtr2
                /end COMPU_METHOD
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no COMPU_TABs, so they are all taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        assert!(a2l_file_a.project.module[0].compu_tab.is_empty());
        assert!(a2l_file_a.project.module[0].compu_vtab.is_empty());
        assert!(a2l_file_a.project.module[0].compu_vtab_range.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].compu_tab.len(), 2);
        assert_eq!(a2l_file_a.project.module[0].compu_vtab.len(), 2);
        assert_eq!(a2l_file_a.project.module[0].compu_vtab_range.len(), 2);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, true).unwrap();

        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].compu_tab.len(), 3);
        assert_eq!(a2l_file_b.project.module[0].compu_vtab.len(), 3);
        assert_eq!(a2l_file_b.project.module[0].compu_vtab_range.len(), 3);
    }

    #[test]
    fn test_merge_unit() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin UNIT unit1 "" "x" DERIVED
                /end UNIT
                /begin UNIT unit2 "" "x" DERIVED
                /end UNIT
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin UNIT unit1 "" "x" DERIVED
                /end UNIT
                /begin UNIT unit2 "" "xyz" DERIVED
                /end UNIT
                /begin UNIT unit3 "" "xyz" DERIVED
                /end UNIT
                /begin COMPU_METHOD m1 "" TAB_NOINTP "%6.3" ""
                    REF_UNIT unit2
                /end COMPU_METHOD
                /begin COMPU_METHOD m2 "" TAB_NOINTP "%6.3" ""
                /end COMPU_METHOD
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no UNIT, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].unit.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].unit.len(), 2);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].unit.len(), 2);
        assert_eq!(a2l_file_c.project.module[0].unit.len(), 3);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        // uni1 is identical in both files, so it is not duplicated
        // unit2 is present in both, but with different content -> unit2 is renamed
        // unit3 is only present in C
        assert_eq!(a2l_file_b.project.module[0].unit.len(), 4);

        let module = &a2l_file_b.project.module[0];
        let unit1 = &module.unit[0];
        assert_eq!(unit1.name, "unit1");
        let unit2 = &module.unit[1];
        assert_eq!(unit2.name, "unit2");
        let unit2_merge = &module.unit[2];
        assert_eq!(unit2_merge.name, "unit2.MERGE");
        let unit3 = &module.unit[3];
        assert_eq!(unit3.name, "unit3");

        // check that the references in file C to unit2 have been renamed
        let compu_method1 = &module.compu_method[0];
        let ref_unit = compu_method1.ref_unit.as_ref().unwrap();
        assert_eq!(ref_unit.unit, "unit2.MERGE");
    }

    #[test]
    fn test_merge_record_layout() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin RECORD_LAYOUT record_layout1
                /end RECORD_LAYOUT
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin RECORD_LAYOUT record_layout1
                    AXIS_PTS_X 1 SWORD INDEX_INCR DIRECT
                /end RECORD_LAYOUT
                /begin RECORD_LAYOUT record_layout2
                /end RECORD_LAYOUT
                /begin AXIS_PTS axispts1 "" 0x1234 NO_INPUT_QUANTITY record_layout1 0 compu_method 3 0.0 10.0
                /end AXIS_PTS
                /begin AXIS_PTS axispts2 "" 0x1234 NO_INPUT_QUANTITY record_layout2 0 compu_method 3 0.0 10.0
                /end AXIS_PTS
                /begin CHARACTERISTIC characteristic1 "" CURVE 0x1234 record_layout1 0 compu_method 0.0 1.0
                /end CHARACTERISTIC
                /begin CHARACTERISTIC characteristic2 "" CURVE 0x1234 record_layout2 0 compu_method 0.0 1.0
                /end CHARACTERISTIC
                /begin TYPEDEF_AXIS typedef_axis1 "" measurement_name record_layout1 0 compu_method 1 0 100
                /end TYPEDEF_AXIS
                /begin TYPEDEF_AXIS typedef_axis2 "" measurement_name record_layout2 0 compu_method 1 0 100
                /end TYPEDEF_AXIS
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic1 "" VALUE record_layout1 0 compu_method 0 100
                /end TYPEDEF_CHARACTERISTIC
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic2 "" VALUE record_layout2 0 compu_method 0 100
                /end TYPEDEF_CHARACTERISTIC
                /begin MOD_COMMON ""
                    S_REC_LAYOUT record_layout1
                /end MOD_COMMON
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no RECORD_LAYOUT, so it is taken from B
        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, false).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        assert!(a2l_file_a.project.module[0].record_layout.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].record_layout.len(), 1);

        // merging C into B
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].record_layout.len(), 1);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].record_layout.len(), 3);

        let module = &a2l_file_b.project.module[0];

        // check that the references in file C to record_layout1 have been renamed
        let axispts1 = &module.axis_pts[0];
        assert_eq!(axispts1.deposit_record, "record_layout1.MERGE");

        let characteristic1 = &module.characteristic[0];
        assert_eq!(characteristic1.deposit, "record_layout1.MERGE");

        let typedef_axis1 = &module.typedef_axis[0];
        assert_eq!(typedef_axis1.record_layout, "record_layout1.MERGE");

        let typedef_characteristic1 = &module.typedef_characteristic[0];
        assert_eq!(
            typedef_characteristic1.record_layout,
            "record_layout1.MERGE"
        );

        // check that the references in file C to record_layout2 have not been renamed
        let axispts2 = &module.axis_pts[1];
        assert_eq!(axispts2.deposit_record, "record_layout2");

        let characteristic2 = &module.characteristic[1];
        assert_eq!(characteristic2.deposit, "record_layout2");

        let typedef_axis2 = &module.typedef_axis[1];
        assert_eq!(typedef_axis2.record_layout, "record_layout2");

        let typedef_characteristic2 = &module.typedef_characteristic[1];
        assert_eq!(typedef_characteristic2.record_layout, "record_layout2");

        // for coverage - merge with rename, where MOD_COMMON does not exist, or does not have S_REC_LAYOUT
        static FILE_D: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin RECORD_LAYOUT record_layout1
                    AXIS_PTS_X 1 SWORD INDEX_INCR DIRECT
                /end RECORD_LAYOUT
            /end MODULE
        /end PROJECT"#;
        static FILE_E: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin RECORD_LAYOUT record_layout1
                    AXIS_PTS_X 1 SWORD INDEX_INCR DIRECT
                /end RECORD_LAYOUT
                /begin MOD_COMMON ""
                /end MOD_COMMON
            /end MODULE
        /end PROJECT"#;
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_d, _) = load_from_string(FILE_D, None, false).unwrap();
        a2l_file_b.merge_modules(&mut a2l_file_d);
        assert_eq!(a2l_file_b.project.module[0].record_layout.len(), 2);

        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, false).unwrap();
        let (mut a2l_file_e, _) = load_from_string(FILE_E, None, false).unwrap();
        a2l_file_b.merge_modules(&mut a2l_file_e);
        assert_eq!(a2l_file_b.project.module[0].record_layout.len(), 2);
    }

    #[test]
    fn merge_group() {
        static FILE_A: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "" FLOAT32_IEEE NO_COMPU_METHOD 1 1.0 0 100
            /end MEASUREMENT
            /begin GROUP g1 ""
                /begin REF_MEASUREMENT m1 /end REF_MEASUREMENT
            /end GROUP
            /begin GROUP g2 ""
            /end GROUP
            /begin GROUP g3 ""
            /end GROUP
            /begin GROUP g4 ""
            /end GROUP
            /begin GROUP g5 ""
                /begin FUNCTION_LIST func1
                /end FUNCTION_LIST
                /begin REF_CHARACTERISTIC ch1
                /end REF_CHARACTERISTIC
                /begin REF_MEASUREMENT meas1
                /end REF_MEASUREMENT
                /begin SUB_GROUP group1
                /end SUB_GROUP
            /end GROUP
        /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "different content" FLOAT32_IEEE NO_COMPU_METHOD 1 2.0 0 200
            /end MEASUREMENT
            /begin GROUP g1 ""
                /begin REF_MEASUREMENT m1 /end REF_MEASUREMENT
            /end GROUP
            /begin GROUP g2 ""
            /end GROUP
            /begin GROUP g3 "foo"
            /end GROUP
            /begin GROUP g4 ""
                /begin FUNCTION_LIST
                /end FUNCTION_LIST
                /begin REF_CHARACTERISTIC
                /end REF_CHARACTERISTIC
                /begin REF_MEASUREMENT
                /end REF_MEASUREMENT
                /begin SUB_GROUP
                /end SUB_GROUP
            /end GROUP
            /begin GROUP g5 ""
                /begin FUNCTION_LIST func1 func2
                /end FUNCTION_LIST
                /begin REF_CHARACTERISTIC ch1 ch2
                /end REF_CHARACTERISTIC
                /begin REF_MEASUREMENT meas1 meas2
                /end REF_MEASUREMENT
                /begin SUB_GROUP group1 group2
                /end SUB_GROUP
            /end GROUP
        /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();

        a2l_file_a.merge_modules(&mut a2l_file_b);

        // the MEASUREMENT m1 in B is renamed to m1.MERGE, because it is not identical to the one in A
        assert_eq!(a2l_file_a.project.module[0].measurement.len(), 2);

        // the group g1 is initially identical in both files, but the MEASUREMENT reference is renamed
        // to m1.MERGE. The group is merged, because GROUPS are merged based on the name only
        let group = &a2l_file_a.project.module[0].group[0];
        assert_eq!(group.name, "g1");
        let meas = group.ref_measurement.as_ref().unwrap();
        assert_eq!(meas.identifier_list[0], "m1");
        assert_eq!(meas.identifier_list[1], "m1.MERGE");
        assert_eq!(meas.identifier_list.len(), 2);

        // group g2 is identical in both files. There is nothing to merge or rename
        let group = &a2l_file_a.project.module[0].group[1];
        assert_eq!(group.name, "g2");

        // group g3 is not identical, due to the differnent long identifier, but it is not renamed
        let group = &a2l_file_a.project.module[0].group[2];
        assert_eq!(group.name, "g3");

        // group g4 is not identical, due to the differnent long identifier; the content is merged
        let group = &a2l_file_a.project.module[0].group[3];
        assert_eq!(group.name, "g4");
        assert!(group.function_list.is_some());
        assert!(group.ref_characteristic.is_some());
        assert!(group.ref_measurement.is_some());
        assert!(group.sub_group.is_some());

        // group g5 is not identical, due to the differnent long identifier; the content is merged
        let group = &a2l_file_a.project.module[0].group[4];
        assert_eq!(group.name, "g5");
        let func_list = group.function_list.as_ref().unwrap();
        assert_eq!(func_list.name_list.len(), 2);
        let ref_chr_list = group.ref_characteristic.as_ref().unwrap();
        assert_eq!(ref_chr_list.identifier_list.len(), 2);
        let ref_meas_list = group.ref_measurement.as_ref().unwrap();
        assert_eq!(ref_meas_list.identifier_list.len(), 2);
        assert_eq!(group.sub_group.as_ref().unwrap().identifier_list.len(), 2);
    }

    #[test]
    fn merge_function() {
        static FILE_A: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "" FLOAT32_IEEE NO_COMPU_METHOD 1 1.0 0 100
            /end MEASUREMENT
            /begin CHARACTERISTIC c "" VALUE 0x1234 deposit_ident 0 NO_COMPU_METHOD 0.0 10.0
            /end CHARACTERISTIC
            /begin FUNCTION f ""
                /begin IN_MEASUREMENT m1 /end IN_MEASUREMENT
                /begin LOC_MEASUREMENT m1 /end LOC_MEASUREMENT
                /begin OUT_MEASUREMENT m1 /end OUT_MEASUREMENT
                /begin REF_CHARACTERISTIC c /end REF_CHARACTERISTIC
                /begin SUB_FUNCTION f2
                /end SUB_FUNCTION
            /end FUNCTION
            /begin FUNCTION f2 ""
            /end FUNCTION
            /begin FUNCTION f3 ""
            /end FUNCTION
        /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "" FLOAT32_IEEE NO_COMPU_METHOD 1 2.0 0 200
            /end MEASUREMENT
            /begin CHARACTERISTIC c "" VALUE 0x1234 deposit_ident 0 NO_COMPU_METHOD 0.0 10.0
            /end CHARACTERISTIC
            /begin FUNCTION f ""
                /begin IN_MEASUREMENT m1 /end IN_MEASUREMENT
                /begin LOC_MEASUREMENT m1 /end LOC_MEASUREMENT
                /begin OUT_MEASUREMENT m1 /end OUT_MEASUREMENT
                /begin REF_CHARACTERISTIC c /end REF_CHARACTERISTIC
                /begin SUB_FUNCTION f2 other
                /end SUB_FUNCTION
            /end FUNCTION
            /begin FUNCTION f2 ""
                /begin IN_MEASUREMENT m1 /end IN_MEASUREMENT
                /begin LOC_MEASUREMENT m1 /end LOC_MEASUREMENT
                /begin OUT_MEASUREMENT m1 /end OUT_MEASUREMENT
                /begin REF_CHARACTERISTIC c /end REF_CHARACTERISTIC
                /begin SUB_FUNCTION sub_func2
                /end SUB_FUNCTION
            /end FUNCTION
            /begin FUNCTION f3 "foo"
            /end FUNCTION
        /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();

        a2l_file_a.merge_modules(&mut a2l_file_b);

        // the MEASUREMENT m1 in B is renamed to m1.MERGE, because it is not identical to the one in A
        assert_eq!(a2l_file_a.project.module[0].measurement.len(), 2);

        // the CHARACTERISTIC c is identical in both files, so it is not renamed
        assert_eq!(a2l_file_a.project.module[0].characteristic.len(), 1);

        assert_eq!(a2l_file_a.project.module[0].function.len(), 3);
        let function = &a2l_file_a.project.module[0].function[0];
        assert_eq!(function.name, "f");

        // the function f is initially identical in both files, but the MEASUREMENT references are renamed
        // to m1.MERGE. The function is merged, because FUNCTIONS are merged based on the name only
        let in_meas = function.in_measurement.as_ref().unwrap();
        assert_eq!(in_meas.identifier_list[0], "m1");
        assert_eq!(in_meas.identifier_list[1], "m1.MERGE");
        assert_eq!(in_meas.identifier_list.len(), 2);
        let loc_meas = function.loc_measurement.as_ref().unwrap();
        assert_eq!(loc_meas.identifier_list[0], "m1");
        assert_eq!(loc_meas.identifier_list[1], "m1.MERGE");
        assert_eq!(loc_meas.identifier_list.len(), 2);
        assert_eq!(
            function.out_measurement.as_ref().unwrap().identifier_list[0],
            "m1"
        );
        let out_meas = function.out_measurement.as_ref().unwrap();
        assert_eq!(out_meas.identifier_list[1], "m1.MERGE");
        assert_eq!(out_meas.identifier_list.len(), 2);
        let ref_meas = function.ref_characteristic.as_ref().unwrap();
        assert_eq!(ref_meas.identifier_list[0], "c");
        assert_eq!(ref_meas.identifier_list.len(), 1);

        let sub_func = &function.sub_function.as_ref().unwrap();
        assert_eq!(sub_func.identifier_list.len(), 2);
        assert_eq!(sub_func.identifier_list[0], "f2");
        assert_eq!(sub_func.identifier_list[1], "other");

        // function f2 was not renamed
        let function = &a2l_file_a.project.module[0].function[1];
        assert_eq!(function.name, "f2");
        assert!(function.sub_function.is_some());

        // function f3 was not renamed
        let function = &a2l_file_a.project.module[0].function[2];
        assert_eq!(function.name, "f3");
        assert!(function.sub_function.is_none());
    }

    #[test]
    fn test_merge_module_ifdata() {
        static FILE_A: &str =
            r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin IF_DATA foo
            /end IF_DATA
        /end MODULE /end PROJECT"#;
        static FILE_C: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin IF_DATA bar
            /end IF_DATA
            /begin IF_DATA baz
            /end IF_DATA
        /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        a2l_file_a.merge_modules(&mut a2l_file_b);
        // since file A has no IF_DATA, it is taken from B
        assert_eq!(a2l_file_a.project.module[0].if_data.len(), 1);

        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, true).unwrap();
        a2l_file_b.merge_modules(&mut a2l_file_c);
        // file B already has IF_DATA, so the ones from C are ignored
        assert_eq!(a2l_file_b.project.module[0].if_data.len(), 1);
    }

    #[test]
    fn test_merge_frame() {
        static FILE_A: &str =
            r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin FRAME frame1 "" 1 2
            /end FRAME
        /end MODULE /end PROJECT"#;
        static FILE_C: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin FRAME frame1 "" 2 2
            /end FRAME
            /begin FRAME frame2 "" 2 3
            /end FRAME
        /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        a2l_file_a.merge_modules(&mut a2l_file_b);
        // since file A has no FRAME, it is taken from B
        assert_eq!(a2l_file_a.project.module[0].frame.len(), 1);

        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, true).unwrap();
        a2l_file_b.merge_modules(&mut a2l_file_c);
        // frames from B and C are merged.
        // frame1 is present in both, but with different content -> frame1 is renamed
        // frame2 is only present in C, and is added to B
        assert_eq!(a2l_file_b.project.module[0].frame.len(), 3);
    }

    #[test]
    fn test_merge_transformer() {
        static FILE_A: &str =
            r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TRANSFORMER transformer1 "version string" "dll32" "dll64" 1 ON_CHANGE NO_INVERSE_TRANSFORMER
            /end TRANSFORMER
        /end MODULE /end PROJECT"#;
        static FILE_C: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TRANSFORMER transformer1 "other version" "dll32" "dll64" 1 ON_CHANGE transformer2
            /end TRANSFORMER
            /begin TRANSFORMER transformer2 "version string" "dll32" "dll64" 1 ON_CHANGE transformer1
            /end TRANSFORMER
        /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        a2l_file_a.merge_modules(&mut a2l_file_b);
        // since file A has no TRANSFORMER, it is taken from B
        assert_eq!(a2l_file_a.project.module[0].transformer.len(), 1);

        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, true).unwrap();
        a2l_file_b.merge_modules(&mut a2l_file_c);
        // transformers from B and C are merged.
        // transformer1 is present in both, but with different content -> transformer1 is renamed
        // transformer2 is only present in C, and is added to B
        assert_eq!(a2l_file_b.project.module[0].transformer.len(), 3);

        let transformer1 = &a2l_file_b.project.module[0].transformer[0];
        assert_eq!(transformer1.name, "transformer1");

        let transformer1_merged = &a2l_file_b.project.module[0].transformer[1];
        assert_eq!(transformer1_merged.name, "transformer1.MERGE");

        let transformer2 = &a2l_file_b.project.module[0].transformer[2];
        assert_eq!(transformer2.name, "transformer2");
    }

    #[test]
    fn test_merge_user_rights() {
        static FILE_A: &str =
            r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin USER_RIGHTS user1
            /end USER_RIGHTS
        /end MODULE /end PROJECT"#;
        static FILE_C: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin USER_RIGHTS user1
                READ_ONLY
            /end USER_RIGHTS
            /begin USER_RIGHTS user2
            /end USER_RIGHTS
        /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        a2l_file_a.merge_modules(&mut a2l_file_b);
        // since file A has no USER_RIGHTS, it is taken from B
        assert_eq!(a2l_file_a.project.module[0].user_rights.len(), 1);

        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        let (mut a2l_file_c, _) = load_from_string(FILE_C, None, true).unwrap();
        a2l_file_b.merge_modules(&mut a2l_file_c);
        // user_rights from B and C are merged.
        // user1 is already present in B, so the new copy from C is ignored
        // user2 is only present in C, and is added to B
        assert_eq!(a2l_file_b.project.module[0].user_rights.len(), 2);
        let user_rights1 = &a2l_file_b.project.module[0].user_rights[0];
        assert_eq!(user_rights1.user_level_id, "user1");
        assert!(user_rights1.read_only.is_none());

        let user_rights2 = &a2l_file_b.project.module[0].user_rights[1];
        assert_eq!(user_rights2.user_level_id, "user2");
    }

    #[test]
    fn test_make_unique_name() {
        static FILE_A: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "" FLOAT32_IEEE NO_COMPU_METHOD 1 1.0 0 100
            /end MEASUREMENT
            /begin MEASUREMENT m1.MERGE "" FLOAT32_IEEE NO_COMPU_METHOD 1 1.0 0 100
            /end MEASUREMENT
        /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "" FLOAT32_IEEE NO_COMPU_METHOD 1 2.0 0 200
            /end MEASUREMENT
        /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        a2l_file_a.merge_modules(&mut a2l_file_b);

        // the MEASUREMENT m1 in B is renamed, because it is not identical to the one in A
        // the name m1.MERGE is already taken, so the name is changed to m1.MERGE2
        assert_eq!(a2l_file_a.project.module[0].measurement.len(), 3);
        let measurement = &a2l_file_a.project.module[0].measurement[2];
        assert_eq!(measurement.name, "m1.MERGE2");
    }

    #[test]
    fn test_import_new() {
        static FILE_A: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "" FLOAT32_IEEE NO_COMPU_METHOD 1 1.0 0 100
            /end MEASUREMENT
            /begin VARIANT_CODING
            /end VARIANT_CODING
        /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "text" SWORD NO_COMPU_METHOD 1 2.0 0 200
            /end MEASUREMENT
            /begin MEASUREMENT m2 "" UBYTE NO_COMPU_METHOD 1 0 0 100
            /end MEASUREMENT
            /begin GROUP grp ""
            /end GROUP
            /begin VARIANT_CODING
            /end VARIANT_CODING
        /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        super::import_new_module_items(
            &mut a2l_file_a.project.module[0],
            &mut a2l_file_b.project.module[0],
        );

        assert_eq!(a2l_file_a.project.module[0].measurement.len(), 2);
        // m1 was not replaced, because it already exists in A
        let meas1 = a2l_file_a.project.module[0].measurement.get("m1").unwrap();
        assert_eq!(meas1.datatype, DataType::Float32Ieee);
        assert_eq!(meas1.long_identifier, "");
        // m2 was imported
        let meas2 = a2l_file_a.project.module[0].measurement.get("m2").unwrap();
        assert_eq!(meas2.datatype, DataType::Ubyte);
        assert_eq!(meas2.long_identifier, "");
    }

    #[test]
    fn test_import_all() {
        static FILE_A: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "" FLOAT32_IEEE NO_COMPU_METHOD 1 1.0 0 100
            /end MEASUREMENT
            /begin MEASUREMENT m3 "third" ULONG NO_COMPU_METHOD 1 1.0 0 10000
            /end MEASUREMENT
            /begin CHARACTERISTIC c1 "" VALUE 0 deposit_ident 0 NO_COMPU_METHOD 0.0 10.0
            /end CHARACTERISTIC
            /begin USER_RIGHTS user_a
            /end USER_RIGHTS
            /begin GROUP grp ""
                /begin FUNCTION_LIST
                /end FUNCTION_LIST
                /begin REF_CHARACTERISTIC c1
                /end REF_CHARACTERISTIC
                /begin REF_MEASUREMENT m1 m3
                /end REF_MEASUREMENT
                /begin SUB_GROUP
                /end SUB_GROUP
            /end GROUP
            /begin VARIANT_CODING
            /end VARIANT_CODING
        /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m1 "text" SWORD NO_COMPU_METHOD 1 2.0 0 200
            /end MEASUREMENT
            /begin MEASUREMENT m2 "second" UBYTE NO_COMPU_METHOD 1 0 0 100
            /end MEASUREMENT
            /begin CHARACTERISTIC c2 "" VALUE 0 deposit_ident 0 NO_COMPU_METHOD 0.0 10.0
            /end CHARACTERISTIC
            /begin USER_RIGHTS user_b
            /end USER_RIGHTS
            /begin GROUP grp ""
                /begin FUNCTION_LIST
                /end FUNCTION_LIST
                /begin REF_CHARACTERISTIC c2
                /end REF_CHARACTERISTIC
                /begin REF_MEASUREMENT m1 m2
                /end REF_MEASUREMENT
                /begin SUB_GROUP
                /end SUB_GROUP
            /end GROUP
            /begin VARIANT_CODING
            /end VARIANT_CODING
        /end MODULE /end PROJECT"#;

        let (mut a2l_file_a, _) = load_from_string(FILE_A, None, true).unwrap();
        let (mut a2l_file_b, _) = load_from_string(FILE_B, None, true).unwrap();
        super::import_all_module_items(
            &mut a2l_file_a.project.module[0],
            &mut a2l_file_b.project.module[0],
        );

        // m1 is taken from B, because import_all replaces existing items
        // m2 is imported from B
        // m3 is kept from A
        assert_eq!(a2l_file_a.project.module[0].measurement.len(), 3);
        let meas1 = a2l_file_a.project.module[0].measurement.get("m1").unwrap();
        assert_eq!(meas1.datatype, DataType::Sword);
        assert_eq!(meas1.long_identifier, "text");
        let meas1_merge = a2l_file_a.project.module[0].measurement.get("m2").unwrap();
        assert_eq!(meas1_merge.datatype, DataType::Ubyte);
        assert_eq!(meas1_merge.long_identifier, "second");
        let meas2 = a2l_file_a.project.module[0].measurement.get("m3").unwrap();
        assert_eq!(meas2.datatype, DataType::Ulong);
        assert_eq!(meas2.long_identifier, "third");

        let group = a2l_file_a.project.module[0].group.get("grp").unwrap();
        let ref_meas = group.ref_measurement.as_ref().unwrap();
        assert_eq!(ref_meas.identifier_list.len(), 3);
        assert!(ref_meas.identifier_list.contains(&"m1".to_string()));
        assert!(ref_meas.identifier_list.contains(&"m2".to_string()));
        assert!(ref_meas.identifier_list.contains(&"m3".to_string()));
    }
}
