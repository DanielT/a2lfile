use crate::namemap::*;
use crate::specification::{A2lObject, Module};
use std::collections::HashMap;
use std::collections::HashSet;

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

    // merge TYPEDEF_* - no dependencies
    merge_typedef(orig_module, merge_module);

    // merge AXIS_PTS, CHARACTERISTIC, MEASUREMENT, INSTANCE, BLOB and FUNCTION
    // depends on each other, as well as COMPU_METHOD, TYPEDEF_* and MOD_COMMON.MEMORY_SEGMENT
    merge_objects(orig_module, merge_module);

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
    if merge_module.a2ml.is_some() && orig_module.a2ml.is_none() {
        if let Some(mut a2ml) = std::mem::take(&mut merge_module.a2ml) {
            a2ml.reset_location();
            orig_module.a2ml = Some(a2ml);
        }
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
    let merge_memory_layout = &mut merge_module.mod_par.as_mut().unwrap().memory_layout;

    while let Some(mut merge_ml) = merge_memory_layout.pop() {
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
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_memory_segment(orig_module, &mut log_msgs);
    let merge_map = build_namemap_memory_segment(merge_module, &mut log_msgs);
    let (merge_action, rename_table) = calculate_item_actions(&orig_map, &merge_map);

    rename_memory_segments(merge_module, &rename_table);

    let orig_mod_par = orig_module.mod_par.as_mut().unwrap();
    let merge_mod_par = merge_module.mod_par.as_mut().unwrap();
    while let Some(mut memory_segment) = merge_mod_par.memory_segment.pop() {
        if let Some(true) = merge_action.get(&memory_segment.name) {
            memory_segment.reset_location();
            orig_mod_par.memory_segment.push(memory_segment);
        }
    }
}

fn rename_memory_segments(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    let mod_par = merge_module.mod_par.as_mut().unwrap();

    for memory_segment in &mut mod_par.memory_segment {
        if let Some(newname) = rename_table.get(&memory_segment.name) {
            memory_segment.name = newname.to_owned();
        }
    }

    for axis_pts in &mut merge_module.axis_pts {
        if let Some(ref_memory_segment) = &mut axis_pts.ref_memory_segment {
            if let Some(newname) = rename_table.get(&ref_memory_segment.name) {
                ref_memory_segment.name = newname.to_owned();
            }
        }
    }

    for characteristic in &mut merge_module.characteristic {
        if let Some(ref_memory_segment) = &mut characteristic.ref_memory_segment {
            if let Some(newname) = rename_table.get(&ref_memory_segment.name) {
                ref_memory_segment.name = newname.to_owned();
            }
        }
    }

    for measurement in &mut merge_module.measurement {
        if let Some(ref_memory_segment) = &mut measurement.ref_memory_segment {
            if let Some(newname) = rename_table.get(&ref_memory_segment.name) {
                ref_memory_segment.name = newname.to_owned();
            }
        }
    }
}

// ------------------------ SYSTEM_CONSTANT ------------------------

fn merge_system_constant(orig_module: &mut Module, merge_module: &mut Module) {
    let orig_system_constant = &mut orig_module.mod_par.as_mut().unwrap().system_constant;
    let merge_system_constant = &mut merge_module.mod_par.as_mut().unwrap().system_constant;

    let orig_sc_names: HashSet<String> = orig_system_constant
        .iter()
        .map(|sc| sc.name.clone())
        .collect();

    while let Some(mut merge_sysconst) = merge_system_constant.pop() {
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
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_unit(orig_module, &mut log_msgs);
    let merge_map = build_namemap_unit(merge_module, &mut log_msgs);
    let (merge_action, rename_table) = calculate_item_actions(&orig_map, &merge_map);

    rename_units(merge_module, &rename_table);

    while let Some(mut unit) = merge_module.unit.pop() {
        if let Some(true) = merge_action.get(&unit.name) {
            unit.reset_location();
            orig_module.unit.push(unit);
        }
    }
}

fn rename_units(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for unit in &mut merge_module.unit {
        if let Some(newname) = rename_table.get(&unit.name) {
            unit.name = newname.to_owned();
        }
    }

    for compu_method in &mut merge_module.compu_method {
        if let Some(ref_unit) = &mut compu_method.ref_unit {
            if let Some(newname) = rename_table.get(&ref_unit.unit) {
                ref_unit.unit = newname.to_owned();
            }
        }
    }
}

// ------------------------ COMPU_TAB / COMPU_VTAB / COMPU_VTAB_RANGE ------------------------

fn merge_compu_tab(orig_module: &mut Module, merge_module: &mut Module) {
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_compu_tab(orig_module, &mut log_msgs);
    let merge_map = build_namemap_compu_tab(merge_module, &mut log_msgs);
    let (merge_action, rename_table) = calculate_item_actions(&orig_map, &merge_map);

    rename_compu_tabs(merge_module, &rename_table);

    while let Some(mut compu_tab) = merge_module.compu_tab.pop() {
        if let Some(true) = merge_action.get(&compu_tab.name) {
            compu_tab.reset_location();
            orig_module.compu_tab.push(compu_tab);
        }
    }
    while let Some(mut compu_vtab) = merge_module.compu_vtab.pop() {
        if let Some(true) = merge_action.get(&compu_vtab.name) {
            compu_vtab.reset_location();
            orig_module.compu_vtab.push(compu_vtab);
        }
    }
    while let Some(mut compu_vtab_range) = merge_module.compu_vtab_range.pop() {
        if let Some(true) = merge_action.get(&compu_vtab_range.name) {
            compu_vtab_range.reset_location();
            orig_module.compu_vtab_range.push(compu_vtab_range);
        }
    }
}

fn rename_compu_tabs(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for compu_tab in &mut merge_module.compu_tab {
        if let Some(newname) = rename_table.get(&compu_tab.name) {
            compu_tab.name = newname.to_owned();
        }
    }
    for compu_vtab in &mut merge_module.compu_vtab {
        if let Some(newname) = rename_table.get(&compu_vtab.name) {
            compu_vtab.name = newname.to_owned();
        }
    }
    for compu_vtab_range in &mut merge_module.compu_vtab_range {
        if let Some(newname) = rename_table.get(&compu_vtab_range.name) {
            compu_vtab_range.name = newname.to_owned();
        }
    }

    // COMPU_METHODs can refer to any of COMPU_TAB / COMPU_VTAB / COMPU_VTAB_RANGE via a COMPU_TAB_REF
    for compu_method in &mut merge_module.compu_method {
        if let Some(compu_tab_ref) = &mut compu_method.compu_tab_ref {
            if let Some(newname) = rename_table.get(&compu_tab_ref.conversion_table) {
                compu_tab_ref.conversion_table = newname.to_owned();
            }
        }
        if let Some(status_string_ref) = &mut compu_method.status_string_ref {
            if let Some(newname) = rename_table.get(&status_string_ref.conversion_table) {
                status_string_ref.conversion_table = newname.to_owned();
            }
        }
    }
}

// ------------------------ COMPU_METHOD ------------------------

fn merge_compu_method(orig_module: &mut Module, merge_module: &mut Module) {
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_compu_method(orig_module, &mut log_msgs);
    let merge_map = build_namemap_compu_method(merge_module, &mut log_msgs);
    let (merge_action, rename_table) = calculate_item_actions(&orig_map, &merge_map);

    rename_compu_methods(merge_module, &rename_table);

    while let Some(mut compu_method) = merge_module.compu_method.pop() {
        if let Some(true) = merge_action.get(&compu_method.name) {
            compu_method.reset_location();
            orig_module.compu_method.push(compu_method);
        }
    }
}

fn rename_compu_methods(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for compu_method in &mut merge_module.compu_method {
        if let Some(newname) = rename_table.get(&compu_method.name) {
            compu_method.name = newname.to_owned();
        }
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
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_record_layout(orig_module, &mut log_msgs);
    let merge_map = build_namemap_record_layout(merge_module, &mut log_msgs);
    let (merge_action, rename_table) = calculate_item_actions(&orig_map, &merge_map);

    rename_record_layouts(merge_module, &rename_table);

    while let Some(mut record_layout) = merge_module.record_layout.pop() {
        if let Some(true) = merge_action.get(&record_layout.name) {
            record_layout.reset_location();
            orig_module.record_layout.push(record_layout);
        }
    }
}

fn rename_record_layouts(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for record_layout in &mut merge_module.record_layout {
        if let Some(newname) = rename_table.get(&record_layout.name) {
            record_layout.name = newname.to_owned();
        }
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

    if let Some(mod_common) = &mut merge_module.mod_common {
        if let Some(s_rec_layout) = &mut mod_common.s_rec_layout {
            if let Some(newname) = rename_table.get(&s_rec_layout.name) {
                s_rec_layout.name = newname.to_owned();
            }
        }
    }
}

// ------------------------ MOD_COMMON ------------------------

fn merge_mod_common(orig_module: &mut Module, merge_module: &mut Module) {
    if merge_module.mod_common.is_some() && orig_module.mod_common.is_none() {
        if let Some(mut mod_common) = std::mem::take(&mut merge_module.mod_common) {
            mod_common.reset_location();
            orig_module.mod_common = Some(mod_common);
        }
    }
}

// ------------------------ AXIS_PTS, CHARACTERISTIC, MEASUREMENT, INSTANCE, BLOB ------------------------

fn merge_objects(orig_module: &mut Module, merge_module: &mut Module) {
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_object(orig_module, &mut log_msgs);
    let merge_map = build_namemap_object(merge_module, &mut log_msgs);
    let (object_merge_action, object_rename_table) = calculate_item_actions(&orig_map, &merge_map);
    let orig_map = build_namemap_function(orig_module, &mut log_msgs);
    let merge_map = build_namemap_function(merge_module, &mut log_msgs);
    let (function_merge_action, function_rename_table) =
        calculate_item_actions(&orig_map, &merge_map);

    rename_objects(merge_module, &object_rename_table);
    rename_functions(merge_module, &function_rename_table);

    while let Some(mut axis_pts) = merge_module.axis_pts.pop() {
        if let Some(true) = object_merge_action.get(&axis_pts.name) {
            axis_pts.reset_location();
            orig_module.axis_pts.push(axis_pts);
        }
    }
    while let Some(mut blob) = merge_module.blob.pop() {
        if let Some(true) = object_merge_action.get(&blob.name) {
            blob.reset_location();
            orig_module.blob.push(blob);
        }
    }
    while let Some(mut characteristic) = merge_module.characteristic.pop() {
        if let Some(true) = object_merge_action.get(&characteristic.name) {
            characteristic.reset_location();
            orig_module.characteristic.push(characteristic);
        }
    }
    while let Some(mut instance) = merge_module.instance.pop() {
        if let Some(true) = object_merge_action.get(&instance.name) {
            instance.reset_location();
            orig_module.instance.push(instance);
        }
    }
    while let Some(mut measurement) = merge_module.measurement.pop() {
        if let Some(true) = object_merge_action.get(&measurement.name) {
            measurement.reset_location();
            orig_module.measurement.push(measurement);
        }
    }

    while let Some(mut function) = merge_module.function.pop() {
        if let Some(true) = function_merge_action.get(&function.name) {
            function.reset_location();
            orig_module.function.push(function);
        }
    }
}

fn rename_objects(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    // MODULE.AXIS_PTS
    for axis_pts in &mut merge_module.axis_pts {
        // MODULE.AXIS_PTS.name
        if let Some(newname) = rename_table.get(&axis_pts.name) {
            axis_pts.name = newname.to_owned();
        }
        // MODULE.AXIS_PTS.input_quantity
        if let Some(newname) = rename_table.get(&axis_pts.input_quantity) {
            axis_pts.input_quantity = newname.to_owned();
        }
    }
    // MODULE.BLOB
    for blob in &mut merge_module.blob {
        // MODULE.BLOB.name
        if let Some(newname) = rename_table.get(&blob.name) {
            blob.name = newname.to_owned();
        }
    }
    // MODULE.CHARACTERISTIC
    for characteristic in &mut merge_module.characteristic {
        // MODULE.CHARACTERISTIC.name
        if let Some(newname) = rename_table.get(&characteristic.name) {
            characteristic.name = newname.to_owned();
        }
        // MODULE.CHARACTERISTIC.AXIS_DESCR
        for axis_descr in &mut characteristic.axis_descr {
            // MODULE.CHARACTERISTIC.AXIS_DESCR.input_quantity
            if let Some(newname) = rename_table.get(&axis_descr.input_quantity) {
                axis_descr.input_quantity = newname.to_owned();
            }
            // MODULE.CHARACTERISTIC.AXIS_DESCR.AXIS_PTS_REF
            if let Some(axis_pts_ref) = &mut axis_descr.axis_pts_ref {
                if let Some(newname) = rename_table.get(&axis_pts_ref.axis_points) {
                    axis_pts_ref.axis_points = newname.to_owned();
                }
            }
            // MODULE.CHARACTERISTIC.AXIS_DESCR.CURVE_AXIS_REF
            if let Some(curve_axis_ref) = &mut axis_descr.curve_axis_ref {
                if let Some(newname) = rename_table.get(&curve_axis_ref.curve_axis) {
                    curve_axis_ref.curve_axis = newname.to_owned();
                }
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
            if let Some(axis_pts_ref) = &mut axis_descr.axis_pts_ref {
                if let Some(newname) = rename_table.get(&axis_pts_ref.axis_points) {
                    axis_pts_ref.axis_points = newname.to_owned();
                }
            }
            // MODULE.TYPEDEF_CHARACTERISTIC.AXIS_DESCR.CURVE_AXIS_REF
            if let Some(curve_axis_ref) = &mut axis_descr.curve_axis_ref {
                if let Some(newname) = rename_table.get(&curve_axis_ref.curve_axis) {
                    curve_axis_ref.curve_axis = newname.to_owned();
                }
            }
        }
    }
    // MODULE.INSTANCE
    for instance in &mut merge_module.instance {
        if let Some(newname) = rename_table.get(&instance.name) {
            instance.name = newname.to_owned();
        }
    }
    // MODULE.MEASUREMENT
    for measurement in &mut merge_module.measurement {
        if let Some(newname) = rename_table.get(&measurement.name) {
            measurement.name = newname.to_owned();
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
            if let Some(var_measurement) = &mut var_criterion.var_measurement {
                if let Some(newname) = rename_table.get(&var_measurement.name) {
                    var_measurement.name = newname.to_owned();
                }
            }
            // MODULE.VARIANT_CODING.VAR_CRITERION.VAR_SELECTION
            if let Some(var_selection_characteristic) =
                &mut var_criterion.var_selection_characteristic
            {
                if let Some(newname) = rename_table.get(&var_selection_characteristic.name) {
                    var_selection_characteristic.name = newname.to_owned();
                }
            }
        }
    }
}

fn rename_functions(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    for function in &mut merge_module.function {
        if let Some(newname) = rename_table.get(&function.name) {
            function.name = newname.to_owned();
        }
        if let Some(sub_function) = &mut function.sub_function {
            rename_item_list(&mut sub_function.identifier_list, rename_table);
        }
    }

    for axis_pts in &mut merge_module.axis_pts {
        if let Some(function_list) = &mut axis_pts.function_list {
            rename_item_list(&mut function_list.name_list, rename_table);
        }
    }
    for characteristic in &mut merge_module.characteristic {
        if let Some(function_list) = &mut characteristic.function_list {
            rename_item_list(&mut function_list.name_list, rename_table);
        }
    }
    for group in &mut merge_module.group {
        if let Some(function_list) = &mut group.function_list {
            rename_item_list(&mut function_list.name_list, rename_table);
        }
    }
    for measurement in &mut merge_module.measurement {
        if let Some(function_list) = &mut measurement.function_list {
            rename_item_list(&mut function_list.name_list, rename_table);
        }
    }
}

// ------------------------ GROUP ------------------------

fn merge_group(orig_module: &mut Module, merge_module: &mut Module) {
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_group(orig_module, &mut log_msgs);
    let merge_map = build_namemap_group(merge_module, &mut log_msgs);
    let (merge_action, rename_table) = calculate_item_actions(&orig_map, &merge_map);

    rename_groups(merge_module, &rename_table);

    while let Some(mut group) = merge_module.group.pop() {
        if let Some(true) = merge_action.get(&group.name) {
            group.reset_location();
            orig_module.group.push(group);
        }
    }
}

fn rename_groups(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for group in &mut merge_module.group {
        if let Some(newname) = rename_table.get(&group.name) {
            group.name = newname.to_owned();
        }
        if let Some(sub_group) = &mut group.sub_group {
            rename_item_list(&mut sub_group.identifier_list, rename_table);
        }
    }

    for user_rights in &mut merge_module.user_rights {
        for ref_group in &mut user_rights.ref_group {
            rename_item_list(&mut ref_group.identifier_list, rename_table);
        }
    }
}

// ------------------------ FRAME ------------------------

fn merge_frame(orig_module: &mut Module, merge_module: &mut Module) {
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_frame(orig_module, &mut log_msgs);
    let merge_map = build_namemap_frame(merge_module, &mut log_msgs);
    let (merge_action, rename_table) = calculate_item_actions(&orig_map, &merge_map);

    rename_frames(merge_module, &rename_table);

    while let Some(mut frame) = merge_module.frame.pop() {
        if let Some(true) = merge_action.get(&frame.name) {
            frame.reset_location();
            orig_module.frame.push(frame);
        }
    }
}

fn rename_frames(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for frame in &mut merge_module.frame {
        if let Some(newname) = rename_table.get(&frame.name) {
            frame.name = newname.to_owned();
        }
    }
}

// ------------------------ TRANSFORMER ------------------------

fn merge_transformer(orig_module: &mut Module, merge_module: &mut Module) {
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_transformer(orig_module, &mut log_msgs);
    let merge_map = build_namemap_transformer(merge_module, &mut log_msgs);
    let (merge_action, rename_table) = calculate_item_actions(&orig_map, &merge_map);

    rename_transformers(merge_module, &rename_table);

    while let Some(mut transformer) = merge_module.transformer.pop() {
        if let Some(true) = merge_action.get(&transformer.name) {
            transformer.reset_location();
            orig_module.transformer.push(transformer);
        }
    }
}

fn rename_transformers(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    for transformer in &mut merge_module.transformer {
        if let Some(newname) = rename_table.get(&transformer.name) {
            transformer.name = newname.to_owned();
        }
        if let Some(newname) = rename_table.get(&transformer.inverse_transformer) {
            transformer.inverse_transformer = newname.to_owned();
        }
    }
}

// ------------------------ TYPEDEF_* ------------------------

fn merge_typedef(orig_module: &mut Module, merge_module: &mut Module) {
    let mut log_msgs = Vec::<String>::new();
    let orig_map = build_namemap_typedef(orig_module, &mut log_msgs);
    let merge_map = build_namemap_typedef(merge_module, &mut log_msgs);
    let (merge_action, rename_table) = calculate_item_actions(&orig_map, &merge_map);

    rename_typedefs(merge_module, &rename_table);

    while let Some(mut typedef_axis) = merge_module.typedef_axis.pop() {
        if let Some(true) = merge_action.get(&typedef_axis.name) {
            typedef_axis.reset_location();
            orig_module.typedef_axis.push(typedef_axis);
        }
    }
    while let Some(mut typedef_blob) = merge_module.typedef_blob.pop() {
        if let Some(true) = merge_action.get(&typedef_blob.name) {
            typedef_blob.reset_location();
            orig_module.typedef_blob.push(typedef_blob);
        }
    }
    while let Some(mut typedef_characteristic) = merge_module.typedef_characteristic.pop() {
        if let Some(true) = merge_action.get(&typedef_characteristic.name) {
            typedef_characteristic.reset_location();
            orig_module
                .typedef_characteristic
                .push(typedef_characteristic);
        }
    }
    while let Some(mut typedef_measurement) = merge_module.typedef_measurement.pop() {
        if let Some(true) = merge_action.get(&typedef_measurement.name) {
            typedef_measurement.reset_location();
            orig_module.typedef_measurement.push(typedef_measurement);
        }
    }
    while let Some(mut typedef_structure) = merge_module.typedef_structure.pop() {
        if let Some(true) = merge_action.get(&typedef_structure.name) {
            typedef_structure.reset_location();
            orig_module.typedef_structure.push(typedef_structure);
        }
    }
}

fn rename_typedefs(merge_module: &mut Module, rename_table: &HashMap<String, String>) {
    if rename_table.is_empty() {
        return;
    }

    // MODULE.TYPEDEF_AXIS
    for typedef_axis in &mut merge_module.typedef_axis {
        if let Some(newname) = rename_table.get(&typedef_axis.name) {
            typedef_axis.name = newname.to_owned();
        }
    }
    // MODULE.TYPEDEF_BLOB
    for typedef_blob in &mut merge_module.typedef_blob {
        if let Some(newname) = rename_table.get(&typedef_blob.name) {
            typedef_blob.name = newname.to_owned();
        }
    }
    // MODULE.TYPEDEF_CHARACTERISTIC
    for typedef_characteristic in &mut merge_module.typedef_characteristic {
        if let Some(newname) = rename_table.get(&typedef_characteristic.name) {
            typedef_characteristic.name = newname.to_owned();
        }
    }
    // MODULE.TYPEDEF_MEASUREMENT
    for typedef_measurement in &mut merge_module.typedef_measurement {
        if let Some(newname) = rename_table.get(&typedef_measurement.name) {
            typedef_measurement.name = newname.to_owned();
        }
    }
    // MODULE.TYPEDEF_STRUCTURE
    for typedef_structure in &mut merge_module.typedef_structure {
        if let Some(newname) = rename_table.get(&typedef_structure.name) {
            typedef_structure.name = newname.to_owned();
        }
        // MODULE.TYPEDEF_STRUCTURE.STRUCTURE_COMPONENT
        for structure_component in &mut typedef_structure.structure_component {
            if let Some(newname) = rename_table.get(&structure_component.component_type) {
                structure_component.component_type = newname.to_owned();
            }
        }
    }

    // MODULE.INSTANCE
    for instance in &mut merge_module.instance {
        if let Some(newname) = rename_table.get(&instance.type_ref) {
            instance.type_ref = newname.to_owned();
        }
    }
}

// ------------------------ USER_RIGHTS ------------------------

fn merge_user_rights(orig_module: &mut Module, merge_module: &mut Module) {
    // there is no renaming here; as far as I can tell there is no requirement that there should only be one entry per user id
    while let Some(mut merge_user_rights) = merge_module.user_rights.pop() {
        merge_user_rights.reset_location();
        // don't create any exact duplicates, but copy everything else
        if !orig_module
            .user_rights
            .iter()
            .any(|user_rights| user_rights == &merge_user_rights)
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
    orig_map: &HashMap<String, T>,
    merge_map: &HashMap<String, T>,
) -> (HashMap<String, bool>, HashMap<String, String>)
where
    T: PartialEq,
{
    let mut rename_table = HashMap::<String, String>::new();
    let mut merge_action = HashMap::<String, bool>::new();

    for (name, merge_compu_tab) in merge_map {
        if let Some(orig_compu_tab) = orig_map.get(name) {
            if *orig_compu_tab == *merge_compu_tab {
                // identical items exists on both sides, no need to merge
                merge_action.insert(name.to_owned(), false);
            } else {
                // items with the same name but with different content exist on both sides. Rename the new ones before merging
                let newname = make_unique_name(name, orig_map, merge_map);
                rename_table.insert(name.to_owned(), newname.clone());
                merge_action.insert(newname, true);
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

fn make_unique_name<T>(
    current_name: &str,
    orig_map: &HashMap<String, T>,
    merge_map: &HashMap<String, T>,
) -> String {
    let mut newname = format!("{current_name}.MERGE");
    let mut idx = 1;

    while merge_map.get(&newname).is_some() || orig_map.get(&newname).is_some() {
        idx += 1;
        newname = format!("{current_name}.MERGE{idx}");
    }

    newname
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn test_merge_a2ml() {
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p "" /begin MODULE m "" /begin A2ML block "IF_DATA" struct { int; }; /end A2ML /end MODULE /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p "" /begin MODULE m "" /begin A2ML block "IF_DATA" struct { float; }; /end A2ML /end MODULE /end PROJECT"#;

        let mut log_msgs = vec![];
        let mut a2l_file_a = load_from_string(FILE_A, None, &mut log_msgs, false).unwrap();
        let mut a2l_file_b = load_from_string(FILE_B, None, &mut log_msgs, false).unwrap();

        // merging B into A: A2ML is copied
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert!(a2l_file_a.project.module[0].a2ml.is_some());

        let mut a2l_file_b = load_from_string(FILE_B, None, &mut log_msgs, false).unwrap();
        let mut a2l_file_c = load_from_string(FILE_C, None, &mut log_msgs, false).unwrap();
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
                /begin AXIS_PTS axispts_name "long_identifier" 0x1234 input_qty deposit_record 0 conversion 3 0.0 10.0
                    REF_MEMORY_SEGMENT seg_b
                /end AXIS_PTS
                /begin CHARACTERISTIC characteristic_name "long_identifier" VALUE 0x1234 deposit_ident 0 conversion 0.0 10.0
                    REF_MEMORY_SEGMENT seg_b
                /end CHARACTERISTIC
                /begin MEASUREMENT measurement_name "long_identifier" FLOAT32_IEEE conversion 1 1.0 0 100
                    REF_MEMORY_SEGMENT seg_b
                /end MEASUREMENT
            /end MODULE
        /end PROJECT"#;

        let mut log_msgs = vec![];

        // merging B into A -> A has no MOD_PAR, so it is taken from B
        let mut a2l_file_a = load_from_string(FILE_A, None, &mut log_msgs, false).unwrap();
        let mut a2l_file_b = load_from_string(FILE_B, None, &mut log_msgs, false).unwrap();
        assert!(a2l_file_a.project.module[0].mod_par.is_none());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert!(a2l_file_a.project.module[0].mod_par.is_some());

        // merging C into B
        let mut a2l_file_b = load_from_string(FILE_B, None, &mut log_msgs, false).unwrap();
        let mut a2l_file_c = load_from_string(FILE_C, None, &mut log_msgs, false).unwrap();
        assert!(a2l_file_b.project.module[0].mod_par.is_some());
        assert!(a2l_file_c.project.module[0].mod_par.is_some());
        a2l_file_b.merge_modules(&mut a2l_file_c);
        let mod_par_b = a2l_file_b.project.module[0].mod_par.as_ref().unwrap();
        assert_eq!(mod_par_b.memory_layout.len(), 3);
        assert_eq!(mod_par_b.memory_segment.len(), 4);
        assert_eq!(mod_par_b.system_constant.len(), 3);
        // the references to MEMORY_SGMENT seg_b should be renamed
        assert_ne!(
            a2l_file_b.project.module[0].axis_pts[0]
                .ref_memory_segment
                .as_ref()
                .unwrap()
                .name,
            "seg_b"
        );
        assert_ne!(
            a2l_file_b.project.module[0].characteristic[0]
                .ref_memory_segment
                .as_ref()
                .unwrap()
                .name,
            "seg_b"
        );
        assert_ne!(
            a2l_file_b.project.module[0].measurement[0]
                .ref_memory_segment
                .as_ref()
                .unwrap()
                .name,
            "seg_b"
        );
    }

    #[test]
    fn test_merge_compu_tab() {
        let mut log_msgs = vec![];
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin COMPU_TAB ct1 "" TAB_NOINTP 1
                    1 1
                /end COMPU_TAB
                /begin COMPU_VTAB cvt1 "" TAB_VERB 1
                    1 "v"
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE cvtr1 TAB_VERB 1
                    1 2 "v"
                /end COMPU_VTAB_RANGE
                /begin COMPU_TAB ct2 "" TAB_NOINTP 1
                    1 1
                /end COMPU_TAB
                /begin COMPU_VTAB cvt2 "" TAB_VERB 1
                    1 "v"
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE cvtr2 TAB_VERB 1
                    1 2 "v"
                /end COMPU_VTAB_RANGE
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin COMPU_TAB ct1 "" TAB_NOINTP 1
                    1 1
                /end COMPU_TAB
                /begin COMPU_VTAB cvt1 "" TAB_VERB 1
                    1 "v"
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE cvtr1 TAB_VERB 1
                    1 2 "v"
                /end COMPU_VTAB_RANGE
                /begin COMPU_TAB ct2 "" TAB_NOINTP 1
                    2 2
                /end COMPU_TAB
                /begin COMPU_VTAB cvt2 "" TAB_VERB 1
                    2 "v"
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE cvtr2 TAB_VERB 1
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
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no COMPU_TABs, so they are all taken from B
        let mut a2l_file_a = load_from_string(FILE_A, None, &mut log_msgs, false).unwrap();
        let mut a2l_file_b = load_from_string(FILE_B, None, &mut log_msgs, false).unwrap();
        assert!(a2l_file_a.project.module[0].compu_tab.is_empty());
        assert!(a2l_file_a.project.module[0].compu_vtab.is_empty());
        assert!(a2l_file_a.project.module[0].compu_vtab_range.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].compu_tab.len(), 2);
        assert_eq!(a2l_file_a.project.module[0].compu_vtab.len(), 2);
        assert_eq!(a2l_file_a.project.module[0].compu_vtab_range.len(), 2);

        // merging C into B
        let mut a2l_file_b = load_from_string(FILE_B, None, &mut log_msgs, false).unwrap();
        let mut a2l_file_c = load_from_string(FILE_C, None, &mut log_msgs, false).unwrap();

        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].compu_tab.len(), 3);
        assert_eq!(a2l_file_b.project.module[0].compu_vtab.len(), 3);
        assert_eq!(a2l_file_b.project.module[0].compu_vtab_range.len(), 3);
    }

    #[test]
    fn test_merge_unit() {
        let mut log_msgs = vec![];
        static FILE_A: &str = r#"/begin PROJECT p "" /begin MODULE m "" /end MODULE /end PROJECT"#;
        static FILE_B: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin UNIT unit_name "" "x" DERIVED
                /end UNIT
                /begin UNIT unit_name2 "" "x" DERIVED
                /end UNIT
            /end MODULE
        /end PROJECT"#;
        static FILE_C: &str = r#"/begin PROJECT p ""
            /begin MODULE m ""
                /begin UNIT unit_name "" "x" DERIVED
                /end UNIT
                /begin UNIT unit_name2 "" "xyz" DERIVED
                /end UNIT
                /begin COMPU_METHOD m1 "" TAB_NOINTP "%6.3" ""
                    REF_UNIT unit_name2
                /end COMPU_METHOD
            /end MODULE
        /end PROJECT"#;

        // merging B into A -> A has no UNIT, so it is taken from B
        let mut a2l_file_a = load_from_string(FILE_A, None, &mut log_msgs, false).unwrap();
        let mut a2l_file_b = load_from_string(FILE_B, None, &mut log_msgs, false).unwrap();
        assert!(a2l_file_a.project.module[0].unit.is_empty());
        a2l_file_a.merge_modules(&mut a2l_file_b);
        assert_eq!(a2l_file_a.project.module[0].unit.len(), 2);

        // merging C into B
        let mut a2l_file_b = load_from_string(FILE_B, None, &mut log_msgs, false).unwrap();
        let mut a2l_file_c = load_from_string(FILE_C, None, &mut log_msgs, false).unwrap();
        assert_eq!(a2l_file_b.project.module[0].unit.len(), 2);
        println!("units: {:?}", a2l_file_b.project.module[0].unit);
        a2l_file_b.merge_modules(&mut a2l_file_c);
        assert_eq!(a2l_file_b.project.module[0].unit.len(), 3);
        println!("units: {:?}", a2l_file_b.project.module[0].unit);
        println!("cm: {:?}", a2l_file_b.project.module[0].compu_method);
    }
}
