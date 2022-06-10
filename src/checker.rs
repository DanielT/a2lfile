use crate::namemap::*;
use crate::specification::*;
use std::collections::HashMap;

// check the cross references between various elements
pub fn check(a2l_file: &A2lFile, log_msgs: &mut Vec<String>) {
    for module in &a2l_file.project.module {
        let name_map = ModuleNameMap::build(&a2l_file.project.module[0], log_msgs);

        for axis_pts in &module.axis_pts {
            check_axis_pts(axis_pts, &name_map, log_msgs);
        }

        for characteristic in &module.characteristic {
            check_characteristic(characteristic, &name_map, log_msgs);
        }

        for compu_method in &module.compu_method {
            check_compu_method(compu_method, &name_map, log_msgs);
        }

        for function in &module.function {
            check_function(function, &name_map, log_msgs);
        }

        for group in &module.group {
            check_group(group, &name_map, log_msgs);
        }
        check_group_structure(&module.group, log_msgs);

        for measurement in &module.measurement {
            check_measurement(measurement, &name_map, log_msgs);
        }

        for transformer in &module.transformer {
            check_transformer(transformer, &name_map, log_msgs);
        }
    }
}

fn check_axis_descr(
    parent_name: &str,
    axis_descr: &AxisDescr,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let line = axis_descr.get_line();
    if axis_descr.input_quantity != "NO_INPUT_QUANTITY"
        && name_map.object.get(&axis_descr.input_quantity).is_none()
    {
        log_msgs.push(format!("In AXIS_DESCR of CHARACTERISTIC {} on line {}: Referenced input MEASUREMENT {} does not exist.",
        parent_name, line, axis_descr.input_quantity));
    }

    if axis_descr.conversion != "NO_COMPU_METHOD"
        && name_map.compu_method.get(&axis_descr.conversion).is_none()
    {
        log_msgs.push(format!("In AXIS_DESCR of CHARACTERISTIC {} on line {}: Referenced COMPU_METHOD {} does not exist.",
        parent_name, line, axis_descr.conversion));
    }

    if let Some(axis_pts_ref) = &axis_descr.axis_pts_ref {
        let apr_line = axis_pts_ref.get_line();
        if name_map.object.get(&axis_pts_ref.axis_points).is_none() {
            log_msgs.push(format!("In AXIS_PTS_REF of CHARACTERISTIC {} on line {}: Referenced AXIS_PTS {} does not exist",
            parent_name, apr_line, axis_pts_ref.axis_points));
        }
    }

    if let Some(curve_axis_ref) = &axis_descr.curve_axis_ref {
        let car_line = curve_axis_ref.get_line();
        if name_map.object.get(&curve_axis_ref.curve_axis).is_none() {
            log_msgs.push(format!(
                "In CURVE_AXIS_REF on line {}: Referenced CHARACTERISTIC {} does not exist",
                car_line, curve_axis_ref.curve_axis
            ));
        }
    }
}

fn check_axis_pts(axis_pts: &AxisPts, name_map: &ModuleNameMap, log_msgs: &mut Vec<String>) {
    let name = &axis_pts.name;
    let line = axis_pts.get_line();

    if axis_pts.conversion != "NO_COMPU_METHOD"
        && name_map.compu_method.get(&axis_pts.conversion).is_none()
    {
        log_msgs.push(format!(
            "In AXIS_PTS {} on line {}: Referenced COMPU_METHOD {} does not exist.",
            name, line, axis_pts.conversion
        ));
    }

    check_function_list(&axis_pts.function_list, name_map, log_msgs);
    check_ref_memory_segment(&axis_pts.ref_memory_segment, name_map, log_msgs);
}

fn check_characteristic(
    characteristic: &Characteristic,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let name = &characteristic.name;
    let line = characteristic.get_line();

    if characteristic.conversion != "NO_COMPU_METHOD"
        && name_map
            .compu_method
            .get(&characteristic.conversion)
            .is_none()
    {
        log_msgs.push(format!(
            "In CHARACTERISTIC {} on line {}: Referenced COMPU_METHOD {} does not exist.",
            name, line, characteristic.conversion
        ));
    }

    if name_map
        .record_layout
        .get(&characteristic.deposit)
        .is_none()
    {
        log_msgs.push(format!(
            "In CHARACTERISTIC {} on line {}: Referenced RECORD_LAYOUT {} does not exist.",
            name, line, characteristic.deposit
        ));
    }

    for axis_descr in &characteristic.axis_descr {
        check_axis_descr(name, axis_descr, name_map, log_msgs);
    }

    if let Some(comparison_quantity) = &characteristic.comparison_quantity {
        let cqline = comparison_quantity.get_line();
        if name_map.object.get(&comparison_quantity.name).is_none() {
            log_msgs.push(format!(
                "In COMPARISON_QUANTITY on line {}: Referenced MEASUREMENT {} does not exist",
                cqline, comparison_quantity.name
            ));
        }
    }

    if let Some(dependent_characteristic) = &characteristic.dependent_characteristic {
        let depline = dependent_characteristic.get_line();
        check_reference_list(
            "DEPENDENT_CHARACTERISTIC",
            "CHARACTERISTIC",
            depline,
            &dependent_characteristic.characteristic_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(map_list) = &characteristic.map_list {
        let ml_line = map_list.get_line();
        check_reference_list(
            "MAP_LIST",
            "CHARACTERISTIC",
            ml_line,
            &map_list.name_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(virtual_characteristic) = &characteristic.virtual_characteristic {
        let vc_line = virtual_characteristic.get_line();
        check_reference_list(
            "VIRTUAL_CHARACTERISTIC",
            "CHARACTERISTIC",
            vc_line,
            &virtual_characteristic.characteristic_list,
            &name_map.object,
            log_msgs,
        );
    }

    check_function_list(&characteristic.function_list, name_map, log_msgs);
    check_ref_memory_segment(&characteristic.ref_memory_segment, name_map, log_msgs);
}

fn check_compu_method(
    compu_method: &CompuMethod,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let line = compu_method.get_line();

    if let Some(compu_tab_ref) = &compu_method.compu_tab_ref {
        if name_map
            .compu_tab
            .get(&compu_tab_ref.conversion_table)
            .is_none()
        {
            log_msgs.push(format!(
                "In COMPU_METHOD on line {}: The COMPU_TAB_REF references nonexistent COMPU_TAB {}",
                line, compu_tab_ref.conversion_table
            ));
        }
    }

    if let Some(ref_unit) = &compu_method.ref_unit {
        if name_map.compu_tab.get(&ref_unit.unit).is_none() {
            log_msgs.push(format!(
                "In COMPU_METHOD on line {}: The REF_UNIT references nonexistent UNIT {}",
                line, ref_unit.unit
            ));
        }
    }
}

fn check_function(function: &Function, name_map: &ModuleNameMap, log_msgs: &mut Vec<String>) {
    if let Some(in_measurement) = &function.in_measurement {
        let line = in_measurement.get_line();
        check_reference_list(
            "IN_MEASUREMENT",
            "MEASUREMENT",
            line,
            &in_measurement.identifier_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(loc_measurement) = &function.loc_measurement {
        let line = loc_measurement.get_line();
        check_reference_list(
            "LOC_MEASUREMENT",
            "MEASUREMENT",
            line,
            &loc_measurement.identifier_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(out_measurement) = &function.out_measurement {
        let line = out_measurement.get_line();
        check_reference_list(
            "OUT_MEASUREMENT",
            "MEASUREMENT",
            line,
            &out_measurement.identifier_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(def_characteristic) = &function.def_characteristic {
        let line = def_characteristic.get_line();
        check_reference_list(
            "DEF_CHARACTERISTIC",
            "CHARACTERISTIC",
            line,
            &def_characteristic.identifier_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(ref_characteristic) = &function.ref_characteristic {
        let line = ref_characteristic.get_line();
        check_reference_list(
            "REF_CHARACTERISTIC",
            "CHARACTERISTIC",
            line,
            &ref_characteristic.identifier_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(sub_function) = &function.sub_function {
        let line = sub_function.get_line();
        check_reference_list(
            "SUB_FUNCTION",
            "FUNCTION",
            line,
            &sub_function.identifier_list,
            &name_map.function,
            log_msgs,
        );
    }
}

fn check_function_list(
    opt_function_list: &Option<FunctionList>,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    if let Some(function_list) = opt_function_list {
        let line = function_list.get_line();
        check_reference_list(
            "FUNCTION_LIST",
            "FUNCTION",
            line,
            &function_list.name_list,
            &name_map.function,
            log_msgs,
        );
    }
}

fn check_group(group: &Group, name_map: &ModuleNameMap, log_msgs: &mut Vec<String>) {
    if let Some(ref_characteristic) = &group.ref_characteristic {
        let line = ref_characteristic.get_line();
        check_reference_list(
            "REF_CHARACTERISTIC",
            "CHARACTERISTIC",
            line,
            &ref_characteristic.identifier_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(ref_measurement) = &group.ref_measurement {
        let line = ref_measurement.get_line();
        check_reference_list(
            "REF_MEASUREMENT",
            "MEASUREMENT",
            line,
            &ref_measurement.identifier_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(sub_group) = &group.sub_group {
        let line = sub_group.get_line();
        check_reference_list(
            "SUB_GROUP",
            "GROUP",
            line,
            &sub_group.identifier_list,
            &name_map.group,
            log_msgs,
        );
    }
}

fn check_measurement(
    measurement: &Measurement,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let name = &measurement.name;
    let line = measurement.get_line();

    if measurement.conversion != "NO_COMPU_METHOD"
        && name_map.compu_method.get(&measurement.conversion).is_none()
    {
        log_msgs.push(format!(
            "In MEASUREMENT {} on line {}: Referenced COMPU_METHOD {} does not exist.",
            name, line, measurement.conversion
        ));
    }

    check_ref_memory_segment(&measurement.ref_memory_segment, name_map, log_msgs);
}

fn check_transformer(
    transformer: &Transformer,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let name = &transformer.name;
    let line = transformer.get_line();

    if transformer.inverse_transformer != "NO_INVERSE_TRANSFORMER"
        && name_map
            .transformer
            .get(&transformer.inverse_transformer)
            .is_none()
    {
        log_msgs.push(format!(
            "In TRANSFORMTER {} on line {}: Referenced inverse TRANSFORMER {} does not exist.",
            name, line, transformer.inverse_transformer
        ));
    }

    if let Some(transformer_in_objects) = &transformer.transformer_in_objects {
        let line = transformer_in_objects.get_line();
        check_reference_list(
            "TRANSFORMER_IN_OBJECTS",
            "CHARACTERISTIC",
            line,
            &transformer_in_objects.identifier_list,
            &name_map.object,
            log_msgs,
        );
    }

    if let Some(transformer_out_objects) = &transformer.transformer_out_objects {
        let line = transformer_out_objects.get_line();
        check_reference_list(
            "TRANSFORMER_OUT_OBJECTS",
            "CHARACTERISTIC",
            line,
            &transformer_out_objects.identifier_list,
            &name_map.object,
            log_msgs,
        );
    }
}

fn check_ref_memory_segment(
    opt_ref_memory_segment: &Option<RefMemorySegment>,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    if let Some(ref_memory_segment) = opt_ref_memory_segment {
        let line = ref_memory_segment.get_line();

        if name_map
            .memory_segment
            .get(&ref_memory_segment.name)
            .is_none()
        {
            log_msgs.push(format!(
                "In REF_MEMORY_SEGMENT on line {}: reference to unknown memory segment {}",
                line, ref_memory_segment.name
            ));
        }
    }
}

fn check_reference_list<T>(
    container_type: &str,
    ref_type: &str,
    line: u32,
    identifier_list: &[String],
    map: &HashMap<String, T>,
    log_msgs: &mut Vec<String>,
) {
    for ident in identifier_list {
        if map.get(ident).is_none() {
            log_msgs.push(format!(
                "In {} on line {}: Reference to nonexistent {} \"{}\"",
                container_type, line, ref_type, ident
            ));
        }
    }
}

struct GroupInfo<'a> {
    is_root: bool,
    parents: Vec<&'a str>,
}

fn check_group_structure(grouplist: &[Group], log_msgs: &mut Vec<String>) {
    let mut groupinfo: HashMap<String, GroupInfo> = HashMap::new();

    for group in grouplist {
        groupinfo.insert(
            group.name.to_owned(),
            GroupInfo {
                is_root: group.root.is_some(),
                parents: Vec::new(),
            },
        );
    }

    for group in grouplist {
        if let Some(sub_group) = &group.sub_group {
            for sg in &sub_group.identifier_list {
                if let Some(gi) = groupinfo.get_mut(sg) {
                    gi.parents.push(&group.name);
                } else {
                    log_msgs.push(format!(
                        "GROUP {} references non-existent sub-group {}",
                        group.name, sg
                    ));
                }
            }
        }
    }

    for group in grouplist {
        if let Some(gi) = groupinfo.get(&group.name) {
            if gi.is_root && gi.parents.len() > 1 {
                log_msgs.push(format!("GROUP {} has the ROOT attribute, but is also referenced as a sub-group by GROUPs {}", group.name, gi.parents.join(", ")));
            } else if gi.is_root && gi.parents.len() == 1 {
                log_msgs.push(format!("GROUP {} has the ROOT attribute, but is also referenced as a sub-group by GROUP {}", group.name, gi.parents[0]));
            } else if !gi.is_root && gi.parents.len() > 1 {
                log_msgs.push(format!(
                    "GROUP {} is referenced as a sub-group by multiple groups: {}",
                    group.name,
                    gi.parents.join(", ")
                ));
            } else if !gi.is_root && gi.parents.is_empty() {
                log_msgs.push(format!("GROUP {} does not have the ROOT attribute, and is not referenced as a sub-group by any other group", group.name));
            }
        }
    }
}
