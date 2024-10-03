use crate::namemap::ModuleNameMap;
use crate::specification::*;
use std::collections::HashMap;

// check the cross references between various elements
pub fn check(a2l_file: &A2lFile, log_msgs: &mut Vec<String>) {
    for module in &a2l_file.project.module {
        let name_map = ModuleNameMap::build(&a2l_file.project.module[0], log_msgs);

        for axis_pts in &module.axis_pts {
            check_axis_pts(axis_pts, &name_map, log_msgs);
        }

        for t_axis in &module.typedef_axis {
            check_typedef_axis(t_axis, &name_map, log_msgs);
        }

        for characteristic in &module.characteristic {
            check_characteristic(characteristic, &name_map, log_msgs);
        }

        for t_characteristic in &module.typedef_characteristic {
            check_typedef_characteristic(t_characteristic, &name_map, log_msgs);
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

        for t_measurement in &module.typedef_measurement {
            check_typedef_measurement(t_measurement, &name_map, log_msgs);
        }

        for transformer in &module.transformer {
            check_transformer(transformer, &name_map, log_msgs);
        }

        for instance in &module.instance {
            check_instance(instance, &name_map, log_msgs);
        }

        for typedef_structure in &module.typedef_structure {
            check_typedef_structure(typedef_structure, &name_map, log_msgs);
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
        && !name_map.object.contains_key(&axis_descr.input_quantity)
    {
        log_msgs.push(format!("In AXIS_DESCR of CHARACTERISTIC {} on line {}: Referenced input MEASUREMENT {} does not exist.",
        parent_name, line, axis_descr.input_quantity));
    }

    if axis_descr.conversion != "NO_COMPU_METHOD"
        && !name_map.compu_method.contains_key(&axis_descr.conversion)
    {
        log_msgs.push(format!("In AXIS_DESCR of CHARACTERISTIC {} on line {}: Referenced COMPU_METHOD {} does not exist.",
        parent_name, line, axis_descr.conversion));
    }

    if let Some(axis_pts_ref) = &axis_descr.axis_pts_ref {
        let apr_line = axis_pts_ref.get_line();
        if !name_map.object.contains_key(&axis_pts_ref.axis_points) {
            log_msgs.push(format!("In AXIS_PTS_REF of CHARACTERISTIC {} on line {}: Referenced AXIS_PTS {} does not exist",
            parent_name, apr_line, axis_pts_ref.axis_points));
        }
    }

    if let Some(curve_axis_ref) = &axis_descr.curve_axis_ref {
        let car_line = curve_axis_ref.get_line();
        if !name_map.object.contains_key(&curve_axis_ref.curve_axis) {
            log_msgs.push(format!(
                "In CURVE_AXIS_REF on line {}: Referenced CHARACTERISTIC {} does not exist",
                car_line, curve_axis_ref.curve_axis
            ));
        }
    }
}

fn check_typedef_axis(t_axis: &TypedefAxis, name_map: &ModuleNameMap, log_msgs: &mut Vec<String>) {
    let name = t_axis.get_name();
    let line = t_axis.get_line();

    if t_axis.input_quantity != "NO_INPUT_QUANTITY"
        && !name_map.object.contains_key(&t_axis.input_quantity)
    {
        log_msgs.push(format!(
            "In TYPEDEF_AXIS {} on line {}: Referenced input MEASUREMENT {} does not exist.",
            name, line, t_axis.input_quantity
        ));
    }

    if !name_map.record_layout.contains_key(&t_axis.record_layout) {
        log_msgs.push(format!(
            "In TYPEDEF_AXIS {} on line {}: Referenced RECORD_LAYOUT {} does not exist.",
            name, line, t_axis.record_layout
        ));
    }

    if t_axis.conversion != "NO_COMPU_METHOD"
        && !name_map.compu_method.contains_key(&t_axis.conversion)
    {
        log_msgs.push(format!(
            "In TYPEDEF_AXIS {} on line {}: Referenced COMPU_METHOD {} does not exist.",
            name, line, t_axis.conversion
        ));
    }
}

fn check_axis_pts(axis_pts: &AxisPts, name_map: &ModuleNameMap, log_msgs: &mut Vec<String>) {
    let name = &axis_pts.name;
    let line = axis_pts.get_line();

    if axis_pts.conversion != "NO_COMPU_METHOD"
        && !name_map.compu_method.contains_key(&axis_pts.conversion)
    {
        log_msgs.push(format!(
            "In AXIS_PTS {} on line {}: Referenced COMPU_METHOD {} does not exist.",
            name, line, axis_pts.conversion
        ));
    }

    if !name_map.object.contains_key(&axis_pts.input_quantity) {
        log_msgs.push(format!(
            "In AXIS_PTS {} on line {}: Referenced input MEASUREMENT {} does not exist.",
            name, line, axis_pts.input_quantity
        ));
    }

    if !name_map
        .record_layout
        .contains_key(&axis_pts.deposit_record)
    {
        log_msgs.push(format!(
            "In AXIS_PTS {} on line {}: Referenced RECORD_LAYOUT {} does not exist.",
            name, line, axis_pts.deposit_record
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
        && !name_map
            .compu_method
            .contains_key(&characteristic.conversion)
    {
        log_msgs.push(format!(
            "In CHARACTERISTIC {} on line {}: Referenced COMPU_METHOD {} does not exist.",
            name, line, characteristic.conversion
        ));
    }

    if !name_map.record_layout.contains_key(&characteristic.deposit) {
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
        if !name_map.object.contains_key(&comparison_quantity.name) {
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

fn check_typedef_characteristic(
    t_characteristic: &TypedefCharacteristic,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let name = &t_characteristic.name;
    let line = t_characteristic.get_line();

    if t_characteristic.conversion != "NO_COMPU_METHOD"
        && !name_map
            .compu_method
            .contains_key(&t_characteristic.conversion)
    {
        log_msgs.push(format!(
            "In TYPEDEF_CHARACTERISTIC {} on line {}: Referenced COMPU_METHOD {} does not exist.",
            name, line, t_characteristic.conversion
        ));
    }

    if !name_map
        .record_layout
        .contains_key(&t_characteristic.record_layout)
    {
        log_msgs.push(format!(
            "In TYPEDEF_CHARACTERISTIC {} on line {}: Referenced RECORD_LAYOUT {} does not exist.",
            name, line, t_characteristic.record_layout
        ));
    }

    for axis_descr in &t_characteristic.axis_descr {
        check_axis_descr(name, axis_descr, name_map, log_msgs);
    }
}

fn check_compu_method(
    compu_method: &CompuMethod,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let line = compu_method.get_line();

    if let Some(compu_tab_ref) = &compu_method.compu_tab_ref {
        if !name_map
            .compu_tab
            .contains_key(&compu_tab_ref.conversion_table)
        {
            log_msgs.push(format!(
                "In COMPU_METHOD on line {}: The COMPU_TAB_REF references nonexistent COMPU_TAB {}",
                line, compu_tab_ref.conversion_table
            ));
        }
    }

    if let Some(ref_unit) = &compu_method.ref_unit {
        if !name_map.unit.contains_key(&ref_unit.unit) {
            log_msgs.push(format!(
                "In COMPU_METHOD on line {}: The REF_UNIT references nonexistent UNIT {}",
                line, ref_unit.unit
            ));
        }
    }

    if let Some(status_string_ref) = &compu_method.status_string_ref {
        if !name_map
            .compu_tab
            .contains_key(&status_string_ref.conversion_table)
        {
            log_msgs.push(format!(
                "In COMPU_METHOD on line {}: The STATUS_STRING_REF references nonexistent COMPU_TAB {}",
                line, status_string_ref.conversion_table
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

    if let Some(function_list) = &group.function_list {
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
        && !name_map.compu_method.contains_key(&measurement.conversion)
    {
        log_msgs.push(format!(
            "In MEASUREMENT {} on line {}: Referenced COMPU_METHOD {} does not exist.",
            name, line, measurement.conversion
        ));
    }

    check_ref_memory_segment(&measurement.ref_memory_segment, name_map, log_msgs);
    check_function_list(&measurement.function_list, name_map, log_msgs);
}

fn check_typedef_measurement(
    t_measurement: &TypedefMeasurement,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let name = &t_measurement.name;
    let line = t_measurement.get_line();

    if t_measurement.conversion != "NO_COMPU_METHOD"
        && !name_map
            .compu_method
            .contains_key(&t_measurement.conversion)
    {
        log_msgs.push(format!(
            "In TYPEDEF_MEASUREMENT {} on line {}: Referenced COMPU_METHOD {} does not exist.",
            name, line, t_measurement.conversion
        ));
    }
}

fn check_transformer(
    transformer: &Transformer,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let name = &transformer.name;
    let line = transformer.get_line();

    if transformer.inverse_transformer != "NO_INVERSE_TRANSFORMER"
        && !name_map
            .transformer
            .contains_key(&transformer.inverse_transformer)
    {
        log_msgs.push(format!(
            "In TRANSFORMER {} on line {}: Referenced inverse TRANSFORMER {} does not exist.",
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

        if !name_map
            .memory_segment
            .contains_key(&ref_memory_segment.name)
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
                "In {container_type} on line {line}: Reference to nonexistent {ref_type} \"{ident}\""
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
            group.name.clone(),
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
        let gi = groupinfo
            .get(&group.name)
            .expect("all groups should be in the groupinfo map");
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

fn check_instance(instance: &Instance, name_map: &ModuleNameMap, log_msgs: &mut Vec<String>) {
    let name = &instance.name;
    let line = instance.get_line();

    if !name_map.typedef.contains_key(&instance.type_ref) {
        log_msgs.push(format!(
            "In INSTANCE {} on line {}: Referenced TYPEDEF_<x> {} does not exist.",
            name, line, instance.type_ref
        ));
    }
}

fn check_typedef_structure(
    typedef_structure: &TypedefStructure,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<String>,
) {
    let name = &typedef_structure.name;
    let line = typedef_structure.get_line();

    for sc in &typedef_structure.structure_component {
        if !name_map.typedef.contains_key(&sc.component_type) {
            log_msgs.push(format!(
                "In STRUCTURE_COMPONENT {} of TYPEDEF_STRUCTURE {} on line {}: Referenced TYPEDEF_<x> {} does not exist.",
                sc.component_name, name, line, sc.component_type
            ));
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn check_axis_descr() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" VALUE 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0
                /begin AXIS_DESCR STD_AXIS input_quantity conversion 1 0 100
                    AXIS_PTS_REF axis_points
                    CURVE_AXIS_REF curve_axis
                /end AXIS_DESCR
            /end CHARACTERISTIC
            /begin RECORD_LAYOUT rl /end RECORD_LAYOUT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid input quantity, conversion, axis points ref, and curve axis ref
        assert_eq!(log_msgs.len(), 4);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" VALUE 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0
                /begin AXIS_DESCR STD_AXIS meas cm 1 0 100
                /end AXIS_DESCR
            /end CHARACTERISTIC
            /begin RECORD_LAYOUT rl /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
            /begin MEASUREMENT meas "" FLOAT32_IEEE cm  1 1.0 0 100
            /end MEASUREMENT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid input quantity
        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_typedef_axis() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_AXIS typedef_axis_name "" input_quantity record_layout 0 conversion 1 0 100
            /end TYPEDEF_AXIS
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid input quantity, record layout and conversion
        assert_eq!(log_msgs.len(), 3);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_AXIS typedef_axis_name "" meas rl 0 cm 1 0 100
            /end TYPEDEF_AXIS
            /begin MEASUREMENT meas "" FLOAT32_IEEE cm 1 1.0 0 100
            /end MEASUREMENT
            /begin RECORD_LAYOUT rl /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_axis_pts() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin AXIS_PTS axis_pts_name "" 0x1234 input_qty record_layout 0 conversion 3 0.0 10.0
            /end AXIS_PTS
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid input quantity, record layout (aka deposit_record), and conversion
        assert_eq!(log_msgs.len(), 3);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin AXIS_PTS axis_pts_name "" 0x1234 meas rl 0 cm 3 0.0 10.0
            /end AXIS_PTS
            /begin MEASUREMENT meas "" FLOAT32_IEEE cm 1 1.0 0 100
            /end MEASUREMENT
            /begin RECORD_LAYOUT rl /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);
        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_characteristic() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" VALUE 0x1234 rl 0 conversion 0.0 1.0
                COMPARISON_QUANTITY comp_qty
                REF_MEMORY_SEGMENT mem_seg
                /begin DEPENDENT_CHARACTERISTIC "formula"
                    name
                /end DEPENDENT_CHARACTERISTIC
                /begin FUNCTION_LIST
                    name
                /end FUNCTION_LIST
                /begin MAP_LIST
                    name
                /end MAP_LIST
                /begin VIRTUAL_CHARACTERISTIC "virt_char"
                    name
                /end VIRTUAL_CHARACTERISTIC
            /end CHARACTERISTIC
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid conversion, record layout, comparison quantity, memory segment,
        // dependent characteristic, function, map list, and virtual characteristic
        assert_eq!(log_msgs.len(), 8);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" VALUE 0x1234 rl 0 cm 0.0 1.0
            /end CHARACTERISTIC
            /begin RECORD_LAYOUT rl /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);
        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_typedef_characteristic() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_CHARACTERISTIC name "" VALUE record_layout 0 conversion 0 100
                /begin AXIS_DESCR STD_AXIS input_quantity conversion 1 0 100
                /end AXIS_DESCR
            /end TYPEDEF_CHARACTERISTIC
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid record layout and conversion, additionally the axis descr check reports invalid input quantity and conversion
        assert_eq!(log_msgs.len(), 4);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_CHARACTERISTIC name "" VALUE rl 0 cm 0 100
            /end TYPEDEF_CHARACTERISTIC
            /begin RECORD_LAYOUT rl /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);
        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_compu_method() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit"
                COMPU_TAB_REF compu_tab_ref
                REF_UNIT ref_unit
                STATUS_STRING_REF status_string_ref
            /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid compu tab ref, ref unit, and status string ref
        assert_eq!(log_msgs.len(), 3);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit"
            /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);
        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_function() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin FUNCTION f ""
                /begin IN_MEASUREMENT
                    name
                /end IN_MEASUREMENT
                /begin LOC_MEASUREMENT
                    name
                /end LOC_MEASUREMENT
                /begin OUT_MEASUREMENT
                    name
                /end OUT_MEASUREMENT
                /begin DEF_CHARACTERISTIC
                    name
                /end DEF_CHARACTERISTIC
                /begin REF_CHARACTERISTIC
                    name
                /end REF_CHARACTERISTIC
                /begin SUB_FUNCTION
                    name
                /end SUB_FUNCTION
            /end FUNCTION
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid in measurement, loc measurement, out measurement, def characteristic,
        // ref characteristic, and sub function
        assert_eq!(log_msgs.len(), 6);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin FUNCTION f ""
            /end FUNCTION
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);
        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_group() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin GROUP g ""
                /begin REF_CHARACTERISTIC
                    name
                /end REF_CHARACTERISTIC
                /begin REF_MEASUREMENT
                    name
                /end REF_MEASUREMENT
                /begin FUNCTION_LIST
                    name
                /end FUNCTION_LIST
                /begin SUB_GROUP
                    name
                /end SUB_GROUP
            /end GROUP
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid ref characteristic, ref measurement, function list, and sub group
        // additionally the group consistency check reports that the group is not a root group, and refereces a non-existent sub-group
        assert_eq!(log_msgs.len(), 6);
    }

    #[test]
    fn check_group_structure() {
        // grp1 is a root group, but is also referenced as a sub-group by grp2
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin GROUP grp1 "" ROOT
            /end GROUP
            /begin GROUP grp2 "" ROOT
                /begin SUB_GROUP
                    grp1
                /end SUB_GROUP
            /end GROUP
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);

        // grp1 is a root group, but is also referenced as a sub-group by grp2 and grp3
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin GROUP grp1 "" ROOT
            /end GROUP
            /begin GROUP grp2 "" ROOT
                /begin SUB_GROUP
                    grp1
                /end SUB_GROUP
            /end GROUP
            /begin GROUP grp3 "" ROOT
                /begin SUB_GROUP
                    grp1
                /end SUB_GROUP
            /end GROUP
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);

        // grp1 (not a root group) is referenced as a sub-group by grp2 and grp3
        static A2L_TEXT3: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin GROUP grp1 ""
            /end GROUP
            /begin GROUP grp2 "" ROOT
                /begin SUB_GROUP
                    grp1
                /end SUB_GROUP
            /end GROUP
            /begin GROUP grp3 "" ROOT
                /begin SUB_GROUP
                    grp1
                /end SUB_GROUP
            /end GROUP
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT3, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
    }

    #[test]
    fn check_measurement() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin MEASUREMENT m "" FLOAT32_IEEE conversion 1 1.0 0 100
                REF_MEMORY_SEGMENT mem_seg
                /begin FUNCTION_LIST
                    name
                /end FUNCTION_LIST
            /end MEASUREMENT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid conversion, memory segment, and function list
        assert_eq!(log_msgs.len(), 3);
    }

    #[test]
    fn check_typedef_measurement() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_MEASUREMENT tm "" UBYTE conversion 1 1 0 100
            /end TYPEDEF_MEASUREMENT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid conversion
        assert_eq!(log_msgs.len(), 1);
    }

    #[test]
    fn check_transformer() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TRANSFORMER transformer_name "version string" "dll32" "dll64" 1 ON_CHANGE inverse_transformer
                /begin TRANSFORMER_IN_OBJECTS
                    name
                /end TRANSFORMER_IN_OBJECTS
                /begin TRANSFORMER_OUT_OBJECTS
                    name
                /end TRANSFORMER_OUT_OBJECTS
            /end TRANSFORMER
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid inverse transformer, transformer in objects, and transformer out objects
        assert_eq!(log_msgs.len(), 3);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TRANSFORMER transformer_name "version string" "dll32" "dll64" 1 ON_CHANGE NO_INVERSE_TRANSFORMER
            /end TRANSFORMER
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);
        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_instance() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin INSTANCE i "" type_ref 0x1234
            /end INSTANCE
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid type ref
        assert_eq!(log_msgs.len(), 1);
    }

    #[test]
    fn check_typedef_structure() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_STRUCTURE ts "" 1
                /begin STRUCTURE_COMPONENT component_name component_type 1
                /end STRUCTURE_COMPONENT
            /end TYPEDEF_STRUCTURE
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let mut log_msgs = Vec::new();
        super::check(&a2lfile, &mut log_msgs);

        // invalid component type
        assert_eq!(log_msgs.len(), 1);
    }
}
