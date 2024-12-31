use crate::namemap::ModuleNameMap;
use crate::{specification::*, A2lError};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
enum CharacteristicWrapper<'a> {
    Characteristic(&'a Characteristic),
    TypedefCharacteristic(&'a TypedefCharacteristic),
}

// check the cross references between various elements
pub fn check(a2l_file: &A2lFile) -> Vec<A2lError> {
    let mut results = Vec::new();

    for module in &a2l_file.project.module {
        let (name_map, errors) = ModuleNameMap::build(&a2l_file.project.module[0]);
        results.extend(errors);

        for axis_pts in &module.axis_pts {
            check_axis_pts(axis_pts, &name_map, &mut results);
        }

        for t_axis in &module.typedef_axis {
            check_typedef_axis(t_axis, &name_map, &mut results);
        }

        for characteristic in &module.characteristic {
            check_characteristic(characteristic, &name_map, &mut results);
        }

        for t_characteristic in &module.typedef_characteristic {
            check_typedef_characteristic(t_characteristic, &name_map, &mut results);
        }

        for compu_method in &module.compu_method {
            check_compu_method(compu_method, &name_map, &mut results);
        }

        for function in &module.function {
            check_function(function, &name_map, &mut results);
        }

        for group in &module.group {
            check_group(group, &name_map, &mut results);
        }
        check_group_structure(&module.group, &mut results);

        for measurement in &module.measurement {
            check_measurement(measurement, &name_map, &mut results);
        }

        for t_measurement in &module.typedef_measurement {
            check_typedef_measurement(t_measurement, &name_map, &mut results);
        }

        for transformer in &module.transformer {
            check_transformer(transformer, &name_map, &mut results);
        }

        for instance in &module.instance {
            check_instance(instance, &name_map, &mut results);
        }

        for typedef_structure in &module.typedef_structure {
            check_typedef_structure(typedef_structure, &name_map, &mut results);
        }
    }

    results
}

fn check_axis_descr(
    idx: usize,
    parent_name: &str,
    axis_descr: &AxisDescr,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<A2lError>,
) {
    let line = axis_descr.get_line();
    if axis_descr.input_quantity != "NO_INPUT_QUANTITY"
        && !name_map.object.contains_key(&axis_descr.input_quantity)
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: format!("AXIS_DESCR[{idx}] of CHARACTERISTIC"),
            source_name: parent_name.to_string(),
            source_line: line,
            target_type: "MEASUREMENT".to_string(),
            target_name: axis_descr.input_quantity.to_string(),
        });
    }

    if axis_descr.conversion != "NO_COMPU_METHOD"
        && !name_map.compu_method.contains_key(&axis_descr.conversion)
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: format!("AXIS_DESCR[{idx}] of CHARACTERISTIC"),
            source_name: parent_name.to_string(),
            source_line: line,
            target_type: "COMPU_METHOD".to_string(),
            target_name: axis_descr.conversion.to_string(),
        });
    }

    // type is (COM_AXIS, RES_AXIS) XOR AXIS_PTS_REF is present
    match (axis_descr.attribute, axis_descr.axis_pts_ref.is_some()) {
        (AxisDescrAttribute::ComAxis, false) | (AxisDescrAttribute::ResAxis, false) => {
            log_msgs.push(A2lError::ContentError {
                item_name: parent_name.to_string(),
                blockname: format!("AXIS_DESCR[{idx}] of CHARACTERISTIC"),
                line,
                description: format!("{} requires an AXIS_PTS_REF.", axis_descr.attribute),
            });
        }
        (AxisDescrAttribute::StdAxis, true) | (AxisDescrAttribute::FixAxis, true) => {
            log_msgs.push(A2lError::ContentError {
                item_name: parent_name.to_string(),
                blockname: format!("AXIS_DESCR[{idx}] of CHARACTERISTIC"),
                line,
                description: format!("{} does not use AXIS_PTS_REF.", axis_descr.attribute),
            });
        }
        // other combinations are valid
        _ => {}
    }

    if let Some(axis_pts_ref) = &axis_descr.axis_pts_ref {
        let apr_line = axis_pts_ref.get_line();
        if !name_map.object.contains_key(&axis_pts_ref.axis_points) {
            log_msgs.push(A2lError::CrossReferenceError {
                source_type: format!("AXIS_DESCR[{idx}] of CHARACTERISTIC"),
                source_name: parent_name.to_string(),
                source_line: apr_line,
                target_type: "AXIS_PTS".to_string(),
                target_name: axis_pts_ref.axis_points.to_string(),
            });
        }
    }

    if let Some(curve_axis_ref) = &axis_descr.curve_axis_ref {
        let car_line = curve_axis_ref.get_line();
        if !name_map.object.contains_key(&curve_axis_ref.curve_axis) {
            log_msgs.push(A2lError::CrossReferenceError {
                source_type: format!("AXIS_DESCR[{idx}] of CHARACTERISTIC"),
                source_name: parent_name.to_string(),
                source_line: car_line,
                target_type: "CHARACTERISTIC".to_string(),
                target_name: curve_axis_ref.curve_axis.to_string(),
            });
        }
    }
}

fn check_typedef_axis(
    t_axis: &TypedefAxis,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<A2lError>,
) {
    let name = t_axis.get_name();
    let line = t_axis.get_line();

    if t_axis.input_quantity != "NO_INPUT_QUANTITY"
        && !name_map.object.contains_key(&t_axis.input_quantity)
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "TYPEDEF_AXIS".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "MEASUREMENT".to_string(),
            target_name: t_axis.input_quantity.to_string(),
        });
    }

    if !name_map.record_layout.contains_key(&t_axis.record_layout) {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "TYPEDEF_AXIS".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "RECORD_LAYOUT".to_string(),
            target_name: t_axis.record_layout.to_string(),
        });
    }

    if t_axis.conversion != "NO_COMPU_METHOD"
        && !name_map.compu_method.contains_key(&t_axis.conversion)
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "TYPEDEF_AXIS".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "COMPU_METHOD".to_string(),
            target_name: t_axis.conversion.to_string(),
        });
    }
}

fn check_axis_pts(axis_pts: &AxisPts, name_map: &ModuleNameMap, log_msgs: &mut Vec<A2lError>) {
    let name = &axis_pts.name;
    let line = axis_pts.get_line();

    if axis_pts.conversion != "NO_COMPU_METHOD"
        && !name_map.compu_method.contains_key(&axis_pts.conversion)
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "AXIS_PTS".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "COMPU_METHOD".to_string(),
            target_name: axis_pts.conversion.to_string(),
        });
    }

    if axis_pts.input_quantity != "NO_INPUT_QUANTITY"
        && !name_map.object.contains_key(&axis_pts.input_quantity)
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "AXIS_PTS".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "MEASUREMENT".to_string(),
            target_name: axis_pts.input_quantity.to_string(),
        });
    }

    if let Some(rl) = name_map.record_layout.get(&axis_pts.deposit_record) {
        if let Some(axis_pts_x) = &rl.axis_pts_x {
            let opt_compu_method = name_map.compu_method.get(&axis_pts.conversion);
            let calculated_limits = calc_compu_method_limits(opt_compu_method, axis_pts_x.datatype);
            let existing_limits = (axis_pts.lower_limit, axis_pts.upper_limit);
            if !check_limits_valid(existing_limits, calculated_limits) {
                log_msgs.push(A2lError::LimitCheckError {
                    item_name: name.to_string(),
                    blockname: "AXIS_PTS".to_string(),
                    line,
                    lower_limit: axis_pts.lower_limit,
                    upper_limit: axis_pts.upper_limit,
                    calculated_lower_limit: calculated_limits.0,
                    calculated_upper_limit: calculated_limits.1,
                });
            }
        } else {
            log_msgs.push(A2lError::ContentError {
                item_name: name.to_string(),
                blockname: "AXIS_PTS".to_string(),
                line,
                description: format!(
                    "Referenced RECORD_LAYOUT {} does not have AXIS_PTS_X.",
                    axis_pts.deposit_record
                ),
            });
        }
    } else {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "AXIS_PTS".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "RECORD_LAYOUT".to_string(),
            target_name: axis_pts.deposit_record.to_string(),
        });
    }

    check_function_list(&axis_pts.function_list, name_map, log_msgs);
    check_ref_memory_segment(&axis_pts.ref_memory_segment, name_map, log_msgs);
}

fn check_characteristic(
    characteristic: &Characteristic,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<A2lError>,
) {
    check_characteristic_common(
        CharacteristicWrapper::Characteristic(characteristic),
        name_map,
        log_msgs,
    );

    if let Some(comparison_quantity) = &characteristic.comparison_quantity {
        let cqline = comparison_quantity.get_line();
        if !name_map.object.contains_key(&comparison_quantity.name) {
            log_msgs.push(A2lError::CrossReferenceError {
                source_type: "CHARACTERISTIC".to_string(),
                source_name: characteristic.name.to_string(),
                source_line: cqline,
                target_type: "MEASUREMENT".to_string(),
                target_name: comparison_quantity.name.to_string(),
            });
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
    log_msgs: &mut Vec<A2lError>,
) {
    check_characteristic_common(
        CharacteristicWrapper::TypedefCharacteristic(t_characteristic),
        name_map,
        log_msgs,
    );
}

// check the items that are shared between CHARACTERISTIC and TYPEDEF_CHARACTERISTIC
fn check_characteristic_common(
    characteristic: CharacteristicWrapper,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<A2lError>,
) {
    let kind = characteristic.kind();
    let name = characteristic.name();
    let line = characteristic.line();

    if characteristic.conversion() != "NO_COMPU_METHOD"
        && !name_map
            .compu_method
            .contains_key(characteristic.conversion())
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: kind.to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "COMPU_METHOD".to_string(),
            target_name: characteristic.conversion().to_string(),
        });
    }

    for (idx, axis_descr) in characteristic.axis_descr().iter().enumerate() {
        check_axis_descr(idx, name, axis_descr, name_map, log_msgs);
    }

    let expected_axis_count = match characteristic.characteristic_type() {
        CharacteristicType::Value | CharacteristicType::ValBlk | CharacteristicType::Ascii => 0,
        CharacteristicType::Curve => 1,
        CharacteristicType::Map => 2,
        CharacteristicType::Cuboid => 3,
        CharacteristicType::Cube4 => 4,
        CharacteristicType::Cube5 => 5,
    };
    if characteristic.axis_descr().len() != expected_axis_count {
        log_msgs.push(A2lError::ContentError {
            item_name: name.to_string(),
            blockname: kind.to_string(),
            line,
            description: format!(
                "Expected {expected_axis_count} AXIS_DESCR for type {}, found {}",
                characteristic.characteristic_type(),
                characteristic.axis_descr().len()
            ),
        });
    }

    let rl_name = characteristic.record_layout();
    if let Some(record_layout) = name_map.record_layout.get(rl_name) {
        if let Some(fnc_values) = &record_layout.fnc_values {
            // check the limits of the characteristic based on the compu method
            // Not all characteristics have a compu method, since it can be NO_COMPU_METHOD
            let opt_compu_method = name_map.compu_method.get(characteristic.conversion());
            let calculated_limits = calc_compu_method_limits(opt_compu_method, fnc_values.datatype);
            let existing_limits = (characteristic.lower_limit(), characteristic.upper_limit());
            if !check_limits_valid(existing_limits, calculated_limits) {
                log_msgs.push(A2lError::LimitCheckError {
                    item_name: name.to_string(),
                    blockname: kind.to_string(),
                    line,
                    lower_limit: characteristic.lower_limit(),
                    upper_limit: characteristic.upper_limit(),
                    calculated_lower_limit: calculated_limits.0,
                    calculated_upper_limit: calculated_limits.1,
                });
            }
        } else {
            log_msgs.push(A2lError::ContentError {
                item_name: name.to_string(),
                blockname: kind.to_string(),
                line,
                description: format!(
                    "Referenced RECORD_LAYOUT {rl_name} does not have FNC_VALUES."
                ),
            });
        }

        // make an array of references to the axis_pts fields in the record layout, some or all of which may be None
        let axis_refs = [
            &record_layout.axis_pts_x,
            &record_layout.axis_pts_y,
            &record_layout.axis_pts_z,
            &record_layout.axis_pts_4,
            &record_layout.axis_pts_5,
        ];
        let axis_pts_names = ["X", "Y", "Z", "4", "5"];
        for (idx, axis_descr) in characteristic.axis_descr().iter().enumerate() {
            if axis_descr.attribute == AxisDescrAttribute::StdAxis {
                // an STD_AXIS must be described by the record layout - should this also apply to CURVE_AXIS?
                if let Some(axis_pts_dim) = axis_refs[idx] {
                    // the compu method is optional, it could be set to NO_COMPU_METHOD
                    let opt_compu_method = name_map.compu_method.get(&axis_descr.conversion);
                    let calculated_limits =
                        calc_compu_method_limits(opt_compu_method, axis_pts_dim.datatype);
                    let existing_limits = (axis_descr.lower_limit, axis_descr.upper_limit);
                    if !check_limits_valid(existing_limits, calculated_limits) {
                        let ad_line = axis_descr.get_line();
                        log_msgs.push(A2lError::LimitCheckError {
                            item_name: name.to_string(),
                            blockname: "AXIS_DESCR".to_string(),
                            line: ad_line,
                            lower_limit: axis_descr.lower_limit,
                            upper_limit: axis_descr.upper_limit,
                            calculated_lower_limit: calculated_limits.0,
                            calculated_upper_limit: calculated_limits.1,
                        });
                    }
                } else {
                    log_msgs.push(A2lError::ContentError {
                        item_name: name.to_string(),
                        blockname: kind.to_string(),
                        line,
                        description: format!(
                            "Referenced RECORD_LAYOUT {rl_name} does not have AXIS_PTS_{}.",
                            axis_pts_names[idx]
                        ),
                    });
                }
            }
        }
    } else {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: kind.to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "RECORD_LAYOUT".to_string(),
            target_name: rl_name.to_string(),
        });
    }
}

fn check_compu_method(
    compu_method: &CompuMethod,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<A2lError>,
) {
    let line = compu_method.get_line();

    if let Some(compu_tab_ref) = &compu_method.compu_tab_ref {
        if !name_map
            .compu_tab
            .contains_key(&compu_tab_ref.conversion_table)
        {
            log_msgs.push(A2lError::CrossReferenceError {
                source_type: "COMPU_METHOD".to_string(),
                source_name: compu_method.name.to_string(),
                source_line: line,
                target_type: "COMPU_TAB".to_string(),
                target_name: compu_tab_ref.conversion_table.to_string(),
            });
        }
    }

    if let Some(ref_unit) = &compu_method.ref_unit {
        if !name_map.unit.contains_key(&ref_unit.unit) {
            log_msgs.push(A2lError::CrossReferenceError {
                source_type: "COMPU_METHOD".to_string(),
                source_name: compu_method.name.to_string(),
                source_line: line,
                target_type: "UNIT".to_string(),
                target_name: ref_unit.unit.to_string(),
            });
        }
    }

    if let Some(status_string_ref) = &compu_method.status_string_ref {
        if !name_map
            .compu_tab
            .contains_key(&status_string_ref.conversion_table)
        {
            log_msgs.push(A2lError::CrossReferenceError {
                source_type: "COMPU_METHOD".to_string(),
                source_name: compu_method.name.to_string(),
                source_line: line,
                target_type: "COMPU_VTAB".to_string(),
                target_name: status_string_ref.conversion_table.to_string(),
            });
        }
    }
}

fn check_function(function: &Function, name_map: &ModuleNameMap, log_msgs: &mut Vec<A2lError>) {
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
    log_msgs: &mut Vec<A2lError>,
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

fn check_group(group: &Group, name_map: &ModuleNameMap, log_msgs: &mut Vec<A2lError>) {
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
    log_msgs: &mut Vec<A2lError>,
) {
    let name = &measurement.name;
    let line = measurement.get_line();

    if measurement.conversion != "NO_COMPU_METHOD"
        && !name_map.compu_method.contains_key(&measurement.conversion)
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "MEASUREMENT".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "COMPU_METHOD".to_string(),
            target_name: measurement.conversion.to_string(),
        });
    }

    let opt_compu_method = name_map.compu_method.get(&measurement.conversion);
    let calculated_limits = calc_compu_method_limits(opt_compu_method, measurement.datatype);
    let existing_limits = (measurement.lower_limit, measurement.upper_limit);
    if !check_limits_valid(existing_limits, calculated_limits) {
        log_msgs.push(A2lError::LimitCheckError {
            item_name: name.to_string(),
            blockname: "MEASUREMENT".to_string(),
            line,
            lower_limit: measurement.lower_limit,
            upper_limit: measurement.upper_limit,
            calculated_lower_limit: calculated_limits.0,
            calculated_upper_limit: calculated_limits.1,
        });
    }

    check_ref_memory_segment(&measurement.ref_memory_segment, name_map, log_msgs);
    check_function_list(&measurement.function_list, name_map, log_msgs);
}

fn check_typedef_measurement(
    t_measurement: &TypedefMeasurement,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<A2lError>,
) {
    let name = &t_measurement.name;
    let line = t_measurement.get_line();

    if t_measurement.conversion != "NO_COMPU_METHOD"
        && !name_map
            .compu_method
            .contains_key(&t_measurement.conversion)
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "TYPEDEF_MEASUREMENT".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "COMPU_METHOD".to_string(),
            target_name: t_measurement.conversion.to_string(),
        });
    }

    let opt_compu_method = name_map.compu_method.get(&t_measurement.conversion);
    let (lower_limit, upper_limit) =
        calc_compu_method_limits(opt_compu_method, t_measurement.datatype);
    if lower_limit > t_measurement.lower_limit || upper_limit < t_measurement.upper_limit {
        log_msgs.push(A2lError::LimitCheckError {
            item_name: name.to_string(),
            blockname: "TYPEDEF_MEASUREMENT".to_string(),
            line,
            lower_limit: t_measurement.lower_limit,
            upper_limit: t_measurement.upper_limit,
            calculated_lower_limit: lower_limit,
            calculated_upper_limit: upper_limit,
        });
    }
}

fn check_transformer(
    transformer: &Transformer,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<A2lError>,
) {
    let name = &transformer.name;
    let line = transformer.get_line();

    if transformer.inverse_transformer != "NO_INVERSE_TRANSFORMER"
        && !name_map
            .transformer
            .contains_key(&transformer.inverse_transformer)
    {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "TRANSFORMER".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "inverse TRANSFORMER".to_string(),
            target_name: transformer.inverse_transformer.to_string(),
        });
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
    log_msgs: &mut Vec<A2lError>,
) {
    if let Some(ref_memory_segment) = opt_ref_memory_segment {
        let line = ref_memory_segment.get_line();

        if !name_map
            .memory_segment
            .contains_key(&ref_memory_segment.name)
        {
            log_msgs.push(A2lError::CrossReferenceError {
                source_type: "REF_MEMORY_SEGMENT".to_string(),
                source_name: ref_memory_segment.name.to_string(),
                source_line: line,
                target_type: "MEMORY_SEGMENT".to_string(),
                target_name: ref_memory_segment.name.to_string(),
            });
        }
    }
}

fn check_reference_list<T>(
    container_type: &str,
    ref_type: &str,
    line: u32,
    identifier_list: &[String],
    map: &HashMap<String, T>,
    log_msgs: &mut Vec<A2lError>,
) {
    for ident in identifier_list {
        if map.get(ident).is_none() {
            log_msgs.push(A2lError::CrossReferenceError {
                source_type: container_type.to_string(),
                source_name: ident.to_string(),
                source_line: line,
                target_type: ref_type.to_string(),
                target_name: ident.to_string(),
            });
        }
    }
}

struct GroupInfo<'a> {
    is_root: bool,
    parents: Vec<&'a str>,
}

fn check_group_structure(grouplist: &[Group], log_msgs: &mut Vec<A2lError>) {
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
                    // nonexistent sub-group
                    log_msgs.push(A2lError::CrossReferenceError {
                        source_type: "GROUP".to_string(),
                        source_name: group.name.clone(),
                        source_line: group.get_line(),
                        target_type: "GROUP".to_string(),
                        target_name: sg.clone(),
                    });
                }
            }
        }
    }

    for group in grouplist {
        let gi = groupinfo
            .get(&group.name)
            .expect("all groups should be in the groupinfo map");
        if gi.is_root && gi.parents.len() > 1 {
            log_msgs.push(A2lError::GroupStructureError {
                group_name: group.name.clone(),
                line: group.get_line(),
                description: format!(
                    "has the ROOT attribute, but is also referenced as a sub-group by GROUPs {}",
                    gi.parents.join(", ")
                ),
            });
        } else if gi.is_root && gi.parents.len() == 1 {
            log_msgs.push(A2lError::GroupStructureError {
                group_name: group.name.clone(),
                line: group.get_line(),
                description: format!(
                    "has the ROOT attribute, but is also referenced as a sub-group by GROUP {}",
                    gi.parents[0]
                ),
            });
        } else if !gi.is_root && gi.parents.len() > 1 {
            log_msgs.push(A2lError::GroupStructureError {
                group_name: group.name.clone(),
                line: group.get_line(),
                description: format!(
                    "is referenced as a sub-group by multiple groups: {}",
                    gi.parents.join(", ")
                ),
            });
        } else if !gi.is_root && gi.parents.is_empty() {
            log_msgs.push(A2lError::GroupStructureError {
                group_name: group.name.clone(),
                line: group.get_line(),
                description: "does not have the ROOT attribute, and is not referenced as a sub-group by any other group".to_string(),
            });
        }
    }
}

fn check_instance(instance: &Instance, name_map: &ModuleNameMap, log_msgs: &mut Vec<A2lError>) {
    let name = &instance.name;
    let line = instance.get_line();

    if !name_map.typedef.contains_key(&instance.type_ref) {
        log_msgs.push(A2lError::CrossReferenceError {
            source_type: "INSTANCE".to_string(),
            source_name: name.to_string(),
            source_line: line,
            target_type: "TYPEDEF_<x>".to_string(),
            target_name: instance.type_ref.to_string(),
        });
    }
}

fn check_typedef_structure(
    typedef_structure: &TypedefStructure,
    name_map: &ModuleNameMap,
    log_msgs: &mut Vec<A2lError>,
) {
    let name = &typedef_structure.name;
    let line = typedef_structure.get_line();

    for sc in &typedef_structure.structure_component {
        if !name_map.typedef.contains_key(&sc.component_type) {
            log_msgs.push(A2lError::CrossReferenceError {
                source_type: format!(
                    "STRUCTURE_COMPONENT {} of TYPEDEF_STRUCTURE",
                    sc.component_name
                ),
                source_name: name.to_string(),
                source_line: line,
                target_type: "TYPEDEF_<x>".to_string(),
                target_name: sc.component_type.clone(),
            });
        }
    }
}

// calculate the limits of a characteristic based on the compu method
fn calc_compu_method_limits(
    opt_compu_method: Option<&&CompuMethod>,
    datatype: DataType,
) -> (f64, f64) {
    let (mut lower_limit, mut upper_limit) = get_datatype_limits(datatype);

    if let Some(cm) = opt_compu_method {
        match cm.conversion_type {
            ConversionType::Form => {
                // In order to handle this textual formula, we would need to parse it and evaluate it.
                // For now, we just allow any value.
                lower_limit = f64::MIN;
                upper_limit = f64::MAX;
            }
            ConversionType::Linear => {
                // for a linear compu method, the limits are physical values
                // f(x)=ax + b; PHYS = f(INT)
                if let Some(c) = &cm.coeffs_linear {
                    if c.a >= 0.0 {
                        lower_limit = c.a * lower_limit + c.b;
                        upper_limit = c.a * upper_limit + c.b;
                    } else {
                        // factor a is negative, so the lower and upper limits are swapped
                        upper_limit = c.a * lower_limit + c.b;
                        lower_limit = c.a * upper_limit + c.b;
                    }
                }
            }
            ConversionType::RatFunc => {
                // f(x)=(ax^2 + bx + c)/(dx^2 + ex + f); INT = f(PHYS)
                if let Some(c) = &cm.coeffs {
                    // we're only handling the simple linear case here
                    if c.a == 0.0 && c.d == 0.0 && c.e == 0.0 && c.f != 0.0 {
                        // now the rational function is reduced to
                        //   y = (bx + c) / f
                        // which can be inverted to
                        //   x = (fy - c) / b
                        // this is rewritten to, to fix the edge case where f is f64::MAX, y > 1, but y/b < 1
                        //   x = (f * (y/b)) - c/b
                        let func = |y: f64| (c.f * (y / c.b) - (c.c / c.b));
                        lower_limit = func(lower_limit);
                        upper_limit = func(upper_limit);
                        if lower_limit > upper_limit {
                            std::mem::swap(&mut lower_limit, &mut upper_limit);
                        }
                    } else {
                        // complex formula:
                        // deriving the limits from the coefficients is not implemented here, so allow any value
                        lower_limit = f64::MIN;
                        upper_limit = f64::MAX;
                    }
                }
            }
            ConversionType::Identical
            | ConversionType::TabIntp
            | ConversionType::TabNointp
            | ConversionType::TabVerb => {
                // identical and all table-based compu methods have direct int-to-phys mapping:
                // The compu method does not modify the limits given by the datatype.
            }
        }
    }

    (lower_limit, upper_limit)
}

// Compare the existing limits with the calculated limits
// Returns true if the existing limits are inside the calculated limits
//
// This should be simple, but is made more difficult by precision issues ...
fn check_limits_valid(existing: (f64, f64), calculated: (f64, f64)) -> bool {
    // calculated lower limit should be less than or equal to the existing lower limit
    // calculated upper limit should be greater than or equal to the existing upper limit

    // In order to allow for floating point precision issues and imprecise data entry in the A2L file,
    // we allow for a tolerance of 0.0001% of the calculated value.
    let epsilon_lower = (calculated.0 * 1E-6).abs();
    let epsilon_upper = (calculated.1 * 1E-6).abs();
    (calculated.0 - existing.0) <= epsilon_lower && (existing.1 - calculated.1) <= epsilon_upper
}

fn get_datatype_limits(a2l_datatype: DataType) -> (f64, f64) {
    match a2l_datatype {
        DataType::Ubyte => (0.0, 255.0),
        DataType::Sbyte => (-128.0, 127.0),
        DataType::Uword => (0.0, 65535.0),
        DataType::Sword => (-32768.0, 32767.0),
        DataType::Ulong => (0.0, 4294967295.0),
        DataType::Slong => (-2147483648.0, 2147483647.0),
        DataType::AUint64 => (0.0, 18446744073709551615.0),
        DataType::AInt64 => (-9223372036854775808.0, 9223372036854775807.0),
        DataType::Float16Ieee => (-6.5504e+4_f64, 6.5504e+4_f64),
        DataType::Float32Ieee => (f32::MIN as f64, f32::MAX as f64),
        DataType::Float64Ieee => (f64::MIN, f64::MAX),
    }
}

impl CharacteristicWrapper<'_> {
    fn name(&self) -> &str {
        match self {
            CharacteristicWrapper::Characteristic(c) => &c.name,
            CharacteristicWrapper::TypedefCharacteristic(tc) => &tc.name,
        }
    }

    fn line(&self) -> u32 {
        match self {
            CharacteristicWrapper::Characteristic(c) => c.get_line(),
            CharacteristicWrapper::TypedefCharacteristic(tc) => tc.get_line(),
        }
    }

    fn axis_descr(&self) -> &[AxisDescr] {
        match self {
            CharacteristicWrapper::Characteristic(c) => &c.axis_descr,
            CharacteristicWrapper::TypedefCharacteristic(tc) => &tc.axis_descr,
        }
    }

    fn conversion(&self) -> &str {
        match self {
            CharacteristicWrapper::Characteristic(c) => &c.conversion,
            CharacteristicWrapper::TypedefCharacteristic(tc) => &tc.conversion,
        }
    }

    fn record_layout(&self) -> &str {
        match self {
            CharacteristicWrapper::Characteristic(c) => &c.deposit,
            CharacteristicWrapper::TypedefCharacteristic(tc) => &tc.record_layout,
        }
    }

    fn characteristic_type(&self) -> CharacteristicType {
        match self {
            CharacteristicWrapper::Characteristic(c) => c.characteristic_type,
            CharacteristicWrapper::TypedefCharacteristic(tc) => tc.characteristic_type,
        }
    }

    fn lower_limit(&self) -> f64 {
        match self {
            CharacteristicWrapper::Characteristic(c) => c.lower_limit,
            CharacteristicWrapper::TypedefCharacteristic(tc) => tc.lower_limit,
        }
    }

    fn upper_limit(&self) -> f64 {
        match self {
            CharacteristicWrapper::Characteristic(c) => c.upper_limit,
            CharacteristicWrapper::TypedefCharacteristic(tc) => tc.upper_limit,
        }
    }

    fn kind(&self) -> &str {
        match self {
            CharacteristicWrapper::Characteristic(_) => "CHARACTERISTIC",
            CharacteristicWrapper::TypedefCharacteristic(_) => "TYPEDEF_CHARACTERISTIC",
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    fn check_axis_descr() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" CURVE 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0
                /begin AXIS_DESCR COM_AXIS input_quantity conversion 1 0 100
                    AXIS_PTS_REF axis_points
                    CURVE_AXIS_REF curve_axis
                /end AXIS_DESCR
            /end CHARACTERISTIC
            /begin RECORD_LAYOUT rl
                FNC_VALUES 0 FLOAT32_IEEE ROW_DIR DIRECT
            /end RECORD_LAYOUT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);

        // invalid input quantity, conversion, axis points ref, and curve axis ref
        assert_eq!(log_msgs.len(), 4);

        // error: COM_AXIS should have an AXIS_PTS_REF
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" CURVE 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0
                /begin AXIS_DESCR COM_AXIS NO_INPUT_QUANTITY NO_COMPU_METHOD 1 0 100
                /end AXIS_DESCR
            /end CHARACTERISTIC
            /begin RECORD_LAYOUT rl
                FNC_VALUES 0 FLOAT32_IEEE ROW_DIR DIRECT
            /end RECORD_LAYOUT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        assert_eq!(log_msgs.len(), 1);

        // error: STD_AXIS should not have an AXIS_PTS_REF
        static A2L_TEXT3: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" CURVE 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0
                /begin AXIS_DESCR STD_AXIS meas cm 1 0 100
                    AXIS_PTS_REF axis_points
                /end AXIS_DESCR
            /end CHARACTERISTIC
            /begin AXIS_PTS axis_points "" 0x1234 NO_INPUT_QUANTITY rl 0 NO_COMPU_METHOD 3 0.0 10.0
            /end AXIS_PTS
            /begin RECORD_LAYOUT rl
                FNC_VALUES 0 FLOAT32_IEEE ROW_DIR DIRECT
                AXIS_PTS_X 0 FLOAT32_IEEE INDEX_INCR DIRECT
            /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
            /begin MEASUREMENT meas "" FLOAT32_IEEE cm  1 1.0 0 100
            /end MEASUREMENT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT3, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        // for msg in &log_msgs {
        //     println!("{msg}");
        // }
        assert_eq!(log_msgs.len(), 1);

        // valid input, no errors to report
        static A2L_TEXT4: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" CURVE 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0
                /begin AXIS_DESCR STD_AXIS meas cm 1 0 100
                /end AXIS_DESCR
            /end CHARACTERISTIC
            /begin RECORD_LAYOUT rl
                FNC_VALUES 0 FLOAT32_IEEE ROW_DIR DIRECT
                AXIS_PTS_X 1 FLOAT32_IEEE INDEX_INCR DIRECT
            /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
            /begin MEASUREMENT meas "" FLOAT32_IEEE cm  1 1.0 0 100
            /end MEASUREMENT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT4, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);

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
        let log_msgs = super::check(&a2lfile);

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
        let log_msgs = super::check(&a2lfile);

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
        let log_msgs = super::check(&a2lfile);

        // invalid input quantity, record layout (aka deposit_record), and conversion
        assert_eq!(log_msgs.len(), 3);

        // AXIS_PTS_X is missing from the record layout
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin AXIS_PTS axis_pts_name "" 0x1234 NO_INPUT_QUANTITY rl 0 NO_COMPU_METHOD 3 0.0 10.0
            /end AXIS_PTS
            /begin RECORD_LAYOUT rl
            /end RECORD_LAYOUT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        assert_eq!(log_msgs.len(), 1);

        // valid input, no errors to report
        static A2L_TEXT3: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin AXIS_PTS axis_pts_name "" 0x1234 meas rl 0 cm 3 0.0 10.0
            /end AXIS_PTS
            /begin MEASUREMENT meas "" FLOAT32_IEEE cm 1 1.0 0 100
            /end MEASUREMENT
            /begin RECORD_LAYOUT rl
                AXIS_PTS_X 0 FLOAT32_IEEE INDEX_INCR DIRECT
            /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT3, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_characteristic() {
        // invalid conversion, record layout, comparison quantity, memory segment,
        // dependent characteristic, function, map list, and virtual characteristic
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
        let log_msgs = super::check(&a2lfile);
        assert_eq!(log_msgs.len(), 8);

        // record layout exists, but lacks FNC_VALUES
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" VALUE 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0
            /end CHARACTERISTIC
            /begin RECORD_LAYOUT rl
            /end RECORD_LAYOUT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        assert_eq!(log_msgs.len(), 1);

        // invalid axis descr
        static A2L_TEXT3: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" VALUE 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0
                /begin AXIS_DESCR STD_AXIS NO_INPUT_QUANTITY NO_COMPU_METHOD 1 0 100
                /end AXIS_DESCR
            /end CHARACTERISTIC
            /begin CHARACTERISTIC c2 "" MAP 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0
            /end CHARACTERISTIC
            /begin RECORD_LAYOUT rl
                FNC_VALUES 0 FLOAT32_IEEE ROW_DIR DIRECT
                AXIS_PTS_X 1 FLOAT32_IEEE INDEX_INCR DIRECT
            /end RECORD_LAYOUT
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT3, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        // invalid axis descr for c, no axis descr for c2
        assert_eq!(log_msgs.len(), 2);

        // valid input, no errors to report
        static A2L_TEXT4: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" VALUE 0x1234 rl 0 cm 0.0 1.0
            /end CHARACTERISTIC
            /begin RECORD_LAYOUT rl
                FNC_VALUES 0 FLOAT32_IEEE ROW_DIR DIRECT
            /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT4, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        assert_eq!(log_msgs.len(), 0);
    }

    #[test]
    fn check_typedef_characteristic() {
        // invalid record layout and conversion, additionally the axis descr check reports invalid input quantity and conversion
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_CHARACTERISTIC name "" CURVE record_layout 0 conversion 0 100
                /begin AXIS_DESCR STD_AXIS input_quantity conversion 1 0 100
                /end AXIS_DESCR
            /end TYPEDEF_CHARACTERISTIC
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        assert_eq!(log_msgs.len(), 4);

        // missing axis descr
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_CHARACTERISTIC name "" CUBOID rl 0 NO_COMPU_METHOD 0 100
            /end TYPEDEF_CHARACTERISTIC
            /begin RECORD_LAYOUT rl
                FNC_VALUES 0 FLOAT32_IEEE ROW_DIR DIRECT
            /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        // invalid axis descr
        assert_eq!(log_msgs.len(), 1);

        // valid input, no errors to report
        static A2L_TEXT3: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_CHARACTERISTIC name "" VALUE rl 0 cm 0 100
            /end TYPEDEF_CHARACTERISTIC
            /begin RECORD_LAYOUT rl
                FNC_VALUES 0 FLOAT32_IEEE ROW_DIR DIRECT
            /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit" /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT3, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
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
        let log_msgs = super::check(&a2lfile);

        // invalid compu tab ref, ref unit, and status string ref
        assert_eq!(log_msgs.len(), 3);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin COMPU_METHOD cm "" IDENTICAL "%4.2" "unit"
            /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
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
        let log_msgs = super::check(&a2lfile);

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
        let log_msgs = super::check(&a2lfile);
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
        let log_msgs = super::check(&a2lfile);

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
        let log_msgs = super::check(&a2lfile);
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
        let log_msgs = super::check(&a2lfile);
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
        let log_msgs = super::check(&a2lfile);
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
        let log_msgs = super::check(&a2lfile);

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
        let log_msgs = super::check(&a2lfile);

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
        let log_msgs = super::check(&a2lfile);

        // invalid inverse transformer, transformer in objects, and transformer out objects
        assert_eq!(log_msgs.len(), 3);

        // valid input, no errors to report
        static A2L_TEXT2: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TRANSFORMER transformer_name "version string" "dll32" "dll64" 1 ON_CHANGE NO_INVERSE_TRANSFORMER
            /end TRANSFORMER
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let a2lfile = load_from_string(A2L_TEXT2, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
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
        let log_msgs = super::check(&a2lfile);

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
        let log_msgs = super::check(&a2lfile);

        // invalid component type
        assert_eq!(log_msgs.len(), 1);
    }

    #[test]
    fn check_calc_limits() {
        // Compu method type FORM: limits are set to f64::MIN and f64::MAX
        let cm = CompuMethod::new(
            "".to_string(),
            "".to_string(),
            ConversionType::Form,
            "format".to_string(),
            "unit".to_string(),
        );
        let datatype = DataType::Ubyte;
        let (lower_limit, upper_limit) = super::calc_compu_method_limits(Some(&&cm), datatype);
        // FORM conversion type: any value is allowed, because the formula is not evaluated
        assert_eq!(lower_limit, f64::MIN);
        assert_eq!(upper_limit, f64::MAX);

        // Compu method type LINEAR: limits are calculated
        let mut cm = CompuMethod::new(
            "".to_string(),
            "".to_string(),
            ConversionType::Linear,
            "format".to_string(),
            "unit".to_string(),
        );
        cm.coeffs_linear = Some(CoeffsLinear::new(33.0, 2.0));
        let datatype = DataType::Sbyte;
        let (lower_limit, upper_limit) = super::calc_compu_method_limits(Some(&&cm), datatype);
        // linear conversion type: lower and upper limits are physical values
        assert_eq!(lower_limit, 33.0 * -128.0 + 2.0);
        assert_eq!(upper_limit, 33.0 * 127.0 + 2.0);

        // Compu method type RAT_FUNC, with a restricted formula: limits are calculated
        let mut cm = CompuMethod::new(
            "".to_string(),
            "".to_string(),
            ConversionType::RatFunc,
            "format".to_string(),
            "unit".to_string(),
        );
        cm.coeffs = Some(Coeffs::new(0.0, 3.5, 4.44, 0.0, 0.0, 2.0));
        let datatype = DataType::Slong;
        let (lower_limit, upper_limit) = super::calc_compu_method_limits(Some(&&cm), datatype);
        // rational function conversion type: lower and upper limits are physical values
        // y = (3.5x + 4.44) / 2 -> x = (2y - 4.44) / 3.5
        assert_eq!(lower_limit, (2.0 * -2147483648.0 - 4.44) / 3.5);
        assert_eq!(upper_limit, (2.0 * 2147483647.0 - 4.44) / 3.5);

        // Compu method type RAT_FUNC, with a restricted formula for f64 values: limits are calculated
        // In this calculation, the value could go to INFINITY if the operations are done in the wrong order
        let mut cm = CompuMethod::new(
            "".to_string(),
            "".to_string(),
            ConversionType::RatFunc,
            "format".to_string(),
            "unit".to_string(),
        );
        cm.coeffs = Some(Coeffs::new(0.0, -3.5, 4.44, 0.0, 0.0, 2.0));
        let datatype = DataType::Float64Ieee;
        let (lower_limit, upper_limit) = super::calc_compu_method_limits(Some(&&cm), datatype);
        // min and max values are not f64::MIN, f64::MAX, -f64::INFINITY or f64::INFINITY
        assert_ne!(lower_limit, f64::MIN);
        assert_ne!(upper_limit, f64::MAX);
        assert_ne!(lower_limit, -f64::INFINITY);
        assert_ne!(upper_limit, f64::INFINITY);

        // Compu method type RAT_FUNC, with a full formula: limits are set to f64::MIN and f64::MAX
        let mut cm = CompuMethod::new(
            "".to_string(),
            "".to_string(),
            ConversionType::RatFunc,
            "format".to_string(),
            "unit".to_string(),
        );
        cm.coeffs = Some(Coeffs::new(1.0, 3.5, 4.44, 2.0, 3.0, 4.0));
        let datatype = DataType::Slong;
        let (lower_limit, upper_limit) = super::calc_compu_method_limits(Some(&&cm), datatype);
        assert_eq!(lower_limit, f64::MIN);
        assert_eq!(upper_limit, f64::MAX);

        // Compu method type IDENTICAL: limits are set to the datatype limits
        let cm = CompuMethod::new(
            "".to_string(),
            "".to_string(),
            ConversionType::Identical,
            "format".to_string(),
            "unit".to_string(),
        );
        let datatype = DataType::Float32Ieee;
        let (lower_limit, upper_limit) = super::calc_compu_method_limits(Some(&&cm), datatype);
        // identical conversion type: any value is allowed, because the compu method does not modify the limits given by the datatype
        assert_eq!(lower_limit, f32::MIN as f64);
        assert_eq!(upper_limit, f32::MAX as f64);
    }

    #[test]
    fn check_limits() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin CHARACTERISTIC c "" CURVE 0x1234 rl 0 cm 0 100000
                /begin AXIS_DESCR STD_AXIS NO_INPUT_QUANTITY NO_COMPU_METHOD 1 -1E39 1E39
                /end AXIS_DESCR
            /end CHARACTERISTIC
            /begin AXIS_PTS axis_points "" 0x1234 NO_INPUT_QUANTITY rl2 0 cm2 3 -100000.0 100000.0
            /end AXIS_PTS
            /begin MEASUREMENT m "" ULONG cm3 1 1.0 -1000000 1000000
            /end MEASUREMENT
            /begin RECORD_LAYOUT rl
                FNC_VALUES 0 UWORD ROW_DIR DIRECT
                AXIS_PTS_X 0 FLOAT32_IEEE INDEX_INCR DIRECT
            /end RECORD_LAYOUT
            /begin RECORD_LAYOUT rl2
                AXIS_PTS_X 0 SWORD INDEX_INCR DIRECT
            /end RECORD_LAYOUT
            /begin COMPU_METHOD cm "" LINEAR "%4.2" "unit"
                COEFFS_LINEAR 0.5 0
            /end COMPU_METHOD
            /begin COMPU_METHOD cm2 "" RAT_FUNC "%4.2" "unit"
                COEFFS 0 500 4000 0 0 2.222
            /end COMPU_METHOD
            /begin COMPU_METHOD cm3 "" IDENTICAL "%4.2" "unit"
            /end COMPU_METHOD
        /end MODULE /end PROJECT"#;
        let mut load_errors = Vec::new();
        let mut a2lfile = load_from_string(A2L_TEXT, None, &mut load_errors, true).unwrap();
        let log_msgs = super::check(&a2lfile);
        // invalid limits for each of CHARACTERISTIC, AXIS_DESCR, AXIS_PTS, and MEASUREMENT
        assert_eq!(log_msgs.len(), 4);

        // fix the errors in each item, now no errors should be reported
        a2lfile.project.module[0].characteristic[0].upper_limit = u16::MAX as f64 * 0.5;
        a2lfile.project.module[0].characteristic[0].axis_descr[0].upper_limit = f32::MAX as f64;
        a2lfile.project.module[0].characteristic[0].axis_descr[0].lower_limit = f32::MIN as f64;
        a2lfile.project.module[0].axis_pts[0].upper_limit =
            ((i16::MAX as f64 * 2.222) - 4000.0) / 500.0;
        a2lfile.project.module[0].axis_pts[0].lower_limit =
            ((i16::MIN as f64 * 2.222) - 4000.0) / 500.0;
        a2lfile.project.module[0].measurement[0].lower_limit = u32::MIN as f64;
        let log_msgs = super::check(&a2lfile);
        assert_eq!(log_msgs.len(), 0);
    }
}
