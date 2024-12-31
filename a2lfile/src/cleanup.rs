use crate::specification::A2lFile;

mod compu_methods;
mod functions;
mod groups;
mod record_layouts;

pub(crate) fn cleanup(a2l_file: &mut A2lFile) {
    for module in &mut a2l_file.project.module {
        groups::cleanup(module);
        functions::cleanup(module);
        compu_methods::cleanup(module);
        record_layouts::cleanup(module);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_cleanup() {
        let a2l_text = r#"
        ASAP2_VERSION 1 71
        /begin PROJECT project "" /begin MODULE module ""
            /begin MEASUREMENT measurement1 "" FLOAT32_IEEE compu_method_used1 1 1.0 0 100
                /begin FUNCTION_LIST
                    function1 nonexistent_function
                /end FUNCTION_LIST
            /end MEASUREMENT
            /begin MEASUREMENT measurement2 "" FLOAT32_IEEE nonexistent_compu_method 1 1.0 0 100
            /end MEASUREMENT
            /begin CHARACTERISTIC characteristic1 "" CURVE 0x1234 record_layout 0 compu_method_used2 0.0 1.0
                /begin FUNCTION_LIST
                    function1 nonexistent_function
                /end FUNCTION_LIST
            /end CHARACTERISTIC
            /begin CHARACTERISTIC characteristic2 "" CURVE 0x1234 record_layout 0 nonexistent_compu_method 0.0 1.0
                /begin AXIS_DESCR COM_AXIS measurement1 compu_method_used4 1 0 100
                /end AXIS_DESCR
                /begin AXIS_DESCR COM_AXIS measurement1 nonexistent_compu_method 1 0 100
                /end AXIS_DESCR
            /end CHARACTERISTIC
            /begin AXIS_PTS axispts1 "" 0x1234 measurement1 record_layout 0 compu_method_used3 3 0.0 10.0
                /begin FUNCTION_LIST
                    function1 nonexistent_function
                /end FUNCTION_LIST
            /end AXIS_PTS
            /begin AXIS_PTS axispts1 "" 0x1234 measurement1 record_layout 0 nonexistent_compu_method 3 0.0 10.0
            /end AXIS_PTS

            /begin USER_RIGHTS user1
                /begin REF_GROUP user_rights_group /end REF_GROUP
            /end USER_RIGHTS

            /begin GROUP valid_group ""
                /begin REF_MEASUREMENT
                    measurement1 invalid_measurement
                /end REF_MEASUREMENT
                /begin REF_CHARACTERISTIC
                    characteristic1 invalid_characteristic
                /end REF_CHARACTERISTIC
                /begin FUNCTION_LIST
                    function1 nonexistent_function
                /end FUNCTION_LIST
                /begin SUB_GROUP
                    empty_sub_group
                /end SUB_GROUP
            /end GROUP
            /begin GROUP invalid_group ""
                /begin REF_MEASUREMENT
                    invalid_measurement
                /end REF_MEASUREMENT
                /begin REF_CHARACTERISTIC
                    invalid_characteristic
                /end REF_CHARACTERISTIC
            /end GROUP
            /begin GROUP empty_sub_group ""
            /end GROUP
            /begin GROUP user_rights_group ""
            /end GROUP

            /begin FUNCTION function1 ""
            /end FUNCTION
            /begin FUNCTION unused_function ""
                /begin SUB_FUNCTION
                    unused_sub_function
                /end SUB_FUNCTION
            /end FUNCTION
            /begin FUNCTION unused_sub_function ""
            /end FUNCTION

            /begin COMPU_METHOD compu_method_used1 "" TAB_NOINTP "%6.3" ""
                COMPU_TAB_REF compu_tab_used
                REF_UNIT used_unit
            /end COMPU_METHOD
            /begin COMPU_METHOD compu_method_used2 "" TAB_NOINTP "%6.3" ""
                COMPU_TAB_REF compu_vtab_used
            /end COMPU_METHOD
            /begin COMPU_METHOD compu_method_used3 "" TAB_NOINTP "%6.3" ""
                COMPU_TAB_REF compu_vtab_range_used
            /end COMPU_METHOD
            /begin COMPU_METHOD compu_method_used4 "" TAB_NOINTP "%6.3" ""
                COMPU_TAB_REF invalid_compu_tab
                REF_UNIT invalid_unit
            /end COMPU_METHOD
            /begin COMPU_METHOD compu_method_unused1 "" TAB_NOINTP "%6.3" ""
                COMPU_TAB_REF compu_tab_unused
            /end COMPU_METHOD
            /begin COMPU_METHOD compu_method_unused2 "" TAB_NOINTP "%6.3" ""
                COMPU_TAB_REF compu_vtab_unused
            /end COMPU_METHOD
            /begin COMPU_METHOD compu_method_unused3 "" TAB_NOINTP "%6.3" ""
                COMPU_TAB_REF compu_vtab_range_unused
            /end COMPU_METHOD
            /begin COMPU_TAB compu_tab_used "" TAB_NOINTP 1
                1 1
            /end COMPU_TAB
            /begin COMPU_TAB compu_tab_unused "" TAB_NOINTP 1
                1 1
            /end COMPU_TAB
            /begin COMPU_VTAB compu_vtab_used "" TAB_VERB 1
                1 "v"
            /end COMPU_VTAB
            /begin COMPU_VTAB compu_vtab_unused "" TAB_VERB 1
                1 "v"
            /end COMPU_VTAB
            /begin COMPU_VTAB_RANGE compu_vtab_range_used "" 1
                1 2 "v"
            /end COMPU_VTAB_RANGE
            /begin COMPU_VTAB_RANGE compu_vtab_range_unused "" 1
                1 2 "v"
            /end COMPU_VTAB_RANGE
            /begin UNIT used_unit "" "x" DERIVED
            /end UNIT
            /begin UNIT unused_unit "" "x" DERIVED
            /end UNIT
        /end MODULE
        /end PROJECT"#;

        let mut log_msgs = Vec::new();
        let mut a2l_file = crate::load_from_string(a2l_text, None, &mut log_msgs, true).unwrap();
        cleanup(&mut a2l_file);

        let module = &a2l_file.project.module[0];
        let (namemap, _) = module.build_namemap();

        // MEASUREMENTs are not removed during cleanup, so measurement_1 should still be present
        assert!(namemap.object.contains_key("measurement1"));
        let measurement1 = &module.measurement[0];
        let measurement2 = &module.measurement[1];
        // CHARACTERISTICs are not removed during cleanup, so characteristic1 should still be present
        assert!(namemap.object.contains_key("characteristic1"));
        let characteristic1 = &module.characteristic[0];
        let characteristic2 = &module.characteristic[1];
        // AXIS_PTS are not removed during cleanup, so axispts1 should still be present
        assert!(namemap.object.contains_key("axispts1"));
        let axispts1 = &module.axis_pts[0];
        let axispts2 = &module.axis_pts[1];

        // ====== Groups ======

        // valid_group references measurement_1, so it is not empty and should still be present
        let valid_group = namemap.group.get("valid_group").unwrap();
        // valid_group also references invalid measurement and characteristic, which should both be removed
        let ref_measurement = valid_group.ref_measurement.as_ref().unwrap();
        assert_eq!(ref_measurement.identifier_list, vec!["measurement1"]);
        let ref_characteristic = valid_group.ref_characteristic.as_ref().unwrap();
        assert_eq!(ref_characteristic.identifier_list, vec!["characteristic1"]);

        // invalid_group references an invalid measurement. This invalid reference is deleted during cleanup
        // since invalid_group would be empty, it should be removed
        assert!(!namemap.group.contains_key("invalid_group"));

        // user_rights_group is empty, but it is used by USER_RIGHTS for user1, so it should not be removed
        assert!(namemap.group.contains_key("user_rights_group"));

        // valid_group references the empty group empty_sub_group. The empty group should be removed, and valid_group should be updated
        assert_eq!(valid_group.sub_group, None);
        assert!(!namemap.group.contains_key("empty_sub_group"));

        // ====== Functions ======

        // function1 is used by measurement1, characteristic1, axispts1, and valid_group
        assert!(namemap.function.contains_key("function1"));
        // unused_function is not used and should be removed
        assert!(!namemap.function.contains_key("unused_function"));
        // unused_sub_function is not used after the removal of unused_function and should be removed
        assert!(!namemap.function.contains_key("unused_sub_function"));

        // the FUNCTION_LIST for measurement1, characteristic1, and axispts1 all reference function1
        // which is valid, so function1 should not be removed. nonexistent_function is not used and should be removed
        let meas_func_list = &measurement1.function_list.as_ref().unwrap();
        assert_eq!(meas_func_list.name_list, vec!["function1"]);
        let char_func_list = &characteristic1.function_list.as_ref().unwrap();
        assert_eq!(char_func_list.name_list, vec!["function1"]);
        let axis_func_list = &axispts1.function_list.as_ref().unwrap();
        assert_eq!(axis_func_list.name_list, vec!["function1"]);
        let grp_func_list = &valid_group.function_list.as_ref().unwrap();
        assert_eq!(grp_func_list.name_list, vec!["function1"]);

        // ====== Compu Methods ======

        // compu_method_used1 is used by measurement1
        assert!(namemap.compu_method.contains_key("compu_method_used1"));
        // compu_method_used2 is used by characteristic1
        assert!(namemap.compu_method.contains_key("compu_method_used2"));
        // compu_method_used3 is used by axispts1
        assert!(namemap.compu_method.contains_key("compu_method_used3"));
        // compu_method_used4 is used by the AXIS_DESCR in characteristic2
        assert!(namemap.compu_method.contains_key("compu_method_used4"));
        // compu_method_unused1 is not used and should be removed
        assert!(!namemap.compu_method.contains_key("compu_method_unused1"));
        // compu_method_unused2 is not used and should be removed
        assert!(!namemap.compu_method.contains_key("compu_method_unused2"));
        // compu_method_unused3 is not used and should be removed
        assert!(!namemap.compu_method.contains_key("compu_method_unused3"));

        // the references to compu_method_used are retained in the MEASUREMENT, CHARACTERISTIC, and AXIS_PTS
        assert_eq!(measurement1.conversion, "compu_method_used1");
        assert_eq!(characteristic1.conversion, "compu_method_used2");
        assert_eq!(axispts1.conversion, "compu_method_used3");

        // nonexistent_compu_method does not exist, and references to it should be removed
        assert_eq!(measurement2.conversion, "NO_COMPU_METHOD");
        assert_eq!(characteristic2.conversion, "NO_COMPU_METHOD");
        assert_eq!(axispts2.conversion, "NO_COMPU_METHOD");

        // compu_tab_used is used by compu_method_used1
        assert!(namemap.compu_tab.contains_key("compu_tab_used"));
        // compu_tab_unused is not used and should be removed
        assert!(!namemap.compu_tab.contains_key("compu_tab_unused"));
        // compu_vtab_used is used by compu_method_used2
        assert!(namemap.compu_tab.contains_key("compu_vtab_used"));
        // compu_vtab_unused is not used and should be removed
        assert!(!namemap.compu_tab.contains_key("compu_vtab_unused"));
        // compu_vtab_range_used is used by compu_method_used3
        assert!(namemap.compu_tab.contains_key("compu_vtab_range_used"));
        // compu_vtab_range_unused is not used and should be removed
        assert!(!namemap.compu_tab.contains_key("compu_vtab_range_unused"));

        // used_unit is used by compu_method_used1
        assert!(namemap.unit.contains_key("used_unit"));
        // unused_unit is not used and should be removed
        assert!(!namemap.unit.contains_key("unused_unit"));

        let compu_method_used4 = &module.compu_method[3];
        // invalid_compu_tab does not exist, and references to it should be removed
        assert_eq!(compu_method_used4.compu_tab_ref, None);
        // invalid_unit does not exist, and references to it should be removed
        assert_eq!(compu_method_used4.ref_unit, None);
    }
}
