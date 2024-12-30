use std::collections::HashMap;

use crate::specification::*;

#[derive(Debug, PartialEq)]
pub enum NameMapObject<'a> {
    Measurement(&'a Measurement),
    Characteristic(&'a Characteristic),
    AxisPts(&'a AxisPts),
    Blob(&'a Blob),
    Instance(&'a Instance),
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, PartialEq)]
pub enum NameMapCompuTab<'a> {
    CompuTab(&'a CompuTab),
    CompuVtab(&'a CompuVtab),
    CompuVtabRange(&'a CompuVtabRange),
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, PartialEq)]
pub enum NameMapTypedef<'a> {
    TypedefBlob(&'a TypedefBlob),
    TypedefAxis(&'a TypedefAxis),
    TypedefMeasurement(&'a TypedefMeasurement),
    TypedefCharacteristic(&'a TypedefCharacteristic),
    TypedefStructure(&'a TypedefStructure),
}

macro_rules! check_and_insert {
    ( $hash:expr, $key:expr, $item:expr, $log_msgs:expr, $blockname:expr ) => {
        if let Some(existing_val) = $hash.get(&$key) {
            $log_msgs.push(format!(
                "Name collision: The {} blocks on line {} and {} both use the name {}",
                $blockname,
                existing_val.get_line(),
                $item.get_line(),
                $key
            ));
        } else {
            $hash.insert($key, $item);
        }
    };
}

macro_rules! check_and_insert_multi {
    ( $hash:expr, $key:expr, $item:expr, $log_msgs:expr, $blockname:expr ) => {
        if let Some(existing_val) = $hash.get(&$key) {
            $log_msgs.push(format!("Name collision: The block {} on line {} and the block {} on line {} both use the name {}",
                existing_val.get_blockname(), existing_val.get_line(), $blockname, $item.get_line(), $key));
        } else {
            $hash.insert($key, $item);
        }
    }
}

/// `ModuleNameMap` collects references to all items with a [Module] into `HashMaps`, making it possible to access them all by name.
///
/// There are several name spaces per module, each stored in a different `HashMap` in the `ModuleNameMap`:
/// - `object`: `CHARACTERISTIC`, `MEASUREMENT`, `AXIS_PTS`, `BLOB`, `INSTANCE`
/// - `compu_method`: `COMPU_METHOD`
/// - `compu_tab`: `COMPU_VTAB`, `COMPU_VTAB_RANGE`, `COMPU_TAB`
/// - `frame`: `FRAME`
/// - `function`: `FUNCTION`
/// - `group`: `GROUP`
/// - `memory_segment`: `MEMORY_SEGMENT`
/// - `record_layout`: `RECORD_LAYOUT`
/// - `transformer`: `TRANSFORMER`
/// - `typedef`: `TYPEDEF_AXIS`, `TYPEDEF_BLOB`, `TYPEDEF_CHARACTERISTIC`, `TYPEDEF_INSTANCE`, `TYPEDEF_MEASUREMENT`
/// - `unit`: `UNIT`
/// - `variant`: `VARIANT`
///
/// While the `ModuleNameMap` is holding these references, the borrow checker will prevent any of these items from being modified.
#[derive(Debug, PartialEq)]
pub struct ModuleNameMap<'a> {
    pub compu_method: HashMap<String, &'a CompuMethod>,
    pub compu_tab: HashMap<String, NameMapCompuTab<'a>>,
    pub frame: HashMap<String, &'a Frame>,
    pub function: HashMap<String, &'a Function>,
    pub group: HashMap<String, &'a Group>,
    pub memory_segment: HashMap<String, &'a MemorySegment>,
    pub object: HashMap<String, NameMapObject<'a>>,
    pub record_layout: HashMap<String, &'a RecordLayout>,
    pub transformer: HashMap<String, &'a Transformer>,
    pub typedef: HashMap<String, NameMapTypedef<'a>>,
    pub unit: HashMap<String, &'a Unit>,
    pub variant: HashMap<String, &'a VarCriterion>,
}

impl<'a> ModuleNameMap<'a> {
    /// build a new `ModuleNameMap` for the given [Module]
    pub fn build(module: &'a Module, log_msgs: &mut Vec<String>) -> Self {
        Self {
            compu_method: build_namemap_compu_method(module, log_msgs),
            compu_tab: build_namemap_compu_tab(module, log_msgs),
            frame: build_namemap_frame(module, log_msgs),
            function: build_namemap_function(module, log_msgs),
            group: build_namemap_group(module, log_msgs),
            memory_segment: build_namemap_memory_segment(module, log_msgs),
            object: build_namemap_object(module, log_msgs),
            record_layout: build_namemap_record_layout(module, log_msgs),
            transformer: build_namemap_transformer(module, log_msgs),
            typedef: build_namemap_typedef(module, log_msgs),
            unit: build_namemap_unit(module, log_msgs),
            variant: build_namemap_variant(module, log_msgs),
        }
    }
}

pub(crate) fn build_namemap_unit<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, &'a Unit> {
    let mut namelist_unit = HashMap::<String, &'a Unit>::new();
    for unit in &module.unit {
        check_and_insert!(namelist_unit, unit.name.clone(), unit, log_msgs, "UNIT");
    }
    namelist_unit
}

pub(crate) fn build_namemap_typedef<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, NameMapTypedef<'a>> {
    let mut namelist_typedef = HashMap::<String, NameMapTypedef<'a>>::new();
    for typedef_axis in &module.typedef_axis {
        check_and_insert_multi!(
            namelist_typedef,
            typedef_axis.name.clone(),
            NameMapTypedef::TypedefAxis(typedef_axis),
            log_msgs,
            "TYPEDEF_AXIS"
        );
    }
    for typedef_blob in &module.typedef_blob {
        check_and_insert_multi!(
            namelist_typedef,
            typedef_blob.name.clone(),
            NameMapTypedef::TypedefBlob(typedef_blob),
            log_msgs,
            "TYPEDEF_BLOB"
        );
    }
    for typedef_characteristic in &module.typedef_characteristic {
        check_and_insert_multi!(
            namelist_typedef,
            typedef_characteristic.name.clone(),
            NameMapTypedef::TypedefCharacteristic(typedef_characteristic),
            log_msgs,
            "TYPEDEF_CHARACTERISTIC"
        );
    }
    for typedef_measurement in &module.typedef_measurement {
        check_and_insert_multi!(
            namelist_typedef,
            typedef_measurement.name.clone(),
            NameMapTypedef::TypedefMeasurement(typedef_measurement),
            log_msgs,
            "TYPEDEF_MEASUREMENT"
        );
    }
    for typedef_structure in &module.typedef_structure {
        check_and_insert_multi!(
            namelist_typedef,
            typedef_structure.name.clone(),
            NameMapTypedef::TypedefStructure(typedef_structure),
            log_msgs,
            "TYPEDEF_STRUCTURE"
        );
    }
    namelist_typedef
}

pub(crate) fn build_namemap_record_layout<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, &'a RecordLayout> {
    let mut namelist_record_layout = HashMap::<String, &'a RecordLayout>::new();
    for record_layout in &module.record_layout {
        check_and_insert!(
            namelist_record_layout,
            record_layout.name.clone(),
            record_layout,
            log_msgs,
            "RECORD_LAYOUT"
        );
    }
    namelist_record_layout
}

pub(crate) fn build_namemap_memory_segment<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, &'a MemorySegment> {
    let mut namelist_memory_segment = HashMap::<String, &'a MemorySegment>::new();
    if let Some(mod_par) = &module.mod_par {
        for memory_segment in &mod_par.memory_segment {
            check_and_insert!(
                namelist_memory_segment,
                memory_segment.name.clone(),
                memory_segment,
                log_msgs,
                "MEMORY_SEGMENT"
            );
        }
    }
    namelist_memory_segment
}

pub(crate) fn build_namemap_compu_tab<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, NameMapCompuTab<'a>> {
    let mut namelist_compu_tab = HashMap::<String, NameMapCompuTab<'a>>::new();
    for compu_tab in &module.compu_tab {
        check_and_insert_multi!(
            namelist_compu_tab,
            compu_tab.name.clone(),
            NameMapCompuTab::CompuTab(compu_tab),
            log_msgs,
            "COMPU_TAB"
        );
    }
    for compu_vtab in &module.compu_vtab {
        check_and_insert_multi!(
            namelist_compu_tab,
            compu_vtab.name.clone(),
            NameMapCompuTab::CompuVtab(compu_vtab),
            log_msgs,
            "COMPU_VTAB"
        );
    }
    for compu_vtab_range in &module.compu_vtab_range {
        check_and_insert_multi!(
            namelist_compu_tab,
            compu_vtab_range.name.clone(),
            NameMapCompuTab::CompuVtabRange(compu_vtab_range),
            log_msgs,
            "COMPU_VTAB_RANGE"
        );
    }
    namelist_compu_tab
}

pub(crate) fn build_namemap_compu_method<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, &'a CompuMethod> {
    let mut namelist_compu_method = HashMap::<String, &'a CompuMethod>::new();
    for compu_method in &module.compu_method {
        check_and_insert!(
            namelist_compu_method,
            compu_method.name.clone(),
            compu_method,
            log_msgs,
            "COMPU_METHOD"
        );
    }
    namelist_compu_method
}

pub(crate) fn build_namemap_transformer<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, &'a Transformer> {
    let mut namelist_transformer = HashMap::<String, &'a Transformer>::new();
    for transformer in &module.transformer {
        check_and_insert!(
            namelist_transformer,
            transformer.name.clone(),
            transformer,
            log_msgs,
            "TRANSFORMER"
        );
    }
    namelist_transformer
}

pub(crate) fn build_namemap_object<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, NameMapObject<'a>> {
    let mut namelist_object = HashMap::<String, NameMapObject<'a>>::new();
    for measurement in &module.measurement {
        check_and_insert_multi!(
            namelist_object,
            measurement.name.clone(),
            NameMapObject::Measurement(measurement),
            log_msgs,
            "MEASUREMENT"
        );
    }
    for characteristic in &module.characteristic {
        check_and_insert_multi!(
            namelist_object,
            characteristic.name.clone(),
            NameMapObject::Characteristic(characteristic),
            log_msgs,
            "CHARACTERISTIC"
        );
    }
    for axis_pts in &module.axis_pts {
        check_and_insert_multi!(
            namelist_object,
            axis_pts.name.clone(),
            NameMapObject::AxisPts(axis_pts),
            log_msgs,
            "AXIS_PTS"
        );
    }
    for blob in &module.blob {
        check_and_insert_multi!(
            namelist_object,
            blob.name.clone(),
            NameMapObject::Blob(blob),
            log_msgs,
            "BLOB"
        );
    }
    for instance in &module.instance {
        check_and_insert_multi!(
            namelist_object,
            instance.name.clone(),
            NameMapObject::Instance(instance),
            log_msgs,
            "INSTANCE"
        );
    }
    namelist_object
}

pub(crate) fn build_namemap_variant<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, &'a VarCriterion> {
    let mut namelist_variant = HashMap::<String, &'a VarCriterion>::new();
    if let Some(variant_coding) = &module.variant_coding {
        for var_criterion in &variant_coding.var_criterion {
            check_and_insert!(
                namelist_variant,
                var_criterion.name.clone(),
                var_criterion,
                log_msgs,
                "VAR_CRITERION"
            );
        }
    }
    namelist_variant
}

pub(crate) fn build_namemap_group<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, &'a Group> {
    let mut namelist_group = HashMap::<String, &'a Group>::new();
    for group in &module.group {
        check_and_insert!(namelist_group, group.name.clone(), group, log_msgs, "GROUP");
    }
    namelist_group
}

pub(crate) fn build_namemap_frame<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, &'a Frame> {
    let mut namelist_frame = HashMap::<String, &'a Frame>::new();
    for frame in &module.frame {
        check_and_insert!(namelist_frame, frame.name.clone(), frame, log_msgs, "FRAME");
    }
    namelist_frame
}

pub(crate) fn build_namemap_function<'a>(
    module: &'a Module,
    log_msgs: &mut Vec<String>,
) -> HashMap<String, &'a Function> {
    let mut namelist_function = HashMap::<String, &'a Function>::new();
    for function in &module.function {
        check_and_insert!(
            namelist_function,
            function.name.clone(),
            function,
            log_msgs,
            "FUNCTION"
        );
    }
    namelist_function
}

impl NameMapObject<'_> {
    pub(crate) fn get_line(&self) -> u32 {
        match self {
            Self::AxisPts(axis) => axis.get_line(),
            Self::Blob(blob) => blob.get_line(),
            Self::Characteristic(characteristic) => characteristic.get_line(),
            Self::Instance(instance) => instance.get_line(),
            Self::Measurement(measurement) => measurement.get_line(),
        }
    }

    fn get_blockname(&self) -> &'static str {
        match self {
            Self::AxisPts(_) => "AXIS_PTS",
            Self::Blob(_) => "BLOB",
            Self::Characteristic(_) => "CHARACTERISTIC",
            Self::Instance(_) => "INSTANCE",
            Self::Measurement(_) => "MEASUREMENT",
        }
    }
}

impl NameMapCompuTab<'_> {
    pub(crate) fn get_line(&self) -> u32 {
        match self {
            Self::CompuTab(computab) => computab.get_line(),
            Self::CompuVtab(compuvtab) => compuvtab.get_line(),
            Self::CompuVtabRange(compuvtabrange) => compuvtabrange.get_line(),
        }
    }

    fn get_blockname(&self) -> &'static str {
        match self {
            Self::CompuTab(_) => "COMPU_TAB",
            Self::CompuVtab(_) => "COMPU_VTAB",
            Self::CompuVtabRange(_) => "COMPU_VTAB_RANGE",
        }
    }
}

impl NameMapTypedef<'_> {
    pub(crate) fn get_line(&self) -> u32 {
        match self {
            Self::TypedefAxis(axis) => axis.get_line(),
            Self::TypedefBlob(blob) => blob.get_line(),
            Self::TypedefCharacteristic(characteristic) => characteristic.get_line(),
            Self::TypedefMeasurement(measurement) => measurement.get_line(),
            Self::TypedefStructure(structure) => structure.get_line(),
        }
    }

    fn get_blockname(&self) -> &'static str {
        match self {
            Self::TypedefAxis(_) => "TYPEDEF_AXIS",
            Self::TypedefBlob(_) => "TYPEDEF_BLOB",
            Self::TypedefCharacteristic(_) => "TYPEDEF_CHARACTERISTIC",
            Self::TypedefMeasurement(_) => "TYPEDEF_MEASUREMENT",
            Self::TypedefStructure(_) => "TYPEDEF_STRUCTURE",
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{CompuMethod, NameMapTypedef, Transformer, Unit};

    #[test]
    fn test_namemap_unit() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin UNIT unit1 "" "x" DERIVED
                /end UNIT
                /begin UNIT unit1 "" "x" DERIVED
                /end UNIT
                /begin UNIT unit2 "" "x" EXTENDED_SI
                /end UNIT
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_unit = build_namemap_unit(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
        assert_eq!(namemap_unit.len(), 2);
        assert!(matches!(namemap_unit.get("unit1"), Some(Unit { .. })));
        assert!(matches!(namemap_unit.get("unit2"), Some(Unit { .. })));
    }

    #[test]
    fn test_namemap_typedef() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin TYPEDEF_AXIS axis1 "" measurement_name record_layout_name 0 compu_method_name 1 0 100
                /end TYPEDEF_AXIS
                /begin TYPEDEF_AXIS axis1 "" measurement_name record_layout_name 0 compu_method_name 1 0 100
                /end TYPEDEF_AXIS
                /begin TYPEDEF_BLOB blob1 "" 0
                /end TYPEDEF_BLOB
                /begin TYPEDEF_BLOB blob1 "" 0
                /end TYPEDEF_BLOB
                /begin TYPEDEF_CHARACTERISTIC char1 "" VALUE record_layout_name 0 compu_method_name 0 100
                /end TYPEDEF_CHARACTERISTIC
                /begin TYPEDEF_CHARACTERISTIC char1 "" VALUE record_layout_name 0 compu_method_name 0 100
                /end TYPEDEF_CHARACTERISTIC
                /begin TYPEDEF_MEASUREMENT meas1 "" UBYTE compu_method_name 1 1 0 100
                /end TYPEDEF_MEASUREMENT
                /begin TYPEDEF_MEASUREMENT meas1 "" UBYTE compu_method_name 1 1 0 100
                /end TYPEDEF_MEASUREMENT
                /begin TYPEDEF_STRUCTURE struct1 "" 0
                /end TYPEDEF_STRUCTURE
                /begin TYPEDEF_STRUCTURE struct1 "" 0
                /end TYPEDEF_STRUCTURE
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_typedef = build_namemap_typedef(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 5);
        assert_eq!(namemap_typedef.len(), 5);
        assert!(matches!(
            namemap_typedef.get("axis1"),
            Some(NameMapTypedef::TypedefAxis(_))
        ));
        assert!(matches!(
            namemap_typedef.get("blob1"),
            Some(NameMapTypedef::TypedefBlob(_))
        ));
        assert!(matches!(
            namemap_typedef.get("char1"),
            Some(NameMapTypedef::TypedefCharacteristic(_))
        ));
        assert!(matches!(
            namemap_typedef.get("meas1"),
            Some(NameMapTypedef::TypedefMeasurement(_))
        ));
        assert!(matches!(
            namemap_typedef.get("struct1"),
            Some(NameMapTypedef::TypedefStructure(_))
        ));
    }

    #[test]
    fn test_namemap_record_layout() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin RECORD_LAYOUT layout1
                /end RECORD_LAYOUT
                /begin RECORD_LAYOUT layout1
                /end RECORD_LAYOUT
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_record_layout =
            build_namemap_record_layout(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
        assert_eq!(namemap_record_layout.len(), 1);
        assert!(matches!(
            namemap_record_layout.get("layout1"),
            Some(RecordLayout { .. })
        ));
    }

    #[test]
    fn test_namemap_memory_segment() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin MOD_PAR ""
                    /begin MEMORY_SEGMENT mem1 "" DATA RAM EXTERN 0 0 0 0 0 0 0
                    /end MEMORY_SEGMENT
                    /begin MEMORY_SEGMENT mem1 "" DATA RAM EXTERN 0 0 0 0 0 0 0
                    /end MEMORY_SEGMENT
                /end MOD_PAR
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_memory_segment =
            build_namemap_memory_segment(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
        assert_eq!(namemap_memory_segment.len(), 1);
        assert!(matches!(
            namemap_memory_segment.get("mem1"),
            Some(MemorySegment { .. })
        ));
    }

    #[test]
    fn test_namemap_compu_tab() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin COMPU_TAB tab1 "" IDENTICAL 0
                /end COMPU_TAB
                /begin COMPU_TAB tab1 "" IDENTICAL 0
                /end COMPU_TAB
                /begin COMPU_VTAB vtab1 "" IDENTICAL 0
                /end COMPU_VTAB
                /begin COMPU_VTAB vtab1 "" IDENTICAL 0
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE vtabrange1 "" 0
                /end COMPU_VTAB_RANGE
                /begin COMPU_VTAB_RANGE vtabrange1 "" 0
                /end COMPU_VTAB_RANGE
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_compu_tab = build_namemap_compu_tab(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 3);
        assert_eq!(namemap_compu_tab.len(), 3);
        assert!(matches!(
            namemap_compu_tab.get("tab1"),
            Some(crate::NameMapCompuTab::CompuTab(_))
        ));
        assert!(matches!(
            namemap_compu_tab.get("vtab1"),
            Some(crate::NameMapCompuTab::CompuVtab(_))
        ));
        assert!(matches!(
            namemap_compu_tab.get("vtabrange1"),
            Some(crate::NameMapCompuTab::CompuVtabRange(_))
        ));
    }

    #[test]
    fn test_namemap_compu_method() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin COMPU_METHOD method1 "" IDENTICAL "%4.2" "unit"
                /end COMPU_METHOD
                /begin COMPU_METHOD method1 "" IDENTICAL "%4.2" "unit"
                /end COMPU_METHOD
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_compu_method =
            build_namemap_compu_method(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
        assert_eq!(namemap_compu_method.len(), 1);
        assert!(matches!(
            namemap_compu_method.get("method1"),
            Some(CompuMethod { .. })
        ));
    }

    #[test]
    fn test_namemap_transformer() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin TRANSFORMER trans1 "version string" "dll32" "dll64" 1 ON_CHANGE NO_INVERSE_TRANSFORMER
                /end TRANSFORMER
                /begin TRANSFORMER trans1 "version string" "dll32" "dll64" 1 ON_CHANGE NO_INVERSE_TRANSFORMER
                /end TRANSFORMER
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_transformer =
            build_namemap_transformer(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
        assert_eq!(namemap_transformer.len(), 1);
        assert!(matches!(
            namemap_transformer.get("trans1"),
            Some(Transformer { .. })
        ));
    }

    #[test]
    fn test_namemap_object() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin AXIS_PTS axis1 "" 0x1234 measurement_name record_layout_name 0 compu_method_name 1 0.0 100.0
                /end AXIS_PTS
                /begin AXIS_PTS axis1 "" 0x1234 measurement_name record_layout_name 0 compu_method_name 1 0.0 100.0
                /end AXIS_PTS
                /begin BLOB blob1 "" 1 100
                /end BLOB
                /begin BLOB blob1 "" 1 100
                /end BLOB
                /begin MEASUREMENT meas1 "" FLOAT32_IEEE compu_method_name 1 1.0 0 100
                /end MEASUREMENT
                /begin MEASUREMENT meas1 "" FLOAT32_IEEE compu_method_name 1 1.0 0 100
                /end MEASUREMENT
                /begin CHARACTERISTIC char1 "" CUBE_5 0x1234 record_layout_name 0 compu_method_name 0.0 10.0
                /end CHARACTERISTIC
                /begin CHARACTERISTIC char1 "" CUBE_5 0x1234 record_layout_name 0 compu_method_name 0.0 10.0
                /end CHARACTERISTIC
                /begin INSTANCE inst1 "" typedef_structure_name 0x1234
                /end INSTANCE
                /begin INSTANCE inst1 "" typedef_structure_name 0x1234
                /end INSTANCE
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_object = build_namemap_object(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 5);
        assert_eq!(namemap_object.len(), 5);
        assert!(matches!(
            namemap_object.get("meas1"),
            Some(crate::NameMapObject::Measurement(_))
        ));
        let meas1 = namemap_object.get("meas1").unwrap();
        assert_eq!(meas1.get_blockname(), "MEASUREMENT");
        assert!(matches!(
            namemap_object.get("char1"),
            Some(crate::NameMapObject::Characteristic(_))
        ));
        assert!(matches!(
            namemap_object.get("axis1"),
            Some(crate::NameMapObject::AxisPts(_))
        ));
        assert!(matches!(
            namemap_object.get("blob1"),
            Some(crate::NameMapObject::Blob(_))
        ));
        assert!(matches!(
            namemap_object.get("inst1"),
            Some(crate::NameMapObject::Instance(_))
        ));
    }

    #[test]
    fn test_namemap_variant() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin VARIANT_CODING
                    /begin VAR_CRITERION var1 ""
                    /end VAR_CRITERION
                    /begin VAR_CRITERION var1 ""
                    /end VAR_CRITERION
                /end VARIANT_CODING
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_variant = build_namemap_variant(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
        assert_eq!(namemap_variant.len(), 1);
        assert!(matches!(
            namemap_variant.get("var1"),
            Some(VarCriterion { .. })
        ));
    }

    #[test]
    fn test_namemap_group() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin GROUP group1 ""
                /end GROUP
                /begin GROUP group1 ""
                /end GROUP
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_group = build_namemap_group(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
        assert_eq!(namemap_group.len(), 1);
        assert!(matches!(namemap_group.get("group1"), Some(Group { .. })));
    }

    #[test]
    fn test_namemap_frame() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin FRAME frame1 "" 1 1
                /end FRAME
                /begin FRAME frame1 "" 1 1
                /end FRAME
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_frame = build_namemap_frame(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
        assert_eq!(namemap_frame.len(), 1);
        assert!(matches!(namemap_frame.get("frame1"), Some(Frame { .. })));
    }

    #[test]
    fn test_namemap_function() {
        static DATA: &str = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin FUNCTION func1 ""
                /end FUNCTION
                /begin FUNCTION func1 ""
                /end FUNCTION
            /end MODULE
        /end PROJECT"#;
        let mut log_msgs = Vec::new();
        let a2l_file = crate::load_from_string(DATA, None, &mut log_msgs, true).unwrap();
        let mut log_msgs = Vec::new();
        let namemap_function = build_namemap_function(&a2l_file.project.module[0], &mut log_msgs);
        assert_eq!(log_msgs.len(), 1);
        assert_eq!(namemap_function.len(), 1);
        assert!(matches!(
            namemap_function.get("func1"),
            Some(Function { .. })
        ));
    }
}
