use std::collections::HashMap;

use crate::specification::*;
use crate::Logger;


pub enum NameMapObject<'a> {
    Measurement(&'a Measurement),
    Characteristic(&'a Characteristic),
    AxisPts(&'a AxisPts),
    Blob(&'a Blob),
    Instance(&'a Instance),
}

pub enum NameMapCompuTab<'a> {
    CompuTab(&'a CompuTab),
    CompuVtab(&'a CompuVtab),
    CompuVtabRange(&'a CompuVtabRange),
}

pub enum NameMapTypedef<'a> {
    TypedefBlob(&'a TypedefBlob),
    TypedefAxis(&'a TypedefAxis),
    TypedefMeasurement(&'a TypedefMeasurement),
    TypedefCharacteristic(&'a TypedefCharacteristic),
    TypedefStructure(&'a TypedefStructure),
}


macro_rules! check_and_insert {
    ( $hash:expr, $key:expr, $item:expr, $logger:expr, $blockname:expr ) => {
        if let Some(existing_val) = $hash.get(&$key) {
            $logger.log_message(format!("Name collision: The {} blocks on line {} and {} both use the name {}",
                $blockname, existing_val.get_line(), $item.get_line(), $key));
        } else {
            $hash.insert($key, $item);
        }
    }
}

macro_rules! check_and_insert_multi {
    ( $hash:expr, $key:expr, $item:expr, $logger:expr, $blockname:expr ) => {
        if let Some(existing_val) = $hash.get(&$key) {
            $logger.log_message(format!("Name collision: The block {} on line {} and the block {} on line {} both use the name {}",
                existing_val.get_blockname(), existing_val.get_line(), $blockname, $item.get_line(), $key));
        } else {
            $hash.insert($key, $item);
        }
    }
}

pub struct ModuleNameMap<'a> {
    pub function: HashMap::<String, &'a Function>,
    pub group: HashMap::<String, &'a Group>,
    pub variant: HashMap::<String, &'a VarCriterion>,
    pub object: HashMap::<String, NameMapObject<'a>>,
    pub transformer: HashMap::<String, &'a Transformer>,
    pub compu_method: HashMap::<String, &'a CompuMethod>,
    pub compu_tab: HashMap::<String, NameMapCompuTab<'a>>,
    pub memory_segment: HashMap::<String, &'a MemorySegment>,
    pub record_layout: HashMap::<String, &'a RecordLayout>,
    pub typedef: HashMap::<String, NameMapTypedef<'a>>,
    pub unit: HashMap::<String, &'a Unit>
}


impl<'a> ModuleNameMap<'a> {
    /*
    There are several name spaces per module, officialy starting with 1.7, but this matches informal practice from previous versions.
    The following name spaces are defined:
    - FUNCTION
    - GROUP
    - VARIANT
    - MEASUREMENT, CHARACTERISTIC, AXIS_PTS, BLOB, INSTANCE
    - TRANSFORMER
    - COMPU_METHOD
    - COMPU_VTAB, COMPU_VTAB_RANGE, COMPU_TAB
    - MEMORY_SEGMENT
    - RECORD_LAYOUT
    - TYPEDEF_*
    */
    pub fn build(module: &'a Module, logger: &mut dyn Logger) -> Self {
        let mut namelist_function = HashMap::<String, &'a Function>::new();
        for function in &module.function {
            check_and_insert!(namelist_function, function.name.to_owned(), function, logger, "FUNCTION");
        }

        let mut namelist_group = HashMap::<String, &'a Group>::new();
        for group in &module.group {
            check_and_insert!(namelist_group, group.group_name.to_owned(), group, logger, "GROUP");
        }

        let mut namelist_variant = HashMap::<String, &'a VarCriterion>::new();
        for variant_coding in &module.variant_coding {
            for var_criterion in &variant_coding.var_criterion {
                check_and_insert!(namelist_variant, var_criterion.name.to_owned(), var_criterion, logger, "VAR_CRITERION");
            }
        }

        let mut namelist_object = HashMap::<String, NameMapObject<'a>>::new();
        for measurement in &module.measurement {
            check_and_insert_multi!(namelist_object, measurement.name.to_owned(), NameMapObject::Measurement(measurement), logger, "MEASUREMENT");
        }
        for characteristic in &module.characteristic {
            check_and_insert_multi!(namelist_object, characteristic.name.to_owned(), NameMapObject::Characteristic(characteristic), logger, "CHARACTERISTIC");
        }
        for axis_pts in &module.axis_pts {
            check_and_insert_multi!(namelist_object, axis_pts.name.to_owned(), NameMapObject::AxisPts(axis_pts), logger, "AXIS_PTS");
        }
        for blob in &module.blob {
            check_and_insert_multi!(namelist_object, blob.name.to_owned(), NameMapObject::Blob(blob), logger, "BLOB");
        }
        for instance in &module.instance {
            check_and_insert_multi!(namelist_object, instance.name.to_owned(), NameMapObject::Instance(instance), logger, "INSTANCE");
        }

        let mut namelist_transformer = HashMap::<String, &'a Transformer>::new();
        for transformer in &module.transformer {
            check_and_insert!(namelist_transformer, transformer.transformer_name.to_owned(), transformer, logger, "TRANSFORMER");
        }

        let mut namelist_compu_method = HashMap::<String, &'a CompuMethod>::new();
        for compu_method in &module.compu_method {
            check_and_insert!(namelist_compu_method, compu_method.name.to_owned(), compu_method, logger, "COMPU_METHOD");
        }

        let mut namelist_compu_tab = HashMap::<String, NameMapCompuTab<'a>>::new();
        for compu_tab in &module.compu_tab {
            check_and_insert_multi!(namelist_compu_tab, compu_tab.name.to_owned(), NameMapCompuTab::CompuTab(compu_tab), logger, "COMPU_TAB");
        }
        for compu_vtab in &module.compu_vtab {
            check_and_insert_multi!(namelist_compu_tab, compu_vtab.name.to_owned(), NameMapCompuTab::CompuVtab(compu_vtab), logger, "COMPU_VTAB");
        }
        for compu_vtab_range in &module.compu_vtab_range {
            check_and_insert_multi!(namelist_compu_tab, compu_vtab_range.name.to_owned(), NameMapCompuTab::CompuVtabRange(compu_vtab_range), logger, "COMPU_VTAB_RANGE");
        }

        let mut namelist_memory_segment = HashMap::<String, &'a MemorySegment>::new();
        if let Some(mod_par) = &module.mod_par {
            for memory_segment in &mod_par.memory_segment {
                check_and_insert!(namelist_memory_segment, memory_segment.name.to_owned(), memory_segment, logger, "MEMORY_SEGMENT");
            }
        }

        let mut namelist_record_layout = HashMap::<String, &'a RecordLayout>::new();
        for record_layout in &module.record_layout {
            check_and_insert!(namelist_record_layout, record_layout.name.to_owned(), record_layout, logger, "RECORD_LAYOUT");
        }

        let mut namelist_typedef = HashMap::<String, NameMapTypedef<'a>>::new();
        for typedef_axis in &module.typedef_axis {
            check_and_insert_multi!(namelist_typedef, typedef_axis.name.to_owned(), NameMapTypedef::TypedefAxis(typedef_axis), logger, "TYPEDEF_AXIS");
        }
        for typedef_blob in &module.typedef_blob {
            check_and_insert_multi!(namelist_typedef, typedef_blob.name.to_owned(), NameMapTypedef::TypedefBlob(typedef_blob), logger, "TYPEDEF_BLOB");
        }
        for typedef_characteristic in &module.typedef_characteristic {
            check_and_insert_multi!(namelist_typedef, typedef_characteristic.name.to_owned(), NameMapTypedef::TypedefCharacteristic(typedef_characteristic), logger, "TYPEDEF_CHARACTERISTIC");
        }
        for typedef_measurement in &module.typedef_measurement {
            check_and_insert_multi!(namelist_typedef, typedef_measurement.name.to_owned(), NameMapTypedef::TypedefMeasurement(typedef_measurement), logger, "TYPEDEF_MEASUREMENT");
        }
        for typedef_structure in &module.typedef_structure {
            check_and_insert_multi!(namelist_typedef, typedef_structure.name.to_owned(), NameMapTypedef::TypedefStructure(typedef_structure), logger, "TYPEDEF_STRUCTURE");
        }

        let mut namelist_unit = HashMap::<String, &'a Unit>::new();
        for unit in &module.unit {
            check_and_insert!(namelist_unit, unit.name.to_owned(), unit, logger, "UNIT");
        }


        Self {
            function: namelist_function,
            group: namelist_group,
            variant: namelist_variant,
            object: namelist_object,
            transformer: namelist_transformer,
            compu_method: namelist_compu_method,
            compu_tab: namelist_compu_tab,
            memory_segment: namelist_memory_segment,
            record_layout: namelist_record_layout,
            typedef: namelist_typedef,
            unit: namelist_unit
        }
    }
}


impl<'a> NameMapObject<'a> {
    fn get_line(&self) -> u32 {
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


impl<'a> NameMapCompuTab<'a> {
    fn get_line(&self) -> u32 {
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


impl<'a> NameMapTypedef<'a> {
    fn get_line(&self) -> u32 {
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
