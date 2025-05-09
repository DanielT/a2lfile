//! Additional operations for the a2l `Module`

use crate::{ItemList, specification::*};

#[derive(Debug, PartialEq)]
pub enum AnyObject<'a> {
    Measurement(&'a Measurement),
    Characteristic(&'a Characteristic),
    AxisPts(&'a AxisPts),
    Blob(&'a Blob),
    Instance(&'a Instance),
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, PartialEq)]
pub enum AnyCompuTab<'a> {
    CompuTab(&'a CompuTab),
    CompuVtab(&'a CompuVtab),
    CompuVtabRange(&'a CompuVtabRange),
}

#[allow(clippy::enum_variant_names)]
#[derive(Debug, PartialEq)]
pub enum AnyTypedef<'a> {
    TypedefBlob(&'a TypedefBlob),
    TypedefAxis(&'a TypedefAxis),
    TypedefMeasurement(&'a TypedefMeasurement),
    TypedefCharacteristic(&'a TypedefCharacteristic),
    TypedefStructure(&'a TypedefStructure),
}

impl AnyObject<'_> {
    pub fn get_line(&self) -> u32 {
        match self {
            Self::AxisPts(axis) => axis.get_line(),
            Self::Blob(blob) => blob.get_line(),
            Self::Characteristic(characteristic) => characteristic.get_line(),
            Self::Instance(instance) => instance.get_line(),
            Self::Measurement(measurement) => measurement.get_line(),
        }
    }
}

impl AnyCompuTab<'_> {
    pub fn get_line(&self) -> u32 {
        match self {
            Self::CompuTab(computab) => computab.get_line(),
            Self::CompuVtab(compuvtab) => compuvtab.get_line(),
            Self::CompuVtabRange(compuvtabrange) => compuvtabrange.get_line(),
        }
    }
}

impl AnyTypedef<'_> {
    pub fn get_line(&self) -> u32 {
        match self {
            Self::TypedefAxis(axis) => axis.get_line(),
            Self::TypedefBlob(blob) => blob.get_line(),
            Self::TypedefCharacteristic(characteristic) => characteristic.get_line(),
            Self::TypedefMeasurement(measurement) => measurement.get_line(),
            Self::TypedefStructure(structure) => structure.get_line(),
        }
    }
}

impl Module {
    #[cfg(feature = "merge")]
    /// merge another module with this module
    ///
    /// Any elements in other that are not present in this module will be moved over. The other module will typically be empty at the end of the merge.
    pub fn merge(&mut self, other: &mut Module) {
        use crate::merge;

        merge::merge_modules(self, other);
    }

    /// build a map of all objects in the module
    ///
    /// An object is one of the following:
    /// - AXIS_PTS
    /// - BLOB
    /// - CHARACTERISTIC
    /// - INSTANCE
    /// - MEASUREMENT
    ///
    /// The map is indexed by the object's name, and each value contains a reference to the actual object.
    pub fn objects(&self) -> ItemList<AnyObject> {
        let mut objects = self
            .axis_pts
            .iter()
            .map(AnyObject::AxisPts)
            .collect::<ItemList<_>>();
        objects.extend(self.blob.iter().map(AnyObject::Blob));
        objects.extend(self.characteristic.iter().map(AnyObject::Characteristic));
        objects.extend(self.instance.iter().map(AnyObject::Instance));
        objects.extend(self.measurement.iter().map(AnyObject::Measurement));
        objects
    }

    /// build a map of all compu tabs in the module
    ///
    /// A compu tab is one of the following:
    /// - COMPU_TAB
    /// - COMPU_VTAB
    /// - COMPU_VTAB_RANGE
    ///
    /// The map is indexed by the compu tab's name, and each value contains a reference to the actual compu tab.
    pub fn compu_tabs(&self) -> ItemList<AnyCompuTab> {
        let mut compu_tabs = self
            .compu_tab
            .iter()
            .map(AnyCompuTab::CompuTab)
            .collect::<ItemList<_>>();
        compu_tabs.extend(self.compu_vtab.iter().map(AnyCompuTab::CompuVtab));
        compu_tabs.extend(
            self.compu_vtab_range
                .iter()
                .map(AnyCompuTab::CompuVtabRange),
        );
        compu_tabs
    }

    /// build a map of all typedefs in the module
    ///
    /// A typedef is one of the following:
    /// - TYPEDEF_AXIS
    /// - TYPEDEF_BLOB
    /// - TYPEDEF_CHARACTERISTIC
    /// - TYPEDEF_MEASUREMENT
    /// - TYPEDEF_STRUCTURE
    ///
    /// The map is indexed by the typedef's name, and each value contains a reference to the actual typedef.
    pub fn typedefs(&self) -> ItemList<AnyTypedef> {
        let mut typedefs = self
            .typedef_axis
            .iter()
            .map(AnyTypedef::TypedefAxis)
            .collect::<ItemList<_>>();
        typedefs.extend(self.typedef_blob.iter().map(AnyTypedef::TypedefBlob));
        typedefs.extend(
            self.typedef_characteristic
                .iter()
                .map(AnyTypedef::TypedefCharacteristic),
        );
        typedefs.extend(
            self.typedef_measurement
                .iter()
                .map(AnyTypedef::TypedefMeasurement),
        );
        typedefs.extend(
            self.typedef_structure
                .iter()
                .map(AnyTypedef::TypedefStructure),
        );
        typedefs
    }
}

impl A2lObjectName for AnyObject<'_> {
    fn get_name(&self) -> &str {
        match self {
            Self::AxisPts(axis) => axis.get_name(),
            Self::Blob(blob) => blob.get_name(),
            Self::Characteristic(characteristic) => characteristic.get_name(),
            Self::Instance(instance) => instance.get_name(),
            Self::Measurement(measurement) => measurement.get_name(),
        }
    }
}

impl A2lObjectName for AnyCompuTab<'_> {
    fn get_name(&self) -> &str {
        match self {
            Self::CompuTab(computab) => computab.get_name(),
            Self::CompuVtab(compuvtab) => compuvtab.get_name(),
            Self::CompuVtabRange(compuvtabrange) => compuvtabrange.get_name(),
        }
    }
}

impl A2lObjectName for AnyTypedef<'_> {
    fn get_name(&self) -> &str {
        match self {
            Self::TypedefAxis(axis) => axis.get_name(),
            Self::TypedefBlob(blob) => blob.get_name(),
            Self::TypedefCharacteristic(characteristic) => characteristic.get_name(),
            Self::TypedefMeasurement(measurement) => measurement.get_name(),
            Self::TypedefStructure(structure) => structure.get_name(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::load_from_string;

    use super::*;

    #[test]
    fn test_merge() {
        let mut module1 = Module::new("mod".to_string(), String::new());
        let mut module2 = Module::new("mod2".to_string(), String::new());

        module1
            .blob
            .push(Blob::new("blob1".to_string(), String::new(), 0, 1000));
        module2
            .blob
            .push(Blob::new("blob2".to_string(), String::new(), 50000, 1024));

        module1.merge(&mut module2);

        assert_eq!(module1.blob.len(), 2);
        assert_eq!(module2.blob.len(), 0);
    }

    #[test]
    fn test_objects() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin AXIS_PTS axispts "" 0x1234 meas record_layout 0 compu_method 3 0.0 10.0 /end AXIS_PTS
            /begin BLOB blob "" 0x1234 100 /end BLOB
            /begin CHARACTERISTIC chara "" CURVE 0x1234 rl 0 NO_COMPU_METHOD 0.0 1.0 /end CHARACTERISTIC
            /begin INSTANCE inst "" td_struct 0x1234 /end INSTANCE
            /begin MEASUREMENT meas "" FLOAT32_IEEE cm  1 1.0 0 100 /end MEASUREMENT
        /end MODULE /end PROJECT"#;
        let (a2lfile, _) = load_from_string(A2L_TEXT, None, true).unwrap();

        let module = &a2lfile.project.module[0];

        assert_eq!(module.axis_pts.len(), 1);
        assert_eq!(module.blob.len(), 1);
        assert_eq!(module.characteristic.len(), 1);
        assert_eq!(module.instance.len(), 1);
        assert_eq!(module.measurement.len(), 1);

        let objects = module.objects();
        assert_eq!(objects.len(), 5);
        assert_eq!(objects[0].get_name(), "axispts");
        assert_ne!(objects[0].get_line(), 0);
        assert_eq!(objects[1].get_name(), "blob");
        assert_ne!(objects[1].get_line(), 0);
        assert_eq!(objects[2].get_name(), "chara");
        assert_ne!(objects[2].get_line(), 0);
        assert_eq!(objects[3].get_name(), "inst");
        assert_ne!(objects[3].get_line(), 0);
        assert_eq!(objects[4].get_name(), "meas");
        assert_ne!(objects[4].get_line(), 0);
    }

    #[test]
    fn test_typedef() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin TYPEDEF_AXIS typedef_axis "" meas record_layout 0 compu_method 1 0 100 /end TYPEDEF_AXIS
            /begin TYPEDEF_BLOB typedef_blob "" 1 /end TYPEDEF_BLOB
            /begin TYPEDEF_CHARACTERISTIC typedef_characteristic "" VALUE record_layout 0 compu_method 0 100 /end TYPEDEF_CHARACTERISTIC
            /begin TYPEDEF_MEASUREMENT typedef_measurement "" UBYTE compu_method 1 1 0 100 /end TYPEDEF_MEASUREMENT
            /begin TYPEDEF_STRUCTURE typedef_structure "" 1 /end TYPEDEF_STRUCTURE
        /end MODULE /end PROJECT"#;
        let (a2lfile, _) = load_from_string(A2L_TEXT, None, true).unwrap();

        let module = &a2lfile.project.module[0];

        assert_eq!(module.typedef_axis.len(), 1);
        assert_eq!(module.typedef_blob.len(), 1);
        assert_eq!(module.typedef_characteristic.len(), 1);
        assert_eq!(module.typedef_measurement.len(), 1);
        assert_eq!(module.typedef_structure.len(), 1);

        let typedefs = module.typedefs();
        assert_eq!(typedefs.len(), 5);
        assert_eq!(typedefs[0].get_name(), "typedef_axis");
        assert_ne!(typedefs[0].get_line(), 0);
        assert_eq!(typedefs[1].get_name(), "typedef_blob");
        assert_ne!(typedefs[1].get_line(), 0);
        assert_eq!(typedefs[2].get_name(), "typedef_characteristic");
        assert_ne!(typedefs[2].get_line(), 0);
        assert_eq!(typedefs[3].get_name(), "typedef_measurement");
        assert_ne!(typedefs[3].get_line(), 0);
        assert_eq!(typedefs[4].get_name(), "typedef_structure");
        assert_ne!(typedefs[4].get_line(), 0);
    }

    #[test]
    fn test_computabs() {
        static A2L_TEXT: &str = r#"ASAP2_VERSION 1 71 /begin PROJECT p "" /begin MODULE m ""
            /begin COMPU_TAB compu_tab "" IDENTICAL 2 /end COMPU_TAB
            /begin COMPU_VTAB compu_vtab "" IDENTICAL 2 /end COMPU_VTAB
            /begin COMPU_VTAB_RANGE compu_vtab_range "" 2 /end COMPU_VTAB_RANGE
        /end MODULE /end PROJECT"#;
        let (a2lfile, _) = load_from_string(A2L_TEXT, None, true).unwrap();

        let module = &a2lfile.project.module[0];

        assert_eq!(module.compu_tab.len(), 1);
        assert_eq!(module.compu_vtab.len(), 1);
        assert_eq!(module.compu_vtab_range.len(), 1);

        let computabs = module.compu_tabs();
        assert_eq!(computabs.len(), 3);
        assert_eq!(computabs[0].get_name(), "compu_tab");
        assert_ne!(computabs[0].get_line(), 0);
        assert_eq!(computabs[1].get_name(), "compu_vtab");
        assert_ne!(computabs[1].get_line(), 0);
        assert_eq!(computabs[2].get_name(), "compu_vtab_range");
        assert_ne!(computabs[2].get_line(), 0);
    }
}
