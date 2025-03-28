//! Additional operations for the a2l `Module`

use crate::specification::*;

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
    pub fn objects(&self) -> FnvIndexMap<String, AnyObject> {
        let mut objects = self
            .axis_pts
            .iter()
            .map(|(k, v)| (k.clone(), AnyObject::AxisPts(v)))
            .collect::<FnvIndexMap<_, _>>();
        objects.extend(
            self.blob
                .iter()
                .map(|(k, v)| (k.clone(), AnyObject::Blob(v))),
        );
        objects.extend(
            self.characteristic
                .iter()
                .map(|(k, v)| (k.clone(), AnyObject::Characteristic(v))),
        );
        objects.extend(
            self.instance
                .iter()
                .map(|(k, v)| (k.clone(), AnyObject::Instance(v))),
        );
        objects.extend(
            self.measurement
                .iter()
                .map(|(k, v)| (k.clone(), AnyObject::Measurement(v))),
        );
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
    pub fn compu_tabs(&self) -> FnvIndexMap<String, AnyCompuTab> {
        let mut compu_tabs = self
            .compu_tab
            .iter()
            .map(|(k, v)| (k.clone(), AnyCompuTab::CompuTab(v)))
            .collect::<FnvIndexMap<_, _>>();
        compu_tabs.extend(
            self.compu_vtab
                .iter()
                .map(|(k, v)| (k.clone(), AnyCompuTab::CompuVtab(v))),
        );
        compu_tabs.extend(
            self.compu_vtab_range
                .iter()
                .map(|(k, v)| (k.clone(), AnyCompuTab::CompuVtabRange(v))),
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
    pub fn typedefs(&self) -> FnvIndexMap<String, AnyTypedef> {
        let mut typedefs = self
            .typedef_axis
            .iter()
            .map(|(k, v)| (k.clone(), AnyTypedef::TypedefAxis(v)))
            .collect::<FnvIndexMap<_, _>>();
        typedefs.extend(
            self.typedef_blob
                .iter()
                .map(|(k, v)| (k.clone(), AnyTypedef::TypedefBlob(v))),
        );
        typedefs.extend(
            self.typedef_characteristic
                .iter()
                .map(|(k, v)| (k.clone(), AnyTypedef::TypedefCharacteristic(v))),
        );
        typedefs.extend(
            self.typedef_measurement
                .iter()
                .map(|(k, v)| (k.clone(), AnyTypedef::TypedefMeasurement(v))),
        );
        typedefs.extend(
            self.typedef_structure
                .iter()
                .map(|(k, v)| (k.clone(), AnyTypedef::TypedefStructure(v))),
        );
        typedefs
    }
}
