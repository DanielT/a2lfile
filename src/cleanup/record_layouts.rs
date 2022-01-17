use std::collections::HashSet;

use crate::specification::*;

pub(crate) fn cleanup(module: &mut Module) {
    let mut used_record_layouts = HashSet::<String>::new();
    // AXIS_PTS, CHARACTERISTIC and TYPEDEF_CHARACTERISTIC can reference RECORD_LAYOUTs
    for axis_pts in &module.axis_pts {
        used_record_layouts.insert(axis_pts.deposit_record.to_owned());
    }
    for characteristic in &module.characteristic {
        used_record_layouts.insert(characteristic.deposit.to_owned());
    }
    for typedef_characteristic in &module.typedef_characteristic {
        used_record_layouts.insert(typedef_characteristic.record_layout.to_owned());
    }
    // deprecated since 1.60: MOD_COMMON / S_REC_LAOUT can specify the standard RECORD_LAYOUT
    if let Some(mod_common) = &module.mod_common {
        if let Some(s_rec_layout) = &mod_common.s_rec_layout {
            used_record_layouts.insert(s_rec_layout.name.to_owned());
        }
    }

    // remove all unused RECORD_LAYOUTs
    module.record_layout.retain(|item| used_record_layouts.get(&item.name).is_some());
}
