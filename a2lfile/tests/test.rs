#[cfg(test)]
mod test {
    use a2lfile::*;
    use std::{collections::HashMap, vec};
    use tempfile::tempdir;

    a2ml_specification! {
        <A2mlTest>

        block "IF_DATA" taggedunion if_data {
            "CHAR" char a;
            "INT" int b;
            "LONG" long c;
            "INT64" int64 d;
            "UCHAR" uchar e;
            "UINT" uint64 f;
            "ULONG" ulong g;
            "UINT64" uint64 h;
            "DOUBLE" double i;
            "FLOAT" float j;
            "STRUCT" struct structname {
                char[256];
                int;
            };
            block "BLOCK" taggedstruct tagged_struct {
                "TAG1" int intval;
            };
            "ENUM" enum EnumTest {
                "ENUMVAL1" = 1,
                "ENUMVAL2"
            } named_enum;
            "ARRAY" uint arr[3];
            block "SEQUENCE" (char[256] name)*;
            "NONE";
        };
    }

    const TEST_A2L: &str = r###"
ASAP2_VERSION 1 61
/begin PROJECT SOMETHING ""

  /begin MODULE CPP ""
    /begin MOD_COMMON ""
      BYTE_ORDER MSB_LAST
      ALIGNMENT_BYTE 1
      ALIGNMENT_WORD 1
      ALIGNMENT_LONG 1
      ALIGNMENT_INT64 1
      ALIGNMENT_FLOAT32_IEEE 1
      ALIGNMENT_FLOAT64_IEEE 1
    /end MOD_COMMON
    /begin COMPU_METHOD compumethod
      ""
      RAT_FUNC
      "%6.3"
      ""
      COEFFS 0 1 0 0 0 1
      REF_UNIT abc
    /end COMPU_METHOD
    /begin MEASUREMENT measurement ""
      UBYTE CM.IDENTICAL 0 0 0 255
      /begin IF_DATA ETK
        KP_BLOB 0x13A30 INTERN 0x1 RASTER 0x4
      /end IF_DATA
    /end MEASUREMENT
  /end MODULE
/end PROJECT"###;

    #[test]
    fn round_trip() {
        let (a2lfile, _) = a2lfile::load_from_string(TEST_A2L, None, false).unwrap();
        let text = a2lfile.write_to_string();
        println!("input:\n{}\noutput:\n{}\n", TEST_A2L, text);

        assert_eq!(TEST_A2L, text);
    }

    #[cfg(all(
        feature = "merge",
        feature = "sort",
        feature = "check",
        feature = "cleanup"
    ))]
    #[test]
    fn full_test() {
        // work in a tempdir
        let dir = tempdir().unwrap();
        std::env::set_current_dir(dir.path()).unwrap();

        let mut a2l_file = a2lfile::new();

        a2l_file.a2ml_version = Some(A2mlVersion::new(1, 31));

        let a2ml = A2ml::new(
            r##"  block "IF_DATA" taggedunion if_data {
            block "BLOCK_A" struct {
                int;
            };
            block "BLOCK_B" struct {
                char[256];
            };
        };
        "##
            .to_string(),
        );

        let mut if_data_a = IfData::new();
        let mut if_data_content: HashMap<String, Vec<GenericIfDataTaggedItem>> = HashMap::new();
        if_data_content.insert(
            "BLOCK_A".to_string(),
            vec![GenericIfDataTaggedItem {
                incfile: None,
                line: 0,
                uid: 0,
                start_offset: 1,
                end_offset: 1,
                tag: "BLOCK_A".to_string(),
                data: GenericIfData::Block {
                    incfile: None,
                    line: 0,
                    items: vec![GenericIfData::Int(1, (42, false))],
                },
                is_block: true,
            }],
        );
        if_data_a.ifdata_items = Some(GenericIfData::Block {
            incfile: None,
            line: 0,
            items: vec![GenericIfData::TaggedUnion(if_data_content)],
        });

        let mut memory_segment = a2lfile::MemorySegment::new(
            "seg1".to_string(),
            "seg1".to_string(),
            PrgType::Data,
            MemoryType::Rom,
            MemoryAttribute::Extern,
            0xDEADBEEF,
            0x1337,
            [-1, -1, -1, -1, -1],
        );
        memory_segment.if_data = vec![if_data_a.clone()];

        let mut mod_par = ModPar::new("comment".to_string());
        mod_par.addr_epk = vec![AddrEpk::new(0x12345678)];
        mod_par.calibration_method =
            vec![CalibrationMethod::new("calibration_method".to_string(), 0)];
        mod_par.cpu_type = Some(CpuType::new("cpu_type".to_string()));
        mod_par.customer = Some(Customer::new("customer".to_string()));
        mod_par.customer_no = Some(CustomerNo::new("customer_no".to_string()));
        mod_par.ecu = Some(Ecu::new("ecu".to_string()));
        mod_par.ecu_calibration_offset = Some(EcuCalibrationOffset::new(0x12345678));
        mod_par.epk = Some(Epk::new("abcdef".to_string()));
        mod_par.memory_layout = vec![MemoryLayout::new(ProgType::PrgCode, 0, 0, [0, 0, 0, 0, 0])];
        mod_par.memory_segment = vec![memory_segment];
        mod_par.no_of_interfaces = Some(NoOfInterfaces::new(1));
        mod_par.phone_no = Some(PhoneNo::new("phone_no".to_string()));
        mod_par.supplier = Some(Supplier::new("supplier".to_string()));
        mod_par.system_constant = vec![SystemConstant::new(
            "system_constant".to_string(),
            "x".to_string(),
        )];
        mod_par.user = Some(User::new("user".to_string()));
        mod_par.version = Some(Version::new("version".to_string()));

        let mut mod_common = ModCommon::new("comment".to_string());
        mod_common.byte_order = Some(ByteOrder::new(ByteOrderEnum::MsbLast));
        mod_common.alignment_byte = Some(AlignmentByte::new(1));
        mod_common.alignment_word = Some(AlignmentWord::new(2));
        mod_common.alignment_long = Some(AlignmentLong::new(4));
        mod_common.alignment_int64 = Some(AlignmentInt64::new(8));
        mod_common.alignment_float16_ieee = Some(AlignmentFloat16Ieee::new(2));
        mod_common.alignment_float32_ieee = Some(AlignmentFloat32Ieee::new(4));
        mod_common.alignment_float64_ieee = Some(AlignmentFloat64Ieee::new(8));
        mod_common.deposit = Some(Deposit::new(DepositMode::Absolute));
        mod_common.s_rec_layout = Some(SRecLayout::new("a_rec_layout".to_string()));

        let mut fix_axis_par_list = FixAxisParList::new();
        fix_axis_par_list.axis_pts_value_list =
            vec![0f64, 1f64, 2f64, 3f64, 4f64, 5f64, 6f64, 7f64, 8f64, 9f64];

        let mut axis_descr = AxisDescr::new(
            AxisDescrAttribute::CurveAxis,
            "chr2_axis_input".to_string(),
            "chr2_axis_conversion".to_string(),
            10,
            0f64,
            f64::MAX,
        );
        axis_descr.annotation = vec![Annotation::new()];
        axis_descr.axis_pts_ref = Some(AxisPtsRef::new("axis_pts_ref".to_string()));
        axis_descr.byte_order = Some(ByteOrder::new(ByteOrderEnum::MsbLast));
        axis_descr.curve_axis_ref = Some(CurveAxisRef::new("curve_axis_ref".to_string()));
        axis_descr.deposit = Some(Deposit::new(DepositMode::Absolute));
        axis_descr.extended_limits = Some(ExtendedLimits::new(0f64, f64::MAX));
        axis_descr.fix_axis_par = Some(FixAxisPar::new(0i16, 0i16, 0u16));
        axis_descr.fix_axis_par_dist = Some(FixAxisParDist::new(0i16, 0i16, 0u16));
        axis_descr.fix_axis_par_list = Some(fix_axis_par_list);
        axis_descr.format = Some(Format::new("format".to_string()));
        axis_descr.max_grad = Some(MaxGrad::new(0f64));
        axis_descr.monotony = Some(Monotony::new(MonotonyType::MonIncrease));
        axis_descr.phys_unit = Some(PhysUnit::new("phys_unit".to_string()));
        axis_descr.read_only = Some(ReadOnly::new());
        axis_descr.step_size = Some(StepSize::new(1f64));

        let mut axis_pts = AxisPts::new(
            "axis_pts".to_string(),
            "long_identifier".to_string(),
            0,
            "input_quantity".to_string(),
            "deposit_record".to_string(),
            f64::MAX,
            "conversion".to_string(),
            10u16,
            0f64,
            f64::MAX,
        );
        axis_pts.annotation = vec![Annotation::new()];
        axis_pts.byte_order = Some(ByteOrder::new(ByteOrderEnum::MsbLast));
        axis_pts.deposit = Some(Deposit::new(DepositMode::Absolute));
        axis_pts.display_identifier =
            Some(DisplayIdentifier::new("display_identifier".to_string()));
        axis_pts.ecu_address_extension = Some(EcuAddressExtension::new(10000i16));
        axis_pts.extended_limits = Some(ExtendedLimits::new(0f64, f64::MAX));
        axis_pts.format = Some(Format::new("format".to_string()));
        axis_pts.if_data = vec![if_data_a.clone()];
        axis_pts.max_refresh = Some(MaxRefresh::new(0, 0));
        axis_pts.model_link = Some(ModelLink::new("model_link".to_string()));
        axis_pts.monotony = Some(Monotony::new(MonotonyType::MonIncrease));
        axis_pts.phys_unit = Some(PhysUnit::new("phys_unit".to_string()));
        axis_pts.read_only = Some(ReadOnly::new());
        axis_pts.step_size = Some(StepSize::new(1f64));
        axis_pts.ref_memory_segment = Some(RefMemorySegment::new("ref_memory_segment".to_string()));
        axis_pts.step_size = Some(StepSize::new(1f64));
        axis_pts.symbol_link = Some(SymbolLink::new("symbol_link".to_string(), 0));

        let mut blob = Blob::new("name".to_string(), "long_identifier".to_string(), 0, 0);
        blob.annotation = vec![Annotation::new()];
        blob.address_type = Some(AddressType::new(AddrType::Direct));
        blob.calibration_access = Some(CalibrationAccess::new(CalibrationAccessEnum::Calibration));
        blob.display_identifier = Some(DisplayIdentifier::new("display_identifier".to_string()));
        blob.ecu_address_extension = Some(EcuAddressExtension::new(10000i16));
        blob.max_refresh = Some(MaxRefresh::new(0, 0));
        blob.model_link = Some(ModelLink::new("model_link".to_string()));
        blob.symbol_link = Some(SymbolLink::new("symbol_link".to_string(), 0));

        let mut function_list = FunctionList::new();
        function_list.name_list = vec!["name1".to_string(), "name2".to_string()];

        let mut map_list = MapList::new();
        map_list.name_list = vec!["map1".to_string(), "map2".to_string()];

        let mut matrix_dim = MatrixDim::new();
        matrix_dim.dim_list = vec![1, 2, 3];

        let mut characteristic = Characteristic::new(
            "characteristic2".to_string(),
            "long_identifier".to_string(),
            CharacteristicType::Curve,
            0xF1223344,
            "deposit_ref_curve".to_string(),
            0.0f64,
            "conversion_chr2".to_string(),
            0.0f64,
            f64::MAX,
        );
        characteristic.annotation = vec![Annotation::new()];
        characteristic.axis_descr = vec![axis_descr.clone()];
        characteristic.bit_mask = Some(BitMask::new(0x12345678));
        characteristic.byte_order = Some(ByteOrder::new(ByteOrderEnum::MsbLast));
        characteristic.calibration_access =
            Some(CalibrationAccess::new(CalibrationAccessEnum::Calibration));
        characteristic.comparison_quantity =
            Some(ComparisonQuantity::new("comparison_quantity".to_string()));
        characteristic.dependent_characteristic = Some(DependentCharacteristic::new(
            "dependent_characteristic".to_string(),
        ));
        characteristic.discrete = Some(Discrete::new());
        characteristic.display_identifier =
            Some(DisplayIdentifier::new("display_identifier".to_string()));
        characteristic.ecu_address_extension = Some(EcuAddressExtension::new(10000i16));
        characteristic.encoding = Some(Encoding::new(CharacterEncoding::Utf8));
        characteristic.extended_limits = Some(ExtendedLimits::new(0f64, f64::MAX));
        characteristic.format = Some(Format::new("format".to_string()));
        characteristic.function_list = Some(function_list.clone());
        characteristic.guard_rails = Some(GuardRails::new());
        characteristic.if_data = vec![if_data_a.clone()];
        characteristic.map_list = Some(map_list);
        characteristic.matrix_dim = Some(matrix_dim.clone());
        characteristic.max_refresh = Some(MaxRefresh::new(0u16, 0u32));
        characteristic.model_link = Some(ModelLink::new("model_link".to_string()));
        characteristic.number = Some(Number::new(0u16));
        characteristic.phys_unit = Some(PhysUnit::new("phys_unit".to_string()));
        characteristic.read_only = Some(ReadOnly::new());
        characteristic.ref_memory_segment =
            Some(RefMemorySegment::new("ref_memory_segment".to_string()));
        characteristic.step_size = Some(StepSize::new(1f64));
        characteristic.symbol_link = Some(SymbolLink::new("symbol_link".to_string(), 0i32));
        characteristic.virtual_characteristic = Some(VirtualCharacteristic::new(
            "virtual_characteristic".to_string(),
        ));

        let mut compu_method = CompuMethod::new(
            "name".to_string(),
            "display_identifier".to_string(),
            ConversionType::TabVerb,
            "format".to_string(),
            "unit".to_string(),
        );
        compu_method.coeffs = Some(Coeffs::new(0f64, 1f64, 2f64, 3f64, 4f64, 5f64));
        compu_method.coeffs_linear = Some(CoeffsLinear::new(0f64, 1f64));
        compu_method.compu_tab_ref = Some(CompuTabRef::new("conversion_table".to_string()));
        compu_method.formula = Some(Formula::new("formula".to_string()));
        compu_method.ref_unit = Some(RefUnit::new("ref_unit".to_string()));
        compu_method.status_string_ref =
            Some(StatusStringRef::new("status_string_ref".to_string()));

        let compu_method_2 = CompuMethod::new(
            "compu_method_2".to_string(),
            "display_identifier".to_string(),
            ConversionType::TabVerb,
            "format".to_string(),
            "unit".to_string(),
        );

        let mut compu_tab = CompuTab::new(
            "name".to_string(),
            "long_identifier".to_string(),
            ConversionType::TabVerb,
            1,
        );
        compu_tab.default_value = Some(DefaultValue::new("default_value".to_string()));
        compu_tab.default_value_numeric = Some(DefaultValueNumeric::new(0f64));
        compu_tab.tab_entry = vec![TabEntryStruct::new(0f64, 1f64)];

        let mut compu_vtab = CompuVtab::new(
            "name".to_string(),
            "long_identifier".to_string(),
            ConversionType::TabVerb,
            1,
        );
        compu_vtab.default_value = Some(DefaultValue::new("default_value".to_string()));
        compu_vtab.value_pairs = vec![ValuePairsStruct::new(0f64, "value".to_string())];

        let mut compu_vtab_range =
            CompuVtabRange::new("name".to_string(), "long_identifier".to_string(), 1);
        compu_vtab_range.default_value = Some(DefaultValue::new("default_value".to_string()));
        compu_vtab_range.value_triples =
            vec![ValueTriplesStruct::new(0f64, 1f64, "value".to_string())];

        let mut frame_measurement = FrameMeasurement::new();
        frame_measurement.identifier_list = vec!["identifier".to_string()];

        let mut frame = Frame::new("name".to_string(), "long_identifier".to_string(), 0, 0);
        frame.frame_measurement = Some(frame_measurement);

        let mut def_characteristic = DefCharacteristic::new();
        def_characteristic.identifier_list = vec!["identifier".to_string()];
        let mut in_measurement = InMeasurement::new();
        in_measurement.identifier_list = vec!["identifier".to_string()];
        let mut loc_measurement = LocMeasurement::new();
        loc_measurement.identifier_list = vec!["identifier".to_string()];
        let mut out_measurement = OutMeasurement::new();
        out_measurement.identifier_list = vec!["identifier".to_string()];
        let mut ref_characteristic = RefCharacteristic::new();
        ref_characteristic.identifier_list = vec!["identifier".to_string()];
        let mut sub_function = SubFunction::new();
        sub_function.identifier_list = vec!["identifier".to_string()];

        let mut function = Function::new("name".to_string(), "long_identifier".to_string());
        function.annotation = vec![Annotation::new()];
        let mut ar_component = ArComponent::new("name".to_string());
        ar_component.ar_prototype_of = Some(ArPrototypeOf::new("function".to_string()));
        function.ar_component = Some(ar_component);
        function.def_characteristic = Some(def_characteristic);
        function.function_version = Some(FunctionVersion::new("version".to_string()));
        function.if_data = vec![if_data_a.clone()];
        function.in_measurement = Some(in_measurement);
        function.loc_measurement = Some(loc_measurement);
        function.out_measurement = Some(out_measurement);
        function.ref_characteristic = Some(ref_characteristic.clone());
        function.sub_function = Some(sub_function);

        let mut ref_measurement = RefMeasurement::new();
        ref_measurement.identifier_list = vec!["identifier".to_string()];
        let mut sub_group = SubGroup::new();
        sub_group.identifier_list = vec!["identifier".to_string()];

        let mut group = Group::new("name".to_string(), "long_identifier".to_string());
        group.annotation = vec![Annotation::new()];
        group.function_list = Some(function_list.clone());
        group.ref_characteristic = Some(ref_characteristic);
        group.ref_measurement = Some(ref_measurement);
        group.root = Some(Root::new());
        group.sub_group = Some(sub_group);

        let mut overwrite = Overwrite::new("name".to_string(), 0);
        overwrite.conversion = Some(Conversion::new("conversion".to_string()));
        overwrite.extended_limits = Some(ExtendedLimits::new(0f64, 0f64));
        overwrite.format = Some(Format::new("format".to_string()));
        overwrite.input_quantity = Some(InputQuantity::new("input_quantity".to_string()));
        overwrite.limits = Some(Limits::new(0f64, 0f64));
        overwrite.monotony = Some(Monotony::new(MonotonyType::MonIncrease));
        overwrite.phys_unit = Some(PhysUnit::new("phys_unit".to_string()));

        let mut instance = Instance::new(
            "name".to_string(),
            "long_identifier".to_string(),
            "type_ref".to_string(),
            0,
        );
        instance.annotation = vec![Annotation::new()];
        instance.calibration_access =
            Some(CalibrationAccess::new(CalibrationAccessEnum::Calibration));
        instance.display_identifier =
            Some(DisplayIdentifier::new("display_identifier".to_string()));
        instance.ecu_address_extension = Some(EcuAddressExtension::new(0i16));
        instance.if_data = vec![if_data_a.clone()];
        instance.layout = Some(Layout::new(IndexMode::RowDir));
        instance.matrix_dim = Some(matrix_dim.clone());
        instance.max_refresh = Some(MaxRefresh::new(0u16, 0u32));
        instance.model_link = Some(ModelLink::new("model_link".to_string()));
        instance.overwrite = vec![overwrite];
        instance.read_only = Some(ReadOnly::new());
        instance.symbol_link = Some(SymbolLink::new("symbol_link".to_string(), 0));

        let mut bit_operation = BitOperation::new();
        bit_operation.left_shift = Some(LeftShift::new(1));
        bit_operation.right_shift = Some(RightShift::new(2));
        bit_operation.sign_extend = Some(SignExtend::new());

        let mut var_virtual = Virtual::new();
        var_virtual.measuring_channel_list = vec!["x".to_string()];

        let mut measurement = Measurement::new(
            "name".to_string(),
            "long_identifier".to_string(),
            DataType::Ulong,
            "conversion".to_string(),
            1,
            0.0,
            0f64,
            f64::MAX,
        );
        measurement.address_type = Some(AddressType::new(AddrType::Direct));
        measurement.annotation = vec![Annotation::new()];
        measurement.array_size = Some(ArraySize::new(0u16));
        measurement.bit_mask = Some(BitMask::new(0));
        measurement.bit_operation = Some(bit_operation.clone());
        measurement.byte_order = Some(ByteOrder::new(ByteOrderEnum::BigEndian));
        measurement.discrete = Some(Discrete::new());
        measurement.display_identifier =
            Some(DisplayIdentifier::new("display_identifier".to_string()));
        measurement.ecu_address = Some(EcuAddress::new(0u32));
        measurement.ecu_address_extension = Some(EcuAddressExtension::new(0i16));
        measurement.error_mask = Some(ErrorMask::new(0));
        measurement.format = Some(Format::new("format".to_string()));
        measurement.function_list = Some(function_list);
        measurement.if_data = vec![if_data_a.clone()];
        measurement.layout = Some(Layout::new(IndexMode::RowDir));
        measurement.matrix_dim = Some(matrix_dim.clone());
        measurement.max_refresh = Some(MaxRefresh::new(0u16, 0u32));
        measurement.model_link = Some(ModelLink::new("model_link".to_string()));
        measurement.phys_unit = Some(PhysUnit::new("phys_unit".to_string()));
        measurement.read_write = Some(ReadWrite::new());
        measurement.ref_memory_segment =
            Some(RefMemorySegment::new("ref_memory_segment".to_string()));
        measurement.symbol_link = Some(SymbolLink::new("symbol_link".to_string(), 0));
        measurement.var_virtual = Some(var_virtual);

        let mut record_layout = RecordLayout::new("name".to_string());
        record_layout.alignment_byte = Some(AlignmentByte::new(0));
        record_layout.alignment_float16_ieee = Some(AlignmentFloat16Ieee::new(0));
        record_layout.alignment_float32_ieee = Some(AlignmentFloat32Ieee::new(0));
        record_layout.alignment_float64_ieee = Some(AlignmentFloat64Ieee::new(0));
        record_layout.alignment_int64 = Some(AlignmentInt64::new(0));
        record_layout.alignment_long = Some(AlignmentLong::new(0));
        record_layout.alignment_word = Some(AlignmentWord::new(0));
        record_layout.axis_pts_4 = Some(AxisPtsDim::new(
            0u16,
            DataType::Ubyte,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_pts_5 = Some(AxisPtsDim::new(
            0u16,
            DataType::Sbyte,
            IndexOrder::IndexIncr,
            AddrType::Pbyte,
        ));
        record_layout.axis_pts_x = Some(AxisPtsDim::new(
            0u16,
            DataType::Slong,
            IndexOrder::IndexIncr,
            AddrType::Plong,
        ));
        record_layout.axis_pts_y = Some(AxisPtsDim::new(
            0u16,
            DataType::Sword,
            IndexOrder::IndexIncr,
            AddrType::Plonglong,
        ));
        record_layout.axis_pts_z = Some(AxisPtsDim::new(
            0u16,
            DataType::Ulong,
            IndexOrder::IndexIncr,
            AddrType::Pword,
        ));
        record_layout.axis_rescale_4 = Some(AxisRescaleDim::new(
            0,
            DataType::Uword,
            1,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_rescale_5 = Some(AxisRescaleDim::new(
            0,
            DataType::AUint64,
            1,
            IndexOrder::IndexDecr,
            AddrType::Direct,
        ));
        record_layout.axis_rescale_x = Some(AxisRescaleDim::new(
            0,
            DataType::Ubyte,
            1,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_rescale_y = Some(AxisRescaleDim::new(
            0,
            DataType::Ubyte,
            1,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_rescale_z = Some(AxisRescaleDim::new(
            0,
            DataType::Ubyte,
            1,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.dist_op_4 = Some(DistOpDim::new(0, DataType::Ubyte));
        record_layout.dist_op_5 = Some(DistOpDim::new(0, DataType::Ubyte));
        record_layout.dist_op_x = Some(DistOpDim::new(0, DataType::Ubyte));
        record_layout.dist_op_y = Some(DistOpDim::new(0, DataType::Ubyte));
        record_layout.dist_op_z = Some(DistOpDim::new(0, DataType::Ubyte));
        record_layout.fix_no_axis_pts_4 = Some(FixNoAxisPtsDim::new(0));
        record_layout.fix_no_axis_pts_5 = Some(FixNoAxisPtsDim::new(0));
        record_layout.fix_no_axis_pts_x = Some(FixNoAxisPtsDim::new(0));
        record_layout.fix_no_axis_pts_y = Some(FixNoAxisPtsDim::new(0));
        record_layout.fix_no_axis_pts_z = Some(FixNoAxisPtsDim::new(0));
        record_layout.fnc_values = Some(FncValues::new(
            0,
            DataType::AInt64,
            IndexMode::RowDir,
            AddrType::Direct,
        ));
        record_layout.identification = Some(Identification::new(0, DataType::AInt64));
        record_layout.no_axis_pts_4 = Some(NoAxisPtsDim::new(0, DataType::AInt64));
        record_layout.no_axis_pts_5 = Some(NoAxisPtsDim::new(0, DataType::AUint64));
        record_layout.no_axis_pts_x = Some(NoAxisPtsDim::new(0, DataType::Float16Ieee));
        record_layout.no_axis_pts_y = Some(NoAxisPtsDim::new(0, DataType::Float32Ieee));
        record_layout.no_axis_pts_z = Some(NoAxisPtsDim::new(0, DataType::Float64Ieee));
        record_layout.no_rescale_4 = Some(NoRescaleDim::new(0, DataType::AInt64));
        record_layout.no_rescale_5 = Some(NoRescaleDim::new(0, DataType::AInt64));
        record_layout.no_rescale_x = Some(NoRescaleDim::new(0, DataType::AInt64));
        record_layout.no_rescale_y = Some(NoRescaleDim::new(0, DataType::AInt64));
        record_layout.no_rescale_z = Some(NoRescaleDim::new(0, DataType::AInt64));
        record_layout.offset_4 = Some(OffsetDim::new(0, DataType::AInt64));
        record_layout.offset_5 = Some(OffsetDim::new(0, DataType::AInt64));
        record_layout.offset_x = Some(OffsetDim::new(0, DataType::AInt64));
        record_layout.offset_y = Some(OffsetDim::new(0, DataType::AInt64));
        record_layout.offset_z = Some(OffsetDim::new(0, DataType::AInt64));
        record_layout.reserved = vec![Reserved::new(0, DataTypeSize::Byte)];
        record_layout.rip_addr_4 = Some(RipAddrDim::new(0, DataType::AInt64));
        record_layout.rip_addr_5 = Some(RipAddrDim::new(0, DataType::AInt64));
        record_layout.rip_addr_w = Some(RipAddrDim::new(0, DataType::AInt64));
        record_layout.rip_addr_x = Some(RipAddrDim::new(0, DataType::AInt64));
        record_layout.rip_addr_y = Some(RipAddrDim::new(0, DataType::AInt64));
        record_layout.rip_addr_z = Some(RipAddrDim::new(0, DataType::AInt64));
        record_layout.shift_op_4 = Some(ShiftOpDim::new(0, DataType::AInt64));
        record_layout.shift_op_5 = Some(ShiftOpDim::new(0, DataType::AInt64));
        record_layout.shift_op_x = Some(ShiftOpDim::new(0, DataType::AInt64));
        record_layout.shift_op_y = Some(ShiftOpDim::new(0, DataType::AInt64));
        record_layout.shift_op_z = Some(ShiftOpDim::new(0, DataType::AInt64));
        record_layout.src_addr_4 = Some(SrcAddrDim::new(0, DataType::AInt64));
        record_layout.src_addr_5 = Some(SrcAddrDim::new(0, DataType::AInt64));
        record_layout.src_addr_x = Some(SrcAddrDim::new(0, DataType::AInt64));
        record_layout.src_addr_y = Some(SrcAddrDim::new(0, DataType::AInt64));
        record_layout.src_addr_z = Some(SrcAddrDim::new(0, DataType::AInt64));
        record_layout.static_address_offsets = Some(StaticAddressOffsets::new());
        record_layout.static_record_layout = Some(StaticRecordLayout::new());

        let mut transformer_in_objects = TransformerInObjects::new();
        transformer_in_objects.identifier_list = vec!["name1".to_string(), "name2".to_string()];
        let mut transformer_out_objects = TransformerOutObjects::new();
        transformer_out_objects.identifier_list = vec!["name1".to_string(), "name2".to_string()];

        let mut transformer = Transformer::new(
            "name".to_string(),
            "version".to_string(),
            "dllname_32bit".to_string(),
            "dllname_64bit".to_string(),
            100,
            TransformerTrigger::OnChange,
            "inverse_transformer".to_string(),
        );
        transformer.transformer_in_objects = Some(transformer_in_objects);
        transformer.transformer_out_objects = Some(transformer_out_objects);

        let mut typedef_axis = TypedefAxis::new(
            "name".to_string(),
            "long_identifier".to_string(),
            "input_quantity".to_string(),
            "record_layout".to_string(),
            0.0f64,
            "conversion".to_string(),
            1,
            0.0f64,
            f64::MAX,
        );
        typedef_axis.deposit = Some(Deposit::new(DepositMode::Difference));
        typedef_axis.extended_limits = Some(ExtendedLimits::new(0f64, f64::MAX));
        typedef_axis.format = Some(Format::new("format".to_string()));
        typedef_axis.monotony = Some(Monotony::new(MonotonyType::MonIncrease));
        typedef_axis.phys_unit = Some(PhysUnit::new("phys_unit".to_string()));
        typedef_axis.step_size = Some(StepSize::new(0f64));

        let mut typedef_blob =
            TypedefBlob::new("name".to_string(), "long_identifier".to_string(), 0);
        typedef_blob.address_type = Some(AddressType::new(AddrType::Pbyte));

        let mut typedef_characteristic = TypedefCharacteristic::new(
            "name".to_string(),
            "long_identifier".to_string(),
            CharacteristicType::Value,
            "record_layout".to_string(),
            0.0f64,
            "conversion".to_string(),
            0.0f64,
            f64::MAX,
        );
        typedef_characteristic.axis_descr = vec![axis_descr];
        typedef_characteristic.bit_mask = Some(BitMask::new(0));
        typedef_characteristic.byte_order = Some(ByteOrder::new(ByteOrderEnum::MsbFirst));
        typedef_characteristic.discrete = Some(Discrete::new());
        typedef_characteristic.encoding = Some(Encoding::new(CharacterEncoding::Utf8));
        typedef_characteristic.extended_limits = Some(ExtendedLimits::new(0f64, f64::MAX));
        typedef_characteristic.format = Some(Format::new("format".to_string()));
        typedef_characteristic.matrix_dim = Some(matrix_dim.clone());
        typedef_characteristic.number = Some(Number::new(0u16));
        typedef_characteristic.phys_unit = Some(PhysUnit::new("phys_unit".to_string()));
        typedef_characteristic.step_size = Some(StepSize::new(1f64));

        let mut typedef_measurement = TypedefMeasurement::new(
            "name".to_string(),
            "long_identifier".to_string(),
            DataType::Sword,
            "conversion".to_string(),
            0,
            0f64,
            0f64,
            f64::MAX,
        );
        typedef_measurement.address_type = Some(AddressType::new(AddrType::Direct));
        typedef_measurement.bit_mask = Some(BitMask::new(0));
        typedef_measurement.bit_operation = Some(bit_operation);
        typedef_measurement.byte_order = Some(ByteOrder::new(ByteOrderEnum::BigEndian));
        typedef_measurement.discrete = Some(Discrete::new());
        typedef_measurement.error_mask = Some(ErrorMask::new(0));
        typedef_measurement.format = Some(Format::new("format".to_string()));
        typedef_measurement.layout = Some(Layout::new(IndexMode::RowDir));
        typedef_measurement.matrix_dim = Some(matrix_dim.clone());
        typedef_measurement.phys_unit = Some(PhysUnit::new("phys_unit".to_string()));

        let mut structure_component = StructureComponent::new(
            "component_name".to_string(),
            "component_type".to_string(),
            69,
        );
        structure_component.address_type = Some(AddressType::new(AddrType::Direct));
        structure_component.layout = Some(Layout::new(IndexMode::RowDir));
        structure_component.matrix_dim = Some(matrix_dim);
        structure_component.symbol_type_link = Some(SymbolTypeLink::new("symbol_type".to_string()));

        let mut typedef_structure =
            TypedefStructure::new("name".to_string(), "long_identifier".to_string(), 0);
        typedef_structure.address_type = Some(AddressType::new(AddrType::Direct));
        typedef_structure.consistent_exchange = Some(ConsistentExchange::new());
        typedef_structure.structure_component = vec![structure_component];
        typedef_structure.symbol_type_link = Some(SymbolTypeLink::new("symbol_type".to_string()));

        let mut unit = Unit::new(
            "name".to_string(),
            "long_identifier".to_string(),
            "display".to_string(),
            UnitType::Derived,
        );
        unit.ref_unit = Some(RefUnit::new("ref_unit".to_string()));
        unit.si_exponents = Some(SiExponents::new(1, 1, 1, 1, 1, 1, 1));
        unit.unit_conversion = Some(UnitConversion::new(0f64, 0f64));

        let mut ref_group = RefGroup::new();
        ref_group.identifier_list = vec![
            "identifier".to_string(),
            "name".to_string(),
            "other".to_string(),
        ];

        let mut user_rights = UserRights::new("user_level_id".to_string());
        user_rights.read_only = Some(ReadOnly::new());
        user_rights.ref_group = vec![ref_group];

        let mut var_address = VarAddress::new();
        var_address.address_list = vec![123, 0xdeadbeef, 0xcafebabe, 0, 42, 1, 0xffffffff];

        let mut var_characteristic = VarCharacteristic::new("name".to_string());
        var_characteristic.criterion_name_list = vec!["criterion_name".to_string()];
        var_characteristic.var_address = Some(var_address);

        let mut variant_coding = VariantCoding::new();
        variant_coding.var_characteristic = vec![var_characteristic];

        a2l_file.project.module[0].a2ml = Some(a2ml);
        a2l_file.project.module[0].mod_par = Some(mod_par);
        a2l_file.project.module[0].mod_common = Some(mod_common);
        a2l_file.project.module[0].axis_pts = vec![axis_pts];
        a2l_file.project.module[0].blob = vec![blob];
        a2l_file.project.module[0].characteristic = vec![characteristic];
        a2l_file.project.module[0].compu_method = vec![compu_method, compu_method_2];
        a2l_file.project.module[0].compu_tab = vec![compu_tab];
        a2l_file.project.module[0].compu_vtab = vec![compu_vtab];
        a2l_file.project.module[0].compu_vtab_range = vec![compu_vtab_range];
        a2l_file.project.module[0].frame = vec![frame];
        a2l_file.project.module[0].function = vec![function];
        a2l_file.project.module[0].group = vec![group];
        a2l_file.project.module[0].if_data = vec![if_data_a];
        a2l_file.project.module[0].instance = vec![instance];
        a2l_file.project.module[0].measurement = vec![measurement];
        a2l_file.project.module[0].record_layout = vec![record_layout];
        a2l_file.project.module[0].transformer = vec![transformer];
        a2l_file.project.module[0].typedef_axis = vec![typedef_axis];
        a2l_file.project.module[0].typedef_blob = vec![typedef_blob];
        a2l_file.project.module[0].typedef_characteristic = vec![typedef_characteristic];
        a2l_file.project.module[0].typedef_measurement = vec![typedef_measurement];
        a2l_file.project.module[0].typedef_structure = vec![typedef_structure];
        a2l_file.project.module[0].unit = vec![unit];
        a2l_file.project.module[0].user_rights = vec![user_rights];
        a2l_file.project.module[0].variant_coding = Some(variant_coding);

        let mut cloned_a2l_file = a2l_file.clone();
        let unchanged_a2l_file = a2l_file.clone();
        assert_eq!(a2l_file.project.module.len(), 1);
        // merge_modules should be a no-op in this case, because the content of the merged file is the same as the original
        a2l_file.merge_modules(&mut cloned_a2l_file);
        a2l_file.write("merged.a2l", None).unwrap();
        unchanged_a2l_file.write("orig.a2l", None).unwrap();
        assert_eq!(a2l_file, unchanged_a2l_file);
        assert_eq!(a2l_file.project.module.len(), 1);

        let (mut other_a2l_file, _) = a2lfile::load_from_string(TEST_A2L, None, false).unwrap();
        assert_eq!(a2l_file.project.module[0].measurement.len(), 1);
        a2l_file.merge_modules(&mut other_a2l_file);
        assert_eq!(a2l_file.project.module[0].measurement.len(), 2);

        a2l_file.project.module[0].measurement[0].reset_location();
        // initially the compu_methods are not sorted alphabetically
        assert_eq!(a2l_file.project.module[0].compu_method[0].name, "name");
        assert_eq!(
            a2l_file.project.module[0].compu_method[1].name,
            "compu_method_2"
        );
        a2l_file.sort_new_items();
        // now they are sorted alphabetically
        assert_eq!(
            a2l_file.project.module[0].compu_method[0].name,
            "compu_method_2"
        );
        assert_eq!(a2l_file.project.module[0].compu_method[1].name, "name");

        // the newly created elements do not have a uid, even after sort_new_items()
        assert!(
            a2l_file.project.module[0].characteristic[0]
                .get_layout()
                .uid
                == 0
        );
        a2l_file.sort();
        // fully sorting the file assigns a uid to all elements
        assert!(
            a2l_file.project.module[0].characteristic[0]
                .get_layout()
                .uid
                != 0
        );
        assert!(a2l_file.project.module[0].measurement[0].get_layout().uid != 0);

        // the data above is not sane, there are multiple warnings reported in log_msgs
        let log_msgs = a2l_file.check();
        assert_ne!(log_msgs.len(), 0);

        // the compu_methods in the a2l_file are not referenced by any other element, so they get removed by cleanup()
        assert_eq!(a2l_file.project.module[0].compu_method.len(), 3);
        a2l_file.cleanup();
        assert_eq!(a2l_file.project.module[0].compu_method.len(), 0);

        let a2ldata = a2l_file.write_to_string();

        let result = a2l_file.write("test.a2l", None);
        assert!(result.is_ok());
        assert!(std::path::Path::new("test.a2l").exists());

        // verify that the loaded data structure is identical to the original data structure
        let a2l_file2 = a2lfile::load_from_string(&a2ldata, None, false);
        assert!(a2l_file2.is_ok());
        let (a2l_file2, _) = a2l_file2.unwrap();
        assert_eq!(a2l_file, a2l_file2);

        let (mut a2l_file3, _) =
            a2lfile::load("test.a2l", Some(A2MLTEST_TEXT.to_string()), false).unwrap();
        assert_eq!(a2l_file2, a2l_file3);

        // set the incfile propery of one element. This would normally indicate that is was loaded from an /include file
        a2l_file3.project.module[0].measurement[0]
            .get_layout_mut()
            .incfile = Some("bla".to_string());
        a2l_file3.merge_includes();
        // the incfile property should be removed from the measurement element
        assert_eq!(
            a2l_file3.project.module[0].measurement[0]
                .get_layout_mut()
                .incfile,
            None
        );

        // the first measurement was merged from the data that was loaded from the TEST_A2L string and then sorted to the beginning of the list.
        // It contains an IF_DATA block, but the content is nonsense
        assert_eq!(a2l_file3.project.module[0].measurement[0].if_data.len(), 1);
        assert!(!a2l_file3.project.module[0].measurement[0].if_data[0].ifdata_valid);
        // ifdata on the other measurement is valid
        assert_eq!(a2l_file3.project.module[0].measurement[1].if_data.len(), 1);
        assert!(a2l_file3.project.module[0].measurement[1].if_data[0].ifdata_valid);
        a2l_file3.ifdata_cleanup();
        // the nonsensical IF_DATA has been removed
        assert_eq!(a2l_file3.project.module[0].measurement[0].if_data.len(), 0);
        // valid ifdata is retained
        assert_eq!(a2l_file3.project.module[0].measurement[1].if_data.len(), 1);

        let delete_result = std::fs::remove_file("test.a2l");
        assert!(delete_result.is_ok());
    }

    #[test]
    fn parsing_weird_data() {
        let data_bad = r##"abcdef"##;
        let load_result = a2lfile::load_from_string(data_bad, None, false);
        assert!(load_result.is_err());

        let data_bad = r##"ASAP2_VERSION 1 71"##;
        let load_result = a2lfile::load_from_string(data_bad, None, false);
        assert!(load_result.is_err());

        let data_bad = r##"ASAP2_VERSION 1 71
        /begin PROJECT
        /end PROJECT"##;
        let load_result = a2lfile::load_from_string(data_bad, None, false);
        assert!(load_result.is_err());

        let data_bad = r##"ASAP2_VERSION 1 71
        /begin PROJECT //x ""
        /end PROJECT"##;
        let load_result = a2lfile::load_from_string(data_bad, None, false);
        assert!(load_result.is_err());

        let data_bad = r##"ASAP2_VERSION 1 71
        /beginPROJECT x ""
        /end PROJECT"##;
        let load_result = a2lfile::load_from_string(data_bad, None, false);
        assert!(load_result.is_err());

        let data_good = r##"ASAP2_VERSION 1 0x47
        /begin PROJECT x ""
        /end PROJECT"##;
        let load_result = a2lfile::load_from_string(data_good, None, false);
        assert!(load_result.is_ok());

        let data_good = r##"ASAP2_VERSION 1 71
        /begin PROJECT x ""
        /end PROJECT"##;
        let load_result = a2lfile::load_from_string(data_good, None, false);
        assert!(load_result.is_ok());

        let data_good = r##"//comment
        ASAP2_VERSION 1 71 //comment
        /begin PROJECT x "" /*/*////*
        /end PROJECT"##;
        let load_result = a2lfile::load_from_string(data_good, None, false);
        assert!(load_result.is_ok());
    }

    // a test file that contains one of each specified block
    static TEST_A2L_2: &str = r#"ASAP2_VERSION 1 71
        A2ML_VERSION 1 31
        /begin PROJECT prj ""
            /begin HEADER "abc"
                PROJECT_NO ident_12345
                VERSION "version_string"
            /end HEADER
            /begin MODULE mod "long_identifier"
                /begin A2ML
                    block "IF_DATA" struct {
                        int;
                    };
                /end A2ML
                /begin AXIS_PTS axispts_name "long_identifier" 0x1234 measurement_name record_layout_name 0 compu_method_name 3 0.0 10.0
                    /begin ANNOTATION
                        ANNOTATION_LABEL "label" ANNOTATION_ORIGIN "origin"
                        /begin ANNOTATION_TEXT
                            "text"
                        /end ANNOTATION_TEXT
                    /end ANNOTATION
                    /begin FUNCTION_LIST
                    /end FUNCTION_LIST
                    CALIBRATION_ACCESS NO_CALIBRATION
                    GUARD_RAILS READ_ONLY
                    /begin IF_DATA
                    /end IF_DATA
                /end AXIS_PTS
                /begin BLOB blob_name "long_identifier" 0x1234 100
                    /begin IF_DATA
                    /end IF_DATA
                /end BLOB
                /begin CHARACTERISTIC characteristic_name "long_identifier" CURVE 0x1234 record_layout_name 0 compu_method_name 0.0 10.0
                    /begin AXIS_DESCR COM_AXIS measurement_name compu_method_name 1 0 100
                        AXIS_PTS_REF axispts_name BYTE_ORDER MSB_LAST CURVE_AXIS_REF characteristic_name DEPOSIT ABSOLUTE
                        EXTENDED_LIMITS -100 200 FIX_AXIS_PAR 0 0 0 FIX_AXIS_PAR_DIST 0 0 0
                        FORMAT "%1.1" MAX_GRAD 1 READ_ONLY STEP_SIZE 1
                        /begin FIX_AXIS_PAR_LIST 0
                        /end FIX_AXIS_PAR_LIST
                    /end AXIS_DESCR
                    COMPARISON_QUANTITY measurement_name
                    /begin DEPENDENT_CHARACTERISTIC "formula"
                        characteristic_name
                    /end DEPENDENT_CHARACTERISTIC
                    /begin MAP_LIST
                    /end MAP_LIST
                    /begin IF_DATA
                    /end IF_DATA
                    /begin VIRTUAL_CHARACTERISTIC
                        "formula" characteristic_name
                    /end VIRTUAL_CHARACTERISTIC
                    NUMBER 1
                /end CHARACTERISTIC
                /begin COMPU_METHOD compu_method_name "" IDENTICAL "%4.2" "unit"
                    COEFFS 1 2 3 4 5 6
                    COEFFS_LINEAR 1 2
                    COMPU_TAB_REF compu_tab_name
                    /begin FORMULA formula
                        FORMULA_INV "inverse"
                    /end FORMULA
                    REF_UNIT unit_name
                    STATUS_STRING_REF compu_vtab_name
                /end COMPU_METHOD
                /begin COMPU_TAB compu_tab_name "long_identifier" IDENTICAL 2
                    1 22
                    2 33
                    DEFAULT_VALUE "abc"
                    DEFAULT_VALUE_NUMERIC 44
                /end COMPU_TAB
                /begin COMPU_VTAB compu_vtab_name "long_identifier" IDENTICAL 2
                    1 "abc"
                    2 "def"
                    DEFAULT_VALUE "abc"
                /end COMPU_VTAB
                /begin COMPU_VTAB_RANGE compu_vtab_range_name "long_identifier" 2
                    1 2 "abc"
                    DEFAULT_VALUE "abc"
                /end COMPU_VTAB_RANGE
                /begin FRAME frame_name "long_identifier" 1 2
                    FRAME_MEASUREMENT measurement_name
                    /begin IF_DATA
                    /end IF_DATA
                /end FRAME
                /begin FUNCTION function_name "long_identifier"
                    /begin ANNOTATION
                    /end ANNOTATION
                    /begin AR_COMPONENT "ar_component"
                        AR_PROTOTYPE_OF function
                    /end AR_COMPONENT
                    FUNCTION_VERSION "version-1.1.1"
                    /begin DEF_CHARACTERISTIC characteristic_name
                    /end DEF_CHARACTERISTIC
                    /begin IN_MEASUREMENT measurement_name
                    /end IN_MEASUREMENT
                    /begin LOC_MEASUREMENT measurement_name
                    /end LOC_MEASUREMENT
                    /begin OUT_MEASUREMENT measurement_name
                    /end OUT_MEASUREMENT
                    /begin REF_CHARACTERISTIC characteristic_name
                    /end REF_CHARACTERISTIC
                    /begin SUB_FUNCTION function_name
                    /end SUB_FUNCTION
                    /begin IF_DATA
                    /end IF_DATA
                /end FUNCTION
                /begin GROUP group_name "long_identifier"
                    /begin FUNCTION_LIST
                    /end FUNCTION_LIST
                    /begin REF_CHARACTERISTIC
                    /end REF_CHARACTERISTIC
                    /begin REF_MEASUREMENT
                    /end REF_MEASUREMENT
                    ROOT
                    /begin SUB_GROUP
                    /end SUB_GROUP
                    /begin IF_DATA
                    /end IF_DATA
                /end GROUP
                /begin IF_DATA
                    1
                /end IF_DATA
                /begin INSTANCE instance_name "long_identifier" typedef_structure_name 0x1234
                    ADDRESS_TYPE PLONGLONG
                    CALIBRATION_ACCESS CALIBRATION
                    DISPLAY_IDENTIFIER display_identifier
                    ECU_ADDRESS_EXTENSION 0x3000
                    LAYOUT ROW_DIR
                    MATRIX_DIM 3
                    MAX_REFRESH 1 1
                    MODEL_LINK "model link"
                    /begin OVERWRITE overwrite_name 0
                        CONVERSION compu_method_name
                        EXTENDED_LIMITS -100 1000
                        FORMAT "%1.3"
                        INPUT_QUANTITY input_quantity
                        LIMITS 0 100
                        MONOTONY MON_INCREASE
                        PHYS_UNIT "unit"
                    /end OVERWRITE
                    READ_ONLY
                    SYMBOL_LINK "symbol name" 0x1234
                    /begin IF_DATA
                    /end IF_DATA
                /end INSTANCE
                /begin MEASUREMENT measurement_name "long_identifier" FLOAT32_IEEE compu_method_name 1 1.0 0 100
                    ARRAY_SIZE 1
                    BIT_MASK 0xF0
                    /begin BIT_OPERATION
                        LEFT_SHIFT 1
                        RIGHT_SHIFT 2
                        SIGN_EXTEND
                    /end BIT_OPERATION
                    BYTE_ORDER MSB_LAST
                    DISCRETE
                    DISPLAY_IDENTIFIER display_identifier
                    ECU_ADDRESS 0x1234
                    ECU_ADDRESS_EXTENSION 0x4444
                    ERROR_MASK 0xab
                    FORMAT "%1.2"
                    LAYOUT COLUMN_DIR
                    MATRIX_DIM 1 2 3
                    MAX_REFRESH 1 1
                    MODEL_LINK "model link"
                    PHYS_UNIT "unit"
                    READ_WRITE
                    REF_MEMORY_SEGMENT memory_segment
                    SYMBOL_LINK "symbol" 0x1234
                    /begin VIRTUAL
                        ident
                    /end VIRTUAL
                    /begin IF_DATA
                    /end IF_DATA
                /end MEASUREMENT
                /begin MOD_COMMON "description"
                    ALIGNMENT_BYTE 1
                    ALIGNMENT_FLOAT16_IEEE 2
                    ALIGNMENT_FLOAT32_IEEE 4
                    ALIGNMENT_FLOAT64_IEEE 8
                    ALIGNMENT_INT64 8
                    ALIGNMENT_LONG 4
                    ALIGNMENT_WORD 2
                    BYTE_ORDER MSB_LAST
                    DATA_SIZE 42
                    DEPOSIT ABSOLUTE
                    S_REC_LAYOUT name
                /end MOD_COMMON
                /begin MOD_PAR "Comment"
                    ADDR_EPK 0x1234567
                    /begin CALIBRATION_METHOD "InCircuit" 1
                        /begin CALIBRATION_HANDLE 1 2 3 4 5
                            CALIBRATION_HANDLE_TEXT "txt"
                        /end CALIBRATION_HANDLE
                    /end CALIBRATION_METHOD
                    CPU_TYPE "leg"
                    CUSTOMER "c"
                    CUSTOMER_NO "1"
                    ECU "e"
                    ECU_CALIBRATION_OFFSET 1
                    EPK "e"
                    /begin MEMORY_LAYOUT PRG_DATA 0x1234 1 0 0 0 0 0
                        /begin IF_DATA
                        /end IF_DATA
                    /end MEMORY_LAYOUT
                    /begin MEMORY_SEGMENT memory_segment "long_identifier" DATA RAM EXTERN 0 0 0 0 0 0 0
                    /end MEMORY_SEGMENT
                    NO_OF_INTERFACES 1
                    PHONE_NO "1"
                    SUPPLIER "s"
                    SYSTEM_CONSTANT "c" "1"
                    USER "u"
                    VERSION "1"
                /end MOD_PAR
                /begin RECORD_LAYOUT record_layout_name
                    ALIGNMENT_BYTE 1
                    ALIGNMENT_FLOAT16_IEEE 2
                    ALIGNMENT_FLOAT32_IEEE 4
                    ALIGNMENT_FLOAT64_IEEE 8
                    ALIGNMENT_INT64 8
                    ALIGNMENT_LONG 4
                    ALIGNMENT_WORD 2
                    AXIS_PTS_X 1 SWORD INDEX_INCR DIRECT
                    AXIS_PTS_Y 1 SWORD INDEX_INCR DIRECT
                    AXIS_PTS_Z 1 SWORD INDEX_INCR DIRECT
                    AXIS_PTS_4 1 SWORD INDEX_INCR DIRECT
                    AXIS_PTS_5 1 SWORD INDEX_INCR DIRECT
                    AXIS_RESCALE_X 2 SWORD 1 INDEX_INCR DIRECT
                    AXIS_RESCALE_Y 2 SWORD 1 INDEX_INCR DIRECT
                    AXIS_RESCALE_Z 2 SWORD 1 INDEX_INCR DIRECT
                    AXIS_RESCALE_4 2 SWORD 1 INDEX_INCR DIRECT
                    AXIS_RESCALE_5 2 SWORD 1 INDEX_INCR DIRECT
                    DIST_OP_X 3 SWORD
                    DIST_OP_Y 3 SWORD
                    DIST_OP_Z 3 SWORD
                    DIST_OP_4 3 SWORD
                    DIST_OP_5 3 SWORD
                    FIX_NO_AXIS_PTS_X 1
                    FIX_NO_AXIS_PTS_Y 1
                    FIX_NO_AXIS_PTS_Z 1
                    FIX_NO_AXIS_PTS_4 1
                    FIX_NO_AXIS_PTS_5 1
                    FNC_VALUES 4 SWORD ROW_DIR DIRECT
                    RESERVED 5 WORD
                    RIP_ADDR_W 6 SWORD
                    RIP_ADDR_X 6 SWORD
                    RIP_ADDR_Y 6 SWORD
                    RIP_ADDR_Z 6 SWORD
                    RIP_ADDR_4 6 SWORD
                    RIP_ADDR_5 6 SWORD
                    SRC_ADDR_X 7 SWORD
                    SRC_ADDR_Y 7 SWORD
                    SRC_ADDR_Z 7 SWORD
                    SRC_ADDR_4 7 SWORD
                    SRC_ADDR_5 7 SWORD
                    SHIFT_OP_X 8 SWORD
                    SHIFT_OP_Y 8 SWORD
                    SHIFT_OP_Z 8 SWORD
                    SHIFT_OP_4 8 SWORD
                    SHIFT_OP_5 8 SWORD
                    IDENTIFICATION 9 SWORD
                    NO_AXIS_PTS_X 10 SWORD
                    NO_AXIS_PTS_Y 10 SWORD
                    NO_AXIS_PTS_Z 10 SWORD
                    NO_AXIS_PTS_4 10 SWORD
                    NO_AXIS_PTS_5 10 SWORD
                    NO_RESCALE_X 11 SWORD
                    NO_RESCALE_Y 11 SWORD
                    NO_RESCALE_Z 11 SWORD
                    NO_RESCALE_4 11 SWORD
                    NO_RESCALE_5 11 SWORD
                    OFFSET_X 12 SWORD
                    OFFSET_Y 12 SWORD
                    OFFSET_Z 12 SWORD
                    OFFSET_4 12 SWORD
                    OFFSET_5 12 SWORD
                    STATIC_RECORD_LAYOUT
                    STATIC_ADDRESS_OFFSETS
                /end RECORD_LAYOUT
                /begin TRANSFORMER transformer_name "version string" "dll32" "dll64" 1 ON_CHANGE NO_INVERSE_TRANSFORMER
                    /begin TRANSFORMER_IN_OBJECTS
                    /end TRANSFORMER_IN_OBJECTS
                    /begin TRANSFORMER_OUT_OBJECTS
                    /end TRANSFORMER_OUT_OBJECTS
                /end TRANSFORMER
                /begin TYPEDEF_AXIS typedef_axis_name "long_identifier" measurement_name record_layout_name 0 compu_method_name 1 0 100
                    BYTE_ORDER MSB_LAST
                    MONOTONY MON_DECREASE
                    STEP_SIZE 3
                /end TYPEDEF_AXIS
                /begin TYPEDEF_BLOB typedef_blob_name "long_identifier" 1
                    ADDRESS_TYPE DIRECT
                /end TYPEDEF_BLOB
                /begin TYPEDEF_CHARACTERISTIC typedef_characteristic_name "long_identifier" VALUE record_layout_name 0 compu_method_name 0 100
                    BIT_MASK 0x22
                    DISCRETE
                    ENCODING UTF8
                /end TYPEDEF_CHARACTERISTIC
                /begin TYPEDEF_MEASUREMENT typedef_measurement_name "long_identifier" UBYTE compu_method_name 1 1 0 100
                    ADDRESS_TYPE DIRECT
                /end TYPEDEF_MEASUREMENT
                /begin TYPEDEF_STRUCTURE typedef_structure_name "long_identifier" 1
                    CONSISTENT_EXCHANGE
                    /begin STRUCTURE_COMPONENT component_name typedef_characteristic_name 1
                        ADDRESS_TYPE DIRECT
                        LAYOUT COLUMN_DIR
                        MATRIX_DIM 1
                        SYMBOL_TYPE_LINK "abc"
                    /end STRUCTURE_COMPONENT
                    SYMBOL_TYPE_LINK "abcdef"
                /end TYPEDEF_STRUCTURE
                /begin UNIT unit_name "long_identifier" "x" DERIVED
                    REF_UNIT ref_unit
                    SI_EXPONENTS 1 2 3 4 5 6 7
                    UNIT_CONVERSION 1 1
                /end UNIT
                /begin USER_RIGHTS user
                    READ_ONLY
                    /begin REF_GROUP group
                    /end REF_GROUP
                /end USER_RIGHTS
                /begin USER_RIGHTS user2
                /end USER_RIGHTS
                /begin VARIANT_CODING
                    /begin VAR_CHARACTERISTIC name criterion_name
                        /begin VAR_ADDRESS 0x1234
                        /end VAR_ADDRESS
                    /end VAR_CHARACTERISTIC 
                    VAR_NAMING NUMERIC
                    VAR_SEPARATOR "."
                    /begin VAR_FORBIDDEN_COMB
                        abc def
                    /end VAR_FORBIDDEN_COMB
                    /begin VAR_CRITERION var_criterion_name ""
                        abc def
                        VAR_MEASUREMENT measurement_name
                        VAR_SELECTION_CHARACTERISTIC characteristic_name
                    /end VAR_CRITERION
                /end VARIANT_CODING
            /end MODULE
        /end PROJECT
        "#;

    #[cfg(all(feature = "check", feature = "sort"))]
    #[test]
    fn specification_test() {
        let (mut a2l_file, _) = load_from_string(TEST_A2L_2, None, false).unwrap();
        let mut a2l_file2 = a2l_file.clone();
        assert_eq!(a2l_file, a2l_file2);

        let serialized = a2l_file2.write_to_string();
        // unfortunately minor formatting differences (whitespace, floats) prevent comparison of the serialized text
        let (a2l_file3, _) = load_from_string(&serialized, None, false).unwrap();
        assert_eq!(a2l_file, a2l_file3);

        // reset the location, serialize & reload again
        a2l_file2.reset_location();
        a2l_file2.project.reset_location();
        a2l_file2.project.module[0].reset_location();
        a2l_file2.project.module[0].characteristic[0].reset_location();
        let serialized2 = a2l_file2.write_to_string();
        let (a2l_file4, _) = load_from_string(&serialized2, None, false).unwrap();
        // serialized text is not equal, because location info was reset with reset_location() and some elements were arranged differently
        assert_ne!(serialized, serialized2);
        // the files are still equal, because location info is not considered when comparing data
        assert_eq!(a2l_file3, a2l_file4);

        // create the same file using the API
        let mut a2l_file5 = new();
        a2l_file5.asap2_version = Some(Asap2Version::new(1, 71));
        a2l_file5.a2ml_version = Some(A2mlVersion::new(1, 31));
        a2l_file5.project.name = "prj".to_string();
        let mut header = Header::new("abc".to_string());
        header.project_no = Some(ProjectNo::new("ident_12345".to_string()));
        header.version = Some(Version::new("version_string".to_string()));
        a2l_file5.project.header = Some(header);
        let module = &mut a2l_file5.project.module[0];
        module.name = "mod".to_string();
        module.long_identifier = "long_identifier".to_string();
        module.a2ml = Some(A2ml::new(
            r#"
                    block "IF_DATA" struct {
                        int;
                    };"#
            .to_string(),
        ));
        let mut axis_pts = AxisPts::new(
            "axispts_name".to_string(),
            "long_identifier".to_string(),
            0x1234,
            "measurement_name".to_string(),
            "record_layout_name".to_string(),
            0.0,
            "compu_method_name".to_string(),
            3,
            0.0,
            10.0,
        );
        let mut annotation = Annotation::new();
        annotation.annotation_label = Some(AnnotationLabel::new("label".to_string()));
        annotation.annotation_origin = Some(AnnotationOrigin::new("origin".to_string()));
        let mut annotation_text = AnnotationText::new();
        annotation_text
            .annotation_text_list
            .push("text".to_string());
        annotation.annotation_text = Some(annotation_text);
        axis_pts.annotation.push(annotation);
        axis_pts.guard_rails = Some(GuardRails::new());
        axis_pts.read_only = Some(ReadOnly::new());
        axis_pts.calibration_access =
            Some(CalibrationAccess::new(CalibrationAccessEnum::NoCalibration));
        axis_pts.function_list = Some(FunctionList::new());
        axis_pts.if_data.push(IfData::default());
        module.axis_pts.push(axis_pts);
        let mut blob = Blob::new(
            "blob_name".to_string(),
            "long_identifier".to_string(),
            0x1234,
            100,
        );
        blob.if_data.push(IfData::default());
        module.blob.push(blob);
        let mut characteristic = Characteristic::new(
            "characteristic_name".to_string(),
            "long_identifier".to_string(),
            CharacteristicType::Curve,
            0x1234,
            "record_layout_name".to_string(),
            0.0,
            "compu_method_name".to_string(),
            0.0,
            10.0,
        );
        let mut axis_descr = AxisDescr::new(
            AxisDescrAttribute::ComAxis,
            "measurement_name".to_string(),
            "compu_method_name".to_string(),
            1,
            0.0,
            100.0,
        );
        axis_descr.axis_pts_ref = Some(AxisPtsRef::new("axispts_name".to_string()));
        axis_descr.byte_order = Some(ByteOrder::new(ByteOrderEnum::MsbLast));
        axis_descr.curve_axis_ref = Some(CurveAxisRef::new("characteristic_name".to_string()));
        axis_descr.deposit = Some(Deposit::new(DepositMode::Absolute));
        axis_descr.extended_limits = Some(ExtendedLimits::new(-100.0, 200.0));
        axis_descr.fix_axis_par = Some(FixAxisPar::new(0, 0, 0));
        axis_descr.fix_axis_par_dist = Some(FixAxisParDist::new(0, 0, 0));
        let mut fix_axis_par_list = FixAxisParList::new();
        fix_axis_par_list.axis_pts_value_list.push(0.0);
        axis_descr.fix_axis_par_list = Some(fix_axis_par_list);
        axis_descr.format = Some(Format::new("%1.1".to_string()));
        axis_descr.max_grad = Some(MaxGrad::new(1.0));
        axis_descr.read_only = Some(ReadOnly::new());
        axis_descr.step_size = Some(StepSize::new(1.0));
        characteristic.axis_descr.push(axis_descr);
        characteristic.comparison_quantity =
            Some(ComparisonQuantity::new("measurement_name".to_string()));
        let mut dependent_characteristic = DependentCharacteristic::new("formula".to_string());
        dependent_characteristic
            .characteristic_list
            .push("characteristic_name".to_string());
        characteristic.dependent_characteristic = Some(dependent_characteristic);
        characteristic.map_list = Some(MapList::new());
        characteristic.number = Some(Number::new(1));
        let mut virtual_characteristic = VirtualCharacteristic::new("formula".to_string());
        virtual_characteristic
            .characteristic_list
            .push("characteristic_name".to_string());
        characteristic.virtual_characteristic = Some(virtual_characteristic);
        characteristic.if_data.push(IfData::default());
        module.characteristic.push(characteristic);
        let mut compu_method = CompuMethod::new(
            "compu_method_name".to_string(),
            "".to_string(),
            ConversionType::Identical,
            "%4.2".to_string(),
            "unit".to_string(),
        );
        compu_method.coeffs = Some(Coeffs::new(1.0, 2.0, 3.0, 4.0, 5.0, 6.0));
        compu_method.coeffs_linear = Some(CoeffsLinear::new(1.0, 2.0));
        compu_method.compu_tab_ref = Some(CompuTabRef::new("compu_tab_name".to_string()));
        let mut formula = Formula::new("formula".to_string());
        formula.formula_inv = Some(FormulaInv::new("inverse".to_string()));
        compu_method.formula = Some(formula);
        compu_method.ref_unit = Some(RefUnit::new("unit_name".to_string()));
        compu_method.status_string_ref = Some(StatusStringRef::new("compu_vtab_name".to_string()));
        module.compu_method.push(compu_method);
        let mut compu_tab = CompuTab::new(
            "compu_tab_name".to_string(),
            "long_identifier".to_string(),
            ConversionType::Identical,
            2,
        );
        compu_tab.tab_entry.push(TabEntryStruct::new(1.0, 22.0));
        compu_tab.tab_entry.push(TabEntryStruct::new(2.0, 33.0));
        compu_tab.default_value = Some(DefaultValue::new("abc".to_string()));
        compu_tab.default_value_numeric = Some(DefaultValueNumeric::new(44.0));
        module.compu_tab.push(compu_tab);
        let mut compu_vtab = CompuVtab::new(
            "compu_vtab_name".to_string(),
            "long_identifier".to_string(),
            ConversionType::Identical,
            2,
        );
        compu_vtab
            .value_pairs
            .push(ValuePairsStruct::new(1.0, "abc".to_string()));
        compu_vtab
            .value_pairs
            .push(ValuePairsStruct::new(2.0, "def".to_string()));
        compu_vtab.default_value = Some(DefaultValue::new("abc".to_string()));
        module.compu_vtab.push(compu_vtab);
        let mut compu_vtab_range = CompuVtabRange::new(
            "compu_vtab_range_name".to_string(),
            "long_identifier".to_string(),
            2,
        );
        compu_vtab_range
            .value_triples
            .push(ValueTriplesStruct::new(1.0, 2.0, "abc".to_string()));
        compu_vtab_range.default_value = Some(DefaultValue::new("abc".to_string()));
        module.compu_vtab_range.push(compu_vtab_range);
        let mut frame = Frame::new(
            "frame_name".to_string(),
            "long_identifier".to_string(),
            1,
            2,
        );
        let mut frame_measurement = FrameMeasurement::new();
        frame_measurement
            .identifier_list
            .push("measurement_name".to_string());
        frame.frame_measurement = Some(frame_measurement);
        frame.if_data.push(IfData::default());
        module.frame.push(frame);
        let mut function =
            Function::new("function_name".to_string(), "long_identifier".to_string());
        function.annotation.push(Annotation::new());
        let mut ar_component = ArComponent::new("ar_component".to_string());
        ar_component.ar_prototype_of = Some(ArPrototypeOf::new("function".to_string()));
        function.ar_component = Some(ar_component);
        function.function_version = Some(FunctionVersion::new("version-1.1.1".to_string()));
        let mut def_characteristic = DefCharacteristic::new();
        def_characteristic
            .identifier_list
            .push("characteristic_name".to_string());
        function.def_characteristic = Some(def_characteristic);
        let mut in_measurement = InMeasurement::new();
        in_measurement
            .identifier_list
            .push("measurement_name".to_string());
        function.in_measurement = Some(in_measurement);
        let mut loc_measurement = LocMeasurement::new();
        loc_measurement
            .identifier_list
            .push("measurement_name".to_string());
        function.loc_measurement = Some(loc_measurement);
        let mut out_measurement = OutMeasurement::new();
        out_measurement
            .identifier_list
            .push("measurement_name".to_string());
        function.out_measurement = Some(out_measurement);
        let mut ref_characteristic = RefCharacteristic::new();
        ref_characteristic
            .identifier_list
            .push("characteristic_name".to_string());
        function.ref_characteristic = Some(ref_characteristic);
        let mut sub_function = SubFunction::new();
        sub_function
            .identifier_list
            .push("function_name".to_string());
        function.sub_function = Some(sub_function);
        function.if_data.push(IfData::default());
        module.function.push(function);
        let mut group = Group::new("group_name".to_string(), "long_identifier".to_string());
        group.function_list = Some(FunctionList::new());
        group.ref_characteristic = Some(RefCharacteristic::new());
        group.ref_measurement = Some(RefMeasurement::new());
        group.root = Some(Root::new());
        group.sub_group = Some(SubGroup::new());
        group.if_data.push(IfData::default());
        module.group.push(group);
        let mut if_data = IfData::new();
        if_data.ifdata_items = Some(GenericIfData::Block {
            incfile: None,
            line: 0,
            items: vec![GenericIfData::Int(1, (1, false))],
        });
        if_data.ifdata_valid = true;
        module.if_data.push(if_data);
        let mut instance = Instance::new(
            "instance_name".to_string(),
            "long_identifier".to_string(),
            "typedef_structure_name".to_string(),
            0x1234,
        );
        instance.address_type = Some(AddressType::new(AddrType::Plonglong));
        instance.calibration_access =
            Some(CalibrationAccess::new(CalibrationAccessEnum::Calibration));
        instance.display_identifier =
            Some(DisplayIdentifier::new("display_identifier".to_string()));
        instance.ecu_address_extension = Some(EcuAddressExtension::new(0x3000));
        instance.layout = Some(Layout::new(IndexMode::RowDir));
        let mut matrix_dim = MatrixDim::new();
        matrix_dim.dim_list.push(3);
        instance.matrix_dim = Some(matrix_dim);
        instance.max_refresh = Some(MaxRefresh::new(1, 1));
        instance.model_link = Some(ModelLink::new("model link".to_string()));
        let mut overwrite = Overwrite::new("overwrite_name".to_string(), 0);
        overwrite.conversion = Some(Conversion::new("compu_method_name".to_string()));
        overwrite.extended_limits = Some(ExtendedLimits::new(-100.0, 1000.0));
        overwrite.format = Some(Format::new("%1.3".to_string()));
        overwrite.input_quantity = Some(InputQuantity::new("input_quantity".to_string()));
        overwrite.limits = Some(Limits::new(0.0, 100.0));
        overwrite.monotony = Some(Monotony::new(MonotonyType::MonIncrease));
        overwrite.phys_unit = Some(PhysUnit::new("unit".to_string()));
        instance.overwrite.push(overwrite);
        instance.read_only = Some(ReadOnly::new());
        instance.symbol_link = Some(SymbolLink::new("symbol name".to_string(), 0x1234));
        instance.if_data.push(IfData::default());
        module.instance.push(instance);
        let mut measurement = Measurement::new(
            "measurement_name".to_string(),
            "long_identifier".to_string(),
            DataType::Float32Ieee,
            "compu_method_name".to_string(),
            1,
            1.0,
            0.0,
            100.0,
        );
        measurement.array_size = Some(ArraySize::new(1));
        measurement.bit_mask = Some(BitMask::new(0xF0));
        measurement.if_data.push(IfData::default());
        let mut bit_operation = BitOperation::new();
        bit_operation.left_shift = Some(LeftShift::new(1));
        bit_operation.right_shift = Some(RightShift::new(2));
        bit_operation.sign_extend = Some(SignExtend::new());
        measurement.bit_operation = Some(bit_operation);
        measurement.byte_order = Some(ByteOrder::new(ByteOrderEnum::MsbLast));
        measurement.discrete = Some(Discrete::new());
        measurement.display_identifier =
            Some(DisplayIdentifier::new("display_identifier".to_string()));
        measurement.ecu_address = Some(EcuAddress::new(0x1234));
        measurement.ecu_address_extension = Some(EcuAddressExtension::new(0x4444));
        measurement.error_mask = Some(ErrorMask::new(0xab));
        measurement.format = Some(Format::new("%1.2".to_string()));
        measurement.layout = Some(Layout::new(IndexMode::ColumnDir));
        let mut matrix_dim = MatrixDim::new();
        matrix_dim.dim_list.append(&mut vec![1, 2, 3]);
        measurement.matrix_dim = Some(matrix_dim);
        measurement.max_refresh = Some(MaxRefresh::new(1, 1));
        measurement.model_link = Some(ModelLink::new("model link".to_string()));
        measurement.phys_unit = Some(PhysUnit::new("unit".to_string()));
        measurement.read_write = Some(ReadWrite::new());
        measurement.ref_memory_segment = Some(RefMemorySegment::new("memory_segment".to_string()));
        measurement.symbol_link = Some(SymbolLink::new("symbol".to_string(), 0x1234));
        let mut var_virtual = Virtual::new();
        var_virtual.measuring_channel_list.push("ident".to_string());
        measurement.var_virtual = Some(var_virtual);
        module.measurement.push(measurement);
        let mut mod_common = ModCommon::new("description".to_string());
        mod_common.alignment_byte = Some(AlignmentByte::new(1));
        mod_common.alignment_float16_ieee = Some(AlignmentFloat16Ieee::new(2));
        mod_common.alignment_float32_ieee = Some(AlignmentFloat32Ieee::new(4));
        mod_common.alignment_float64_ieee = Some(AlignmentFloat64Ieee::new(8));
        mod_common.alignment_int64 = Some(AlignmentInt64::new(8));
        mod_common.alignment_long = Some(AlignmentLong::new(4));
        mod_common.alignment_word = Some(AlignmentWord::new(2));
        mod_common.byte_order = Some(ByteOrder::new(ByteOrderEnum::MsbLast));
        mod_common.data_size = Some(DataSize::new(42));
        mod_common.deposit = Some(Deposit::new(DepositMode::Absolute));
        mod_common.s_rec_layout = Some(SRecLayout::new("name".to_string()));
        module.mod_common = Some(mod_common);
        let mut mod_par = ModPar::new("Comment".to_string());
        mod_par.addr_epk.push(AddrEpk::new(0x1234567));
        let mut calibration_method = CalibrationMethod::new("InCircuit".to_string(), 1);
        let mut calibration_handle = CalibrationHandle::new();
        calibration_handle.handle_list = vec![1, 2, 3, 4, 5];
        calibration_handle.calibration_handle_text =
            Some(CalibrationHandleText::new("txt".to_string()));
        calibration_method.calibration_handle = vec![calibration_handle];
        mod_par.calibration_method.push(calibration_method);
        mod_par.cpu_type = Some(CpuType::new("leg".to_string()));
        mod_par.customer = Some(Customer::new("c".to_string()));
        mod_par.customer_no = Some(CustomerNo::new("1".to_string()));
        mod_par.ecu = Some(Ecu::new("e".to_string()));
        mod_par.ecu_calibration_offset = Some(EcuCalibrationOffset::new(1));
        mod_par.epk = Some(Epk::new("e".to_string()));
        let mut memory_layout = MemoryLayout::new(ProgType::PrgData, 0x1234, 1, [0; 5]);
        memory_layout.if_data.push(IfData::default());
        mod_par.memory_layout.push(memory_layout);
        mod_par.memory_segment.push(MemorySegment::new(
            "memory_segment".to_string(),
            "long_identifier".to_string(),
            PrgType::Data,
            MemoryType::Ram,
            MemoryAttribute::Extern,
            0,
            0,
            [0; 5],
        ));
        mod_par.no_of_interfaces = Some(NoOfInterfaces::new(1));
        mod_par.phone_no = Some(PhoneNo::new("1".to_string()));
        mod_par.supplier = Some(Supplier::new("s".to_string()));
        mod_par
            .system_constant
            .push(SystemConstant::new("c".to_string(), "1".to_string()));
        mod_par.user = Some(User::new("u".to_string()));
        mod_par.version = Some(Version::new("1".to_string()));
        module.mod_par = Some(mod_par);
        let mut record_layout = RecordLayout::new("record_layout_name".to_string());
        record_layout.alignment_byte = Some(AlignmentByte::new(1));
        record_layout.alignment_float16_ieee = Some(AlignmentFloat16Ieee::new(2));
        record_layout.alignment_float32_ieee = Some(AlignmentFloat32Ieee::new(4));
        record_layout.alignment_float64_ieee = Some(AlignmentFloat64Ieee::new(8));
        record_layout.alignment_int64 = Some(AlignmentInt64::new(8));
        record_layout.alignment_long = Some(AlignmentLong::new(4));
        record_layout.alignment_word = Some(AlignmentWord::new(2));
        record_layout.axis_pts_x = Some(AxisPtsDim::new(
            1,
            DataType::Sword,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_pts_y = Some(AxisPtsDim::new(
            1,
            DataType::Sword,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_pts_z = Some(AxisPtsDim::new(
            1,
            DataType::Sword,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_pts_4 = Some(AxisPtsDim::new(
            1,
            DataType::Sword,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_pts_5 = Some(AxisPtsDim::new(
            1,
            DataType::Sword,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_rescale_x = Some(AxisRescaleDim::new(
            2,
            DataType::Sword,
            1,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_rescale_y = Some(AxisRescaleDim::new(
            2,
            DataType::Sword,
            1,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_rescale_z = Some(AxisRescaleDim::new(
            2,
            DataType::Sword,
            1,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_rescale_4 = Some(AxisRescaleDim::new(
            2,
            DataType::Sword,
            1,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.axis_rescale_5 = Some(AxisRescaleDim::new(
            2,
            DataType::Sword,
            1,
            IndexOrder::IndexIncr,
            AddrType::Direct,
        ));
        record_layout.dist_op_x = Some(DistOpDim::new(3, DataType::Sword));
        record_layout.dist_op_y = Some(DistOpDim::new(3, DataType::Sword));
        record_layout.dist_op_z = Some(DistOpDim::new(3, DataType::Sword));
        record_layout.dist_op_4 = Some(DistOpDim::new(3, DataType::Sword));
        record_layout.dist_op_5 = Some(DistOpDim::new(3, DataType::Sword));
        record_layout.fix_no_axis_pts_x = Some(FixNoAxisPtsDim::new(1));
        record_layout.fix_no_axis_pts_y = Some(FixNoAxisPtsDim::new(1));
        record_layout.fix_no_axis_pts_z = Some(FixNoAxisPtsDim::new(1));
        record_layout.fix_no_axis_pts_4 = Some(FixNoAxisPtsDim::new(1));
        record_layout.fix_no_axis_pts_5 = Some(FixNoAxisPtsDim::new(1));
        record_layout.fnc_values = Some(FncValues::new(
            4,
            DataType::Sword,
            IndexMode::RowDir,
            AddrType::Direct,
        ));
        record_layout.identification = Some(Identification::new(9, DataType::Sword));
        record_layout.no_axis_pts_x = Some(NoAxisPtsDim::new(10, DataType::Sword));
        record_layout.no_axis_pts_y = Some(NoAxisPtsDim::new(10, DataType::Sword));
        record_layout.no_axis_pts_z = Some(NoAxisPtsDim::new(10, DataType::Sword));
        record_layout.no_axis_pts_4 = Some(NoAxisPtsDim::new(10, DataType::Sword));
        record_layout.no_axis_pts_5 = Some(NoAxisPtsDim::new(10, DataType::Sword));
        record_layout.no_rescale_x = Some(NoRescaleDim::new(11, DataType::Sword));
        record_layout.no_rescale_y = Some(NoRescaleDim::new(11, DataType::Sword));
        record_layout.no_rescale_z = Some(NoRescaleDim::new(11, DataType::Sword));
        record_layout.no_rescale_4 = Some(NoRescaleDim::new(11, DataType::Sword));
        record_layout.no_rescale_5 = Some(NoRescaleDim::new(11, DataType::Sword));
        record_layout.offset_x = Some(OffsetDim::new(12, DataType::Sword));
        record_layout.offset_y = Some(OffsetDim::new(12, DataType::Sword));
        record_layout.offset_z = Some(OffsetDim::new(12, DataType::Sword));
        record_layout.offset_4 = Some(OffsetDim::new(12, DataType::Sword));
        record_layout.offset_5 = Some(OffsetDim::new(12, DataType::Sword));
        record_layout
            .reserved
            .push(Reserved::new(5, DataTypeSize::Word));
        record_layout.rip_addr_w = Some(RipAddrDim::new(6, DataType::Sword));
        record_layout.rip_addr_x = Some(RipAddrDim::new(6, DataType::Sword));
        record_layout.rip_addr_y = Some(RipAddrDim::new(6, DataType::Sword));
        record_layout.rip_addr_z = Some(RipAddrDim::new(6, DataType::Sword));
        record_layout.rip_addr_4 = Some(RipAddrDim::new(6, DataType::Sword));
        record_layout.rip_addr_5 = Some(RipAddrDim::new(6, DataType::Sword));
        record_layout.src_addr_x = Some(SrcAddrDim::new(7, DataType::Sword));
        record_layout.src_addr_y = Some(SrcAddrDim::new(7, DataType::Sword));
        record_layout.src_addr_z = Some(SrcAddrDim::new(7, DataType::Sword));
        record_layout.src_addr_4 = Some(SrcAddrDim::new(7, DataType::Sword));
        record_layout.src_addr_5 = Some(SrcAddrDim::new(7, DataType::Sword));
        record_layout.shift_op_x = Some(ShiftOpDim::new(8, DataType::Sword));
        record_layout.shift_op_y = Some(ShiftOpDim::new(8, DataType::Sword));
        record_layout.shift_op_z = Some(ShiftOpDim::new(8, DataType::Sword));
        record_layout.shift_op_4 = Some(ShiftOpDim::new(8, DataType::Sword));
        record_layout.shift_op_5 = Some(ShiftOpDim::new(8, DataType::Sword));
        record_layout.static_address_offsets = Some(StaticAddressOffsets::new());
        record_layout.static_record_layout = Some(StaticRecordLayout::new());
        module.record_layout.push(record_layout);
        let mut transformer = Transformer::new(
            "transformer_name".to_string(),
            "version string".to_string(),
            "dll32".to_string(),
            "dll64".to_string(),
            1,
            TransformerTrigger::OnChange,
            "NO_INVERSE_TRANSFORMER".to_string(),
        );
        transformer.transformer_in_objects = Some(TransformerInObjects::new());
        transformer.transformer_out_objects = Some(TransformerOutObjects::new());
        module.transformer.push(transformer);
        let mut typedef_axis = TypedefAxis::new(
            "typedef_axis_name".to_string(),
            "long_identifier".to_string(),
            "measurement_name".to_string(),
            "record_layout_name".to_string(),
            0.0,
            "compu_method_name".to_string(),
            1,
            0.0,
            100.0,
        );
        typedef_axis.byte_order = Some(ByteOrder::new(ByteOrderEnum::MsbLast));
        typedef_axis.monotony = Some(Monotony::new(MonotonyType::MonDecrease));
        typedef_axis.step_size = Some(StepSize::new(3.0));
        module.typedef_axis.push(typedef_axis);
        let mut typedef_blob = TypedefBlob::new(
            "typedef_blob_name".to_string(),
            "long_identifier".to_string(),
            1,
        );
        typedef_blob.address_type = Some(AddressType::new(AddrType::Direct));
        module.typedef_blob.push(typedef_blob);
        let mut typedef_characteristic = TypedefCharacteristic::new(
            "typedef_characteristic_name".to_string(),
            "long_identifier".to_string(),
            CharacteristicType::Value,
            "record_layout_name".to_string(),
            0.0,
            "compu_method_name".to_string(),
            0.0,
            100.0,
        );
        typedef_characteristic.bit_mask = Some(BitMask::new(0x22));
        typedef_characteristic.discrete = Some(Discrete::new());
        typedef_characteristic.encoding = Some(Encoding::new(CharacterEncoding::Utf8));
        module.typedef_characteristic.push(typedef_characteristic);
        let mut typedef_measurement = TypedefMeasurement::new(
            "typedef_measurement_name".to_string(),
            "long_identifier".to_string(),
            DataType::Ubyte,
            "compu_method_name".to_string(),
            1,
            1.0,
            0.0,
            100.0,
        );
        typedef_measurement.address_type = Some(AddressType::new(AddrType::Direct));
        module.typedef_measurement.push(typedef_measurement);
        let mut typedef_structure = TypedefStructure::new(
            "typedef_structure_name".to_string(),
            "long_identifier".to_string(),
            1,
        );
        typedef_structure.consistent_exchange = Some(ConsistentExchange::new());
        let mut structure_component = StructureComponent::new(
            "component_name".to_string(),
            "typedef_characteristic_name".to_string(),
            1,
        );
        structure_component.address_type = Some(AddressType::new(AddrType::Direct));
        structure_component.layout = Some(Layout::new(IndexMode::ColumnDir));
        let mut matrix_dim = MatrixDim::new();
        matrix_dim.dim_list.push(1);
        structure_component.matrix_dim = Some(matrix_dim);
        structure_component.symbol_type_link = Some(SymbolTypeLink::new("abc".to_string()));
        typedef_structure
            .structure_component
            .push(structure_component);
        typedef_structure.symbol_type_link = Some(SymbolTypeLink::new("abcdef".to_string()));
        module.typedef_structure.push(typedef_structure);
        let mut unit = Unit::new(
            "unit_name".to_string(),
            "long_identifier".to_string(),
            "x".to_string(),
            UnitType::Derived,
        );
        unit.ref_unit = Some(RefUnit::new("ref_unit".to_string()));
        unit.si_exponents = Some(SiExponents::new(1, 2, 3, 4, 5, 6, 7));
        unit.unit_conversion = Some(UnitConversion::new(1.0, 1.0));
        module.unit.push(unit);
        let mut user_rights = UserRights::new("user".to_string());
        user_rights.read_only = Some(ReadOnly::new());
        let mut ref_group = RefGroup::new();
        ref_group.identifier_list.push("group".to_string());
        user_rights.ref_group.push(ref_group);
        module.user_rights.push(user_rights);
        module
            .user_rights
            .push(UserRights::new("user2".to_string()));

        let mut variant_coding = VariantCoding::new();
        let mut var_characteristic = VarCharacteristic::new("name".to_string());
        var_characteristic
            .criterion_name_list
            .push("criterion_name".to_string());
        let mut var_address = VarAddress::new();
        var_address.address_list.push(0x1234);
        var_characteristic.var_address = Some(var_address);
        let mut var_forbidden_comb = VarForbiddenComb::new();
        var_forbidden_comb
            .combination
            .push(CombinationStruct::new("abc".to_string(), "def".to_string()));
        let mut var_criterion = VarCriterion::new("var_criterion_name".to_string(), "".to_string());
        var_criterion
            .value_list
            .extend(["abc".to_string(), "def".to_string()]);
        var_criterion.var_measurement = Some(VarMeasurement::new("measurement_name".to_string()));
        var_criterion.var_selection_characteristic = Some(VarSelectionCharacteristic::new(
            "characteristic_name".to_string(),
        ));
        variant_coding.var_characteristic.push(var_characteristic);
        variant_coding.var_naming = Some(VarNaming::new(VarNamingTag::Numeric));
        variant_coding.var_separator = Some(VarSeparator::new(".".to_string()));
        variant_coding.var_forbidden_comb.push(var_forbidden_comb);
        variant_coding.var_criterion.push(var_criterion);
        module.variant_coding = Some(variant_coding);
        assert_eq!(a2l_file, a2l_file5);

        // check the data in a2l_file and a2l_file5
        let check_msgs = a2l_file.check();
        for msg in &check_msgs {
            println!("{}", msg);
        }
        assert_eq!(check_msgs.len(), 0);
        let check_msgs = a2l_file5.check();
        assert_eq!(check_msgs.len(), 0);

        // verify store + load round trip of newly created data
        let serialized3 = a2l_file5.write_to_string();
        let (a2l_file6, _) = load_from_string(&serialized3, None, false).unwrap();
        assert_eq!(a2l_file, a2l_file6);

        let mut a2l_file7 = a2l_file5.clone();

        // a2l_file5 was not loaded from a file/string, so the elements have no layout info
        assert_eq!(a2l_file5.project.get_layout().uid, 0);
        assert_eq!(a2l_file5.project.module[0].get_layout().uid, 0);
        assert_eq!(
            a2l_file5.project.module[0].measurement[0].get_layout().uid,
            0
        );

        // sorting does not affect equality
        a2l_file.sort();
        assert_eq!(a2l_file, a2l_file5);
        a2l_file5.sort();
        assert_eq!(a2l_file, a2l_file5);

        // after sorting, the data contains uids to indicate the desired ordering
        assert_ne!(a2l_file5.project.get_layout().uid, 0);
        assert_ne!(a2l_file5.project.module[0].get_layout().uid, 0);
        assert_ne!(
            a2l_file5.project.module[0].measurement[0].get_layout().uid,
            0
        );

        a2l_file.sort_new_items();
        a2l_file7.sort_new_items();
        assert_eq!(a2l_file, a2l_file7);
    }

    #[test]
    fn long_masks() {
        // regression test for issue #41 - 64bit BIT_MASK and ERROR_MASK values are not parsed correctly
        let a2l = r#"ASAP2_VERSION 1 71
        /begin PROJECT project ""
            /begin MODULE module ""
                /begin MEASUREMENT measurement_name "long_identifier" FLOAT32_IEEE compu_method_name 1 1.0 0 100
                    BIT_MASK 0xFFFFFFFFFFFFFFFF
                    ERROR_MASK 0xFFFFFFFFFFFFFFFF
                /end MEASUREMENT
            /end MODULE
        /end PROJECT
        "#;
        let (a2lfile, _) = a2lfile::load_from_string(a2l, None, false).unwrap();

        let measurement = &a2lfile.project.module[0].measurement[0];
        assert_eq!(
            measurement.bit_mask.as_ref().unwrap().mask,
            0xFFFFFFFFFFFFFFFFu64
        );
        assert_eq!(
            measurement.error_mask.as_ref().unwrap().mask,
            0xFFFFFFFFFFFFFFFFu64
        );
    }
}
