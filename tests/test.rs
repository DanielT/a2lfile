#[cfg(test)]
mod test {
    use std::{collections::HashMap, vec};

    use a2lfile::*;

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
        let mut log_msgs = Vec::<A2lError>::new();
        let a2lfile = a2lfile::load_from_string(TEST_A2L, None, &mut log_msgs, false).unwrap();
        let text = a2lfile.write_to_string();
        println!("input:\n{}\noutput:\n{}\n", TEST_A2L, text);

        assert_eq!(TEST_A2L, text);
    }

    #[test]
    fn full_test() {
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
        function.ar_component = Some(ArComponent::new("name".to_string()));
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

        let mut log_msgs = Vec::<A2lError>::new();
        let mut other_a2l_file =
            a2lfile::load_from_string(TEST_A2L, None, &mut log_msgs, false).unwrap();
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
        let mut log_msgs = Vec::<String>::new();
        a2l_file.check(&mut log_msgs);
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
        let mut log_msgs = Vec::<A2lError>::new();
        let a2l_file2 = a2lfile::load_from_string(&a2ldata, None, &mut log_msgs, false);
        assert!(a2l_file2.is_ok());
        let a2l_file2 = a2l_file2.unwrap();
        assert_eq!(a2l_file, a2l_file2);

        let mut a2l_file3 = a2lfile::load(
            "test.a2l",
            Some(A2MLTEST_TEXT.to_string()),
            &mut log_msgs,
            false,
        )
        .unwrap();
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
        assert_eq!(
            a2l_file3.project.module[0].measurement[0].if_data[0].ifdata_valid,
            false
        );
        // ifdata on the other measurement is valid
        assert_eq!(a2l_file3.project.module[0].measurement[1].if_data.len(), 1);
        assert_eq!(
            a2l_file3.project.module[0].measurement[1].if_data[0].ifdata_valid,
            true
        );
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
        let mut log_msgs = Vec::<A2lError>::new();
        let load_result = a2lfile::load_from_string(&data_bad, None, &mut log_msgs, false);
        assert!(load_result.is_err());

        let data_bad = r##"ASAP2_VERSION 1 71"##;
        let mut log_msgs = Vec::<A2lError>::new();
        let load_result = a2lfile::load_from_string(&data_bad, None, &mut log_msgs, false);
        assert!(load_result.is_err());

        let data_bad = r##"ASAP2_VERSION 1 71
        /begin PROJECT
        /end PROJECT"##;
        let mut log_msgs = Vec::<A2lError>::new();
        let load_result = a2lfile::load_from_string(&data_bad, None, &mut log_msgs, false);
        assert!(load_result.is_err());

        let data_bad = r##"ASAP2_VERSION 1 71
        /begin PROJECT //x ""
        /end PROJECT"##;
        let mut log_msgs = Vec::<A2lError>::new();
        let load_result = a2lfile::load_from_string(&data_bad, None, &mut log_msgs, false);
        assert!(load_result.is_err());

        let data_bad = r##"ASAP2_VERSION 1 71
        /beginPROJECT x ""
        /end PROJECT"##;
        let mut log_msgs = Vec::<A2lError>::new();
        let load_result = a2lfile::load_from_string(&data_bad, None, &mut log_msgs, false);
        assert!(load_result.is_err());

        let data_good = r##"ASAP2_VERSION 1 0x47
        /begin PROJECT x ""
        /end PROJECT"##;
        let mut log_msgs = Vec::<A2lError>::new();
        let load_result = a2lfile::load_from_string(&data_good, None, &mut log_msgs, false);
        assert!(load_result.is_ok());

        let data_good = r##"ASAP2_VERSION 1 71
        /begin PROJECT x ""
        /end PROJECT"##;
        let mut log_msgs = Vec::<A2lError>::new();
        let load_result = a2lfile::load_from_string(&data_good, None, &mut log_msgs, false);
        assert!(load_result.is_ok());

        let data_good = r##"//comment
        ASAP2_VERSION 1 71 //comment
        /begin PROJECT x "" /*/*////*
        /end PROJECT"##;
        let mut log_msgs = Vec::<A2lError>::new();
        let load_result = a2lfile::load_from_string(&data_good, None, &mut log_msgs, false);
        assert!(load_result.is_ok());
    }
}
