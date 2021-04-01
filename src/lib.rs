mod a2lloader;
mod a2ltokenizer;
mod a2lparser;

use a2lmacros::a2l_specification;
pub use a2lmacros::a2ml_specification;
pub use a2ltokenizer::{A2lToken, A2lTokenType};
pub use a2lparser::{ParseContext, ParseError, ParserState};

trait A2lObject {
    fn parse(parser: &mut a2lparser::ParserState) -> Result<Box<Self>, ParseError>;
}


a2l_specification! {
    block A2L_FILE {
        [-> ASAP2_VERSION]!
        [-> A2ML_VERSION]
        [-> PROJECT]!
    }

    // Specification: predefined data types
    enum AddrType {
        PBYTE,
        PWORD,
        PLONG,
        DIRECT
    }

     // Specification: predefined data types (datasize)
     enum DataTypeSize {
        BYTE,
        WORD,
        LONG
    }

    // Specification: predefined data types
    enum DataType {
        UBYTE,
        SBYTE,
        UWORD,
        SWORD,
        ULONG,
        SLONG,
        A_UINT64,
        A_INT64,
        FLOAT32_IEEE,
        FLOAT64_IEEE
    }

    // Specification: predefined data types
    enum IndexOrder {
        INDEX_INCR,
        INDEX_DECR
    }

    // Specification: 3.5.2
    block A2ML {
        string a2ml_text
        string filename
    }

    // Specification: 3.5.3
    keyword A2ML_VERSION {
        uint version_no
        uint upgrade_no
    }

    // Specification: 3.5.4
    keyword ADDR_EPK {
        ulong address
    }

    // Specification: 3.5.5
    keyword ALIGNMENT_BYTE {
        uint alignment_border
    }

    // Specification: 3.5.6
    keyword ALIGNMENT_FLOAT32_IEEE {
        uint alignment_border
    }

    // Specification: 3.5.7
    keyword ALIGNMENT_FLOAT64_IEEE {
        uint alignment_border
    }

    // Specification: 3.5.8
    keyword ALIGNMENT_INT64 {
        uint alignment_border
    }

    // Specification: 3.5.9
    keyword ALIGNMENT_LONG {
        uint alignment_border
    }

    // Specification: 3.5.10
    keyword ALIGNMENT_WORD {
        uint alignment_border
    }

    // Specification: 3.5.11
    block ANNOTATION {
        [-> ANNOTATION_LABEL]
        [-> ANNOTATION_ORIGIN]
        [-> ANNOTATION_TEXT]
    }

    // Specification: 3.5.12
    keyword ANNOTATION_LABEL {
        string label
    }

    // Specification: 3.5.13
    keyword ANNOTATION_ORIGIN {
        string origin
    }

    // Specification: 3.5.14
    block ANNOTATION_TEXT {
        {string annotation_text}* annotation_text_list
    }

    // Specification: 3.5.15
    keyword ARRAY_SIZE {
        uint number
    }

    // Specification: 3.5.16
    keyword ASAP2_VERSION {
        uint version_no
        uint upgrade_no
    }

    // Specification: 3.5.17
    enum AxisDescrAttribute {
        CURVE_AXIS,
        COM_AXIS,
        FIX_AXIS,
        RES_AXIS,
        STD_AXIS
    }

    // Specification: 3.5.17
    block AXIS_DESCR {
        AxisDescrAttribute attribute
        ident input_quantity
        ident conversion
        uint max_axis_points
        float lower_limit
        float upper_limit
        [-> ANNOTATION]*
        [-> AXIS_PTS_REF]
        [-> BYTE_ORDER]
        [-> CURVE_AXIS_REF]
        [-> DEPOSIT]
        [-> EXTENDED_LIMITS]
        [-> FIX_AXIS_PAR]
        [-> FIX_AXIS_PAR_DIST]
        [-> FIX_AXIS_PAR_LIST]
        [-> FORMAT]
        [-> MAX_GRAD]
        [-> MONOTONY]
        [-> PHYS_UNIT]
        [-> READ_ONLY]
        [-> STEP_SIZE]
    }

    // Specification: 3.5.18
    block AXIS_PTS {
        ident name
        string long_identifier
        ulong address
        ident input_quantity
        ident deposit_record
        float max_diff
        ident conversion
        uint max_axis_points
        float lower_limit
        float upper_limit
        [-> ANNOTATION]*
        [-> BYTE_ORDER]
        [-> CALIBRATION_ACCESS]
        [-> DEPOSIT]
        [-> DISPLAY_IDENTIFIER]
        [-> ECU_ADDRESS_EXTENSION]
        [-> EXTENDED_LIMITS]
        [-> FORMAT]
        [-> FUNCTION_LIST]
        [-> GUARD_RAILS]
        [-> IF_DATA]*
        [-> MONOTONY]
        [-> PHYS_UNIT]
        [-> READ_ONLY]
        [-> REF_MEMORY_SEGMENT]
        [-> STEP_SIZE]
        [-> SYMBOL_LINK]
    }

    // Specification: 3.5.19
    keyword AXIS_PTS_REF {
        ident axis_points
    }

    // Specification: 3.5.20
    keyword AXIS_PTS_X / _Y / _Z / _4 / _5 {
        uint position
        DataType datatype
        IndexOrder index_incr
        AddrType addressing
    }

    // Specification: 3.5.21
    keyword AXIS_RESCALE_X /_Y /_Z / _4 / _5 {
        uint position
        DataType datatype
        uint max_number_of_rescale_pairs
        IndexOrder index_incr
        AddrType addressing
    }

    // Specification: 3.5.22
    keyword BIT_MASK {
        ulong mask
    }

    // Specification: 3.5.23
    block BIT_OPERATION {
        [-> LEFT_SHIFT]
        [-> RIGHT_SHIFT]
        [-> SIGN_EXTEND]
    }

    // Specification: 3.5.24
    enum ByteOrderEnum {
        LITTLE_ENDIAN,
        BIG_ENDIAN,
        MSB_LAST,
        MSB_FIRST
    }

    keyword BYTE_ORDER {
        ByteOrderEnum byte_order
    }

    // Specification: 3.5.25
    enum CalibrationAccessEnum {
        CALIBRATION,
        NO_CALIBRATION,
        NOT_IN_MCD_SYSTEM,
        OFFLINE_CALIBRATION
    }

    keyword CALIBRATION_ACCESS {
        CalibrationAccessEnum calibration_access
    }

    // Specification: 3.5.26
    block CALIBRATION_HANDLE {
        {long handle}* handle_list
        [-> CALIBRATION_HANDLE_TEXT]
    }

    // Specification: 3.5.27
    keyword CALIBRATION_HANDLE_TEXT {
        string text
    }

    // Specification: 3.5.28
    block CALIBRATION_METHOD {
        string method
        ulong version
        [-> CALIBRATION_HANDLE]
    }

    // Specification: 3.5.29
    enum CharacteristicType {
        ASCII,
        CURVE,
        MAP,
        CUBOID,
        CUBE_4,
        CUBE_5,
        VAL_BLK,
        VALUE
    }

    // Specification: 3.5.29
    block CHARACTERISTIC {
        ident name
        string long_identifier
        CharacteristicType characteristic_type
        ulong address
        ident deposit
        float max_diff
        ident conversion
        float lower_limit
        float upper_limit
        [-> ANNOTATION]*
        [-> AXIS_DESCR]*
        [-> BIT_MASK]
        [-> BYTE_ORDER]
        [-> CALIBRATION_ACCESS]
        [-> COMPARISON_QUANTITY]
        [-> DEPENDENT_CHARACTERISTIC]
        [-> DISCRETE]
        [-> DISPLAY_IDENTIFIER]
        [-> ECU_ADDRESS_EXTENSION]
        [-> EXTENDED_LIMITS]
        [-> FORMAT]
        [-> FUNCTION_LIST]
        [-> GUARD_RAILS]
        [-> IF_DATA]*
        [-> MAP_LIST]
        [-> MATRIX_DIM]
        [-> MAX_REFRESH]
        [-> NUMBER]
        [-> PHYS_UNIT]
        [-> READ_ONLY]
        [-> REF_MEMORY_SEGMENT]
        [-> STEP_SIZE]
        [-> SYMBOL_LINK]
        [-> VIRTUAL_CHARACTERISTIC]
    }

    // Specification: 3.5.30
    keyword COEFFS {
        float a
        float b
        float c
        float d
        float e
        float f
    }

    // Specification: 3.5.31
    keyword COEFFS_LINEAR {
        float a
        float b
    }

    // Specification: 3.5.32
    keyword COMPARISON_QUANTITY {
        ident name
    }

    // Specification: 3.5.33
    enum ConversionType {
        IDENTICAL,
        FORM,
        LINEAR,
        RAT_FUNC,
        TAB_INTP,
        TAB_NOINTP,
        TAB_VERB
    }

    // Specification: 3.5.33
    block COMPU_METHOD {
        ident name
        string long_identifier
        ConversionType conversion_type
        string format
        string unit
        [-> COEFFS]
        [-> COEFFS_LINEAR]
        [-> COMPU_TAB_REF]
        [-> FORMULA]
        [-> REF_UNIT]
        [-> STATUS_STRING_REF]
    }

    // Specification: 3.5.34
    block COMPU_TAB {
        ident name
        string long_identifier
        ConversionType conversion_type
        uint number_value_pairs
        {
            float in_val
            float out_val
        }* tab_entry
        [-> DEFAULT_VALUE]
        [-> DEFAULT_VALUE_NUMERIC]
    }

    // Specification: 3.5.35
    keyword COMPU_TAB_REF {
        ident conversion_table
    }

    // Specification: 3.5.36
    block COMPU_VTAB {
        ident name
        string long_identifier
        ConversionType conversion_type
        uint number_value_pairs
        {
            float in_val
            string out_val
        }* value_pairs
        [-> DEFAULT_VALUE]
    }

    // Specification: 3.5.37
    block COMPU_VTAB_RANGE {
        ident name
        string long_identifier
        uint number_value_triples
        {
            float in_val_min
            float in_val_max
            string out_val
        }* value_triples
        [-> DEFAULT_VALUE]
    }

    // Specification: 3.5.38
    keyword CPU_TYPE {
        string cpu
    }

    // Specification: 3.5.39
    keyword CURVE_AXIS_REF {
        ident curve_axis
    }

    // Specification: 3.5.40
    keyword CUSTOMER {
        string customer
    }

    // Specification: 3.5.41
    keyword CUSTOMER_NO {
        string number
    }

    // Specification: 3.5.42
    keyword DATA_SIZE {
        uint size
    }

    // Specification: 3.5.43
    block DEF_CHARACTERISTIC {
        { ident identifier }* identifier_list
    }

    // Specification: 3.5.44
    keyword DEFAULT_VALUE {
        string display_string
    }

    // Specification: 3.5.45
    keyword DEFAULT_VALUE_NUMERIC {
        float display_value
    }

    // Specification: 3.5.46
    block DEPENDENT_CHARACTERISTIC {
        string formula
        {ident characteristic}* characteristic_list
    }

    // Specification: 3.5.47
    enum DepositMode {
        ABSOLUTE,
        DIFFERENCE
    }

    // Specification: 3.5.47
    keyword DEPOSIT {
        DepositMode mode
    }

    // Specification: 3.5.48
    keyword DISCRETE {}

    // Specification: 3.5.49
    keyword DISPLAY_IDENTIFIER {
        ident display_name
    }

    // Specification: 3.5.50
    keyword DIST_OP_X / _Y / _Z / _4 / _5 {
        uint position
        DataType datatype
    }

    // Specification: 3.5.51
    keyword ECU {
        string control_unit
    }

    // Specification: 3.5.52
    keyword ECU_ADDRESS {
        ulong address
    }

    // Specification: 3.5.53
    keyword ECU_ADDRESS_EXTENSION {
        int extension
    }

    // Specification: 3.5.54
    keyword ECU_CALIBRATION_OFFSET {
        long offset
    }

    // Specification: 3.5.55
    keyword EPK {
        string identifier
    }

    // Specification: 3.5.56
    keyword ERROR_MASK {
        ulong mask
    }

    // Specification: 3.5.57
    keyword EXTENDED_LIMITS {
        float lower_limit
        float upper_limit
    }

    // Specification: 3.5.58
    keyword FIX_AXIS_PAR {
        int offset
        int shift
        uint number_apo
    }

    // Specification: 3.5.59
    keyword FIX_AXIS_PAR_DIST {
        int offset
        int distance
        uint number_apo
    }

    // Specification: 3.5.60
    block FIX_AXIS_PAR_LIST {
        { float axis_pts_value }* axis_pts_value_list
    }

    // Specification: 3.5.61
    keyword FIX_NO_AXIS_PTS_X / _Y / _Z / _4 / _5 {
        uint number_of_axis_points
    }

    // Specification: 3.5.62
    enum IndexMode {
        ALTERNATE_CURVES,
        ALTERNATE_WITH_X,
        ALTERNATE_WITH_Y,
        COLUMN_DIR,
        ROW_DIR
    }

    // Specification: 3.5.62
    keyword FNC_VALUES {
        uint position
        DataType datatype
        IndexMode index_mode
        AddrType address_type
    }

    // Specification: 3.5.63
    keyword FORMAT {
        string format_string
    }

    // Specification: 3.5.64
    block FORMULA {
        string fx
        [-> FORMULA_INV]
    }

    // Specification: 3.5.65
    keyword FORMULA_INV {
        string gx
    }

    // Specification: 3.5.66
    block FRAME {
        ident name
        string long_identifier
        uint scaling_unit
        ulong rate
        [-> FRAME_MEASUREMENT]
        [-> IF_DATA]*
    }

    // Specification: 3.5.67
    keyword FRAME_MEASUREMENT {
        { ident identifier}* identifier_list
    }

    // Specification: 3.5.68
    block FUNCTION {
        ident name
        string long_identifier
        [-> ANNOTATION]*
        [-> DEF_CHARACTERISTIC]
        [-> FUNCTION_VERSION]
        [-> IF_DATA]*
        [-> IN_MEASUREMENT]
        [-> LOC_MEASUREMENT]
        [-> OUT_MEASUREMENT]
        [-> REF_CHARACTERISTIC]
        [-> SUB_FUNCTION]
    }

    // Specification: 3.5.69
    block FUNCTION_LIST {
        {ident name}* name_list
    }

    // Specification: 3.5.70
    keyword FUNCTION_VERSION {
        string version_identifier
    }

    // Specification: 3.5.71
    block GROUP {
        ident group_name
        string group_long_identifier
        [-> ANNOTATION]*
        [-> FUNCTION_LIST]
        [-> IF_DATA]*
        [-> REF_CHARACTERISTIC]
        [-> REF_MEASUREMENT]
        [-> ROOT]
        [-> SUB_GROUP]
    }

    // Specification: 3.5.72
    keyword GUARD_RAILS {}

    // Specification: 3.5.73
    block HEADER {
        string comment
        [-> PROJECT_NO]
        [-> VERSION]
    }

    // Specification: 3.5.74
    keyword IDENTIFICATION {
        uint position
        DataType datatype
    }

    // Specification: 3.5.75
    block IF_DATA {
        string ifdata_text
        string ifdata_filename
    }

    // Specification: 3.5.76
    block IN_MEASUREMENT {
        {ident identifier}* identifier_list
    }

    // Specification: 3.5.77
    keyword LAYOUT {
        IndexMode index_mode
    }

    // Specification: 3.5.78
    keyword LEFT_SHIFT {
        ulong bitcount
    }

    // Specification: 3.5.79
    block LOC_MEASUREMENT {
        {ident identifier}* identifier_list
    }

    // Specification: 3.5.80
    block MAP_LIST {
        {ident name}* name_list
    }

    // Specification: 3.5.81
    keyword MATRIX_DIM {
        {uint dim}* dim_list // note: changed for 1.70
    }

    // Specification: 3.5.82
    keyword MAX_GRAD {
        float max_gradient
    }

    // Specification: 3.5.83
    keyword MAX_REFRESH {
        uint scaling_unit
        ulong rate
    }

    // Specification: 3.5.84
    block MEASUREMENT {
        ident name
        string long_identifier
        DataType datatype
        ident conversion
        uint resolution
        float accuracy
        float lower_limit
        float upper_limit
        [-> ANNOTATION]*
        [-> ARRAY_SIZE]
        [-> BIT_MASK]
        [-> BIT_OPERATION]
        [-> BYTE_ORDER]
        [-> DISCRETE]
        [-> DISPLAY_IDENTIFIER]
        [-> ECU_ADDRESS]
        [-> ECU_ADDRESS_EXTENSION]
        [-> ERROR_MASK]
        [-> FORMAT]
        [-> FUNCTION_LIST]
        [-> IF_DATA]*
        [-> LAYOUT]
        [-> MATRIX_DIM]
        [-> MAX_REFRESH]
        [-> PHYS_UNIT]
        [-> READ_WRITE]
        [-> REF_MEMORY_SEGMENT]
        [-> SYMBOL_LINK]
        [-> VIRTUAL]
    }

    // Specification: 3.5.85
    enum ProgType {
        PRG_CODE,
        PRG_DATA,
        PRG_RESERVED
    }

    // Specification: 3.5.85
    block MEMORY_LAYOUT {
        ProgType prog_type
        ulong address
        ulong size
        long[5] offset
        [-> IF_DATA]*
    }

    // Specification: 3.5.86
    enum PrgType {
        CALIBRATION_VARIABLES,
        CODE,
        DATA,
        EXCLUDE_FROM_FLASH,
        OFFLINE_DATA,
        RESERVED,
        SERAM,
        VARIABLES
    }

    // Specification: 3.5.86
    enum MemoryType {
        EEPROM,
        EPROM,
        FLASH,
        RAM,
        ROM,
        REGISTER
    }

    // Specification: 3.5.86
    enum MemoryAttribute {
        INTERN,
        EXTERN
    }

    // Specification: 3.5.86
    block MEMORY_SEGMENT {
        ident name
        string long_identifier
        PrgType prg_type
        MemoryType memory_type
        MemoryAttribute attribute
        ulong address
        ulong size
        long[5] offset
        [-> IF_DATA]*
    }

    // Specification: 3.5.87
    block MOD_COMMON {
        string comment
        [-> ALIGNMENT_BYTE]
        [-> ALIGNMENT_FLOAT32_IEEE]
        [-> ALIGNMENT_FLOAT64_IEEE]
        [-> ALIGNMENT_INT64]
        [-> ALIGNMENT_LONG]
        [-> ALIGNMENT_WORD]
        [-> BYTE_ORDER]
        [-> DATA_SIZE]
        [-> DEPOSIT]
        [-> S_REC_LAYOUT]
    }

    // Specification: 3.5.88
    block MOD_PAR {
        string comment
        [-> ADDR_EPK]*
        [-> CALIBRATION_METHOD]*
        [-> CPU_TYPE]
        [-> CUSTOMER]
        [-> CUSTOMER_NO]
        [-> ECU]
        [-> ECU_CALIBRATION_OFFSET]
        [-> EPK]
        [-> MEMORY_LAYOUT]*
        [-> MEMORY_SEGMENT]*
        [-> NO_OF_INTERFACES]
        [-> PHONE_NO]
        [-> SUPPLIER]
        [-> SYSTEM_CONSTANT]*
        [-> USER]
        [-> VERSION]
    }

    // Specification: 3.5.89
    block MODULE {
        ident name
        string long_identifier
        [-> A2ML]
        [-> AXIS_PTS]*
        [-> CHARACTERISTIC]*
        [-> COMPU_METHOD]*
        [-> COMPU_TAB]*
        [-> COMPU_VTAB]*
        [-> COMPU_VTAB_RANGE]*
        [-> FRAME]
        [-> FUNCTION]*
        [-> GROUP]*
        [-> IF_DATA]*
        [-> MEASUREMENT]*
        [-> MOD_COMMON]
        [-> MOD_PAR]
        [-> RECORD_LAYOUT]*
        [-> UNIT]*
        [-> USER_RIGHTS]*
        [-> VARIANT_CODING]
    }

    // Specification: 3.5.90
    enum MonotonyType {
        MON_DECREASE,
        MON_INCREASE,
        STRICT_DECREASE,
        STRICT_INCREASE,
        MONOTONOUS,
        STRICT_MON,
        NOT_MON
    }

    // Specification: 3.5.90
    keyword MONOTONY {
        MonotonyType monotony
    }

    // Specification: 3.5.91
    keyword NO_AXIS_PTS_X / _Y / _Z / _4 / _5 {
        uint position
        DataType datatype
    }

    // Specification: 3.5.92
    keyword NO_OF_INTERFACES {
        uint num
    }

    // Specification: 3.5.93
    keyword NO_RESCALE_X / _Y / _Z / _4 / _5 {
        uint position
        DataType datatype
    }

    // Specification: 3.5.94
    keyword NUMBER {
        uint number
    }

    // Specification: 3.5.95
    keyword OFFSET_X / _Y / _Z / _4 / _5 {
        uint position
        DataType datatype
    }

    // Specification: 3.5.96
    block OUT_MEASUREMENT {
        {ident identifier}* identifier_list
    }

    // Specification: 3.5.97
    keyword PHONE_NO {
        string telnum
    }

    // Specification: 3.5.98
    keyword PHYS_UNIT {
        string unit
    }

    // Specification: 3.5.99
    block PROJECT {
        ident Name
        string long_identifier
        [-> HEADER]
        [-> MODULE]+
    }

    // Specification: 3.5.100
    keyword PROJECT_NO {
        ident project_number
    }

    // Specification: 3.5.101
    keyword READ_ONLY {}

    // Specification: 3.5.102
    keyword READ_WRITE {}

    // Specification: 3.5.103
    block RECORD_LAYOUT {
        ident name
        [-> ALIGNMENT_BYTE]
        [-> ALIGNMENT_FLOAT32_IEEE]
        [-> ALIGNMENT_FLOAT64_IEEE]
        [-> ALIGNMENT_INT64]
        [-> ALIGNMENT_LONG]
        [-> ALIGNMENT_WORD]
        [-> AXIS_PTS_X/_Y/_Z/_4/_5]
        [-> AXIS_RESCALE_X/_Y/_Z/_4/_5]
        [-> DIST_OP_X/_Y/_Z/_4/_5]
        [-> FIX_NO_AXIS_PTS_X/_Y/_Z/_4/_5]
        [-> FNC_VALUES]
        [-> IDENTIFICATION]
        [-> NO_AXIS_PTS_X/_Y/_Z/_4/_5]
        [-> NO_RESCALE_X/_Y/_Z/_4/_5]
        [-> OFFSET_X/_Y/_Z/_4/_5]
        [-> RESERVED]*
        [-> RIP_ADDR_W/_X/_Y/_Z/_4/_5]
        [-> SRC_ADDR_X/_Y/_Z/_4/_5]
        [-> SHIFT_OP_X/_Y/_Z/_4/_5]
        [-> STATIC_RECORD_LAYOUT]
    }

    // Specification: 3.5.104
    block REF_CHARACTERISTIC {
        { ident identifier}* identifier_list
    }

    // Specification: 3.5.105
    block REF_GROUP {
        { ident identifier}* identifier_list
    }

    // Specification: 3.5.106
    block REF_MEASUREMENT {
        { ident identifier}* identifier_list
    }

    // Specification: 3.5.107
    block REF_MEMORY_SEGMENT {
        ident name
    }

    // Specification: 3.5.108
    block REF_UNIT {
        ident unit
    }

    // Specification: 3.5.109
    keyword RESERVED {
        uint position
        DataTypeSize data_size
    }

    // Specification: 3.5.110
    keyword RIGHT_SHIFT {
        ulong bitcount
    }

    // Specification: 3.5.111
    keyword RIP_ADDR_W / _X / _Y / _Z / _4 / _5 {
        uint position
        DataType datatype
    }
    
    // Specification: 3.5.112
    keyword ROOT {}

    // Specification: 3.5.113
    keyword SHIFT_OP_X / _Y / _Z / _4 / _5 {
        uint position
        DataType datatype
    }
    
    // Specification: 3.5.114
    keyword SIGN_EXTEND {}

    // Specification: 3.5.115
    keyword SI_EXPONENTS {
        int length
        int mass
        int time
        int electric_current
        int temperature
        int amount_of_substance
        int luminous_intensity
    }

    // Specification: 3.5.116
    keyword SRC_ADDR_X / _Y / _Z / _4 / _5 {
        uint position
        DataType datatype
    }

    // Specification: 3.5.117
    keyword STATIC_RECORD_LAYOUT {}

    // Specification: 3.5.118
    keyword STATUS_STRING_REF {
        ident conversion_table
    }

    // Specification: 3.5.119
    keyword STEP_SIZE {
        float step_size
    }

    // Specification: 3.5.120
    block SUB_FUNCTION {
        { ident identifier}* identifier_list
    }

    // Specification: 3.5.121
    block SUB_GROUP {
        { ident identifier}* identifier_list
    }

    // Specification: 3.5.122
    keyword SUPPLIER {
        string manufacturer
    }

    // Specification: 3.5.123
    keyword SYMBOL_LINK {
        string symbol_name
        long offset
    }

    // Specification: 3.5.124
    keyword SYSTEM_CONSTANT {
        string name
        string value
    }

    // Specification: 3.5.125
    keyword S_REC_LAYOUT {
        ident name
    }

    // Specification: 3.5.126
    enum UnitType {
        DERIVED,
        EXTENDED_SI
    }

    // Specification: 3.5.126
    block UNIT {
        ident name
        string long_identifier
        string display
        UnitType unit_type
        [-> REF_UNIT]
        [-> SI_EXPONENTS]
        [-> UNIT_CONVERSION]
    }

    // Specification: 3.5.127
    keyword UNIT_CONVERSION {
        float gradient
        float offset
    }

    // Specification: 3.5.128
    keyword USER {
        string user_name
    }

    // Specification: 3.5.129
    block USER_RIGHTS {
        ident user_level_id
        [-> READ_ONLY]
        [-> REF_GROUP]*
    }

    // Specification: 3.5.130
    block VAR_ADDRESS {
        { ulong address}* address_list
    }

    // Specification: 3.5.131
    block VAR_CHARACTERISTIC {
        ident name
        { ident criterion_name }* criterion_name_list
        [-> VAR_ADDRESS]
    }

    // Specification: 3.5.132
    block VAR_CRITERION {
        ident name
        string long_identifier
        {ident  value}* value_list
        [-> VAR_MEASUREMENT]
        [-> VAR_SELECTION_CHARACTERISTIC]
    }

    // Specification: 3.5.133
    block VAR_FORBIDDEN_COMB {
        {
            ident criterion_name
            ident criterion_value
        }* combination
    }

    // Specification: 3.5.134
    keyword VAR_MEASUREMENT {
        ident name
    }

    // Specification: 3.5.135
    enum VarNamingTag {
        NUMERIC
    }

    // Specification: 3.5.135
    keyword VAR_NAMING {
        VarNamingTag tag
    }

    // Specification: 3.5.136
    keyword VAR_SELECTION_CHARACTERISTIC {
        ident name
    }

    // Specification: 3.5.137
    keyword VAR_SEPARATOR {
        string separator
    }

    // Specification: 3.5.138
    block VARIANT_CODING {
        [-> VAR_CHARACTERISTIC]*
        [-> VAR_CRITERION]*
        [-> VAR_FORBIDDEN_COMB]*
        [-> VAR_NAMING]
        [-> VAR_SEPARATOR]
    }

    // Specification: 3.5.139
    keyword VERSION {
        string version_identifier
    }

    // Specification: 3.5.140
    block VIRTUAL {
        { ident measuring_channel }* measuring_channel_list
    }

    // Specification: 3.5.141
    block VIRTUAL_CHARACTERISTIC {
        string formula
        {ident characteristic }* characteristic_list
    }
}


pub trait Logger {
    fn log_message(&mut self, msg: String);
}



fn get_version(parser: &mut ParserState, context: &ParseContext) -> Result<Asap2Version, String> {
    if let Some(token) = parser.peek_token() {
        let ident = parser.get_identifier(context);
        let ver_context = ParseContext::from_token(token, false);
        if let Ok(tag) = ident {
            if tag == "ASAP2_VERSION" {
                let version = Asap2Version::parse(parser, &ver_context);
                if version.is_ok() {
                    parser.set_tokenpos(0);
                    return Ok(version.unwrap());
                }
            }
        }
    }
    Err("File is not recognized as an a2l file. Mandatory version information is missing.".to_string())
}


pub fn load(filename: &str, logger: &mut dyn Logger, strict_parsing: bool) -> Result<A2lFile, String> {
    let filedata = a2lloader::load(filename)?;
    let mut tokenresult = a2ltokenizer::tokenize(String::from(filename), 0, &filedata)?;
    tokenresult.finalize();

    let mut parser = ParserState::new(&tokenresult.tokens, logger, strict_parsing);
    let context = &ParseContext::from_token(&A2lToken {ttype: A2lTokenType::Identifier, text: "A2L_FILE".to_string(), fileid: 0, line: 1}, true);

    let _version = get_version(&mut parser, &context)?;
    let a2lfile = A2lFile::parse(&mut parser, &context);
    if let Err(parse_error) = a2lfile {
        return Err(parser.stringify_parse_error(&parse_error, true));
    }
    let a2lfile = a2lfile.unwrap();

    Ok(a2lfile)
}


pub fn tokenize_ifdata(ifdata: &IfData) -> Vec<A2lToken> {
    let tokenresult = a2ltokenizer::tokenize(ifdata.ifdata_filename.clone(), ifdata.fileid, &ifdata.ifdata_text);
    let mut ifdata_tokens;

    if tokenresult.is_err() {
        ifdata_tokens = Vec::new();
    } else {
        ifdata_tokens = tokenresult.unwrap().tokens;
        let mut lastline = ifdata.line;
        for tok in &mut ifdata_tokens {
            tok.line += ifdata.line - 1;
            lastline = tok.line;
        }
        ifdata_tokens.push(A2lToken {fileid: ifdata.fileid, line: lastline, ttype: A2lTokenType::End, text: "".to_string()});
        ifdata_tokens.push(A2lToken {fileid: ifdata.fileid, line: lastline, ttype: A2lTokenType::Identifier, text: "IF_DATA".to_string()});
    }

    ifdata_tokens
}
