//! a2lmacros is a crate for internal use by the a2lfile crate. It contains the proc macros used by a2lfile.

extern crate proc_macro;
use proc_macro::TokenStream;

pub(crate) mod a2lspec;
pub(crate) mod a2mlspec;
pub(crate) mod codegenerator;
pub(crate) mod util;

#[proc_macro]
pub fn a2l_specification(tokens: TokenStream) -> TokenStream {
    let tokens2: proc_macro2::TokenStream = tokens.into();
    a2lspec::a2l_specification(tokens2).into()
}

/**
The `a2ml_specification`! macro enables application to conveniently decode and use `IF_DATA` defined though A2ML.
The macro uses an "enhanced A2ML" language to define the items.
Rust data structures and associated code will be generated for items defined inside of the macro.

For example a file might have the following definition inside its A2ML block:
```ignore
block "IF_DATA" taggedunion {
    "SOME_DATA" struct {
        uint;
        uint;
    }
}
```
The `a2ml_specification`! can process this form directly, but the struct and its members have no names.
The "enhanced A2ML" form of this would be:

```ignore
block "IF_DATA" taggedunion {
    "SOME_DATA" struct SomeData {
        uint month;
        uint day;
    }
}
```
Finally, the A2ml inside the macro must be preceded by a header that names the content; all together it looks like this:
```ignore
a2ml_specification! {
    <MyA2mlSpec>
    block "IF_DATA" taggedunion {
        "SOME_DATA" struct SomeData {
            uint month; /// a doc comment for month that will be preserved
            uint day; /// a doc comment for day
        };
    };
}
```
The macro will also generate a text constant containing standard (basic) A2ML matching the enhanced definition in the macro; it is called uppercase(macroname)_text.
For the example it is `MYA2MLSPEC_TEXT`.

Now you can load `IfData` from an a2l file:

```ignore
let a2l_file: A2lFile = a2lfile::load(the_filename, Some(MYA2MLSPEC_TEXT.to_string()), &mut logger, false);
let my_data = MyA2mlSpec::load_from_ifdata(&a2l_file.project.modules[0].if_data[0]);
```

*/
#[proc_macro]
pub fn a2ml_specification(tokens: TokenStream) -> TokenStream {
    let tokens2: proc_macro2::TokenStream = tokens.into();
    a2mlspec::a2ml_specification(tokens2).into()
}

#[cfg(test)]
mod test {
    use super::*;
    use quote::quote;

    #[test]
    fn test_a2l_specification() {
        let a2l_spec: proc_macro2::TokenStream = quote! {
        /// Contains all the objects of an A2lfile
        ///
        /// An instance of this struct is returned when an a2l file is loaded successfully
        keyword A2L_FILE {
            [-> ASAP2_VERSION]
            [-> A2ML_VERSION]
            [-> PROJECT]!
        }

        /// Description of the addressing of table values or axis point values.
        ///
        /// Specification: predefined data types
        enum AddrType {
            PBYTE,
            PWORD,
            PLONG,
            PLONGLONG   (1.70 ..),
            DIRECT
        }

        /// Description of the word lengths in the ECU program.
        ///
        /// Specification: predefined data types (datasize)
        enum DataTypeSize {
            BYTE,
            WORD,
            LONG
        }

        /// Description of the basic data types in the ECU program.
        ///
        /// Specification: predefined data types
        enum DataType {
            UBYTE,
            SBYTE,
            UWORD,
            SWORD,
            ULONG,
            SLONG,
            A_UINT64       (1.60 ..),
            A_INT64        (1.60 ..),
            FLOAT16_IEEE   (1.71 ..),
            FLOAT32_IEEE,
            FLOAT64_IEEE
        }

        /// Description of the axis point sequence in the memory.
        ///
        /// Specification: predefined data types
        enum IndexOrder {
            INDEX_INCR,
            INDEX_DECR
        }

        /// Contains AML code for description of interface specific description data.
        ///
        /// Specification: 3.5.2
        block A2ML {
            // the A2ML block gets special treatment in the code generator based on the block name
        }

        /// A2ML_VERSION is currently ignored
        keyword A2ML_VERSION {
            uint version_no
            uint upgrade_no
        }

        /// Address of the EPROM identifier
        keyword ADDR_EPK {
            ulong address
        }


        /// Description of the addressing of table values or axis point values.
        keyword ADDRESS_TYPE {
            AddrType address_type
        }

        /// Defines the alignment of byte-sized values in complex objects (maps and axis)
        keyword ALIGNMENT_BYTE {
            uint alignment_border
        }

        /// Defines the alignment of 16bit floats in complex objects (maps and axis)
        keyword ALIGNMENT_FLOAT16_IEEE {
            uint alignment_border
        }

        /// Defines the alignment of 32bit floats in complex objects (maps and axis)
        keyword ALIGNMENT_FLOAT32_IEEE {
            uint alignment_border
        }

        /// Defines the alignment of 64bit floats in complex objects (maps and axis)
        keyword ALIGNMENT_FLOAT64_IEEE {
            uint alignment_border
        }

        /// Defines the alignment of int64 values in complex objects (maps and axis)
        keyword ALIGNMENT_INT64 {
            uint alignment_border
        }

        /// Defines the alignment of long-sized values in complex objects (maps and axis)
        keyword ALIGNMENT_LONG {
            uint alignment_border
        }

        /// Defines the alignment of word-sized values in complex objects (maps and axis)
        keyword ALIGNMENT_WORD {
            uint alignment_border
        }

        /// An extended description text
        ///
        /// One ANNOTATION may represent a voluminous description. Its purpose is to be e.g.
        /// an application note which explains the function of an identifier for the calibration
        /// engineer.
        block ANNOTATION {
            [-> ANNOTATION_LABEL]
            [-> ANNOTATION_ORIGIN]
            [-> ANNOTATION_TEXT]
        }

        /// The label or title of an annotation
        keyword ANNOTATION_LABEL {
            string label
        }

        /// Identify who or which system has created an annotation
        keyword ANNOTATION_ORIGIN {
            string origin
        }

        /// Text of an annotation
        ///
        /// One ANNOTATION_TEXT may represent a multi-line description text.
        block ANNOTATION_TEXT {
            {string annotation_text}* annotation_text_list
        }

        /// marks a measurement object as an array of <Number> measurement values
        ///
        /// ARRAY_SIZE is obsolete: MATRIX_DIM should be used instead.
        keyword ARRAY_SIZE {
            uint number
        }

        /// describes the Autosar component type of a function
        keyword AR_COMPONENT {
            string component_type
            [-> AR_PROTOTYPE_OF]
        }

        /// Describes the resationship of the component type to to a component prototype in the Autosar system
        keyword AR_PROTOTYPE_OF {
            ident name
        }

        /// Version of the ASAM MCD-2MC standard used by this file
        ///
        /// This keyword is mandatory. Example:
        ///     ASAP2_VERSION 1 61
        keyword ASAP2_VERSION {
            uint version_no
            uint upgrade_no
        }

        /// Description of the axis points
        enum AxisDescrAttribute {
            CURVE_AXIS,
            COM_AXIS,
            FIX_AXIS,
            RES_AXIS,
            STD_AXIS
        }

        /// Axis description within an adjustable object
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
            [-> PHYS_UNIT]    (1.60 ..)
            [-> READ_ONLY]
            [-> STEP_SIZE]    (1.60 ..)
        }

        /// Parameters for the handling of an axis points distribution
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
            [-> MAX_REFRESH]   (1.70 ..)
            [-> MODEL_LINK]    (1.70 ..)
            [-> MONOTONY]
            [-> PHYS_UNIT]     (1.60 ..)
            [-> READ_ONLY]
            [-> REF_MEMORY_SEGMENT]
            [-> STEP_SIZE]     (1.60 ..)
            [-> SYMBOL_LINK]
        }

        /// Reference to an AXIS_PTS record
        keyword AXIS_PTS_REF {
            ident axis_points
        }

        /// Description of the X, Y, Z, Z4 or Z5 axis points in memory
        keyword AXIS_PTS_X / _Y / _Z / _4 / _5 {
            uint position
            DataType datatype
            IndexOrder index_incr
            AddrType addressing
        }

        /// Description of rescaling the axis values of an adjustable object
        keyword AXIS_RESCALE_X /_Y /_Z / _4 / _5 {
            uint position
            DataType datatype
            uint max_number_of_rescale_pairs
            IndexOrder index_incr
            AddrType addressing
        }

        /// The BIT_MASK keyword can be used to mask out single bits of the value to be processed.
        keyword BIT_MASK {
            ulong mask
        }

        /// Used to perform bit operation on a value
        block BIT_OPERATION {
            [-> LEFT_SHIFT]
            [-> RIGHT_SHIFT]
            [-> SIGN_EXTEND]
        }

        /// Special data object that can be used to handle domain specific data, which are processed inside the ECU in a dedicated way
        ///
        /// To the MCD system a blob is just an array of bytes without any interpretation
        block BLOB {
            ident name
            string long_identifier
            ulong start_address
            ulong size
            [-> ADDRESS_TYPE]
            [-> ANNOTATION]*
            [-> CALIBRATION_ACCESS]
            [-> DISPLAY_IDENTIFIER]
            [-> ECU_ADDRESS_EXTENSION]
            [-> IF_DATA]*
            [-> MAX_REFRESH]
            [-> MODEL_LINK]
            [-> SYMBOL_LINK]
        }

        /// Byte ordering of a value on the ECU
        enum ByteOrderEnum {
            LITTLE_ENDIAN        (.. 1.51),
            BIG_ENDIAN           (.. 1.51),
            MSB_LAST,
            MSB_FIRST,
            MSB_FIRST_MSW_LAST   (1.70 ..),
            MSB_LAST_MSW_FIRST   (1.70 ..)
        }

        /// Where the standard value does not apply this parameter can be used to specify the byte order
        ///
        /// Specification: 3.5.24
        keyword BYTE_ORDER {
            ByteOrderEnum byte_order
        }

        /// Type of access that is possible for a CHARACTERISTIC or AXIS_PTS object
        enum CalibrationAccessEnum {
            CALIBRATION,
            NO_CALIBRATION,
            NOT_IN_MCD_SYSTEM,
            OFFLINE_CALIBRATION
        }

        /// Specifies the access of a CHARACTERISTIC or AXIS_PTS for calibration
        keyword CALIBRATION_ACCESS {
            CalibrationAccessEnum calibration_access
        }

        /// calibration method specific data
        block CALIBRATION_HANDLE {
            {long handle}* handle_list
            [-> CALIBRATION_HANDLE_TEXT] (1.60 ..)
        }

        /// Additional text for a calibration handle
        ///
        /// Specification: 3.5.27
        keyword CALIBRATION_HANDLE_TEXT {
            string text
        }

        /// Indicates the different methods of access that are implemented in the ECU
        ///
        /// Valid method strings are: InCircuit, SERAM, DSERAP, BSERAP
        block CALIBRATION_METHOD {
            string method
            ulong version
            [-> CALIBRATION_HANDLE]
        }

        /// Specifies the type of an adjustable object
        enum CharacteristicType {
            ASCII,
            CURVE,
            MAP,
            CUBOID,
            CUBE_4  (1.60 ..),
            CUBE_5  (1.60 ..),
            VAL_BLK,
            VALUE
        }

        /// Specifies all the parameters of an adjustable object
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
            [-> DISCRETE]   (1.60 ..)
            [-> DISPLAY_IDENTIFIER]
            [-> ECU_ADDRESS_EXTENSION]
            [-> ENCODING]   (1.70 ..)
            [-> EXTENDED_LIMITS]
            [-> FORMAT]
            [-> FUNCTION_LIST]
            [-> GUARD_RAILS]
            [-> IF_DATA]*
            [-> MAP_LIST]
            [-> MATRIX_DIM]
            [-> MAX_REFRESH]
            [-> MODEL_LINK]  (1.70 ..)
            [-> NUMBER]      // (.. 1.51) - causes too many deprecation warnings in real files
            [-> PHYS_UNIT]   (1.60 ..)
            [-> READ_ONLY]
            [-> REF_MEMORY_SEGMENT]
            [-> STEP_SIZE]   (1.60 ..)
            [-> SYMBOL_LINK] (1.60 ..)
            [-> VIRTUAL_CHARACTERISTIC]
        }

        /// Specifies the coefficients for the formula f(x) = (axx + bx + c) / (dxx + ex + f)
        keyword COEFFS {
            float a
            float b
            float c
            float d
            float e
            float f
        }

        /// Specifies the coefficients for the linear formula f(x) = ax + b
        keyword COEFFS_LINEAR {
            float a
            float b
        }

        /// references a valid MEASUREMENT
        keyword COMPARISON_QUANTITY {
            ident name
        }

        /// Describes how to convert internal input values to physical values
        enum ConversionType {
            IDENTICAL  (1.60 ..),
            FORM,
            LINEAR  (1.60 ..),
            RAT_FUNC,
            TAB_INTP,
            TAB_NOINTP,
            TAB_VERB
        }

        /// Specification of a conversion method from internal values to physical values
        block COMPU_METHOD {
            ident name
            string long_identifier
            ConversionType conversion_type
            string format
            string unit
            [-> COEFFS]
            [-> COEFFS_LINEAR]        (1.60 ..)
            [-> COMPU_TAB_REF]
            [-> FORMULA]
            [-> REF_UNIT]
            [-> STATUS_STRING_REF]    (1.60 ..)
        }

        /// Conversion table for conversions that cannot be represented as a function
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
            [-> DEFAULT_VALUE_NUMERIC]  (1.60 ..)
        }

        /// reference to a conversion table
        keyword COMPU_TAB_REF {
            ident conversion_table
        }

        /// Conversion table for the assignment of display strings to values. Typically used for enums.
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

        /// Conversion table for the assignment of display strings to a value range
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

        /// indicates that an instance of a structure should always be handled completely
        keyword CONSISTENT_EXCHANGE {}

        /// CONVERSION is used inside OVERWRITE to override the default conversion method
        keyword CONVERSION {
            ident name
        }

        /// Identifies the CPU used in the ECU
        keyword CPU_TYPE {
            string cpu
        }

        /// Used to specify the adjustable CURVE CHARACTERISTIC that is used to normalize or scale the axis in an AXIS_DESCR
        keyword CURVE_AXIS_REF {
            ident curve_axis
        }

        /// Allows a customer name to be specified
        keyword CUSTOMER {
            string customer
        }

        /// specify a customer number or identifier as a string
        keyword CUSTOMER_NO {
            string number
        }

        /// Data size in bits
        keyword DATA_SIZE {
            uint size
        }

        /// Defines which adjustable objects are used by a FUNCTION
        block DEF_CHARACTERISTIC {
            { ident identifier }* identifier_list
        }

        /// Sets the default text value of COMPU_TAB, COMPU_VTAB or COMPU_VTAB_RANGE
        keyword DEFAULT_VALUE {
            string display_string
        }

        /// Sets the default numerical value of COMPU_TAB, COMPU_VTAB or COMPU_VTAB_RANGE
        keyword DEFAULT_VALUE_NUMERIC {
            float display_value
        }

        /// Specify characteristics that depend on a formula
        block DEPENDENT_CHARACTERISTIC {
            string formula
            {ident characteristic}* characteristic_list
        }

        /// Deposit of the axis points of a characteristic curve or map
        enum DepositMode {
            ABSOLUTE,
            DIFFERENCE
        }

        /// Specifies how the axis points of a characteristic are deposited in memory
        keyword DEPOSIT {
            DepositMode mode
        }

        /// Indicates that a measurement or calibration object has discrete values which should not be interpolated
        keyword DISCRETE {}

        /// Gives the display name of a CHARACTERISTIC or MEASUREMENT value
        keyword DISPLAY_IDENTIFIER {
            ident display_name
        }

        /// Description of the distance operand in the deposit structure to compute the axis points for fixed characteristic curves and fixed characteristic maps
        keyword DIST_OP_X / _Y / _Z / _4 / _5 {
            uint position
            DataType datatype
        }

        /// String for identification of the control unit.
        keyword ECU {
            string control_unit
        }

        /// Provides the address of a MEASUREMENT
        keyword ECU_ADDRESS {
            ulong address
        }

        /// Used to specify additional address information
        keyword ECU_ADDRESS_EXTENSION {
            int extension
        }

        /// Provide an address offset in order to handle near pointers or variant coding
        keyword ECU_CALIBRATION_OFFSET {
            long offset
        }

        /// Describes the encoding of a string, if it is not ASCII
        enum CharacterEncoding {
            UTF8,
            UTF16,
            UTF32
        }

        /// a CHARACTERISTIC of type ASCII can be configured to use a multi-byte encoding instead
        keyword ENCODING {
            CharacterEncoding encoding
        }

        /// EPROM identifier
        keyword EPK {
            string identifier
        }

        /// Used to mask bits of a MEASUREMENT which indicate that the value is in error
        keyword ERROR_MASK {
            ulong mask
        }

        /// used to specify an extended range of values
        keyword EXTENDED_LIMITS {
            float lower_limit
            float upper_limit
        }

        /// Parameters for the calculation of fixed axis points: X_i = Offset + (i - 1)*2^shift
        keyword FIX_AXIS_PAR {
            int offset
            int shift
            uint number_apo
        }

        /// Parameters for the calculation of fixed axis points: X_i = Offset + (i - 1)*distance
        keyword FIX_AXIS_PAR_DIST {
            int offset
            int distance
            uint number_apo
        }

        /// A list of fixed axis point, as implemented on the ECU
        block FIX_AXIS_PAR_LIST {
            { float axis_pts_value }* axis_pts_value_list
        }

        /// Specifies the number of axis points available to CURVE, MAP, CUBOID, CUBE_4 or CUBE_5
        keyword FIX_NO_AXIS_PTS_X / _Y / _Z / _4 / _5 {
            uint number_of_axis_points
        }

        /// Describes how the 2-dimensional table values are mapped onto the 1-dimensional address space
        enum IndexMode {
            ALTERNATE_CURVES,
            ALTERNATE_WITH_X,
            ALTERNATE_WITH_Y,
            COLUMN_DIR,
            ROW_DIR
        }

        /// Description of the table values (function values) of an adjustable object
        keyword FNC_VALUES {
            uint position
            DataType datatype
            IndexMode index_mode
            AddrType address_type
        }

        /// Allows a display format string to be specified for a MEASUREMENT, CHARACTERISTIC or AXIS_PTS object
        keyword FORMAT {
            string format_string
        }

        /// Allows any kind of formula to be specified
        block FORMULA {
            string fx
            [-> FORMULA_INV]
        }

        /// Allows an inverse formula to be specified
        keyword FORMULA_INV {
            string gx
        }

        /// Defines a function frame to structure large amounts of measurement objects
        block FRAME {
            ident name
            string long_identifier
            uint scaling_unit
            ulong rate
            [-> FRAME_MEASUREMENT]
            [-> IF_DATA]*
        }

        /// Contains a list of identifiers of measurement objects
        keyword FRAME_MEASUREMENT {
            { ident identifier}* identifier_list
        }

        /// Describes the input, local, and output variables of a function on the ECU
        block FUNCTION {
            ident name
            string long_identifier
            [-> ANNOTATION]*
            [-> AR_COMPONENT]   (1.70 ..)
            [-> DEF_CHARACTERISTIC]
            [-> FUNCTION_VERSION]
            [-> IF_DATA]*    (1.60 ..)
            [-> IN_MEASUREMENT]
            [-> LOC_MEASUREMENT]
            [-> OUT_MEASUREMENT]
            [-> REF_CHARACTERISTIC]
            [-> SUB_FUNCTION]
        }

        /// a list of FUNCTION objects
        block FUNCTION_LIST {
            {ident name}* name_list
        }

        /// A string containing the version of a FUNCTION
        keyword FUNCTION_VERSION {
            string version_identifier
        }

        /// Defines a group of releated CHARACTERISTIC and MEASUREMENT objects
        block GROUP {
            ident name
            string long_identifier
            [-> ANNOTATION]*
            [-> FUNCTION_LIST]
            [-> IF_DATA]*     (1.60 ..)
            [-> REF_CHARACTERISTIC]
            [-> REF_MEASUREMENT]
            [-> ROOT]
            [-> SUB_GROUP]
        }

        /// Used to indicate that an adjustable CURVE, MAP or AXIS_PTS uses guard rails
        keyword GUARD_RAILS {}

        /// The header of a project
        block HEADER {
            string comment
            [-> PROJECT_NO]
            [-> VERSION]
        }

        /// used to describe that an 'identifier' is deposited in a specific position in the adjustable object
        keyword IDENTIFICATION {
            uint position
            DataType datatype
        }

        /// Interface specific data
        block IF_DATA {
            // the A2ML block gets special treatment in the code generator based on the block name
        }

        /// A list of measurement objects that are used as the inputs of a function
        block IN_MEASUREMENT {
            {ident identifier}* identifier_list
        }

        ///INPUT_QUANTITY is used inside OVERWRITE to override the input_quantity of an INSTANCE
        keyword INPUT_QUANTITY {
            ident name
        }

        /// Creates an instance of a type defined using TYPEDEF_STRUCTURE, TYPEDEF_MEASUREMENT or TYPEDEF_CHARACTERISTIC
        block INSTANCE {
            ident name
            string long_identifier
            ident type_ref
            ulong start_address
            [-> ADDRESS_TYPE]  (1.71 ..)
            [-> ANNOTATION]*
            [-> CALIBRATION_ACCESS]
            [-> DISPLAY_IDENTIFIER]
            [-> ECU_ADDRESS_EXTENSION]
            [-> IF_DATA]*
            [-> LAYOUT]
            [-> MATRIX_DIM]
            [-> MAX_REFRESH]
            [-> MODEL_LINK]
            [-> OVERWRITE]*
            [-> READ_ONLY]
            [-> SYMBOL_LINK]
        }

        /// describes the layout of a multi-dimensional measurement array
        keyword LAYOUT {
            IndexMode index_mode
        }

        /// Used within BIT_OPERATION to left-shift the bits of a value
        keyword LEFT_SHIFT {
            ulong bitcount
        }

        /// LIMITS is used inside OVERWRITE to override the limits of an INSTANCE
        keyword LIMITS {
            float lower_limit
            float upper_limit
        }

        /// A list of measurement objects that are local variables of a function
        block LOC_MEASUREMENT {
            {ident identifier}* identifier_list
        }

        /// used to specify the list of MAPs which comprise a CUBOID
        block MAP_LIST {
            {ident name}* name_list
        }

        /// describes the dimensions of a multidimensional array of values
        keyword MATRIX_DIM {
            {uint dim}* dim_list // note: changed for 1.70
        }

        /// specifies a maximum permissible gradient for an adjustable object
        keyword MAX_GRAD {
            float max_gradient
        }

        /// specifies the maximum refresh rate in the control unit
        keyword MAX_REFRESH {
            uint scaling_unit
            ulong rate
        }

        /// describes the parameters for a measurement object
        block MEASUREMENT {
            ident name
            string long_identifier
            DataType datatype
            ident conversion
            uint resolution
            float accuracy
            float lower_limit
            float upper_limit
            [-> ADDRESS_TYPE] (1.70 ..)
            [-> ANNOTATION]*
            [-> ARRAY_SIZE] (.. 1.51)
            [-> BIT_MASK]
            [-> BIT_OPERATION]
            [-> BYTE_ORDER]
            [-> DISCRETE]  (1.60 ..)
            [-> DISPLAY_IDENTIFIER]
            [-> ECU_ADDRESS]
            [-> ECU_ADDRESS_EXTENSION]
            [-> ERROR_MASK]
            [-> FORMAT]
            [-> FUNCTION_LIST]
            [-> IF_DATA]*
            [-> LAYOUT]   (1.60 ..)
            [-> MATRIX_DIM]
            [-> MAX_REFRESH]
            [-> MODEL_LINK] (1.70 ..)
            [-> PHYS_UNIT]   (1.60 ..)
            [-> READ_WRITE]
            [-> REF_MEMORY_SEGMENT]
            [-> SYMBOL_LINK]   (1.60 ..)
            [-> VIRTUAL]
        }

        /// describes the types of program segments
        enum ProgType {
            PRG_CODE,
            PRG_DATA,
            PRG_RESERVED
        }

        /// describes the layout of the ECU memory
        block MEMORY_LAYOUT {
            ProgType prog_type
            ulong address
            ulong size
            long[5] offset
            [-> IF_DATA]*
        }

        /// Describes the types of data in the ECU program
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

        /// describes the type of memory used
        enum MemoryType {
            EEPROM,
            EPROM,
            FLASH,
            RAM,
            ROM,
            REGISTER,
            NOT_IN_ECU   (1.70 ..)
        }

        /// specifies if a given memory region is internal or external
        enum MemoryAttribute {
            INTERN,
            EXTERN
        }

        /// describes a memory segment of the ECU program
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

        /// defines default values for the  entire module
        block MOD_COMMON {
            string comment
            [-> ALIGNMENT_BYTE]
            [-> ALIGNMENT_FLOAT16_IEEE]   (1.71 ..)
            [-> ALIGNMENT_FLOAT32_IEEE]
            [-> ALIGNMENT_FLOAT64_IEEE]
            [-> ALIGNMENT_INT64]    (1.60 ..)
            [-> ALIGNMENT_LONG]
            [-> ALIGNMENT_WORD]
            [-> BYTE_ORDER]
            [-> DATA_SIZE]
            [-> DEPOSIT]
            [-> S_REC_LAYOUT] (.. 1.60) // deprecated in 1.61: RECORD_LAYOUT is always mandatory
        }

        /// defines system information and management data for the module
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

        /// add a string to a CHARACTERISTIC linking it to a name in the model
        keyword MODEL_LINK {
            string model_link
        }

        /// The MODULE keyword describes a complete ECU or device with all adjustable and measurement objects, conversion methods and functions
        ///
        /// At least one module must be defined within the PROJECT
        block MODULE {
            ident name
            string long_identifier
            [-> A2ML]
            [-> AXIS_PTS]*
            [-> BLOB]*                     (1.70 ..)
            [-> CHARACTERISTIC]*
            [-> COMPU_METHOD]*
            [-> COMPU_TAB]*
            [-> COMPU_VTAB]*
            [-> COMPU_VTAB_RANGE]*
            [-> FRAME]*
            [-> FUNCTION]*
            [-> GROUP]*
            [-> IF_DATA]*
            [-> INSTANCE]*                 (1.70 ..)
            [-> MEASUREMENT]*
            [-> MOD_COMMON]
            [-> MOD_PAR]
            [-> RECORD_LAYOUT]*
            [-> TRANSFORMER]*              (1.70 ..)
            [-> TYPEDEF_AXIS]*             (1.70 ..)
            [-> TYPEDEF_BLOB]*             (1.70 ..)
            [-> TYPEDEF_CHARACTERISTIC]*   (1.70 ..)
            [-> TYPEDEF_MEASUREMENT]*      (1.70 ..)
            [-> TYPEDEF_STRUCTURE]*        (1.70 ..)
            [-> UNIT]*
            [-> USER_RIGHTS]*
            [-> VARIANT_CODING]
        }

        /// describes the possible ways an adjustment object can be monotonous
        enum MonotonyType {
            MON_DECREASE,
            MON_INCREASE,
            STRICT_DECREASE,
            STRICT_INCREASE,
            MONOTONOUS    (1.60 ..),
            STRICT_MON    (1.60 ..),
            NOT_MON       (1.60 ..)
        }


        /// specifies the monotony of an adjustment object
        keyword MONOTONY {
            MonotonyType monotony
        }

        /// Description of the number of axis points in an adjustable object
        keyword NO_AXIS_PTS_X / _Y / _Z / _4 / _5 {
            uint position
            DataType datatype
        }

        /// the number of interfaces
        keyword NO_OF_INTERFACES {
            uint num
        }

        /// number of rescaling axis point value pairs
        keyword NO_RESCALE_X / _Y / _Z / _4 / _5 {
            uint position
            DataType datatype
        }

        /// specifies the number of values in an array. Obsolete, replaced by MATRIX_DIM
        keyword NUMBER {
            uint number
        }

        /// Description of the 'offset' parameter in the deposit structure
        keyword OFFSET_X / _Y / _Z / _4 / _5 {
            uint position
            DataType datatype
        }

        /// defines output quantities of a function
        block OUT_MEASUREMENT {
            {ident identifier}* identifier_list
        }

        /// override some default attributes of a type definition in a specific INSTANCE.
        block OVERWRITE {
            ident name
            ulong axis_number
            [-> CONVERSION]
            [-> EXTENDED_LIMITS]
            [-> FORMAT]
            [-> INPUT_QUANTITY]
            [-> LIMITS]
            [-> MONOTONY]
            [-> PHYS_UNIT]
        }

        /// contains a phone number, e.g. of the calibration engineer
        keyword PHONE_NO {
            string telnum
        }

        /// specifies the physical unit of a measurement or calibration object as a string
        keyword PHYS_UNIT {
            string unit
        }

        /// Project description with project header and all modules belonging to the project. Required.
        block PROJECT {
            ident name
            string long_identifier
            [-> HEADER]
            [-> MODULE]+
        }

        /// Gives the project identifier
        keyword PROJECT_NO {
            ident project_number
        }

        /// used to indicate that an adjustable object is read-only
        keyword READ_ONLY {}

        /// used to indicate that a measurement object is writeable
        keyword READ_WRITE {}

        /// specifies the various data structures of an adjustable objects in memory
        block RECORD_LAYOUT {
            ident name
            [-> ALIGNMENT_BYTE]
            [-> ALIGNMENT_FLOAT16_IEEE]  (1.71 ..)
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
            [-> STATIC_RECORD_LAYOUT]    (1.60 ..)
            [-> STATIC_ADDRESS_OFFSETS]  (1.70 ..)
        }

        /// defines a list of adjustable objects that can be referenced by a function or group
        block REF_CHARACTERISTIC {
            { ident identifier}* identifier_list
        }

        /// defines a list of groups for use by USER_RIGHTS
        block REF_GROUP {
            { ident identifier}* identifier_list
        }

        /// defines a list of measurement objects that can be referenced by a group
        block REF_MEASUREMENT {
            { ident identifier}* identifier_list
        }

        /// reference to a MEMORY_SEGMENT
        keyword REF_MEMORY_SEGMENT {
            ident name
        }

        /// reference to a UNIT
        keyword REF_UNIT {
            ident unit
        }

        /// indicates that the data at the given position is reserved and should not be interpreted by the MCD system
        keyword RESERVED {
            uint position
            DataTypeSize data_size
        }

        /// Used within BIT_OPERATION to right-shift the bits of a value
        keyword RIGHT_SHIFT {
            ulong bitcount
        }

        /// Describes the storage of the ECU-internal result of interpolation (RIP)
        keyword RIP_ADDR_W / _X / _Y / _Z / _4 / _5 {
            uint position
            DataType datatype
        }

        /// indicates that the current group is at the root of the navigation tree
        keyword ROOT {}

        /// Description of the shift operand in the deposit structure to compute the axis points for fixed characteristic curves and fixed characteristic maps
        keyword SHIFT_OP_X / _Y / _Z / _4 / _5 {
            uint position
            DataType datatype
        }

        /// used in BIT_OPERATION to specify that sign extension should be performed
        keyword SIGN_EXTEND {}

        /// the seven base dimensions required to define an extended SI unit
        keyword SI_EXPONENTS {
            int length
            int mass
            int time
            int electric_current
            int temperature
            int amount_of_substance
            int luminous_intensity
        }

        /// Description of the address of the input quantity in an adjustable object
        keyword SRC_ADDR_X / _Y / _Z / _4 / _5 {
            uint position
            DataType datatype
        }

        /// indicates that the start addresses of axes and function values of an adjustable object do not change when removing or inserting axis points
        keyword STATIC_ADDRESS_OFFSETS {}

        /// indicates that an adjustable object with dynamic number of axis points does not compact or expand data when removing or inserting axis points
        keyword STATIC_RECORD_LAYOUT {}

        /// used to split up the value range of ECU internal values into a numerical and a verbal part
        keyword STATUS_STRING_REF {
            ident conversion_table
        }

        /// step size when adjusting the value of a CHARACTERISTIC, AXIS_PTS or AXIS_DESCR
        keyword STEP_SIZE {
            float step_size
        }

        /// defines a single component of a TYPEDEF_STRUCTURE
        block STRUCTURE_COMPONENT {
            ident component_name
            ident component_type
            ulong address_offset
            [-> ADDRESS_TYPE]  (1.71 ..)
            [-> LAYOUT]
            [-> MATRIX_DIM]
            [-> SYMBOL_TYPE_LINK]
        }

        /// a list of identifiers of functions which are sub-functions of the current function
        block SUB_FUNCTION {
            { ident identifier}* identifier_list
        }

        /// a list of identifiers of groups which are subgroups of the current group
        block SUB_GROUP {
            { ident identifier}* identifier_list
        }

        /// Name of the ECU manufacturer
        keyword SUPPLIER {
            string manufacturer
        }

        /// specifes the name of a symbol within a linker map file that corresponds to the a2l object
        keyword SYMBOL_LINK {
            string symbol_name
            long offset
        }

        /// Specifies the name of a symbol within a linker map file or debug file that describes a class, class member, structure or structure component
        keyword SYMBOL_TYPE_LINK {
            string symbol_type
        }

        /// defines a system constant that can be used in conversion formulas
        keyword SYSTEM_CONSTANT {
            string name
            string value
        }

        /// sets the standard record layout for the module
        keyword S_REC_LAYOUT {
            ident name
        }

        /// the trigger conditions of a TRANSFORMER
        enum TransformerTrigger {
            ON_USER_REQUEST,
            ON_CHANGE
        }

        /// Definition of call to an external function (32-bit or 64-bit DLL) for converting calibration object values between their implementation format and physical format
        block TRANSFORMER {
            ident name
            string version
            string dllname_32bit
            string dllname_64bit
            uint timeout
            TransformerTrigger trigger
            ident inverse_transformer
            [-> TRANSFORMER_IN_OBJECTS]
            [-> TRANSFORMER_OUT_OBJECTS]
        }

        /// provides a list of inputs for a TRANSFORMER
        block TRANSFORMER_IN_OBJECTS {
            {ident identifier}* identifier_list
        }

        /// provides a list of outputs for a TRANSFORMER
        block TRANSFORMER_OUT_OBJECTS {
            {ident identifier}* identifier_list
        }

        /// Type definition of an axis object
        block TYPEDEF_AXIS {
            ident name
            string long_identifier
            ident input_quantity
            ident record_layout
            float max_diff
            ident conversion
            uint max_axis_points
            float lower_limit
            float upper_limit
            [-> BYTE_ORDER]
            [-> DEPOSIT]
            [-> EXTENDED_LIMITS]
            [-> FORMAT]
            [-> MONOTONY]
            [-> PHYS_UNIT]
            [-> STEP_SIZE]
        }

        /// Type definition of a BLOB
        block TYPEDEF_BLOB {
            ident name
            string long_identifier
            ulong size
            [-> ADDRESS_TYPE]  (1.71 ..)
        }

        /// Type definition of a calibration object
        block TYPEDEF_CHARACTERISTIC {
            ident name
            string long_identifier
            CharacteristicType characteristic_type
            ident record_layout
            float max_diff
            ident conversion
            float lower_limit
            float upper_limit
            [-> AXIS_DESCR]*
            [-> BIT_MASK]
            [-> BYTE_ORDER]
            [-> DISCRETE]
            [-> ENCODING]
            [-> EXTENDED_LIMITS]
            [-> FORMAT]
            [-> MATRIX_DIM]
            [-> NUMBER]
            [-> PHYS_UNIT]
            [-> STEP_SIZE]
        }

        /// Type definition of a measurement object
        block TYPEDEF_MEASUREMENT {
            ident name
            string long_identifier
            DataType datatype
            ident conversion
            uint resolution
            float accuracy
            float lower_limit
            float upper_limit
            [-> ADDRESS_TYPE]
            [-> BIT_MASK]
            [-> BIT_OPERATION]
            [-> BYTE_ORDER]
            [-> DISCRETE]
            [-> ERROR_MASK]
            [-> FORMAT]
            [-> LAYOUT]
            [-> MATRIX_DIM]
            [-> PHYS_UNIT]
        }

        /// Definition of structured data types similar to the typedef command in C
        block TYPEDEF_STRUCTURE {
            ident name
            string long_identifier
            ulong total_size
            [-> ADDRESS_TYPE]
            [-> CONSISTENT_EXCHANGE]
            [-> STRUCTURE_COMPONENT]*
            [-> SYMBOL_TYPE_LINK]
        }

        /// Type of the UNIT
        enum UnitType {
            DERIVED,
            EXTENDED_SI
        }

        /// Specification of a measurement unit
        block UNIT {
            ident name
            string long_identifier
            string display
            UnitType unit_type
            [-> REF_UNIT]
            [-> SI_EXPONENTS]
            [-> UNIT_CONVERSION]
        }

        /// Specification of the linear relationship between two measurement units
        keyword UNIT_CONVERSION {
            float gradient
            float offset
        }

        /// Name of the user
        keyword USER {
            string user_name
        }

        /// used to define groups accessible only for certain users
        block USER_RIGHTS {
            ident user_level_id
            [-> READ_ONLY]
            [-> REF_GROUP]*
        }

        /// define a list of start addresses of variant coded adjustable objects
        block VAR_ADDRESS {
            { ulong address}* address_list
        }

        /// defines one adjustable object to be variant coded
        block VAR_CHARACTERISTIC {
            ident name
            { ident criterion_name }* criterion_name_list
            [-> VAR_ADDRESS]
        }

        /// describes a variant criterion
        block VAR_CRITERION {
            ident name
            string long_identifier
            {ident  value}* value_list
            [-> VAR_MEASUREMENT]
            [-> VAR_SELECTION_CHARACTERISTIC]
        }

        /// describes a forbidden combination of values of different variant criteria
        block VAR_FORBIDDEN_COMB {
            {
                ident criterion_name
                ident criterion_value
            }* combination
        }

        /// specify a special measurement object which indicates the currently active variant
        keyword VAR_MEASUREMENT {
            ident name
        }

        /// intended to define the format of the variant extension. Currently only one format is supported
        enum VarNamingTag {
            NUMERIC
        }

        /// defines the format of the variant extension (index) of adjustable object names
        keyword VAR_NAMING {
            VarNamingTag tag
        }

        /// used to specify a special characteristic object which can change the currently active variant
        keyword VAR_SELECTION_CHARACTERISTIC {
            ident name
        }

        /// defines the separating symbol between the two parts of an adjustable object name
        keyword VAR_SEPARATOR {
            string separator
        }

        /// All information related to variant coding is grouped in this structure
        block VARIANT_CODING {
            [-> VAR_CHARACTERISTIC]*
            [-> VAR_CRITERION]*
            [-> VAR_FORBIDDEN_COMB]*
            [-> VAR_NAMING]
            [-> VAR_SEPARATOR]
        }

        /// version identifier
        keyword VERSION {
            string version_identifier
        }

        /// specification of the measurement objects for a virtual measurement channel
        block VIRTUAL {
            { ident measuring_channel }* measuring_channel_list
        }

        /// defines characteristics that are not deposited in the memory of the control unit, but can be used to indirectly calibrate other characteristic values
        block VIRTUAL_CHARACTERISTIC {
            string formula
            {ident characteristic }* characteristic_list
        }
        };

        // let input: proc_macro2::TokenStream = quote! {
        //     /// Contains all the objects of an A2lfile
        //     ///
        //     /// An instance of this struct is returned when an a2l file is loaded successfully
        //     keyword A2L_FILE {
        //         [-> OPTIONAL_BLOCK]
        //         [-> MULTI_BLOCK]
        //         [-> REQUIRED_BLOCK]!
        //     }

        //     block OPTIONAL_BLOCK {
        //         ident name
        //         string long_identifier
        //         [-> KW1] (..1.50)
        //         [-> KW2] (..1.51)
        //         [-> KW3] (..1.60)
        //         [-> KW4] (..1.61)
        //         [-> KW5] (..1.70)
        //         [-> KW6] (..1.71)
        //         [-> KW7] (1.50..)
        //         [-> KW8] (1.51..)
        //         [-> KW9] (1.60..)
        //         [-> KW10] (1.61..)
        //         [-> KW11] (1.70..)
        //         [-> KW12] (1.71..)
        //     }

        //     block MULTI_BLOCK {

        //     }

        //     block REQUIRED_BLOCK {

        //     }

        //     keyword KW1 {
        //         ident name
        //     }
        //     keyword KW2 {
        //         ident name
        //     }
        //     keyword KW3 {
        //         ident name
        //     }
        //     keyword KW4 {
        //         ident name
        //     }
        //     keyword KW5 {
        //         ident name
        //     }
        //     keyword KW6 {
        //         ident name
        //     }
        //     keyword KW7 {
        //         ident name
        //     }
        //     keyword KW8 {
        //         ident name
        //     }
        //     keyword KW9 {
        //         ident name
        //     }
        //     keyword KW10 {
        //         ident name
        //     }
        //     keyword KW11 {
        //         ident name
        //     }
        //     keyword KW12 {
        //         ident name
        //     }
        // };
        // let _output = a2lspec::a2l_specification(input);
        let _output2 = a2lspec::a2l_specification(a2l_spec);
    }

    #[test]
    fn test_a2ml_specification() {
        let input = quote! {
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
            "ARRAY" int arr[3];
            block "SEQUENCE" (char[256] name)*;
            "NONE";
        };
                };
        let output = a2mlspec::a2ml_specification(input);
        println!("{:#?}", output);
    }
}
