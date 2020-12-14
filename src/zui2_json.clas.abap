*----------------------------------------------------------------------*
*       CLASS ZUI2_JSON DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ZUI2_JSON definition
  public
  create public .

public section.
  type-pools ABAP .
  class CL_ABAP_TSTMP definition load .
  class CX_SY_CONVERSION_ERROR definition load .

  types JSON type STRING .
  types:
    BEGIN OF name_mapping,
            abap  TYPE abap_compname,
            json  TYPE string,
           END OF name_mapping .
  types:
    name_mappings TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY abap .
  types BOOL type CHAR1 .
  types TRIBOOL type CHAR1 .
  types PRETTY_NAME_MODE type CHAR1 .

  constants:
    BEGIN OF pretty_mode,
                  none          TYPE char1  VALUE ``,
                  low_case      TYPE char1  VALUE `L`,
                  camel_case    TYPE char1  VALUE `X`,
                  extended      TYPE char1  VALUE `Y`,
                  user          TYPE char1  VALUE `U`,
                  user_low_case TYPE char1  VALUE `C`,
                END OF  pretty_mode .
  constants:
    BEGIN OF c_bool,
                  true       TYPE bool  VALUE `X`,
                  false      TYPE bool  VALUE ``,
                END OF  c_bool .
  constants:
    BEGIN OF c_tribool,
                  true       TYPE tribool  VALUE c_bool-true,
                  false      TYPE tribool  VALUE `-`,
                  undefined  TYPE tribool  VALUE ``,
                END OF  c_tribool .
  class-data SV_WHITE_SPACE type STRING read-only .
  constants MC_KEY_SEPARATOR type STRING value `-` ##NO_TEXT.
  class-data MC_BOOL_TYPES type STRING read-only value `\TYPE-POOL=ABAP\TYPE=ABAP_BOOL\TYPE=BOOLEAN\TYPE=BOOLE_D\TYPE=XFELD` ##NO_TEXT.
  class-data MC_BOOL_3STATE type STRING read-only value `\TYPE=BOOLEAN` ##NO_TEXT.
  constants VERSION type I value 6 ##NO_TEXT.
  class-data MC_JSON_TYPE type STRING read-only .

  class-methods CLASS_CONSTRUCTOR .
  class-methods STRING_TO_XSTRING
    importing
      !IN type STRING
    changing
      value(OUT) type ANY .
  class-methods XSTRING_TO_STRING
    importing
      !IN type ANY
    returning
      value(OUT) type STRING .
  class-methods RAW_TO_STRING
    importing
      !IV_XSTRING type XSTRING
      !IV_ENCODING type ABAP_ENCODING optional
    returning
      value(RV_STRING) type STRING .
  class-methods STRING_TO_RAW
    importing
      !IV_STRING type STRING
      !IV_ENCODING type ABAP_ENCODING optional
    returning
      value(RV_XSTRING) type XSTRING .
  class-methods DUMP
    importing
      !DATA type DATA
      !COMPRESS type BOOL default C_BOOL-FALSE
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
      !PRETTY_NAME type PRETTY_NAME_MODE default PRETTY_MODE-NONE
      !ASSOC_ARRAYS type BOOL default C_BOOL-FALSE
      !TS_AS_ISO8601 type BOOL default C_BOOL-FALSE
    returning
      value(R_JSON) type JSON .
  class-methods DESERIALIZE
    importing
      !JSON type JSON optional
      !JSONX type XSTRING optional
      !PRETTY_NAME type PRETTY_NAME_MODE default PRETTY_MODE-NONE
      !ASSOC_ARRAYS type BOOL default C_BOOL-FALSE
      !ASSOC_ARRAYS_OPT type BOOL default C_BOOL-FALSE
      !NAME_MAPPINGS type NAME_MAPPINGS optional
    changing
      !DATA type DATA .
  class-methods SERIALIZE
    importing
      !DATA type DATA
      !COMPRESS type BOOL default C_BOOL-FALSE
      !NAME type STRING optional
      !PRETTY_NAME type PRETTY_NAME_MODE default PRETTY_MODE-NONE
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
      !ASSOC_ARRAYS type BOOL default C_BOOL-FALSE
      !TS_AS_ISO8601 type BOOL default C_BOOL-FALSE
      !EXPAND_INCLUDES type BOOL default C_BOOL-TRUE
      !ASSOC_ARRAYS_OPT type BOOL default C_BOOL-FALSE
      !NUMC_AS_STRING type BOOL default C_BOOL-FALSE
      !NAME_MAPPINGS type NAME_MAPPINGS optional
    returning
      value(R_JSON) type JSON .
  methods DESERIALIZE_INT
    importing
      !JSON type JSON optional
      !JSONX type XSTRING optional
    changing
      !DATA type DATA
    raising
      CX_SY_MOVE_CAST_ERROR .
  class-methods GENERATE
    importing
      !JSON type JSON
      !PRETTY_NAME type PRETTY_NAME_MODE default PRETTY_MODE-NONE
      !NAME_MAPPINGS type NAME_MAPPINGS optional
    returning
      value(RR_DATA) type ref to DATA .
  methods SERIALIZE_INT
    importing
      !DATA type DATA
      !NAME type STRING optional
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
    returning
      value(R_JSON) type JSON .
  methods GENERATE_INT
    importing
      !JSON type JSON
    returning
      value(RR_DATA) type ref to DATA .
  methods CONSTRUCTOR
    importing
      !COMPRESS type BOOL default C_BOOL-FALSE
      !PRETTY_NAME type PRETTY_NAME_MODE default PRETTY_MODE-NONE
      !ASSOC_ARRAYS type BOOL default C_BOOL-FALSE
      !TS_AS_ISO8601 type BOOL default C_BOOL-FALSE
      !EXPAND_INCLUDES type BOOL default C_BOOL-TRUE
      !ASSOC_ARRAYS_OPT type BOOL default C_BOOL-FALSE
      !STRICT_MODE type BOOL default C_BOOL-FALSE
      !NUMC_AS_STRING type BOOL default C_BOOL-FALSE
      !NAME_MAPPINGS type NAME_MAPPINGS optional .
  class-methods BOOL_TO_TRIBOOL
    importing
      !IV_BOOL type BOOL
    returning
      value(RV_TRIBOOL) type TRIBOOL .
  class-methods TRIBOOL_TO_BOOL
    importing
      !IV_TRIBOOL type TRIBOOL
    returning
      value(RV_BOOL) type BOOL .
protected section.

  types:
    BEGIN OF t_s_symbol,
        header TYPE string,
        name            TYPE string,
        type   TYPE REF TO cl_abap_datadescr,
        value           TYPE REF TO data,
        compressable    TYPE abap_bool,
        read_only       TYPE abap_bool,
      END OF t_s_symbol .
  types:
    t_t_symbol TYPE STANDARD TABLE OF t_s_symbol WITH DEFAULT KEY .
  types:
    BEGIN OF t_s_field_cache,
        name  TYPE string,
        type  TYPE REF TO cl_abap_datadescr,
        value TYPE REF TO data,
      END OF t_s_field_cache .
  types:
    t_t_field_cache TYPE HASHED TABLE OF t_s_field_cache WITH UNIQUE KEY name .
  types:
    name_mappings_ex TYPE HASHED TABLE OF name_mapping WITH UNIQUE KEY json .

  data MV_COMPRESS type BOOL .
  data MV_PRETTY_NAME type PRETTY_NAME_MODE .
  data MV_ASSOC_ARRAYS type BOOL .
  data MV_TS_AS_ISO8601 type BOOL .
  data MT_NAME_MAPPINGS type NAME_MAPPINGS .
  data MT_NAME_MAPPINGS_EX type NAME_MAPPINGS_EX .
  data MV_EXPAND_INCLUDES type BOOL .
  data MV_ASSOC_ARRAYS_OPT type BOOL .
  data MV_STRICT_MODE type BOOL .
  data MV_NUMC_AS_STRING type BOOL .

  methods GET_SYMBOLS
  final
    importing
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !DATA type ref to DATA optional
      !OBJECT type ref to OBJECT optional
      !INCLUDE_ALIASES type ABAP_BOOL default ABAP_FALSE
    returning
      value(RESULT) type T_T_SYMBOL .
  methods DUMP_SYMBOLS
  final
    importing
      !IT_SYMBOLS type T_T_SYMBOL
    returning
      value(R_JSON) type JSON .
  methods GET_FIELDS
  final
    importing
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !DATA type ref to DATA optional
      !OBJECT type ref to OBJECT optional
    returning
      value(RT_FIELDS) type T_T_FIELD_CACHE .
  methods DUMP_INT
    importing
      !DATA type DATA
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR optional
    returning
      value(R_JSON) type JSON .
  methods IS_COMPRESSABLE
    importing
      !TYPE_DESCR type ref to CL_ABAP_TYPEDESCR
      !NAME type CSEQUENCE
    returning
      value(RV_COMPRESS) type ABAP_BOOL .
  methods RESTORE
    importing
      !JSON type JSON
      !LENGTH type I
      value(TYPE_DESCR) type ref to CL_ABAP_TYPEDESCR optional
      !FIELD_CACHE type T_T_FIELD_CACHE optional
    changing
      !DATA type DATA optional
      !OFFSET type I default 0
    raising
      CX_SY_MOVE_CAST_ERROR .
  methods RESTORE_TYPE
    importing
      !JSON type JSON
      !LENGTH type I
      value(TYPE_DESCR) type ref to CL_ABAP_TYPEDESCR optional
      !FIELD_CACHE type T_T_FIELD_CACHE optional
    changing
      !DATA type DATA optional
      !OFFSET type I default 0
    raising
      CX_SY_MOVE_CAST_ERROR .
  methods DUMP_TYPE
    importing
      !DATA type DATA
      !TYPE_DESCR type ref to CL_ABAP_ELEMDESCR
    returning
      value(R_JSON) type JSON .
  methods DUMP_TYPE_EX
    importing
      !DATA type DATA
    returning
      value(R_JSON) type JSON .
  methods PRETTY_NAME_EX
    importing
      !IN type CSEQUENCE
    returning
      value(OUT) type STRING .
  methods GENERATE_INT_EX
  final
    importing
      !JSON type JSON
      !LENGTH type I
    changing
      !DATA type DATA
      !OFFSET type I .
  methods PRETTY_NAME
    importing
      !IN type CSEQUENCE
    returning
      value(OUT) type STRING .
  class-methods ESCAPE
    importing
      !IN type ANY
    returning
      value(OUT) type STRING .
  class-methods EDM_DATETIME_TO_TS
    importing
      !TICKS type STRING
      !OFFSET type STRING optional
      !TYPEKIND type ABAP_TYPEKIND
    returning
      value(R_DATA) type STRING .
private section.

  data MV_EXTENDED type BOOL .
  data: NEW_VARIABLE type CHAR128.
  class-data MC_ME_TYPE type STRING .
*"* private components of class ZUI2_JSON
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZUI2_JSON IMPLEMENTATION.


METHOD BOOL_TO_TRIBOOL.
  IF iv_bool EQ c_bool-true.
    rv_tribool = c_tribool-true.
  ELSEIF iv_bool EQ abap_undefined. " fall back for abap _bool
    rv_tribool = c_tribool-undefined.
  ELSE.
    rv_tribool = c_tribool-false.
  ENDIF.
ENDMETHOD.                    "bool_to_tribool


METHOD CLASS_CONSTRUCTOR.

  DATA: lo_bool_type_descr    TYPE REF TO cl_abap_typedescr,
        lo_tribool_type_descr TYPE REF TO cl_abap_typedescr,
        lo_json_type_descr    TYPE REF TO cl_abap_typedescr,
        lv_pos                LIKE sy-fdpos,
        lv_json_string        TYPE json.

  lo_bool_type_descr    = cl_abap_typedescr=>describe_by_data( c_bool-true ).
  lo_tribool_type_descr = cl_abap_typedescr=>describe_by_data( c_tribool-true ).
  lo_json_type_descr    = cl_abap_typedescr=>describe_by_data( lv_json_string ).

  CONCATENATE mc_bool_types lo_bool_type_descr->absolute_name lo_tribool_type_descr->absolute_name INTO mc_bool_types.
  CONCATENATE mc_bool_3state lo_tribool_type_descr->absolute_name INTO mc_bool_3state.
  CONCATENATE mc_json_type lo_json_type_descr->absolute_name INTO mc_json_type.

  FIND FIRST OCCURRENCE OF `\TYPE=` IN lo_json_type_descr->absolute_name MATCH OFFSET lv_pos.
  IF sy-subrc IS INITIAL.
    mc_me_type = lo_json_type_descr->absolute_name(lv_pos).
  ENDIF.

  sv_white_space = cl_abap_char_utilities=>get_simple_spaces_for_cur_cp( ).

ENDMETHOD.                    "class_constructor


METHOD CONSTRUCTOR.

  DATA: rtti TYPE REF TO cl_abap_classdescr,
        pair LIKE LINE OF name_mappings.

  mv_compress         = compress.
  mv_pretty_name    = pretty_name.
  mv_assoc_arrays   = assoc_arrays.
  mv_ts_as_iso8601  = ts_as_iso8601.
  mv_expand_includes  = expand_includes.
  mv_assoc_arrays_opt = assoc_arrays_opt.
  mv_strict_mode      = strict_mode.
  mv_numc_as_string   = numc_as_string.

  LOOP AT name_mappings INTO pair.
    TRANSLATE pair-abap TO UPPER CASE.
    INSERT pair INTO TABLE mt_name_mappings.
  ENDLOOP.

  INSERT LINES OF mt_name_mappings INTO TABLE mt_name_mappings_ex.

  IF mt_name_mappings IS NOT INITIAL.
    IF mv_pretty_name EQ pretty_mode-none.
      mv_pretty_name = pretty_mode-user.
    ELSEIF pretty_name EQ pretty_mode-low_case.
      mv_pretty_name = pretty_mode-user_low_case.
    ENDIF.
  ENDIF.

  rtti ?= cl_abap_classdescr=>describe_by_object_ref( me ).
  IF rtti->absolute_name NE mc_me_type.
    mv_extended = c_bool-true.
  ENDIF.

ENDMETHOD.


METHOD DESERIALIZE.

  DATA: lo_json TYPE REF TO ZUI2_JSON.

  " **********************************************************************
  "! Usage examples and documentation can be found on SCN:
  " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
  " **********************************************************************  "

  IF json IS NOT INITIAL OR jsonx IS NOT INITIAL.

    CREATE OBJECT lo_json
      EXPORTING
        pretty_name  = pretty_name
        name_mappings    = name_mappings
        assoc_arrays     = assoc_arrays
        assoc_arrays_opt = assoc_arrays_opt.

    TRY .
        lo_json->deserialize_int( EXPORTING json = json jsonx = jsonx CHANGING data = data ).
      CATCH cx_sy_move_cast_error.
    ENDTRY.

  ENDIF.

ENDMETHOD.                    "deserialize


METHOD DESERIALIZE_INT.

  DATA: length    TYPE i,
        unescaped LIKE json.

  " **********************************************************************
  "! Usage examples and documentation can be found on SCN:
  " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
  " **********************************************************************  "

  IF json IS NOT INITIAL OR jsonx IS NOT INITIAL.

    IF jsonx IS NOT INITIAL.
      unescaped = raw_to_string( jsonx ).
    ELSE.
      unescaped = json.
    ENDIF.

    " to eliminate numeric replacement calls for every single sting value, we do
    " replacement over all JSON text, while this shall not destroy JSON structure
    REPLACE ALL OCCURRENCES OF `\r\n` IN unescaped WITH cl_abap_char_utilities=>cr_lf.
    REPLACE ALL OCCURRENCES OF `\n`   IN unescaped WITH cl_abap_char_utilities=>newline.
    REPLACE ALL OCCURRENCES OF `\t`   IN unescaped WITH cl_abap_char_utilities=>horizontal_tab.
    " REPLACE ALL OCCURRENCES OF `\f`   IN r_json WITH cl_abap_char_utilities=>form_feed.
    " REPLACE ALL OCCURRENCES OF `\b`   IN r_json WITH cl_abap_char_utilities=>backspace.

    length = NUMOFCHAR( unescaped ).
    restore_type( EXPORTING json = unescaped length = length CHANGING data = data ).

  ENDIF.

ENDMETHOD.                    "deserialize


METHOD DUMP.

  DATA: lo_json TYPE REF TO ZUI2_JSON.

  CREATE OBJECT lo_json
    EXPORTING
      compress      = compress
      pretty_name   = pretty_name
      assoc_arrays  = assoc_arrays
      ts_as_iso8601 = ts_as_iso8601.

  r_json = lo_json->dump_int( data = data type_descr = type_descr ).

ENDMETHOD.                    "dump


METHOD DUMP_INT.

  DATA: lo_typedesc        TYPE REF TO cl_abap_typedescr,
        lo_elem_descr      TYPE REF TO cl_abap_elemdescr,
        lo_classdesc       TYPE REF TO cl_abap_classdescr,
        lo_structdesc      TYPE REF TO cl_abap_structdescr,
        lo_tabledescr      TYPE REF TO cl_abap_tabledescr,
        lt_symbols         TYPE t_t_symbol,
        lt_keys            LIKE lt_symbols,
        lt_properties      TYPE STANDARD TABLE OF string,
        lt_fields          TYPE STANDARD TABLE OF string,
        lo_obj_ref         TYPE REF TO object,
        lo_data_ref        TYPE REF TO data,
        ls_skip_key        TYPE LINE OF abap_keydescr_tab,
        lv_array_opt       TYPE abap_bool,
        lv_prop_name       TYPE string,
        lv_keyval          TYPE string,
        lv_itemval         TYPE string.

  FIELD-SYMBOLS: <line>    TYPE ANY,
                 <value>   TYPE ANY,
                 <data>    TYPE data,
                 <key>     TYPE LINE OF abap_keydescr_tab,
                 <symbol>  LIKE LINE OF lt_symbols,
                 <table>   TYPE ANY TABLE.

  " we need here macro instead of method calls because of the performance reasons.
  " based on SAT measurements.

  CASE type_descr->kind.
    WHEN cl_abap_typedescr=>kind_ref.

      IF data IS INITIAL.
        r_json = `null`.                                    "#EC NOTEXT
      ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
        lo_data_ref ?= data.
        lo_typedesc = cl_abap_typedescr=>describe_by_data_ref( lo_data_ref ).
        ASSIGN lo_data_ref->* TO <data>.
        r_json = dump_int( data = <data> type_descr = lo_typedesc ).
      ELSE.
        lo_obj_ref ?= data.
        lo_classdesc ?= cl_abap_typedescr=>describe_by_object_ref( lo_obj_ref ).
        lt_symbols = get_symbols( type_descr = lo_classdesc object = lo_obj_ref ).
        r_json = dump_symbols( lt_symbols ).
      ENDIF.

    WHEN cl_abap_typedescr=>kind_elem.
      lo_elem_descr ?= type_descr.
      dump_type data lo_elem_descr r_json.

    WHEN cl_abap_typedescr=>kind_struct.

      lo_structdesc ?= type_descr.
      GET REFERENCE OF data INTO lo_data_ref.
      lt_symbols = get_symbols( type_descr = lo_structdesc data = lo_data_ref ).
      r_json = dump_symbols( lt_symbols ).

    WHEN cl_abap_typedescr=>kind_table.

      lo_tabledescr ?= type_descr.
      lo_typedesc = lo_tabledescr->get_table_line_type( ).

      ASSIGN data TO <table>.

      " optimization for structured tables
      IF lo_typedesc->kind EQ cl_abap_typedescr=>kind_struct.
        lo_structdesc ?= lo_typedesc.
        CREATE DATA lo_data_ref LIKE LINE OF <table>.
        ASSIGN lo_data_ref->* TO <line>.
        lt_symbols = get_symbols( type_descr = lo_structdesc data = lo_data_ref ).

        " here we have differentiation of output of simple table to JSON array
        " and sorted or hashed table with unique key into JSON associative array
        IF lo_tabledescr->has_unique_key IS NOT INITIAL AND mv_assoc_arrays IS NOT INITIAL.

          IF lo_tabledescr->key_defkind EQ lo_tabledescr->keydefkind_user.
            LOOP AT lo_tabledescr->key ASSIGNING <key>.
              READ TABLE lt_symbols WITH KEY name = <key>-name ASSIGNING <symbol>.
              APPEND <symbol> TO lt_keys.
            ENDLOOP.
          ENDIF.

          IF LINES( lo_tabledescr->key ) EQ 1.
            READ TABLE lo_tabledescr->key INDEX 1 INTO ls_skip_key.
            DELETE lt_symbols WHERE name EQ ls_skip_key-name.
            " remove object wrapping for simple name-value tables
            IF mv_assoc_arrays_opt EQ abap_true AND LINES( lt_symbols ) EQ 1.
              lv_array_opt = abap_true.
            ENDIF.
          ENDIF.

          LOOP AT <table> INTO <line>.
            CLEAR: lt_fields, lv_prop_name.
            LOOP AT lt_symbols ASSIGNING <symbol>.
              ASSIGN <symbol>-value->* TO <value>.
              IF mv_compress IS INITIAL OR <value> IS NOT INITIAL OR <symbol>-compressable EQ abap_false.
                IF <symbol>-type->kind EQ cl_abap_typedescr=>kind_elem.
                  lo_elem_descr ?= <symbol>-type.
                  dump_type <value> lo_elem_descr lv_itemval.
                ELSE.
                  lv_itemval = dump_int( data = <value> type_descr = <symbol>-type ).
                ENDIF.
                IF lv_array_opt EQ abap_false.
                  CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
                ENDIF.
                APPEND lv_itemval TO lt_fields.
              ENDIF.
            ENDLOOP.

            IF lo_tabledescr->key_defkind EQ lo_tabledescr->keydefkind_user.
              LOOP AT lt_keys ASSIGNING <symbol>.
                ASSIGN <symbol>-value->* TO <value>.
                MOVE <value> TO lv_keyval.
                CONDENSE lv_keyval.
                IF lv_prop_name IS NOT INITIAL.
                  CONCATENATE lv_prop_name mc_key_separator lv_keyval INTO lv_prop_name.
                ELSE.
                  lv_prop_name = lv_keyval.
                ENDIF.
              ENDLOOP.
            ELSE.
              LOOP AT lt_symbols ASSIGNING <symbol>.
                ASSIGN <symbol>-value->* TO <value>.
                MOVE <value> TO lv_keyval.
                CONDENSE lv_keyval.
                IF lv_prop_name IS NOT INITIAL.
                  CONCATENATE lv_prop_name mc_key_separator lv_keyval INTO lv_prop_name.
                ELSE.
                  lv_prop_name = lv_keyval.
                ENDIF.
              ENDLOOP.
            ENDIF.

            CONCATENATE LINES OF lt_fields INTO lv_itemval SEPARATED BY `,`.
            IF lv_array_opt EQ abap_false.
              CONCATENATE `"` lv_prop_name `":{` lv_itemval `}` INTO lv_itemval.
            ELSE.
              CONCATENATE `"` lv_prop_name `":` lv_itemval `` INTO lv_itemval.
            ENDIF.
            APPEND lv_itemval TO lt_properties.

          ENDLOOP.

          CONCATENATE LINES OF lt_properties INTO r_json SEPARATED BY `,`.
          CONCATENATE `{` r_json `}` INTO r_json.

        ELSE.

          LOOP AT <table> INTO <line>.
            CLEAR lt_fields.
            LOOP AT lt_symbols ASSIGNING <symbol>.
              ASSIGN <symbol>-value->* TO <value>.
              IF mv_compress IS INITIAL OR <value> IS NOT INITIAL OR <symbol>-compressable EQ abap_false.
                IF <symbol>-type->kind EQ cl_abap_typedescr=>kind_elem.
                  lo_elem_descr ?= <symbol>-type.
                  dump_type <value> lo_elem_descr lv_itemval.
                ELSE.
                  lv_itemval = dump_int( data = <value> type_descr = <symbol>-type ).
                ENDIF.
                CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
                APPEND lv_itemval TO lt_fields.
              ENDIF.
            ENDLOOP.

            CONCATENATE LINES OF lt_fields INTO lv_itemval SEPARATED BY `,`.
            CONCATENATE `{` lv_itemval `}` INTO lv_itemval.
            APPEND lv_itemval TO lt_properties.
          ENDLOOP.

          CONCATENATE LINES OF lt_properties INTO r_json SEPARATED BY `,`.
          CONCATENATE `[` r_json `]` INTO r_json.

        ENDIF.
      ELSE.
        LOOP AT <table> ASSIGNING <value>.
          lv_itemval = dump_int( data = <value> type_descr = lo_typedesc ).
          APPEND lv_itemval TO lt_properties.
        ENDLOOP.

        CONCATENATE LINES OF lt_properties INTO r_json SEPARATED BY `,`.
        CONCATENATE `[` r_json `]` INTO r_json.
      ENDIF.

  ENDCASE.

ENDMETHOD.                    "dump


METHOD DUMP_SYMBOLS.

  DATA: lv_properties      TYPE STANDARD TABLE OF string,
        lv_itemval         TYPE string.

  FIELD-SYMBOLS: <value>   TYPE ANY,
                 <symbol>  LIKE LINE OF it_symbols.

  LOOP AT it_symbols ASSIGNING <symbol>.
    ASSIGN <symbol>-value->* TO <value>.
    IF mv_compress IS INITIAL OR <value> IS NOT INITIAL OR <symbol>-compressable EQ abap_false.
      lv_itemval = dump_int( data = <value> type_descr = <symbol>-type ).
      CONCATENATE <symbol>-header lv_itemval INTO lv_itemval.
      APPEND lv_itemval TO lv_properties.
    ENDIF.
  ENDLOOP.

  CONCATENATE LINES OF lv_properties INTO r_json SEPARATED BY `,`.
  CONCATENATE `{` r_json `}` INTO r_json.

ENDMETHOD.


METHOD DUMP_TYPE.

  CASE type_descr->type_kind.
    WHEN cl_abap_typedescr=>typekind_float OR cl_abap_typedescr=>typekind_int OR cl_abap_typedescr=>typekind_int1 OR
         cl_abap_typedescr=>typekind_int2 OR cl_abap_typedescr=>typekind_packed OR `8`. " TYPEKIND_INT8 -> '8' only from 7.40

      IF type_descr->type_kind EQ cl_abap_typedescr=>typekind_packed AND mv_ts_as_iso8601 EQ c_bool-true AND type_descr->absolute_name CP `\TYPE=TIMESTAMP*`.
        IF data IS INITIAL.
          r_json = `""`.
        ELSE.
          MOVE data TO r_json.
          IF type_descr->absolute_name EQ `\TYPE=TIMESTAMP`.
            CONCATENATE `"` r_json(4) `-` r_json+4(2) `-` r_json+6(2) `T` r_json+8(2) `:` r_json+10(2) `:` r_json+12(2) `.0000000Z"`  INTO r_json.
          ELSEIF type_descr->absolute_name EQ `\TYPE=TIMESTAMPL`.
            CONCATENATE `"` r_json(4) `-` r_json+4(2) `-` r_json+6(2) `T` r_json+8(2) `:` r_json+10(2) `:` r_json+12(2) `.` r_json+15(7) `Z"`  INTO r_json.
          ENDIF.
        ENDIF.
      ELSEIF data IS INITIAL.
        r_json = `0`.
      ELSE.
        MOVE data TO r_json.
        IF data LT 0.
          IF type_descr->type_kind <> cl_abap_typedescr=>typekind_float. "float: sign is already at the beginning
            SHIFT r_json RIGHT CIRCULAR.
          ENDIF.
        ELSE.
          CONDENSE r_json.
        ENDIF.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_num.
      IF mv_numc_as_string EQ abap_true.
        IF data IS INITIAL.
          r_json = `""`.
        ELSE.
          CONCATENATE `"` data `"` INTO r_json.
        ENDIF.
      ELSE.
        IF data IS INITIAL.
        r_json = `0`.
      ELSE.
        MOVE data TO r_json.
        SHIFT r_json LEFT DELETING LEADING ` 0`.
        ENDIF.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_string OR cl_abap_typedescr=>typekind_csequence OR cl_abap_typedescr=>typekind_clike.
      IF data IS INITIAL.
        r_json = `""`.
      ELSEIF type_descr->absolute_name EQ mc_json_type.
        r_json = data.
      ELSE.
        r_json = escape( data ).
        CONCATENATE `"` r_json `"` INTO r_json.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
      IF data IS INITIAL.
        r_json = `""`.
      ELSE.
        r_json = xstring_to_string( data ).
        r_json = escape( r_json ).
        CONCATENATE `"` r_json `"` INTO r_json.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_char.
      IF type_descr->output_length EQ 1 AND mc_bool_types CS type_descr->absolute_name.
        IF data EQ c_bool-true.
          r_json = `true`.                                "#EC NOTEXT
        ELSEIF mc_bool_3state CS type_descr->absolute_name AND data IS INITIAL.
          r_json = `null`.                                "#EC NOTEXT
        ELSE.
          r_json = `false`.                               "#EC NOTEXT
        ENDIF.
      ELSE.
        r_json = escape( data ).
        CONCATENATE `"` r_json `"` INTO r_json.
      ENDIF.
    WHEN cl_abap_typedescr=>typekind_date.
      CONCATENATE `"` data(4) `-` data+4(2) `-` data+6(2) `"` INTO r_json.
    WHEN cl_abap_typedescr=>typekind_time.
      CONCATENATE `"` data(2) `:` data+2(2) `:` data+4(2) `"` INTO r_json.
    WHEN OTHERS.
      IF data IS INITIAL.
        r_json = `null`.                                  "#EC NOTEXT
      ELSE.
        MOVE data TO r_json.
      ENDIF.
  ENDCASE.

ENDMETHOD.                    "dump_type


METHOD DUMP_TYPE_EX.

  DATA: lo_descr TYPE REF TO cl_abap_elemdescr.
  lo_descr ?= cl_abap_typedescr=>describe_by_data( data ).
  r_json = dump_type( data = data type_descr = lo_descr ).

ENDMETHOD.                    "DUMP_TYPE_EX


METHOD EDM_DATETIME_TO_TS.

  DATA: lv_ticks     TYPE p,
        lv_timestamp TYPE timestamp VALUE `19700101000000`.

  lv_ticks     = ticks.
  lv_ticks     = lv_ticks / 1000. " in seconds
  lv_timestamp = cl_abap_tstmp=>add( tstmp = lv_timestamp secs = lv_ticks ).

  IF offset IS NOT INITIAL.
    lv_ticks = offset+1.
    lv_ticks = lv_ticks * 60. "offset is in minutes
    IF offset(1) = '+'.
      lv_timestamp = cl_abap_tstmp=>subtractsecs( tstmp = lv_timestamp secs = lv_ticks ).
    ELSE.
      lv_timestamp = cl_abap_tstmp=>add( tstmp = lv_timestamp secs = lv_ticks ).
    ENDIF.
  ENDIF.

  CASE typekind.
    WHEN cl_abap_typedescr=>typekind_time.
      r_data = lv_timestamp.
      r_data = r_data+8(6).
    WHEN cl_abap_typedescr=>typekind_date.
      r_data = lv_timestamp.
      r_data = r_data(8).
    WHEN cl_abap_typedescr=>typekind_packed.
      r_data = lv_timestamp.
  ENDCASE.

ENDMETHOD.


METHOD ESCAPE.

  MOVE in TO out.

  REPLACE ALL OCCURRENCES OF `\` IN out WITH `\\`.
  REPLACE ALL OCCURRENCES OF `"` IN out WITH `\"`.

ENDMETHOD.                    "escape


METHOD GENERATE.

  DATA: lo_json   TYPE REF TO ZUI2_JSON,
        lv_json   LIKE json.

  lv_json = json.

  REPLACE ALL OCCURRENCES OF `\r\n` IN lv_json WITH cl_abap_char_utilities=>cr_lf.
  REPLACE ALL OCCURRENCES OF `\n`   IN lv_json WITH cl_abap_char_utilities=>newline.
  REPLACE ALL OCCURRENCES OF `\t`   IN lv_json WITH cl_abap_char_utilities=>horizontal_tab.

  CREATE OBJECT lo_json
    EXPORTING
      pretty_name      = pretty_name
      name_mappings    = name_mappings
      assoc_arrays     = c_bool-true
      assoc_arrays_opt = c_bool-true.

  TRY .
      rr_data = lo_json->generate_int( lv_json ).
    CATCH cx_sy_move_cast_error.
  ENDTRY.

ENDMETHOD.


METHOD generate_int.

  TYPES: BEGIN OF ts_field,
           name  TYPE string,
           value TYPE json,
         END OF ts_field.

  DATA: length TYPE i,
        offset TYPE i.

  DATA: lt_json      TYPE STANDARD TABLE OF json WITH DEFAULT KEY,
        lv_json      LIKE LINE OF lt_json,
        lv_comp_name TYPE abap_compname,
        lt_fields    TYPE HASHED TABLE OF ts_field WITH UNIQUE KEY name,
        lo_type      TYPE REF TO cl_abap_datadescr,
        lt_comp      TYPE abap_component_tab,
        lt_names     TYPE HASHED TABLE OF string WITH UNIQUE KEY table_line,
        cache        LIKE LINE OF mt_name_mappings_ex,
        ls_comp      LIKE LINE OF lt_comp.

  FIELD-SYMBOLS: <data>   TYPE any,
                 <struct> TYPE any,
                 <field>  LIKE LINE OF lt_fields,
                 <table>  TYPE STANDARD TABLE,
                 <cache>  LIKE LINE OF mt_name_mappings_ex.

  length = numofchar( json ).

  eat_white.

  CASE json+offset(1).
    WHEN `{`."result must be a structure
      restore_type( EXPORTING json = json length = length CHANGING  data = lt_fields ).
      IF lt_fields IS NOT INITIAL.
        ls_comp-type = cl_abap_refdescr=>get_ref_to_data( ).
        LOOP AT lt_fields ASSIGNING <field>.
          READ TABLE mt_name_mappings_ex WITH TABLE KEY json = <field>-name ASSIGNING <cache>.
          IF sy-subrc IS INITIAL.
            ls_comp-name = <cache>-abap.
          ELSE.
            cache-json = ls_comp-name = <field>-name.
            TRANSLATE ls_comp-name USING `/_:_~_._-_`. " remove characters not allowed in component names
            IF mv_pretty_name EQ pretty_mode-camel_case OR mv_pretty_name EQ pretty_mode-extended.
              REPLACE ALL OCCURRENCES OF REGEX `([a-z])([A-Z])` IN ls_comp-name WITH `$1_$2`. "#EC NOTEXT
            ENDIF.
            TRANSLATE ls_comp-name TO UPPER CASE.
            cache-abap = ls_comp-name = lv_comp_name = ls_comp-name. " truncate by allowed field name length
            INSERT cache INTO TABLE mt_name_mappings_ex.
          ENDIF.
          INSERT ls_comp-name INTO TABLE lt_names.
          IF sy-subrc IS INITIAL.
            APPEND ls_comp TO lt_comp.
          ELSE.
            DELETE lt_fields WHERE name = <field>-name.
          ENDIF.
        ENDLOOP.
        TRY.
            lo_type = cl_abap_structdescr=>create( p_components = lt_comp p_strict = c_bool-false ).
            CREATE DATA rr_data TYPE HANDLE lo_type.
            ASSIGN rr_data->* TO <struct>.
            DATA: l_index TYPE i.
            l_index = 0.
            LOOP AT lt_fields ASSIGNING <field>.
              l_index = l_index + 1.
              ASSIGN COMPONENT l_index OF STRUCTURE <struct> TO <data>.
              <data> = generate_int( <field>-value ).
            ENDLOOP.
          CATCH cx_sy_create_data_error cx_sy_struct_creation.
        ENDTRY.
      ENDIF.
    WHEN `[`."result must be a table of ref
      restore_type( EXPORTING json = json length = length CHANGING  data = lt_json ).
      CREATE DATA rr_data TYPE TABLE OF REF TO data.
      ASSIGN rr_data->* TO <table>.
      LOOP AT lt_json INTO lv_json.
        APPEND INITIAL LINE TO <table> ASSIGNING <data>.
        <data> = generate_int( lv_json ).
      ENDLOOP.
    WHEN OTHERS.
      IF json+offset(1) EQ `"`.
        CREATE DATA rr_data TYPE string.
      ELSEIF json+offset(1) CA `-0123456789.`.
        IF json+offset CS '.'.
          CREATE DATA rr_data TYPE f.
        ELSEIF length GT 9.
          CREATE DATA rr_data TYPE p LENGTH 16.
        ELSE.
          CREATE DATA rr_data TYPE i.
        ENDIF.
      ELSEIF json+offset EQ `true` OR json+offset EQ `false`.
        CREATE DATA rr_data TYPE abap_bool.
      ENDIF.
      IF rr_data IS BOUND.
        ASSIGN rr_data->* TO <data>.
        restore_type( EXPORTING json = json length = length CHANGING  data = <data> ).
      ENDIF.
  ENDCASE.

ENDMETHOD.


METHOD GENERATE_INT_EX.

  DATA: lv_assoc_arrays     LIKE mv_assoc_arrays,
        lv_assoc_arrays_opt LIKE mv_assoc_arrays_opt,
        lv_mark             LIKE offset,
        lv_match            LIKE lv_mark,
        lv_json             TYPE ZUI2_JSON=>json.

  lv_mark = offset.
  restore_type( EXPORTING json = json length = length CHANGING offset = offset ).
  lv_match = offset - lv_mark.
  lv_json = json+lv_mark(lv_match).

  lv_assoc_arrays     = mv_assoc_arrays.
  lv_assoc_arrays_opt = mv_assoc_arrays_opt.

  mv_assoc_arrays     = abap_true.
  mv_assoc_arrays_opt = abap_true.

  data = generate_int( lv_json ).

  mv_assoc_arrays = lv_assoc_arrays.
  mv_assoc_arrays_opt = lv_assoc_arrays_opt.

ENDMETHOD.


METHOD GET_FIELDS.

  DATA: lt_symbols      TYPE t_t_symbol,
        lv_name     TYPE char128,
        ls_field    LIKE LINE OF rt_fields.

  FIELD-SYMBOLS: <sym>   LIKE LINE OF lt_symbols,
                 <cache> LIKE LINE OF mt_name_mappings.

  lt_symbols = get_symbols( type_descr = type_descr data = data object = object include_aliases = abap_true ).

  LOOP AT lt_symbols ASSIGNING <sym> WHERE read_only EQ abap_false.
    ls_field-name  = <sym>-name.
    ls_field-type  = <sym>-type.
    ls_field-value = <sym>-value.

    " insert as UPPER CASE
    INSERT ls_field INTO TABLE rt_fields.

    " insert as lower case
    TRANSLATE ls_field-name TO LOWER CASE.
    INSERT ls_field INTO TABLE rt_fields.

    " as pretty printed
    IF mv_pretty_name NE pretty_mode-none AND mv_pretty_name NE pretty_mode-low_case.
      format_name <sym>-name mv_pretty_name ls_field-name.
      INSERT ls_field INTO TABLE rt_fields.
      " let us check for not well formed canelCase to be compatible with old logic
      lv_name = ls_field-name.
      TRANSLATE lv_name(1) TO UPPER CASE.
      new_variable = ls_field-name = lv_name.
      INSERT ls_field INTO TABLE rt_fields.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD GET_SYMBOLS.

  DATA: comp_tab TYPE cl_abap_structdescr=>component_table,
        symb_tab LIKE result,
        symb     LIKE LINE OF symb_tab,
        class_descr   TYPE REF TO cl_abap_classdescr,
        struct_descr  TYPE REF TO cl_abap_structdescr.

  FIELD-SYMBOLS: <comp>  LIKE LINE OF comp_tab,
                 <attr>  LIKE LINE OF cl_abap_objectdescr=>attributes,
                 <cache> LIKE LINE OF mt_name_mappings,
                 <field> TYPE any.

  IF type_descr->kind EQ cl_abap_typedescr=>kind_struct.

    struct_descr ?= type_descr.
  comp_tab = struct_descr->get_components( ).

  LOOP AT comp_tab ASSIGNING <comp>.
      IF <comp>-name IS NOT INITIAL AND
        ( <comp>-as_include EQ abap_false OR include_aliases EQ abap_true OR mv_expand_includes EQ abap_false ).
        symb-name = <comp>-name.
        symb-type = <comp>-type.
        IF data IS BOUND.
          is_compressable symb-type symb-name symb-compressable.
          ASSIGN data->(symb-name) TO <field>.
          GET REFERENCE OF <field> INTO symb-value.
          format_name symb-name mv_pretty_name symb-header.
        CONCATENATE `"` symb-header  `":` INTO symb-header.
        ENDIF.
        APPEND symb TO result.
      ENDIF.
      IF <comp>-as_include EQ abap_true AND mv_expand_includes EQ abap_true.
        struct_descr ?= <comp>-type.
        symb_tab = get_symbols( type_descr = struct_descr include_aliases = include_aliases ).
        LOOP AT symb_tab INTO symb.
          CONCATENATE symb-name <comp>-suffix INTO symb-name.
          IF data IS BOUND.
            is_compressable symb-type symb-name symb-compressable.
        ASSIGN data->(symb-name) TO <field>.
        GET REFERENCE OF <field> INTO symb-value.
        format_name symb-name mv_pretty_name symb-header.
      CONCATENATE `"` symb-header  `":` INTO symb-header.
          ENDIF.
          APPEND symb TO result.
        ENDLOOP.
    ENDIF.
    ENDLOOP.

  ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_class.

    class_descr ?= type_descr.
    LOOP AT class_descr->attributes ASSIGNING <attr> WHERE is_constant IS INITIAL AND alias_for IS INITIAL AND
      ( is_interface IS INITIAL OR type_kind NE cl_abap_typedescr=>typekind_oref ).
      ASSIGN object->(<attr>-name) TO <field>.
      CHECK sy-subrc IS INITIAL. " we can only assign to public attributes
      symb-name = <attr>-name.
      symb-read_only = <attr>-is_read_only.
      symb-type = class_descr->get_attribute_type( <attr>-name ).
      is_compressable symb-type symb-name symb-compressable.
      GET REFERENCE OF <field> INTO symb-value.
      format_name symb-name mv_pretty_name symb-header.
      CONCATENATE `"` symb-header  `":` INTO symb-header.
      APPEND symb TO result.
    ENDLOOP.

  ENDIF.

ENDMETHOD.                    "GET_SYMBOLS


METHOD IS_COMPRESSABLE.
  rv_compress = abap_true.
ENDMETHOD.


METHOD PRETTY_NAME.

  DATA: tokens TYPE TABLE OF char128,
        cache  LIKE LINE OF mt_name_mappings.

  FIELD-SYMBOLS: <token> LIKE LINE OF tokens,
                 <cache> LIKE LINE OF mt_name_mappings.

  READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
  IF sy-subrc IS INITIAL.
    out = <cache>-json.
  ELSE.
    out = in.

    REPLACE ALL OCCURRENCES OF `__` IN out WITH `*`.

    TRANSLATE out TO LOWER CASE.
    TRANSLATE out USING `/_:_~_`.
    SPLIT out AT `_` INTO TABLE tokens.
    LOOP AT tokens ASSIGNING <token> FROM 2.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO out.
    REPLACE ALL OCCURRENCES OF `*` IN out WITH `_`.

    cache-abap  = in.
    cache-json = out.
    INSERT cache INTO TABLE mt_name_mappings.
    INSERT cache INTO TABLE mt_name_mappings_ex.
  ENDIF.

ENDMETHOD.                    "pretty_name


METHOD PRETTY_NAME_EX.

  DATA: tokens TYPE TABLE OF char128,
        cache     LIKE LINE OF mt_name_mappings,
        lt_match  TYPE match_result_tab.

  FIELD-SYMBOLS: <token>      LIKE LINE OF tokens,
                 <cache>      LIKE LINE OF mt_name_mappings,
                 <match>      LIKE LINE OF lt_match,
                 <sub_match>  TYPE LINE OF submatch_result_tab.

  READ TABLE mt_name_mappings WITH TABLE KEY abap = in ASSIGNING <cache>.
  IF sy-subrc IS INITIAL.
    out = <cache>-json.
  ELSE.
    out = in.


    TRANSLATE out TO LOWER CASE.
    TRANSLATE out USING `/_:_~_`.

    REPLACE ALL OCCURRENCES OF `__e__` IN out WITH `!`.
    REPLACE ALL OCCURRENCES OF `__n__` IN out WITH `#`.
    REPLACE ALL OCCURRENCES OF `__d__` IN out WITH `$`.
    REPLACE ALL OCCURRENCES OF `__p__` IN out WITH `%`.
    REPLACE ALL OCCURRENCES OF `__m__` IN out WITH `&`.
    REPLACE ALL OCCURRENCES OF `__s__` IN out WITH `*`.
    REPLACE ALL OCCURRENCES OF `__h__` IN out WITH `-`.
    REPLACE ALL OCCURRENCES OF `__t__` IN out WITH `~`.
    REPLACE ALL OCCURRENCES OF `__l__` IN out WITH `/`.
    REPLACE ALL OCCURRENCES OF `__c__` IN out WITH `:`.
    REPLACE ALL OCCURRENCES OF `__v__` IN out WITH `|`.
    REPLACE ALL OCCURRENCES OF `__a__` IN out WITH `@`.
    REPLACE ALL OCCURRENCES OF `__o__` IN out WITH `.`.
    REPLACE ALL OCCURRENCES OF `___` IN out WITH `.`.

    REPLACE ALL OCCURRENCES OF `__` IN out WITH `"`.

    SPLIT out AT `_` INTO TABLE tokens.
    LOOP AT tokens ASSIGNING <token> FROM 2.
      TRANSLATE <token>(1) TO UPPER CASE.
    ENDLOOP.

    CONCATENATE LINES OF tokens INTO out.
    REPLACE ALL OCCURRENCES OF `"` IN out WITH `_`.

    cache-abap  = in.
    cache-json = out.
    INSERT cache INTO TABLE mt_name_mappings.
    INSERT cache INTO TABLE mt_name_mappings_ex.
  ENDIF.

ENDMETHOD.                    "pretty_name_ex


METHOD RAW_TO_STRING.

  DATA: lv_output_length TYPE i,
        lt_binary_tab    TYPE STANDARD TABLE OF sdokcntbin.

  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      buffer        = iv_xstring
    IMPORTING
      output_length = lv_output_length
    TABLES
      binary_tab    = lt_binary_tab.

  CALL FUNCTION 'SCMS_BINARY_TO_STRING'
    EXPORTING
      input_length  = lv_output_length
      encoding      = iv_encoding
    IMPORTING
      text_buffer   = rv_string
      output_length = lv_output_length
    TABLES
      binary_tab    = lt_binary_tab.

ENDMETHOD.


METHOD RESTORE.

  DATA: mark        LIKE offset,
        match       LIKE offset,
        pos         LIKE offset,
        unescape    TYPE abap_bool,
        ref_descr   TYPE REF TO cl_abap_refdescr,
        data_descr  TYPE REF TO cl_abap_datadescr,
        data_ref    TYPE REF TO data,
        object_ref  TYPE REF TO object,
        fields      LIKE field_cache,
        name_json   TYPE string.

  FIELD-SYMBOLS: <value>       TYPE ANY,
                 <field_cache> LIKE LINE OF field_cache.

  fields = field_cache.

  IF type_descr IS NOT INITIAL AND type_descr->kind EQ type_descr->kind_ref.
      ref_descr ?= type_descr.
      type_descr = ref_descr->get_referenced_type( ).
      IF ref_descr->type_kind EQ ref_descr->typekind_oref.
        IF data IS INITIAL.
        " can fire an exception, if type is abstract or constructor protected
          CREATE OBJECT data TYPE (type_descr->absolute_name).
        ENDIF.
        object_ref ?= data.
      fields = get_fields( type_descr = type_descr object = object_ref ).
    ELSEIF ref_descr->type_kind EQ ref_descr->typekind_dref.
        IF data IS INITIAL.
        data_descr ?= type_descr.
        CREATE DATA data TYPE HANDLE data_descr.
        ENDIF.
        data_ref ?= data.
        ASSIGN data_ref->* TO <value>.
      fields = get_fields( type_descr = type_descr data = data_ref ).
      restore( EXPORTING json = json length = length type_descr = type_descr field_cache = fields
                 CHANGING data = <value> offset = offset ).
        RETURN.
      ENDIF.
    ENDIF.

  IF fields IS INITIAL AND type_descr IS NOT INITIAL AND type_descr->kind EQ type_descr->kind_struct.
    GET REFERENCE OF data INTO data_ref.
    fields = get_fields( type_descr = type_descr data = data_ref ).
  ENDIF.

  eat_white.
  eat_char `{`.
  eat_white.

  WHILE offset < length AND json+offset(1) NE `}`.

    eat_white.
    eat_string name_json.
    eat_white.
    eat_char `:`.
    eat_white.

    READ TABLE fields WITH TABLE KEY name = name_json ASSIGNING <field_cache>.
    IF sy-subrc IS NOT INITIAL.
      TRANSLATE name_json TO UPPER CASE.
      READ TABLE fields WITH TABLE KEY name = name_json ASSIGNING <field_cache>.
    ENDIF.

    IF sy-subrc IS INITIAL.
      ASSIGN <field_cache>-value->* TO <value>.
      restore_type( EXPORTING json = json length = length type_descr = <field_cache>-type CHANGING data = <value> offset = offset ).
    ELSE.
      restore_type( EXPORTING json = json length = length CHANGING offset = offset ).
    ENDIF.

    eat_white.

    IF offset < length AND json+offset(1) NE `}`.
      eat_char `,`.
    ELSE.
      EXIT.
    ENDIF.

  ENDWHILE.

  eat_char `}`.

ENDMETHOD.                    "restore


METHOD RESTORE_TYPE.

  DATA: mark        LIKE offset,
        match       LIKE offset,
        unescape    TYPE abap_bool,
        sdummy      TYPE string,                          "#EC NEEDED
        lr_idummy   TYPE REF TO i,                        "#EC NEEDED
        lr_bdummy   TYPE REF TO bool,                     "#EC NEEDED
        lr_sdummy   TYPE REF TO string,                   "#EC NEEDED
        pos         LIKE offset,
        line        TYPE REF TO data,
        key_ref     TYPE REF TO data,
        data_ref    TYPE REF TO data,
        key_name    TYPE string,
        key_value   TYPE string,
        lt_fields   LIKE field_cache,
        lt_symbols  TYPE t_t_symbol,
        lv_ticks    TYPE string,
        lv_offset   TYPE string,
        lo_exp      TYPE REF TO cx_root,
        elem_descr  TYPE REF TO cl_abap_elemdescr,
        table_descr TYPE REF TO cl_abap_tabledescr,
        data_descr  TYPE REF TO cl_abap_datadescr.

  FIELD-SYMBOLS: <line>           TYPE any,
                 <value>          TYPE any,
                 <data>      TYPE data,
                 <field>     LIKE LINE OF lt_fields,
                 <table>     TYPE ANY TABLE,
                 <value_sym> LIKE LINE OF lt_symbols.

  IF type_descr IS INITIAL AND data IS SUPPLIED.
    type_descr = cl_abap_typedescr=>describe_by_data( data ).
  ENDIF.

  eat_white.

  TRY .
      IF type_descr IS NOT INITIAL AND type_descr->absolute_name EQ mc_json_type.
        " skip deserialization
        mark = offset.
        restore_type( EXPORTING json = json length = length CHANGING offset = offset ).
        match = offset - mark.
        data = json+mark(match).
      ENDIF.

      CASE json+offset(1).
        WHEN `{`. " object
          IF type_descr IS NOT INITIAL.
            IF mv_assoc_arrays EQ c_bool-true AND type_descr->kind EQ cl_abap_typedescr=>kind_table.
              table_descr ?= type_descr.
              data_descr = table_descr->get_table_line_type( ).
                IF table_descr->has_unique_key IS NOT INITIAL.
                  eat_char `{`.
                  eat_white.
                  IF json+offset(1) NE `}`.
                    ASSIGN data TO <table>.
                  CLEAR <table>.
                  CREATE DATA line LIKE LINE OF <table>.
                    ASSIGN line->* TO <line>.
                  lt_fields = get_fields( type_descr = data_descr data = line ).
                  IF table_descr->key_defkind EQ table_descr->keydefkind_user AND LINES( table_descr->key ) EQ 1.
                    READ TABLE table_descr->key INDEX 1 INTO key_name.
                    READ TABLE lt_fields WITH TABLE KEY name = key_name ASSIGNING <field>.
                    key_ref = <field>-value.
                    IF mv_assoc_arrays_opt EQ c_bool-true.
                      lt_symbols = get_symbols( type_descr = data_descr data = line ).
                      DELETE lt_symbols WHERE name EQ key_name.
                      IF LINES( lt_symbols ) EQ 1.
                        READ TABLE lt_symbols INDEX 1 ASSIGNING <value_sym>.
                      ENDIF.
                    ENDIF.
                  ENDIF.
                  WHILE offset < length AND json+offset(1) NE `}`.
                      CLEAR <line>.
                      eat_white.
                      eat_string key_value.
                      eat_white.
                      eat_char `:`.
                      eat_white.
                    IF <value_sym> IS ASSIGNED.
                      ASSIGN <value_sym>-value->* TO <value>.
                      restore_type( EXPORTING json = json length = length type_descr = <value_sym>-type
                                    CHANGING data = <value> offset = offset ).
                    ELSE.
                      restore_type( EXPORTING json = json length = length type_descr = data_descr field_cache = lt_fields
                                    CHANGING data = <line> offset = offset ).
                    ENDIF.
                      IF table_descr->key_defkind EQ table_descr->keydefkind_user.
                      IF key_ref IS BOUND.
                        ASSIGN key_ref->* TO <value>.
                          IF <value> IS INITIAL.
                            MOVE key_value TO <value>.
                          ENDIF.
                        ENDIF.
                      ELSEIF <line> IS INITIAL.
                        MOVE key_value TO <line>.
                      ENDIF.

                    INSERT <line> INTO TABLE <table>.
                      eat_white.
                      IF offset < length AND json+offset(1) NE `}`.
                        eat_char `,`.
                      ELSE.
                        EXIT.
                      ENDIF.
                    ENDWHILE.
                ELSE.
                  CLEAR data.
                ENDIF.
                  eat_char `}`.
                ELSE.
                  restore( EXPORTING json = json length = length CHANGING  offset = offset ).
              ENDIF.
            ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
              IF data IS INITIAL.
                generate_int_ex( EXPORTING json = json length = length CHANGING offset = offset data = data ).
              ELSE.
                data_ref ?= data.
                type_descr = cl_abap_typedescr=>describe_by_data_ref( data_ref ).
                ASSIGN data_ref->* TO <data>.
                restore_type( EXPORTING json = json length = length type_descr = type_descr CHANGING data = <data> offset = offset ).
              ENDIF.
            ELSE.
              restore( EXPORTING json = json length = length type_descr = type_descr field_cache = field_cache
                       CHANGING data = data offset = offset ).
            ENDIF.
          ELSE.
            restore( EXPORTING json = json length = length CHANGING  offset = offset ).
          ENDIF.
        WHEN `[`. " array
          IF type_descr IS NOT INITIAL AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
            IF data IS INITIAL.
              generate_int_ex( EXPORTING json = json length = length CHANGING offset = offset data = data ).
            ELSE.
              data_ref ?= data.
              type_descr = cl_abap_typedescr=>describe_by_data_ref( data_ref ).
              ASSIGN data_ref->* TO <data>.
              restore_type( EXPORTING json = json length = length type_descr = type_descr CHANGING data = <data> offset = offset ).
            ENDIF.
          ELSE.
            eat_char `[`.
          eat_white.
          IF json+offset(1) NE `]`.
            IF type_descr IS NOT INITIAL AND type_descr->kind EQ cl_abap_typedescr=>kind_table.
              table_descr ?= type_descr.
              data_descr = table_descr->get_table_line_type( ).
              ASSIGN data TO <table>.
                CLEAR <table>.
                CREATE DATA line LIKE LINE OF <table>.
              ASSIGN line->* TO <line>.
              lt_fields = get_fields( type_descr = data_descr data = line ).
              WHILE offset < length AND json+offset(1) NE `]`.
                CLEAR <line>.
                restore_type( EXPORTING json = json length = length type_descr = data_descr field_cache = lt_fields
                              CHANGING data = <line> offset = offset ).
                INSERT <line> INTO TABLE <table>.
                eat_white.
                IF offset < length AND json+offset(1) NE `]`.
                  eat_char `,`.
                ELSE.
                  EXIT.
                ENDIF.
              ENDWHILE.
            ELSE.
              " skip array
              WHILE offset < length AND json+offset(1) NE `}`.
                eat_white.
                restore_type( EXPORTING json = json length = length CHANGING offset = offset ).
                eat_white.
                IF offset < length AND json+offset(1) NE `]`.
                  eat_char `,`.
                ELSE.
                  EXIT.
                ENDIF.
              ENDWHILE.
                IF type_descr IS NOT INITIAL.
                  eat_char `]`.
                  throw_error.
                ENDIF.
            ENDIF.
            ELSE.
              CLEAR data.
            ENDIF.
            eat_char `]`.
          ENDIF.
        WHEN `"`. " string
          eat_string sdummy.
          IF type_descr IS NOT INITIAL.
            " unescape string
            IF sdummy IS NOT INITIAL.
              IF type_descr->kind EQ cl_abap_typedescr=>kind_elem.
                elem_descr ?= type_descr.
                CASE elem_descr->type_kind.
                  WHEN cl_abap_typedescr=>typekind_char.
                    IF elem_descr->output_length EQ 1 AND mc_bool_types CS elem_descr->absolute_name.
                      IF sdummy(1) CA `XxTt1`.
                        data = c_bool-true.
                      ELSE.
                        data = c_bool-false.
                      ENDIF.
                      RETURN.
                    ENDIF.
                  WHEN cl_abap_typedescr=>typekind_xstring OR cl_abap_typedescr=>typekind_hex.
                    string_to_xstring( EXPORTING in = sdummy CHANGING out = data ).
                    RETURN.
                  WHEN cl_abap_typedescr=>typekind_date.
                    " support for ISO8601 => https://en.wikipedia.org/wiki/ISO_8601
                    REPLACE FIRST OCCURRENCE OF REGEX `^(\d{4})-(\d{2})-(\d{2})` IN sdummy WITH `$1$2$3`
                    REPLACEMENT LENGTH match.               "#EC NOTEXT
                    IF sy-subrc EQ 0.
                      sdummy = sdummy(match).
                    ELSE.
                      " support for Edm.DateTime => http://www.odata.org/documentation/odata-version-2-0/json-format/
                      FIND FIRST OCCURRENCE OF REGEX `^\/Date\((-?\d+)([+-]\d{1,4})?\)\/` IN sdummy SUBMATCHES lv_ticks lv_offset IGNORING CASE.
                      IF sy-subrc EQ 0.
                        sdummy = edm_datetime_to_ts( ticks = lv_ticks offset = lv_offset typekind = elem_descr->type_kind ).
                      ELSE.
                        " support for Edm.Time => https://www.w3.org/TR/xmlschema11-2/#nt-durationRep
                        REPLACE FIRST OCCURRENCE OF REGEX `^-?P(?:(\d+)Y)?(?:(\d+)M)?(?:(\d+)D)?(?:T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)(?:\.(\d+))?S)?)?` IN sdummy WITH `$1$2$3`
                        REPLACEMENT LENGTH match.               "#EC NOTEXT
                        IF sy-subrc EQ 0.
                          sdummy = sdummy(match).
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  WHEN cl_abap_typedescr=>typekind_time.
                    " support for ISO8601 => https://en.wikipedia.org/wiki/ISO_8601
                    REPLACE FIRST OCCURRENCE OF REGEX `^(\d{2}):(\d{2}):(\d{2})` IN sdummy WITH `$1$2$3`
                    REPLACEMENT LENGTH match.               "#EC NOTEXT
                    IF sy-subrc EQ 0.
                      sdummy = sdummy(match).
                    ELSE.
                      " support for Edm.DateTime => http://www.odata.org/documentation/odata-version-2-0/json-format/
                      FIND FIRST OCCURRENCE OF REGEX '^\/Date\((-?\d+)([+-]\d{1,4})?\)\/' IN sdummy SUBMATCHES lv_ticks lv_offset IGNORING CASE.
                      IF sy-subrc EQ 0.
                        sdummy = edm_datetime_to_ts( ticks = lv_ticks offset = lv_offset typekind = elem_descr->type_kind ).
                      ELSE.
                        " support for Edm.Time => https://www.w3.org/TR/xmlschema11-2/#nt-durationRep
                        REPLACE FIRST OCCURRENCE OF REGEX `^-?P(?:(\d+)Y)?(?:(\d+)M)?(?:(\d+)D)?(?:T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)(?:\.(\d+))?S)?)?` IN sdummy WITH `$4$5$6`
                        REPLACEMENT LENGTH match.               "#EC NOTEXT
                        IF sy-subrc EQ 0.
                          sdummy = sdummy(match).
                        ENDIF.
                      ENDIF.
                    ENDIF.
                  WHEN cl_abap_typedescr=>typekind_packed.
                    REPLACE FIRST OCCURRENCE OF REGEX `^(\d{4})-?(\d{2})-?(\d{2})T(\d{2}):?(\d{2}):?(\d{2})(?:[\.,](\d{0,7}))?Z?` IN sdummy WITH `$1$2$3$4$5$6.$7`
                    REPLACEMENT LENGTH match.               "#EC NOTEXT
                    IF sy-subrc EQ 0.
                      sdummy = sdummy(match).
                    ELSE.
                      FIND FIRST OCCURRENCE OF REGEX '^\/Date\((-?\d+)([+-]\d{1,4})?\)\/' IN sdummy SUBMATCHES lv_ticks lv_offset IGNORING CASE.
                      IF sy-subrc EQ 0.
                        sdummy = edm_datetime_to_ts( ticks = lv_ticks offset = lv_offset typekind = elem_descr->type_kind ).
                      ELSE.
                        " support for Edm.Time => https://www.w3.org/TR/xmlschema11-2/#nt-durationRep
                        REPLACE FIRST OCCURRENCE OF REGEX `^-?P(?:(\d+)Y)?(?:(\d+)M)?(?:(\d+)D)?(?:T(?:(\d+)H)?(?:(\d+)M)?(?:(\d+)(?:\.(\d+))?S)?)?` IN sdummy WITH `$1$2$3$4$5$6.$7`
                        REPLACEMENT LENGTH match.               "#EC NOTEXT
                        IF sy-subrc EQ 0.
                          sdummy = sdummy(match).
                        ENDIF.
                      ENDIF.
                    ENDIF.
                ENDCASE.
              ELSEIF type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                CREATE DATA lr_sdummy TYPE string.
                MOVE sdummy TO lr_sdummy->*.
                data ?= lr_sdummy.
                RETURN.
              ELSE.
                throw_error. " Other wise dumps with OBJECTS_MOVE_NOT_SUPPORTED
              ENDIF.
              MOVE sdummy TO data.
            ELSEIF type_descr->kind EQ cl_abap_typedescr=>kind_elem.
              CLEAR data.
            ELSE.
              throw_error. " Other wise dumps with OBJECTS_MOVE_NOT_SUPPORTED
            ENDIF.
          ENDIF.
        WHEN `-`. " number
          IF type_descr IS NOT INITIAL.
            IF type_descr->kind EQ type_descr->kind_ref AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
              CREATE DATA lr_idummy TYPE i.
              eat_number lr_idummy->*.                    "#EC NOTEXT
              data ?= lr_idummy.
            ELSEIF type_descr->kind EQ type_descr->kind_elem.
              eat_number data.                            "#EC NOTEXT
            ELSE.
              eat_number sdummy.                            "#EC NOTEXT
              throw_error.
            ENDIF.
          ELSE.
            eat_number sdummy.                            "#EC NOTEXT
          ENDIF.
        WHEN OTHERS.
          FIND FIRST OCCURRENCE OF json+offset(1) IN `0123456789`.
          IF sy-subrc IS INITIAL. " number
            IF type_descr IS NOT INITIAL.
              IF type_descr->kind EQ type_descr->kind_ref AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                CREATE DATA lr_idummy TYPE i.
                eat_number lr_idummy->*.                  "#EC NOTEXT
                data ?= lr_idummy.
              ELSEIF type_descr->kind EQ type_descr->kind_elem.
                eat_number data.                          "#EC NOTEXT
              ELSE.
                eat_number sdummy.                            "#EC NOTEXT
                throw_error.
              ENDIF.
            ELSE.
              eat_number sdummy.                          "#EC NOTEXT
            ENDIF.
          ELSE. " true/false/null
            IF type_descr IS NOT INITIAL.
              IF type_descr->kind EQ type_descr->kind_ref AND type_descr->type_kind EQ cl_abap_typedescr=>typekind_dref.
                CREATE DATA lr_bdummy TYPE bool.
                eat_bool lr_bdummy->*.                    "#EC NOTEXT
                data ?= lr_bdummy.
              ELSEIF type_descr->kind EQ type_descr->kind_elem.
                eat_bool data.                            "#EC NOTEXT
              ELSE.
                eat_bool sdummy.                            "#EC NOTEXT
                throw_error.
              ENDIF.
            ELSE.
              eat_bool sdummy.                            "#EC NOTEXT
            ENDIF.
          ENDIF.
      ENDCASE.
    CATCH cx_sy_move_cast_error cx_sy_conversion_no_number cx_sy_conversion_overflow INTO lo_exp.
      CLEAR data.
      IF mv_strict_mode EQ abap_true.
        RAISE EXCEPTION TYPE cx_sy_move_cast_error EXPORTING previous = lo_exp.
      ENDIF.
  ENDTRY.

ENDMETHOD.                    "restore_type


METHOD SERIALIZE.

  " **********************************************************************
  "! Usage examples and documentation can be found on SCN:
  " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
  " **********************************************************************  "

  DATA: lo_json  TYPE REF TO ZUI2_JSON.  "test

  CREATE OBJECT lo_json
    EXPORTING
      compress      = compress
      pretty_name   = pretty_name
      name_mappings     = name_mappings
      assoc_arrays      = assoc_arrays
      assoc_arrays_opt  = assoc_arrays_opt
      expand_includes   = expand_includes
      numc_as_string   = numc_as_string
      ts_as_iso8601    = ts_as_iso8601.

  r_json = lo_json->serialize_int( name = name data = data type_descr = type_descr ).

ENDMETHOD.                    "serialize


METHOD SERIALIZE_INT.

  " **********************************************************************
  "! Usage examples and documentation can be found on SCN:
  " http://wiki.scn.sap.com/wiki/display/Snippets/One+more+ABAP+to+JSON+Serializer+and+Deserializer
  " **********************************************************************  "
  constants: aaaa type char01 value '1'.
  DATA: lo_descr   TYPE REF TO cl_abap_typedescr.

  IF type_descr IS INITIAL.
    lo_descr = cl_abap_typedescr=>describe_by_data( data ).
  ELSE.
    lo_descr = type_descr.
  ENDIF.

  r_json = dump_int( data = data type_descr = lo_descr ).

  " we do not do escaping of every single string value for white space characters,
  " but we do it on top, to replace multiple calls by 3 only, while we do not serialize
  " outlined/formatted JSON this shall not produce any harm
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf          IN r_json WITH `\r\n`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline        IN r_json WITH `\n`.
  REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN r_json WITH `\t`.
* REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>form_feed      IN r_json WITH `\f`.
* REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>backspace      IN r_json WITH `\b`.

  IF name IS NOT INITIAL AND ( mv_compress IS INITIAL OR r_json IS NOT INITIAL ).
    CONCATENATE `"` name `":` r_json INTO r_json.
  ENDIF.

ENDMETHOD.                    "serialize


METHOD STRING_TO_RAW.

  CALL FUNCTION 'SCMS_STRING_TO_XSTRING'
    EXPORTING
      text   = iv_string
      encoding = iv_encoding
    IMPORTING
      buffer = rv_xstring
    EXCEPTIONS
      OTHERS = 1.

  IF sy-subrc IS NOT INITIAL.
    CLEAR rv_xstring.
  ENDIF.

ENDMETHOD.


METHOD STRING_TO_XSTRING.

  DATA: lv_xstring TYPE xstring.

  CALL FUNCTION 'SSFC_BASE64_DECODE'
    EXPORTING
      b64data = in
    IMPORTING
      bindata = lv_xstring
    EXCEPTIONS
      OTHERS  = 1.

  IF sy-subrc IS INITIAL.
    MOVE lv_xstring TO out.
  ELSE.
    MOVE in TO out.
  ENDIF.

ENDMETHOD.                    "string_to_xstring


METHOD TRIBOOL_TO_BOOL.
  IF iv_tribool EQ c_tribool-true.
    rv_bool = c_bool-true.
  ELSEIF iv_tribool EQ c_tribool-undefined.
    rv_bool = abap_undefined. " fall back to abap_undefined
  ENDIF.
ENDMETHOD.                    "TRIBOOL_TO_BOOL


METHOD XSTRING_TO_STRING.

  DATA: lv_xstring TYPE xstring,
        lv_in TYPE string.

  " let us fix data conversion issues here
  lv_in = in.
  lv_xstring = lv_in.

  CALL FUNCTION 'SSFC_BASE64_ENCODE'
    EXPORTING
      bindata = lv_xstring
    IMPORTING
      b64data = out
    EXCEPTIONS
      OTHERS  = 1.

  IF sy-subrc IS NOT INITIAL.
    MOVE lv_in TO out.
  ENDIF.

ENDMETHOD.                    "xstring_to_string
ENDCLASS.
