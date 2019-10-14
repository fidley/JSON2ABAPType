CLASS zui2_data_access DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS abap .

    CLASS-METHODS create
      IMPORTING
        !ir_data      TYPE REF TO data OPTIONAL
        !iv_data      TYPE data OPTIONAL
        !iv_component TYPE string OPTIONAL
      RETURNING
        VALUE(ro_ref) TYPE REF TO zui2_data_access .
    METHODS constructor
      IMPORTING
        !ir_data TYPE REF TO data OPTIONAL
        !iv_data TYPE data OPTIONAL .
    METHODS at
      IMPORTING
        !iv_component TYPE string OPTIONAL
      RETURNING
        VALUE(ro_ref) TYPE REF TO zui2_data_access .
    METHODS empty
      RETURNING
        VALUE(rv_val) TYPE abap_bool .
    METHODS ref
      RETURNING
        VALUE(rv_data) TYPE REF TO data .
    METHODS value
      IMPORTING
        !iv_default TYPE data OPTIONAL
      EXPORTING
        !ev_data    TYPE data .
    METHODS set
      IMPORTING
        !iv_data          TYPE data
      RETURNING
        VALUE(rv_success) TYPE abap_bool .
  PROTECTED SECTION.

    DATA mr_data TYPE REF TO data .

    CLASS-METHODS deref
      IMPORTING
        !ir_data       TYPE REF TO data
      RETURNING
        VALUE(rr_data) TYPE REF TO data .
    METHODS at_int
      IMPORTING
        !iv_component TYPE string OPTIONAL
        !iv_index     TYPE i OPTIONAL
        !iv_keys      TYPE string OPTIONAL
      RETURNING
        VALUE(ro_ref) TYPE REF TO zui2_data_access .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZUI2_DATA_ACCESS IMPLEMENTATION.


  METHOD at.

    DATA:
      lv_component TYPE string,
      lv_sindex    TYPE string,
      lv_keys      TYPE string,
      lv_index     TYPE i,
      lt_hier      TYPE match_result_tab.

    FIELD-SYMBOLS:
      <component> LIKE LINE OF lt_hier,
      <sub_match> TYPE LINE OF submatch_result_tab.

    " (?:(\w+)|^)(?:\[(?:(\d+)|([^\]]+))\])? - no check for separators
    FIND ALL OCCURRENCES OF REGEX `(?:(\w+)|^)(?:\[(?:(\d+)|([^\]]+))\])?(?:(?:-\>)|(?:-)|(?:=>)|$)` IN iv_component RESULTS lt_hier.

    ro_ref = me.

    LOOP AT lt_hier ASSIGNING <component>.
      CHECK ro_ref->empty( ) EQ abap_false.
      READ TABLE <component>-submatches INDEX 1 ASSIGNING <sub_match>.
      IF <sub_match>-length IS INITIAL.
        CLEAR lv_component.
      ELSE.
        lv_component = iv_component+<sub_match>-offset(<sub_match>-length).
        TRANSLATE lv_component TO UPPER CASE.
      ENDIF.
      READ TABLE <component>-submatches INDEX 2 ASSIGNING <sub_match>.
      IF <sub_match>-length IS INITIAL.
        CLEAR lv_index.
      ELSE.
        lv_index = lv_sindex = iv_component+<sub_match>-offset(<sub_match>-length).
      ENDIF.
      READ TABLE <component>-submatches INDEX 3 ASSIGNING <sub_match>.
      IF <sub_match>-length IS INITIAL.
        CLEAR lv_keys.
      ELSE.
        lv_keys = iv_component+<sub_match>-offset(<sub_match>-length).
      ENDIF.
      ro_ref = ro_ref->at_int( iv_component = lv_component iv_index = lv_index iv_keys = lv_keys ).
    ENDLOOP.

  ENDMETHOD.                    "at_int


  METHOD at_int.

    DATA: lv_key   TYPE string,
          lv_value TYPE string,
          lt_keys  TYPE match_result_tab,
          lr_data  TYPE REF TO data,
          lo_type  TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <data>      TYPE data,
                   <comp>      TYPE data,
                   <key>       LIKE LINE OF lt_keys,
                   <sub_match> TYPE LINE OF submatch_result_tab,
                   <ref>       TYPE REF TO data,
                   <table>     TYPE ANY TABLE,
                   <idx_table> TYPE INDEX TABLE.

    IF mr_data IS BOUND.
      IF iv_component IS NOT INITIAL.
        ASSIGN mr_data->* TO <data>.
        ASSIGN COMPONENT iv_component OF STRUCTURE <data> TO <comp>.
        IF <comp> IS ASSIGNED.
          GET REFERENCE OF <comp> INTO lr_data.
          lr_data = deref( lr_data ).
          ASSIGN lr_data TO <ref>.
        ENDIF.
      ELSE.
        ASSIGN mr_data TO <ref>.
      ENDIF.
    ENDIF.

    IF <ref> IS ASSIGNED AND ( iv_index IS NOT INITIAL OR iv_keys IS NOT INITIAL ).
      lo_type = cl_abap_typedescr=>describe_by_data_ref( <ref> ).
      IF lo_type->kind EQ cl_abap_typedescr=>kind_table.
        " check for table index access
        IF iv_index IS NOT INITIAL.
          ASSIGN <ref>->* TO <idx_table>.
          IF sy-subrc IS INITIAL.
            READ TABLE <idx_table> INDEX iv_index REFERENCE INTO <ref>.
            IF sy-subrc IS NOT INITIAL.
              UNASSIGN <ref>.
            ENDIF.
          ELSE.
            UNASSIGN <ref>.
          ENDIF.
        ELSEIF iv_keys IS NOT INITIAL.
          ASSIGN <ref>->* TO <table>.
          IF sy-subrc IS INITIAL.
            CREATE DATA lr_data LIKE LINE OF <table>.
            ASSIGN lr_data->* TO <data>.
            FIND ALL OCCURRENCES OF REGEX `(\w+)\s*=\s*([^,]*),?` IN iv_keys RESULTS lt_keys.
            IF sy-subrc IS INITIAL.
              LOOP AT lt_keys ASSIGNING <key>.
                READ TABLE <key>-submatches INDEX 1 ASSIGNING <sub_match>.
                lv_key = iv_keys+<sub_match>-offset(<sub_match>-length).
                TRANSLATE lv_key TO UPPER CASE.
                READ TABLE <key>-submatches INDEX 2 ASSIGNING <sub_match>.
                lv_value = iv_keys+<sub_match>-offset(<sub_match>-length).
                ASSIGN COMPONENT lv_key OF STRUCTURE <data> TO <comp>.
                CHECK sy-subrc IS INITIAL.
                <comp> = lv_value.
              ENDLOOP.
            ELSE.
              <data> = lv_key.
            ENDIF.
            READ TABLE <table> FROM <data> REFERENCE INTO <ref>.
            IF sy-subrc IS NOT INITIAL.
              UNASSIGN <ref>.
            ENDIF.
          ELSE.
            UNASSIGN <ref>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <ref> IS ASSIGNED.
      CREATE OBJECT ro_ref
        EXPORTING
          ir_data = <ref>.
    ELSE.
      CREATE OBJECT ro_ref.
    ENDIF.

  ENDMETHOD.                    "at_int


  METHOD constructor.
    IF ir_data IS NOT INITIAL.
      mr_data = ir_data.
    ELSEIF iv_data IS SUPPLIED.
      GET REFERENCE OF iv_data INTO mr_data.
    ENDIF.
    mr_data = deref( mr_data ).
  ENDMETHOD.                    "constructor


  METHOD create.
    IF iv_data IS SUPPLIED.
      CREATE OBJECT ro_ref
        EXPORTING
          iv_data = iv_data.
    ELSE.
      CREATE OBJECT ro_ref
        EXPORTING
          ir_data = ir_data.
    ENDIF.

    IF iv_component IS NOT INITIAL.
      ro_ref = ro_ref->at( iv_component ).
    ENDIF.
  ENDMETHOD.                    "create


  METHOD deref.

    DATA: lo_type TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <data> TYPE data.

    rr_data = ir_data.
    IF rr_data IS NOT INITIAL.
      lo_type = cl_abap_typedescr=>describe_by_data_ref( ir_data ).
      IF lo_type->kind EQ cl_abap_typedescr=>kind_ref.
        ASSIGN ir_data->* TO <data>.
        rr_data = deref( <data> ).
      ENDIF.
    ENDIF.

  ENDMETHOD.                    "deref


  METHOD empty.
    IF mr_data IS INITIAL.
      rv_val = abap_true.
    ENDIF.
  ENDMETHOD.                    "empty


  METHOD ref.
    rv_data = mr_data.
  ENDMETHOD.                    "ref


  METHOD set.

    FIELD-SYMBOLS: <data> TYPE data.

    IF mr_data IS BOUND.
      ASSIGN mr_data->* TO <data>.
      <data> = iv_data.
      rv_success = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD value.

    DATA: lo_type_out TYPE REF TO cl_abap_typedescr,
          lo_type_in  TYPE REF TO cl_abap_typedescr.

    FIELD-SYMBOLS: <data> TYPE data.

    CLEAR ev_data.

    IF mr_data IS BOUND.
      ASSIGN mr_data->* TO <data>.
      lo_type_out = cl_abap_typedescr=>describe_by_data( ev_data ).
      lo_type_in = cl_abap_typedescr=>describe_by_data( <data> ).
      IF lo_type_out->kind EQ lo_type_in->kind.
        ev_data = <data>.
      ENDIF.
    ELSEIF iv_default IS SUPPLIED.
      ev_data = iv_default.
    ENDIF.

  ENDMETHOD.                    "value
ENDCLASS.
