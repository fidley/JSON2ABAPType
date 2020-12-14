"! This is demo for converting JSON structure into
"! ABAP type
"! done by Lukasz Pegiel for http://abapblog.com

REPORT zjson2abaptype.
DATA: ok_code TYPE sy-ucomm.


SELECTION-SCREEN BEGIN OF SCREEN 1001 AS SUBSCREEN.

PARAMETERS: p_none   RADIOBUTTON GROUP gr1,
            p_low    RADIOBUTTON GROUP gr1,
            p_camel  RADIOBUTTON GROUP gr1,
            p_ext    RADIOBUTTON GROUP gr1 DEFAULT 'X',
            p_user   RADIOBUTTON GROUP gr1,
            p_userlo RADIOBUTTON GROUP gr1.

SELECTION-SCREEN END OF SCREEN 1001.

CLASS lcl_json_structure DEFINITION DEFERRED.
CLASS lcl_hlp DEFINITION.
  PUBLIC SECTION.
    TYPES: BEGIN OF t_source,
             line TYPE char255,
           END OF t_source.
    TYPES: tt_source TYPE STANDARD TABLE OF t_source WITH DEFAULT KEY.
    DATA: converter     TYPE REF TO lcl_json_structure,
          results       TYPE string,
          source_editor TYPE REF TO cl_gui_textedit,
          abap_editor   TYPE REF TO cl_gui_abapedit.
    METHODS: constructor.
    METHODS: create_source_editor.
    METHODS: convert.
    METHODS: pai IMPORTING VALUE(i_okcode) TYPE sy-ucomm.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF user_command,
                 back    TYPE sy-ucomm VALUE 'BACK' ##NO_TEXT,
                 up      TYPE sy-ucomm VALUE 'UP' ##NO_TEXT,
                 exit    TYPE sy-ucomm VALUE 'EXIT' ##NO_TEXT,
                 convert TYPE sy-ucomm VALUE 'CONVERT' ##NO_TEXT,
               END OF user_command.

    METHODS call_editor
      CHANGING
        c_source TYPE tt_source OPTIONAL.
    METHODS pretty_print_code
      CHANGING
        c_source TYPE tt_source.
    METHODS get_pretty_name_mode RETURNING VALUE(pretty_name_mode) TYPE char1.
    DATA: handler TYPE REF TO cl_wb_editor.

ENDCLASS.

CLASS lcl_json_structure DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF t_hierarchy,
             level           TYPE i,
             name            TYPE string,
             table           TYPE abap_bool,
             structure       TYPE abap_bool,
             type            TYPE string,
             length          TYPE i,
             decimals        TYPE i,
             absolute_type   TYPE  abap_abstypename,
             parent          TYPE string,
             final_type      TYPE string,
             type_definition TYPE string,
             id              TYPE i,
           END OF t_hierarchy,
           tt_hierarchy TYPE STANDARD TABLE OF t_hierarchy WITH DEFAULT KEY.
    CONSTANTS: c_components TYPE string VALUE '&&components&&'.
    DATA: hierarchy TYPE tt_hierarchy.

    METHODS: build_structure IMPORTING i_data TYPE REF TO data
                             EXPORTING e_data TYPE string.
  PRIVATE SECTION.
    DATA: current_id TYPE i.
    METHODS check_component
      IMPORTING
        i_comp          TYPE abap_compdescr
        VALUE(i_data)   TYPE REF TO data
        VALUE(i_parent) TYPE  abap_abstypename
        i_level         TYPE i
        i_keep_level    TYPE abap_bool DEFAULT abap_false
        i_keep_id       TYPE abap_bool DEFAULT abap_false.
    METHODS check_object
      IMPORTING
        VALUE(i_data)   TYPE REF TO data
        VALUE(i_parent) TYPE  abap_abstypename
        i_abap_type     TYPE REF TO cl_abap_structdescr
        i_level         TYPE i.
    METHODS create_types RETURNING VALUE(r_definition) TYPE string.
    METHODS: get_id RETURNING VALUE(r_id) TYPE i.
    METHODS: display.
    METHODS: get_types RETURNING VALUE(r_types) TYPE string,
      init,
      get_internal_types
        CHANGING
          VALUE(c_type) TYPE t_hierarchy.
ENDCLASS.


START-OF-SELECTION.
  DATA(hlp) = NEW lcl_hlp( ).
  CALL SCREEN 0100.




*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pbo OUTPUT.
  SET PF-STATUS 'STATUS_0100'.
  SET TITLEBAR 'TITLE'.
  hlp->create_source_editor( ).
ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE pai INPUT.
  hlp->pai( ok_code ).
ENDMODULE.

CLASS lcl_hlp IMPLEMENTATION.

  METHOD constructor.
    converter = NEW #( ).
  ENDMETHOD.

  METHOD create_source_editor.

    IF source_editor IS INITIAL.
      source_editor = NEW #( parent =  NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_left
                                                                     no_autodef_progid_dynnr = abap_true
                                                           extension = 500 ) ) .
    ENDIF.

  ENDMETHOD.

  METHOD convert.
    DATA: source TYPE soli_tab.
    source_editor->get_text_as_stream(
      IMPORTING
        text                   =  source
      EXCEPTIONS
        error_cntl_call_method = 1
        OTHERS                 = 3
    ).
    IF sy-subrc EQ 0.

      IF source IS INITIAL.
        MESSAGE s001(00) WITH 'Data input is required' DISPLAY LIKE 'E' ##MG_MISSING ##NO_TEXT.
        RETURN.
      ENDIF.

*      DATA(json_data) = /ui2/cl_json=>generate(
      DATA(json_data) = zui2_json=>generate(
        json        = cl_bcs_convert=>txt_to_string( it_soli = source )
        pretty_name = get_pretty_name_mode( )
      ).

      IF json_data IS INITIAL.
        MESSAGE s001(00) WITH 'Problem converting JSON.' DISPLAY LIKE 'E' ##MG_MISSING ##NO_TEXT.
      ELSE.

        converter->build_structure(
          EXPORTING
            i_data = json_data
          IMPORTING
            e_data = results
        ).

        DATA: target TYPE STANDARD TABLE OF t_source.
        SPLIT results AT cl_abap_char_utilities=>newline INTO TABLE target.
        pretty_print_code( CHANGING c_source = target ).

        call_editor( CHANGING c_source = target ).

      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD call_editor.

    IF abap_editor IS INITIAL.
      DATA(container) = NEW cl_gui_custom_container( container_name = 'CC_INPUT' ).
      abap_editor = NEW #( parent = container  ).
    ENDIF.

    abap_editor->draw( ).
    abap_editor->set_text( c_source ).
    abap_editor->undraw( ).

  ENDMETHOD.



  METHOD pretty_print_code.

    CALL FUNCTION 'PRETTY_PRINTER'
      EXPORTING
        inctoo             = abap_false  " X = Process Include Programs as Well
      TABLES
        ntext              = c_source " Table of Formatted Source Code
        otext              = c_source   " Table of Source Code Pending Editing
      EXCEPTIONS
        enqueue_table_full = 0
        include_enqueued   = 0
        include_readerror  = 0
        include_writeerror = 0
        OTHERS             = 0.
  ENDMETHOD.



  METHOD pai.
    CLEAR sy-ucomm.
    CASE i_okcode.
      WHEN user_command-back OR user_command-up OR user_command-exit .
        LEAVE PROGRAM.
      WHEN user_command-convert .
        convert( ).
    ENDCASE.
  ENDMETHOD.

  METHOD get_pretty_name_mode.

    pretty_name_mode = COND #( WHEN p_none   EQ abap_true THEN zui2_json=>pretty_mode-none
                               WHEN p_camel  EQ abap_true THEN zui2_json=>pretty_mode-camel_case
                               WHEN p_ext    EQ abap_true THEN zui2_json=>pretty_mode-extended
                               WHEN p_low    EQ abap_true THEN zui2_json=>pretty_mode-low_case
                               WHEN p_user   EQ abap_true THEN zui2_json=>pretty_mode-user
                               WHEN p_userlo EQ abap_true THEN zui2_json=>pretty_mode-user_low_case
                               ELSE zui2_json=>pretty_mode-extended
                                 ).
  ENDMETHOD.

ENDCLASS.


CLASS lcl_json_structure IMPLEMENTATION.
  METHOD get_id.
    ADD 1 TO current_id.
    r_id = current_id.
  ENDMETHOD.
  METHOD build_structure.
    init( ).
    DATA: level         TYPE i VALUE 0,
          abap_type     TYPE REF TO cl_abap_structdescr,
          abap_ref_type TYPE REF TO cl_abap_refdescr.
    FIELD-SYMBOLS: <table> TYPE ANY TABLE.

    TRY.
        DATA(abap_type_table) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( p_data_ref = i_data ) ).
        ASSIGN i_data->* TO <table>.
        LOOP AT <table> ASSIGNING FIELD-SYMBOL(<line>).
          EXIT.
        ENDLOOP.

        APPEND VALUE #( level = level name = 'JSON' type = abap_type_table->type_kind absolute_type = abap_type_table->absolute_name table = abap_true id = get_id( ) ) TO hierarchy.
        abap_type = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( p_data_ref = <line> ) ).

        ADD 1 TO level.
        APPEND VALUE #( level = level name = 'JSON' type = abap_type->type_kind absolute_type = abap_type->absolute_name structure = abap_true id = get_id( ) ) TO hierarchy.
        check_object( i_abap_type = abap_type
                      i_level = level
                      i_data = <line>
                      i_parent = '' ).

      CATCH cx_sy_move_cast_error.
        abap_type = CAST cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( p_data_ref = i_data ) ).
        APPEND VALUE #( level = level name = 'JSON' type = abap_type->type_kind absolute_type = abap_type->absolute_name structure = abap_true id = get_id( ) ) TO hierarchy.
        check_object( i_abap_type = abap_type i_level = level i_data = i_data i_parent = '' ).
    ENDTRY.
    e_data = create_types( ).
  ENDMETHOD.

  METHOD init.

    REFRESH: hierarchy.
    CLEAR current_id.

  ENDMETHOD.

  METHOD display.
    cl_demo_output=>display( create_types( ) ).
  ENDMETHOD.
  METHOD check_object.

    LOOP AT i_abap_type->components ASSIGNING FIELD-SYMBOL(<comp>).
      DATA(field) = |i_data->{ <comp>-name }|.
      ASSIGN (field) TO FIELD-SYMBOL(<data>).
      IF <data> IS ASSIGNED AND <data> IS NOT INITIAL.

        check_component(
              i_parent = i_abap_type->absolute_name
              i_comp = <comp>
              i_data = <data>
              i_level = i_level ).

      ENDIF.
      UNASSIGN <data>.
    ENDLOOP.

  ENDMETHOD.                                             "#EC CI_VALPAR

  METHOD check_component.

    DATA level TYPE i VALUE 0.
    IF i_keep_level EQ abap_false.
      level = i_level + 1.
    ELSE.
      level = i_level.
    ENDIF.
    TRY.
        DATA(str_type) = CAST cl_abap_structdescr(  cl_abap_structdescr=>describe_by_data_ref( p_data_ref  = i_data ) ).

        check_object( i_parent = i_parent
                      i_data  = i_data
                      i_abap_type = str_type
                      i_level     = level
                     ).
        APPEND VALUE #( level = level name = i_comp-name type = str_type->type_kind absolute_type = str_type->absolute_name parent = i_parent structure = abap_true  id = get_id( ) ) TO hierarchy.
      CATCH cx_root.

        TRY.
            DATA(table_type) = CAST cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( p_data_ref = i_data ) ).
            IF i_keep_id EQ abap_false.
              DATA(id) = get_id( ).
              APPEND VALUE #( level = level name = i_comp-name type = table_type->type_kind absolute_type = table_type->absolute_name parent = i_parent table  = abap_true  id = id ) TO hierarchy.
            ELSE.
              id = current_id.
            ENDIF.

            FIELD-SYMBOLS: <tab>  TYPE STANDARD TABLE,
                           <test> TYPE any.
            ASSIGN i_data->* TO <tab>.
            TRY.
                ASSIGN <tab>[ 1 ] TO <test>.
                IF sy-subrc NE 0.
                  APPEND INITIAL LINE TO <tab> ASSIGNING <test>.
                ENDIF.
              CATCH cx_root.
                APPEND INITIAL LINE TO <tab> ASSIGNING <test>.
            ENDTRY.

            cl_abap_structdescr=>describe_by_data_ref( EXPORTING p_data_ref = <test>
                                                       RECEIVING p_descr_ref = DATA(table_line_ref)
                                                       EXCEPTIONS OTHERS = 1 ).
            IF sy-subrc NE 0.
              DATA: table_of_strings TYPE STANDARD TABLE OF string.
              check_component(
                EXPORTING
                  i_comp   = i_comp
                  i_data   = REF #( table_of_strings )
                  i_parent = i_parent
                  i_level  = i_level
                  i_keep_level = abap_true
                  i_keep_id  = abap_true
              ).
              RETURN.
            ENDIF.

            DATA(table_line_type) = CAST cl_abap_structdescr( table_line_ref  ).
            APPEND VALUE #( level = level name = i_comp-name type = table_line_type->type_kind absolute_type = table_line_type->absolute_name parent = table_type->absolute_name structure  = abap_true  id = id ) TO hierarchy.

            check_object( i_parent = table_type->absolute_name
                          i_data  = <test>
                          i_abap_type = table_line_type
                          i_level     = level
                        ).

          CATCH cx_root.
            "May be a table of strings or integers.
            IF table_type IS NOT INITIAL.
              TRY.
                  IF table_type->type_kind EQ 'h'.
                    DATA(table_line_as_el_type) = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data_ref( p_data_ref = <test> )  ).
                    ADD 1 TO level.
                    APPEND VALUE #( level = level name = i_comp-name type = table_line_as_el_type->type_kind absolute_type = table_line_as_el_type->absolute_name parent = table_line_as_el_type->absolute_name structure  = abap_true  id = id ) TO hierarchy.
                  ENDIF.
                CATCH cx_root.
                  TRY.
                      IF table_type->type_kind EQ 'h'.
                        table_line_as_el_type = CAST cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( p_data = <test> )  ).
                        ADD 1 TO level.
                    APPEND VALUE #( level = level name = i_comp-name type = table_line_as_el_type->type_kind absolute_type = table_line_as_el_type->absolute_name parent = table_line_as_el_type->absolute_name structure  = abap_true  id = id ) TO hierarchy.
                      ENDIF.
                    CATCH cx_root.

                      DATA(other_type) =  cl_abap_typedescr=>describe_by_data_ref( p_data_ref  = i_data ) .
                      APPEND VALUE #( level = level name = i_comp-name type = other_type->type_kind length = other_type->length decimals = other_type->decimals absolute_type = other_type->absolute_name parent = i_parent ) TO hierarchy.
                  ENDTRY.

              ENDTRY.
            ELSE.
              other_type =  cl_abap_typedescr=>describe_by_data_ref( p_data_ref  = i_data ) .
              APPEND VALUE #( level = level name = i_comp-name type = other_type->type_kind length = other_type->length decimals = other_type->decimals absolute_type = other_type->absolute_name parent = i_parent ) TO hierarchy.
            ENDIF.
        ENDTRY.
    ENDTRY.
  ENDMETHOD.                                             "#EC CI_VALPAR

  METHOD create_types.
    DATA: components TYPE string.
    LOOP AT hierarchy ASSIGNING FIELD-SYMBOL(<h>).
      IF <h>-structure EQ abap_true AND <h>-type NE 'g'.
        <h>-final_type = |{ <h>-name } type t_{ <h>-name }{ <h>-id }| ##NO_TEXT.
        <h>-type_definition = |types: begin of t_{ <h>-name }{ <h>-id },{ cl_abap_char_utilities=>newline }{ c_components }end of t_{ <h>-name }{ <h>-id }.| ##NO_TEXT.
      ELSEIF <h>-structure EQ abap_true.
        <h>-final_type = |{ <h>-name } type t_{ <h>-name }{ <h>-id }| ##NO_TEXT.
        get_internal_types( CHANGING  c_type = <h> ).
        <h>-type_definition = |types: t_{ <h>-name }{ <h>-id } type { <h>-absolute_type }.| ##NO_TEXT.
      ELSEIF <h>-table EQ abap_true.
        <h>-final_type = |{ <h>-name } type tt_{ <h>-name }{ <h>-id }| ##NO_TEXT.
        <h>-type_definition = |types: tt_{ <h>-name }{ <h>-id } type standard table of t_{ <h>-name }{ <h>-id } with default key.| ##NO_TEXT.
      ELSE.

        get_internal_types( CHANGING  c_type = <h> ).
      ENDIF.
    ENDLOOP.

    LOOP AT hierarchy ASSIGNING <h> GROUP BY ( parent = <h>-parent ).
      CLEAR components.
      LOOP AT GROUP <h> ASSIGNING FIELD-SYMBOL(<g>).
        components = components && <g>-final_type && ',' && cl_abap_char_utilities=>newline.
      ENDLOOP.
      ASSIGN hierarchy[ absolute_type = <h>-parent ] TO FIELD-SYMBOL(<parent>).
      IF sy-subrc EQ 0.
        REPLACE ALL OCCURRENCES OF c_components IN <parent>-type_definition WITH components.
      ENDIF.
    ENDLOOP.

    SORT hierarchy BY level DESCENDING structure DESCENDING.
    LOOP AT hierarchy ASSIGNING <h> WHERE structure EQ abap_true
                                       OR table EQ abap_true.
      r_definition = r_definition && <h>-type_definition && cl_abap_char_utilities=>newline.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_types.
    r_types = create_types( ).
  ENDMETHOD.

  METHOD get_internal_types.
    REPLACE FIRST OCCURRENCE OF REGEX '\\TYPE-POOL=(.*)\\TYPE=' IN c_type-absolute_type WITH ''.
    IF sy-subrc EQ 0.
      c_type-final_type = |{ c_type-name } type { c_type-absolute_type }|.
      RETURN.
    ELSE.
      REPLACE FIRST OCCURRENCE OF '\TYPE=' IN c_type-absolute_type WITH ' '.
    ENDIF.

    IF c_type-type EQ cl_abap_typedescr=>typekind_char.
      c_type-final_type = |{ c_type-name } type { c_type-absolute_type } length { c_type-length }|.
    ELSEIF c_type-type EQ cl_abap_typedescr=>typekind_packed.
      c_type-final_type = |{ c_type-name } type { c_type-type } length { c_type-length } decimals { c_type-decimals }|.
    ELSEIF c_type-type EQ cl_abap_typedescr=>typekind_num.
      c_type-final_type = |{ c_type-name } type { c_type-absolute_type } length { c_type-length }|.
    ELSE.
      c_type-final_type = |{ c_type-name } type { c_type-absolute_type }|.
    ENDIF.
  ENDMETHOD.                                             "#EC CI_VALPAR
ENDCLASS.
