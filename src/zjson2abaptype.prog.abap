"! This is demo for converting JSON structure into
"! ABAP type
"! done by Lukasz Pegiel for http://abapblog.com

report zjson2abaptype.
data: ok_code type sy-ucomm.

class lcl_json_structure definition deferred.
class lcl_hlp definition.
  public section.

    data: converter      type ref to lcl_json_structure,
          results        type string,
          source_editor  type ref to cl_gui_textedit,
          results_editor type ref to cl_gui_textedit.
    methods: constructor.
    methods: create_source_editor.
    methods: create_results_editor.
    methods: convert.
endclass.

class lcl_json_structure definition.

  public section.

    types: begin of t_hierarchy,
             level           type i,
             name            type string,
             table           type abap_bool,
             structure       type abap_bool,
             type            type string,
             lenght          type i,
             decimals        type i,
             absolute_type   type  abap_abstypename,
             parent          type string,
             final_type      type string,
             type_definition type string,
             id              type i,
           end of t_hierarchy,
           tt_hierarchy type standard table of t_hierarchy with default key.
    constants: c_components type string value '&&components&&'.
    data: hierarchy type tt_hierarchy.

    methods: build_structure importing i_data type ref to data
                             exporting e_data type string.
  private section.
    data: current_id type i.
    methods check_component
      importing
        i_comp          type abap_compdescr
        value(i_data)   type ref to data
        value(i_parent) type  abap_abstypename
        i_level         type i.
    methods check_object
      importing
        value(i_data)   type ref to data
        value(i_parent) type  abap_abstypename
        i_abap_type     type ref to cl_abap_structdescr
        i_level         type i.
    methods create_types returning value(r_definition) type string.
    methods: get_id returning value(r_id) type i.
    methods: display.
    methods: get_types returning value(r_types) type string,
      init.
endclass.


start-of-selection.
  data(hlp) = new lcl_hlp( ).
  call screen 0100.




*&---------------------------------------------------------------------*
*&      Module  PBO  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pbo output.
  set pf-status 'STATUS_0100'.
  hlp->create_results_editor( ).
  hlp->create_source_editor( ).
endmodule.
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai input.
  case ok_code.
    when 'BACK' or 'UP' or 'EXIT'.
      leave program.
    when '&CONVERT'.
      hlp->convert( ).
  endcase.
  clear ok_code.
endmodule.

class lcl_hlp implementation.

  method constructor.
    converter = new #( ).
  endmethod.

  method create_results_editor.
    if results_editor is initial.
      results_editor = new #( parent = new cl_gui_custom_container( container_name = 'CC_OUTPUT' ) ) .
    endif.
  endmethod.

  method create_source_editor.
    if source_editor is initial.
      source_editor = new #( parent = new cl_gui_custom_container( container_name = 'CC_INPUT' ) ) .
    endif.
  endmethod.

  method convert.
    data: source type soli_tab.
    source_editor->get_text_as_stream(
      importing
        text                   =  source
      exceptions
        error_cntl_call_method = 1
        others                 = 3
    ).
    if sy-subrc eq 0.
      converter->build_structure(
        exporting
          i_data = /ui2/cl_json=>generate( json = cl_bcs_convert=>txt_to_string( it_soli   = source ) pretty_name = 'Y' )
        importing
          e_data = results
      ).

      results_editor->set_textstream(
        exporting
          text                   =  results
        exceptions
          error_cntl_call_method = 1
          not_supported_by_gui   = 2
          others                 = 3
      ).
      if sy-subrc <> 0.
        message id sy-msgid type sy-msgty number sy-msgno
                   with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      endif.
    endif.
  endmethod.

endclass.


class lcl_json_structure implementation.
  method get_id.
    add 1 to current_id.
    r_id = current_id.
  endmethod.
  method build_structure.
    init( ).
    data: level type i value 0.
    data(abap_type) = cast cl_abap_structdescr( cl_abap_structdescr=>describe_by_data_ref( p_data_ref = i_data ) ).
    append value #( level = level name = 'JSON' type = abap_type->type_kind absolute_type = abap_type->absolute_name structure = abap_true id = get_id( ) ) to hierarchy.
    check_object( i_abap_type = abap_type i_level = level i_data = i_data i_parent = '' ).
    e_data = create_types( ).
  endmethod.

  method init.

    refresh: hierarchy.
    clear current_id.

  endmethod.



  method display.
    cl_demo_output=>display( create_types( ) ).
  endmethod.
  method check_object.

    loop at i_abap_type->components assigning field-symbol(<comp>).
      data(field) = |i_data->{ <comp>-name }|.
      assign (field) to field-symbol(<data>).
      if <data> is assigned and <data> is not initial.

        check_component(
              i_parent = i_abap_type->absolute_name
              i_comp = <comp>
              i_data = <data>
              i_level = i_level ).

      endif.
      unassign <data>.
    endloop.

  endmethod.



  method check_component.

    data level type i value 0.

    level = i_level + 1.

    try.
        data(str_type) = cast cl_abap_structdescr(  cl_abap_structdescr=>describe_by_data_ref( p_data_ref  = i_data ) ).

        check_object( i_parent = i_parent
                      i_data  = i_data
                      i_abap_type = str_type
                      i_level     = level
                     ).
        append value #( level = level name = i_comp-name type = str_type->type_kind absolute_type = str_type->absolute_name parent = i_parent structure = abap_true  id = get_id( ) ) to hierarchy.
      catch cx_root.

        try.
            data(table_type) = cast cl_abap_tabledescr( cl_abap_tabledescr=>describe_by_data_ref( p_data_ref = i_data ) ).
            append value #( level = level name = i_comp-name type = table_type->type_kind absolute_type = table_type->absolute_name parent = i_parent table  = abap_true  id = get_id( ) ) to hierarchy.

            field-symbols: <tab>  type standard table,
                           <test> type any.
            assign i_data->* to <tab>.
            try.
                assign <tab>[ 1 ] to <test>.
              catch cx_root.
                append initial line to <tab> assigning <test>.
            endtry.

            data(table_line_type) = cast cl_abap_structdescr(  cl_abap_structdescr=>describe_by_data_ref( p_data_ref = <test> ) ).
            append value #( level = level name = i_comp-name type = table_line_type->type_kind absolute_type = table_line_type->absolute_name parent = table_type->absolute_name structure  = abap_true  id = get_id( ) ) to hierarchy.

            check_object( i_parent = table_type->absolute_name
                          i_data  = <test>
                          i_abap_type = table_line_type
                          i_level     = level
                        ).

          catch cx_root.

            data(other_type) =  cl_abap_typedescr=>describe_by_data_ref( p_data_ref  = i_data ) .
            append value #( level = level name = i_comp-name type = other_type->type_kind lenght = other_type->length decimals = other_type->decimals absolute_type = other_type->absolute_name parent = i_parent ) to hierarchy.
        endtry.
    endtry.

  endmethod.



  method create_types.
    data: components type string.
    loop at hierarchy assigning field-symbol(<h>).
      if <h>-structure eq abap_true.
        <h>-final_type = |{ <h>-name } type t_{ <h>-name }{ <h>-id }|.
        <h>-type_definition = |types: begin of t_{ <h>-name }{ <h>-id },{ cl_abap_char_utilities=>newline }{ c_components }end of t_{ <h>-name }{ <h>-id }.|.
      elseif <h>-table eq abap_true.
        <h>-final_type = |{ <h>-name } type tt_{ <h>-name }{ <h>-id }|.
        <h>-type_definition = |types: tt_{ <h>-name }{ <h>-id } type standard table of t_{ <h>-name }{ <h>-id } with default key.|.
      else.

        replace first occurrence of regex '\\TYPE-POOL=(.*)\\TYPE=' in <h>-absolute_type with ''.
        if sy-subrc eq 0.
          <h>-final_type = |{ <h>-name } type { <h>-absolute_type }|.
          continue.
        else.
          replace first occurrence of '\TYPE=' in <h>-absolute_type with ' '.
        endif.

        if <h>-type eq cl_abap_typedescr=>typekind_char.
          <h>-final_type = |{ <h>-name } type { <h>-absolute_type } lenght { <h>-lenght }|.
        elseif <h>-type eq cl_abap_typedescr=>typekind_packed.
          <h>-final_type = |{ <h>-name } type { <h>-absolute_type } lenght { <h>-lenght } decimals { <h>-decimals }|.
        elseif <h>-type eq cl_abap_typedescr=>typekind_num.
          <h>-final_type = |{ <h>-name } type { <h>-absolute_type } lenght { <h>-lenght }|.
        else.
          <h>-final_type = |{ <h>-name } type { <h>-absolute_type }|.
        endif.
      endif.
    endloop.

    loop at hierarchy assigning <h> group by ( parent = <h>-parent ).
      clear components.
      loop at group <h> assigning field-symbol(<g>).
        components = components && <g>-final_type && ',' && cl_abap_char_utilities=>newline.
      endloop.
      assign hierarchy[ absolute_type = <h>-parent ] to field-symbol(<parent>).
      if sy-subrc eq 0.
        replace all occurrences of c_components in <parent>-type_definition with components.
      endif.
    endloop.

    sort hierarchy by level descending.
    loop at hierarchy assigning <h> where structure eq abap_true
                                       or table eq abap_true.
      r_definition = r_definition && <h>-type_definition && cl_abap_char_utilities=>newline.
    endloop.
  endmethod.

  method get_types.
    r_types = create_types( ).
  endmethod.

endclass.
