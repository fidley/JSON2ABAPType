"! This is demo for converting JSON structure into
"! ABAP type
"! done by Lukasz Pegiel for http://abapblog.com

report zjson2abaptype.
data: ok_code type sy-ucomm.

class lcl_json_structure definition deferred.
class lcl_hlp definition.
  public section.
    types: begin of t_source,
             line type char255,
           end of t_source.
    types: tt_source type standard table of t_source with default key.
    data: converter     type ref to lcl_json_structure,
          results       type string,
          source_editor type ref to cl_gui_textedit,
          abap_editor   type ref to cl_wb_editor.
    methods: constructor.
    methods: create_source_editor.
    methods: convert.
    methods: pai importing value(i_okcode) type sy-ucomm,
      save_editor.
  private section.

    methods update_editor
      importing
        i_source type tt_source.
    methods call_editor
      changing
        c_source type tt_source.
    methods pretty_print_code
      changing
        c_source type tt_source.
    data: handler type ref to cl_wb_editor.
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
        i_level         type i
        i_keep_level    type abap_bool default abap_false
        i_keep_id       type abap_bool default abap_false.
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
      init,
      get_internal_types
        changing
          value(c_type) type t_hierarchy.
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
  set titlebar 'TITLE'.
  hlp->create_source_editor( ).
endmodule.
*&---------------------------------------------------------------------*
*&      Module  PAI  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module pai input.
  hlp->pai( ok_code ).
endmodule.

class lcl_hlp implementation.

  method constructor.
    converter = new #( ).
  endmethod.

  method create_source_editor.
    if source_editor is initial.
      source_editor = new #( parent =  new cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_left
                                                                     no_autodef_progid_dynnr = abap_true
                                                           extension = 500 ) ) .
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

      data(json_data) = /ui2/cl_json=>generate( json = cl_bcs_convert=>txt_to_string( it_soli   = source ) pretty_name = /ui2/cl_json=>pretty_mode-extended ).
      if json_data is initial.
        message s001(00) with 'Problem converting JSON.' display like 'E' ##MG_MISSING ##NO_TEXT.
      else.

        converter->build_structure(
          exporting
            i_data = json_data
          importing
            e_data = results
        ).

        data: target type standard table of t_source.
        split results at cl_abap_char_utilities=>newline into table target.
        pretty_print_code( changing c_source = target ).

        if abap_editor is initial.
          call_editor( changing c_source = target ).
        else.
          update_editor( target ).
        endif.
      endif.
    endif.
  endmethod.

  method update_editor.

    abap_editor->get_source_instance( importing source_object = data(source_object) ).
    source_object->set_source_tab( i_source ).
    abap_editor->visualize_source(
      exceptions
        initializing_error = 1
        others             = 2
    ).
    if sy-subrc eq 0.
      message s001(00) with 'JSON converted.' ##MG_MISSING ##NO_TEXT.
    endif.

  endmethod.



  method call_editor.

    call function 'EDITOR_APPLICATION'
      exporting
        application        = 'TT'
        name               = space
        new                = 'X'
        title_text         = 'JSON2ABAPType'
        callback_program   = sy-repid
        callback_usercom   = 'CALLBACK_USERCOMM'
        callback_set_pfkey = 'CALLBACK_SET_PFKEY'
      tables
        content            = c_source
      exceptions
        line               = 0
        linenumbers        = 0
        offset             = 0
        others             = 0.

  endmethod.



  method pretty_print_code.

    call function 'PRETTY_PRINTER'
      exporting
        inctoo             = abap_false  " X = Process Include Programs as Well
      tables
        ntext              = c_source " Table of Formatted Source Code
        otext              = c_source   " Table of Source Code Pending Editing
      exceptions
        enqueue_table_full = 0
        include_enqueued   = 0
        include_readerror  = 0
        include_writeerror = 0
        others             = 0.
  endmethod.



  method pai.
    clear sy-ucomm.
    case i_okcode.
      when 'BACK' or 'UP' or 'EXIT'.
        leave program.
      when 'CONVERT'.
        convert( ).
    endcase.
  endmethod.


  method save_editor.
    field-symbols:  <abap_editor>   type ref to cl_wb_tbeditor.
    assign  ('(SAPLS38E)ABAP_TBEDITOR') to <abap_editor>.
    abap_editor = <abap_editor>->abap_editor.
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

  endmethod.                                             "#EC CI_VALPAR

  method check_component.

    data level type i value 0.
    if i_keep_level eq abap_false.
      level = i_level + 1.
    else.
      level = i_level.
    endif.
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
            if i_keep_id eq abap_false.
              data(id) = get_id( ).
              append value #( level = level name = i_comp-name type = table_type->type_kind absolute_type = table_type->absolute_name parent = i_parent table  = abap_true  id = id ) to hierarchy.
            else.
              id = current_id.
            endif.

            field-symbols: <tab>  type standard table,
                           <test> type any.
            assign i_data->* to <tab>.
            try.
                assign <tab>[ 1 ] to <test>.
                if sy-subrc ne 0.
                  append initial line to <tab> assigning <test>.
                endif.
              catch cx_root.
                append initial line to <tab> assigning <test>.
            endtry.

            cl_abap_structdescr=>describe_by_data_ref( exporting p_data_ref = <test>
                                                       receiving p_descr_ref = data(table_line_ref)
                                                       exceptions others = 1 ).
            if sy-subrc ne 0.
              data: table_of_strings type standard table of string.
              check_component(
                exporting
                  i_comp   = i_comp
                  i_data   = ref #( table_of_strings )
                  i_parent = i_parent
                  i_level  = i_level
                  i_keep_level = abap_true
                  i_keep_id  = abap_true
              ).
              return.
            endif.

            data(table_line_type) = cast cl_abap_structdescr( table_line_ref  ).
            append value #( level = level name = i_comp-name type = table_line_type->type_kind absolute_type = table_line_type->absolute_name parent = table_type->absolute_name structure  = abap_true  id = id ) to hierarchy.

            check_object( i_parent = table_type->absolute_name
                          i_data  = <test>
                          i_abap_type = table_line_type
                          i_level     = level
                        ).

          catch cx_root.
            "May be a table of strings or integers.
            if table_type is not initial.
              try.
                  if table_type->type_kind eq 'h'.
                    data(table_line_as_el_type) = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data_ref( p_data_ref = <test> )  ).
                    add 1 to level.
                    append value #( level = level name = i_comp-name type = table_line_as_el_type->type_kind absolute_type = table_line_as_el_type->absolute_name parent = table_line_as_el_type->absolute_name structure  = abap_true  id = id ) to hierarchy.
                  endif.
                catch cx_root.
                  try.
                      if table_type->type_kind eq 'h'.
                        table_line_as_el_type = cast cl_abap_elemdescr( cl_abap_elemdescr=>describe_by_data( p_data = <test> )  ).
                        add 1 to level.
                    append value #( level = level name = i_comp-name type = table_line_as_el_type->type_kind absolute_type = table_line_as_el_type->absolute_name parent = table_line_as_el_type->absolute_name structure  = abap_true  id = id ) to hierarchy.
                      endif.
                    catch cx_root.

                      data(other_type) =  cl_abap_typedescr=>describe_by_data_ref( p_data_ref  = i_data ) .
                      append value #( level = level name = i_comp-name type = other_type->type_kind lenght = other_type->length decimals = other_type->decimals absolute_type = other_type->absolute_name parent = i_parent ) to hierarchy.
                  endtry.

              endtry.
            else.
              other_type =  cl_abap_typedescr=>describe_by_data_ref( p_data_ref  = i_data ) .
              append value #( level = level name = i_comp-name type = other_type->type_kind lenght = other_type->length decimals = other_type->decimals absolute_type = other_type->absolute_name parent = i_parent ) to hierarchy.
            endif.
        endtry.
    endtry.
  endmethod.                                             "#EC CI_VALPAR

  method create_types.
    data: components type string.
    loop at hierarchy assigning field-symbol(<h>).
      if <h>-structure eq abap_true and <h>-type ne 'g'.
        <h>-final_type = |{ <h>-name } type t_{ <h>-name }{ <h>-id }| ##NO_TEXT.
        <h>-type_definition = |types: begin of t_{ <h>-name }{ <h>-id },{ cl_abap_char_utilities=>newline }{ c_components }end of t_{ <h>-name }{ <h>-id }.| ##NO_TEXT.
      elseif <h>-structure eq abap_true.
        <h>-final_type = |{ <h>-name } type t_{ <h>-name }{ <h>-id }| ##NO_TEXT.
        get_internal_types( changing  c_type = <h> ).
        <h>-type_definition = |types: t_{ <h>-name }{ <h>-id } type { <h>-absolute_type }.| ##NO_TEXT.
      elseif <h>-table eq abap_true.
        <h>-final_type = |{ <h>-name } type tt_{ <h>-name }{ <h>-id }| ##NO_TEXT.
        <h>-type_definition = |types: tt_{ <h>-name }{ <h>-id } type standard table of t_{ <h>-name }{ <h>-id } with default key.| ##NO_TEXT.
      else.

        get_internal_types( changing  c_type = <h> ).
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

    sort hierarchy by level descending structure descending.
    loop at hierarchy assigning <h> where structure eq abap_true
                                       or table eq abap_true.
      r_definition = r_definition && <h>-type_definition && cl_abap_char_utilities=>newline.
    endloop.
  endmethod.

  method get_types.
    r_types = create_types( ).
  endmethod.

  method get_internal_types.
    replace first occurrence of regex '\\TYPE-POOL=(.*)\\TYPE=' in c_type-absolute_type with ''.
    if sy-subrc eq 0.
      c_type-final_type = |{ c_type-name } type { c_type-absolute_type }|.
      return.
    else.
      replace first occurrence of '\TYPE=' in c_type-absolute_type with ' '.
    endif.

    if c_type-type eq cl_abap_typedescr=>typekind_char.
      c_type-final_type = |{ c_type-name } type { c_type-absolute_type } lenght { c_type-lenght }|.
    elseif c_type-type eq cl_abap_typedescr=>typekind_packed.
      c_type-final_type = |{ c_type-name } type { c_type-absolute_type } lenght { c_type-lenght } decimals { c_type-decimals }|.
    elseif c_type-type eq cl_abap_typedescr=>typekind_num.
      c_type-final_type = |{ c_type-name } type { c_type-absolute_type } lenght { c_type-lenght }|.
    else.
      c_type-final_type = |{ c_type-name } type { c_type-absolute_type }|.
    endif.
  endmethod.                                             "#EC CI_VALPAR
endclass.

form callback_usercomm.
  hlp->pai( sy-ucomm ).
endform.


form callback_set_pfkey.
  set pf-status 'STATUS_0100'.
  set titlebar 'TITLE'.
  hlp->save_editor( ).
endform.
