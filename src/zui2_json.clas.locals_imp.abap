*"* local class implementation for public class
*"* use this source file for the implementation part of
*"* local helper classes

DEFINE escape_json_inplace.
*  replace all occurrences of regex `[\\"]` in &1 with `\\$0`. <-- this is slower than 2 plain replaces
  replace all occurrences of `\` in &1 with `\\`.
  replace all occurrences of `"` in &1 with `\"`.
END-OF-DEFINITION.

DEFINE escape_json.
  move &1 to &2.
  escape_json_inplace &2.
END-OF-DEFINITION.

DEFINE is_compressable.
  IF mv_extended IS INITIAL.
    &3 = abap_true.
  ELSE.
    &3 = is_compressable( type_descr = &1 name = &2 ).
  ENDIF.
END-OF-DEFINITION.

DEFINE dump_type.
  IF mv_extended IS INITIAL.
    dump_type_int &1 &2 &3.
  ELSE.
    &3 = dump_type( data = &1 type_descr = &2 ).
  ENDIF.
END-OF-DEFINITION.

DEFINE dump_type_int.

  case &2->type_kind.
    when cl_abap_typedescr=>typekind_float or cl_abap_typedescr=>typekind_int or cl_abap_typedescr=>typekind_int1 or
         cl_abap_typedescr=>typekind_int2 or cl_abap_typedescr=>typekind_packed or `8`. " TYPEKIND_INT8 -> '8' only from 7.40.
      if &2->type_kind eq cl_abap_typedescr=>typekind_packed and mv_ts_as_iso8601 eq c_bool-true and &2->absolute_name cp `\TYPE=TIMESTAMP*`.
        if &1 is initial.
          &3 = `""`.
        else.
          move &1 to &3.
          if &2->absolute_name eq `\TYPE=TIMESTAMP`.
            concatenate `"` &3(4) `-` &3+4(2) `-` &3+6(2) `T` &3+8(2) `:` &3+10(2) `:` &3+12(2) `.0000000Z"`  into &3.
          elseif &2->absolute_name eq `\TYPE=TIMESTAMPL`.
            concatenate `"` &3(4) `-` &3+4(2) `-` &3+6(2) `T` &3+8(2) `:` &3+10(2) `:` &3+12(2) `.` &3+15(7) `Z"`  into &3.
          endif.
        endif.
      elseif &1 is initial.
        &3 = `0`.
      else.
        move &1 to &3.
        if &1 lt 0.
          if &2->type_kind <> cl_abap_typedescr=>typekind_float. "float: sign is already at the beginning
            shift &3 right circular.
          endif.
        else.
          condense &3.
        endif.
      endif.
    when cl_abap_typedescr=>typekind_num.
      if mv_numc_as_string eq abap_true.
        if &1 is initial.
          &3 = `""`.
        else.
          concatenate `"` &1 `"` into &3.
        endif.
      else.
        if &1 is initial.
        &3 = `0`.
      else.
        move &1 to &3.
        shift &3 left deleting leading ` 0`.
        endif.
      endif.
    when cl_abap_typedescr=>typekind_string or cl_abap_typedescr=>typekind_csequence or cl_abap_typedescr=>typekind_clike.
      if &1 is initial.
        &3 = `""`.
      elseif &2->absolute_name eq mc_json_type.
        &3 = &1.
      else.
        escape_json &1 &3.
        concatenate `"` &3 `"` into &3.
      endif.
    when cl_abap_typedescr=>typekind_xstring or cl_abap_typedescr=>typekind_hex.
      if &1 is initial.
        &3 = `""`.
      else.
        &3 = xstring_to_string( &1 ).
        escape_json_inplace &3.
        concatenate `"` &3 `"` into &3.
      endif.
    when cl_abap_typedescr=>typekind_char.
      if &2->output_length eq 1 and mc_bool_types cs &2->absolute_name.
        if &1 eq c_bool-true.
          &3 = `true`.                                      "#EC NOTEXT
        elseif mc_bool_3state cs &2->absolute_name and &1 is initial.
          &3 = `null`.                                      "#EC NOTEXT
        else.
          &3 = `false`.                                     "#EC NOTEXT
        endif.
      else.
        escape_json &1 &3.
        concatenate `"` &3 `"` into &3.
      endif.
    when cl_abap_typedescr=>typekind_date.
      concatenate `"` &1(4) `-` &1+4(2) `-` &1+6(2) `"` into &3.
    when cl_abap_typedescr=>typekind_time.
      concatenate `"` &1(2) `:` &1+2(2) `:` &1+4(2) `"` into &3.
    when others.
      if &1 is initial.
        &3 = `null`.                                        "#EC NOTEXT
      else.
        move &1 to &3.
      endif.
  endcase.

END-OF-DEFINITION.

DEFINE format_name.
  case &2.
    when pretty_mode-camel_case.
      &3 = pretty_name( &1 ).
    when pretty_mode-extended.
      &3 = pretty_name_ex( &1 ).
    when pretty_mode-user_low_case.
      read table mt_name_mappings with table key abap = &1 assigning <cache>.
      if sy-subrc is initial.
        &3 = <cache>-json.
      else.
        &3 = &1.
        translate &3 to lower case.                       "#EC SYNTCHAR
      endif.
    when pretty_mode-user.
      read table mt_name_mappings with table key abap = &1 assigning <cache>.
      if sy-subrc is initial.
        &3 = <cache>-json.
      else.
        &3 = &1.
      endif.
    when pretty_mode-low_case.
      &3 = &1.
      translate &3 to lower case.                         "#EC SYNTCHAR
    when others.
      &3 = &1.
  endcase.
END-OF-DEFINITION.

DEFINE throw_error.
  raise exception type cx_sy_move_cast_error.
END-OF-DEFINITION.

DEFINE while_offset_cs.
*  >= 7.02 alternative
*  pos = find_any_not_of( val = json sub = &1 off = offset ).
*  if pos eq -1. offset = length.
*  else. offset = pos. endif.

* < 7.02
  while offset < length.
    find first occurrence of json+offset(1) in &1.
    if sy-subrc is not initial.
      exit.
    endif.
    offset = offset + 1.
  endwhile.
* < 7.02

END-OF-DEFINITION.


DEFINE eat_white.
  while_offset_cs sv_white_space.
END-OF-DEFINITION.

DEFINE eat_string.
  if json+offset(1) eq `"`.
    mark   = offset + 1.
    offset = mark.
    do.
      find first occurrence of `"` in section offset offset of json match offset pos.
      if sy-subrc is not initial.
        throw_error.
      endif.
        offset = pos.
        pos = pos - 1.
        " if escaped search further
        while pos ge 0 and json+pos(1) eq `\`.
          pos = pos - 1.
        endwhile.
      match = ( offset - pos ) mod 2.
      if match ne 0.
        exit.
      endif.
      offset = offset + 1.
    enddo.
    match = offset - mark.
    &1 = json+mark(match).
    " unescaped singe characters, e.g \\, \", \/ etc
    replace all occurrences of regex `\\(.)` in &1 with `$1`.
    offset = offset + 1.
  else.
    throw_error.
  endif.
END-OF-DEFINITION.

DEFINE eat_number.
  mark   = offset.
  while_offset_cs `0123456789+-eE.`.                        "#EC NOTEXT
  match = offset - mark.
  &1 = json+mark(match).
END-OF-DEFINITION.

DEFINE eat_bool.
  mark   = offset.
  while_offset_cs `aeflnrstu`.                              "#EC NOTEXT
  match = offset - mark.
  if json+mark(match) eq `true`.                            "#EC NOTEXT
    &1 = c_bool-true.
  elseif json+mark(match) eq `false`.                       "#EC NOTEXT
    if type_descr is bound and mc_bool_3state cs type_descr->absolute_name.
      &1 = c_tribool-false.
    else.
      &1 = c_bool-false.
    endif.
  elseif json+mark(match) eq `null`.                        "#EC NOTEXT
    clear &1.
  endif.
END-OF-DEFINITION.

DEFINE eat_char.
  if offset < length and json+offset(1) eq &1.
    offset = offset + 1.
  else.
    throw_error.
  endif.
END-OF-DEFINITION.
