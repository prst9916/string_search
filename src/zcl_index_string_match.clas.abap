class ZCL_INDEX_STRING_MATCH definition
  public
  final
  create public .

public section.

  interfaces ZIF_STRING_MATCH .

  types:
    Begin of y_result,
                tdname TYPE tdname,
                tdline TYPE tdline,
          End of y_result .
  types:
    YT_RESULT TYPE SORTED TABLE OF y_result WITH NON-UNIQUE KEY tdname .
  types:
    yt_zso10_file TYPE STANDARD TABLE OF zso10_file .

  class-data T_RESULT type YT_RESULT .
  class-data T_ZSO10_FILE type YT_ZSO10_FILE .
  class-data S_ZSO10_FILE type ZSO10_FILE .
  class-data S_RESULT type Y_RESULT .

  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_INDEX_STRING_MATCH IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
* Change to get the filenames
* loaded from the table zso10_file
* to the internal table
    Select tdname filetext
      from zso10_file
      into corresponding fields
      of table t_zso10_file.
    If sy-subrc EQ 0.
       SORT t_zso10_file BY tdname.
    Endif.
  endmethod.


  METHOD zif_string_match~match.
    TYPES : BEGIN OF ly_text,
              tline TYPE tline,
            END OF ly_text.
    DATA : lv_start           TYPE i,
           lv_end             TYPE i,
           ls_text            TYPE ly_text,
           lt_text            TYPE STANDARD TABLE OF ly_text,
           lt_results         TYPE match_result_tab,
           ls_results         LIKE LINE OF lt_results,
           ld_no_of_recs      TYPE i,
           ld_index           TYPE i,
           s_file_output      TYPE zso10_output,
           ld_space_exists(1) TYPE c.
    CONSTANTS : lc_small_letter(26) TYPE c VALUE 'abcdefghijklmnopqrstuvwxyz'.
    GET RUN TIME FIELD lv_start.
    DATA(lv_query) = |SELECT tdname,tdline FROM zso10_srchtext WHERE CONTAINS (tdline, '{ ip_string }')|.
    DATA(lr_sql) = NEW cl_sql_statement( ).
    DATA(lr_res) = lr_sql->execute_query( lv_query ).
    lr_res->set_param_table( REF #( t_result ) ).
    DATA(lv_recs) = lr_res->next_package( ).
    IF t_result[] IS NOT INITIAL.
      IF ip_perf = 'X'.
        DATA(ld_strlen) = strlen( ip_string ).
        CLEAR : ld_space_exists.
        DO ld_strlen TIMES.
          ld_index = sy-index - 1.
          IF ip_string+ld_index(1) CA ' '.
            ld_space_exists = 'X'.
          ELSE.
            CONTINUE.
          ENDIF.
        ENDDO.
        LOOP AT t_zso10_file
             INTO s_zso10_file.
          CLEAR : lt_text.
          REFRESH : lt_text.
          READ TABLE t_result
                INTO s_result
          WITH KEY tdname = s_zso10_file-tdname.
          IF sy-subrc EQ 0.
            LOOP AT t_result
                 INTO s_result
             FROM sy-tabix.
              IF s_result-tdname = s_zso10_file-tdname.
                ls_text-tline = s_result-tdline.
                APPEND ls_text TO lt_text.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
            IF ld_space_exists = 'X'.
              FIND ALL OCCURRENCES OF ip_string IN TABLE lt_text
              RESULTS lt_results.
              DESCRIBE TABLE lt_results LINES ld_no_of_recs.
            ELSE.
              FIND ALL OCCURRENCES OF ip_string IN TABLE lt_text
              RESULTS lt_results.
              CLEAR : ld_no_of_recs.
              LOOP AT lt_results INTO ls_results.
                READ TABLE lt_text INTO ls_text INDEX ls_results-line.
                IF sy-subrc EQ 0.
                  IF ls_results-offset GT 0.
                    DATA(ld_prev_char) = ls_results-offset - 1.
                    IF ls_text-tline+ld_prev_char(1) = ' '.
                    ELSE.
                      CONTINUE.
                    ENDIF.
                  ENDIF.
                  DATA(ld_offset_char) = ls_results-offset + ls_results-length.
                  IF ls_text-tline+ld_offset_char(1) CA '0123456789' OR
                     ls_text-tline+ld_offset_char(1) CA sy-abcde OR
                     ls_text-tline+ld_offset_char(1) CA lc_small_letter.
                    CONTINUE.
                  ENDIF.
                  ld_no_of_recs = ld_no_of_recs + 1.
                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
         ld_strlen = strlen( ip_string ).
            CLEAR : ld_space_exists.
            DO ld_strlen TIMES.
              ld_index = sy-index - 1.
              IF ip_string+ld_index(1) CA ' '.
                ld_space_exists = 'X'.
              ELSE.
                CONTINUE.
              ENDIF.
            ENDDO.
        LOOP AT t_zso10_file
             INTO s_zso10_file.
          CLEAR : lt_text,
                  ld_no_of_recs.
          REFRESH : lt_text.
          READ TABLE t_result
                INTO s_result
          WITH KEY tdname = s_zso10_file-tdname.
          IF sy-subrc EQ 0.
            LOOP AT t_result
                INTO s_result
            FROM sy-tabix.
              IF s_result-tdname = s_zso10_file-tdname.
                ls_text-tline = s_result-tdline.
                APPEND ls_text TO lt_text.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
            IF ld_space_exists = 'X'.
              FIND ALL OCCURRENCES OF ip_string IN TABLE lt_text
              RESULTS lt_results.
              DESCRIBE TABLE lt_results LINES ld_no_of_recs.
            ELSE.
              FIND ALL OCCURRENCES OF ip_string IN TABLE lt_text
              RESULTS lt_results.
              CLEAR : ld_no_of_recs.
              LOOP AT lt_results INTO ls_results.
                READ TABLE lt_text INTO ls_text INDEX ls_results-line.
                IF sy-subrc EQ 0.
                  IF ls_results-offset GT 0.
                    ld_prev_char = ls_results-offset - 1.
                    IF ls_text-tline+ld_prev_char(1) = ' '.
                    ELSE.
                      CONTINUE.
                    ENDIF.
                  ENDIF.
                  ld_offset_char = ls_results-offset + ls_results-length.
                  IF ls_text-tline+ld_offset_char(1) CA '0123456789' OR
                     ls_text-tline+ld_offset_char(1) CA sy-abcde OR
                     ls_text-tline+ld_offset_char(1) CA lc_small_letter.
                    CONTINUE.
                  ENDIF.
                  ld_no_of_recs = ld_no_of_recs + 1.
                ENDIF.
              ENDLOOP.
            ENDIF.
            s_file_output-name = s_zso10_file-tdname.
            s_file_output-filetext = s_zso10_file-filetext.
            s_file_output-numofrecs = ld_no_of_recs.
            APPEND s_file_output TO t_file_output.
          ELSE.
            s_file_output-name = s_zso10_file-tdname.
            s_file_output-filetext = s_zso10_file-filetext.
            s_file_output-numofrecs = ld_no_of_recs.
            APPEND s_file_output TO t_file_output.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
    GET RUN TIME FIELD lv_end.
    time_taken = lv_end - lv_start.
  ENDMETHOD.
ENDCLASS.
