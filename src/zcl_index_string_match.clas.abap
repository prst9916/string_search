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
    Select tdname filetext
      from zso10_file
      into corresponding fields
      of table t_zso10_file.
    If sy-subrc EQ 0.
       SORT t_zso10_file BY tdname.
    Endif.
  endmethod.


  method ZIF_STRING_MATCH~MATCH.
 Types : Begin of ly_text,
               tline TYPE tline,
          End of ly_text.
 Data : lv_start TYPE i,
        lv_end TYPE i,
        ls_text             TYPE ly_text,
        lt_text             TYPE STANDARD TABLE OF ly_text,
        lt_results          TYPE match_result_tab,
        ls_result           LIKE LINE OF lt_results,
        ld_no_of_recs       TYPE i,
        ld_index            TYPE i,
        s_file_output       TYPE zso10_output,
        ld_space_exists(1)  TYPE C.
  GET RUN TIME FIELD lv_start.
  DATA(lv_query) = |SELECT tdname,tdline FROM zso10_srchtext WHERE CONTAINS (tdline, '{ ip_string }')|.
  DATA(lr_sql) = NEW cl_sql_statement( ).
  DATA(lr_res) = lr_sql->execute_query( lv_query ).
  lr_res->set_param_table( REF #( t_result ) ).
  Data(lv_recs) = lr_res->next_package( ).
  If ip_perf = 'X'.
     Data(ld_strlen) = strlen( ip_string ).
     Clear : ld_space_exists.
     DO ld_strlen TIMES.
         ld_index = sy-index - 1.
         If ip_string+ld_index(1) CA ' '.
             ld_space_exists = 'X'.
          Else.
             CONTINUE.
          Endif.
       ENDDO.
       If ld_space_exists = 'X'.
          FIND ALL OCCURRENCES OF ip_string IN TABLE lt_text
          RESULTS lt_results.
          DESCRIBE TABLE lt_results LINES ld_no_of_recs.
       Else.
          FIND ALL OCCURRENCES OF ip_string IN TABLE lt_text
          RESULTS lt_results.
          Clear : ld_no_of_recs.
          SORT lt_results BY line.
          DELETE ADJACENT DUPLICATES FROM lt_results COMPARING line.
          LOOP AT lt_results INTO data(ls_results).
               READ TABLE lt_text INTO ls_text INDEX ls_results-line.
               If sy-subrc EQ 0.
                  SPLIT ls_text AT ' ' INTO TABLE Data(lt_match_string).
                  LOOP AT lt_match_string INTO Data(ls_match_string).
                       If ls_match_string = ip_string.
                          ld_no_of_recs = ld_no_of_recs + 1.
                       Endif.
                  ENDLOOP.
                  Clear : lt_match_string.
                  Refresh : lt_match_string.
               Endif.
          ENDLOOP.
       Endif.
  Else.
  LOOP AT t_zso10_file
       INTO s_zso10_file.
  Clear : lt_text.
  Refresh : lt_text.
  READ TABLE t_result
        INTO s_result
  WITH KEY tdname = s_zso10_file-tdname.
  If sy-subrc EQ 0.
      LOOP AT t_result
          INTO s_result
      FROM sy-tabix.
      IF s_result-tdname = s_zso10_file-tdname.
         ls_text-tline = s_result-tdline.
         APPEND ls_text TO lt_text.
       ELSE.
          Exit.
       ENDIF.
       ENDLOOP.
       FIND ALL OCCURRENCES OF ip_string IN TABLE lt_text
       RESULTS lt_results.
       ld_strlen = strlen( ip_string ).
       Clear : ld_space_exists.
       DO ld_strlen TIMES.
          ld_index = sy-index - 1.
          If ip_string+ld_index(1) CA ' '.
             ld_space_exists = 'X'.
          Else.
             CONTINUE.
          Endif.
       ENDDO.
       If ld_space_exists = 'X'.
          FIND ALL OCCURRENCES OF ip_string IN TABLE lt_text
          RESULTS lt_results.
          DESCRIBE TABLE lt_results LINES ld_no_of_recs.
       Else.
          FIND ALL OCCURRENCES OF ip_string IN TABLE lt_text
          RESULTS lt_results.
          Clear : ld_no_of_recs.
          SORT lt_results BY line.
          DELETE ADJACENT DUPLICATES FROM lt_results COMPARING line.
          LOOP AT lt_results INTO ls_results.
               READ TABLE lt_text INTO ls_text INDEX ls_results-line.
               If sy-subrc EQ 0.
                  SPLIT ls_text AT ' ' INTO TABLE lt_match_string.
                  LOOP AT lt_match_string INTO ls_match_string.
                       If ls_match_string = ip_string.
                          ld_no_of_recs = ld_no_of_recs + 1.
                       Endif.
                  ENDLOOP.
                  Clear : lt_match_string.
                  Refresh : lt_match_string.
               Endif.
          ENDLOOP.
       Endif.
       s_file_output-name = s_zso10_file-tdname.
       s_file_output-filetext = s_zso10_file-filetext.
       s_file_output-numofrecs = ld_no_of_recs.
       APPEND s_file_output TO t_file_output.
  Endif.
  ENDLOOP.
  Endif.
  GET RUN TIME FIELD lv_end.
  time_taken = lv_end - lv_start.
  endmethod.
ENDCLASS.
