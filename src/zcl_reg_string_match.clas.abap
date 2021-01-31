class ZCL_REG_STRING_MATCH definition
  public
  final
  create public .

public section.

  interfaces ZIF_STRING_MATCH .

  types:
    yt_zso10_table TYPE STANDARD TABLE OF zso10_srchtext .
  types:
    Begin of y_zso10_tdname,
      tdname TYPE tdname,
    End of y_zso10_tdname .
  types:
    yt_zso10_tdname TYPE STANDARD TABLE OF y_zso10_tdname .
  types:
    yt_zso10_file TYPE STANDARD TABLE OF zso10_file .

  class-data T_ZSO10_TABLE type YT_ZSO10_TABLE .
  class-data S_ZSO10_TABLE type ZSO10_SRCHTEXT .
  class-data T_ZSO10_FILE type YT_ZSO10_FILE .
  class-data S_ZSO10_FILE type ZSO10_FILE .

  class-methods CLASS_CONSTRUCTOR .
protected section.
private section.
ENDCLASS.



CLASS ZCL_REG_STRING_MATCH IMPLEMENTATION.


  method CLASS_CONSTRUCTOR.
    Select tdname tdline
      from zso10_srchtext
      into corresponding fields
      of table t_zso10_table.
    If sy-subrc EQ 0.
       SORT t_zso10_table BY tdname.
    Endif.
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
  Data : lt_results          TYPE match_result_tab,
         ls_result           LIKE LINE OF lt_results,
         lv_start            TYPE i,
         lv_end              TYPE i,
         ls_text             TYPE ly_text,
         lt_text             TYPE STANDARD TABLE OF ly_text,
         ld_no_of_recs       TYPE i,
         ld_strlen           TYPE i,
         ld_index            TYPE i,
         ld_space_exists(1)  TYPE C,
         s_file_output       TYPE zso10_output.
  GET RUN TIME FIELD lv_start.
  CLEAR : lt_results.
  REFRESH : lt_results.
  If ip_perf = 'X'.
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
    READ TABLE t_zso10_table
           INTO s_zso10_table
       WITH KEY tdname = s_zso10_file-tdname
    BINARY SEARCH.
    If sy-subrc EQ 0.
       Clear : lt_text.
       Refresh : lt_text.
       LOOP AT t_zso10_table
            INTO s_zso10_table
       FROM sy-tabix.
       If s_zso10_table-tdname = s_zso10_file-tdname.
          ls_text-tline = s_zso10_table-tdline.
          APPEND ls_text TO lt_text.
       Else.
          Exit.
       Endif.
       ENDLOOP.
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
          s_file_output-name = s_zso10_file-tdname.
          s_file_output-filetext = s_zso10_file-filetext.
          s_file_output-numofrecs = ld_no_of_recs.
           APPEND s_file_output TO t_file_output.
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
          s_file_output-name = s_zso10_file-tdname.
          s_file_output-filetext = s_zso10_file-filetext.
          s_file_output-numofrecs = ld_no_of_recs.
          APPEND s_file_output TO t_file_output.
       Endif.
    Endif.
   ENDLOOP.
   Endif.
   GET RUN TIME FIELD lv_end.
   time_taken = lv_end - lv_start.
  endmethod.
ENDCLASS.
