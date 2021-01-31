*&---------------------------------------------------------------------*
*& Report ZSTRING_SEARCH
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSTRING_SEARCH.

TYPES : BEGIN OF y_analysis,
              result TYPE string,
              time   TYPE i,
        END OF y_analysis.

Data : d_intrf TYPE REF TO ZIF_STRING_MATCH,
       d_ucomm TYPE sy-ucomm,
       d_no_of_recs TYPE i,
       d_time_taken TYPE i,
       d_regtotsrch_time TYPE i,
       d_regextotsrch_time TYPE i,
       d_indextotsrch_time TYPE i,
       d_random_string     TYPE zsearch_string,
       t_file_output TYPE zso10_tt,
       s_file_output TYPE zso10_output,
       t_analysis    TYPE STANDARD TABLE OF y_analysis,
       s_analysis    TYPE y_analysis.

PARAMETERS : p_sterm TYPE zsearch_string MODIF ID stm.

SELECTION-SCREEN BEGIN OF BLOCK blck1 WITH FRAME TITLE TEXT-001.
  PARAMETERS : p_rad1 RADIOBUTTON GROUP rad1,
               p_rad2 RADIOBUTTON GROUP rad1,
               p_rad3 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF BLOCK blck1.

PARAMETERS : p_perf  AS CHECKBOX USER-COMMAND perf,
             p_nchar TYPE i MODIF ID nof.

AT SELECTION-SCREEN.
  d_ucomm = sy-ucomm.

AT SELECTION-SCREEN OUTPUT.
  IF d_ucomm = 'PERF' AND p_perf = 'X'.
    LOOP AT SCREEN.
      IF screen-group1 = 'NOF'.
        screen-input = 1.
        screen-active = 1.
        screen-invisible = 0.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'STM'.
        screen-input = 0.
        screen-output = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-group1 = 'NOF'.
        screen-input = 0.
        screen-active = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF.
      IF screen-group1 = 'STM'.
        screen-input = 1.
        screen-output = 1.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

START-OF-SELECTION.
  IF p_perf NE 'X' AND p_sterm IS INITIAL.
    MESSAGE TEXT-001 TYPE 'E'.
  ENDIF.
  PERFORM remove_double_quotes.
  WRITE: TEXT-019.
  IF p_perf = 'X'.
     Clear : d_regtotsrch_time,
             d_regextotsrch_time,
             d_indextotsrch_time.
     DO 2000000 TIMES.
      PERFORM generate_random_word USING p_nchar
                                   CHANGING d_random_string.
      Clear : d_time_taken.
      d_intrf = zcl_fc_string_match=>get_string_match( i_match_method = 'REG' ).
      d_intrf->match(
        EXPORTING
          ip_string     =   d_random_string   " String
          ip_perf       =   'X'               " Single-Character Flag
       IMPORTING
          time_taken    =  d_time_taken
      ).
      d_regtotsrch_time = d_time_taken + d_regtotsrch_time.
      Clear : d_time_taken.
      d_intrf = zcl_fc_string_match=>get_string_match( i_match_method = 'REGEX' ).
      d_intrf->match(
        EXPORTING
          ip_string     =  d_random_string   " String
          ip_perf       =  'X'               " Single-Character Flag
        IMPORTING
          time_taken    = d_time_taken
      ).
      d_regextotsrch_time = d_time_taken + d_regextotsrch_time.
      Clear : d_time_taken.
      d_intrf = zcl_fc_string_match=>get_string_match( i_match_method = 'INDEX' ).
      d_intrf->match(
        EXPORTING
          ip_string     =  d_random_string " String
          ip_perf       =  'X'             " Single-Character Flag
        IMPORTING
          time_taken    = d_time_taken
      ).
      d_indextotsrch_time = d_time_taken + d_indextotsrch_time.
     ENDDO.
     PERFORM build_analysis_data.
     PERFORM display_analysis_data.
  ELSE.
    IF p_rad1 = 'X'.
       Clear : d_time_taken,
               t_file_output.
       Refresh : t_file_output.
       d_intrf = zcl_fc_string_match=>get_string_match( i_match_method = 'REG' ).
       d_intrf->match(
         EXPORTING
           ip_string     =  p_sterm    " String
           ip_perf       =  ' '        " Single-Character Flag
        IMPORTING
           t_file_output =  t_file_output   " SO10 Output
           time_taken    =  d_time_taken
       ).
       PERFORM display_data.
    ELSEIF p_rad2 = 'X'.
       Clear : d_time_taken,
               t_file_output.
       Refresh : t_file_output.
       d_intrf = zcl_fc_string_match=>get_string_match( i_match_method = 'REGEX' ).
       d_intrf->match(
         EXPORTING
           ip_string     = p_sterm    " String
           ip_perf       = ' '        " Single-Character Flag
         IMPORTING
           t_file_output = t_file_output  " SO10 Output
           time_taken    = d_time_taken
       ).
       PERFORM display_data.
    ELSEIF p_rad3 = 'X'.
       d_intrf = zcl_fc_string_match=>get_string_match( i_match_method = 'INDEX' ).
       d_intrf->match(
         EXPORTING
           ip_string     = p_sterm    " String
           ip_perf       = ' '        " Single-Character Flag
         IMPORTING
           t_file_output = t_file_output  " SO10 Output
           time_taken    = d_time_taken
       ).
       PERFORM display_data.
    ENDIF.
  ENDIF.
*&---------------------------------------------------------------------*
*& Form generate_random_word
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> UP_NCHAR
*&      <-- CP_RANDOM_STRING
*&---------------------------------------------------------------------*
FORM generate_random_word  USING    up_nchar
                           CHANGING cp_random_string.
  DATA : ld_random_string TYPE string.
  CALL FUNCTION 'GENERAL_GET_RANDOM_STRING'
    EXPORTING
      number_chars  = up_nchar
    IMPORTING
      random_string = ld_random_string.
  cp_random_string = ld_random_string.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form build_analysis_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM build_analysis_data .
  CLEAR : s_analysis,
          t_analysis.
  REFRESH : t_analysis.
  s_analysis-result = TEXT-008.
  s_analysis-time = d_regtotsrch_time.
  APPEND s_analysis TO t_analysis.
  s_analysis-result = TEXT-009.
  s_analysis-time = d_regextotsrch_time.
  APPEND s_analysis TO t_analysis.
  s_analysis-result = TEXT-010.
  s_analysis-time = d_indextotsrch_time.
  APPEND s_analysis TO t_analysis.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_analysis_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_analysis_data.
  LOOP AT t_analysis INTO s_analysis.
    WRITE:/ s_analysis-result UNDER TEXT-011,
            s_analysis-time UNDER TEXT-012.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form remove_double_quotes
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM remove_double_quotes.
If p_sterm CA '""'.
   TRANSLATE p_sterm USING '" '.
   SHIFT p_sterm LEFT DELETING LEADING space.
Endif.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .
LOOP AT t_file_output
     INTO s_file_output.
Write:/5 text-020,
      35 s_file_output-filetext,
      60 s_file_output-numofrecs.
ENDLOOP.
Write:/5 text-012,
      60 d_time_taken.
ENDFORM.
