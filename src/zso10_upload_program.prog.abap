*&---------------------------------------------------------------------*
*& Report ZSO10_UPLOAD_PROGRAM
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZSO10_UPLOAD_PROGRAM.

TABLES : stxh.

DATA : it_tlines1 TYPE STANDARD TABLE OF tline,
       so_tlines1 TYPE tline,
       t_zso10_text TYPE STANDARD TABLE OF zso10_text,
       s_zso10_text TYPE zso10_text.
SELECT-OPTIONS : so_tdnm FOR stxh-tdname NO INTERVALS.

START-OF-SELECTION.
LOOP AT so_tdnm.
 refresh it_tlines1.
 CALL FUNCTION 'READ_TEXT'
      EXPORTING
       CLIENT                         = SY-MANDT
        ID                            = 'ST'
        LANGUAGE                      = 'E'
        NAME                          = so_tdnm-low
        OBJECT                        = 'TEXT'
      TABLES
        LINES                         = it_tlines1
     EXCEPTIONS
       ID                            = 1
       LANGUAGE                      = 2
       NAME                          = 3
       NOT_FOUND                     = 4
       OBJECT                        = 5
       REFERENCE_CHECK               = 6
       WRONG_ACCESS_TO_ARCHIVE       = 7
       OTHERS                        = 8
              .
    IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ELSE.
       DELETE it_tlines1 WHERE tdline IS INITIAL.
       LOOP AT it_tlines1
            INTO so_tlines1.
            s_zso10_text-tdname = so_tdnm-low.
            s_zso10_text-tdline = so_tlines1-tdline.
            APPEND s_zso10_text TO t_zso10_text.
       ENDLOOP.
    ENDIF.
 ENDLOOP.

 INSERT zso10_srchtext FROM table t_zso10_text.
 If sy-subrc EQ 0.
    COMMIT WORK.
 Endif.
