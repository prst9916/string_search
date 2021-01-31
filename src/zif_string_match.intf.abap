interface ZIF_STRING_MATCH
  public .


  methods MATCH
    importing
      !IP_STRING type ZSEARCH_STRING
      !IP_PERF type CHAR1
    exporting
      !T_FILE_OUTPUT type ZSO10_TT
      !TIME_TAKEN type I .
endinterface.
