interface ZIF_GTT_POF_EF_TYPES
  public .


  types TS_CONTROL_DATA type /SAPTRX/CONTROL_DATA .
  types:
    tt_control_data TYPE STANDARD TABLE OF ts_control_data .
  types TS_TRACK_ID_DATA type /SAPTRX/TRACK_ID_DATA .
  types:
    tt_track_id_data TYPE STANDARD TABLE OF ts_track_id_data .
  types TS_STRUCDATADEF type /SAPTRX/STRUCDATADEF .
  types:
    tt_strucdatadef TYPE STANDARD TABLE OF ts_strucdatadef .
  types TS_EXPEVENTDATA type /SAPTRX/EXP_EVENTS .
  types:
    tt_expeventdata TYPE STANDARD TABLE OF ts_expeventdata .
  types TS_MEASRMNTDATA type /SAPTRX/MEASR_DATA .
  types:
    tt_measrmntdata TYPE STANDARD TABLE OF ts_measrmntdata .
  types TS_INFODATA type /SAPTRX/INFO_DATA .
  types:
    tt_infodata TYPE STANDARD TABLE OF ts_infodata .
  types:
    BEGIN OF ts_definition,
           maintab   TYPE /saptrx/strucdatadef,
           mastertab TYPE /saptrx/strucdatadef,
         END OF ts_definition .
  types TV_PARAMETER_ID type I .
  types TV_PARAMETER_VALUE type CHAR50 .
  types TV_CONDITION type SY-BINPT .
  types TV_FIELD_NAME type CHAR20 .
  types:
    tt_field_name TYPE STANDARD TABLE OF tv_field_name
                              WITH EMPTY KEY .
  types TV_CURRENCY_AMNT type BAPICURR_D .
endinterface.
