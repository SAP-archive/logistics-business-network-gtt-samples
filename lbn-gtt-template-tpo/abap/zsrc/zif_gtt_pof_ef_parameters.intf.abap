interface ZIF_GTT_POF_EF_PARAMETERS
  public .


  methods GET_APPSYS
    returning
      value(RV_APPSYS) type /SAPTRX/APPLSYSTEM .
  methods GET_APP_OBJ_TYPES
    returning
      value(RS_APP_OBJ_TYPES) type /SAPTRX/AOTYPES .
  methods GET_APP_OBJECTS
    returning
      value(RR_DATA) type ref to DATA .
  methods GET_APPL_TABLE
    importing
      !IV_TABLEDEF type CLIKE
    returning
      value(RR_DATA) type ref to DATA
    raising
      CX_UDM_MESSAGE .
endinterface.
