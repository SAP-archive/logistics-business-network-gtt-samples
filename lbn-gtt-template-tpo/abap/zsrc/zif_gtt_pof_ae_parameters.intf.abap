interface ZIF_GTT_POF_AE_PARAMETERS
  public .


  methods GET_APPSYS
    returning
      value(RV_APPSYS) type /SAPTRX/APPLSYSTEM .
  methods GET_EVENT_TYPE
    returning
      value(RS_EVENT_TYPE) type /SAPTRX/EVTYPES .
  methods GET_APPL_TABLE
    importing
      !IV_TABLEDEF type CLIKE
    returning
      value(RR_DATA) type ref to DATA
    raising
      CX_UDM_MESSAGE .
  methods GET_EVENTS
    returning
      value(RR_DATA) type ref to DATA
    raising
      CX_UDM_MESSAGE .
endinterface.
