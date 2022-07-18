interface ZIF_GTT_POF_AE_TYPES
  public .


  types TS_TRACKINGHEADER type /SAPTRX/BAPI_EVM_HEADER .
  types:
    tt_trackingheader TYPE STANDARD TABLE OF ts_trackingheader .
  types TS_TRACKLOCATION type /SAPTRX/BAPI_EVM_LOCATIONID .
  types:
    tt_tracklocation TYPE STANDARD TABLE OF ts_tracklocation .
  types TS_TRACKREFERENCES type /SAPTRX/BAPI_EVM_REFERENCE .
  types:
    tt_trackreferences TYPE STANDARD TABLE OF ts_trackreferences .
  types TS_TRACKPARAMETERS type /SAPTRX/BAPI_EVM_PARAMETERS .
  types:
    tt_trackparameters TYPE STANDARD TABLE OF ts_trackparameters .
endinterface.
