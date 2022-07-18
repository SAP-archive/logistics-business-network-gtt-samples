interface ZIF_GTT_POF_APP_TYPES
  public .


  types TS_EKKO type /SAPTRX/MM_PO_HDR .
  types:
    tt_ekko TYPE STANDARD TABLE OF ts_ekko .
  types TS_UEKPO type UEKPO .
  types:
    tt_uekpo TYPE STANDARD TABLE OF ts_uekpo .
  types TS_UEKET type UEKET .
  types:
    tt_ueket TYPE STANDARD TABLE OF ts_ueket .
  types TS_UEKES type UEKES .
  types:
    tt_uekes TYPE STANDARD TABLE OF ts_uekes .
  types TS_LIKPVB type LIKP .
  types:
    tt_likpvb TYPE STANDARD TABLE OF ts_likpvb .
  types TS_LIPSVB type LIPSVB .
  types:
    tt_lipsvb TYPE STANDARD TABLE OF ts_lipsvb .
  types TS_VTTKVB type VTTKVB .
  types:
    tt_vttkvb TYPE STANDARD TABLE OF ts_vttkvb .
  types TS_VTTPVB type VTTPVB .
  types:
    tt_vttpvb TYPE STANDARD TABLE OF ts_vttpvb .
  types TS_VTTSVB type VTTSVB .
  types:
    tt_vttsvb TYPE STANDARD TABLE OF ts_vttsvb .
  types TS_VTSPVB type VTSPVB .
  types:
    tt_vtspvb TYPE STANDARD TABLE OF ts_vtspvb .
  types TS_MSEG type MSEG .
  types:
    tt_mseg TYPE STANDARD TABLE OF ts_mseg .
  types TV_SHIP_TYPE type CHAR2 .
  types TV_TRANS_MODE type CHAR2 .
  types TV_DEPARTURE_DT type TIMESTAMP .
  types TV_DEPARTURE_TZ type TIMEZONE .
  types TV_ARRIVAL_DT type TIMESTAMP .
  types TV_ARRIVAL_TZ type TIMEZONE .
  types TV_DELIV_CNT type INT4 .
  types TV_TROBJ_RES_ID type CHAR20 .
  types TV_TROBJ_RES_VAL type CHAR20 .
  types TV_RESRC_CNT type INT4 .
  types TV_RESRC_TP_ID type CHAR30 .
  types TV_CRDOC_REF_TYP type CHAR3 .
  types TV_CRDOC_REF_VAL type TNDR_TRKID .
  types TV_STOPNUM type INT4 .
  types TV_STOPID type CHAR255 .
  types TV_STOPCNT type INT4 .
  types TV_LOCCAT type CHAR1 .
  types TV_LOCTYPE type CHAR20 .
  types TV_LOCID type CHAR10 .
  types TV_LSTELZ_TXT type CHAR20 .
  types TV_KUNABLAZ_TXT type CHAR25 .
  types TV_LGORTAZ_TXT type CHAR16 .
  types TV_LGNUMAZ type CHAR3 .
  types TV_TORAZ type CHAR3 .
  types TV_LGTRAZ_TXT type CHAR50 .
  types TV_TSRFO type NUM4 .
  types TV_PLN_EVT_DATETIME type TIMESTAMP .
  types TV_PLN_EVT_TIMEZONE type CHAR6 .
  types:
    BEGIN OF ts_stops,
           stopid           TYPE tv_stopid,
           stopcnt          TYPE tv_stopcnt,
           loccat           TYPE tv_loccat,
           loctype          TYPE tv_loctype,
           locid            TYPE tv_locid,
           lstelz_txt       TYPE tv_lstelz_txt,
           kunablaz_txt     TYPE tv_kunablaz_txt,
           lgortaz_txt      TYPE tv_lgortaz_txt,
           lgnumaz          TYPE tv_lgnumaz,
           toraz            TYPE tv_toraz,
           lgtraz_txt       TYPE tv_lgtraz_txt,
           tknum            TYPE tknum,
           tsnum            TYPE tsnum,
           tsrfo            TYPE tsrfo,
           pln_evt_datetime TYPE timestamp,
           pln_evt_timezone TYPE timezone,
         END OF ts_stops .
  types:
    tt_stops TYPE STANDARD TABLE OF ts_stops
                    WITH EMPTY KEY .
  types:
    BEGIN OF ts_dlv_watch_stops,
           vbeln  TYPE vbeln_vl,
           stopid TYPE tv_stopid,
           loccat TYPE tv_loccat,
         END OF ts_dlv_watch_stops .
  types:
    tt_dlv_watch_stops TYPE STANDARD TABLE OF ts_dlv_watch_stops
                              WITH EMPTY KEY .
endinterface.
