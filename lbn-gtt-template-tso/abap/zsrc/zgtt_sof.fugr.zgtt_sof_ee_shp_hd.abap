FUNCTION zgtt_sof_ee_shp_hd.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_APPSYS) TYPE  /SAPTRX/APPLSYSTEM
*"     REFERENCE(I_APP_OBJ_TYPES) TYPE  /SAPTRX/AOTYPES
*"     REFERENCE(I_ALL_APPL_TABLES) TYPE  TRXAS_TABCONTAINER
*"     REFERENCE(I_APP_TYPE_CNTL_TABS) TYPE  TRXAS_APPTYPE_TABS
*"     REFERENCE(I_APP_OBJECTS) TYPE  TRXAS_APPOBJ_CTABS
*"  TABLES
*"      E_EXPEVENTDATA STRUCTURE  /SAPTRX/EXP_EVENTS
*"      E_MEASRMNTDATA STRUCTURE  /SAPTRX/MEASR_DATA OPTIONAL
*"      E_INFODATA STRUCTURE  /SAPTRX/INFO_DATA OPTIONAL
*"      E_LOGTABLE STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      EXP_EVENT_DETERM_ERROR
*"      TABLE_DETERMINATION_ERROR
*"      STOP_PROCESSING
*"----------------------------------------------------------------------
  DATA:
*   Definition of all application objects
    ls_app_objects  TYPE trxas_appobj_ctab_wa,
*   Work Structure for Expected Event
    ls_expeventdata TYPE /saptrx/exp_events,
*   Shipment Stage(Leg) New
    lt_xvtts        TYPE STANDARD TABLE OF vttsvb,
*   Shipment Stage(Leg) New
    ls_xvtts        TYPE vttsvb,
*   Shipment Item Stage(Leg) New
    lt_xvtsp        TYPE STANDARD TABLE OF vtspvb,
*   Shipment Item Stage(Leg) New
    ls_xvtsp        TYPE vtspvb,
*   Shipment Item New
    lt_xvttp        TYPE STANDARD TABLE OF vttpvb,
*   Shipment Item New
    ls_xvttp        TYPE vttpvb,
*   Shipment Stops New
    lt_stops        TYPE zgtt_stops,
*   Shipment Stops New
    ls_stop         TYPE zgtt_stop_info,
*   System Timezone
    lv_timezone     TYPE timezone,
*   Location Id
    lv_locid        TYPE zgtt_locid,
*   POD relevance
    lv_pdstk        TYPE pdstk,
*   POD
    lv_pod_pln      TYPE boolean.

  FIELD-SYMBOLS:
*   Shipment Header
    <ls_xvttk> TYPE vttkvb.

* <1> Read necessary application tables
* Read Shipment Stage(Leg) Data New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_leg_new
    CHANGING lt_xvtts.

* Read Shipment Item Stage(Leg) Data New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_item_leg_new
    CHANGING lt_xvtsp.

* Read Shipment Item Data New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_item_new
    CHANGING lt_xvttp.


* <2> Fill general data for all control data records
  CLEAR ls_expeventdata.
* Logical System ID of an application system
  ls_expeventdata-appsys     = i_appsys.
* Application Object type
  ls_expeventdata-appobjtype = i_app_obj_types-aotype.
* Login Language
  ls_expeventdata-language   = sy-langu.

*   Get System TimeZone
  CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
    IMPORTING
      timezone            = lv_timezone
    EXCEPTIONS
      customizing_missing = 1
      OTHERS              = 2.

* <3> Loop at application objects
  LOOP AT i_app_objects INTO ls_app_objects.

*   Application Object ID
    ls_expeventdata-appobjid = ls_app_objects-appobjid.

*   Check if Main table is Delivery Header or not.
    IF ls_app_objects-maintabdef >< gc_bpt_shipment_header_new.
      PERFORM create_logtable_aot
        TABLES e_logtable
        USING  ls_app_objects-maintabdef
               space
               i_app_obj_types-expeventfunc
               ls_app_objects-appobjtype
               i_appsys.
      EXIT.
    ENDIF.
*   Read Main Table
    ASSIGN ls_app_objects-maintabref->* TO <ls_xvttk>.

    ls_expeventdata-evt_exp_tzone = lv_timezone.

    CLEAR: ls_expeventdata-locid1, ls_expeventdata-locid2.

* Planned Check-in
    ls_expeventdata-milestone    = 'CHECK_IN'.
*   Get Planned Check-in datetime
    PERFORM set_local_timestamp
      USING    <ls_xvttk>-dpreg
               <ls_xvttk>-upreg
      CHANGING ls_expeventdata-evt_exp_datetime.
    APPEND ls_expeventdata TO e_expeventdata.

* Planned Load-Start
    ls_expeventdata-milestone    = 'LOAD_BEGIN'.
*   Get Planned Load-Start datetime
    PERFORM set_local_timestamp
      USING    <ls_xvttk>-dplbg
               <ls_xvttk>-uplbg
      CHANGING ls_expeventdata-evt_exp_datetime.
    APPEND ls_expeventdata TO e_expeventdata.

* Planned Load-End
    ls_expeventdata-milestone    = 'LOAD_END'.
*   Get Planned Load-End datetime
    PERFORM set_local_timestamp
      USING    <ls_xvttk>-dplen
               <ls_xvttk>-uplen
      CHANGING ls_expeventdata-evt_exp_datetime.
    APPEND ls_expeventdata TO e_expeventdata.

    CLEAR lt_stops.
    CALL FUNCTION 'ZGTT_GET_STOPS_FROM_SHIPMENT'
      EXPORTING
        iv_tknum    = <ls_xvttk>-tknum
        it_vtts_new = lt_xvtts
      IMPORTING
        et_stops    = lt_stops.

    LOOP AT lt_stops INTO ls_stop.
      IF ls_stop-loccat = 'S'.
        ls_expeventdata-milestone    = 'DEPARTURE'.
      ELSE.
        ls_expeventdata-milestone    = 'ARRIV_DEST'.
      ENDIF.

      ls_expeventdata-locid2       = ls_stop-stopid.
      ls_expeventdata-loctype      = ls_stop-loctype.
      ls_expeventdata-locid1       = ls_stop-locid.
      ls_expeventdata-evt_exp_datetime  = ls_stop-pln_evt_datetime.
      ls_expeventdata-evt_exp_tzone = ls_stop-pln_evt_timezone.

      APPEND ls_expeventdata TO e_expeventdata.

      CHECK ls_stop-loccat = 'D' AND ls_stop-loctype = 'Customer'.

      LOOP AT lt_xvtsp INTO ls_xvtsp WHERE tknum = ls_stop-tknum
                                     AND   tsnum = ls_stop-tsnum.
        READ TABLE lt_xvttp INTO ls_xvttp WITH KEY tknum = ls_xvtsp-tknum
                                                   tpnum = ls_xvtsp-tpnum.
        CLEAR: lv_locid.
        SELECT SINGLE kunnr INTO lv_locid FROM likp WHERE vbeln = ls_xvttp-vbeln.

        CLEAR: lv_pdstk.
        SELECT SINGLE z_pdstk INTO lv_pdstk FROM zgtt_sof_ee_rel WHERE appobjid = ls_xvttp-vbeln.

        IF lv_locid EQ ls_stop-locid AND lv_pdstk = 'X'.
          lv_pod_pln = 'X'.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_pod_pln = 'X'.
        ls_expeventdata-milestone    = 'POD'.
        ls_expeventdata-locid2       = ls_stop-stopid.
        ls_expeventdata-loctype      = ls_stop-loctype.
        ls_expeventdata-locid1       = ls_stop-locid.
        ls_expeventdata-evt_exp_datetime  = ls_stop-pln_evt_datetime.
        ls_expeventdata-evt_exp_tzone = ls_stop-pln_evt_timezone.

        APPEND ls_expeventdata TO e_expeventdata.
      ENDIF.

      CLEAR lv_pod_pln.

    ENDLOOP.

    READ TABLE e_expeventdata WITH KEY appobjid = ls_app_objects-appobjid TRANSPORTING NO FIELDS.
    IF sy-subrc NE 0.
      ls_expeventdata-evt_exp_datetime = '000000000000000'.
      ls_expeventdata-milestone     = ''.
      ls_expeventdata-evt_exp_tzone = ''.
      ls_expeventdata-loctype = ''.
      ls_expeventdata-locid1 = ''.
      ls_expeventdata-locid2 = ''.
      APPEND ls_expeventdata TO e_expeventdata.
    ENDIF.


  ENDLOOP.

ENDFUNCTION.
