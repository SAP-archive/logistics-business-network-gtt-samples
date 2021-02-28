FUNCTION zgtt_sof_ee_de_itm.
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
*   Event expected date/time
    lv_tsmp         TYPE /saptrx/event_exp_datetime,
*   System Timezone
    lv_timezone     TYPE timezone,
*   header status new
    lt_xvbuk        TYPE STANDARD TABLE OF vbukvb,
*   item status new
    lt_xvbup        TYPE STANDARD TABLE OF vbupvb,
*   Status Relevance
    ls_eerel        TYPE zgtt_sof_ee_rel.

  DATA: ls_vbfa_new               TYPE vbfavb,
        lt_vbfa_new               TYPE STANDARD TABLE OF vbfavb,
        ls_stop                   TYPE zgtt_stop_info,
        ls_dlv_watching_stop      TYPE  zgtt_dlv_watch_stop,
        lt_stops_tmp              TYPE  zgtt_stops,
        lt_dlv_watching_stops_tmp TYPE  zgtt_dlv_watch_stops,
        lt_stops                  TYPE  zgtt_stops,
        lt_dlv_watching_stops     TYPE  zgtt_dlv_watch_stops.

  FIELD-SYMBOLS:
*   Delivery Header
    <ls_xlikp> TYPE likpvb,
*   Delivery Item
    <ls_xlips> TYPE lipsvb,
*   Header Status New
    <ls_xvbuk> TYPE vbukvb,
*   Item Status New
    <ls_xvbup> TYPE vbupvb.

* <1> Read necessary application tables
* Read Header Status Data New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_hdrstatus_new
    CHANGING lt_xvbuk.
* Read Item Status Data New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_item_stat_new
    CHANGING lt_xvbup.
* Read Header Status Data New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_document_flow_new
    CHANGING lt_vbfa_new.

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

*   Check if Main table is Delivery Item or not.
    IF ls_app_objects-maintabdef >< gc_bpt_delivery_item_new.
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
    ASSIGN ls_app_objects-maintabref->* TO <ls_xlips>.

*   Check if Master table is Delivery Header or not.
    IF ls_app_objects-mastertabdef >< gc_bpt_delivery_header_new.
      PERFORM create_logtable_aot
        TABLES e_logtable
        USING  ls_app_objects-mastertabdef
               space
               i_app_obj_types-expeventfunc
               ls_app_objects-appobjtype
               i_appsys.
      EXIT.
    ENDIF.
*   Read Master Table
    ASSIGN ls_app_objects-mastertabref->* TO <ls_xlikp>.

    READ TABLE lt_xvbuk ASSIGNING <ls_xvbuk> WITH KEY vbeln = <ls_xlikp>-vbeln.
    READ TABLE lt_xvbup ASSIGNING <ls_xvbup> WITH KEY vbeln = <ls_xlips>-vbeln
                                                      posnr = <ls_xlips>-posnr.

    CLEAR ls_eerel.
    SELECT SINGLE * INTO ls_eerel FROM zgtt_sof_ee_rel WHERE appobjid = ls_app_objects-appobjid.
    ls_eerel-mandt    = sy-mandt.
    ls_eerel-appobjid = ls_app_objects-appobjid.
    IF <ls_xvbup>-kosta = 'A'.
      ls_eerel-z_kostk    = gc_true.
    ELSEIF <ls_xvbup>-kosta = ''.
      ls_eerel-z_kostk    = gc_false.
    ENDIF.
    IF <ls_xvbup>-pksta = 'A'.
      ls_eerel-z_pkstk    = gc_true.
    ELSEIF <ls_xvbup>-pksta = ''.
      ls_eerel-z_pkstk    = gc_false.
    ENDIF.
    IF <ls_xvbuk>-trsta = 'A'.
      ls_eerel-z_trsta    = gc_true.
    ELSEIF <ls_xvbuk>-trsta = ''.
      ls_eerel-z_trsta    = gc_false.
    ENDIF.
    IF <ls_xvbup>-wbsta = 'A'.
      ls_eerel-z_wbstk    = gc_true.
    ELSEIF <ls_xvbup>-wbsta = ''.
      ls_eerel-z_wbstk    = gc_false.
    ENDIF.
    IF <ls_xvbup>-pdsta = 'A'.
      ls_eerel-z_pdstk    = gc_true.
    ELSEIF <ls_xvbup>-pdsta = ''.
      ls_eerel-z_pdstk    = gc_false.
    ENDIF.
    MODIFY zgtt_sof_ee_rel FROM ls_eerel.
    COMMIT WORK.

    CLEAR ls_expeventdata-locid2.
*   < Picking>
    IF ls_eerel-z_kostk = gc_true.
      CLEAR ls_expeventdata-evt_exp_datetime.
      ls_expeventdata-milestone    = 'PICKING'.
*     Get Planned Picking datetime
      PERFORM set_local_timestamp
        USING    <ls_xlikp>-kodat
                 <ls_xlikp>-kouhr
        CHANGING ls_expeventdata-evt_exp_datetime.
      ls_expeventdata-evt_exp_tzone = lv_timezone.
      ls_expeventdata-loctype = 'ShippingPoint'.
      ls_expeventdata-locid1 = <ls_xlikp>-vstel.
      APPEND ls_expeventdata TO e_expeventdata.
    ENDIF.

*   < Packing>
    IF ls_eerel-z_pkstk = gc_true.
      CLEAR ls_expeventdata-evt_exp_datetime.
      ls_expeventdata-milestone    = 'PACKING'.
      ls_expeventdata-evt_exp_tzone = lv_timezone.
      ls_expeventdata-loctype = 'ShippingPoint'.
      ls_expeventdata-locid1 = <ls_xlikp>-vstel.
      APPEND ls_expeventdata TO e_expeventdata.
    ENDIF.

*   < Goods Issued >
    IF ls_eerel-z_wbstk = gc_true.
      CLEAR ls_expeventdata-evt_exp_datetime.
      ls_expeventdata-milestone     = 'GOODS_ISSUE'.
*   Get Planned GI datetime
      PERFORM set_local_timestamp
        USING    <ls_xlikp>-wadat
                 <ls_xlikp>-wauhr
        CHANGING ls_expeventdata-evt_exp_datetime.
      ls_expeventdata-evt_exp_tzone = lv_timezone.
      ls_expeventdata-loctype = 'ShippingPoint'.
      ls_expeventdata-locid1 = <ls_xlikp>-vstel.
      APPEND ls_expeventdata TO e_expeventdata.
    ENDIF.

*   < POD >
    IF ls_eerel-z_pdstk = gc_true.
      CLEAR ls_expeventdata-evt_exp_datetime.
      ls_expeventdata-milestone     = 'DLV_POD'.
      ls_expeventdata-evt_exp_tzone = lv_timezone.
      ls_expeventdata-loctype = 'Customer'.
      ls_expeventdata-locid1 = <ls_xlikp>-kunnr.
      APPEND ls_expeventdata TO e_expeventdata.
    ENDIF.

    CLEAR: lt_stops, lt_dlv_watching_stops.
    DELETE lt_vbfa_new WHERE vbtyp_n NE '8' OR vbtyp_v NE 'J' OR vbelv NE <ls_xlikp>-vbeln.
    LOOP AT lt_vbfa_new INTO ls_vbfa_new.
      CLEAR: lt_stops_tmp, lt_dlv_watching_stops_tmp.
      CALL FUNCTION 'ZGTT_GET_STOPS_FROM_SHIPMENT'
        EXPORTING
          iv_tknum              = ls_vbfa_new-vbeln
        IMPORTING
          et_stops              = lt_stops_tmp
          et_dlv_watching_stops = lt_dlv_watching_stops_tmp.
      APPEND LINES OF lt_stops_tmp TO lt_stops.
      APPEND LINES OF lt_dlv_watching_stops_tmp TO lt_dlv_watching_stops.
    ENDLOOP.
    SORT lt_stops BY stopid loccat.
    SORT lt_dlv_watching_stops BY vbeln stopid loccat.
    DELETE ADJACENT DUPLICATES FROM lt_stops COMPARING stopid loccat.
    DELETE ADJACENT DUPLICATES FROM lt_dlv_watching_stops COMPARING vbeln stopid loccat.

    ls_expeventdata-milestone    = 'DEPARTURE'.
    LOOP AT lt_dlv_watching_stops INTO ls_dlv_watching_stop WHERE vbeln = <ls_xlikp>-vbeln
                                                              AND loccat = 'S'.
      READ TABLE lt_stops INTO ls_stop WITH KEY stopid = ls_dlv_watching_stop-stopid
                                                loccat = ls_dlv_watching_stop-loccat.
      ls_expeventdata-locid2       = ls_stop-stopid.
      ls_expeventdata-loctype      = ls_stop-loctype.
      ls_expeventdata-locid1       = ls_stop-locid.
      ls_expeventdata-evt_exp_datetime  = ls_stop-pln_evt_datetime.
      ls_expeventdata-evt_exp_tzone = ls_stop-pln_evt_timezone.
      APPEND ls_expeventdata TO e_expeventdata.
    ENDLOOP.

    ls_expeventdata-milestone    = 'ARRIV_DEST'.
    LOOP AT lt_dlv_watching_stops INTO ls_dlv_watching_stop WHERE vbeln = <ls_xlikp>-vbeln
                                                              AND loccat = 'D'.
      READ TABLE lt_stops INTO ls_stop WITH KEY stopid = ls_dlv_watching_stop-stopid
                                                loccat = ls_dlv_watching_stop-loccat.
      ls_expeventdata-locid2       = ls_stop-stopid.
      ls_expeventdata-loctype      = ls_stop-loctype.
      ls_expeventdata-locid1       = ls_stop-locid.
      ls_expeventdata-evt_exp_datetime  = ls_stop-pln_evt_datetime.
      ls_expeventdata-evt_exp_tzone = ls_stop-pln_evt_timezone.
      APPEND ls_expeventdata TO e_expeventdata.
    ENDLOOP.

    ls_expeventdata-milestone    = 'POD'.
    LOOP AT lt_dlv_watching_stops INTO ls_dlv_watching_stop WHERE vbeln = <ls_xlikp>-vbeln
                                                              AND loccat = 'D'.
      READ TABLE lt_stops INTO ls_stop WITH KEY stopid = ls_dlv_watching_stop-stopid
                                                loccat = ls_dlv_watching_stop-loccat.

      IF <ls_xlikp>-kunnr EQ ls_stop-locid AND ls_eerel-z_pdstk = 'X'.
        ls_expeventdata-locid2       = ls_stop-stopid.
        ls_expeventdata-loctype      = ls_stop-loctype.
        ls_expeventdata-locid1       = ls_stop-locid.
        ls_expeventdata-evt_exp_datetime  = ls_stop-pln_evt_datetime.
        ls_expeventdata-evt_exp_tzone = ls_stop-pln_evt_timezone.
        APPEND ls_expeventdata TO e_expeventdata.
      ENDIF.
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
