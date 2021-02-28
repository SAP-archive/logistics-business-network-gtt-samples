FUNCTION zgtt_sof_ee_shp_arrival .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_APPSYS) TYPE  /SAPTRX/APPLSYSTEM
*"     REFERENCE(I_EVENT_TYPE) TYPE  /SAPTRX/EVTYPES
*"     REFERENCE(I_ALL_APPL_TABLES) TYPE  TRXAS_TABCONTAINER
*"     REFERENCE(I_EVENT_TYPE_CNTL_TABS) TYPE  TRXAS_EVENTTYPE_TABS
*"     REFERENCE(I_EVENTS) TYPE  TRXAS_EVT_CTABS
*"  TABLES
*"      CT_TRACKINGHEADER STRUCTURE  /SAPTRX/BAPI_EVM_HEADER
*"      CT_TRACKLOCATION STRUCTURE  /SAPTRX/BAPI_EVM_LOCATIONID
*"       OPTIONAL
*"      CT_TRACKADDRESS STRUCTURE  /SAPTRX/BAPI_EVM_ADDRESS OPTIONAL
*"      CT_TRACKLOCATIONDESCR STRUCTURE  /SAPTRX/BAPI_EVM_LOCDESCR
*"       OPTIONAL
*"      CT_TRACKLOCADDITIONALID STRUCTURE  /SAPTRX/BAPI_EVM_LOCADDID
*"       OPTIONAL
*"      CT_TRACKPARTNERID STRUCTURE  /SAPTRX/BAPI_EVM_PARTNERID
*"       OPTIONAL
*"      CT_TRACKPARTNERADDID STRUCTURE  /SAPTRX/BAPI_EVM_PARTNERADDID
*"       OPTIONAL
*"      CT_TRACKESTIMDEADLINE STRUCTURE  /SAPTRX/BAPI_EVM_ESTIMDEADL
*"       OPTIONAL
*"      CT_TRACKCONFIRMSTATUS STRUCTURE  /SAPTRX/BAPI_EVM_CONFSTAT
*"       OPTIONAL
*"      CT_TRACKNEXTEVENT STRUCTURE  /SAPTRX/BAPI_EVM_NEXTEVENT
*"       OPTIONAL
*"      CT_TRACKNEXTEVDEADLINES STRUCTURE  /SAPTRX/BAPI_EVM_NEXTEVDEADL
*"       OPTIONAL
*"      CT_TRACKREFERENCES STRUCTURE  /SAPTRX/BAPI_EVM_REFERENCE
*"       OPTIONAL
*"      CT_TRACKMEASURESULTS STRUCTURE  /SAPTRX/BAPI_EVM_MEASRESULT
*"       OPTIONAL
*"      CT_TRACKSTATUSATTRIB STRUCTURE  /SAPTRX/BAPI_EVM_STATUSATTR
*"       OPTIONAL
*"      CT_TRACKPARAMETERS STRUCTURE  /SAPTRX/BAPI_EVM_PARAMETERS
*"       OPTIONAL
*"      CT_TRACKFILEHEADER STRUCTURE  /SAPTRX/BAPI_EVM_FILEHEADER
*"       OPTIONAL
*"      CT_TRACKFILEREF STRUCTURE  /SAPTRX/BAPI_EVM_FILEREF OPTIONAL
*"      CT_TRACKFILEBIN STRUCTURE  /SAPTRX/BAPI_EVM_FILEBIN OPTIONAL
*"      CT_TRACKFILECHAR STRUCTURE  /SAPTRX/BAPI_EVM_FILECHAR OPTIONAL
*"      CT_TRACKTEXTHEADER STRUCTURE  /SAPTRX/BAPI_EVM_TEXTHEADER
*"       OPTIONAL
*"      CT_TRACKTEXTLINES STRUCTURE  /SAPTRX/BAPI_EVM_TEXTLINES
*"       OPTIONAL
*"      CT_TRACKEEMODIFY STRUCTURE  /SAPTRX/BAPI_EVM_EE_MODIFY OPTIONAL
*"      CT_EXTENSIONIN STRUCTURE  BAPIPAREX OPTIONAL
*"      CT_EXTENSIONOUT STRUCTURE  BAPIPAREX OPTIONAL
*"      CT_LOGTABLE STRUCTURE  BAPIRET2 OPTIONAL
*"  CHANGING
*"     REFERENCE(C_EVENTID_MAP) TYPE  TRXAS_EVTID_EVTCNT_MAP
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      EVENT_DATA_ERROR
*"      STOP_PROCESSING
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Top Include
* TYPE-POOLS:trxas.
*----------------------------------------------------------------------*
  DATA:
*   Container with references
    ls_one_app_tables TYPE trxas_tabcontainer_wa,
*   Definition of all event type
    ls_events         TYPE trxas_evt_ctab_wa,
*   Bapi for message input: message Header
    ls_trackingheader TYPE /saptrx/bapi_evm_header,
*   Event Mapping
    ls_eventid_map    TYPE trxas_evtid_evtcnt_map_wa,
*   Location  table for event message input
    ls_tracklocation  TYPE /saptrx/bapi_evm_locationid,
*   Shipment Stage(Leg) New
    lt_xvtts          TYPE STANDARD TABLE OF vttsvb,
*   Shipment Stage(Leg) Old
    lt_yvtts          TYPE STANDARD TABLE OF vttsvb,
*   Stops Info table
    lt_stops          TYPE zgtt_stops,
    lv_relevance      TYPE boole_d,
    lv_cnt            TYPE /saptrx/evtcnt,
    lv_evtcnt         TYPE /saptrx/evtcnt,
*   BAPI structure for Event Handler control parameters
    ls_parameters     type /SAPTRX/BAPI_EVM_PARAMETERS.

  FIELD-SYMBOLS:
*   Shipment Header
    <ls_xvttk> TYPE vttkvb,
*   Shipment Stage <leg>
    <ls_xvtts> TYPE vttsvb,
*   Shipment Stage <leg>
    <ls_yvtts> TYPE vttsvb,
*   Stops Info
    <ls_stops> TYPE zgtt_stop_info.

* <1> Fill general data for all control data records
* Login Language
  ls_trackingheader-language   = sy-langu.

* <2> Read necessary application tables from table reference
  PERFORM read_appl_tables_shipment_leg
  TABLES lt_xvtts
         lt_yvtts
  USING  i_all_appl_tables.

* <3> Loop at application objects for geting shipment item data
  LOOP AT i_events INTO ls_events.
*   Check if Main table is Shipment Header or not.
    IF ls_events-maintabdef <> gc_bpt_shipment_header_new.
      PERFORM create_logtable_et
          TABLES ct_logtable
          USING  ls_events-maintabdef
                 space
                 i_event_type-eventdatafunc
                 ls_events-eventtype
                 i_appsys.
      EXIT.
    ENDIF.

*   Read Main Object Table (Shipment Header - VTTKVB)
    ASSIGN ls_events-maintabref->* TO <ls_xvttk>.

    IF lt_stops IS INITIAL.
      CALL FUNCTION 'ZGTT_GET_STOPS_FROM_SHIPMENT'
        EXPORTING
          iv_tknum    = <ls_xvttk>-tknum
          it_vtts_new = lt_xvtts
        IMPORTING
          et_stops    = lt_stops.
      SORT lt_stops BY tknum tsnum loccat.
    ENDIF.

    LOOP AT lt_xvtts ASSIGNING <ls_xvtts>
         WHERE tknum = <ls_xvttk>-tknum.
      lv_relevance = gc_false.
      IF <ls_xvtts>-updkz = gc_insert.
* Date cannot be intial.Time can be initial.
        IF <ls_xvtts>-daten IS NOT INITIAL.
          lv_relevance = gc_true.
        ENDIF.
      ELSEIF <ls_xvtts>-updkz = gc_update.
        READ TABLE lt_yvtts ASSIGNING <ls_yvtts>
             WITH KEY tknum = <ls_xvtts>-tknum
                      tsnum = <ls_xvtts>-tsnum
        BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          IF <ls_xvtts>-daten IS NOT INITIAL
            AND ( <ls_xvtts>-daten <> <ls_yvtts>-daten
                  OR <ls_xvtts>-uaten <> <ls_yvtts>-uaten ).
            lv_relevance = gc_true.
          ENDIF.
        ENDIF.
      ENDIF.
      IF lv_relevance = gc_true.
        READ TABLE lt_stops ASSIGNING <ls_stops>
             WITH KEY tknum = <ls_xvtts>-tknum
                      tsnum = <ls_xvtts>-tsnum
                      loccat = 'D'.
        IF sy-subrc IS INITIAL.
*         Check In Code Set
          ls_trackingheader-trxcod = 'SHIPMENT_ORDER'.
*         Delivery Order Id
          ls_trackingheader-trxid = <ls_xvtts>-tknum.

*         < Arrival Event  >
*         Counter for Event
          IMPORT lv_evtcnt FROM MEMORY ID 'EVTCNT'.
          IF sy-subrc = 0.
            lv_evtcnt = lv_evtcnt + 1.
            lv_cnt = lv_evtcnt.
            EXPORT lv_evtcnt TO MEMORY ID 'EVTCNT'.
            FREE lv_evtcnt.
          ELSE.
            lv_evtcnt = 1000000000.
            lv_cnt = lv_evtcnt.
            EXPORT lv_evtcnt TO MEMORY ID 'EVTCNT'.
            FREE lv_evtcnt.
          ENDIF.

          ls_trackingheader-evtcnt  = lv_cnt.
*         Event ID
          ls_trackingheader-evtid     = 'ARRIV_DEST'.
*         Event Date
          ls_trackingheader-evtdat  = <ls_xvtts>-daten.
*         Event Time
          ls_trackingheader-evttim  = <ls_xvtts>-uaten.
*         Event Time Zone
          CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
            IMPORTING
              timezone            = ls_trackingheader-evtzon
            EXCEPTIONS
              customizing_missing = 1
              OTHERS              = 2.

          APPEND ls_trackingheader TO ct_trackingheader.

*         Mapping table
          ls_eventid_map-eventid    = ls_events-eventid.
          ls_eventid_map-evtcnt     = lv_cnt.
          APPEND ls_eventid_map TO c_eventid_map.

          ls_tracklocation-evtcnt = lv_cnt.
          ls_tracklocation-loccod = <ls_stops>-loctype.
          ls_tracklocation-locid1 = <ls_stops>-locid.
          ls_tracklocation-locid2 = <ls_stops>-stopid.
          APPEND ls_tracklocation TO ct_tracklocation.

*         Actual Technical Datetime & Time zone
          CLEAR ls_parameters.
          ls_parameters-evtcnt = lv_cnt.
          ls_parameters-param_name = gc_cp_yn_acttec_timezone."ACTUAL_TECHNICAL_TIMEZONE
          ls_parameters-param_value = ls_trackingheader-evtzon.
          APPEND ls_parameters TO ct_trackparameters.

          CLEAR ls_parameters.
          ls_parameters-evtcnt = lv_cnt.
          ls_parameters-param_name = gc_cp_yn_acttec_datetime."ACTUAL_TECHNICAL_DATETIME
          CONCATENATE '0' sy-datum sy-uzeit INTO ls_parameters-param_value.
          APPEND ls_parameters TO ct_trackparameters.

        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFUNCTION.
