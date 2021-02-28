FUNCTION ZGTT_SOF_EE_DE_GI.
*"--------------------------------------------------------------------
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
*"         OPTIONAL
*"      CT_TRACKADDRESS STRUCTURE  /SAPTRX/BAPI_EVM_ADDRESS OPTIONAL
*"      CT_TRACKLOCATIONDESCR STRUCTURE  /SAPTRX/BAPI_EVM_LOCDESCR
*"         OPTIONAL
*"      CT_TRACKLOCADDITIONALID STRUCTURE  /SAPTRX/BAPI_EVM_LOCADDID
*"         OPTIONAL
*"      CT_TRACKPARTNERID STRUCTURE  /SAPTRX/BAPI_EVM_PARTNERID
*"         OPTIONAL
*"      CT_TRACKPARTNERADDID STRUCTURE  /SAPTRX/BAPI_EVM_PARTNERADDID
*"         OPTIONAL
*"      CT_TRACKESTIMDEADLINE STRUCTURE  /SAPTRX/BAPI_EVM_ESTIMDEADL
*"         OPTIONAL
*"      CT_TRACKCONFIRMSTATUS STRUCTURE  /SAPTRX/BAPI_EVM_CONFSTAT
*"         OPTIONAL
*"      CT_TRACKNEXTEVENT STRUCTURE  /SAPTRX/BAPI_EVM_NEXTEVENT
*"         OPTIONAL
*"      CT_TRACKNEXTEVDEADLINES
*"  STRUCTURE  /SAPTRX/BAPI_EVM_NEXTEVDEADL OPTIONAL
*"      CT_TRACKREFERENCES STRUCTURE  /SAPTRX/BAPI_EVM_REFERENCE
*"         OPTIONAL
*"      CT_TRACKMEASURESULTS STRUCTURE  /SAPTRX/BAPI_EVM_MEASRESULT
*"         OPTIONAL
*"      CT_TRACKSTATUSATTRIB STRUCTURE  /SAPTRX/BAPI_EVM_STATUSATTR
*"         OPTIONAL
*"      CT_TRACKPARAMETERS STRUCTURE  /SAPTRX/BAPI_EVM_PARAMETERS
*"         OPTIONAL
*"      CT_TRACKFILEHEADER STRUCTURE  /SAPTRX/BAPI_EVM_FILEHEADER
*"         OPTIONAL
*"      CT_TRACKFILEREF STRUCTURE  /SAPTRX/BAPI_EVM_FILEREF OPTIONAL
*"      CT_TRACKFILEBIN STRUCTURE  /SAPTRX/BAPI_EVM_FILEBIN OPTIONAL
*"      CT_TRACKFILECHAR STRUCTURE  /SAPTRX/BAPI_EVM_FILECHAR
*"         OPTIONAL
*"      CT_TRACKTEXTHEADER STRUCTURE  /SAPTRX/BAPI_EVM_TEXTHEADER
*"         OPTIONAL
*"      CT_TRACKTEXTLINES STRUCTURE  /SAPTRX/BAPI_EVM_TEXTLINES
*"         OPTIONAL
*"      CT_TRACKEEMODIFY STRUCTURE  /SAPTRX/BAPI_EVM_EE_MODIFY
*"         OPTIONAL
*"      CT_EXTENSIONIN STRUCTURE  BAPIPAREX OPTIONAL
*"      CT_EXTENSIONOUT STRUCTURE  BAPIPAREX OPTIONAL
*"      CT_LOGTABLE STRUCTURE  BAPIRET2 OPTIONAL
*"  CHANGING
*"     REFERENCE(C_EVENTID_MAP) TYPE  TRXAS_EVTID_EVTCNT_MAP
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      EVENT_DATA_ERROR
*"      STOP_PROCESSING
*"--------------------------------------------------------------------
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
*   BAPI structure for Event Handler control parameters
    ls_parameters     type /SAPTRX/BAPI_EVM_PARAMETERS.

  FIELD-SYMBOLS:
*   Work Structure for Delivery Header
    <ls_xlikp>        TYPE likpvb.

* <1> Fill general data for all control data records
* Login Language
  ls_trackingheader-language   = sy-langu.

* <2> Loop at application objects for geting delivery item data
  LOOP AT i_events INTO ls_events.
*   Check if Main table is Delivery Header or not.
     IF ls_events-maintabdef <> gc_bpt_delivery_header_new.
      PERFORM create_logtable_et
          TABLES ct_logtable
          USING  ls_events-maintabdef
                 space
                 i_event_type-eventdatafunc
                 ls_events-eventtype
                 i_appsys.
      EXIT.
    ENDIF.

*   Read Main Object Table (Delivery Header - LIKPVB)
    ASSIGN ls_events-maintabref->* TO <ls_xlikp>.

*   Goods Issue Code Set
    ls_trackingheader-trxcod = 'OUTBOUND_DELIVERY'.
*   Delivery Order Id
    ls_trackingheader-trxid = <ls_xlikp>-vbeln.

*   < Goods Issue >
*   Counter for Event
    ls_trackingheader-evtcnt  = ls_events-eventid.
*   Event ID
    ls_trackingheader-evtid     = 'GOODS_ISSUE'.
*   Event Date
    ls_trackingheader-evtdat  = sy-datum.
*   Event Time
    ls_trackingheader-evttim  = sy-uzeit.
*   Event Time Zone
    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = ls_trackingheader-evtzon
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.

    APPEND ls_trackingheader TO ct_trackingheader.

*   Mapping table
    ls_eventid_map-eventid    = ls_events-eventid.
    ls_eventid_map-evtcnt     = ls_events-eventid.
    APPEND ls_eventid_map TO c_eventid_map.

    ls_tracklocation-evtcnt = ls_events-eventid.
    ls_tracklocation-loccod = 'ShippingPoint'.
    ls_tracklocation-locid1 = <ls_xlikp>-vstel.
    APPEND ls_tracklocation TO ct_tracklocation.

*   Actual Technical Datetime & Time zone
    CLEAR ls_parameters.
    ls_parameters-evtcnt = ls_events-eventid.
    ls_parameters-param_name = gc_cp_yn_acttec_timezone."ACTUAL_TECHNICAL_TIMEZONE
    ls_parameters-param_value = ls_trackingheader-evtzon.
    APPEND ls_parameters TO ct_trackparameters.

    CLEAR ls_parameters.
    ls_parameters-evtcnt = ls_events-eventid.
    ls_parameters-param_name = gc_cp_yn_acttec_datetime."ACTUAL_TECHNICAL_DATETIME
    CONCATENATE '0' sy-datum sy-uzeit INTO ls_parameters-param_value.
    APPEND ls_parameters TO ct_trackparameters.

  ENDLOOP.
ENDFUNCTION.
