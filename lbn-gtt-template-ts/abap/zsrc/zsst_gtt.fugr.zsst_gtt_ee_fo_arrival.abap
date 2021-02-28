FUNCTION zsst_gtt_ee_fo_arrival.
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
  CALL FUNCTION '/SCMTMS/EXTR_EVT_TO_ARRIVAL'
    EXPORTING
      i_appsys               = i_appsys
      i_event_type           = i_event_type
      i_all_appl_tables      = i_all_appl_tables
      i_event_type_cntl_tabs = i_event_type_cntl_tabs
      i_events               = i_events
    TABLES
      ct_trackingheader      = ct_trackingheader
      ct_tracklocation       = ct_tracklocation
      ct_trackaddress        = ct_trackaddress
    CHANGING
      c_eventid_map          = c_eventid_map
    EXCEPTIONS
      parameter_error        = 1
      event_data_error       = 2
      stop_processing        = 3
      OTHERS                 = 4.
  CASE sy-subrc.
    WHEN 1.
      RAISE parameter_error.
    WHEN 2.
      RAISE event_data_error.
    WHEN 3.
      RAISE stop_processing.
  ENDCASE.

  TRY.
      LOOP AT i_events ASSIGNING FIELD-SYMBOL(<ls_event>).
        NEW lcl_actual_event( )->get_tor_actual_event_class( <ls_event> )->adjust_ae_location_data(
          EXPORTING
            i_all_appl_tables  = i_all_appl_tables
            iv_event_code      = /scmtms/if_tor_const=>sc_tor_event-arriv_dest
            i_event            = <ls_event>
          CHANGING
            ct_trackingheader  = ct_trackingheader[]
            ct_tracklocation   = ct_tracklocation[]
            ct_trackparameters = ct_trackparameters[] ).
      ENDLOOP.
    CATCH cx_udm_message INTO DATA(lo_udm_message).
      lcl_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = DATA(ls_bapiret) ).
      APPEND ls_bapiret TO ct_logtable.
      RAISE stop_processing.
  ENDTRY.

ENDFUNCTION.
