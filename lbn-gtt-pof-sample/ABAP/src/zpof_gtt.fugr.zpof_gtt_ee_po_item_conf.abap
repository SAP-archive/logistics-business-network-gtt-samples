FUNCTION zpof_gtt_ee_po_item_conf.
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
  DATA: lo_udm_message    TYPE REF TO cx_udm_message,
        ls_bapiret        TYPE bapiret2.

  TRY.
      lcl_ae_performer=>get_event_data(
        EXPORTING
          is_definition         = VALUE #(
                                    maintab   = lif_pof_constants=>cs_tabledef-po_item_new
                                    mastertab = lif_pof_constants=>cs_tabledef-po_header_new )
          io_ae_factory           = NEW lcl_ae_factory_po_item_conf( )
          iv_appsys               = i_appsys
          is_event_type           = i_event_type
          it_all_appl_tables      = i_all_appl_tables
          it_event_type_cntl_tabs = i_event_type_cntl_tabs
          it_events               = i_events
        CHANGING
          ct_eventid_map          = c_eventid_map[]
          ct_trackingheader       = ct_trackingheader[]
          ct_tracklocation        = ct_tracklocation[]
          ct_trackreferences      = ct_trackreferences[]
          ct_trackparameters      = ct_trackparameters[]
      ).

    CATCH cx_udm_message INTO lo_udm_message.
      lcl_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = ls_bapiret ).

      " add error message
      APPEND ls_bapiret TO ct_logtable.

      " throw corresponding exception
      CASE lo_udm_message->textid.
        WHEN lif_ef_constants=>cs_errors-stop_processing.
          RAISE stop_processing.
        WHEN lif_ef_constants=>cs_errors-table_determination.
          RAISE event_data_error.
      ENDCASE.
  ENDTRY.
ENDFUNCTION.
