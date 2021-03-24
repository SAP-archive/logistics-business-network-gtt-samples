FUNCTION zsst_gtt_extr_evt_fu_delay.
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
  DATA:
    lv_timezone       TYPE /scmtms/tzone,
    lv_timestamp      TYPE timestampl,
    lv_event_count    TYPE /saptrx/evtcnt,
    ls_trackingheader TYPE /saptrx/bapi_evm_header,
    lt_execinfo_tr    TYPE /scmtms/t_tor_exec_tr_k,
    lt_trackparameter TYPE TABLE OF /saptrx/bapi_evm_parameters.

  FIELD-SYMBOLS:
    <ls_tor_root>            TYPE /scmtms/s_em_bo_tor_root,
    <ls_tor_execinfo>        TYPE /scmtms/s_em_bo_tor_execinfo,
    <ls_tor_execinfo_before> TYPE /scmtms/s_em_bo_tor_execinfo.

  DATA(lo_actual_event) = NEW lcl_actual_event( ).
  lo_actual_event->get_execution(
    EXPORTING
      i_all_appl_tables = i_all_appl_tables
    IMPORTING
      et_execution      = DATA(lt_tor_execinfo) ).

  lo_actual_event->get_execution(
    EXPORTING
      i_all_appl_tables = i_all_appl_tables
      iv_old            = abap_true
    IMPORTING
      et_execution      = DATA(lt_tor_execinfo_before) ).

  DATA(lo_tor_srv_mgr) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_tor_c=>sc_bo_key ).

  LOOP AT i_events ASSIGNING FIELD-SYMBOL(<ls_event>).

    CLEAR ls_trackingheader.
    ls_trackingheader-language = sy-langu.

    ASSIGN <ls_event>-maintabref->* TO <ls_tor_root>.
    CHECK sy-subrc = 0.
    ls_trackingheader-evtid   = /scmtms/if_tor_const=>sc_tor_event-delay.

    CASE <ls_tor_root>-tor_cat.
      WHEN /scmtms/if_tor_const=>sc_tor_category-active.
        ls_trackingheader-trxcod = /scmtms/cl_scem_int_c=>sc_trackid_codesets-tor_tec.
      WHEN /scmtms/if_tor_const=>sc_tor_category-booking.
        ls_trackingheader-trxcod = /scmtms/cl_scem_int_c=>sc_trackid_codesets-tor_tec.
      WHEN /scmtms/if_tor_const=>sc_tor_category-freight_unit.
        ls_trackingheader-trxcod = /scmtms/cl_scem_int_c=>sc_trackid_codesets-freight_unit.
      WHEN /scmtms/if_tor_const=>sc_tor_category-transp_unit.
        ls_trackingheader-trxcod = /scmtms/cl_scem_int_c=>sc_trackid_codesets-tor_tec.
    ENDCASE.

    ls_trackingheader-trxid   = <ls_tor_root>-tor_id.

    GET TIME STAMP FIELD lv_timestamp.
    ls_trackingheader-evttst = lv_timestamp.
    ls_trackingheader-sndnam = /scmtms/cl_scem_int_c=>gc_sendername_tm.

    lo_tor_srv_mgr->retrieve_by_association(
      EXPORTING
        iv_node_key    = /scmtms/if_tor_c=>sc_node-root
        it_key         = VALUE #( ( key = <ls_tor_root>-node_id ) )
        iv_association = /scmtms/if_tor_c=>sc_association-root-executioninformation_tr
        iv_fill_data   = abap_true
      IMPORTING
        et_target_key  = DATA(lt_execinfo_tr_key)
        et_data        = lt_execinfo_tr ).

    LOOP AT lt_tor_execinfo ASSIGNING <ls_tor_execinfo>
      WHERE parent_node_id = <ls_tor_root>-node_id AND
            event_code     = /scmtms/if_tor_const=>sc_tor_event-delay_fu.

      ASSIGN lt_tor_execinfo_before[ KEY node_id COMPONENTS
        node_id = <ls_tor_execinfo>-node_id ] TO <ls_tor_execinfo_before>.
      IF ( sy-subrc = 0 AND <ls_tor_execinfo_before> <> <ls_tor_execinfo> ) OR  sy-subrc <> 0.
        lv_event_count += 1.
        /scmtms/cl_em_tm_helper=>extract_standard_event(
          EXPORTING
            is_execinfo           = <ls_tor_execinfo>
            iv_evt_cnt            = lv_event_count
            iv_eventid            = <ls_event>-eventid
            is_trackingheader     = ls_trackingheader
          CHANGING
            ct_trackingheader     = ct_trackingheader[]
            ct_tracklocation      = ct_tracklocation[]
            ct_trackaddress       = ct_trackaddress[]
            ct_trackparameters    = ct_trackparameters[]
            ct_eventid_map        = c_eventid_map ).
        CLEAR ct_trackparameters.

        ASSIGN ct_trackingheader[ trxid = <ls_tor_root>-tor_id ] TO FIELD-SYMBOL(<ls_trackingheader>).
        CHECK sy-subrc = 0.

        CLEAR ct_trackparameters[].
        ASSIGN lt_execinfo_tr[ parent_key   = <ls_tor_execinfo>-parent_node_id
                               execution_id = <ls_tor_execinfo>-execution_id ] TO FIELD-SYMBOL(<ls_execinfo_tr>) ##PRIMKEY.
        CHECK sy-subrc = 0.

        IF <ls_execinfo_tr>-estimated_date IS NOT INITIAL.
          INSERT VALUE #( evtcnt      = <ls_trackingheader>-evtcnt
                          param_name  = lif_ef_constants=>cs_parameter-estimated_datetime
                          param_value = <ls_execinfo_tr>-estimated_date ) INTO TABLE lt_trackparameter.

          IF <ls_execinfo_tr>-actual_date_tz_em IS NOT INITIAL.
            lv_timezone = <ls_execinfo_tr>-actual_date_tz_em.
          ELSE.
            lv_timezone = <ls_execinfo_tr>-actual_tzone.
          ENDIF.

          IF lv_timezone IS NOT INITIAL.
            INSERT VALUE #( evtcnt      = <ls_trackingheader>-evtcnt
                            param_name  = lif_ef_constants=>cs_parameter-estimated_timezone
                            param_value = lv_timezone ) INTO TABLE lt_trackparameter.
          ENDIF.
        ENDIF.

        IF <ls_tor_execinfo>-ref_event_code IS NOT INITIAL.
          INSERT VALUE #( evtcnt      = <ls_trackingheader>-evtcnt
                          param_name  = lif_ef_constants=>cs_parameter-ref_planned_event_milestone
                          param_value = <ls_tor_execinfo>-ref_event_code ) INTO TABLE lt_trackparameter.

          INSERT VALUE #( evtcnt      = <ls_trackingheader>-evtcnt
                          param_name  = lif_ef_constants=>cs_parameter-ref_planned_event_loctype
                          param_value = lif_ef_constants=>cv_logistic_location ) INTO TABLE lt_trackparameter.
        ENDIF.

      ENDIF.
    ENDLOOP.
  ENDLOOP.

  APPEND LINES OF lt_trackparameter TO ct_trackparameters[].

ENDFUNCTION.
