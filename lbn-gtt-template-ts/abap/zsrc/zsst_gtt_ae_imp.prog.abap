*&---------------------------------------------------------------------*
*& Include          ZSST_GTT_AE_IMP
*&---------------------------------------------------------------------*
CLASS lcl_actual_event IMPLEMENTATION.

  METHOD get_tor_actual_event_class.
    FIELD-SYMBOLS <ls_tor_root> TYPE /scmtms/s_em_bo_tor_root.
    ASSIGN i_event-maintabref->* TO <ls_tor_root>.
    IF sy-subrc = 0.
      CASE <ls_tor_root>-tor_cat.
        WHEN /scmtms/if_tor_const=>sc_tor_category-active.
          ro_actual_event = NEW lcl_fo_actual_event( ).
        WHEN /scmtms/if_tor_const=>sc_tor_category-booking.
          ro_actual_event = NEW lcl_fb_actual_event( ).
        WHEN /scmtms/if_tor_const=>sc_tor_category-freight_unit.
          ro_actual_event = NEW lcl_fu_actual_event( ).
        WHEN OTHERS.
          MESSAGE i009(zsst_gtt) WITH <ls_tor_root>-tor_cat INTO DATA(lv_dummy).
          lcl_tools=>throw_exception( ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD lif_actual_event~check_tor_type_specific_events.
  ENDMETHOD.

  METHOD lif_actual_event~check_trxservername.

    FIELD-SYMBOLS <ls_tor_root> TYPE /scmtms/s_em_bo_tor_root.

    CLEAR e_result.

    ASSIGN i_event-maintabref->* TO <ls_tor_root>.

    SELECT SINGLE /saptrx/aotypes~trxservername
      FROM /scmtms/c_torty
      JOIN /saptrx/aotypes ON /scmtms/c_torty~aotype = /saptrx/aotypes~aotype
      INTO @DATA(lv_trxservername)
      WHERE /scmtms/c_torty~type         = @<ls_tor_root>-tor_type AND
            /saptrx/aotypes~trk_obj_type = @lif_actual_event~cv_tor_trk_obj_typ.

    IF i_event-trxservername <> lv_trxservername.
      e_result = lif_ef_constants=>cs_condition-false.
    ENDIF.

  ENDMETHOD.

  METHOD lif_actual_event~check_event_relevance.

    IF lif_actual_event~check_tor_type_specific_events( iv_event_code ) = lif_ef_constants=>cs_condition-false.
      RETURN.
    ENDIF.

    lif_actual_event~check_application_event_source(
      EXPORTING
        i_all_appl_tables = i_all_appl_tables
        iv_event_code     = iv_event_code
        i_event           = i_event
      IMPORTING
        e_result          = e_result ).
    IF e_result = lif_ef_constants=>cs_condition-false.
      RETURN.
    ENDIF.

    lif_actual_event~check_trxservername(
      EXPORTING
        i_event  = i_event
      IMPORTING
        e_result = e_result ).

  ENDMETHOD.

  METHOD lif_actual_event~check_application_event_source.

    FIELD-SYMBOLS <ls_tor_root> TYPE /scmtms/s_em_bo_tor_root.

    CLEAR e_result.

    get_execution(
      EXPORTING
        i_all_appl_tables = i_all_appl_tables
      IMPORTING
        et_execution      = DATA(lt_tor_execinfo) ).

    get_execution(
      EXPORTING
        i_all_appl_tables = i_all_appl_tables
        iv_old            = abap_true
      IMPORTING
        et_execution      = DATA(lt_tor_execinfo_old) ).

    ASSIGN i_event-maintabref->* TO <ls_tor_root>.

    LOOP AT lt_tor_execinfo ASSIGNING FIELD-SYMBOL(<ls_tor_execinfo>) WHERE parent_node_id = <ls_tor_root>-node_id.

      ASSIGN lt_tor_execinfo_old[ KEY node_id COMPONENTS node_id = <ls_tor_execinfo>-node_id ]
        TO FIELD-SYMBOL(<ls_tor_execinfo_old>).
      IF ( sy-subrc = 0 AND <ls_tor_execinfo_old> <> <ls_tor_execinfo> ) OR sy-subrc <> 0.

        CHECK <ls_tor_execinfo>-event_code = iv_event_code.
        CHECK NOT ( <ls_tor_execinfo>-execinfo_source = /scmtms/if_tor_const=>sc_tor_event_source-application OR
                    <ls_tor_execinfo>-execinfo_source = /scmtms/if_tor_const=>sc_tor_event_source-prop_predecessor OR
                    <ls_tor_execinfo>-execinfo_source = /scmtms/if_tor_const=>sc_tor_event_source-prop_successor ).
        e_result = lif_ef_constants=>cs_condition-false.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
  METHOD lif_actual_event~adjust_ae_location_data.

    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.
    DATA ls_trackparameters TYPE /saptrx/bapi_evm_parameters.
    DATA(lt_stop) = get_stop( i_all_appl_tables ).

    ASSIGN i_event-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN ct_trackingheader[ trxid = <ls_root>-tor_id ] TO FIELD-SYMBOL(<ls_trackingheader>).
    IF sy-subrc = 0.
      <ls_trackingheader>-trxcod = lif_actual_event~cs_trxcode-shipment_order.
      <ls_trackingheader>-evtid  = get_model_event_id( <ls_trackingheader>-evtid ).
    ENDIF.

    lcl_tools=>get_stop_points(
      EXPORTING
        iv_root_id     = <ls_root>-tor_id
        it_stop        = VALUE #( FOR <ls_stop> IN lt_stop USING KEY parent_seqnum
                                  WHERE ( parent_node_id = <ls_root>-node_id ) ( <ls_stop> ) )
      IMPORTING
        et_stop_points = DATA(lt_stop_points) ).

    ASSIGN ct_tracklocation[ evtcnt = <ls_trackingheader>-evtcnt ] TO FIELD-SYMBOL(<ls_tracklocation>).
    IF sy-subrc = 0.
      <ls_tracklocation>-loccod = lif_actual_event~cs_location_type-logistic.

      ASSIGN lt_stop_points[ log_locid = <ls_tracklocation>-locid1 ]-stop_id TO FIELD-SYMBOL(<lv_stop_id>).
      IF sy-subrc = 0.
        SHIFT <lv_stop_id> LEFT DELETING LEADING '0'.
        <ls_tracklocation>-locid2 = <lv_stop_id>.
      ENDIF.
    ENDIF.

    CLEAR ls_trackparameters.
    ls_trackparameters-evtcnt      = <ls_trackingheader>-evtcnt.
    ls_trackparameters-param_name  = lif_ef_constants=>cs_system_fields-actual_technical_timezone.
    ls_trackparameters-param_value = lcl_tools=>get_system_time_zone( ).
    APPEND ls_trackparameters TO ct_trackparameters.

    CLEAR ls_trackparameters.
    ls_trackparameters-evtcnt      = <ls_trackingheader>-evtcnt.
    ls_trackparameters-param_name  = lif_ef_constants=>cs_system_fields-actual_technical_datetime.
    ls_trackparameters-param_value = lcl_tools=>get_system_date_time( ).
    APPEND ls_trackparameters TO ct_trackparameters.

  ENDMETHOD.

  METHOD get_stop.
    FIELD-SYMBOLS <lt_stop> TYPE /scmtms/t_em_bo_tor_stop.
    ASSIGN i_all_appl_tables[ tabledef = lif_actual_event~cs_tabledef-tor_stop ]-tableref TO FIELD-SYMBOL(<lr_tabref>).
    IF sy-subrc = 0.
      ASSIGN <lr_tabref>->* TO <lt_stop>.
      IF sy-subrc = 0.
        rt_stop = <lt_stop>.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_root.
    FIELD-SYMBOLS <lt_root> TYPE /scmtms/t_em_bo_tor_root.
    ASSIGN i_all_appl_tables[ tabledef = lif_actual_event~cs_tabledef-tor_root ]-tableref TO FIELD-SYMBOL(<lr_tabref>).
    IF sy-subrc = 0.
      ASSIGN <lr_tabref>->* TO <lt_root>.
      IF sy-subrc = 0.
        rt_root = <lt_root>.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_capa_stop.
    FIELD-SYMBOLS <lt_capa_stop> TYPE /scmtms/t_em_bo_tor_stop.
    ASSIGN i_all_appl_tables[ tabledef = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-capa_stop ]-tableref
      TO FIELD-SYMBOL(<lr_tabref>).
    IF sy-subrc = 0.
      ASSIGN <lr_tabref>->* TO <lt_capa_stop>.
      IF sy-subrc = 0.
        rt_stop = <lt_capa_stop>.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_capa_root.
    FIELD-SYMBOLS <lt_capa_root> TYPE /scmtms/t_em_bo_tor_root.
    ASSIGN i_all_appl_tables[ tabledef = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-capa_root ]-tableref
      TO FIELD-SYMBOL(<lr_tabref>).
    IF sy-subrc = 0.
      ASSIGN <lr_tabref>->* TO <lt_capa_root>.
      IF sy-subrc = 0.
        rt_root = <lt_capa_root>.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_locid2.
    DATA(lv_tor_id)  = i_tor_id.
    SHIFT lv_tor_id LEFT DELETING LEADING '0'.
    DATA(lv_stop_id) = i_seq_num+1(4).
    rv_locid2 = lv_tor_id && lv_stop_id.
  ENDMETHOD.

  METHOD get_execution.
    FIELD-SYMBOLS <lt_execution> TYPE /scmtms/t_em_bo_tor_execinfo.
    CLEAR et_execution.
    ASSIGN i_all_appl_tables[ tabledef = SWITCH #( iv_old WHEN abap_false
                                                             THEN lif_actual_event~cs_tabledef-tor_execution_info
                                                          ELSE  lif_actual_event~cs_tabledef-tor_execution_info_before )
                             ]-tableref TO FIELD-SYMBOL(<lr_tabref>).
    IF sy-subrc = 0.
      ASSIGN <lr_tabref>->* TO <lt_execution>.
      IF sy-subrc = 0.
        et_execution = <lt_execution>.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_model_event_id.
    CASE iv_standard_event_id.
      WHEN lif_actual_event~cs_event_id-standard-arrival.
        rv_model_event_id = lif_actual_event~cs_event_id-model-shp_arrival.
      WHEN lif_actual_event~cs_event_id-standard-departure.
        rv_model_event_id = lif_actual_event~cs_event_id-model-shp_departure.
      WHEN lif_actual_event~cs_event_id-standard-pod.
        rv_model_event_id = lif_actual_event~cs_event_id-model-shp_pod.
      WHEN lif_actual_event~cs_event_id-standard-popu.
        rv_model_event_id = lif_actual_event~cs_event_id-model-popu.
      WHEN lif_actual_event~cs_event_id-standard-load_begin.
        rv_model_event_id = lif_actual_event~cs_event_id-model-load_start.
      WHEN lif_actual_event~cs_event_id-standard-load_end.
        rv_model_event_id = lif_actual_event~cs_event_id-model-load_end.
      WHEN lif_actual_event~cs_event_id-standard-coupling.
        rv_model_event_id = lif_actual_event~cs_event_id-model-coupling.
      WHEN lif_actual_event~cs_event_id-standard-decoupling.
        rv_model_event_id = lif_actual_event~cs_event_id-model-decoupling.
      WHEN lif_actual_event~cs_event_id-standard-unload_begin.
        rv_model_event_id = lif_actual_event~cs_event_id-model-unload_begin.
      WHEN lif_actual_event~cs_event_id-standard-unload_end.
        rv_model_event_id = lif_actual_event~cs_event_id-model-unload_end.
      WHEN lif_actual_event~cs_event_id-standard-delay.
        rv_model_event_id = lif_actual_event~cs_event_id-model-delay.
      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_fo_actual_event IMPLEMENTATION.
  METHOD lif_actual_event~check_tor_type_specific_events.
    IF iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-departure    AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-arriv_dest   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-popu         AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-pod          AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-load_begin   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-load_end     AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-unload_begin AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-unload_end   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-coupling     AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-decoupling   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-delay.
      e_result = lif_ef_constants=>cs_condition-false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_fb_actual_event IMPLEMENTATION.
  METHOD lif_actual_event~check_tor_type_specific_events.
    IF iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-departure    AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-arriv_dest   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-popu         AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-pod          AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-load_begin   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-load_end     AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-unload_begin AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-unload_end   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-delay.
      e_result = lif_ef_constants=>cs_condition-false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_fu_actual_event IMPLEMENTATION.
  METHOD lif_actual_event~check_tor_type_specific_events.
    IF iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-departure    AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-arriv_dest   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-popu         AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-pod          AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-load_begin   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-load_end     AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-unload_begin AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-unload_end   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-coupling     AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-decoupling   AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-delay        AND
       iv_event_code <> /scmtms/if_tor_const=>sc_tor_event-delay_fu.
      e_result = lif_ef_constants=>cs_condition-false.
    ENDIF.
  ENDMETHOD.

  METHOD lif_actual_event~adjust_ae_location_data.

    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    DATA: lv_stop_category   TYPE /scmtms/tor_category,
          ls_trackparameters TYPE /saptrx/bapi_evm_parameters.

    DATA(lt_stop) = get_stop( i_all_appl_tables ).
    DATA(lt_capa_stop) = get_capa_stop( i_all_appl_tables ).
    DATA(lt_capa_root) = get_capa_root( i_all_appl_tables ).

    ASSIGN i_event-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN ct_trackingheader[ trxid  = <ls_root>-tor_id ] TO FIELD-SYMBOL(<ls_trackingheader>).
    IF sy-subrc = 0.
      <ls_trackingheader>-trxcod = lif_actual_event~cs_trxcode-freight_unit.
      <ls_trackingheader>-evtid  = get_model_event_id( <ls_trackingheader>-evtid ).
    ENDIF.

    CLEAR ls_trackparameters.
    ls_trackparameters-evtcnt      = <ls_trackingheader>-evtcnt.
    ls_trackparameters-param_name  = lif_ef_constants=>cs_system_fields-actual_technical_timezone.
    ls_trackparameters-param_value = lcl_tools=>get_system_time_zone( ).
    APPEND ls_trackparameters TO ct_trackparameters.

    CLEAR ls_trackparameters.
    ls_trackparameters-evtcnt      = <ls_trackingheader>-evtcnt.
    ls_trackparameters-param_name  = lif_ef_constants=>cs_system_fields-actual_technical_datetime.
    ls_trackparameters-param_value = lcl_tools=>get_system_date_time( ).
    APPEND ls_trackparameters TO ct_trackparameters.

    ASSIGN ct_tracklocation[ evtcnt = <ls_trackingheader>-evtcnt ] TO FIELD-SYMBOL(<ls_tracklocation>).
    IF sy-subrc = 0.
      <ls_tracklocation>-loccod = lif_actual_event~cs_location_type-logistic.

      LOOP AT lt_stop ASSIGNING FIELD-SYMBOL(<ls_stop>) USING KEY parent_seqnum WHERE parent_node_id = <ls_root>-node_id.
        IF sy-tabix = 1.
          DATA(lv_first_location) = <ls_stop>-log_locid.
        ENDIF.
        DATA(lv_last_location) = <ls_stop>-log_locid.
      ENDLOOP.

      IF <ls_tracklocation>-locid1 = lv_first_location OR <ls_tracklocation>-locid1 = lv_last_location.
        ASSIGN lt_stop[ parent_node_id = <ls_root>-node_id
                        log_locid      = <ls_tracklocation>-locid1 ] TO <ls_stop>.
      ELSE.
        IF iv_event_code = /scmtms/if_tor_const=>sc_tor_event-departure  OR
           iv_event_code = /scmtms/if_tor_const=>sc_tor_event-popu       OR
           iv_event_code = /scmtms/if_tor_const=>sc_tor_event-load_begin OR
           iv_event_code = /scmtms/if_tor_const=>sc_tor_event-load_end   OR
           iv_event_code = /scmtms/if_tor_const=>sc_tor_event-coupling   OR
           iv_event_code = /scmtms/if_tor_const=>sc_tor_event-delay.
          lv_stop_category = /scmtms/if_tor_const=>sc_tor_stop_cat-outbound.
        ELSEIF iv_event_code = /scmtms/if_tor_const=>sc_tor_event-arriv_dest   OR
               iv_event_code = /scmtms/if_tor_const=>sc_tor_event-pod          OR
               iv_event_code = /scmtms/if_tor_const=>sc_tor_event-unload_begin OR
               iv_event_code = /scmtms/if_tor_const=>sc_tor_event-unload_end   OR
               iv_event_code = /scmtms/if_tor_const=>sc_tor_event-decoupling.
          lv_stop_category = /scmtms/if_tor_const=>sc_tor_stop_cat-inbound.
        ENDIF.
        ASSIGN lt_stop[ parent_node_id = <ls_root>-node_id
                        log_locid      = <ls_tracklocation>-locid1
                        stop_cat       = lv_stop_category ] TO <ls_stop>.
      ENDIF.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      IF iv_event_code = /scmtms/if_tor_const=>sc_tor_event-delay.
        ASSIGN ct_trackparameters[
                  param_name = lif_ef_constants=>cs_parameter-ref_planned_event_milestone
                  evtcnt     = <ls_trackingheader>-evtcnt ] TO FIELD-SYMBOL(<ls_track_param>).
        IF sy-subrc = 0.
          DATA(lv_reference_event) = <ls_track_param>-param_value.
        ENDIF.
      ENDIF.

      IF iv_event_code = /scmtms/if_tor_const=>sc_tor_event-pod OR
         lv_reference_event = /scmtms/if_tor_const=>sc_tor_event-pod.
        <ls_tracklocation>-locid2 = <ls_root>-tor_id.
      ELSE.
        <ls_tracklocation>-locid2 = lcl_tools=>get_capa_match_key(
                                        iv_assgn_stop_key = <ls_stop>-assgn_stop_key
                                        it_capa_stop      = lt_capa_stop
                                        it_capa_root      = lt_capa_root ).
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
