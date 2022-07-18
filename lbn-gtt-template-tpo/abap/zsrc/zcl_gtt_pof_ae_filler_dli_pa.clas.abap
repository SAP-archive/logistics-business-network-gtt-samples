CLASS zcl_gtt_pof_ae_filler_dli_pa DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_ae_filler .

  METHODS constructor
    IMPORTING
      !io_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters .
PROTECTED SECTION.
PRIVATE SECTION.

  DATA mo_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters .

  METHODS get_put_away_quantity
    IMPORTING
      !ir_data TYPE REF TO data
    RETURNING
      VALUE(rv_menge) TYPE menge_d
    RAISING
      cx_udm_message .
  METHODS get_put_away_quantity_diff
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_difference) TYPE menge_d
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_ae_filler_dli_pa IMPLEMENTATION.


  METHOD constructor.

    mo_ae_parameters  = io_ae_parameters.

  ENDMETHOD.


  METHOD get_put_away_quantity.

    rv_menge    = zcl_gtt_pof_tools=>get_field_of_structure(
                    ir_struct_data = ir_data
                    iv_field_name  = 'PIKMG' ).

  ENDMETHOD.


  METHOD get_put_away_quantity_diff.

    DATA: lv_mng_old TYPE menge_d VALUE 0,
          lv_mng_new TYPE menge_d.

    IF is_events-update_indicator <> zif_gtt_pof_ef_constants=>cs_change_mode-insert.
      lv_mng_old  = get_put_away_quantity( ir_data = is_events-mainoldtabref ).
    ENDIF.

    lv_mng_new  = get_put_away_quantity( ir_data = is_events-maintabref ).

    rv_difference = lv_mng_new - lv_mng_old.


  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~check_relevance.

    DATA: lv_difference    TYPE menge_d.

    rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = zif_gtt_pof_app_constants=>cs_tabledef-dl_item_new AND
       zcl_gtt_pof_dl_tools=>is_appropriate_dl_type( ir_struct = is_events-mastertabref ) = abap_true AND
       zcl_gtt_pof_dl_tools=>is_appropriate_dl_item( ir_struct = is_events-maintabref ) = abap_true.

      CASE is_events-update_indicator.
        WHEN zif_gtt_pof_ef_constants=>cs_change_mode-insert OR
             zif_gtt_pof_ef_constants=>cs_change_mode-update OR
             zif_gtt_pof_ef_constants=>cs_change_mode-undefined.

          lv_difference = get_put_away_quantity_diff( is_events = is_events ).

          rv_result   = COND #( WHEN lv_difference <> 0
                                  THEN zif_gtt_pof_ef_constants=>cs_condition-true
                                  ELSE zif_gtt_pof_ef_constants=>cs_condition-false ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~get_event_data.

    DATA(lv_difference)   =  get_put_away_quantity_diff(
                               is_events = is_events ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = zcl_gtt_pof_dl_tools=>get_tracking_id_dl_item(
                      ir_lips = is_events-maintabref )
      trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-dl_position
      evtcnt      = is_events-eventid
      evtid       = zif_gtt_pof_app_constants=>cs_milestone-dl_put_away
      evtdat      = sy-datum
      evttim      = sy-uzeit
      evtzon      = zcl_gtt_pof_tools=>get_system_time_zone( )
    ) ).

    ct_eventid_map  = VALUE #( BASE ct_eventid_map (
      eventid     = is_events-eventid
      evtcnt      = is_events-eventid
    ) ).

    ct_tracklocation  = VALUE #( BASE ct_tracklocation (
      evtcnt      = is_events-eventid
      loccod      = zif_gtt_pof_ef_constants=>cs_loc_types-plant
      locid1      = zcl_gtt_pof_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'WERKS' )
    ) ).

    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = zif_gtt_pof_app_constants=>cs_event_param-quantity
      param_value = zcl_gtt_pof_tools=>get_pretty_value( iv_value = lv_difference )
    ) ).

  ENDMETHOD.
ENDCLASS.
