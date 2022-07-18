CLASS zcl_gtt_pof_ae_filler_poi_del DEFINITION
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

  METHODS is_appropriate_mode
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
ENDCLASS.



CLASS zcl_gtt_pof_ae_filler_poi_del IMPLEMENTATION.


  METHOD constructor.

    mo_ae_parameters  = io_ae_parameters.

  ENDMETHOD.


  METHOD is_appropriate_mode.

    rv_result   = boolc(
      is_events-update_indicator  = zif_gtt_pof_ef_constants=>cs_change_mode-insert OR
      is_events-update_indicator  = zif_gtt_pof_ef_constants=>cs_change_mode-update OR
      is_events-update_indicator  = zif_gtt_pof_ef_constants=>cs_change_mode-undefined ).

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~check_relevance.

    DATA: lv_loekz_old TYPE ekpo-loekz,
          lv_loekz_new TYPE ekpo-loekz.

    rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = zif_gtt_pof_app_constants=>cs_tabledef-po_item_new AND
       zcl_gtt_pof_po_tools=>is_appropriate_po_type( ir_ekko = is_events-mastertabref ) = abap_true AND
       zcl_gtt_pof_po_tools=>is_appropriate_po_item( ir_ekpo = is_events-maintabref ) = abap_true AND
       is_appropriate_mode( is_events = is_events ) = abap_true.

      lv_loekz_old  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = is_events-mainoldtabref
                        iv_field_name  = 'LOEKZ' ).

      lv_loekz_new  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = is_events-maintabref
                        iv_field_name  = 'LOEKZ' ).

      rv_result = COND #( WHEN lv_loekz_new <> lv_loekz_old AND
                             ( lv_loekz_old = zif_gtt_pof_app_constants=>cs_loekz-deleted OR
                               lv_loekz_new = zif_gtt_pof_app_constants=>cs_loekz-deleted )
                            THEN zif_gtt_pof_ef_constants=>cs_condition-true
                            ELSE zif_gtt_pof_ef_constants=>cs_condition-false ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~get_event_data.

    DATA(lv_loekz) = CONV eloek( zcl_gtt_pof_tools=>get_field_of_structure(
                                   ir_struct_data = is_events-maintabref
                                   iv_field_name  = 'LOEKZ' ) ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = zcl_gtt_pof_po_tools=>get_tracking_id_po_item(
                      ir_ekpo = is_events-maintabref )
      trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-po_position
      evtcnt      = is_events-eventid
      evtid       = COND #( WHEN lv_loekz = zif_gtt_pof_app_constants=>cs_loekz-deleted
                              THEN zif_gtt_pof_app_constants=>cs_milestone-po_deletion
                              ELSE zif_gtt_pof_app_constants=>cs_milestone-po_undeletion )
      evtdat      = sy-datum
      evttim      = sy-uzeit
      evtzon      = zcl_gtt_pof_tools=>get_system_time_zone( )
    ) ).

    ct_eventid_map  = VALUE #( BASE ct_eventid_map (
      eventid     = is_events-eventid
      evtcnt      = is_events-eventid
    ) ).

  ENDMETHOD.
ENDCLASS.
