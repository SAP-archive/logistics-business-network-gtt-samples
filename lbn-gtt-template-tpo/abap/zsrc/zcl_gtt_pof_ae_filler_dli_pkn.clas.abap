CLASS zcl_gtt_pof_ae_filler_dli_pkn DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_ae_filler .

  METHODS constructor
    IMPORTING
      !io_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters .
PROTECTED SECTION.
PRIVATE SECTION.

  TYPES:
    tt_vepo            TYPE STANDARD TABLE OF vepovb .

  DATA mo_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters .

  METHODS get_packing_quantity
    IMPORTING
      !ir_lips TYPE REF TO data
      !ir_vepo TYPE REF TO data
    RETURNING
      VALUE(rv_menge) TYPE menge_d
    RAISING
      cx_udm_message .
  METHODS get_packing_quantity_changed
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_changed) TYPE zif_gtt_pof_ef_types=>tv_condition
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_ae_filler_dli_pkn IMPLEMENTATION.


  METHOD constructor.

    mo_ae_parameters  = io_ae_parameters.

  ENDMETHOD.


  METHOD get_packing_quantity.

    DATA: lv_vemng_flo  TYPE vepo-vemng_flo VALUE 0.

    FIELD-SYMBOLS: <lt_vepo> TYPE tt_vepo.

    DATA(lv_vbeln)  = CONV vbeln_vl( zcl_gtt_pof_tools=>get_field_of_structure(
                                       ir_struct_data = ir_lips
                                       iv_field_name  = 'VBELN' ) ).
    DATA(lv_posnr)  = CONV posnr_vl( zcl_gtt_pof_tools=>get_field_of_structure(
                                       ir_struct_data = ir_lips
                                       iv_field_name  = 'POSNR' ) ).

    ASSIGN ir_vepo->* TO <lt_vepo>.

    CLEAR: rv_menge.

    IF <lt_vepo> IS ASSIGNED.
      LOOP AT <lt_vepo> ASSIGNING FIELD-SYMBOL(<ls_vepo>)
        WHERE vbeln = lv_vbeln
          AND posnr = lv_posnr.

        ADD <ls_vepo>-vemng_flo TO lv_vemng_flo.
      ENDLOOP.

      rv_menge  = zcl_gtt_pof_dl_tools=>convert_quantity_into_pounits(
                    iv_quantity_uom = lv_vemng_flo
                    ir_lips         = ir_lips ).
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VEPO' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_packing_quantity_changed.

    FIELD-SYMBOLS: <lt_vepo_old> TYPE tt_vepo,
                   <lt_vepo_new> TYPE tt_vepo.

    IF is_events-update_indicator = zif_gtt_pof_ef_constants=>cs_change_mode-insert.
      DATA(lv_quantity) = get_packing_quantity(
                            ir_lips = is_events-maintabref
                            ir_vepo = mo_ae_parameters->get_appl_table(
                                        iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-dl_hu_item_new ) ).

      rv_changed        = COND #( WHEN lv_quantity > 0
                                    THEN zif_gtt_pof_ef_constants=>cs_condition-true
                                    ELSE zif_gtt_pof_ef_constants=>cs_condition-false ).
    ELSE.
      rv_changed    = zif_gtt_pof_ef_constants=>cs_condition-false.

      DATA(lv_vbeln)  = CONV vbeln_vl( zcl_gtt_pof_tools=>get_field_of_structure(
                                         ir_struct_data = is_events-maintabref
                                         iv_field_name  = 'VBELN' ) ).
      DATA(lv_posnr)  = CONV posnr_vl( zcl_gtt_pof_tools=>get_field_of_structure(
                                         ir_struct_data = is_events-maintabref
                                         iv_field_name  = 'POSNR' ) ).

      DATA(lr_vepo_old) = mo_ae_parameters->get_appl_table(
                            iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-dl_hu_item_old ).
      DATA(lr_vepo_new) = mo_ae_parameters->get_appl_table(
                            iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-dl_hu_item_new ).

      ASSIGN lr_vepo_old->* TO <lt_vepo_old>.
      ASSIGN lr_vepo_new->* TO <lt_vepo_new>.

      IF <lt_vepo_old> IS ASSIGNED AND
         <lt_vepo_new> IS ASSIGNED.

        LOOP AT <lt_vepo_new> ASSIGNING FIELD-SYMBOL(<ls_vepo_new>)
          WHERE vbeln = lv_vbeln
            AND posnr = lv_posnr
            AND updkz IS NOT INITIAL.

          " HU item inserted ?
          IF <ls_vepo_new>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-insert.
            rv_changed  = zif_gtt_pof_ef_constants=>cs_condition-true.
            RETURN.

            " HU item updated ?
          ELSEIF <ls_vepo_new>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-update OR
                 <ls_vepo_new>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-undefined.

            READ TABLE <lt_vepo_old> ASSIGNING FIELD-SYMBOL(<ls_vepo_old>)
              WITH KEY venum = <ls_vepo_new>-venum
                       vepos = <ls_vepo_new>-vepos.

            IF sy-subrc = 0 AND <ls_vepo_new>-vemng_flo <> <ls_vepo_old>-vemng_flo .
              rv_changed  = zif_gtt_pof_ef_constants=>cs_condition-true.
              RETURN.
            ENDIF.
          ENDIF.
        ENDLOOP.

        " HU item deleted ?
        READ TABLE <lt_vepo_old> TRANSPORTING NO FIELDS
          WITH KEY vbeln = lv_vbeln
                   posnr = lv_posnr
                   updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.

        rv_changed  = COND #( WHEN sy-subrc = 0
                                THEN zif_gtt_pof_ef_constants=>cs_condition-true
                                ELSE zif_gtt_pof_ef_constants=>cs_condition-false ).
      ELSE.
        MESSAGE e002(zgtt_pof) WITH 'VEPO' INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

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

          rv_result   = get_packing_quantity_changed( is_events = is_events ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~get_event_data.

    DATA(lv_quantity)   = get_packing_quantity(
                            ir_lips = is_events-maintabref
                            ir_vepo = mo_ae_parameters->get_appl_table(
                                        iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-dl_hu_item_new ) ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = zcl_gtt_pof_dl_tools=>get_tracking_id_dl_item(
                      ir_lips = is_events-maintabref )
      trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-dl_position
      evtcnt      = is_events-eventid
      evtid       = zif_gtt_pof_app_constants=>cs_milestone-dl_packing
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

    " the WHOLE QUANTITY is sent (delta approach might have calculation mistakes
    " because of quantity conversion
    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = zif_gtt_pof_app_constants=>cs_event_param-quantity
      param_value = zcl_gtt_pof_tools=>get_pretty_value( iv_value = lv_quantity )
    ) ).

  ENDMETHOD.
ENDCLASS.
