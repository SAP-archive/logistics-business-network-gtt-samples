CLASS zcl_gtt_pof_ae_filler_poi_gr DEFINITION
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

  METHODS get_goods_receipt_quantity
    IMPORTING
      !ir_goods_receipt TYPE REF TO data
    RETURNING
      VALUE(rv_menge) TYPE menge_d
    RAISING
      cx_udm_message .
  METHODS get_goods_receipt_quantity_dif
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_difference) TYPE menge_d
    RAISING
      cx_udm_message .
  METHODS is_appropriate_definition
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  METHODS is_appropriate_md_type
    IMPORTING
      !ir_md_head TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  METHODS is_appropriate_po_item
    IMPORTING
      !ir_md_pos TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  METHODS is_appropriate_po_type
    IMPORTING
      !ir_md_pos TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  METHODS is_creation_for_po_item
    IMPORTING
      !ir_md_pos TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  METHODS is_creation_mode
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_ae_filler_poi_gr IMPLEMENTATION.


  METHOD constructor.

    mo_ae_parameters  = io_ae_parameters.

  ENDMETHOD.


  METHOD get_goods_receipt_quantity.

    DATA: lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <ls_goods_receipt> TYPE any,
                   <lv_quantity>      TYPE any,
                   <lv_sign>          TYPE any.

    ASSIGN ir_goods_receipt->* TO <ls_goods_receipt>.

    IF <ls_goods_receipt> IS ASSIGNED.
      ASSIGN COMPONENT 'ERFMG' OF STRUCTURE <ls_goods_receipt> TO <lv_quantity>.
      ASSIGN COMPONENT 'SHKZG' OF STRUCTURE <ls_goods_receipt> TO <lv_sign>.

      IF <lv_quantity> IS ASSIGNED.
        rv_menge    = COND #( WHEN <lv_sign> = 'H'
                                THEN - <lv_quantity>
                                ELSE <lv_quantity> ).
      ELSE.
        MESSAGE e001(zgtt_pof) WITH 'ERFMG' 'Goods Receipt' INTO lv_dummy ##NO_TEXT.
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'Goods Receipt' INTO lv_dummy ##NO_TEXT.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_goods_receipt_quantity_dif.

    rv_difference = get_goods_receipt_quantity(
                      ir_goods_receipt = is_events-maintabref ).

  ENDMETHOD.


  METHOD is_appropriate_definition.

    rv_result = boolc( is_events-maintabdef = zif_gtt_pof_app_constants=>cs_tabledef-md_material_segment ).

  ENDMETHOD.


  METHOD is_appropriate_md_type.

    DATA: lv_md_type  TYPE mkpf-blart.

    lv_md_type  = zcl_gtt_pof_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_head
                    iv_field_name  = 'BLART' ).

    rv_result   = boolc( lv_md_type = zif_gtt_pof_app_constants=>cs_md_type-goods_receipt ).

  ENDMETHOD.


  METHOD is_appropriate_po_item.

    DATA: ls_ekpo  TYPE ekpo.

    DATA(lv_ebeln)  = CONV ebeln( zcl_gtt_pof_tools=>get_field_of_structure(
                                    ir_struct_data = ir_md_pos
                                    iv_field_name  = 'EBELN' ) ).

    DATA(lv_ebelp)  = CONV ebelp( zcl_gtt_pof_tools=>get_field_of_structure(
                                    ir_struct_data = ir_md_pos
                                    iv_field_name  = 'EBELP' ) ).

    CALL FUNCTION 'ME_EKPO_SINGLE_READ'
      EXPORTING
        pi_ebeln         = lv_ebeln
        pi_ebelp         = lv_ebelp
      IMPORTING
        po_ekpo          = ls_ekpo
      EXCEPTIONS
        no_records_found = 1
        OTHERS           = 2.

    rv_result   = boolc(
      sy-subrc = 0 AND
      zcl_gtt_pof_po_tools=>is_appropriate_po_item( ir_ekpo = REF #( ls_ekpo ) ) = abap_true
    ).

  ENDMETHOD.


  METHOD is_appropriate_po_type.

    DATA: lv_ebeln TYPE ekko-ebeln,
          ls_ekko  TYPE ekko.

    lv_ebeln  = zcl_gtt_pof_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_pos
                    iv_field_name  = 'EBELN' ).

    CALL FUNCTION 'ME_EKKO_SINGLE_READ'
      EXPORTING
        pi_ebeln         = lv_ebeln
      IMPORTING
        po_ekko          = ls_ekko
      EXCEPTIONS
        no_records_found = 1
        OTHERS           = 2.

    rv_result   = boolc(
      sy-subrc = 0 AND
      zcl_gtt_pof_po_tools=>is_appropriate_po_type( ir_ekko = REF #( ls_ekko ) ) = abap_true
    ).

  ENDMETHOD.


  METHOD is_creation_for_po_item.

    DATA: lv_ebeln TYPE ekko-ebeln,
          lv_vbeln TYPE mseg-vbeln_im.

    lv_ebeln  = zcl_gtt_pof_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_pos
                    iv_field_name  = 'EBELN' ).

    lv_vbeln  = zcl_gtt_pof_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_pos
                    iv_field_name  = 'VBELN_IM' ).

    rv_result = boolc( lv_ebeln IS NOT INITIAL AND
                       lv_vbeln IS INITIAL ).

  ENDMETHOD.


  METHOD is_creation_mode.

    DATA: lv_mblnr TYPE mkpf-mblnr,
          lv_mjahr TYPE mkpf-mjahr.
*          ls_mkpf  TYPE mkpf.

    IF is_events-update_indicator = zif_gtt_pof_ef_constants=>cs_change_mode-insert.
      rv_result = abap_true.
    ELSEIF is_events-update_indicator = zif_gtt_pof_ef_constants=>cs_change_mode-undefined.
      lv_mblnr  = zcl_gtt_pof_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'MBLNR' ).

      lv_mjahr  = zcl_gtt_pof_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'MJAHR' ).

      SELECT SINGLE mblnr INTO lv_mblnr
        FROM mkpf
        WHERE mblnr = lv_mblnr
          AND mjahr = lv_mjahr.

      rv_result = boolc( sy-subrc <> 0 ).
    ELSE.
      rv_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~check_relevance.

    DATA: lv_mng_new    TYPE menge_d.

    rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.

    " mkpf-blart        = 'WE'
    " ekko-bsart        = 'NB'
    " ekpo-wepos        = 'X'
    " update_indicator  = insert
    IF is_appropriate_definition( is_events = is_events ) = abap_true AND
       is_appropriate_md_type( ir_md_head = is_events-mastertabref ) = abap_true AND
*       is_creation_for_po_item( ir_md_pos = is_events-maintabref )   = abap_true AND
       is_appropriate_po_type( ir_md_pos  = is_events-maintabref )   = abap_true AND
       is_appropriate_po_item( ir_md_pos  = is_events-maintabref )   = abap_true AND
       is_creation_mode( is_events = is_events ) = abap_true.

      lv_mng_new  = get_goods_receipt_quantity(
                      ir_goods_receipt = is_events-maintabref ).

      rv_result   = COND #( WHEN lv_mng_new <> 0
                              THEN zif_gtt_pof_ef_constants=>cs_condition-true
                              ELSE zif_gtt_pof_ef_constants=>cs_condition-false ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~get_event_data.

    DATA(lv_difference)   =  get_goods_receipt_quantity_dif(
                               is_events = is_events ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = zcl_gtt_pof_po_tools=>get_tracking_id_po_item(
                      ir_ekpo = is_events-maintabref )      "MSEG contains EBELN/EBELP
      trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-po_position
      evtcnt      = is_events-eventid
      evtid       = zif_gtt_pof_app_constants=>cs_milestone-po_goods_receipt
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
