CLASS zcl_gtt_pof_ae_filler_dlh_gr DEFINITION
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
    BEGIN OF ts_dl_item_id,
             vbeln TYPE vbeln_vl,
             posnr TYPE posnr_vl,
           END OF ts_dl_item_id .
  TYPES:
    tt_vbeln    TYPE STANDARD TABLE OF vbeln_vl .

  DATA mo_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters .

  METHODS get_delivery_ids
    IMPORTING
      !ir_md_pos TYPE REF TO data
    EXPORTING
      !et_vbeln TYPE tt_vbeln
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
  METHODS is_appropriate_dl_item
    IMPORTING
      !ir_md_pos TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  METHODS is_appropriate_dl_type
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
  METHODS is_reveral_document
    IMPORTING
      !ir_md_pos TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_ae_filler_dlh_gr IMPLEMENTATION.


  METHOD constructor.

    mo_ae_parameters  = io_ae_parameters.

  ENDMETHOD.


  METHOD get_delivery_ids.

    DATA: lv_vbeln    TYPE vbeln_vl.

    FIELD-SYMBOLS: <lt_mseg>    TYPE zif_gtt_pof_app_types=>tt_mseg.

    CLEAR: et_vbeln[].

    ASSIGN ir_md_pos->* TO <lt_mseg>.

    IF <lt_mseg> IS ASSIGNED.
      LOOP AT <lt_mseg> ASSIGNING FIELD-SYMBOL(<ls_mseg>)
        WHERE vbeln_im IS NOT INITIAL.

        IF NOT line_exists( et_vbeln[ table_line = <ls_mseg>-vbeln_im ] ).
          APPEND <ls_mseg>-vbeln_im TO et_vbeln.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'MSEG' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_appropriate_definition.

    rv_result = boolc( is_events-maintabdef = zif_gtt_pof_app_constants=>cs_tabledef-md_material_header ).

  ENDMETHOD.


  METHOD is_appropriate_dl_item.

    DATA: lt_vbeln TYPE tt_vbeln,
          lt_lips  TYPE STANDARD TABLE OF lips.

    rv_result   = abap_false.

    get_delivery_ids(
      EXPORTING
        ir_md_pos = ir_md_pos
      IMPORTING
        et_vbeln  = lt_vbeln ).

    IF lt_vbeln[] IS NOT INITIAL.
      SELECT *
        INTO TABLE lt_lips
        FROM lips
        FOR ALL ENTRIES IN lt_vbeln
        WHERE vbeln = lt_vbeln-table_line.

      LOOP AT lt_lips ASSIGNING FIELD-SYMBOL(<ls_lips>).
        rv_result   = boolc( sy-subrc = 0 AND
                             zcl_gtt_pof_dl_tools=>is_appropriate_dl_item(
                               ir_struct = REF #( <ls_lips> ) ) = abap_true ).
        IF rv_result = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD is_appropriate_dl_type.

    DATA: lt_vbeln TYPE tt_vbeln,
          lt_likp  TYPE STANDARD TABLE OF likp.

    rv_result   = abap_false.

    get_delivery_ids(
      EXPORTING
        ir_md_pos = ir_md_pos
      IMPORTING
        et_vbeln  = lt_vbeln ).

    IF lt_vbeln[] IS NOT INITIAL.
      SELECT *
        INTO TABLE lt_likp
        FROM likp
        FOR ALL ENTRIES IN lt_vbeln
        WHERE vbeln = lt_vbeln-table_line.

      LOOP AT lt_likp ASSIGNING FIELD-SYMBOL(<ls_likp>).
        rv_result   = boolc( sy-subrc = 0 AND
                             zcl_gtt_pof_dl_tools=>is_appropriate_dl_type(
                               ir_struct = REF #( <ls_likp> ) ) = abap_true ).
        IF rv_result = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD is_appropriate_md_type.

    DATA: lv_md_type  TYPE mkpf-blart.

    lv_md_type  = zcl_gtt_pof_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_head
                    iv_field_name  = 'BLART' ).

    rv_result   = boolc( lv_md_type = zif_gtt_pof_app_constants=>cs_md_type-goods_receipt ).

  ENDMETHOD.


  METHOD is_creation_mode.

    DATA: lv_mblnr TYPE mkpf-mblnr,
          lv_mjahr TYPE mkpf-mjahr.

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


  METHOD is_reveral_document.

    DATA: lv_vbeln    TYPE vbeln_vl.

    FIELD-SYMBOLS: <lt_mseg>    TYPE zif_gtt_pof_app_types=>tt_mseg.

    ASSIGN ir_md_pos->* TO <lt_mseg>.

    IF <lt_mseg> IS ASSIGNED.
      READ TABLE <lt_mseg> ASSIGNING FIELD-SYMBOL(<ls_mseg>) INDEX 1.

      rv_result = boolc( sy-subrc = 0 AND <ls_mseg>-smbln IS NOT INITIAL ).

    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'MSEG' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~check_relevance.

    DATA: lv_mng_new    TYPE menge_d.

    DATA(lr_md_pos) = mo_ae_parameters->get_appl_table(
                        iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-md_material_segment ).

    rv_result       = zif_gtt_pof_ef_constants=>cs_condition-false.

    " mkpf-blart        = 'WE'
    " update_indicator  = insert
    IF is_appropriate_definition( is_events = is_events ) = abap_true AND
       is_appropriate_md_type( ir_md_head = is_events-maintabref ) = abap_true AND
       is_appropriate_dl_type( ir_md_pos  = lr_md_pos ) = abap_true AND
       is_appropriate_dl_item( ir_md_pos  = lr_md_pos ) = abap_true AND
       is_creation_mode( is_events = is_events ) = abap_true.

      rv_result   = zif_gtt_pof_ef_constants=>cs_condition-true.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~get_event_data.

    DATA: lt_vbeln    TYPE tt_vbeln.

    DATA(lr_md_pos)   = mo_ae_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-md_material_segment ).

    DATA(lv_reversal) = is_reveral_document( ir_md_pos = lr_md_pos ).

    get_delivery_ids(
      EXPORTING
        ir_md_pos = lr_md_pos
      IMPORTING
        et_vbeln  = lt_vbeln ).

    LOOP AT lt_vbeln ASSIGNING FIELD-SYMBOL(<lv_vbeln>).
      DATA(lv_evtcnt) = zcl_gtt_pof_sh_tools=>get_next_event_counter( ).

      ct_trackingheader = VALUE #( BASE ct_trackingheader (
        language    = sy-langu
        trxid       = <lv_vbeln>
        trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-dl_number
        evtcnt      = lv_evtcnt
        evtid       = zif_gtt_pof_app_constants=>cs_milestone-dl_goods_receipt
        evtdat      = sy-datum
        evttim      = sy-uzeit
        evtzon      = zcl_gtt_pof_tools=>get_system_time_zone( )
      ) ).

      ct_eventid_map  = VALUE #( BASE ct_eventid_map (
        eventid     = is_events-eventid
        evtcnt      = lv_evtcnt
      ) ).

      ct_trackparameters  = VALUE #( BASE ct_trackparameters (
        evtcnt      = lv_evtcnt
        param_name  = zif_gtt_pof_app_constants=>cs_event_param-reversal
        param_value = lv_reversal
      ) ).
    ENDLOOP.


  ENDMETHOD.
ENDCLASS.
