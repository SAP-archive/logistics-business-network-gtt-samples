*&---------------------------------------------------------------------*
*& Local class definition - Actual Events Fillers
*&---------------------------------------------------------------------*

**********************************************************************
*** Purchase Order Item Confirmation Event ***************************
**********************************************************************
CLASS lcl_ae_filler_fo_arrival DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ae_filler.

    METHODS constructor
      IMPORTING
        io_ae_parameters TYPE REF TO lif_ae_parameters.

  PRIVATE SECTION.
    DATA: mo_ae_parameters    TYPE REF TO lif_ae_parameters.

    METHODS get_confirmation_quantity
      IMPORTING
        ir_ekpo         TYPE REF TO data
        ir_ekes         TYPE REF TO data
      RETURNING
        VALUE(rv_menge) TYPE menge_d
      RAISING
        cx_udm_message.

    METHODS get_confirmation_quantity_diff
      IMPORTING
        is_events            TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_difference) TYPE menge_d
      RAISING
        cx_udm_message.

    METHODS is_appropriate_conf_type
      IMPORTING
        iv_ebtyp         TYPE clike
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_appropriate_conf_control
      IMPORTING
        iv_bstae         TYPE clike
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS has_changes
      IMPORTING
        is_events        TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE lif_ef_types=>tv_condition
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_ae_filler_fo_arrival IMPLEMENTATION.
  METHOD constructor.
    mo_ae_parameters  = io_ae_parameters.
  ENDMETHOD.

  METHOD has_changes.
    DATA(lv_difference)   = get_confirmation_quantity_diff(
                              is_events = is_events ).

    "TBD : check ekes for changes

    rv_result   = COND #( WHEN lv_difference <> 0
                            THEN lif_ef_constants=>cs_condition-true
                            ELSE lif_ef_constants=>cs_condition-false ).
  ENDMETHOD.

  METHOD lif_ae_filler~check_relevance.
    DATA: lv_difference    TYPE menge_d.

    rv_result   = lif_ef_constants=>cs_condition-false.

*    IF is_events-maintabdef = lif_sst_constants=>cs_tabledef-fo_header_new AND
*       lcl_fo_tools=>is_appropriate_po_type( ir_struct = is_events-mastertabref ) = abap_true.
*
*      CASE is_events-update_indicator.
*        WHEN lif_ef_constants=>cs_change_mode-insert.
**          lv_mng_new  = get_confirmation_quantity(
**                          ir_ekpo = is_events-maintabref
**                          ir_ekes = mo_ae_parameters->get_appl_table(
**                                      iv_tabledef = lif_pof_constants=>cs_tabledef-po_vend_conf_new ) ).
*          lv_difference = get_confirmation_quantity_diff(
*                                 is_events = is_events ).
*
*          rv_result   = COND #( WHEN lv_difference > 0
*                                  THEN lif_ef_constants=>cs_condition-true
*                                  ELSE lif_ef_constants=>cs_condition-false ).
*
*        WHEN lif_ef_constants=>cs_change_mode-update OR
*             lif_ef_constants=>cs_change_mode-undefined.
*
*          rv_result   = has_changes( is_events = is_events ).
*      ENDCASE.
*    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~get_event_data.
    DATA(lv_difference)   =  get_confirmation_quantity_diff(
                                 is_events = is_events ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
*      trxid       = lcl_fo_tools=>get_tracking_id_fo_item(
*                      ir_ekpo = is_events-maintabref )
      trxcod      = lif_sst_constants=>cs_trxcod-fo_number
      evtcnt      = is_events-eventid
*      evtid       = lif_pof_constants=>cs_milestone-po_confirmation
      evtdat      = sy-datum
      evttim      = sy-uzeit
      evtzon      = lcl_tools=>get_system_time_zone( )
    ) ).

    ct_eventid_map  = VALUE #( BASE ct_eventid_map (
      eventid     = is_events-eventid
      evtcnt      = is_events-eventid
    ) ).

    ct_tracklocation  = VALUE #( BASE ct_tracklocation (
      evtcnt      = is_events-eventid
*      loccod      = lif_ef_constants=>cs_loc_types-plant
      locid1      = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'WERKS' )
    ) ).

    " QUANTITY
    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
*      param_name  = lif_pof_constants=>cs_event_param-quantity
      param_value = lcl_tools=>get_pretty_value( iv_value = lv_difference )
    ) ).

    " CONFIRMATION TYPE
    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
*      param_name  = lif_pof_constants=>cs_event_param-confirm_type
*      param_value = lif_pof_constants=>cs_relevance-ebtyp
    ) ).
  ENDMETHOD.

  METHOD get_confirmation_quantity.
    DATA: lv_has_conf TYPE abap_bool,
          lv_dummy    TYPE char100.

    TYPES: tt_ekes    TYPE STANDARD TABLE OF uekes.

    FIELD-SYMBOLS: <ls_ekpo> TYPE uekpo,
                   <lt_ekes> TYPE tt_ekes,
                   <ls_ekes> TYPE uekes.

    CLEAR rv_menge.

    ASSIGN ir_ekpo->* TO <ls_ekpo>.
    ASSIGN ir_ekes->* TO <lt_ekes>.

    IF <ls_ekpo> IS ASSIGNED AND
       <lt_ekes> IS ASSIGNED.

      IF is_appropriate_conf_control( iv_bstae = <ls_ekpo>-bstae ) = abap_true.
*         <ls_ekpo>-kzabs IS NOT INITIAL.   "TBD

        LOOP AT <lt_ekes> ASSIGNING <ls_ekes>
          WHERE ebeln = <ls_ekpo>-ebeln
            AND ebelp = <ls_ekpo>-ebelp.

          IF is_appropriate_conf_type( iv_ebtyp = <ls_ekes>-ebtyp ) = abap_true.
            ADD <ls_ekes>-menge TO rv_menge.
          ENDIF.
        ENDLOOP.

        rv_menge    = COND #( WHEN sy-subrc <> 0
                                THEN <ls_ekpo>-menge ELSE rv_menge ).
      ENDIF.

    ELSEIF <ls_ekpo> IS NOT ASSIGNED.
      MESSAGE e002(zpof_gtt) WITH 'EKPO' INTO lv_dummy.
      lcl_tools=>throw_exception( ).

    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKET' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_confirmation_quantity_diff.
*    DATA: lv_mng_old TYPE menge_d VALUE 0,
*          lv_mng_new TYPE menge_d.
*
*    IF is_events-update_indicator <> lif_ef_constants=>cs_change_mode-insert.
*      lv_mng_old  = get_confirmation_quantity(
*                      ir_ekpo = is_events-mainoldtabref
*                      ir_ekes = mo_ae_parameters->get_appl_table(
*                                  iv_tabledef = lif_pof_constants=>cs_tabledef-po_vend_conf_old ) ).
*    ENDIF.
*
*    lv_mng_new  = get_confirmation_quantity(
*                    ir_ekpo = is_events-maintabref
*                    ir_ekes = mo_ae_parameters->get_appl_table(
*                                iv_tabledef = lif_pof_constants=>cs_tabledef-po_vend_conf_new ) ).
*
*    rv_difference = lv_mng_new - lv_mng_old.

  ENDMETHOD.

  METHOD is_appropriate_conf_control.
*    rv_result = boolc( iv_bstae = lif_pof_constants=>cs_bstae-confirm OR
*                       iv_bstae = lif_pof_constants=>cs_bstae-delivery ).
  ENDMETHOD.

  METHOD is_appropriate_conf_type.
*    rv_result = boolc( iv_ebtyp = lif_pof_constants=>cs_relevance-ebtyp ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
*** Purchase Order Item Goods Receipt Event **************************
**********************************************************************
CLASS lcl_ae_filler_fo_departure DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ae_filler.

    METHODS constructor
      IMPORTING
        io_ae_parameters TYPE REF TO lif_ae_parameters.

  PRIVATE SECTION.
    DATA: mo_ae_parameters    TYPE REF TO lif_ae_parameters.

    METHODS get_goods_receipt_quantity
      IMPORTING
        ir_goods_receipt TYPE REF TO data
      RETURNING
        VALUE(rv_menge)  TYPE menge_d
      RAISING
        cx_udm_message.

    METHODS get_goods_receipt_quantity_dif
      IMPORTING
        is_events            TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_difference) TYPE menge_d
      RAISING
        cx_udm_message.

    METHODS is_appropriate_definition
      IMPORTING
        is_events        TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    METHODS is_appropriate_md_type
      IMPORTING
        ir_md_head       TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    METHODS is_appropriate_po_type
      IMPORTING
        ir_md_pos        TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    METHODS is_creation_mode
      IMPORTING
        is_events        TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_ae_filler_fo_departure IMPLEMENTATION.
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
        MESSAGE e001(zpof_gtt) WITH 'ERFMG' 'Goods Receipt' INTO lv_dummy.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'Goods Receipt' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_goods_receipt_quantity_dif.
*    DATA: lv_mng_old TYPE menge_d,
*          lv_mng_new TYPE menge_d.
*
*    lv_mng_old  = get_goods_receipt_quantity(
*                    ir_goods_receipt = is_events-mainoldtabref ).
*
*    lv_mng_new  = get_goods_receipt_quantity(
*                    ir_goods_receipt = is_events-maintabref ).
*
*    rv_difference = lv_mng_new - lv_mng_old.

    rv_difference = get_goods_receipt_quantity(
                      ir_goods_receipt = is_events-maintabref ).

  ENDMETHOD.

  METHOD lif_ae_filler~check_relevance.
    DATA: lv_mng_new    TYPE menge_d.

    rv_result   = lif_ef_constants=>cs_condition-false.

    " mkpf-blart        = 'WE'
    " ekko-bsart        = 'NB'
    " update_indicator  = insert
    IF is_appropriate_definition( is_events = is_events ) = abap_true AND
       is_appropriate_md_type( ir_md_head = is_events-mastertabref ) = abap_true AND
       is_appropriate_po_type( ir_md_pos  = is_events-maintabref )   = abap_true AND
       is_creation_mode( is_events = is_events ) = abap_true.

      lv_mng_new  = get_goods_receipt_quantity(
                      ir_goods_receipt = is_events-maintabref ).

      rv_result   = COND #( WHEN lv_mng_new <> 0
                              THEN lif_ef_constants=>cs_condition-true
                              ELSE lif_ef_constants=>cs_condition-false ).
    ENDIF.
  ENDMETHOD.

  METHOD is_appropriate_definition.
*    rv_result = boolc( is_events-maintabdef = lif_pof_constants=>cs_tabledef-md_material_segment ).
  ENDMETHOD.

  METHOD is_appropriate_md_type.
    DATA: lv_md_type  TYPE mkpf-blart.

    lv_md_type  = lcl_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_head
                    iv_field_name  = 'BLART' ).

*    rv_result   = boolc( lv_md_type = lif_pof_constants=>cs_md_type-goods_receipt ).
  ENDMETHOD.

  METHOD is_appropriate_po_type.
    DATA: lv_ebeln TYPE ekko-ebeln,
          ls_ekko  TYPE ekko.

    lv_ebeln  = lcl_tools=>get_field_of_structure(
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

*    rv_result   = boolc( sy-subcs = 0 AND
*                         ls_ekko-bsart = lif_pof_constants=>cs_relevance-bsart ).
  ENDMETHOD.

  METHOD is_creation_mode.
    DATA: lv_mblnr TYPE mkpf-mblnr,
          lv_mjahr TYPE mkpf-mjahr.
*          ls_mkpf  TYPE mkpf.

    IF is_events-update_indicator = lif_ef_constants=>cs_change_mode-insert.
      rv_result = abap_true.
    ELSEIF is_events-update_indicator = lif_ef_constants=>cs_change_mode-undefined.
      lv_mblnr  = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'MBLNR' ).

      lv_mjahr  = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'MJAHR' ).

      SELECT SINGLE mblnr INTO lv_mblnr   "TBD replace with FM or bapi
        FROM mkpf
        WHERE mblnr = lv_mblnr
          AND mjahr = lv_mjahr.

      rv_result = boolc( sy-subrc <> 0 ).
    ELSE.
      rv_result = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~get_event_data.
    DATA(lv_difference)   =  get_goods_receipt_quantity_dif(
                               is_events = is_events ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
*      trxid       = lcl_po_tools=>get_tracking_id_po_item(
*                      ir_ekpo = is_events-maintabref )      "MSEG contains EBELN/EBELP
*      trxcod      = lif_pof_constants=>cs_trxcod-po_position
      evtcnt      = is_events-eventid
*      evtid       = lif_pof_constants=>cs_milestone-po_goods_receipt
      evtdat      = sy-datum
      evttim      = sy-uzeit
      evtzon      = lcl_tools=>get_system_time_zone( )
    ) ).

    ct_eventid_map  = VALUE #( BASE ct_eventid_map (
      eventid     = is_events-eventid
      evtcnt      = is_events-eventid
    ) ).

    ct_tracklocation  = VALUE #( BASE ct_tracklocation (
      evtcnt      = is_events-eventid
*      loccod      = lif_ef_constants=>cs_loc_types-plant
      locid1      = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'WERKS' )
    ) ).

    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
*      param_name  = lif_pof_constants=>cs_event_param-quantity
      param_value = lcl_tools=>get_pretty_value( iv_value = lv_difference )
    ) ).
  ENDMETHOD.
ENDCLASS.
