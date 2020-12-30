*&---------------------------------------------------------------------*
*& Local class definition - Actual Events Fillers
*&---------------------------------------------------------------------*

**********************************************************************
*** Purchase Order Item Confirmation Event ***************************
**********************************************************************
CLASS lcl_ae_filler_po_item_conf DEFINITION.
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

CLASS lcl_ae_filler_po_item_conf IMPLEMENTATION.
  METHOD constructor.
    mo_ae_parameters  = io_ae_parameters.
  ENDMETHOD.

  METHOD has_changes.
    DATA(lv_difference)   = get_confirmation_quantity_diff(
                              is_events = is_events ).

    rv_result   = COND #( WHEN lv_difference <> 0
                            THEN lif_ef_constants=>cs_condition-true
                            ELSE lif_ef_constants=>cs_condition-false ).
  ENDMETHOD.

  METHOD lif_ae_filler~check_relevance.
    DATA: lv_difference    TYPE menge_d.

    rv_result   = lif_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = lif_pof_constants=>cs_tabledef-po_item_new AND
       lcl_po_tools=>is_appropriate_po_type( ir_ekko = is_events-mastertabref ) = abap_true AND
       lcl_po_tools=>is_appropriate_po_item( ir_ekpo = is_events-maintabref ) = abap_true.

      CASE is_events-update_indicator.
        WHEN lif_ef_constants=>cs_change_mode-insert.
          lv_difference = get_confirmation_quantity_diff(
                                 is_events = is_events ).

          rv_result   = COND #( WHEN lv_difference <> 0
                                  THEN lif_ef_constants=>cs_condition-true
                                  ELSE lif_ef_constants=>cs_condition-false ).

        WHEN lif_ef_constants=>cs_change_mode-update OR
             lif_ef_constants=>cs_change_mode-undefined.

          rv_result   = has_changes( is_events = is_events ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~get_event_data.
    DATA(lv_difference)   =  get_confirmation_quantity_diff(
                                 is_events = is_events ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = lcl_po_tools=>get_tracking_id_po_item(
                      ir_ekpo = is_events-maintabref )
      trxcod      = lif_pof_constants=>cs_trxcod-po_position
      evtcnt      = is_events-eventid
      evtid       = lif_pof_constants=>cs_milestone-po_confirmation
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
      loccod      = lif_ef_constants=>cs_loc_types-plant
      locid1      = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'WERKS' )
    ) ).

    " QUANTITY
    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = lif_pof_constants=>cs_event_param-quantity
      param_value = lcl_tools=>get_pretty_value( iv_value = lv_difference )
    ) ).

    " CONFIRMATION TYPE
    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = lif_pof_constants=>cs_event_param-confirm_type
      param_value = lif_pof_constants=>cs_relevance-ebtyp
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
    DATA: lv_mng_old TYPE menge_d VALUE 0,
          lv_mng_new TYPE menge_d.

    IF is_events-update_indicator <> lif_ef_constants=>cs_change_mode-insert.
      lv_mng_old  = get_confirmation_quantity(
                      ir_ekpo = is_events-mainoldtabref
                      ir_ekes = mo_ae_parameters->get_appl_table(
                                  iv_tabledef = lif_pof_constants=>cs_tabledef-po_vend_conf_old ) ).
    ENDIF.

    lv_mng_new  = get_confirmation_quantity(
                    ir_ekpo = is_events-maintabref
                    ir_ekes = mo_ae_parameters->get_appl_table(
                                iv_tabledef = lif_pof_constants=>cs_tabledef-po_vend_conf_new ) ).

    rv_difference = lv_mng_new - lv_mng_old.

  ENDMETHOD.

  METHOD is_appropriate_conf_control.
    rv_result = boolc( iv_bstae = lif_pof_constants=>cs_bstae-confirm OR
                       iv_bstae = lif_pof_constants=>cs_bstae-delivery ).
  ENDMETHOD.

  METHOD is_appropriate_conf_type.
    rv_result = boolc( iv_ebtyp = lif_pof_constants=>cs_relevance-ebtyp ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
*** Purchase Order Item Goods Receipt Event **************************
**********************************************************************
CLASS lcl_ae_filler_po_item_gr DEFINITION.
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

    METHODS is_appropriate_po_item
      IMPORTING
        ir_md_pos        TYPE REF TO data
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

    METHODS is_creation_for_po_item
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

CLASS lcl_ae_filler_po_item_gr IMPLEMENTATION.
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
        MESSAGE e001(zpof_gtt) WITH 'ERFMG' 'Goods Receipt' INTO lv_dummy ##NO_TEXT.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'Goods Receipt' INTO lv_dummy ##NO_TEXT.
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
    " ekpo-wepos        = 'X'
    " update_indicator  = insert
    IF is_appropriate_definition( is_events = is_events ) = abap_true AND
       is_appropriate_md_type( ir_md_head = is_events-mastertabref ) = abap_true AND
       is_creation_for_po_item( ir_md_pos = is_events-maintabref )   = abap_true AND
       is_appropriate_po_type( ir_md_pos  = is_events-maintabref )   = abap_true AND
       is_appropriate_po_item( ir_md_pos  = is_events-maintabref )   = abap_true AND
       is_creation_mode( is_events = is_events ) = abap_true.

      lv_mng_new  = get_goods_receipt_quantity(
                      ir_goods_receipt = is_events-maintabref ).

      rv_result   = COND #( WHEN lv_mng_new <> 0
                              THEN lif_ef_constants=>cs_condition-true
                              ELSE lif_ef_constants=>cs_condition-false ).
    ENDIF.
  ENDMETHOD.

  METHOD is_appropriate_definition.
    rv_result = boolc( is_events-maintabdef = lif_pof_constants=>cs_tabledef-md_material_segment ).
  ENDMETHOD.

  METHOD is_appropriate_md_type.
    DATA: lv_md_type  TYPE mkpf-blart.

    lv_md_type  = lcl_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_head
                    iv_field_name  = 'BLART' ).

    rv_result   = boolc( lv_md_type = lif_pof_constants=>cs_md_type-goods_receipt ).
  ENDMETHOD.

  METHOD is_appropriate_po_item.
    DATA: ls_ekpo  TYPE ekpo.

    DATA(lv_ebeln)  = CONV ebeln( lcl_tools=>get_field_of_structure(
                                    ir_struct_data = ir_md_pos
                                    iv_field_name  = 'EBELN' ) ).

    DATA(lv_ebelp)  = CONV ebelp( lcl_tools=>get_field_of_structure(
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
      lcl_po_tools=>is_appropriate_po_item( ir_ekpo = REF #( ls_ekpo ) ) = abap_true
    ).
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

    rv_result   = boolc(
      sy-subrc = 0 AND
      lcl_po_tools=>is_appropriate_po_type( ir_ekko = REF #( ls_ekko ) ) = abap_true
    ).
  ENDMETHOD.

  METHOD is_creation_for_po_item.
    DATA: lv_ebeln TYPE ekko-ebeln,
          lv_vbeln TYPE mseg-vbeln_im.

    lv_ebeln  = lcl_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_pos
                    iv_field_name  = 'EBELN' ).

    lv_vbeln  = lcl_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_pos
                    iv_field_name  = 'VBELN_IM' ).

    rv_result = boolc( lv_ebeln IS NOT INITIAL AND
                       lv_vbeln IS INITIAL ).
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

      SELECT SINGLE mblnr INTO lv_mblnr
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
      trxid       = lcl_po_tools=>get_tracking_id_po_item(
                      ir_ekpo = is_events-maintabref )      "MSEG contains EBELN/EBELP
      trxcod      = lif_pof_constants=>cs_trxcod-po_position
      evtcnt      = is_events-eventid
      evtid       = lif_pof_constants=>cs_milestone-po_goods_receipt
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
      loccod      = lif_ef_constants=>cs_loc_types-plant
      locid1      = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'WERKS' )
    ) ).

    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = lif_pof_constants=>cs_event_param-quantity
      param_value = lcl_tools=>get_pretty_value( iv_value = lv_difference )
    ) ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Purchase Order Item Deletion Event *******************************
**********************************************************************
CLASS lcl_ae_filler_po_item_del DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ae_filler.

    METHODS constructor
      IMPORTING
        io_ae_parameters TYPE REF TO lif_ae_parameters.

  PRIVATE SECTION.
    DATA: mo_ae_parameters    TYPE REF TO lif_ae_parameters.

    METHODS is_appropriate_mode
      IMPORTING
        is_events        TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

ENDCLASS.

CLASS lcl_ae_filler_po_item_del IMPLEMENTATION.
  METHOD constructor.
    mo_ae_parameters  = io_ae_parameters.
  ENDMETHOD.

  METHOD lif_ae_filler~check_relevance.
    DATA: lv_loekz_old TYPE ekpo-loekz,
          lv_loekz_new TYPE ekpo-loekz.

    rv_result   = lif_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = lif_pof_constants=>cs_tabledef-po_item_new AND
       lcl_po_tools=>is_appropriate_po_type( ir_ekko = is_events-mastertabref ) = abap_true AND
       lcl_po_tools=>is_appropriate_po_item( ir_ekpo = is_events-maintabref ) = abap_true AND
       is_appropriate_mode( is_events = is_events ) = abap_true.

      lv_loekz_old  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = is_events-mainoldtabref
                        iv_field_name  = 'LOEKZ' ).

      lv_loekz_new  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = is_events-maintabref
                        iv_field_name  = 'LOEKZ' ).

      rv_result = COND #( WHEN lv_loekz_new <> lv_loekz_old AND
                             ( lv_loekz_old = lif_pof_constants=>cs_loekz-deleted OR
                               lv_loekz_new = lif_pof_constants=>cs_loekz-deleted )
                            THEN lif_ef_constants=>cs_condition-true
                            ELSE lif_ef_constants=>cs_condition-false ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~get_event_data.
    DATA(lv_loekz) = CONV eloek( lcl_tools=>get_field_of_structure(
                                   ir_struct_data = is_events-maintabref
                                   iv_field_name  = 'LOEKZ' ) ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = lcl_po_tools=>get_tracking_id_po_item(
                      ir_ekpo = is_events-maintabref )
      trxcod      = lif_pof_constants=>cs_trxcod-po_position
      evtcnt      = is_events-eventid
      evtid       = COND #( WHEN lv_loekz = lif_pof_constants=>cs_loekz-deleted
                              THEN lif_pof_constants=>cs_milestone-po_deletion
                              ELSE lif_pof_constants=>cs_milestone-po_undeletion )
      evtdat      = sy-datum
      evttim      = sy-uzeit
      evtzon      = lcl_tools=>get_system_time_zone( )
    ) ).

    ct_eventid_map  = VALUE #( BASE ct_eventid_map (
      eventid     = is_events-eventid
      evtcnt      = is_events-eventid
    ) ).
  ENDMETHOD.

  METHOD is_appropriate_mode.
    rv_result   = boolc(
      is_events-update_indicator  = lif_ef_constants=>cs_change_mode-insert OR
      is_events-update_indicator  = lif_ef_constants=>cs_change_mode-update OR
      is_events-update_indicator  = lif_ef_constants=>cs_change_mode-undefined ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Inbound Delivery Item Put Away Event *****************************
**********************************************************************
CLASS lcl_ae_filler_dl_item_pa DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ae_filler.

    METHODS constructor
      IMPORTING
        io_ae_parameters TYPE REF TO lif_ae_parameters.

  PRIVATE SECTION.
    DATA: mo_ae_parameters    TYPE REF TO lif_ae_parameters.

    METHODS get_put_away_quantity
      IMPORTING
        ir_data         TYPE REF TO data
      RETURNING
        VALUE(rv_menge) TYPE menge_d
      RAISING
        cx_udm_message.

    METHODS get_put_away_quantity_diff
      IMPORTING
        is_events            TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_difference) TYPE menge_d
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_ae_filler_dl_item_pa IMPLEMENTATION.
  METHOD constructor.
    mo_ae_parameters  = io_ae_parameters.
  ENDMETHOD.

  METHOD get_put_away_quantity.
    rv_menge    = lcl_tools=>get_field_of_structure(
                    ir_struct_data = ir_data
                    iv_field_name  = 'PIKMG' ).
  ENDMETHOD.

  METHOD get_put_away_quantity_diff.
    DATA: lv_mng_old TYPE menge_d VALUE 0,
          lv_mng_new TYPE menge_d.

    IF is_events-update_indicator <> lif_ef_constants=>cs_change_mode-insert.
      lv_mng_old  = get_put_away_quantity( ir_data = is_events-mainoldtabref ).
    ENDIF.

    lv_mng_new  = get_put_away_quantity( ir_data = is_events-maintabref ).

    rv_difference = lv_mng_new - lv_mng_old.

  ENDMETHOD.

  METHOD lif_ae_filler~check_relevance.
    DATA: lv_difference    TYPE menge_d.

    rv_result   = lif_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = lif_pof_constants=>cs_tabledef-dl_item_new AND
       lcl_dl_tools=>is_appropriate_dl_type( ir_struct = is_events-mastertabref ) = abap_true AND
       lcl_dl_tools=>is_appropriate_dl_item( ir_struct = is_events-maintabref ) = abap_true.

      CASE is_events-update_indicator.
        WHEN lif_ef_constants=>cs_change_mode-insert OR
             lif_ef_constants=>cs_change_mode-update OR
             lif_ef_constants=>cs_change_mode-undefined.

          lv_difference = get_put_away_quantity_diff( is_events = is_events ).

          rv_result   = COND #( WHEN lv_difference <> 0
                                  THEN lif_ef_constants=>cs_condition-true
                                  ELSE lif_ef_constants=>cs_condition-false ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~get_event_data.
    DATA(lv_difference)   =  get_put_away_quantity_diff(
                               is_events = is_events ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = lcl_dl_tools=>get_tracking_id_dl_item(
                      ir_lips = is_events-maintabref )
      trxcod      = lif_pof_constants=>cs_trxcod-dl_position
      evtcnt      = is_events-eventid
      evtid       = lif_pof_constants=>cs_milestone-dl_put_away
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
      loccod      = lif_ef_constants=>cs_loc_types-plant
      locid1      = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'WERKS' )
    ) ).

    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = lif_pof_constants=>cs_event_param-quantity
      param_value = lcl_tools=>get_pretty_value( iv_value = lv_difference )
    ) ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
*** Inbound Delivery Item Packing Event ******************************
**********************************************************************
CLASS lcl_ae_filler_dl_item_pkng DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ae_filler.

    METHODS constructor
      IMPORTING
        io_ae_parameters TYPE REF TO lif_ae_parameters.

  PRIVATE SECTION.
    TYPES: tt_vepo            TYPE STANDARD TABLE OF vepovb.

    DATA: mo_ae_parameters    TYPE REF TO lif_ae_parameters.

    METHODS get_packing_quantity
      IMPORTING
        ir_lips         TYPE REF TO data
        ir_vepo         TYPE REF TO data
      RETURNING
        VALUE(rv_menge) TYPE menge_d
      RAISING
        cx_udm_message.

    METHODS get_packing_quantity_changed
      IMPORTING
        is_events         TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_changed) TYPE lif_ef_types=>tv_condition
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_ae_filler_dl_item_pkng IMPLEMENTATION.
  METHOD constructor.
    mo_ae_parameters  = io_ae_parameters.
  ENDMETHOD.

  METHOD get_packing_quantity.
    DATA: lv_vemng_flo  TYPE vepo-vemng_flo VALUE 0.

    FIELD-SYMBOLS: <lt_vepo> TYPE tt_vepo.

    DATA(lv_vbeln)  = CONV vbeln_vl( lcl_tools=>get_field_of_structure(
                                       ir_struct_data = ir_lips
                                       iv_field_name  = 'VBELN' ) ).
    DATA(lv_posnr)  = CONV posnr_vl( lcl_tools=>get_field_of_structure(
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

      rv_menge  = lcl_dl_tools=>convert_quantity_into_pounits(
                    iv_quantity_uom = lv_vemng_flo
                    ir_lips         = ir_lips ).
    ELSE.
      MESSAGE E002(ZPOF_GTT) WITH 'VEPO' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_packing_quantity_changed.
    FIELD-SYMBOLS: <lt_vepo_old> TYPE tt_vepo,
                   <lt_vepo_new> TYPE tt_vepo.

    IF is_events-update_indicator = lif_ef_constants=>cs_change_mode-insert.
      DATA(lv_quantity) = get_packing_quantity(
                            ir_lips = is_events-maintabref
                            ir_vepo = mo_ae_parameters->get_appl_table(
                                        iv_tabledef = lif_pof_constants=>cs_tabledef-dl_hu_item_new ) ).

      rv_changed        = COND #( WHEN lv_quantity > 0
                                    THEN lif_ef_constants=>cs_condition-true
                                    ELSE lif_ef_constants=>cs_condition-false ).
    ELSE.
      rv_changed    = lif_ef_constants=>cs_condition-false.

      DATA(lv_vbeln)  = CONV vbeln_vl( lcl_tools=>get_field_of_structure(
                                         ir_struct_data = is_events-maintabref
                                         iv_field_name  = 'VBELN' ) ).
      DATA(lv_posnr)  = CONV posnr_vl( lcl_tools=>get_field_of_structure(
                                         ir_struct_data = is_events-maintabref
                                         iv_field_name  = 'POSNR' ) ).

      DATA(lr_vepo_old) = mo_ae_parameters->get_appl_table(
                            iv_tabledef = lif_pof_constants=>cs_tabledef-dl_hu_item_old ).
      DATA(lr_vepo_new) = mo_ae_parameters->get_appl_table(
                            iv_tabledef = lif_pof_constants=>cs_tabledef-dl_hu_item_new ).

      ASSIGN lr_vepo_old->* TO <lt_vepo_old>.
      ASSIGN lr_vepo_new->* TO <lt_vepo_new>.

      IF <lt_vepo_old> IS ASSIGNED AND
         <lt_vepo_new> IS ASSIGNED.

        LOOP AT <lt_vepo_new> ASSIGNING FIELD-SYMBOL(<ls_vepo_new>)
          WHERE vbeln = lv_vbeln
            AND posnr = lv_posnr
            AND updkz IS NOT INITIAL.

          " HU item inserted ?
          IF <ls_vepo_new>-updkz = lif_ef_constants=>cs_change_mode-insert.
            rv_changed  = lif_ef_constants=>cs_condition-true.
            RETURN.

            " HU item updated ?
          ELSEIF <ls_vepo_new>-updkz = lif_ef_constants=>cs_change_mode-update OR
                 <ls_vepo_new>-updkz = lif_ef_constants=>cs_change_mode-undefined.

            READ TABLE <lt_vepo_old> ASSIGNING FIELD-SYMBOL(<ls_vepo_old>)
              WITH KEY venum = <ls_vepo_new>-venum
                       vepos = <ls_vepo_new>-vepos.

            IF sy-subrc = 0 AND <ls_vepo_new>-vemng_flo <> <ls_vepo_old>-vemng_flo .
              rv_changed  = lif_ef_constants=>cs_condition-true.
              RETURN.
            ENDIF.
          ENDIF.
        ENDLOOP.

        " HU item deleted ?
        READ TABLE <lt_vepo_old> TRANSPORTING NO FIELDS
          WITH KEY vbeln = lv_vbeln
                   posnr = lv_posnr
                   updkz = lif_ef_constants=>cs_change_mode-delete.

        rv_changed  = COND #( WHEN sy-subrc = 0
                                THEN lif_ef_constants=>cs_condition-true
                                ELSE lif_ef_constants=>cs_condition-false ).
      ELSE.
        MESSAGE E002(ZPOF_GTT) WITH 'VEPO' INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~check_relevance.
    DATA: lv_difference    TYPE menge_d.

    rv_result   = lif_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = lif_pof_constants=>cs_tabledef-dl_item_new AND
       lcl_dl_tools=>is_appropriate_dl_type( ir_struct = is_events-mastertabref ) = abap_true AND
       lcl_dl_tools=>is_appropriate_dl_item( ir_struct = is_events-maintabref ) = abap_true.

      CASE is_events-update_indicator.
        WHEN lif_ef_constants=>cs_change_mode-insert OR
             lif_ef_constants=>cs_change_mode-update OR
             lif_ef_constants=>cs_change_mode-undefined.

          rv_result   = get_packing_quantity_changed( is_events = is_events ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~get_event_data.
    DATA(lv_quantity)   = get_packing_quantity(
                            ir_lips = is_events-maintabref
                            ir_vepo = mo_ae_parameters->get_appl_table(
                                        iv_tabledef = lif_pof_constants=>cs_tabledef-dl_hu_item_new ) ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = lcl_dl_tools=>get_tracking_id_dl_item(
                      ir_lips = is_events-maintabref )
      trxcod      = lif_pof_constants=>cs_trxcod-dl_position
      evtcnt      = is_events-eventid
      evtid       = lif_pof_constants=>cs_milestone-dl_packing
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
      loccod      = lif_ef_constants=>cs_loc_types-plant
      locid1      = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'WERKS' )
    ) ).

    " the WHOLE QUANTITY is sent (delta approach might have calculation mistakes
    " because of quantity conversion
    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = lif_pof_constants=>cs_event_param-quantity
      param_value = lcl_tools=>get_pretty_value( iv_value = lv_quantity )
    ) ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
*** Inbound Delivery Item Goods Receipt Event *****************************
**********************************************************************
CLASS lcl_ae_filler_dl_item_gr DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ae_filler.

    METHODS constructor
      IMPORTING
        io_ae_parameters TYPE REF TO lif_ae_parameters.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_dl_item_id,
             vbeln TYPE vbeln_vl,
             posnr TYPE posnr_vl,
           END OF ts_dl_item_id.

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

    METHODS get_delivery_item_id
      IMPORTING
        ir_matdoc         TYPE REF TO data
      RETURNING
        VALUE(rv_item_id) TYPE ts_dl_item_id
      RAISING
        cx_udm_message.

    METHODS get_tracking_id_dl_item
      IMPORTING
        ir_md_pos         TYPE REF TO data
      RETURNING
        VALUE(rv_trackid) TYPE /saptrx/trxid
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

    METHODS is_appropriate_dl_item
      IMPORTING
        ir_md_pos        TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    METHODS is_appropriate_dl_type
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

CLASS lcl_ae_filler_dl_item_gr IMPLEMENTATION.
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
        MESSAGE e001(zpof_gtt) WITH 'ERFMG' 'Goods Receipt' INTO lv_dummy ##NO_TEXT.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'Goods Receipt' INTO lv_dummy ##NO_TEXT.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_goods_receipt_quantity_dif.

    rv_difference = get_goods_receipt_quantity(
                      ir_goods_receipt = is_events-maintabref ).

  ENDMETHOD.

  METHOD get_delivery_item_id.
    rv_item_id-vbeln    = lcl_tools=>get_field_of_structure(
                            ir_struct_data = ir_matdoc
                            iv_field_name  = 'VBELN_IM' ).

    rv_item_id-posnr    = lcl_tools=>get_field_of_structure(
                            ir_struct_data = ir_matdoc
                            iv_field_name  = 'VBELP_IM' ).
  ENDMETHOD.

  METHOD get_tracking_id_dl_item.
    DATA(ls_dl_item)  = get_delivery_item_id(
                          ir_matdoc = ir_md_pos ).

    rv_trackid        = lcl_dl_tools=>get_tracking_id_dl_item(
                          ir_lips = REF #( ls_dl_item ) ).
  ENDMETHOD.

  METHOD is_appropriate_definition.
    rv_result = boolc( is_events-maintabdef = lif_pof_constants=>cs_tabledef-md_material_segment ).
  ENDMETHOD.

  METHOD is_appropriate_md_type.
    DATA: lv_md_type  TYPE mkpf-blart.

    lv_md_type  = lcl_tools=>get_field_of_structure(
                    ir_struct_data = ir_md_head
                    iv_field_name  = 'BLART' ).

    rv_result   = boolc( lv_md_type = lif_pof_constants=>cs_md_type-goods_receipt ).
  ENDMETHOD.

  METHOD is_appropriate_dl_item.
    DATA: ls_lips     TYPE lips.

    DATA(ls_dl_item)  = get_delivery_item_id(
                          ir_matdoc = ir_md_pos ).

    IF ls_dl_item-vbeln IS NOT INITIAL AND
       ls_dl_item-posnr IS NOT INITIAL.
      CALL FUNCTION 'CBGL_LB71_BUFFER_READ'
        EXPORTING
          i_rfc_destination  = 'NONE'
          i_vbeln            = ls_dl_item-vbeln
          i_posnr            = ls_dl_item-posnr
        IMPORTING
          e_position         = ls_lips
        EXCEPTIONS
          rfc_error          = 1
          delivery_not_found = 2
          wrong_posnr        = 3
          OTHERS             = 4.

      rv_result   = boolc( sy-subrc = 0 AND
                           lcl_dl_tools=>is_appropriate_dl_item(
                             ir_struct = REF #( ls_lips ) ) ).
    ELSE.
      rv_result   = abap_false.
    ENDIF.
  ENDMETHOD.

  METHOD is_appropriate_dl_type.
    DATA: ls_likp     TYPE likp.

    DATA(ls_dl_item)  = get_delivery_item_id(
                          ir_matdoc = ir_md_pos ).

    IF ls_dl_item-vbeln IS NOT INITIAL.
      CALL FUNCTION 'CBGL_LB71_BUFFER_READ'
        EXPORTING
          i_rfc_destination  = 'NONE'
          i_vbeln            = ls_dl_item-vbeln
          i_posnr            = ls_dl_item-posnr
        IMPORTING
          e_header           = ls_likp
        EXCEPTIONS
          rfc_error          = 1
          delivery_not_found = 2
          wrong_posnr        = 3
          OTHERS             = 4.

      rv_result   = boolc( sy-subrc = 0 AND
                           lcl_dl_tools=>is_appropriate_dl_type(
                             ir_struct = REF #( ls_likp ) ) = abap_true ).
    ELSE.
      rv_result   = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD is_creation_mode.
    DATA: lv_mblnr TYPE mkpf-mblnr,
          lv_mjahr TYPE mkpf-mjahr.

    IF is_events-update_indicator = lif_ef_constants=>cs_change_mode-insert.
      rv_result = abap_true.
    ELSEIF is_events-update_indicator = lif_ef_constants=>cs_change_mode-undefined.
      lv_mblnr  = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'MBLNR' ).

      lv_mjahr  = lcl_tools=>get_field_of_structure(
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

  METHOD lif_ae_filler~check_relevance.
    DATA: lv_mng_new    TYPE menge_d.

    rv_result   = lif_ef_constants=>cs_condition-false.

    " mkpf-blart        = 'WE'
    " update_indicator  = insert
    IF is_appropriate_definition( is_events = is_events ) = abap_true AND
       is_appropriate_md_type( ir_md_head = is_events-mastertabref ) = abap_true AND
       is_appropriate_dl_type( ir_md_pos  = is_events-maintabref )   = abap_true AND
       is_appropriate_dl_item( ir_md_pos  = is_events-maintabref )   = abap_true AND
       is_creation_mode( is_events = is_events ) = abap_true.

      lv_mng_new  = get_goods_receipt_quantity(
                      ir_goods_receipt = is_events-maintabref ).

      rv_result   = COND #( WHEN lv_mng_new <> 0
                              THEN lif_ef_constants=>cs_condition-true
                              ELSE lif_ef_constants=>cs_condition-false ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~get_event_data.
    DATA(lv_difference)   =  get_goods_receipt_quantity_dif(
                               is_events = is_events ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = get_tracking_id_dl_item(
                      ir_md_pos = is_events-maintabref )
      trxcod      = lif_pof_constants=>cs_trxcod-dl_position
      evtcnt      = is_events-eventid
      evtid       = lif_pof_constants=>cs_milestone-dl_goods_receipt
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
      loccod      = lif_ef_constants=>cs_loc_types-plant
      locid1      = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = 'WERKS' )
    ) ).

    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = lif_pof_constants=>cs_event_param-quantity
      param_value = lcl_tools=>get_pretty_value( iv_value = lv_difference )
    ) ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
*** Shipment Header - Header Based Events ****************************
**********************************************************************
CLASS lcl_ae_filler_sh_header_bh DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_ae_filler.

    METHODS constructor
      IMPORTING
        io_ae_parameters TYPE REF TO lif_ae_parameters.

  PROTECTED SECTION.
    METHODS get_eventid ABSTRACT
      RETURNING
        VALUE(rv_eventid) TYPE /saptrx/ev_evtid.

    METHODS get_date_field ABSTRACT
      RETURNING
        VALUE(rv_field) TYPE lif_ef_types=>tv_field_name.

    METHODS get_time_field ABSTRACT
      RETURNING
        VALUE(rv_field) TYPE lif_ef_types=>tv_field_name.

  PRIVATE SECTION.
    TYPES: BEGIN OF ts_dl_item_id,
             vbeln TYPE vbeln_vl,
             posnr TYPE posnr_vl,
           END OF ts_dl_item_id.

    DATA: mo_ae_parameters    TYPE REF TO lif_ae_parameters.

ENDCLASS.

CLASS lcl_ae_filler_sh_header_bh IMPLEMENTATION.
  METHOD constructor.
    mo_ae_parameters  = io_ae_parameters.
  ENDMETHOD.

  METHOD lif_ae_filler~check_relevance.
    DATA: lr_vttk_old TYPE REF TO data,
          lv_date     TYPE d.

    DATA(lr_vttp) = mo_ae_parameters->get_appl_table(
                      iv_tabledef = lif_pof_constants=>cs_tabledef-sh_item_new ).

    lv_date       = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_events-maintabref
                      iv_field_name  = get_date_field( ) ).

    rv_result   = lif_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = lif_pof_constants=>cs_tabledef-sh_header_new AND
       lcl_sh_tools=>is_appropriate_type( ir_vttk = is_events-maintabref ) = abap_true AND
       lcl_sh_tools=>is_delivery_assigned( ir_vttp = lr_vttp ) = abap_true AND
       lv_date IS NOT INITIAL.

      lr_vttk_old = COND #( WHEN is_events-update_indicator = lif_ef_constants=>cs_change_mode-insert
                              THEN NEW vttkvb( )
                              ELSE is_events-mainoldtabref ).

      rv_result   = lcl_tools=>are_fields_different(
                      ir_data1  = is_events-maintabref
                      ir_data2  = lr_vttk_old
                      it_fields = VALUE #( ( get_date_field( ) )
                                           ( get_time_field( ) ) ) ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~get_event_data.
    DATA(lv_evtcnt) = lcl_sh_tools=>get_next_event_counter( ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = lcl_tools=>get_field_of_structure(
                        ir_struct_data = is_events-maintabref
                        iv_field_name  = 'TKNUM' )
      trxcod      = lif_pof_constants=>cs_trxcod-sh_number
      evtcnt      = lv_evtcnt
      evtid       = get_eventid( )
      evtdat      = lcl_tools=>get_field_of_structure(
                        ir_struct_data = is_events-maintabref
                        iv_field_name  = get_date_field( ) )
      evttim      = lcl_tools=>get_field_of_structure(
                        ir_struct_data = is_events-maintabref
                        iv_field_name  = get_time_field( ) )
      evtzon      = lcl_tools=>get_system_time_zone( )
    ) ).

    ct_eventid_map  = VALUE #( BASE ct_eventid_map (
      eventid     = is_events-eventid
      evtcnt      = lv_evtcnt
    ) ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Shipment Header Check In Event ***********************************
**********************************************************************
CLASS lcl_ae_filler_sh_header_ci DEFINITION
  INHERITING FROM lcl_ae_filler_sh_header_bh.

  PROTECTED SECTION.
    METHODS get_eventid REDEFINITION.

    METHODS get_date_field REDEFINITION.

    METHODS get_time_field REDEFINITION.
ENDCLASS.

CLASS lcl_ae_filler_sh_header_ci IMPLEMENTATION.
  METHOD get_eventid.
    rv_eventid  = lif_pof_constants=>cs_milestone-sh_check_in.
  ENDMETHOD.

  METHOD get_date_field.
    rv_field  = 'DAREG'.
  ENDMETHOD.

  METHOD get_time_field.
    rv_field  = 'UAREG'.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Shipment Header Load Start Event *********************************
**********************************************************************
CLASS lcl_ae_filler_sh_header_ls DEFINITION
  INHERITING FROM lcl_ae_filler_sh_header_bh.

  PROTECTED SECTION.
    METHODS get_eventid REDEFINITION.

    METHODS get_date_field REDEFINITION.

    METHODS get_time_field REDEFINITION.
ENDCLASS.

CLASS lcl_ae_filler_sh_header_ls IMPLEMENTATION.
  METHOD get_eventid.
    rv_eventid  = lif_pof_constants=>cs_milestone-sh_load_start.
  ENDMETHOD.

  METHOD get_date_field.
    rv_field  = 'DALBG'.
  ENDMETHOD.

  METHOD get_time_field.
    rv_field  = 'UALBG'.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Shipment Header Load End Event ***********************************
**********************************************************************
CLASS lcl_ae_filler_sh_header_le DEFINITION
  INHERITING FROM lcl_ae_filler_sh_header_bh.

  PROTECTED SECTION.
    METHODS get_eventid REDEFINITION.

    METHODS get_date_field REDEFINITION.

    METHODS get_time_field REDEFINITION.
ENDCLASS.

CLASS lcl_ae_filler_sh_header_le IMPLEMENTATION.
  METHOD get_eventid.
    rv_eventid  = lif_pof_constants=>cs_milestone-sh_load_end.
  ENDMETHOD.

  METHOD get_date_field.
    rv_field  = 'DALEN'.
  ENDMETHOD.

  METHOD get_time_field.
    rv_field  = 'UALEN'.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Shipment Header - Stops Based Events ****************************
**********************************************************************
CLASS lcl_ae_filler_sh_header_bs DEFINITION ABSTRACT.
  PUBLIC SECTION.
    INTERFACES lif_ae_filler.

    METHODS constructor
      IMPORTING
        io_ae_parameters TYPE REF TO lif_ae_parameters
      RAISING
        cx_udm_message.

  PROTECTED SECTION.
    METHODS get_date_field ABSTRACT
      RETURNING
        VALUE(rv_field) TYPE lif_ef_types=>tv_field_name.

    METHODS get_eventid ABSTRACT
      RETURNING
        VALUE(rv_eventid) TYPE /saptrx/ev_evtid.

    METHODS get_location_category ABSTRACT
      RETURNING
        VALUE(rv_loccat) TYPE lif_pof_types=>tv_loccat.

    METHODS get_time_field ABSTRACT
      RETURNING
        VALUE(rv_field) TYPE lif_ef_types=>tv_field_name.

  PRIVATE SECTION.
    TYPES: tt_vttsvb  TYPE vttsvb_tab.

    TYPES: BEGIN OF ts_dl_item_id,
             vbeln TYPE vbeln_vl,
             posnr TYPE posnr_vl,
           END OF ts_dl_item_id.

    DATA: mo_ae_parameters TYPE REF TO lif_ae_parameters,
          mt_vtts_new      TYPE tt_vttsvb,
          mt_vtts_old      TYPE tt_vttsvb.

    METHODS get_copy_of_vtts_table
      IMPORTING
        ir_vtts TYPE REF TO data
      EXPORTING
        et_vtts TYPE tt_vttsvb.

    METHODS get_stops_from_shipment
      IMPORTING
        is_events TYPE trxas_evt_ctab_wa
      EXPORTING
        et_stops  TYPE lif_pof_types=>tt_stops
      RAISING
        cx_udm_message.

    METHODS is_stop_changed
      IMPORTING
        iv_tknum         TYPE tknum
        iv_tsnum         TYPE tsnum
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_ae_filler_sh_header_bs IMPLEMENTATION.
  METHOD constructor.
    mo_ae_parameters  = io_ae_parameters.

    get_copy_of_vtts_table(
      EXPORTING
        ir_vtts = mo_ae_parameters->get_appl_table(
                    iv_tabledef = lif_pof_constants=>cs_tabledef-sh_stage_new )
      IMPORTING
        et_vtts = mt_vtts_new ).

    get_copy_of_vtts_table(
      EXPORTING
        ir_vtts = mo_ae_parameters->get_appl_table(
                    iv_tabledef = lif_pof_constants=>cs_tabledef-sh_stage_old )
      IMPORTING
        et_vtts = mt_vtts_old ).

  ENDMETHOD.

  METHOD get_copy_of_vtts_table.
    FIELD-SYMBOLS: <lt_vtts>  TYPE tt_vttsvb.

    ASSIGN ir_vtts->* TO <lt_vtts>.

    IF <lt_vtts> IS ASSIGNED.
      et_vtts[] = <lt_vtts>[].

      SORT et_vtts BY tknum tsnum.

    ELSE.
      CLEAR et_vtts[].
    ENDIF.
  ENDMETHOD.

  METHOD get_stops_from_shipment.
    DATA: lt_vtts  TYPE vttsvb_tab.

    DATA(lv_tknum) = CONV tknum( lcl_tools=>get_field_of_structure(
                                   ir_struct_data = is_events-maintabref
                                   iv_field_name  = 'TKNUM' ) ).
    DATA(lr_vttp)  = mo_ae_parameters->get_appl_table(
                      iv_tabledef = lif_pof_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vtts)  = mo_ae_parameters->get_appl_table(
                      iv_tabledef = lif_pof_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtsp)  = mo_ae_parameters->get_appl_table(
                      iv_tabledef = lif_pof_constants=>cs_tabledef-sh_item_stage_new ).

    FIELD-SYMBOLS: <lt_vttp> TYPE vttpvb_tab,
                   <lt_vtts> TYPE vttsvb_tab,
                   <lt_vtsp> TYPE vtspvb_tab.

    ASSIGN lr_vtts->* TO <lt_vtts>.
    ASSIGN lr_vttp->* TO <lt_vttp>.
    ASSIGN lr_vtsp->* TO <lt_vtsp>.

    IF <lt_vtts> IS ASSIGNED AND
       <lt_vtsp> IS ASSIGNED AND
       <lt_vttp> IS ASSIGNED.

      lt_vtts[]  = <lt_vtts>[].
      SORT lt_vtts BY tsrfo.

      lcl_sh_tools=>get_stops_from_shipment(
        EXPORTING
          iv_tknum  = lv_tknum
          it_vtts   = lt_vtts
          it_vtsp   = <lt_vtsp>
          it_vttp   = <lt_vttp>
        IMPORTING
          et_stops  = et_stops ).
    ELSE.
      MESSAGE E002(ZPOF_GTT) WITH 'VTTS' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD is_stop_changed.
    FIELD-SYMBOLS: <lv_edate> TYPE d.

    rv_result = lif_ef_constants=>cs_condition-false.

    READ TABLE mt_vtts_new ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
      WITH KEY tknum = iv_tknum
               tsnum = iv_tsnum
      BINARY SEARCH.

    IF sy-subrc = 0.
      CASE <ls_vtts_new>-updkz.
        WHEN lif_ef_constants=>cs_change_mode-insert.

          ASSIGN COMPONENT get_date_field(  ) OF STRUCTURE <ls_vtts_new>
            TO <lv_edate>.

          IF <lv_edate> IS ASSIGNED.
            rv_result   = COND #( WHEN <lv_edate> IS NOT INITIAL
                                    THEN lif_ef_constants=>cs_condition-true
                                    ELSE lif_ef_constants=>cs_condition-false ).
          ELSE.
            MESSAGE E001(ZPOF_GTT) WITH get_date_field(  ) 'VTTS'
              INTO DATA(lv_dummy).
            lcl_tools=>throw_exception( ).
          ENDIF.

        WHEN lif_ef_constants=>cs_change_mode-update OR
             lif_ef_constants=>cs_change_mode-undefined.

          ASSIGN COMPONENT get_date_field(  ) OF STRUCTURE <ls_vtts_new>
            TO <lv_edate>.

          IF <lv_edate> IS ASSIGNED AND
             <lv_edate> is NOT INITIAL.

            READ TABLE mt_vtts_old ASSIGNING FIELD-SYMBOL(<ls_vtts_old>)
              WITH KEY tknum = <ls_vtts_new>-tknum
                       tsnum = <ls_vtts_new>-tsnum
              BINARY SEARCH.

            IF sy-subrc = 0.
              rv_result   = lcl_tools=>are_fields_different(
                              ir_data1  = REF #( <ls_vtts_new> )
                              ir_data2  = REF #( <ls_vtts_old> )
                              it_fields = VALUE #( ( get_date_field( ) )
                                                   ( get_time_field( ) ) ) ).
            ENDIF.
          ENDIF.
      ENDCASE.
    ELSE.
      MESSAGE E005(ZPOF_GTT) WITH |{ iv_tknum }{ iv_tsnum }| 'VTTS' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~check_relevance.
    DATA: lv_date     TYPE d.

    FIELD-SYMBOLS: <lt_vtts_new> TYPE lif_pof_types=>tt_vttsvb,
                   <lt_vtts_old> TYPE lif_pof_types=>tt_vttsvb.

    DATA(lt_fields)   = VALUE lif_ef_types=>tt_field_name( ( get_date_field( ) )
                                                           ( get_time_field( ) ) ).
    DATA(lr_vttp)     = mo_ae_parameters->get_appl_table(
                          iv_tabledef = lif_pof_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vtts_new) = mo_ae_parameters->get_appl_table(
                          iv_tabledef = lif_pof_constants=>cs_tabledef-sh_stage_new ).

    rv_result   = lif_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = lif_pof_constants=>cs_tabledef-sh_header_new AND
       lcl_sh_tools=>is_appropriate_type( ir_vttk = is_events-maintabref ) = abap_true AND
       lcl_sh_tools=>is_delivery_assigned( ir_vttp = lr_vttp ) = abap_true.

      ASSIGN lr_vtts_new->* TO <lt_vtts_new>.

      IF <lt_vtts_new> IS ASSIGNED.
        LOOP AT <lt_vtts_new> ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
          WHERE updkz IS NOT INITIAL.

          rv_result   = is_stop_changed(
                          iv_tknum = <ls_vtts_new>-tknum
                          iv_tsnum = <ls_vtts_new>-tsnum ).

          IF rv_result = lif_ef_constants=>cs_condition-true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE E002(ZPOF_GTT) WITH 'VTTS' INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD lif_ae_filler~get_event_data.
    DATA: lt_stops  TYPE lif_pof_types=>tt_stops.

    get_stops_from_shipment(
      EXPORTING
        is_events = is_events
      IMPORTING
        et_stops  = lt_stops ).

    LOOP AT lt_stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
      WHERE loccat = get_location_category( ).

      IF is_stop_changed( iv_tknum = <ls_stops>-tknum
                          iv_tsnum = <ls_stops>-tsnum ) = lif_ef_constants=>cs_condition-true.

        DATA(lv_evtcnt) = lcl_sh_tools=>get_next_event_counter( ).

        READ TABLE mt_vtts_new ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
          WITH KEY tknum = <ls_stops>-tknum
                   tsnum = <ls_stops>-tsnum
                   BINARY SEARCH.
        IF sy-subrc = 0.
          ct_trackingheader = VALUE #( BASE ct_trackingheader (
            language    = sy-langu
            trxcod      = lif_pof_constants=>cs_trxcod-sh_number
            trxid       = <ls_stops>-tknum
            evtcnt      = lv_evtcnt
            evtid       = get_eventid( )
            evtdat      = lcl_tools=>get_field_of_structure(
                              ir_struct_data = REF #( <ls_vtts_new> )
                              iv_field_name  = get_date_field( ) )
            evttim      = lcl_tools=>get_field_of_structure(
                              ir_struct_data = REF #( <ls_vtts_new> )
                              iv_field_name  = get_time_field( ) )
            evtzon      = lcl_tools=>get_system_time_zone( )
          ) ).

          ct_eventid_map  = VALUE #( BASE ct_eventid_map (
            eventid     = is_events-eventid
            evtcnt      = lv_evtcnt
          ) ).

          ct_tracklocation  = VALUE #( BASE ct_tracklocation (
            evtcnt      = lv_evtcnt
            loccod      = <ls_stops>-loctype
            locid1      = <ls_stops>-locid
            locid2      = <ls_stops>-stopid
          ) ).
        ELSE.
          MESSAGE E005(ZPOF_GTT)
            WITH |{ <ls_stops>-tknum }{ <ls_stops>-tsnum }| 'VTTK'
            INTO DATA(lv_dummy).
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Shipment Header Departure Event **********************************
**********************************************************************
CLASS lcl_ae_filler_sh_header_dep DEFINITION
  INHERITING FROM lcl_ae_filler_sh_header_bs.

  PROTECTED SECTION.
    METHODS get_date_field REDEFINITION.

    METHODS get_eventid REDEFINITION.

    METHODS get_location_category REDEFINITION.

    METHODS get_time_field REDEFINITION.
ENDCLASS.

CLASS lcl_ae_filler_sh_header_dep IMPLEMENTATION.
  METHOD get_date_field.
    rv_field  = 'DATBG'.
  ENDMETHOD.

  METHOD get_eventid.
    rv_eventid  = lif_pof_constants=>cs_milestone-sh_departure.
  ENDMETHOD.

  METHOD get_location_category.
    rv_loccat   = lif_pof_constants=>cs_loccat-departure.
  ENDMETHOD.

  METHOD get_time_field.
    rv_field  = 'UATBG'.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Shipment Header Arrival Event ************************************
**********************************************************************
CLASS lcl_ae_filler_sh_header_arr DEFINITION
  INHERITING FROM lcl_ae_filler_sh_header_bs.

  PROTECTED SECTION.
    METHODS get_date_field REDEFINITION.

    METHODS get_eventid REDEFINITION.

    METHODS get_location_category REDEFINITION.

    METHODS get_time_field REDEFINITION.
ENDCLASS.

CLASS lcl_ae_filler_sh_header_arr IMPLEMENTATION.
  METHOD get_date_field.
    rv_field  = 'DATEN'.
  ENDMETHOD.

  METHOD get_eventid.
    rv_eventid  = lif_pof_constants=>cs_milestone-sh_arrival.
  ENDMETHOD.

  METHOD get_location_category.
    rv_loccat   = lif_pof_constants=>cs_loccat-arrival.
  ENDMETHOD.

  METHOD get_time_field.
    rv_field  = 'UATEN'.
  ENDMETHOD.
ENDCLASS.
