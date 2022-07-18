CLASS zcl_gtt_pof_ae_filler_poi_cnf DEFINITION
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

  METHODS get_confirmation_quantity
    IMPORTING
      !ir_ekpo TYPE REF TO data
      !ir_ekes TYPE REF TO data
    RETURNING
      VALUE(rv_menge) TYPE menge_d
    RAISING
      cx_udm_message .
  METHODS get_confirmation_quantity_diff
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_difference) TYPE menge_d
    RAISING
      cx_udm_message .
  METHODS is_appropriate_conf_type
    IMPORTING
      !iv_ebtyp TYPE clike
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
  METHODS is_appropriate_conf_control
    IMPORTING
      !iv_bstae TYPE clike
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
  METHODS has_changes
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_pof_ef_types=>tv_condition
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_ae_filler_poi_cnf IMPLEMENTATION.


  METHOD constructor.

    mo_ae_parameters  = io_ae_parameters.

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
      MESSAGE e002(zgtt_pof) WITH 'EKPO' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).

    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'EKET' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_confirmation_quantity_diff.

    DATA: lv_mng_old TYPE menge_d VALUE 0,
          lv_mng_new TYPE menge_d.

    IF is_events-update_indicator <> zif_gtt_pof_ef_constants=>cs_change_mode-insert.
      lv_mng_old  = get_confirmation_quantity(
                      ir_ekpo = is_events-mainoldtabref
                      ir_ekes = mo_ae_parameters->get_appl_table(
                                  iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-po_vend_conf_old ) ).
    ENDIF.

    lv_mng_new  = get_confirmation_quantity(
                    ir_ekpo = is_events-maintabref
                    ir_ekes = mo_ae_parameters->get_appl_table(
                                iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-po_vend_conf_new ) ).

    rv_difference = lv_mng_new - lv_mng_old.


  ENDMETHOD.


  METHOD has_changes.

    DATA(lv_difference)   = get_confirmation_quantity_diff(
                              is_events = is_events ).

    rv_result   = COND #( WHEN lv_difference <> 0
                            THEN zif_gtt_pof_ef_constants=>cs_condition-true
                            ELSE zif_gtt_pof_ef_constants=>cs_condition-false ).

  ENDMETHOD.


  METHOD is_appropriate_conf_control.

    rv_result = boolc( iv_bstae = zif_gtt_pof_app_constants=>cs_bstae-confirm OR
                       iv_bstae = zif_gtt_pof_app_constants=>cs_bstae-delivery ).

  ENDMETHOD.


  METHOD is_appropriate_conf_type.

    rv_result = boolc( iv_ebtyp = zif_gtt_pof_app_constants=>cs_relevance-ebtyp ).

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~check_relevance.

    DATA: lv_difference    TYPE menge_d.

    rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = zif_gtt_pof_app_constants=>cs_tabledef-po_item_new AND
       zcl_gtt_pof_po_tools=>is_appropriate_po_type( ir_ekko = is_events-mastertabref ) = abap_true AND
       zcl_gtt_pof_po_tools=>is_appropriate_po_item( ir_ekpo = is_events-maintabref ) = abap_true.

      CASE is_events-update_indicator.
        WHEN zif_gtt_pof_ef_constants=>cs_change_mode-insert.
          lv_difference = get_confirmation_quantity_diff(
                                 is_events = is_events ).

          rv_result   = COND #( WHEN lv_difference <> 0
                                  THEN zif_gtt_pof_ef_constants=>cs_condition-true
                                  ELSE zif_gtt_pof_ef_constants=>cs_condition-false ).

        WHEN zif_gtt_pof_ef_constants=>cs_change_mode-update OR
             zif_gtt_pof_ef_constants=>cs_change_mode-undefined.

          rv_result   = has_changes( is_events = is_events ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~get_event_data.

    DATA(lv_difference)   =  get_confirmation_quantity_diff(
                                 is_events = is_events ).

    ct_trackingheader = VALUE #( BASE ct_trackingheader (
      language    = sy-langu
      trxid       = zcl_gtt_pof_po_tools=>get_tracking_id_po_item(
                      ir_ekpo = is_events-maintabref )
      trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-po_position
      evtcnt      = is_events-eventid
      evtid       = zif_gtt_pof_app_constants=>cs_milestone-po_confirmation
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

    " QUANTITY
    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = zif_gtt_pof_app_constants=>cs_event_param-quantity
      param_value = zcl_gtt_pof_tools=>get_pretty_value( iv_value = lv_difference )
    ) ).

    " CONFIRMATION TYPE
    ct_trackparameters  = VALUE #( BASE ct_trackparameters (
      evtcnt      = is_events-eventid
      param_name  = zif_gtt_pof_app_constants=>cs_event_param-confirm_type
      param_value = zif_gtt_pof_app_constants=>cs_relevance-ebtyp
    ) ).

  ENDMETHOD.
ENDCLASS.
