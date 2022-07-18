CLASS zcl_gtt_pof_ae_filler_shh_bs DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_ae_filler .

  METHODS constructor
    IMPORTING
      !io_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters
    RAISING
      cx_udm_message .
PROTECTED SECTION.

  METHODS get_date_field
  ABSTRACT
    RETURNING
      VALUE(rv_field) TYPE zif_gtt_pof_ef_types=>tv_field_name .
  METHODS get_eventid
  ABSTRACT
    RETURNING
      VALUE(rv_eventid) TYPE /saptrx/ev_evtid .
  METHODS get_location_category
  ABSTRACT
    RETURNING
      VALUE(rv_loccat) TYPE zif_gtt_pof_app_types=>tv_loccat .
  METHODS get_time_field
  ABSTRACT
    RETURNING
      VALUE(rv_field) TYPE zif_gtt_pof_ef_types=>tv_field_name .
  METHODS is_location_required
  ABSTRACT
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
PRIVATE SECTION.

  TYPES tt_vttsvb TYPE vttsvb_tab .
  TYPES:
    BEGIN OF ts_dl_item_id,
             vbeln TYPE vbeln_vl,
             posnr TYPE posnr_vl,
           END OF ts_dl_item_id .

  DATA mo_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters .
  DATA mt_vtts_new TYPE tt_vttsvb .
  DATA mt_vtts_old TYPE tt_vttsvb .

  METHODS get_copy_of_vtts_table
    IMPORTING
      !ir_vtts TYPE REF TO data
    EXPORTING
      !et_vtts TYPE tt_vttsvb .
  METHODS get_stops_from_shipment
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    EXPORTING
      !et_stops TYPE zif_gtt_pof_app_types=>tt_stops
    RAISING
      cx_udm_message .
  METHODS is_stop_changed
    IMPORTING
      !iv_tknum TYPE tknum
      !iv_tsnum TYPE tsnum
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_ae_filler_shh_bs IMPLEMENTATION.


  METHOD constructor.

    mo_ae_parameters  = io_ae_parameters.

    get_copy_of_vtts_table(
      EXPORTING
        ir_vtts = mo_ae_parameters->get_appl_table(
                    iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_new )
      IMPORTING
        et_vtts = mt_vtts_new ).

    get_copy_of_vtts_table(
      EXPORTING
        ir_vtts = mo_ae_parameters->get_appl_table(
                    iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_old )
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

    DATA(lv_tknum) = CONV tknum( zcl_gtt_pof_tools=>get_field_of_structure(
                                   ir_struct_data = is_events-maintabref
                                   iv_field_name  = 'TKNUM' ) ).
    DATA(lr_vttp)  = mo_ae_parameters->get_appl_table(
                      iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vtts)  = mo_ae_parameters->get_appl_table(
                      iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtsp)  = mo_ae_parameters->get_appl_table(
                      iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_stage_new ).

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

      zcl_gtt_pof_sh_tools=>get_stops_from_shipment(
        EXPORTING
          iv_tknum  = lv_tknum
          it_vtts   = lt_vtts
          it_vtsp   = <lt_vtsp>
          it_vttp   = <lt_vttp>
        IMPORTING
          et_stops  = et_stops ).
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTS' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_stop_changed.

    FIELD-SYMBOLS: <lv_edate> TYPE d.

    rv_result = zif_gtt_pof_ef_constants=>cs_condition-false.

    READ TABLE mt_vtts_new ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
      WITH KEY tknum = iv_tknum
               tsnum = iv_tsnum
      BINARY SEARCH.

    IF sy-subrc = 0.
      CASE <ls_vtts_new>-updkz.
        WHEN zif_gtt_pof_ef_constants=>cs_change_mode-insert.

          ASSIGN COMPONENT get_date_field(  ) OF STRUCTURE <ls_vtts_new>
            TO <lv_edate>.

          IF <lv_edate> IS ASSIGNED.
            rv_result   = COND #( WHEN <lv_edate> IS NOT INITIAL
                                    THEN zif_gtt_pof_ef_constants=>cs_condition-true
                                    ELSE zif_gtt_pof_ef_constants=>cs_condition-false ).
          ELSE.
            MESSAGE e001(zgtt_pof) WITH get_date_field(  ) 'VTTS'
              INTO DATA(lv_dummy).
            zcl_gtt_pof_tools=>throw_exception( ).
          ENDIF.

        WHEN zif_gtt_pof_ef_constants=>cs_change_mode-update OR
             zif_gtt_pof_ef_constants=>cs_change_mode-undefined.

          ASSIGN COMPONENT get_date_field(  ) OF STRUCTURE <ls_vtts_new>
            TO <lv_edate>.

          IF <lv_edate> IS ASSIGNED AND
             <lv_edate> IS NOT INITIAL.

            READ TABLE mt_vtts_old ASSIGNING FIELD-SYMBOL(<ls_vtts_old>)
              WITH KEY tknum = <ls_vtts_new>-tknum
                       tsnum = <ls_vtts_new>-tsnum
              BINARY SEARCH.

            IF sy-subrc = 0.
              rv_result   = zcl_gtt_pof_tools=>are_fields_different(
                              ir_data1  = REF #( <ls_vtts_new> )
                              ir_data2  = REF #( <ls_vtts_old> )
                              it_fields = VALUE #( ( get_date_field( ) )
                                                   ( get_time_field( ) ) ) ).
            ENDIF.
          ENDIF.
      ENDCASE.
    ELSE.
      MESSAGE e005(zgtt_pof) WITH |{ iv_tknum }{ iv_tsnum }| 'VTTS' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~check_relevance.

    DATA: lv_date     TYPE d.

    FIELD-SYMBOLS: <lt_vtts_new> TYPE zif_gtt_pof_app_types=>tt_vttsvb,
                   <lt_vtts_old> TYPE zif_gtt_pof_app_types=>tt_vttsvb.

    DATA(lt_fields)   = VALUE zif_gtt_pof_ef_types=>tt_field_name( ( get_date_field( ) )
                                                           ( get_time_field( ) ) ).
    DATA(lr_vttp)     = mo_ae_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vtts_new) = mo_ae_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_new ).

    rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.

    IF is_events-maintabdef = zif_gtt_pof_app_constants=>cs_tabledef-sh_header_new AND
       zcl_gtt_pof_sh_tools=>is_appropriate_type( ir_vttk = is_events-maintabref ) = abap_true AND
       zcl_gtt_pof_sh_tools=>is_delivery_assigned( ir_vttp = lr_vttp ) = abap_true.

      ASSIGN lr_vtts_new->* TO <lt_vtts_new>.

      IF <lt_vtts_new> IS ASSIGNED.
        LOOP AT <lt_vtts_new> ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
          WHERE updkz IS NOT INITIAL.

          rv_result   = is_stop_changed(
                          iv_tknum = <ls_vtts_new>-tknum
                          iv_tsnum = <ls_vtts_new>-tsnum ).

          IF rv_result = zif_gtt_pof_ef_constants=>cs_condition-true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE e002(zgtt_pof) WITH 'VTTS' INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_filler~get_event_data.

    DATA: lt_stops  TYPE zif_gtt_pof_app_types=>tt_stops.

    get_stops_from_shipment(
      EXPORTING
        is_events = is_events
      IMPORTING
        et_stops  = lt_stops ).

    LOOP AT lt_stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
      WHERE loccat = get_location_category( ).

      IF is_stop_changed( iv_tknum = <ls_stops>-tknum
                          iv_tsnum = <ls_stops>-tsnum ) = zif_gtt_pof_ef_constants=>cs_condition-true.

        DATA(lv_evtcnt) = zcl_gtt_pof_sh_tools=>get_next_event_counter( ).

        READ TABLE mt_vtts_new ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
          WITH KEY tknum = <ls_stops>-tknum
                   tsnum = <ls_stops>-tsnum
                   BINARY SEARCH.
        IF sy-subrc = 0.
          ct_trackingheader = VALUE #( BASE ct_trackingheader (
            language    = sy-langu
            trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-sh_number
            trxid       = <ls_stops>-tknum
            evtcnt      = lv_evtcnt
            evtid       = get_eventid( )
            evtdat      = zcl_gtt_pof_tools=>get_field_of_structure(
                              ir_struct_data = REF #( <ls_vtts_new> )
                              iv_field_name  = get_date_field( ) )
            evttim      = zcl_gtt_pof_tools=>get_field_of_structure(
                              ir_struct_data = REF #( <ls_vtts_new> )
                              iv_field_name  = get_time_field( ) )
            evtzon      = zcl_gtt_pof_tools=>get_system_time_zone( )
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

          IF is_location_required( ) = abap_true.
            ct_trackparameters = VALUE #( BASE ct_trackparameters
              (
                evtcnt      = lv_evtcnt
                param_name  = zif_gtt_pof_app_constants=>cs_event_param-location_id
                param_value = <ls_stops>-locid
              )
              (
                evtcnt      = lv_evtcnt
                param_name  = zif_gtt_pof_app_constants=>cs_event_param-location_type
                param_value = <ls_stops>-loctype
              )
            ).
          ENDIF.

        ELSE.
          MESSAGE e005(zgtt_pof)
            WITH |{ <ls_stops>-tknum }{ <ls_stops>-tsnum }| 'VTTK'
            INTO DATA(lv_dummy).
          zcl_gtt_pof_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
