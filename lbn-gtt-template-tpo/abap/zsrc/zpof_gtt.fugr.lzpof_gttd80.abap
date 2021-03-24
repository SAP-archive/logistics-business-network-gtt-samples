*&---------------------------------------------------------------------*
*& Include          LZPOF_GTTD80
*&---------------------------------------------------------------------*

CLASS lcl_ctp_sender DEFINITION
  ABSTRACT.

  PUBLIC SECTION.
    METHODS send_idoc_data
      EXPORTING
        et_bapiret TYPE bapiret2_t
      RAISING
        cx_udm_message.

  PROTECTED SECTION.
    TYPES: BEGIN OF ts_aotype,
             obj_type    TYPE /saptrx/trk_obj_type,
             aot_type    TYPE /saptrx/aotype,
             server_name TYPE /saptrx/trxservername,
           END OF ts_aotype,
           tt_aotype TYPE STANDARD TABLE OF ts_aotype WITH EMPTY KEY.

    TYPES: tt_trxas_appobj_ctab TYPE STANDARD TABLE OF trxas_appobj_ctab_wa
                                  WITH EMPTY KEY.

    TYPES: tt_aotype_rst  TYPE RANGE OF /saptrx/aotype.

    TYPES: tt_trk_obj_type TYPE STANDARD TABLE OF /saptrx/trk_obj_type
                             WITH EMPTY KEY.

    TYPES: BEGIN OF ts_idoc_data,
             control      TYPE /saptrx/bapi_trk_control_tab,
             info         TYPE /saptrx/bapi_trk_info_tab,
             tracking_id  TYPE /saptrx/bapi_trk_trkid_tab,
             exp_event    TYPE /saptrx/bapi_trk_ee_tab,
             trxserv      TYPE /saptrx/trxserv,
             appsys       TYPE logsys,
             appobj_ctabs TYPE tt_trxas_appobj_ctab,
           END OF ts_idoc_data,
           tt_idoc_data TYPE STANDARD TABLE OF ts_idoc_data
                          WITH EMPTY KEY.

    DATA: mv_appsys    TYPE logsys,
          mt_aotype    TYPE tt_aotype,
          mt_idoc_data TYPE tt_idoc_data.

    METHODS get_aotype_restrictions ABSTRACT
      EXPORTING
        et_aotype TYPE tt_aotype_rst.

    METHODS get_object_type ABSTRACT
      RETURNING
        VALUE(rv_objtype) TYPE /saptrx/trk_obj_type.

    METHODS fill_idoc_trxserv
      IMPORTING
        is_aotype    TYPE ts_aotype
      CHANGING
        cs_idoc_data TYPE ts_idoc_data.

    METHODS initiate
      RAISING
        cx_udm_message.

    METHODS initiate_aotypes
      RAISING
        cx_udm_message.

    CLASS-METHODS is_extractor_exist
      IMPORTING
        iv_trk_obj_type  TYPE clike
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_gtt_enabled
      IMPORTING
        it_trk_obj_type  TYPE tt_trk_obj_type
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
ENDCLASS.

CLASS lcl_ctp_sender IMPLEMENTATION.
  METHOD fill_idoc_trxserv.
    SELECT SINGLE /saptrx/trxserv~trx_server_id
                  /saptrx/trxserv~trx_server
      INTO ( cs_idoc_data-trxserv-trx_server_id,
             cs_idoc_data-trxserv-trx_server )
      FROM /saptrx/trxserv
      WHERE trx_server_id = is_aotype-server_name.
  ENDMETHOD.

  METHOD initiate.
    " Get current logical system
    CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
      IMPORTING
        own_logical_system             = mv_appsys
      EXCEPTIONS
        own_logical_system_not_defined = 1
        OTHERS                         = 2.

    IF sy-subrc <> 0.
      MESSAGE e007(zpof_gtt) INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.

    initiate_aotypes( ).

  ENDMETHOD.

  METHOD initiate_aotypes.
    DATA: lt_aotype_rst TYPE tt_aotype_rst.

    DATA(lv_objtype)  = get_object_type(  ).

    get_aotype_restrictions(
      IMPORTING
        et_aotype = lt_aotype_rst ).

    " Prepare AOT list
    SELECT trk_obj_type  AS obj_type
           aotype        AS aot_type
           trxservername AS server_name
      INTO TABLE mt_aotype
      FROM /saptrx/aotypes
      WHERE trk_obj_type  = lv_objtype
        AND aotype       IN lt_aotype_rst
        AND torelevant    = abap_true.

    IF sy-subrc <> 0.
      MESSAGE e008(zpof_gtt) INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD is_extractor_exist.
    DATA: lv_trk_obj_type  TYPE /saptrx/aotypes-trk_obj_type.

    SELECT SINGLE trk_obj_type
      INTO lv_trk_obj_type
      FROM /saptrx/aotypes
      WHERE trk_obj_type = iv_trk_obj_type
        AND torelevant   = abap_true.

    rv_result   = boolc( sy-subrc = 0 ).
  ENDMETHOD.

  METHOD is_gtt_enabled.
    DATA: lv_extflag      TYPE flag.

    rv_result   = abap_false.

    " Check package dependent BADI disabling
    CALL FUNCTION 'GET_R3_EXTENSION_SWITCH'
      EXPORTING
        i_structure_package = lif_ef_constants=>cv_structure_pkg
      IMPORTING
        e_active            = lv_extflag
      EXCEPTIONS
        not_existing        = 1
        object_not_existing = 2
        no_extension_object = 3
        OTHERS              = 4.

    IF sy-subrc = 0 AND lv_extflag = abap_true.
*     Check if any tracking server defined
      CALL FUNCTION '/SAPTRX/EVENT_MGR_CHECK'
        EXCEPTIONS
          no_event_mgr_available = 1
          OTHERS                 = 2.

      "Check whether at least 1 active extractor exists for every object
      IF sy-subrc = 0.
        rv_result = boolc( it_trk_obj_type[] IS NOT INITIAL ).

        LOOP AT it_trk_obj_type ASSIGNING FIELD-SYMBOL(<lv_trk_obj_type>).
          IF is_extractor_exist( iv_trk_obj_type = <lv_trk_obj_type> ) = abap_false.
            rv_result   = abap_false.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD send_idoc_data.
    DATA: lt_bapiret    TYPE bapiret2_t.

    LOOP AT mt_idoc_data ASSIGNING FIELD-SYMBOL(<ls_idoc_data>).
      CALL METHOD zcl_pof_gtt_upd_xtp_references=>send_idoc_ehpost01
        EXPORTING
          it_control      = <ls_idoc_data>-control
          it_tracking_id  = <ls_idoc_data>-tracking_id
          it_exp_event    = <ls_idoc_data>-exp_event
          is_trxserv      = <ls_idoc_data>-trxserv
          iv_appsys       = <ls_idoc_data>-appsys
          it_appobj_ctabs = <ls_idoc_data>-appobj_ctabs
        IMPORTING
          et_bapireturn   = lt_bapiret.

      " collect messages, if it is necessary
      IF et_bapiret IS REQUESTED.
        et_bapiret    = VALUE #( BASE et_bapiret
                                 ( LINES OF lt_bapiret ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_ctp_shipment_data DEFINITION.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_likpdl,
             tknum TYPE vttk-tknum,
             vbeln TYPE likp-vbeln,
             updkz TYPE likpvb-updkz,
           END OF ts_likpdl.

    TYPES: BEGIN OF ts_vbfaex,
             tknum TYPE vttk-tknum.
             INCLUDE TYPE vbfavb.
    TYPES: END OF ts_vbfaex.

    TYPES: BEGIN OF ts_likpex,
             tknum TYPE vttk-tknum.
             INCLUDE TYPE likp.
    TYPES: END OF ts_likpex.

    TYPES: BEGIN OF ts_stops,
             tknum    TYPE vttk-tknum,
             stops    TYPE lif_app_types=>tt_stops,
             watching TYPE lif_app_types=>tt_dlv_watch_stops,
           END OF ts_stops.

    TYPES: ts_vttkvb TYPE vttkvb,
           ts_lips   TYPE lips,
           ts_ee_rel TYPE zpof_gtt_ee_rel.

    TYPES: tt_vttkvb_srt TYPE SORTED TABLE OF ts_vttkvb
                           WITH UNIQUE KEY tknum,
           tt_vttpvb_srt TYPE SORTED TABLE OF vttpvb
                           WITH NON-UNIQUE KEY tknum tpnum,
           tt_vttsvb_srt TYPE SORTED TABLE OF vttsvb
                           WITH NON-UNIQUE KEY tknum tsnum,
           tt_vtspvb_srt TYPE SORTED TABLE OF vtspvb
                           WITH UNIQUE KEY tknum tsnum tpnum
                           WITH NON-UNIQUE SORTED KEY vtts
                             COMPONENTS tknum tsnum,
           tt_vbfaex_srt TYPE SORTED TABLE OF ts_vbfaex
                           WITH NON-UNIQUE KEY tknum vbelv vbeln,
*                           WITH NON-UNIQUE SORTED KEY skey1
*                             COMPONENTS tknum vbelv,
           tt_likpex_srt TYPE SORTED TABLE OF ts_likpex
                           WITH UNIQUE KEY tknum vbeln,
           tt_likpdl_srt TYPE SORTED TABLE OF ts_likpdl
                           WITH UNIQUE KEY vbeln tknum,
           tt_lips_srt   TYPE SORTED TABLE OF ts_lips
                           WITH UNIQUE KEY vbeln posnr,
           tt_stops_srt  TYPE SORTED TABLE OF ts_stops
                           WITH UNIQUE KEY tknum,
           tt_ee_rel_srt TYPE SORTED TABLE OF ts_ee_rel
                           WITH UNIQUE KEY appobjid.

    TYPES: BEGIN OF MESH ts_shipment_merge,
             vttk     TYPE tt_vttkvb_srt
                        ASSOCIATION vttp TO vttp ON tknum = tknum
                        ASSOCIATION vttp_dlt TO vttp_dlt ON tknum = tknum,
             vttp     TYPE tt_vttpvb_srt,
             vttp_dlt TYPE tt_vttpvb_srt,
             vtts     TYPE tt_vttsvb_srt,
             vtts_dlt TYPE tt_vttsvb_srt
                        ASSOCIATION vtsp TO vtsp ON tknum = tknum
                                                AND tsnum = tsnum,
             vtsp     TYPE tt_vtspvb_srt
                        ASSOCIATION vttp TO vttp ON tknum = tknum
                                                AND tpnum = tpnum,
             vtsp_dlt TYPE tt_vtspvb_srt
                        ASSOCIATION vttp TO vttp ON tknum = tknum
                                                AND tpnum = tpnum,
             vbfa     TYPE tt_vbfaex_srt,
             likp     TYPE tt_likpex_srt
                        ASSOCIATION vbfa TO vbfa ON tknum = tknum
                                                AND vbelv = vbeln
                        ASSOCIATION likp_dlt TO likp_dlt ON vbeln = vbeln
                        ASSOCIATION lips TO lips ON vbeln = vbeln,
             lips     TYPE tt_lips_srt,
             likp_dlt TYPE tt_likpdl_srt,
             ee_rel   TYPE tt_ee_rel_srt,
           END OF MESH ts_shipment_merge.

    METHODS constructor
      IMPORTING
        is_shipment TYPE cxshipment
      RAISING
        cx_udm_message.

    METHODS get_data
      RETURNING
        VALUE(rr_ship) TYPE REF TO data.

    METHODS get_stops
      RETURNING
        VALUE(rr_stops) TYPE REF TO data.

  PRIVATE SECTION.
    TYPES: tt_tknum   TYPE RANGE OF tknum.

    DATA: ms_ship  TYPE ts_shipment_merge,
          mt_stops TYPE tt_stops_srt.

    METHODS get_vttk_merged_data
      IMPORTING
        is_shipment TYPE cxshipment
      EXPORTING
        et_vttk     TYPE tt_vttkvb_srt
      RAISING
        cx_udm_message.

    METHODS get_vttp_merged_data
      IMPORTING
        is_shipment   TYPE cxshipment
        it_tknum      TYPE tt_tknum
      EXPORTING
        et_vttp_full  TYPE tt_vttpvb_srt
        et_vttp_delta TYPE tt_vttpvb_srt
      RAISING
        cx_udm_message.

    METHODS get_vtts_merged_data
      IMPORTING
        is_shipment   TYPE cxshipment
        it_tknum      TYPE tt_tknum
      EXPORTING
        et_vtts_full  TYPE tt_vttsvb_srt
        et_vtts_delta TYPE tt_vttsvb_srt
      RAISING
        cx_udm_message.

    METHODS get_vtsp_merged_data
      IMPORTING
        is_shipment   TYPE cxshipment
        it_tknum      TYPE tt_tknum
      EXPORTING
        et_vtsp_full  TYPE tt_vtspvb_srt
        et_vtsp_delta TYPE tt_vtspvb_srt
      RAISING
        cx_udm_message.

    METHODS get_likp_delta_data
      IMPORTING
        is_ship       TYPE ts_shipment_merge
        it_tknum      TYPE tt_tknum
      EXPORTING
        et_likp_delta TYPE tt_likpdl_srt
      RAISING
        cx_udm_message.

    METHODS get_vbfa_and_likp_data
      IMPORTING
        is_ship TYPE ts_shipment_merge
      EXPORTING
        et_vbfa TYPE tt_vbfaex_srt
        et_likp TYPE tt_likpex_srt
      RAISING
        cx_udm_message.

    METHODS get_lips_data
      IMPORTING
        is_ship TYPE ts_shipment_merge
      EXPORTING
        et_lips TYPE tt_lips_srt
      RAISING
        cx_udm_message.

    METHODS get_ee_rel_data
      IMPORTING
        is_ship   TYPE ts_shipment_merge
      EXPORTING
        et_ee_rel TYPE tt_ee_rel_srt
      RAISING
        cx_udm_message.

    METHODS init_shipment_data
      IMPORTING
        is_shipment TYPE cxshipment
      EXPORTING
        es_ship     TYPE ts_shipment_merge
      RAISING
        cx_udm_message.

    METHODS init_stops_data
      IMPORTING
        is_shipment TYPE cxshipment
        is_ship     TYPE ts_shipment_merge
      EXPORTING
        et_stops    TYPE tt_stops_srt
      RAISING
        cx_udm_message.

    METHODS is_vtts_changed
      IMPORTING
        is_shipment      TYPE cxshipment
        is_vtts          TYPE vttsvb
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_ctp_shipment_data IMPLEMENTATION.
  METHOD constructor.
    init_shipment_data(
      EXPORTING
        is_shipment = is_shipment
      IMPORTING
        es_ship     = ms_ship ).

    init_stops_data(
      EXPORTING
        is_shipment = is_shipment
        is_ship     = ms_ship
      IMPORTING
        et_stops    = mt_stops ).
  ENDMETHOD.

  METHOD get_data.
    rr_ship   = REF #( ms_ship ).
  ENDMETHOD.

  METHOD get_stops.
    rr_stops  = REF #( mt_stops ).
  ENDMETHOD.

  METHOD init_shipment_data.
    DATA: lt_tknum    TYPE tt_tknum.

    CLEAR: es_ship.

    get_vttk_merged_data(
      EXPORTING
        is_shipment = is_shipment
      IMPORTING
        et_vttk     = es_ship-vttk ).

    lt_tknum    = VALUE #( FOR ls_vttk IN es_ship-vttk
                           ( low    = ls_vttk-tknum
                             sign   = 'I'
                             option = 'EQ' ) ).

    get_vttp_merged_data(
      EXPORTING
        is_shipment   = is_shipment
        it_tknum      = lt_tknum
      IMPORTING
        et_vttp_full  = es_ship-vttp
        et_vttp_delta = es_ship-vttp_dlt ).

    get_vtts_merged_data(
      EXPORTING
        is_shipment   = is_shipment
        it_tknum      = lt_tknum
      IMPORTING
        et_vtts_full  = es_ship-vtts
        et_vtts_delta = es_ship-vtts_dlt ).

    get_vtsp_merged_data(
      EXPORTING
        is_shipment   = is_shipment
        it_tknum      = lt_tknum
      IMPORTING
        et_vtsp_full  = es_ship-vtsp
        et_vtsp_delta = es_ship-vtsp_dlt ).

    get_likp_delta_data(
      EXPORTING
        is_ship       = es_ship
        it_tknum      = lt_tknum
      IMPORTING
        et_likp_delta = es_ship-likp_dlt ).

    get_vbfa_and_likp_data(
      EXPORTING
        is_ship       = es_ship
      IMPORTING
        et_vbfa       = es_ship-vbfa
        et_likp       = es_ship-likp ).

    get_lips_data(
      EXPORTING
        is_ship       = es_ship
      IMPORTING
        et_lips       = es_ship-lips ).

    get_ee_rel_data(
      EXPORTING
        is_ship       = es_ship
      IMPORTING
        et_ee_rel     = es_ship-ee_rel ).
  ENDMETHOD.

  METHOD get_vttk_merged_data.
    CLEAR: et_vttk[].

    " collect all the current shipments
    LOOP AT is_shipment-new_vttk ASSIGNING FIELD-SYMBOL(<ls_vttk_new>).
      IF lcl_sh_tools=>is_appropriate_type(
           ir_vttk = REF #( <ls_vttk_new> ) ) = abap_true.

        et_vttk   = VALUE #( BASE et_vttk
                             ( <ls_vttk_new> ) ).
      ENDIF.
    ENDLOOP.

    " add deleted shipments
    LOOP AT is_shipment-old_vttk ASSIGNING FIELD-SYMBOL(<ls_vttk_old>)
      WHERE updkz = lif_ef_constants=>cs_change_mode-delete.

      IF lcl_sh_tools=>is_appropriate_type(
           ir_vttk = REF #( <ls_vttk_old> ) ) = abap_true.

        et_vttk   = VALUE #( BASE et_vttk
                             ( <ls_vttk_old> ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_vttp_merged_data.
    FIELD-SYMBOLS: <ls_vttp>  TYPE vttpvb.
    CLEAR: et_vttp_delta[].

    et_vttp_full  = is_shipment-new_vttp[].

    LOOP AT is_shipment-old_vttp ASSIGNING <ls_vttp>
      WHERE updkz = lif_ef_constants=>cs_change_mode-delete.

      et_vttp_full  = VALUE #( BASE et_vttp_full
                               ( <ls_vttp> ) ).
    ENDLOOP.

    LOOP AT et_vttp_full ASSIGNING <ls_vttp>
      WHERE tknum IN it_tknum
        AND ( updkz = lif_ef_constants=>cs_change_mode-insert
           OR updkz = lif_ef_constants=>cs_change_mode-delete ).

      et_vttp_delta   = VALUE #( BASE et_vttp_delta
                                 ( <ls_vttp> ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_vtts_merged_data.
    FIELD-SYMBOLS: <ls_vtts>  TYPE vttsvb.
    CLEAR: et_vtts_delta[].

    et_vtts_full  = is_shipment-new_vtts[].

    LOOP AT is_shipment-old_vtts ASSIGNING <ls_vtts>
      WHERE updkz = lif_ef_constants=>cs_change_mode-delete.

      et_vtts_full  = VALUE #( BASE et_vtts_full
                               ( <ls_vtts> ) ).
    ENDLOOP.

    LOOP AT et_vtts_full ASSIGNING <ls_vtts>
      WHERE tknum IN it_tknum.

      IF is_vtts_changed( is_shipment = is_shipment
                          is_vtts     = <ls_vtts> ) = abap_true.

        et_vtts_delta   = VALUE #( BASE et_vtts_delta
                                   ( <ls_vtts> ) ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_vtsp_merged_data.
    FIELD-SYMBOLS: <ls_vtsp>  TYPE vtspvb.
    CLEAR: et_vtsp_delta[].

    et_vtsp_full  = is_shipment-new_vtsp[].

    LOOP AT is_shipment-old_vtsp ASSIGNING <ls_vtsp>
      WHERE updkz = lif_ef_constants=>cs_change_mode-delete.

      et_vtsp_full  = VALUE #( BASE et_vtsp_full
                               ( <ls_vtsp> ) ).
    ENDLOOP.

    LOOP AT et_vtsp_full ASSIGNING <ls_vtsp>
      WHERE tknum IN it_tknum
        AND ( updkz = lif_ef_constants=>cs_change_mode-insert
           OR updkz = lif_ef_constants=>cs_change_mode-delete ).

      et_vtsp_delta   = VALUE #( BASE et_vtsp_delta
                                 ( <ls_vtsp> ) ).
    ENDLOOP.
  ENDMETHOD.

  METHOD get_likp_delta_data.
    DATA: ls_likp_delta   TYPE ts_likpdl.

    CLEAR: et_likp_delta[].

    FIELD-SYMBOLS: <ls_vttp> TYPE vttpvb,
                   <ls_vtsp> TYPE vtspvb,
                   <ls_vtts> TYPE vttsvb.

    LOOP AT is_ship-vttp_dlt ASSIGNING <ls_vttp>
      WHERE updkz = lif_ef_constants=>cs_change_mode-insert
         OR updkz = lif_ef_constants=>cs_change_mode-delete.

      IF NOT line_exists( et_likp_delta[ KEY primary_key
                                         COMPONENTS tknum = <ls_vttp>-tknum
                                                    vbeln = <ls_vttp>-vbeln ] ).
        ls_likp_delta = CORRESPONDING #( <ls_vttp> ).
        INSERT ls_likp_delta INTO TABLE et_likp_delta.
      ENDIF.
    ENDLOOP.

    LOOP AT is_ship-vtsp_dlt ASSIGNING <ls_vtsp>
      WHERE updkz = lif_ef_constants=>cs_change_mode-insert
         OR updkz = lif_ef_constants=>cs_change_mode-delete.
      ASSIGN is_ship-vtsp_dlt\vttp[ <ls_vtsp> ] TO <ls_vttp>.

      IF sy-subcs = 0.
        IF NOT line_exists( et_likp_delta[ KEY primary_key
                                           COMPONENTS tknum = <ls_vttp>-tknum
                                                      vbeln = <ls_vttp>-vbeln ] ).
          ls_likp_delta = CORRESPONDING #( <ls_vttp> ).
          INSERT ls_likp_delta INTO TABLE et_likp_delta.
        ENDIF.
      ENDIF.
    ENDLOOP.

    LOOP AT is_ship-vtts_dlt ASSIGNING <ls_vtts>.
      LOOP AT is_ship-vtts_dlt\vtsp[ <ls_vtts> ] ASSIGNING <ls_vtsp>.
        ASSIGN is_ship-vtsp_dlt\vttp[ <ls_vtsp> ] TO <ls_vttp>.

        IF sy-subcs = 0.
          IF NOT line_exists( et_likp_delta[ KEY primary_key
                                             COMPONENTS tknum = <ls_vttp>-tknum
                                                        vbeln = <ls_vttp>-vbeln ] ).
            ls_likp_delta = CORRESPONDING #( <ls_vttp> ).
            INSERT ls_likp_delta INTO TABLE et_likp_delta.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_vbfa_and_likp_data.
    DATA: ls_comwa6 TYPE vbco6,
          lt_vbfas  TYPE STANDARD TABLE OF vbfas,
          lv_vbeln  TYPE likp-vbeln,
          ls_vbfa   TYPE ts_vbfaex,
          ls_likp   TYPE ts_likpex,
          ls_vttk   TYPE vttk.

    CLEAR: et_vbfa[], et_likp[].

    LOOP AT is_ship-likp_dlt ASSIGNING FIELD-SYMBOL(<ls_likp_dlt>).
      CLEAR: ls_comwa6, lt_vbfas.
      MOVE-CORRESPONDING <ls_likp_dlt> TO ls_comwa6.

      CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
        EXPORTING
          comwa         = ls_comwa6
        TABLES
          vbfa_tab      = lt_vbfas
        EXCEPTIONS
          no_vbfa       = 1
          no_vbuk_found = 2
          OTHERS        = 3.

      IF sy-subrc = 0.
        LOOP AT lt_vbfas ASSIGNING FIELD-SYMBOL(<ls_vbfas>)
          WHERE vbtyp_n = lif_app_constants=>cs_vbtyp-shipment
            AND vbtyp_v = lif_app_constants=>cs_vbtyp-delivery.

          SELECT SINGLE *
            INTO ls_vttk
            FROM vttk
            WHERE tknum = <ls_vbfas>-vbeln.

          IF sy-subrc = 0 AND
             lcl_sh_tools=>is_appropriate_type(
               ir_vttk = REF #( ls_vttk ) ) = abap_true.

            SELECT SINGLE *
              INTO CORRESPONDING FIELDS OF ls_likp
              FROM likp
              WHERE vbeln = <ls_vbfas>-vbelv.

            IF sy-subrc = 0 AND
               lcl_dl_tools=>is_appropriate_dl_type(
                 ir_struct = REF #( ls_likp ) ) = abap_true.

              ls_vbfa       = CORRESPONDING #( <ls_vbfas> ).
              ls_vbfa-tknum = <ls_likp_dlt>-tknum.

              INSERT ls_vbfa INTO TABLE et_vbfa.

              IF NOT line_exists( et_likp[ KEY primary_key
                                           COMPONENTS tknum = <ls_likp_dlt>-tknum
                                                      vbeln = ls_likp-vbeln ] ).
                ls_likp-tknum = <ls_likp_dlt>-tknum.
                INSERT ls_likp INTO TABLE et_likp.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDLOOP.

    LOOP AT is_ship-vttp_dlt ASSIGNING FIELD-SYMBOL(<ls_vttp_dlt>).
      " DELETED
      IF <ls_vttp_dlt>-updkz = lif_ef_constants=>cs_change_mode-delete.
        DELETE et_vbfa
          WHERE vbeln = <ls_vttp_dlt>-tknum
            AND vbelv = <ls_vttp_dlt>-vbeln.

        " ADDED
      ELSEIF <ls_vttp_dlt>-updkz = lif_ef_constants=>cs_change_mode-insert AND
             NOT line_exists( et_vbfa[ KEY primary_key
                                       COMPONENTS tknum = <ls_vttp_dlt>-tknum
                                                  vbeln = <ls_vttp_dlt>-tknum
                                                  vbelv = <ls_vttp_dlt>-vbeln ] ).

        SELECT SINGLE *
          INTO CORRESPONDING FIELDS OF ls_likp
          FROM likp
          WHERE vbeln = <ls_vttp_dlt>-vbeln.

        IF sy-subrc = 0 AND
               lcl_dl_tools=>is_appropriate_dl_type(
                 ir_struct = REF #( ls_likp ) ) = abap_true.

          ls_vbfa-tknum = <ls_vttp_dlt>-tknum.
          ls_vbfa-vbelv = <ls_vttp_dlt>-vbeln.
          ls_vbfa-vbeln = <ls_vttp_dlt>-tknum.
          INSERT ls_vbfa INTO TABLE et_vbfa.

          IF NOT line_exists( et_likp[ KEY primary_key
                                       COMPONENTS tknum = <ls_vttp_dlt>-tknum
                                                  vbeln = ls_likp-vbeln ] ).
            ls_likp-tknum   = <ls_vttp_dlt>-tknum.
            INSERT ls_likp INTO TABLE et_likp.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_lips_data.
    DATA: ls_lips   TYPE lips.

    CLEAR: et_lips[].

    IF is_ship-likp[] IS NOT INITIAL.
      SELECT * INTO ls_lips
        FROM lips
        FOR ALL ENTRIES IN is_ship-likp
        WHERE vbeln = is_ship-likp-vbeln.

        IF lcl_dl_tools=>is_appropriate_dl_item( ir_struct = REF #( ls_lips ) ) = abap_true.
          INSERT ls_lips INTO TABLE et_lips.
        ENDIF.
      ENDSELECT.
    ENDIF.
  ENDMETHOD.

  METHOD get_ee_rel_data.
    DATA: lt_appobjid TYPE STANDARD TABLE OF ts_ee_rel-appobjid.

    CLEAR: et_ee_rel.

    lt_appobjid   = VALUE #( FOR ls_likp IN is_ship-likp
                             ( ls_likp-vbeln ) ).

    lt_appobjid   = VALUE #( BASE lt_appobjid
                             FOR ls_lips IN is_ship-lips
                             ( |{ ls_lips-vbeln }{ ls_lips-posnr }| ) ).

    IF lt_appobjid[] IS NOT INITIAL.
      SELECT *
        INTO TABLE et_ee_rel
        FROM zpof_gtt_ee_rel
        FOR ALL ENTRIES IN lt_appobjid
        WHERE appobjid = lt_appobjid-table_line.
    ENDIF.
  ENDMETHOD.

  METHOD init_stops_data.
    DATA: lt_stops    TYPE lif_app_types=>tt_stops,
          lt_watching TYPE lif_app_types=>tt_dlv_watch_stops.

    FIELD-SYMBOLS: <ls_stops> TYPE ts_stops.

    CLEAR: et_stops[].

    "initiate table
    LOOP AT is_ship-vttk ASSIGNING FIELD-SYMBOL(<ls_vttk>).
      et_stops    = VALUE #( BASE et_stops
                             ( tknum = <ls_vttk>-tknum ) ).
    ENDLOOP.

    LOOP AT is_ship-vbfa ASSIGNING FIELD-SYMBOL(<ls_vbfa>).
      CLEAR: lt_stops[], lt_watching[].

      IF <ls_vbfa>-vbeln = <ls_vbfa>-tknum.
        lcl_sh_tools=>get_stops_from_shipment(
          EXPORTING
            iv_tknum              = <ls_vbfa>-tknum
            it_vtts               = is_shipment-new_vtts
            it_vtsp               = is_shipment-new_vtsp
            it_vttp               = is_shipment-new_vttp
          IMPORTING
            et_stops              = lt_stops
            et_dlv_watching_stops = lt_watching ).

      ELSE.
        lcl_sh_tools=>get_stops_from_shipment(
          EXPORTING
            iv_tknum              = <ls_vbfa>-vbeln
          IMPORTING
            et_stops              = lt_stops
            et_dlv_watching_stops = lt_watching ).
      ENDIF.

      READ TABLE et_stops ASSIGNING <ls_stops>
        WITH TABLE KEY tknum = <ls_vbfa>-tknum.

      IF sy-subrc = 0.
        <ls_stops>-stops    = VALUE #( BASE <ls_stops>-stops
                                       ( LINES OF lt_stops ) ).

        <ls_stops>-watching = VALUE #( BASE <ls_stops>-watching
                                       ( LINES OF lt_watching ) ).
      ELSE.
        MESSAGE e005(zpof_gtt) WITH |{ <ls_vbfa>-tknum }| 'STOPS' INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
      ENDIF.
    ENDLOOP.

    LOOP AT et_stops ASSIGNING <ls_stops>.
      SORT <ls_stops>-stops    BY stopid loccat.
      SORT <ls_stops>-watching BY vbeln stopid loccat.

      DELETE ADJACENT DUPLICATES FROM <ls_stops>-stops
        COMPARING stopid loccat.
      DELETE ADJACENT DUPLICATES FROM <ls_stops>-watching
        COMPARING vbeln stopid loccat.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_vtts_changed.
    IF is_vtts-updkz = lif_ef_constants=>cs_change_mode-insert OR
       is_vtts-updkz = lif_ef_constants=>cs_change_mode-delete.
      rv_result   = abap_true.
    ELSEIF ( is_vtts-updkz = lif_ef_constants=>cs_change_mode-update OR
             is_vtts-updkz = lif_ef_constants=>cs_change_mode-undefined ).

      READ TABLE is_shipment-old_vtts ASSIGNING FIELD-SYMBOL(<ls_vtts_old>)
        WITH KEY tknum = is_vtts-tknum
                 tsnum = is_vtts-tsnum
                 BINARY SEARCH.

      rv_result = boolc(
         sy-subrc = 0 AND
         ( is_vtts-knota <> <ls_vtts_old>-knota OR
           is_vtts-vstel <> <ls_vtts_old>-vstel OR
           is_vtts-werka <> <ls_vtts_old>-werka OR
           is_vtts-kunna <> <ls_vtts_old>-kunna OR
           is_vtts-lifna <> <ls_vtts_old>-lifna OR
           is_vtts-kunnz <> <ls_vtts_old>-kunnz OR
           is_vtts-vstez <> <ls_vtts_old>-vstez OR
           is_vtts-lifnz <> <ls_vtts_old>-lifnz OR
           is_vtts-werkz <> <ls_vtts_old>-werkz OR
           is_vtts-knotz <> <ls_vtts_old>-knotz OR
           is_vtts-dptbg <> <ls_vtts_old>-dptbg OR
           is_vtts-uptbg <> <ls_vtts_old>-uptbg OR
           is_vtts-dpten <> <ls_vtts_old>-dpten OR
           is_vtts-upten <> <ls_vtts_old>-upten )
       ).
    ELSE.
      rv_result   = abap_false.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_ctp_sender_sh_to_dl_head DEFINITION
  INHERITING FROM lcl_ctp_sender.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_updater) TYPE REF TO lcl_ctp_sender_sh_to_dl_head
      RAISING
        cx_udm_message.

    METHODS prepare_idoc_data
      IMPORTING
        io_ship_data TYPE REF TO lcl_ctp_shipment_data
      RAISING
        cx_udm_message.

  PROTECTED SECTION.
    METHODS get_aotype_restrictions
        REDEFINITION.

    METHODS get_object_type
        REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF cs_mapping,
                 vbeln                 TYPE /saptrx/paramname VALUE 'YN_DLV_NO',
                 shp_count             TYPE /saptrx/paramname VALUE 'YN_SHP_LINE_COUNT',
                 shp_tknum             TYPE /saptrx/paramname VALUE 'YN_SHP_NO',
                 shp_fstop             TYPE /saptrx/paramname VALUE 'YN_SHP_FIRST_STOP',
                 shp_lstop             TYPE /saptrx/paramname VALUE 'YN_SHP_LAST_STOP',
                 shp_lstop_rec_loc     TYPE /saptrx/paramname VALUE 'YN_SHP_LAST_STOP_REC_LOC',
                 shp_lstop_rec_loc_typ TYPE /saptrx/paramname VALUE 'YN_SHP_LAST_STOP_REC_LOC_TYP',
                 pod_relevant          TYPE /saptrx/paramname VALUE 'YN_DL_POD_RELEVANT',
               END OF cs_mapping.

    METHODS fill_idoc_appobj_ctabs
      IMPORTING
        is_aotype    TYPE ts_aotype
        iv_vbeln     TYPE likp-vbeln
      CHANGING
        cs_idoc_data TYPE ts_idoc_data.

    METHODS fill_idoc_control_data
      IMPORTING
        is_aotype    TYPE ts_aotype
        is_ship      TYPE lcl_ctp_shipment_data=>ts_shipment_merge
        is_likp      TYPE lcl_ctp_shipment_data=>ts_likpex
        is_stops     TYPE lcl_ctp_shipment_data=>ts_stops
      CHANGING
        cs_idoc_data TYPE ts_idoc_data
      RAISING
        cx_udm_message.

    METHODS fill_idoc_exp_event
      IMPORTING
        is_aotype    TYPE ts_aotype
        is_ship      TYPE lcl_ctp_shipment_data=>ts_shipment_merge
        is_likp      TYPE lcl_ctp_shipment_data=>ts_likpex
        is_stops     TYPE lcl_ctp_shipment_data=>ts_stops
      CHANGING
        cs_idoc_data TYPE ts_idoc_data
      RAISING
        cx_udm_message.

    METHODS fill_idoc_tracking_id
      IMPORTING
        is_aotype    TYPE ts_aotype
        is_ship      TYPE lcl_ctp_shipment_data=>ts_shipment_merge
        is_likp      TYPE lcl_ctp_shipment_data=>ts_likpex
      CHANGING
        cs_idoc_data TYPE ts_idoc_data
      RAISING
        cx_udm_message.

    METHODS get_delivery_head_planned_evt
      IMPORTING
        is_aotype    TYPE ts_aotype
        is_likp      TYPE lif_app_types=>ts_likpvb
        it_lips      TYPE lif_app_types=>tt_lipsvb
      EXPORTING
        et_exp_event TYPE /saptrx/bapi_trk_ee_tab
      RAISING
        cx_udm_message.

    METHODS get_delivery_head_tracking_id
      IMPORTING
        is_likp               TYPE lif_app_types=>ts_likpvb
      RETURNING
        VALUE(rv_tracking_id) TYPE /saptrx/trxid.

    METHODS get_shipment_stop
      IMPORTING
        is_stops         TYPE lcl_ctp_shipment_data=>ts_stops
        is_vbfa          TYPE lcl_ctp_shipment_data=>ts_vbfaex
        iv_arrival       TYPE abap_bool
      EXPORTING
        es_stop          TYPE lif_app_types=>ts_stops
      RETURNING
        VALUE(rv_stopid) TYPE lif_app_types=>tv_stopid
      RAISING
        cx_udm_message.

    METHODS is_pod_relevant_delivery
      IMPORTING
        is_ship          TYPE lcl_ctp_shipment_data=>ts_shipment_merge
        is_likp          TYPE lcl_ctp_shipment_data=>ts_likpex
        it_stops         TYPE lif_app_types=>tt_stops
      RETURNING
        VALUE(rv_result) TYPE lif_ef_types=>tv_condition.

    METHODS is_pod_relevant_stop
      IMPORTING
        is_ship          TYPE lcl_ctp_shipment_data=>ts_shipment_merge
        is_likp          TYPE lcl_ctp_shipment_data=>ts_likpex
        is_stops         TYPE lif_app_types=>ts_stops
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

ENDCLASS.

CLASS lcl_ctp_sender_sh_to_dl_head IMPLEMENTATION.
  METHOD get_aotype_restrictions.
    et_aotype = VALUE #( (
      low     = 'ZPOF_GTT_*_DL_HD'
      option  = 'CP'
      sign    = 'I'
    ) ).
  ENDMETHOD.

  METHOD get_instance.
    DATA(lt_trk_obj_type) = VALUE tt_trk_obj_type(
       ( lif_ef_constants=>cs_trk_obj_type-esc_shipmt )
       ( lif_ef_constants=>cs_trk_obj_type-esc_deliv )
    ).

    IF is_gtt_enabled( it_trk_obj_type = lt_trk_obj_type ) = abap_true.
      ro_updater  = NEW #( ).

      ro_updater->initiate( ).
    ELSE.
      MESSAGE e006(zpof_gtt) INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_object_type.
    rv_objtype  = lif_ef_constants=>cs_trk_obj_type-esc_deliv.
  ENDMETHOD.

  METHOD get_shipment_stop.
    LOOP AT is_stops-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
      WHERE vbeln       = is_vbfa-vbelv
        AND stopid(10)  = is_vbfa-vbeln.
      rv_stopid   = <ls_watching>-stopid.

      IF iv_arrival = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF rv_stopid IS NOT INITIAL AND
       es_stop IS REQUESTED.
      READ TABLE is_stops-stops
        INTO es_stop
        WITH KEY stopid = rv_stopid
                 loccat = COND #( WHEN iv_arrival = abap_true
                                    THEN lif_app_constants=>cs_loccat-arrival
                                    ELSE lif_app_constants=>cs_loccat-departure ).
      IF sy-subrc <> 0.
        MESSAGE e011(zpof_gtt) WITH rv_stopid is_stops-tknum INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD fill_idoc_appobj_ctabs.
    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = |{ iv_vbeln }|
    ) ).
  ENDMETHOD.

  METHOD fill_idoc_control_data.
    DATA: lt_control TYPE /saptrx/bapi_trk_control_tab,
          lv_count   TYPE i VALUE 0.

    " PO Item key data (obligatory)
    lt_control  = VALUE #(
      (
        paramname = cs_mapping-vbeln
        value     = is_likp-vbeln
      )
      (
        paramname = cs_mapping-pod_relevant
        value     = is_pod_relevant_delivery(
                      is_ship  = is_ship
                      is_likp  = is_likp
                      it_stops = is_stops-stops )
      )
      (
        paramname = lif_ef_constants=>cs_system_fields-actual_bisiness_timezone
        value     = lcl_tools=>get_system_time_zone( )
      )
      (
        paramname = lif_ef_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = lif_ef_constants=>cs_system_fields-actual_technical_timezone
        value     = lcl_tools=>get_system_time_zone( )
      )
      (
        paramname = lif_ef_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
    ).

    LOOP AT is_ship-likp\vbfa[ is_likp ] ASSIGNING FIELD-SYMBOL(<ls_vbfa>).
      ADD 1 TO lv_count.

      get_shipment_stop(
        EXPORTING
          is_stops   = is_stops
          is_vbfa    = <ls_vbfa>
          iv_arrival = abap_true
        IMPORTING
          es_stop    = DATA(ls_lstop) ).

      lt_control  = VALUE #( BASE lt_control
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_count
          value      = |{ lv_count }|
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_tknum
          value      = <ls_vbfa>-vbeln
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_fstop
          value      = get_shipment_stop(
                         EXPORTING
                           is_stops   = is_stops
                           is_vbfa    = <ls_vbfa>
                           iv_arrival = abap_false )
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_lstop
          value      = ls_lstop-stopid
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_lstop_rec_loc
          value      = ls_lstop-locid
        )
        (
          paramindex = lv_count
          paramname  = cs_mapping-shp_lstop_rec_loc_typ
          value      = ls_lstop-loctype
        )
      ).
    ENDLOOP.

    IF sy-subrc <> 0.
      lt_control  = VALUE #( BASE lt_control (
          paramindex = '1'
          paramname  = cs_mapping-shp_count
          value      = ''
      ) ).
    ENDIF.

    " fill technical data into all control data records
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid   = is_likp-vbeln.
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).
  ENDMETHOD.

  METHOD fill_idoc_exp_event.
    DATA: lt_exp_event TYPE /saptrx/bapi_trk_ee_tab.

    get_delivery_head_planned_evt(
      EXPORTING
        is_aotype    = is_aotype
        is_likp      = CORRESPONDING #( is_likp )
        it_lips      = CORRESPONDING #( is_ship-lips )
      IMPORTING
        et_exp_event = lt_exp_event ).


    LOOP AT is_stops-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
      WHERE vbeln = is_likp-vbeln.

      READ TABLE is_stops-stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
        WITH KEY stopid = <ls_watching>-stopid
                 loccat = <ls_watching>-loccat.

      IF sy-subrc = 0.
        " Departure / Arrival
        lt_exp_event = VALUE #( BASE lt_exp_event (
            milestone         = COND #( WHEN <ls_watching>-loccat = lif_app_constants=>cs_loccat-departure
                                          THEN lif_app_constants=>cs_milestone-sh_departure
                                          ELSE lif_app_constants=>cs_milestone-sh_arrival )
            locid2            = <ls_stops>-stopid
            loctype           = <ls_stops>-loctype
            locid1            = <ls_stops>-locid
            evt_exp_datetime  = <ls_stops>-pln_evt_datetime
            evt_exp_tzone     = <ls_stops>-pln_evt_timezone
        ) ).

        " POD
        IF <ls_stops>-loccat  = lif_app_constants=>cs_loccat-arrival AND
           <ls_stops>-loctype = lif_ef_constants=>cs_loc_types-plant AND
           is_pod_relevant_stop( is_ship  = is_ship
                                 is_likp  = is_likp
                                 is_stops = <ls_stops> ) = abap_true.

          lt_exp_event = VALUE #( BASE lt_exp_event (
              milestone         = lif_app_constants=>cs_milestone-sh_pod
              locid2            = <ls_stops>-stopid
              loctype           = <ls_stops>-loctype
              locid1            = <ls_stops>-locid
              evt_exp_datetime  = <ls_stops>-pln_evt_datetime
              evt_exp_tzone     = <ls_stops>-pln_evt_timezone
          ) ).
        ENDIF.
      ELSE.
        MESSAGE e005(zpof_gtt)
          WITH |{ <ls_watching>-stopid }{ <ls_watching>-loccat }| 'STOPS'
          INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
      ENDIF.
    ENDLOOP.

    IF lt_exp_event[] IS INITIAL.
      lt_exp_event = VALUE #( (
          milestone         = ''
          locid2            = ''
          loctype           = ''
          locid1            = ''
          evt_exp_datetime  = '000000000000000'
          evt_exp_tzone     = ''
      ) ).
    ENDIF.

    LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
      <ls_exp_event>-appsys         = mv_appsys.
      <ls_exp_event>-appobjtype     = is_aotype-aot_type.
      <ls_exp_event>-appobjid       = is_likp-vbeln.
      <ls_exp_event>-language       = sy-langu.
      <ls_exp_event>-evt_exp_tzone  = lcl_tools=>get_system_time_zone(  ).
    ENDLOOP.

    cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                      ( LINES OF lt_exp_event ) ).
  ENDMETHOD.

  METHOD fill_idoc_tracking_id.
    "another tip is that: for tracking ID type 'SHIPMENT_ORDER' of delivery header,
    "and for tracking ID type 'RESOURCE' of shipment header,
    "DO NOT enable START DATE and END DATE
    DATA: lt_tracking_id TYPE /saptrx/bapi_trk_trkid_tab.

    " Delivery Header
    lt_tracking_id    = VALUE #( (
      trxcod      = lif_app_constants=>cs_trxcod-dl_number
      trxid       = is_likp-vbeln
      start_date  = lcl_tools=>get_local_timestamp( )
      end_date    = lif_ef_constants=>cv_max_end_date
      timzon      = lcl_tools=>get_system_time_zone( )
    ) ).

    " Shipment
    LOOP AT is_ship-likp\likp_dlt[ is_likp ] ASSIGNING FIELD-SYMBOL(<ls_likp_dlt>).
      IF <ls_likp_dlt>-updkz = lif_ef_constants=>cs_change_mode-insert OR
         <ls_likp_dlt>-updkz = lif_ef_constants=>cs_change_mode-delete.
        lt_tracking_id    = VALUE #( BASE lt_tracking_id (
          trxcod      = lif_app_constants=>cs_trxcod-sh_number
          trxid       = <ls_likp_dlt>-tknum
          action      = COND #( WHEN <ls_likp_dlt>-updkz = lif_ef_constants=>cs_change_mode-delete
                                  THEN lif_ef_constants=>cs_change_mode-delete )
        ) ).
      ENDIF.
    ENDLOOP.

    " Fill general data
    LOOP AT lt_tracking_id ASSIGNING FIELD-SYMBOL(<ls_tracking_id>).
      <ls_tracking_id>-appsys     = mv_appsys.
      <ls_tracking_id>-appobjtype = is_aotype-aot_type.
      <ls_tracking_id>-appobjid   = is_likp-vbeln.
    ENDLOOP.

    cs_idoc_data-tracking_id = VALUE #( BASE cs_idoc_data-tracking_id
                                        ( LINES OF lt_tracking_id ) ).
  ENDMETHOD.

  METHOD get_delivery_head_planned_evt.
    DATA: ls_app_obj_types      TYPE /saptrx/aotypes,
          lt_all_appl_tables    TYPE trxas_tabcontainer,
          lt_app_type_cntl_tabs TYPE trxas_apptype_tabs,
          ls_app_objects        TYPE trxas_appobj_ctab_wa,
          lt_app_objects        TYPE trxas_appobj_ctabs.

    CLEAR: et_exp_event[].

    ls_app_obj_types              = CORRESPONDING #( is_aotype ).

    ls_app_objects                = CORRESPONDING #( ls_app_obj_types ).
    ls_app_objects-appobjtype     = is_aotype-aot_type.
    ls_app_objects-appobjid       = get_delivery_head_tracking_id(
                                      is_likp = CORRESPONDING #( is_likp ) ).
    ls_app_objects-maintabref     = REF #( is_likp ).
    ls_app_objects-maintabdef     = lif_app_constants=>cs_tabledef-dl_header_new.

    lt_app_objects                = VALUE #( ( ls_app_objects ) ).

    lt_all_appl_tables            = VALUE #( (
      tabledef    = lif_app_constants=>cs_tabledef-dl_item_new
      tableref    = REF #( it_lips )
     ) ).

    CALL FUNCTION 'ZPOF_GTT_EE_DL_HDR'
      EXPORTING
        i_appsys                  = mv_appsys
        i_app_obj_types           = CORRESPONDING /saptrx/aotypes( is_aotype )
        i_all_appl_tables         = lt_all_appl_tables
        i_app_type_cntl_tabs      = lt_app_type_cntl_tabs
        i_app_objects             = lt_app_objects
      TABLES
        e_expeventdata            = et_exp_event
      EXCEPTIONS
        parameter_error           = 1
        exp_event_determ_error    = 2
        table_determination_error = 3
        stop_processing           = 4
        OTHERS                    = 5.

    IF sy-subrc <> 0.
      lcl_tools=>throw_exception( ).
    ELSE.
      LOOP AT et_exp_event TRANSPORTING NO FIELDS
        WHERE ( milestone = lif_app_constants=>cs_milestone-sh_arrival OR
                milestone = lif_app_constants=>cs_milestone-sh_departure OR
                milestone = lif_app_constants=>cs_milestone-sh_pod ).
        DELETE et_exp_event.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_delivery_head_tracking_id.
    rv_tracking_id  = |{ is_likp-vbeln }|.
  ENDMETHOD.

  METHOD is_pod_relevant_delivery.
    DATA: lt_werks TYPE RANGE OF lips-werks.

    rv_result   = lif_ef_constants=>cs_condition-false.

    " prepare list of POD relevant plants
    LOOP AT is_ship-likp\lips[ is_likp ] ASSIGNING FIELD-SYMBOL(<ls_lips>).
      IF lt_werks IS INITIAL OR
         <ls_lips>-werks NOT IN lt_werks.

        READ TABLE is_ship-ee_rel ASSIGNING FIELD-SYMBOL(<ls_ee_rel>)
          WITH TABLE KEY appobjid = |{ <ls_lips>-vbeln }{ <ls_lips>-posnr }|.

        IF sy-subrc = 0 AND
           <ls_ee_rel>-z_pdstk = abap_true.

          lt_werks  = VALUE #( BASE lt_werks (
            low       = <ls_lips>-werks
            option    = 'EQ'
            sign      = 'I'
          ) ).
        ENDIF.
      ENDIF.
    ENDLOOP.

    " check whether shipment has any stops with POD Relevant plant
    IF lt_werks[] IS NOT INITIAL.
      LOOP AT it_stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
        WHERE locid    IN lt_werks
          AND loccat   = lif_app_constants=>cs_loccat-arrival
          AND loctype  = lif_ef_constants=>cs_loc_types-plant.

        rv_result   = lif_ef_constants=>cs_condition-true.
        EXIT.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD is_pod_relevant_stop.
    CLEAR: rv_result.

    LOOP AT is_ship-likp\lips[ is_likp ] ASSIGNING FIELD-SYMBOL(<ls_lips>).
      IF is_stops-locid   = <ls_lips>-werks AND
         is_stops-loccat  = lif_app_constants=>cs_loccat-arrival AND
         is_stops-loctype = lif_ef_constants=>cs_loc_types-plant.

        READ TABLE is_ship-ee_rel ASSIGNING FIELD-SYMBOL(<ls_ee_rel>)
          WITH TABLE KEY appobjid = |{ <ls_lips>-vbeln }{ <ls_lips>-posnr }|.

        IF sy-subrc = 0 AND
           <ls_ee_rel>-z_pdstk = abap_true.

          rv_result   = abap_true.
          EXIT.
        ENDIF.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD prepare_idoc_data.
    " prepare DLV Header list (likp or vtrlk_tab)
    " fill DLV Header
    "   control data
    "     YN_SHP_LINE_COUNT
    "     YN_SHP_NO
    "     YN_SHP_FIRST_STOP
    "     YN_SHP_LAST_STOP
    "     YN_SHP_LINE_COUNT
    "     ACTUAL_BUSINESS_TIMEZONE
    "     ACTUAL_BUSINESS_DATETIME
    "   tracking ID
    "     send DLV Header TID
    "     add/delete Shipment TID
    "   planned events
    "     DEPARTURE
    "     ARRIV_DEST
    "     POD

    DATA: ls_idoc_data    TYPE ts_idoc_data.

    DATA(lr_ship)   = io_ship_data->get_data( ).
    DATA(lr_stops)  = io_ship_data->get_stops( ).

    FIELD-SYMBOLS: <ls_ship>  TYPE lcl_ctp_shipment_data=>ts_shipment_merge,
                   <lt_stops> TYPE lcl_ctp_shipment_data=>tt_stops_srt.

    ASSIGN lr_ship->*  TO <ls_ship>.
    ASSIGN lr_stops->* TO <lt_stops>.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT <ls_ship>-likp ASSIGNING FIELD-SYMBOL(<ls_likp>).
        READ TABLE <lt_stops> ASSIGNING FIELD-SYMBOL(<ls_stops>)
          WITH TABLE KEY tknum = <ls_likp>-tknum.

        IF sy-subrc = 0.
          fill_idoc_appobj_ctabs(
            EXPORTING
              is_aotype    = <ls_aotype>
              iv_vbeln     = <ls_likp>-vbeln
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_control_data(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ship      = <ls_ship>
              is_likp      = <ls_likp>
              is_stops     = <ls_stops>
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_exp_event(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ship      = <ls_ship>
              is_likp      = <ls_likp>
              is_stops     = <ls_stops>
            CHANGING
              cs_idoc_data = ls_idoc_data ).

          fill_idoc_tracking_id(
            EXPORTING
              is_aotype    = <ls_aotype>
              is_ship      = <ls_ship>
              is_likp      = <ls_likp>
            CHANGING
              cs_idoc_data = ls_idoc_data ).
        ELSE.
          MESSAGE e005(zpof_gtt) WITH |{ <ls_likp>-tknum }| 'STOPS'
            INTO DATA(lv_dummy).
          lcl_tools=>throw_exception( ).
        ENDIF.

      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_ctp_sender_sh_to_dl_item DEFINITION
  INHERITING FROM lcl_ctp_sender
  CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_updater) TYPE REF TO lcl_ctp_sender_sh_to_dl_item
      RAISING
        cx_udm_message.

    METHODS prepare_idoc_data
      IMPORTING
        io_ship_data TYPE REF TO lcl_ctp_shipment_data
      RAISING
        cx_udm_message.

  PROTECTED SECTION.
    METHODS get_aotype_restrictions
        REDEFINITION.

    METHODS get_object_type
        REDEFINITION.

  PRIVATE SECTION.
    CONSTANTS: BEGIN OF cs_mapping,
                 vbeln TYPE /saptrx/paramname VALUE 'YN_DLV_NO',
                 posnr TYPE /saptrx/paramname VALUE 'YN_DLV_ITEM_NO',
               END OF cs_mapping.

    METHODS fill_idoc_appobj_ctabs
      IMPORTING
        is_aotype    TYPE ts_aotype
        is_lips      TYPE lcl_ctp_shipment_data=>ts_lips
      CHANGING
        cs_idoc_data TYPE ts_idoc_data.

    METHODS fill_idoc_control_data
      IMPORTING
        is_aotype    TYPE ts_aotype
        is_ship      TYPE lcl_ctp_shipment_data=>ts_shipment_merge
        is_likp      TYPE lcl_ctp_shipment_data=>ts_likpex
        is_lips      TYPE lcl_ctp_shipment_data=>ts_lips
        is_stops     TYPE lcl_ctp_shipment_data=>ts_stops
      CHANGING
        cs_idoc_data TYPE ts_idoc_data
      RAISING
        cx_udm_message.

    METHODS fill_idoc_exp_event
      IMPORTING
        is_aotype    TYPE ts_aotype
        is_ship      TYPE lcl_ctp_shipment_data=>ts_shipment_merge
        is_likp      TYPE lcl_ctp_shipment_data=>ts_likpex
        is_lips      TYPE lcl_ctp_shipment_data=>ts_lips
        is_stops     TYPE lcl_ctp_shipment_data=>ts_stops
      CHANGING
        cs_idoc_data TYPE ts_idoc_data
      RAISING
        cx_udm_message.

    METHODS fill_idoc_tracking_id
      IMPORTING
        is_aotype    TYPE ts_aotype
        is_ship      TYPE lcl_ctp_shipment_data=>ts_shipment_merge
        is_likp      TYPE lcl_ctp_shipment_data=>ts_likpex
        is_lips      TYPE lcl_ctp_shipment_data=>ts_lips
      CHANGING
        cs_idoc_data TYPE ts_idoc_data
      RAISING
        cx_udm_message.

    METHODS get_delivery_item_planned_evt
      IMPORTING
        is_aotype    TYPE ts_aotype
        is_likp      TYPE lif_app_types=>ts_likpvb
        is_lips      TYPE lif_app_types=>ts_lipsvb
      EXPORTING
        et_exp_event TYPE /saptrx/bapi_trk_ee_tab
      RAISING
        cx_udm_message.

    METHODS get_delivery_item_tracking_id
      IMPORTING
        is_lips               TYPE lcl_ctp_shipment_data=>ts_lips
      RETURNING
        VALUE(rv_tracking_id) TYPE /saptrx/trxid.

    METHODS get_shipment_stop
      IMPORTING
        is_stops       TYPE lcl_ctp_shipment_data=>ts_stops
        is_vbfa        TYPE lcl_ctp_shipment_data=>ts_vbfaex
        iv_arrival     TYPE abap_bool
      RETURNING
        VALUE(rv_stop) TYPE lif_app_types=>tv_stopid.

    METHODS is_pod_relevant
      IMPORTING
        is_ship          TYPE lcl_ctp_shipment_data=>ts_shipment_merge
        is_lips          TYPE lcl_ctp_shipment_data=>ts_lips
        is_stops         TYPE lif_app_types=>ts_stops
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
ENDCLASS.

CLASS lcl_ctp_sender_sh_to_dl_item IMPLEMENTATION.
  METHOD get_aotype_restrictions.
    et_aotype = VALUE #( (
      low     = 'ZPOF_GTT_*_DL_ITEM'
      option  = 'CP'
      sign    = 'I'
    ) ).
  ENDMETHOD.

  METHOD get_instance.
    DATA(lt_trk_obj_type) = VALUE tt_trk_obj_type(
       ( lif_ef_constants=>cs_trk_obj_type-esc_shipmt )
       ( lif_ef_constants=>cs_trk_obj_type-esc_deliv )
    ).

    IF is_gtt_enabled( it_trk_obj_type = lt_trk_obj_type ) = abap_true.
      ro_updater  = NEW #( ).

      ro_updater->initiate( ).
    ELSE.
      MESSAGE e006(zpof_gtt) INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_object_type.
    rv_objtype  = lif_ef_constants=>cs_trk_obj_type-esc_deliv.
  ENDMETHOD.

  METHOD get_delivery_item_planned_evt.
    DATA: ls_app_obj_types      TYPE /saptrx/aotypes,
          lt_all_appl_tables    TYPE trxas_tabcontainer,
          lt_app_type_cntl_tabs TYPE trxas_apptype_tabs,
          ls_app_objects        TYPE trxas_appobj_ctab_wa,
          lt_app_objects        TYPE trxas_appobj_ctabs,
          lt_lips               TYPE lif_app_types=>tt_lipsvb.

    CLEAR: et_exp_event[].

    ls_app_obj_types              = CORRESPONDING #( is_aotype ).

    ls_app_objects                = CORRESPONDING #( ls_app_obj_types ).
    ls_app_objects-appobjtype     = is_aotype-aot_type.
    ls_app_objects-appobjid       = get_delivery_item_tracking_id(
                                      is_lips = CORRESPONDING #( is_lips ) ).
    ls_app_objects-maintabref     = REF #( is_lips ).
    ls_app_objects-maintabdef     = lif_app_constants=>cs_tabledef-dl_item_new.
    ls_app_objects-mastertabref   = REF #( is_likp ).
    ls_app_objects-mastertabdef   = lif_app_constants=>cs_tabledef-dl_header_new.

    lt_app_objects                = VALUE #( ( ls_app_objects ) ).

    lt_lips                       = VALUE #( ( CORRESPONDING #( is_lips ) ) ).

    lt_all_appl_tables            = VALUE #( (
      tabledef    = lif_app_constants=>cs_tabledef-dl_item_new
      tableref    = REF #( lt_lips )
     ) ).

    CALL FUNCTION 'ZPOF_GTT_EE_DL_ITEM'
      EXPORTING
        i_appsys                  = mv_appsys
        i_app_obj_types           = CORRESPONDING /saptrx/aotypes( is_aotype )
        i_all_appl_tables         = lt_all_appl_tables
        i_app_type_cntl_tabs      = lt_app_type_cntl_tabs
        i_app_objects             = lt_app_objects
      TABLES
        e_expeventdata            = et_exp_event
      EXCEPTIONS
        parameter_error           = 1
        exp_event_determ_error    = 2
        table_determination_error = 3
        stop_processing           = 4
        OTHERS                    = 5.

    IF sy-subrc <> 0.
      lcl_tools=>throw_exception( ).
    ELSE.
      LOOP AT et_exp_event TRANSPORTING NO FIELDS
        WHERE ( milestone = lif_app_constants=>cs_milestone-sh_arrival OR
                milestone = lif_app_constants=>cs_milestone-sh_departure OR
                milestone = lif_app_constants=>cs_milestone-sh_pod ).
        DELETE et_exp_event.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.

  METHOD get_shipment_stop.
    LOOP AT is_stops-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
      WHERE vbeln       = is_vbfa-vbelv
        AND stopid(10)  = is_vbfa-vbeln.
      rv_stop   = <ls_watching>-stopid.

      IF iv_arrival = abap_false.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_delivery_item_tracking_id.
    rv_tracking_id  = |{ is_lips-vbeln }{ is_lips-posnr }|.
  ENDMETHOD.

  METHOD fill_idoc_appobj_ctabs.
    cs_idoc_data-appobj_ctabs = VALUE #( BASE cs_idoc_data-appobj_ctabs (
      trxservername = cs_idoc_data-trxserv-trx_server_id
      appobjtype    = is_aotype-aot_type
      appobjid      = get_delivery_item_tracking_id( is_lips = is_lips )
    ) ).
  ENDMETHOD.

  METHOD fill_idoc_control_data.
    DATA: lt_control    TYPE /saptrx/bapi_trk_control_tab .

    " PO Item key data (obligatory)
    lt_control  = VALUE #(
      (
        paramname = cs_mapping-vbeln
        value     = is_lips-vbeln
      )
      (
        paramname = cs_mapping-posnr
        value     = is_lips-posnr
      )
      (
        paramname = lif_ef_constants=>cs_system_fields-actual_bisiness_timezone
        value     = lcl_tools=>get_system_time_zone( )
      )
      (
        paramname = lif_ef_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = lif_ef_constants=>cs_system_fields-actual_technical_timezone
        value     = lcl_tools=>get_system_time_zone( )
      )
      (
        paramname = lif_ef_constants=>cs_system_fields-actual_technical_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
    ).

    " fill technical data into all control data records
    LOOP AT lt_control ASSIGNING FIELD-SYMBOL(<ls_control>).
      <ls_control>-appsys     = mv_appsys.
      <ls_control>-appobjtype = is_aotype-aot_type.
      <ls_control>-appobjid   = get_delivery_item_tracking_id( is_lips = is_lips ).
    ENDLOOP.

    cs_idoc_data-control  = VALUE #( BASE cs_idoc_data-control
                                     ( LINES OF lt_control ) ).
  ENDMETHOD.

  METHOD fill_idoc_exp_event.
    DATA: lt_exp_event TYPE /saptrx/bapi_trk_ee_tab.

    get_delivery_item_planned_evt(
      EXPORTING
        is_aotype    = is_aotype
        is_likp      = CORRESPONDING #( is_likp )
        is_lips      = CORRESPONDING #( is_lips )
      IMPORTING
        et_exp_event = lt_exp_event ).

    LOOP AT is_stops-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
      WHERE vbeln = is_lips-vbeln.

      READ TABLE is_stops-stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
        WITH KEY stopid = <ls_watching>-stopid
                 loccat = <ls_watching>-loccat.
      IF sy-subrc = 0.
        " Departure / Arrival
        lt_exp_event = VALUE #( BASE lt_exp_event (
            milestone         = COND #( WHEN <ls_watching>-loccat = lif_app_constants=>cs_loccat-departure
                                          THEN lif_app_constants=>cs_milestone-sh_departure
                                          ELSE lif_app_constants=>cs_milestone-sh_arrival )
            locid2            = <ls_stops>-stopid
            loctype           = <ls_stops>-loctype
            locid1            = <ls_stops>-locid
            evt_exp_datetime  = <ls_stops>-pln_evt_datetime
            evt_exp_tzone     = <ls_stops>-pln_evt_timezone
        ) ).

        " POD
        IF <ls_stops>-loccat  = lif_app_constants=>cs_loccat-arrival AND
           <ls_stops>-loctype = lif_ef_constants=>cs_loc_types-plant AND
           is_pod_relevant( is_ship  = is_ship
                            is_lips  = is_lips
                            is_stops = <ls_stops> ) = abap_true.

          lt_exp_event = VALUE #( BASE lt_exp_event (
              milestone         = lif_app_constants=>cs_milestone-sh_pod
              locid2            = <ls_stops>-stopid
              loctype           = <ls_stops>-loctype
              locid1            = <ls_stops>-locid
              evt_exp_datetime  = <ls_stops>-pln_evt_datetime
              evt_exp_tzone     = <ls_stops>-pln_evt_timezone
          ) ).
        ENDIF.
      ELSE.
        MESSAGE e005(zpof_gtt)
          WITH |{ <ls_watching>-stopid }{ <ls_watching>-loccat }| 'STOPS'
          INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
      ENDIF.
    ENDLOOP.

    IF lt_exp_event[] IS INITIAL.
      lt_exp_event = VALUE #( (
          milestone         = ''
          locid2            = ''
          loctype           = ''
          locid1            = ''
          evt_exp_datetime  = '000000000000000'
          evt_exp_tzone     = ''
      ) ).
    ENDIF.

    LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
      <ls_exp_event>-appsys         = mv_appsys.
      <ls_exp_event>-appobjtype     = is_aotype-aot_type.
      <ls_exp_event>-appobjid       = get_delivery_item_tracking_id( is_lips = is_lips ).
      <ls_exp_event>-language       = sy-langu.
      <ls_exp_event>-evt_exp_tzone  = lcl_tools=>get_system_time_zone(  ).
    ENDLOOP.

    cs_idoc_data-exp_event = VALUE #( BASE cs_idoc_data-exp_event
                                      ( LINES OF lt_exp_event ) ).
  ENDMETHOD.

  METHOD fill_idoc_tracking_id.
*    "another tip is that: for tracking ID type 'SHIPMENT_ORDER' of delivery header,
*    "and for tracking ID type 'RESOURCE' of shipment header,
*    "DO NOT enable START DATE and END DATE

    " Delivery Item
    cs_idoc_data-tracking_id    = VALUE #( BASE cs_idoc_data-tracking_id (
      appsys      = mv_appsys
      appobjtype  = is_aotype-aot_type
      appobjid    = get_delivery_item_tracking_id( is_lips = is_lips )
      trxcod      = lif_app_constants=>cs_trxcod-dl_position
      trxid       = get_delivery_item_tracking_id( is_lips = is_lips )
      start_date  = lcl_tools=>get_local_timestamp( )
      end_date    = lif_ef_constants=>cv_max_end_date
      timzon      = lcl_tools=>get_system_time_zone( )
    ) ).
  ENDMETHOD.

  METHOD is_pod_relevant.
    CLEAR: rv_result.

    IF is_stops-locid   = is_lips-werks AND
       is_stops-loctype = lif_ef_constants=>cs_loc_types-plant.

      READ TABLE is_ship-ee_rel ASSIGNING FIELD-SYMBOL(<ls_ee_rel>)
        WITH TABLE KEY appobjid = |{ is_lips-vbeln }{ is_lips-posnr }|.

      rv_result = boolc( sy-subrc = 0 AND
                         <ls_ee_rel>-z_pdstk = abap_true ).
    ENDIF.
  ENDMETHOD.

  METHOD prepare_idoc_data.
    " fill DLV Item
    "   control data
    "     YN_DLV_NO
    "     YN_DLV_ITEM_NO
    "     ACTUAL_BUSINESS_TIMEZONE
    "     ACTUAL_BUSINESS_DATETIME
    "   planned events
    "     PUTAWAY
    "     PACKING
    "     GOODS_RECEIPT
    "     DEPARTURE
    "     ARRIV_DEST
    "     POD
    DATA: ls_idoc_data    TYPE ts_idoc_data.

    DATA(lr_ship)   = io_ship_data->get_data( ).
    DATA(lr_stops)  = io_ship_data->get_stops( ).

    FIELD-SYMBOLS: <ls_ship>  TYPE lcl_ctp_shipment_data=>ts_shipment_merge,
                   <lt_stops> TYPE lcl_ctp_shipment_data=>tt_stops_srt.

    ASSIGN lr_ship->*  TO <ls_ship>.
    ASSIGN lr_stops->* TO <lt_stops>.

    LOOP AT mt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CLEAR: ls_idoc_data.

      ls_idoc_data-appsys   = mv_appsys.

      fill_idoc_trxserv(
        EXPORTING
          is_aotype    = <ls_aotype>
        CHANGING
          cs_idoc_data = ls_idoc_data ).

      LOOP AT <ls_ship>-likp ASSIGNING FIELD-SYMBOL(<ls_likp>).
        READ TABLE <lt_stops> ASSIGNING FIELD-SYMBOL(<ls_stops>)
          WITH TABLE KEY tknum = <ls_likp>-tknum.

        IF sy-subrc = 0.
          LOOP AT <ls_ship>-likp\lips[ <ls_likp> ] ASSIGNING FIELD-SYMBOL(<ls_lips>).
            fill_idoc_appobj_ctabs(
              EXPORTING
                is_aotype    = <ls_aotype>
                is_lips      = <ls_lips>
              CHANGING
                cs_idoc_data = ls_idoc_data ).

            fill_idoc_control_data(
              EXPORTING
                is_aotype    = <ls_aotype>
                is_ship      = <ls_ship>
                is_likp      = <ls_likp>
                is_lips      = <ls_lips>
                is_stops     = <ls_stops>
              CHANGING
                cs_idoc_data = ls_idoc_data ).

            fill_idoc_exp_event(
              EXPORTING
                is_aotype    = <ls_aotype>
                is_ship      = <ls_ship>
                is_likp      = <ls_likp>
                is_lips      = <ls_lips>
                is_stops     = <ls_stops>
              CHANGING
                cs_idoc_data = ls_idoc_data ).

            fill_idoc_tracking_id(
              EXPORTING
                is_aotype    = <ls_aotype>
                is_ship      = <ls_ship>
                is_likp      = <ls_likp>
                is_lips      = <ls_lips>
              CHANGING
                cs_idoc_data = ls_idoc_data ).
          ENDLOOP.
        ELSE.
          MESSAGE e005(zpof_gtt) WITH |{ <ls_likp>-tknum }| 'STOPS'
            INTO DATA(lv_dummy).
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.

      IF ls_idoc_data-appobj_ctabs[] IS NOT INITIAL AND
         ls_idoc_data-control[] IS NOT INITIAL AND
         ls_idoc_data-tracking_id[] IS NOT INITIAL.
        APPEND ls_idoc_data TO mt_idoc_data.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
