CLASS zcl_gtt_pof_ctp_shipment_data DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  TYPES:
    BEGIN OF ts_likpdl,
             tknum TYPE vttk-tknum,
             vbeln TYPE likp-vbeln,
             updkz TYPE likpvb-updkz,
           END OF ts_likpdl .
  TYPES:
    BEGIN OF ts_vbfaex,
             tknum TYPE vttk-tknum.
             INCLUDE TYPE vbfavb.
    TYPES: END OF ts_vbfaex .
  TYPES:
    BEGIN OF ts_likpex,
             tknum TYPE vttk-tknum.
             INCLUDE TYPE likp.
    TYPES: END OF ts_likpex .
  TYPES:
    BEGIN OF ts_stops,
             tknum    TYPE vttk-tknum,
             stops    TYPE zif_gtt_pof_app_types=>tt_stops,
             watching TYPE zif_gtt_pof_app_types=>tt_dlv_watch_stops,
           END OF ts_stops .
  TYPES ts_vttkvb TYPE vttkvb .
  TYPES ts_lips TYPE lips .
  TYPES ts_ee_rel TYPE zpof_gtt_ee_rel .
  TYPES:
    tt_vttkvb_srt TYPE SORTED TABLE OF ts_vttkvb
                           WITH UNIQUE KEY tknum .
  TYPES:
    tt_vttpvb_srt TYPE SORTED TABLE OF vttpvb
                           WITH NON-UNIQUE KEY tknum tpnum .
  TYPES:
    tt_vttsvb_srt TYPE SORTED TABLE OF vttsvb
                           WITH NON-UNIQUE KEY tknum tsnum .
  TYPES:
    tt_vtspvb_srt TYPE SORTED TABLE OF vtspvb
                           WITH UNIQUE KEY tknum tsnum tpnum
                           WITH NON-UNIQUE SORTED KEY vtts
                             COMPONENTS tknum tsnum .
  TYPES:
    tt_vbfaex_srt TYPE SORTED TABLE OF ts_vbfaex
                           WITH NON-UNIQUE KEY tknum vbelv vbeln .
  TYPES:
    tt_likpex_srt TYPE SORTED TABLE OF ts_likpex
                           WITH UNIQUE KEY tknum vbeln .
  TYPES:
    tt_likpdl_srt TYPE SORTED TABLE OF ts_likpdl
                           WITH UNIQUE KEY vbeln tknum .
  TYPES:
    tt_lips_srt   TYPE SORTED TABLE OF ts_lips
                           WITH UNIQUE KEY vbeln posnr .
  TYPES:
    tt_stops_srt  TYPE SORTED TABLE OF ts_stops
                           WITH UNIQUE KEY tknum .
  TYPES:
    tt_ee_rel_srt TYPE SORTED TABLE OF ts_ee_rel
                           WITH UNIQUE KEY appobjid .
  TYPES:
    BEGIN OF MESH ts_shipment_merge,
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
           END OF MESH ts_shipment_merge .

  METHODS constructor
    IMPORTING
      !is_shipment TYPE cxshipment
    RAISING
      cx_udm_message .
  METHODS get_data
    RETURNING
      VALUE(rr_ship) TYPE REF TO data .
  METHODS get_stops
    RETURNING
      VALUE(rr_stops) TYPE REF TO data .
PROTECTED SECTION.
PRIVATE SECTION.

  TYPES:
    tt_tknum   TYPE RANGE OF tknum .

  DATA ms_ship TYPE ts_shipment_merge .
  DATA mt_stops TYPE tt_stops_srt .

  METHODS get_vttk_merged_data
    IMPORTING
      !is_shipment TYPE cxshipment
    EXPORTING
      !et_vttk TYPE tt_vttkvb_srt
    RAISING
      cx_udm_message .
  METHODS get_vttp_merged_data
    IMPORTING
      !is_shipment TYPE cxshipment
      !it_tknum TYPE tt_tknum
    EXPORTING
      !et_vttp_full TYPE tt_vttpvb_srt
      !et_vttp_delta TYPE tt_vttpvb_srt
    RAISING
      cx_udm_message .
  METHODS get_vtts_merged_data
    IMPORTING
      !is_shipment TYPE cxshipment
      !it_tknum TYPE tt_tknum
    EXPORTING
      !et_vtts_full TYPE tt_vttsvb_srt
      !et_vtts_delta TYPE tt_vttsvb_srt
    RAISING
      cx_udm_message .
  METHODS get_vtsp_merged_data
    IMPORTING
      !is_shipment TYPE cxshipment
      !it_tknum TYPE tt_tknum
    EXPORTING
      !et_vtsp_full TYPE tt_vtspvb_srt
      !et_vtsp_delta TYPE tt_vtspvb_srt
    RAISING
      cx_udm_message .
  METHODS get_likp_delta_data
    IMPORTING
      !is_ship TYPE ts_shipment_merge
      !it_tknum TYPE tt_tknum
    EXPORTING
      !et_likp_delta TYPE tt_likpdl_srt
    RAISING
      cx_udm_message .
  METHODS get_vbfa_and_likp_data
    IMPORTING
      !is_ship TYPE ts_shipment_merge
    EXPORTING
      !et_vbfa TYPE tt_vbfaex_srt
      !et_likp TYPE tt_likpex_srt
    RAISING
      cx_udm_message .
  METHODS get_lips_data
    IMPORTING
      !is_ship TYPE ts_shipment_merge
    EXPORTING
      !et_lips TYPE tt_lips_srt
    RAISING
      cx_udm_message .
  METHODS get_ee_rel_data
    IMPORTING
      !is_ship TYPE ts_shipment_merge
    EXPORTING
      !et_ee_rel TYPE tt_ee_rel_srt
    RAISING
      cx_udm_message .
  METHODS init_shipment_data
    IMPORTING
      !is_shipment TYPE cxshipment
    EXPORTING
      !es_ship TYPE ts_shipment_merge
    RAISING
      cx_udm_message .
  METHODS init_stops_data
    IMPORTING
      !is_shipment TYPE cxshipment
      !is_ship TYPE ts_shipment_merge
    EXPORTING
      !et_stops TYPE tt_stops_srt
    RAISING
      cx_udm_message .
  METHODS is_vtts_changed
    IMPORTING
      !is_shipment TYPE cxshipment
      !is_vtts TYPE vttsvb
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_ctp_shipment_data IMPLEMENTATION.


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


  METHOD get_likp_delta_data.

    DATA: ls_likp_delta   TYPE ts_likpdl.

    CLEAR: et_likp_delta[].

    FIELD-SYMBOLS: <ls_vttp> TYPE vttpvb,
                   <ls_vtsp> TYPE vtspvb,
                   <ls_vtts> TYPE vttsvb.

    LOOP AT is_ship-vttp_dlt ASSIGNING <ls_vttp>
      WHERE updkz = zif_gtt_pof_ef_constants=>cs_change_mode-insert
         OR updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.

      IF NOT line_exists( et_likp_delta[ KEY primary_key
                                         COMPONENTS tknum = <ls_vttp>-tknum
                                                    vbeln = <ls_vttp>-vbeln ] ).
        ls_likp_delta = CORRESPONDING #( <ls_vttp> ).
        INSERT ls_likp_delta INTO TABLE et_likp_delta.
      ENDIF.
    ENDLOOP.

    LOOP AT is_ship-vtsp_dlt ASSIGNING <ls_vtsp>
      WHERE updkz = zif_gtt_pof_ef_constants=>cs_change_mode-insert
         OR updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.
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


  METHOD get_lips_data.

    DATA: ls_lips   TYPE lips.

    CLEAR: et_lips[].

    IF is_ship-likp[] IS NOT INITIAL.
      SELECT * INTO ls_lips
        FROM lips
        FOR ALL ENTRIES IN is_ship-likp
        WHERE vbeln = is_ship-likp-vbeln.

        IF zcl_gtt_pof_dl_tools=>is_appropriate_dl_item( ir_struct = REF #( ls_lips ) ) = abap_true.
          INSERT ls_lips INTO TABLE et_lips.
        ENDIF.
      ENDSELECT.
    ENDIF.

  ENDMETHOD.


  METHOD get_stops.

    rr_stops  = REF #( mt_stops ).

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
          WHERE vbtyp_n = zif_gtt_pof_app_constants=>cs_vbtyp-shipment
            AND vbtyp_v = zif_gtt_pof_app_constants=>cs_vbtyp-delivery.

          SELECT SINGLE *
            INTO ls_vttk
            FROM vttk
            WHERE tknum = <ls_vbfas>-vbeln.

          IF sy-subrc = 0 AND
             zcl_gtt_pof_sh_tools=>is_appropriate_type(
               ir_vttk = REF #( ls_vttk ) ) = abap_true.

            SELECT SINGLE *
              INTO CORRESPONDING FIELDS OF ls_likp
              FROM likp
              WHERE vbeln = <ls_vbfas>-vbelv.

            IF sy-subrc = 0 AND
               zcl_gtt_pof_dl_tools=>is_appropriate_dl_type(
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
      IF <ls_vttp_dlt>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.
        DELETE et_vbfa
          WHERE vbeln = <ls_vttp_dlt>-tknum
            AND vbelv = <ls_vttp_dlt>-vbeln.

        " ADDED
      ELSEIF <ls_vttp_dlt>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-insert AND
             NOT line_exists( et_vbfa[ KEY primary_key
                                       COMPONENTS tknum = <ls_vttp_dlt>-tknum
                                                  vbeln = <ls_vttp_dlt>-tknum
                                                  vbelv = <ls_vttp_dlt>-vbeln ] ).

        SELECT SINGLE *
          INTO CORRESPONDING FIELDS OF ls_likp
          FROM likp
          WHERE vbeln = <ls_vttp_dlt>-vbeln.

        IF sy-subrc = 0 AND
               zcl_gtt_pof_dl_tools=>is_appropriate_dl_type(
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


  METHOD get_vtsp_merged_data.

    FIELD-SYMBOLS: <ls_vtsp>  TYPE vtspvb.
    CLEAR: et_vtsp_delta[].

    et_vtsp_full  = is_shipment-new_vtsp[].

    LOOP AT is_shipment-old_vtsp ASSIGNING <ls_vtsp>
      WHERE updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.

      et_vtsp_full  = VALUE #( BASE et_vtsp_full
                               ( <ls_vtsp> ) ).
    ENDLOOP.

    LOOP AT et_vtsp_full ASSIGNING <ls_vtsp>
      WHERE tknum IN it_tknum
        AND ( updkz = zif_gtt_pof_ef_constants=>cs_change_mode-insert
           OR updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete ).

      et_vtsp_delta   = VALUE #( BASE et_vtsp_delta
                                 ( <ls_vtsp> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_vttk_merged_data.

    CLEAR: et_vttk[].

    " collect all the current shipments
    LOOP AT is_shipment-new_vttk ASSIGNING FIELD-SYMBOL(<ls_vttk_new>).
      IF zcl_gtt_pof_sh_tools=>is_appropriate_type(
           ir_vttk = REF #( <ls_vttk_new> ) ) = abap_true.

        et_vttk   = VALUE #( BASE et_vttk
                             ( <ls_vttk_new> ) ).
      ENDIF.
    ENDLOOP.

    " add deleted shipments
    LOOP AT is_shipment-old_vttk ASSIGNING FIELD-SYMBOL(<ls_vttk_old>)
      WHERE updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.

      IF zcl_gtt_pof_sh_tools=>is_appropriate_type(
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
      WHERE updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.

      et_vttp_full  = VALUE #( BASE et_vttp_full
                               ( <ls_vttp> ) ).
    ENDLOOP.

    LOOP AT et_vttp_full ASSIGNING <ls_vttp>
      WHERE tknum IN it_tknum
        AND ( updkz = zif_gtt_pof_ef_constants=>cs_change_mode-insert
           OR updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete ).

      et_vttp_delta   = VALUE #( BASE et_vttp_delta
                                 ( <ls_vttp> ) ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_vtts_merged_data.

    FIELD-SYMBOLS: <ls_vtts>  TYPE vttsvb.
    CLEAR: et_vtts_delta[].

    et_vtts_full  = is_shipment-new_vtts[].

    LOOP AT is_shipment-old_vtts ASSIGNING <ls_vtts>
      WHERE updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.

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


  METHOD init_stops_data.

    DATA: lt_stops    TYPE zif_gtt_pof_app_types=>tt_stops,
          lt_watching TYPE zif_gtt_pof_app_types=>tt_dlv_watch_stops.

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
        zcl_gtt_pof_sh_tools=>get_stops_from_shipment(
          EXPORTING
            iv_tknum              = <ls_vbfa>-tknum
            it_vtts               = is_shipment-new_vtts
            it_vtsp               = is_shipment-new_vtsp
            it_vttp               = is_shipment-new_vttp
          IMPORTING
            et_stops              = lt_stops
            et_dlv_watching_stops = lt_watching ).

      ELSE.
        zcl_gtt_pof_sh_tools=>get_stops_from_shipment(
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
        MESSAGE e005(zgtt_pof) WITH |{ <ls_vbfa>-tknum }| 'STOPS' INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
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

    IF is_vtts-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-insert OR
       is_vtts-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.
      rv_result   = abap_true.
    ELSEIF ( is_vtts-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-update OR
             is_vtts-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-undefined ).

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
