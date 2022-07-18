CLASS zcl_gtt_pof_sh_tools DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  CLASS-METHODS get_stops_from_shipment
    IMPORTING
      !iv_tknum TYPE tknum
      !it_vtts TYPE vttsvb_tab OPTIONAL
      !it_vtsp TYPE vtspvb_tab OPTIONAL
      !it_vttp TYPE vttpvb_tab OPTIONAL
    EXPORTING
      !et_stops TYPE zif_gtt_pof_app_types=>tt_stops
      !et_dlv_watching_stops TYPE zif_gtt_pof_app_types=>tt_dlv_watch_stops .
  CLASS-METHODS get_carrier_reference_document
    IMPORTING
      !is_vttk TYPE vttkvb
    EXPORTING
      !ev_ref_typ TYPE zif_gtt_pof_app_types=>tv_crdoc_ref_typ
      !ev_ref_val TYPE zif_gtt_pof_app_types=>tv_crdoc_ref_val .
  CLASS-METHODS is_appropriate_type
    IMPORTING
      !ir_vttk TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  CLASS-METHODS is_delivery_assigned
    IMPORTING
      !ir_vttp TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  CLASS-METHODS is_object_modified
    IMPORTING
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
  CLASS-METHODS get_next_event_counter
    RETURNING
      VALUE(rv_evtcnt) TYPE /saptrx/evtcnt .
PROTECTED SECTION.
PRIVATE SECTION.

  CLASS-DATA mv_evtcnt TYPE /saptrx/evtcnt VALUE zif_gtt_pof_app_constants=>cs_start_evtcnt-shipment ##NO_TEXT.
ENDCLASS.



CLASS zcl_gtt_pof_sh_tools IMPLEMENTATION.


  METHOD get_carrier_reference_document.

    IF is_vttk-vsart = '01' AND
       is_vttk-tndr_trkid IS NOT INITIAL.
      ev_ref_typ  = 'BN'.
      ev_ref_val  = is_vttk-tndr_trkid.
    ELSEIF is_vttk-vsart = '04' AND
           is_vttk-tndr_trkid IS NOT INITIAL.
      ev_ref_typ  = 'T50'.
      ev_ref_val  = is_vttk-tndr_trkid.
    ELSEIF ( is_vttk-vsart = '05' OR is_vttk-vsart = '15' ) AND
             is_vttk-tndr_trkid IS NOT INITIAL.
      ev_ref_typ  = 'T55'.
      ev_ref_val  = is_vttk-tndr_trkid.
    ELSE.
      CLEAR: ev_ref_typ, ev_ref_val.
    ENDIF.

  ENDMETHOD.


  METHOD get_next_event_counter.

    ADD 1 TO mv_evtcnt.

    rv_evtcnt = mv_evtcnt.

  ENDMETHOD.


  METHOD get_stops_from_shipment.

    DATA:
      ls_vttsvb            TYPE vttsvb,
      lt_vttsvb            TYPE STANDARD TABLE OF vttsvb,
      ls_vtsp              TYPE vtsp,
      lt_vtsp              TYPE STANDARD TABLE OF vtsp,
      ls_vtspvb            TYPE vtspvb,
      lt_vtspvb            TYPE STANDARD TABLE OF vtspvb,
      ls_vttpvb            TYPE vttpvb,
      lt_vttpvb            TYPE STANDARD TABLE OF vttpvb,
      ls_stop              TYPE zif_gtt_pof_app_types=>ts_stops,
      ls_dlv_watching_stop TYPE zif_gtt_pof_app_types=>ts_dlv_watch_stops,
*       Count
      lv_stopcnt           TYPE int4,
      lv_cnt               TYPE char04,
*       Source & Destination
      lv_desloctype        TYPE zif_gtt_pof_app_types=>tv_loctype,
      lv_deslocid          TYPE zif_gtt_pof_app_types=>tv_locid,
      lv_srcloctype        TYPE zif_gtt_pof_app_types=>tv_loctype,
      lv_srclocid          TYPE zif_gtt_pof_app_types=>tv_locid,
*       Timezone
      lv_tzone             TYPE timezone,
*       Door text
      lv_ltort             TYPE t30bt-ltort,
*       Warehouse text
      lv_lnumt             TYPE t300t-lnumt,
*       Warehouse text / door text
      lv_lgtratxt          TYPE char60.

    DATA: lt_tknum_range TYPE STANDARD TABLE OF range_c10.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE' ##FM_SUBRC_OK
      IMPORTING
        timezone            = lv_tzone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.

*   Read Stage Information
    IF it_vtts IS SUPPLIED.
      MOVE it_vtts TO lt_vttsvb.
    ELSE.
      lt_tknum_range = VALUE #( (
        sign   = 'I'
        option = 'EQ'
        low    = iv_tknum
      ) ).

      CALL FUNCTION 'ST_STAGES_READ'
        EXPORTING
          i_vtts_db_tab = 'VTTS'
          i_vtsp_db_tab = 'VTSP'
        TABLES
          i_tknum_range = lt_tknum_range
          c_xvttsvb     = lt_vttsvb.
    ENDIF.

    SORT lt_vttsvb BY tsrfo.

*   Fill source & destination
    LOOP AT lt_vttsvb INTO ls_vttsvb WHERE tknum = iv_tknum
                                       AND updkz <> 'D'.
      IF ls_vttsvb-kunna IS NOT INITIAL.
        lv_srcloctype = zif_gtt_pof_ef_constants=>cs_loc_types-customer.
        lv_srclocid   = ls_vttsvb-kunna.
      ELSEIF ls_vttsvb-vstel IS NOT INITIAL.
        lv_srcloctype = zif_gtt_pof_ef_constants=>cs_loc_types-shippingpoint.
        lv_srclocid   = ls_vttsvb-vstel.
      ELSEIF ls_vttsvb-lifna IS NOT INITIAL.
        lv_srcloctype = zif_gtt_pof_ef_constants=>cs_loc_types-supplier.
        lv_srclocid   = ls_vttsvb-lifna.
      ELSEIF ls_vttsvb-werka IS NOT INITIAL.
        lv_srcloctype = zif_gtt_pof_ef_constants=>cs_loc_types-plant.
        lv_srclocid   = ls_vttsvb-werka.
      ELSEIF ls_vttsvb-knota IS NOT INITIAL.
        lv_srcloctype = zif_gtt_pof_ef_constants=>cs_loc_types-logisticlocation.
        lv_srclocid   = ls_vttsvb-knota.
      ENDIF.

*     if current stage line's source = last stage line's destination, no change on stop id & stop count
      IF lv_srcloctype NE lv_desloctype OR lv_srclocid NE lv_deslocid.
        lv_stopcnt = lv_stopcnt + 1.
      ENDIF.

      IF ls_vttsvb-kunnz IS NOT INITIAL.
        lv_desloctype = zif_gtt_pof_ef_constants=>cs_loc_types-customer.
        lv_deslocid   = ls_vttsvb-kunnz.
      ELSEIF ls_vttsvb-vstez IS NOT INITIAL.
        lv_desloctype = zif_gtt_pof_ef_constants=>cs_loc_types-shippingpoint.
        lv_deslocid   = ls_vttsvb-vstez.
      ELSEIF ls_vttsvb-lifnz IS NOT INITIAL.
        lv_desloctype = zif_gtt_pof_ef_constants=>cs_loc_types-supplier.
        lv_deslocid   = ls_vttsvb-lifnz.
      ELSEIF ls_vttsvb-werkz IS NOT INITIAL.
        lv_desloctype = zif_gtt_pof_ef_constants=>cs_loc_types-plant.
        lv_deslocid   = ls_vttsvb-werkz.
      ELSEIF ls_vttsvb-knotz IS NOT INITIAL.
        lv_desloctype = zif_gtt_pof_ef_constants=>cs_loc_types-logisticlocation.
        lv_deslocid   = ls_vttsvb-knotz.
      ENDIF.

      lv_cnt = lv_stopcnt.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_cnt
        IMPORTING
          output = lv_cnt.

      CONCATENATE iv_tknum lv_cnt INTO ls_stop-stopid.
      ls_stop-stopcnt = lv_stopcnt.
      ls_stop-loccat  = 'S'.
      ls_stop-loctype = lv_srcloctype.
      ls_stop-locid   = lv_srclocid.
      ls_stop-kunablaz_txt = ls_vttsvb-kunabla.
      ls_stop-lgnumaz = ls_vttsvb-lgnuma.
      ls_stop-toraz   = ls_vttsvb-tora.
      ls_stop-tknum   = iv_tknum.
      ls_stop-tsnum   = ls_vttsvb-tsnum.
      ls_stop-tsrfo   = ls_vttsvb-tsrfo.
      IF ls_vttsvb-dptbg IS INITIAL.
        CLEAR ls_stop-pln_evt_datetime.
      ELSE.
        ls_stop-pln_evt_datetime  = |0{ ls_vttsvb-dptbg }{ ls_vttsvb-uptbg }|.
      ENDIF.
      ls_stop-pln_evt_timezone = lv_tzone.

      CLEAR ls_stop-lstelz_txt.
      SELECT SINGLE vtext INTO ls_stop-lstelz_txt FROM tvlat
                                                  WHERE spras = sy-langu
                                                  AND   vstel = ls_vttsvb-vstel
                                                  AND   lstel = ls_vttsvb-lstel.

      CLEAR ls_stop-lgortaz_txt.
      SELECT SINGLE lgobe INTO ls_stop-lgortaz_txt FROM t001l
                                                   WHERE werks = ls_vttsvb-werka
                                                   AND   lgort = ls_vttsvb-lgorta.

*  Warehouse door text: concatenate T300T-LNUMT '/' T30BT-ltort with LGNUM and LGTOR
      CLEAR lv_ltort.
      SELECT SINGLE ltort INTO lv_ltort FROM t30bt WHERE spras = sy-langu
                                                   AND   lgnum = ls_vttsvb-lgnuma
                                                   AND   lgtor = ls_vttsvb-tora.
      CLEAR lv_lnumt.
      SELECT SINGLE lnumt INTO lv_lnumt FROM t300t WHERE spras = sy-langu
                                                   AND   lgnum = ls_vttsvb-lgnuma.
      CLEAR ls_stop-lgtraz_txt.
      IF lv_ltort IS NOT INITIAL OR lv_lnumt IS NOT INITIAL.
        CONCATENATE lv_lnumt lv_ltort INTO ls_stop-lgtraz_txt SEPARATED BY '/'.
      ENDIF.


      APPEND ls_stop TO et_stops.

      lv_stopcnt = lv_stopcnt + 1.

      lv_cnt = lv_stopcnt.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = lv_cnt
        IMPORTING
          output = lv_cnt.

      CONCATENATE iv_tknum lv_cnt INTO ls_stop-stopid.
      ls_stop-stopcnt = lv_stopcnt.
      ls_stop-loccat  = 'D'.
      ls_stop-loctype = lv_desloctype.
      ls_stop-locid   = lv_deslocid.
      ls_stop-kunablaz_txt = ls_vttsvb-kunablz.
      ls_stop-lgnumaz = ls_vttsvb-lgnumz.
      ls_stop-toraz   = ls_vttsvb-torz.
      ls_stop-tknum   = iv_tknum.
      ls_stop-tsnum   = ls_vttsvb-tsnum.
      ls_stop-tsrfo   = ls_vttsvb-tsrfo.
      IF ls_vttsvb-dpten IS INITIAL.
        CLEAR ls_stop-pln_evt_datetime.
      ELSE.
        ls_stop-pln_evt_datetime  = |0{ ls_vttsvb-dpten }{ ls_vttsvb-upten }|.
      ENDIF.
      ls_stop-pln_evt_timezone = lv_tzone.

      CLEAR ls_stop-lstelz_txt.
      SELECT SINGLE vtext INTO ls_stop-lstelz_txt FROM tvlat
                                                  WHERE spras = sy-langu
                                                  AND   vstel = ls_vttsvb-vstez
                                                  AND   lstel = ls_vttsvb-lstez.

      CLEAR ls_stop-lgortaz_txt.
      SELECT SINGLE lgobe INTO ls_stop-lgortaz_txt FROM t001l
                                                   WHERE werks = ls_vttsvb-werkz
                                                   AND   lgort = ls_vttsvb-lgortz.

*  Warehouse door text: concatenate T300T-LNUMT '/' T30BT-ltort with LGNUM and LGTOR
      CLEAR lv_ltort.
      SELECT SINGLE ltort INTO lv_ltort FROM t30bt WHERE spras = sy-langu
                                                   AND   lgnum = ls_vttsvb-lgnumz
                                                   AND   lgtor = ls_vttsvb-torz.
      CLEAR lv_lnumt.
      SELECT SINGLE lnumt INTO lv_lnumt FROM t300t WHERE spras = sy-langu
                                                   AND   lgnum = ls_vttsvb-lgnumz.
      CLEAR ls_stop-lgtraz_txt.
      IF lv_ltort IS NOT INITIAL OR lv_lnumt IS NOT INITIAL.
        CONCATENATE lv_lnumt lv_ltort INTO ls_stop-lgtraz_txt SEPARATED BY '/'.
      ENDIF.

      APPEND ls_stop TO et_stops.

    ENDLOOP.

    CHECK et_stops IS NOT INITIAL.

*   Read Stage / Item relation Information
    IF it_vtsp IS SUPPLIED.
      MOVE it_vtsp TO lt_vtspvb.
    ELSE.
      lt_tknum_range = VALUE #( (
        sign   = 'I'
        option = 'EQ'
        low    = iv_tknum
      ) ).

      CALL FUNCTION 'ST_STAGES_READ'
        EXPORTING
          i_vtts_db_tab = 'VTTS'
          i_vtsp_db_tab = 'VTSP'
        TABLES
          i_tknum_range = lt_tknum_range
          c_xvtsp       = lt_vtsp
          c_xvtspvb     = lt_vtspvb.
    ENDIF.

    CHECK lt_vtspvb IS NOT INITIAL.

*   Read Item Information
    IF it_vttp IS SUPPLIED.
      MOVE it_vttp TO lt_vttpvb.
    ELSE.
      SELECT * INTO CORRESPONDING FIELDS OF TABLE lt_vttpvb
             FROM vttp
             WHERE tknum = iv_tknum.
    ENDIF.

    LOOP AT lt_vtspvb INTO ls_vtspvb WHERE tknum IS NOT INITIAL
                                       AND tpnum IS NOT INITIAL
                                       AND updkz <> 'D'.
      CLEAR ls_dlv_watching_stop.

      READ TABLE lt_vttpvb INTO ls_vttpvb WITH KEY tknum = ls_vtspvb-tknum
                                                   tpnum = ls_vtspvb-tpnum.
      ls_dlv_watching_stop-vbeln = ls_vttpvb-vbeln.
      LOOP AT et_stops INTO ls_stop WHERE tknum = ls_vtspvb-tknum
                                      AND tsnum = ls_vtspvb-tsnum.
        ls_dlv_watching_stop-stopid = ls_stop-stopid.
        ls_dlv_watching_stop-loccat = ls_stop-loccat.
        APPEND ls_dlv_watching_stop TO et_dlv_watching_stops.
      ENDLOOP.
    ENDLOOP.

    SORT et_dlv_watching_stops BY vbeln stopid loccat.

  ENDMETHOD.


  METHOD is_appropriate_type.

    DATA(lv_shtyp)  = CONV shtyp( zcl_gtt_pof_tools=>get_field_of_structure(
                                    ir_struct_data = ir_vttk
                                    iv_field_name  = 'SHTYP' ) ).

    rv_result   = boolc( lv_shtyp = zif_gtt_pof_app_constants=>cs_relevance-shtyp ).

  ENDMETHOD.


  METHOD is_delivery_assigned.

    TYPES: tt_vttp TYPE STANDARD TABLE OF vttpvb.

    FIELD-SYMBOLS: <lt_vttp> TYPE tt_vttp.

    ASSIGN ir_vttp->* TO <lt_vttp>.

    rv_result = boolc( <lt_vttp> IS ASSIGNED AND
                       <lt_vttp> IS NOT INITIAL ).

  ENDMETHOD.


  METHOD is_object_modified.

    rv_result   = boolc(
      is_events-update_indicator = zif_gtt_pof_ef_constants=>cs_change_mode-insert OR
      is_events-update_indicator = zif_gtt_pof_ef_constants=>cs_change_mode-update OR
      is_events-update_indicator = zif_gtt_pof_ef_constants=>cs_change_mode-undefined
    ).

  ENDMETHOD.
ENDCLASS.
