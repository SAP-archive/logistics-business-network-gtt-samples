FUNCTION zgtt_get_stops_from_shipment.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_TKNUM) TYPE  TKNUM
*"     REFERENCE(IT_VTTS_NEW) TYPE  VTTSVB_TAB OPTIONAL
*"     REFERENCE(IT_VTSP_NEW) TYPE  VTSPVB_TAB OPTIONAL
*"     REFERENCE(IT_VTTP_NEW) TYPE  VTTPVB_TAB OPTIONAL
*"  EXPORTING
*"     REFERENCE(ET_STOPS) TYPE  ZGTT_STOPS
*"     REFERENCE(ET_DLV_WATCHING_STOPS) TYPE  ZGTT_DLV_WATCH_STOPS
*"----------------------------------------------------------------------
  DATA:
    ls_vttsvb            TYPE vttsvb,
    lt_vttsvb            TYPE STANDARD TABLE OF vttsvb,
    ls_vtsp              TYPE vtsp,
    lt_vtsp              TYPE STANDARD TABLE OF vtsp,
    ls_vtspvb            TYPE vtspvb,
    lt_vtspvb            TYPE STANDARD TABLE OF vtspvb,
    ls_vttpvb            TYPE vttpvb,
    lt_vttpvb            TYPE STANDARD TABLE OF vttpvb,
    ls_stop              TYPE zgtt_stop_info,
    ls_dlv_watching_stop TYPE zgtt_dlv_watch_stop,
*     Count
    lv_stopcnt           TYPE int4,
    lv_cnt               TYPE char04,
*     Source & Destination
    lv_desloctype        TYPE zgtt_loctype,
    lv_deslocid          TYPE zgtt_locid,
    lv_srcloctype        TYPE zgtt_loctype,
    lv_srclocid          TYPE zgtt_locid,
*     Timezone
    lv_tzone             TYPE timezone,
*     Door text
    lv_ltort             TYPE t30bt-ltort,
*     Warehouse text
    lv_lnumt             TYPE t300t-lnumt,
*     Warehouse text / door text
    lv_lgtratxt          TYPE char60.

  RANGES:
      ls_tknum_range FOR vttk-tknum.

  CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
    IMPORTING
      timezone            = lv_tzone
    EXCEPTIONS
      customizing_missing = 1
      OTHERS              = 2.

* Read Stage Information
  IF it_vtts_new IS SUPPLIED.
    MOVE it_vtts_new TO lt_vttsvb.
  ELSE.
    ls_tknum_range-sign   = 'I'.
    ls_tknum_range-option = 'EQ'.
    ls_tknum_range-low    = iv_tknum.
    APPEND ls_tknum_range.
    CALL FUNCTION 'ST_STAGES_READ'
      EXPORTING
        i_vtts_db_tab = 'VTTS'
        i_vtsp_db_tab = 'VTSP'
      TABLES
        i_tknum_range = ls_tknum_range
        c_xvttsvb     = lt_vttsvb.
  ENDIF.

  SORT lt_vttsvb BY tsrfo.

* Fill source & destination
  LOOP AT lt_vttsvb INTO ls_vttsvb WHERE tknum = iv_tknum
                                     AND updkz <> 'D'.    .
    IF ls_vttsvb-kunna IS NOT INITIAL.
      lv_srcloctype = 'Customer'.
      lv_srclocid   = ls_vttsvb-kunna.
    ELSEIF ls_vttsvb-vstel IS NOT INITIAL.
      lv_srcloctype = 'ShippingPoint'.
      lv_srclocid   = ls_vttsvb-vstel.
    ELSEIF ls_vttsvb-lifna IS NOT INITIAL.
      lv_srcloctype = 'Supplier'.
      lv_srclocid   = ls_vttsvb-lifna.
    ELSEIF ls_vttsvb-werka IS NOT INITIAL.
      lv_srcloctype = 'Plant'.
      lv_srclocid   = ls_vttsvb-werka.
    ELSEIF ls_vttsvb-knota IS NOT INITIAL.
      lv_srcloctype = 'LogisticLocation'.
      lv_srclocid   = ls_vttsvb-knota.
    ENDIF.

*   if current stage line's source = last stage line's destination, no change on stop id & stop count
    IF lv_srcloctype NE lv_desloctype OR lv_srclocid NE lv_deslocid.
      lv_stopcnt = lv_stopcnt + 1.
    ENDIF.

    IF ls_vttsvb-kunnz IS NOT INITIAL.
      lv_desloctype = 'Customer'.
      lv_deslocid   = ls_vttsvb-kunnz.
    ELSEIF ls_vttsvb-vstez IS NOT INITIAL.
      lv_desloctype = 'ShippingPoint'.
      lv_deslocid   = ls_vttsvb-vstez.
    ELSEIF ls_vttsvb-lifnz IS NOT INITIAL.
      lv_desloctype = 'Supplier'.
      lv_deslocid   = ls_vttsvb-lifnz.
    ELSEIF ls_vttsvb-werkz IS NOT INITIAL.
      lv_desloctype = 'Plant'.
      lv_deslocid   = ls_vttsvb-werkz.
    ELSEIF ls_vttsvb-knotz IS NOT INITIAL.
      lv_desloctype = 'LogisticLocation'.
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
      CONCATENATE '0' ls_vttsvb-dptbg ls_vttsvb-uptbg INTO ls_stop-pln_evt_datetime.
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

*Warehouse door text: concatenate T300T-LNUMT '/' T30BT-ltort with LGNUM and LGTOR
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
      CONCATENATE '0' ls_vttsvb-dpten ls_vttsvb-upten INTO ls_stop-pln_evt_datetime.
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

*Warehouse door text: concatenate T300T-LNUMT '/' T30BT-ltort with LGNUM and LGTOR
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

* Read Stage / Item relation Information
  IF it_vtsp_new IS SUPPLIED.
    MOVE it_vtsp_new TO lt_vtspvb.
  ELSE.
    CLEAR ls_tknum_range.
    ls_tknum_range-sign   = 'I'.
    ls_tknum_range-option = 'EQ'.
    ls_tknum_range-low    = iv_tknum.
    APPEND ls_tknum_range.
    CALL FUNCTION 'ST_STAGES_READ'
      EXPORTING
        i_vtts_db_tab = 'VTTS'
        i_vtsp_db_tab = 'VTSP'
      TABLES
        i_tknum_range = ls_tknum_range
        c_xvtsp       = lt_vtsp
        c_xvtspvb     = lt_vtspvb.
  ENDIF.

  CHECK lt_vtspvb IS NOT INITIAL.

* Read Item Information
  IF it_vttp_new IS SUPPLIED.
    MOVE it_vttp_new TO lt_vttpvb.
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


ENDFUNCTION.
