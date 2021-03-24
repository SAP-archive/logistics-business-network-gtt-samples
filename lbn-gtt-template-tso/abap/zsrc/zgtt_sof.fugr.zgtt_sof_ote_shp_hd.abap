FUNCTION zgtt_sof_ote_shp_hd.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_APPSYS) TYPE  /SAPTRX/APPLSYSTEM
*"     REFERENCE(I_APP_OBJ_TYPES) TYPE  /SAPTRX/AOTYPES
*"     REFERENCE(I_ALL_APPL_TABLES) TYPE  TRXAS_TABCONTAINER
*"     REFERENCE(I_APP_TYPE_CNTL_TABS) TYPE  TRXAS_APPTYPE_TABS
*"     REFERENCE(I_APP_OBJECTS) TYPE  TRXAS_APPOBJ_CTABS
*"  TABLES
*"      E_CONTROL_DATA STRUCTURE  /SAPTRX/CONTROL_DATA
*"      E_LOGTABLE STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      CDATA_DETERMINATION_ERROR
*"      TABLE_DETERMINATION_ERROR
*"      STOP_PROCESSING
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Top Include
* TYPE-POOLS:trxas.
*----------------------------------------------------------------------*

  DATA:
    ls_app_objects            TYPE trxas_appobj_ctab_wa,
    ls_control_data           TYPE /saptrx/control_data,
    lt_xvtts                  TYPE STANDARD TABLE OF vttsvb,
    lt_xvttp                  TYPE STANDARD TABLE OF vttpvb,
    ls_xvtts                  TYPE vttsvb,
    ls_xvttp                  TYPE vttpvb,
    lv_tzone                  TYPE timezone,
    lv_forward_agt            TYPE bu_partner,
    lt_bpdetail               TYPE STANDARD TABLE OF bapibus1006_id_details,
    ls_bpdetail               TYPE bapibus1006_id_details,
    lv_count                  TYPE i,
    lv_tabix                  TYPE numc10,
    lv_full_ind               TYPE char01,
    lv_lgtratxt               TYPE char60,
    lv_time_num               TYPE numc15,
    lv_departure_datetime     TYPE timestamp,
    lv_departure_timezone     TYPE timezone,
    lv_departure_datetime_utc TYPE timestamp,
    lv_arrival_datetime       TYPE timestamp,
    lv_arrival_timezone       TYPE timezone,
    lv_arrival_datetime_utc   TYPE timestamp,
    lt_stops                  TYPE zgtt_stops,
    ls_stop                   TYPE zgtt_stop_info.

  DATA: BEGIN OF ls_stop_id,
          stopid  TYPE zgtt_stopid,
          stopcnt TYPE zgtt_stopcnt,
          loctype TYPE zgtt_loctype,
          locid   TYPE zgtt_locid,
        END OF ls_stop_id.

  DATA: lt_stop_ids LIKE STANDARD TABLE OF ls_stop_id.

  FIELD-SYMBOLS:
    <ls_xvttk> TYPE vttkvb.

* <1> Read necessary application tables from table reference
* Read Item Data New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_item_new
    CHANGING lt_xvttp.

* Read Leg Data New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_leg_new
    CHANGING lt_xvtts.
  SORT lt_xvtts BY tsrfo.

* <2> Fill general data for all control data records
  ls_control_data-appsys     = i_appsys.
  ls_control_data-appobjtype = i_app_obj_types-aotype.
  ls_control_data-language   = sy-langu.

* <3> Loop at application objects for geting Shipment Header
*     add the following values which cannot be extracted in EM Data Extraction
* Shipment number
* Service Agent ERP ID
* Service Agent LBN ID
* Contains DG or not
* Forwarding Agent Tracking ID
* Shipping Type
* TransportatonMode
* Actual Business Time zone
* Actual Business Datetime
* Actual Technical Time zone
* Actual Technical Datetime
* Shipment Item table
* TrackedObject table
* Resource table
* CarrierReferenceDocument table
* Stop table
* Stop ID table (for Visibility Provider)

  LOOP AT i_app_objects INTO ls_app_objects.

*   Application Object ID
    ls_control_data-appobjid   = ls_app_objects-appobjid.

*   Check if Main table is Shipment Header or not.
    IF ls_app_objects-maintabdef <> gc_bpt_shipment_header_new.
      PERFORM create_logtable_aot
        TABLES e_logtable
        USING  space
               ls_app_objects-maintabdef
               i_app_obj_types-controldatafunc
               ls_app_objects-appobjtype
               i_appsys.
      RAISE table_determination_error.
    ELSE.
*     Read Main Object Table
      ASSIGN ls_app_objects-maintabref->* TO <ls_xvttk>.
    ENDIF.

* Shipment number: VTTK-TKNUM
    ls_control_data-paramname = gc_cp_yn_shp_no.
    ls_control_data-value     = <ls_xvttk>-tknum.
    APPEND ls_control_data TO e_control_data.

* Service Agent ERP ID
    ls_control_data-paramname = gc_cp_yn_shp_sa_erp_id.
    ls_control_data-value     = <ls_xvttk>-tdlnr.
    APPEND ls_control_data TO e_control_data.

* Service Agent LBN ID
    CLEAR lv_forward_agt.
    CALL METHOD cl_site_bp_assignment=>select_bp_via_cvi_link
      EXPORTING
        i_lifnr = <ls_xvttk>-tdlnr
      IMPORTING
        e_bp    = lv_forward_agt.
    CLEAR: ls_bpdetail, lt_bpdetail.
    CALL FUNCTION 'BAPI_IDENTIFICATIONDETAILS_GET'
      EXPORTING
        businesspartner      = lv_forward_agt
      TABLES
        identificationdetail = lt_bpdetail.
    READ TABLE lt_bpdetail INTO ls_bpdetail WITH KEY identificationtype = 'LBN001' BINARY SEARCH.
    ls_control_data-paramname = gc_cp_yn_shp_sa_lbn_id.
    "    ls_control_data-value     = ls_bpdetail-identificationnumber.
*   According the GTT adjustment,change the value of 'Service Agent LBN ID'
    IF ls_bpdetail-identificationnumber IS NOT INITIAL.
      CONCATENATE 'LBN#' ls_bpdetail-identificationnumber INTO ls_control_data-value.
    ELSE.
      CLEAR ls_control_data-value.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

* Contains DG or not: VTTK-CONT_DG
    ls_control_data-paramname = gc_cp_yn_shp_contain_dg.
    ls_control_data-value     = <ls_xvttk>-cont_dg.
    APPEND ls_control_data TO e_control_data.

* Forwarding Agent Tracking ID
    ls_control_data-paramname = gc_cp_yn_shp_fa_track_id.
    ls_control_data-value     = <ls_xvttk>-tndr_trkid.
    APPEND ls_control_data TO e_control_data.

* Shipping Type
    ls_control_data-paramname = gc_cp_yn_shp_shipping_type.
    CLEAR lv_count.
    LOOP AT lt_xvttp INTO ls_xvttp WHERE tknum = <ls_xvttk>-tknum.
      lv_count = lv_count + 1.
    ENDLOOP.
    IF lv_count LE 1.
      IF <ls_xvttk>-vsart = '01'.
        ls_control_data-value = '18'.
        APPEND ls_control_data TO e_control_data.
      ELSEIF <ls_xvttk>-vsart = '04'.
        ls_control_data-value = '3'.
        APPEND ls_control_data TO e_control_data.
      ELSE.
        ls_control_data-value = ''.
        APPEND ls_control_data TO e_control_data.
      ENDIF.
    ELSE.
      IF <ls_xvttk>-vsart = '01'.
        ls_control_data-value = '17'.
        APPEND ls_control_data TO e_control_data.
      ELSEIF <ls_xvttk>-vsart = '04'.
        ls_control_data-value = '2'.
        APPEND ls_control_data TO e_control_data.
      ELSE.
        ls_control_data-value = ''.
        APPEND ls_control_data TO e_control_data.
      ENDIF.
    ENDIF.


* TransportatonMode
    IF <ls_xvttk>-vsart = '04'. "Sea
      ls_control_data-paramname = gc_cp_yn_shp_trans_mode.
      ls_control_data-value     = '01'.
    ELSEIF <ls_xvttk>-vsart = '03'. "Rail
      ls_control_data-paramname = gc_cp_yn_shp_trans_mode.
      ls_control_data-value     = '02'.
    ELSEIF <ls_xvttk>-vsart = '01'. "Road
      ls_control_data-paramname = gc_cp_yn_shp_trans_mode.
      ls_control_data-value     = '03'.
    ELSEIF <ls_xvttk>-vsart = '05' OR <ls_xvttk>-vsart = '15'.  "Air
      ls_control_data-paramname = gc_cp_yn_shp_trans_mode.
      ls_control_data-value     = '04'.
    ELSEIF <ls_xvttk>-vsart = '02'. "Mail
      ls_control_data-paramname = gc_cp_yn_shp_trans_mode.
      ls_control_data-value     = '05'.
    ELSEIF <ls_xvttk>-vsart = ''. "Transportation mode not specified
      ls_control_data-paramname = gc_cp_yn_shp_trans_mode.
      ls_control_data-value     = '00'.
    ELSE.   "Transportation mode not applicable
      ls_control_data-paramname = gc_cp_yn_shp_trans_mode.
      ls_control_data-value     = '09'.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

* Actual Business Time zone
    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = lv_tzone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    ls_control_data-paramname = gc_cp_yn_act_timezone.
    ls_control_data-value     = lv_tzone.
    APPEND ls_control_data TO e_control_data.

* Actual Business Datetime
    ls_control_data-paramname = gc_cp_yn_act_datetime.
    CONCATENATE '0' sy-datum sy-uzeit INTO ls_control_data-value.
    APPEND ls_control_data TO e_control_data.

*   Actual Technical Datetime & Time zone
    ls_control_data-paramname = gc_cp_yn_acttec_timezone."ACTUAL_TECHNICAL_TIMEZONE
    ls_control_data-value     = lv_tzone.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname = gc_cp_yn_acttec_datetime."ACTUAL_TECHNICAL_DATETIME
    CONCATENATE '0' sy-datum sy-uzeit INTO ls_control_data-value.
    APPEND ls_control_data TO e_control_data.

* Shipment Item table
    LOOP AT lt_xvttp INTO ls_xvttp WHERE tknum IS NOT INITIAL
                                     AND tpnum IS NOT INITIAL
                                     AND updkz <> 'D'.
      lv_tabix = sy-tabix.
      ls_control_data-paramindex = lv_tabix.
      ls_control_data-paramname = gc_cp_yn_shp_dlv_cnt.
      ls_control_data-value = lv_tabix.
      SHIFT ls_control_data-value LEFT  DELETING LEADING space.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_dlv_no.
      ls_control_data-value = ls_xvttp-vbeln.
      APPEND ls_control_data TO e_control_data.
    ENDLOOP.
    IF sy-subrc NE 0.
      ls_control_data-paramindex = '1'.
      ls_control_data-paramname = gc_cp_yn_shp_dlv_cnt.
      ls_control_data-value = ''.
      APPEND ls_control_data TO e_control_data.
    ENDIF.

* TrackedObject table
    IF ( <ls_xvttk>-vsart = '01' or <ls_xvttk>-vsart = '05' or <ls_xvttk>-vsart = '15' ) AND <ls_xvttk>-exti1 IS NOT INITIAL.
      ls_control_data-paramname = gc_cp_yn_shp_res_id.
      ls_control_data-value     = 'NA'.
      ls_control_data-paramindex = '1'.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_res_value.
      ls_control_data-value     = <ls_xvttk>-exti1.
      ls_control_data-paramindex = '1'.
      APPEND ls_control_data TO e_control_data.
    ELSEIF <ls_xvttk>-vsart = '04' AND <ls_xvttk>-signi IS NOT INITIAL.
      ls_control_data-paramname = gc_cp_yn_shp_res_id.
      ls_control_data-value     = 'CONTAINER_ID'.
      ls_control_data-paramindex = '1'.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_res_value.
      ls_control_data-value     = <ls_xvttk>-signi.
      ls_control_data-paramindex = '1'.
      APPEND ls_control_data TO e_control_data.
    ELSE.
      ls_control_data-paramname = gc_cp_yn_shp_res_value.
      ls_control_data-value     = ''.
      ls_control_data-paramindex = '1'.
      APPEND ls_control_data TO e_control_data.
    ENDIF.

* Resource table
    IF ( <ls_xvttk>-vsart = '01' or <ls_xvttk>-vsart = '05' or <ls_xvttk>-vsart = '15' ) AND <ls_xvttk>-exti1 IS NOT INITIAL.
      ls_control_data-paramindex = '1'.
      ls_control_data-paramname = gc_cp_yn_shp_res_tp_cnt.
      ls_control_data-value = '1'.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_res_tp_id.
      CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-exti1 INTO ls_control_data-value.
      ls_control_data-paramindex = '1'.
      APPEND ls_control_data TO e_control_data.
    ELSEIF <ls_xvttk>-vsart = '04' AND <ls_xvttk>-signi IS NOT INITIAL.
      ls_control_data-paramindex = '1'.
      ls_control_data-paramname = gc_cp_yn_shp_res_tp_cnt.
      ls_control_data-value = '1'.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_res_tp_id.
      CONCATENATE <ls_xvttk>-tknum <ls_xvttk>-signi INTO ls_control_data-value.
      ls_control_data-paramindex = '1'.
      APPEND ls_control_data TO e_control_data.
    ELSE.
      ls_control_data-paramindex = '1'.
      ls_control_data-paramname = gc_cp_yn_shp_res_tp_cnt.
      ls_control_data-value = ''.
      APPEND ls_control_data TO e_control_data.
    ENDIF.

* CarrierReferenceDocument table
*Document Reference Type：
*T50 - Bill of Lading
*T51 - House Bill of Lading
*T52 - Master Bill of Lading
*T53 - Air Waybill
*T54 - House Air Waybill
*T55 - Master Air Waybill
*T67 - Reference number of Carrier
*CUSTR - Customer Reference
*BN - Consignment identifier, carrier assigned
    IF <ls_xvttk>-vsart = '01' AND <ls_xvttk>-tndr_trkid IS NOT INITIAL.
      ls_control_data-paramname = gc_cp_yn_shp_carrier_ref_type.
      ls_control_data-paramindex = '1'.
      ls_control_data-value = 'BN'.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_carrier_ref_value.
      ls_control_data-paramindex = '1'.
      ls_control_data-value = <ls_xvttk>-tndr_trkid.
      APPEND ls_control_data TO e_control_data.
    ELSEIF <ls_xvttk>-vsart = '04' AND <ls_xvttk>-tndr_trkid IS NOT INITIAL.
      ls_control_data-paramname = gc_cp_yn_shp_carrier_ref_type.
      ls_control_data-paramindex = '1'.
      ls_control_data-value = 'T50'.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_carrier_ref_value.
      ls_control_data-paramindex = '1'.
      ls_control_data-value = <ls_xvttk>-tndr_trkid.
      APPEND ls_control_data TO e_control_data.
    ELSEIF ( <ls_xvttk>-vsart = '05' or <ls_xvttk>-vsart = '15' ) AND <ls_xvttk>-tndr_trkid IS NOT INITIAL.
      ls_control_data-paramname = gc_cp_yn_shp_carrier_ref_type.
      ls_control_data-paramindex = '1'.
      ls_control_data-value = 'T55'. "T55 - Master Air Waybill
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_carrier_ref_value.
      ls_control_data-paramindex = '1'.
      ls_control_data-value = <ls_xvttk>-tndr_trkid.
      APPEND ls_control_data TO e_control_data.
    ELSE.
      ls_control_data-paramname = gc_cp_yn_shp_carrier_ref_value.
      ls_control_data-paramindex = '1'.
      ls_control_data-value = ''.
      APPEND ls_control_data TO e_control_data.
    ENDIF.

* Stop table
    CALL FUNCTION 'ZGTT_GET_STOPS_FROM_SHIPMENT'
      EXPORTING
        iv_tknum    = <ls_xvttk>-tknum
        it_vtts_new = lt_xvtts
      IMPORTING
        et_stops    = lt_stops.


    CLEAR lv_count.
    LOOP AT lt_stops INTO ls_stop.
      lv_count = lv_count + 1.

      ls_control_data-paramindex = sy-tabix.
      ls_control_data-paramname = gc_cp_yn_shp_stops_line_cnt.
      ls_control_data-value = lv_count.
      SHIFT ls_control_data-value LEFT  DELETING LEADING space.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stops_stop_id.
      ls_control_data-value = ls_stop-stopid.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stops_ordinal_no.
      ls_control_data-value = ls_stop-stopcnt.
      SHIFT ls_control_data-value LEFT  DELETING LEADING space.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stops_loc_cat.
      ls_control_data-value = ls_stop-loccat.
      APPEND ls_control_data TO e_control_data.     .

      ls_control_data-paramname = gc_cp_yn_shp_stops_loc_type.
      ls_control_data-value = ls_stop-loctype.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stops_loc_id.
      ls_control_data-value = ls_stop-locid.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stops_load_pnt.
      ls_control_data-value = ls_stop-lstelz_txt.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stops_unload_pnt.
      ls_control_data-value = ls_stop-kunablaz_txt.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stops_lgort.
      ls_control_data-value = ls_stop-lgortaz_txt.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_warehouse_no.
      ls_control_data-value = ls_stop-lgnumaz.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_gate_no.
      ls_control_data-value = ls_stop-toraz.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_gate_txt.
      ls_control_data-value = ls_stop-lgtraz_txt.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stage_seq.
      ls_control_data-value = ls_stop-tsrfo.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stops_pln_evt_dt.
      IF ls_stop-pln_evt_datetime IS NOT INITIAL.
        ls_control_data-value = ls_stop-pln_evt_datetime.
      ELSE.
        CLEAR ls_control_data-value.
      ENDIF.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stops_pln_evt_tz.
      ls_control_data-value = ls_stop-pln_evt_timezone.
      APPEND ls_control_data TO e_control_data.
    ENDLOOP.
    IF lt_stops IS INITIAL.
      ls_control_data-paramindex = 1.
      ls_control_data-paramname = gc_cp_yn_shp_stops_line_cnt.
      ls_control_data-value = ''.
      APPEND ls_control_data TO e_control_data.
    ENDIF.

    SORT lt_stops BY stopid.
    MOVE-CORRESPONDING lt_stops TO lt_stop_ids.
    DELETE ADJACENT DUPLICATES FROM lt_stop_ids.
    LOOP AT lt_stop_ids INTO ls_stop_id.
      ls_control_data-paramindex = sy-tabix.

      ls_control_data-paramname = gc_cp_yn_shp_stop_id.
      ls_control_data-value = ls_stop_id-stopid.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stop_id_ord_no.
      ls_control_data-value = ls_stop_id-stopcnt.
      SHIFT ls_control_data-value LEFT  DELETING LEADING space.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stop_id_loc_type.
      ls_control_data-value = ls_stop_id-loctype.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_shp_stop_id_loc_id.
      ls_control_data-value = ls_stop_id-locid.
      APPEND ls_control_data TO e_control_data.

    ENDLOOP.
    IF lt_stop_ids IS INITIAL.
      ls_control_data-paramindex = 1.
      ls_control_data-paramname = gc_cp_yn_shp_stop_id.
      ls_control_data-value = ''.
      APPEND ls_control_data TO e_control_data.
    ENDIF.

    CLEAR ls_control_data-paramindex.

*Planned Departure Location ID for 1st stop
*Planned Departure Location Type for 1st stop
*Planned Departure Business Datetime for 1st stop
*Planned Departure Business Datetime Timezone for 1st stop
*Planned Arrival Location ID for last stop
*Planned Arrival Location Type for last stop
*Planned Arrival Business Datetime for last stop
*Planned Arrival  Datetime zone for last stop

    CLEAR: lv_time_num.
    READ TABLE lt_stops INTO ls_stop INDEX 1.
    lv_departure_datetime = ls_stop-pln_evt_datetime.
    lv_departure_timezone = ls_stop-pln_evt_timezone.

    CALL FUNCTION '/SAPAPO/TIMESTAMP_ZONE_TO_UTC'
      EXPORTING
        iv_timezone  = lv_departure_timezone
        iv_timestamp = lv_departure_datetime
      IMPORTING
        ev_timestamp = lv_departure_datetime_utc.

    MOVE lv_departure_datetime_utc TO lv_time_num.

    ls_control_data-paramname = gc_cp_yn_shp_departure_dt.
    IF lv_time_num IS NOT INITIAL.
      ls_control_data-value = lv_time_num.
    ELSE.
      CLEAR ls_control_data-value.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname = gc_cp_yn_shp_departure_tz.
    ls_control_data-value = lv_departure_timezone.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname = gc_cp_yn_shp_departure_locid.
    ls_control_data-value = ls_stop-locid.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname = gc_cp_yn_shp_departure_loctype.
    ls_control_data-value = ls_stop-loctype.
    APPEND ls_control_data TO e_control_data.

    CLEAR: lv_count, lv_time_num.
    DESCRIBE TABLE lt_stops LINES lv_count.
    READ TABLE lt_stops INTO ls_stop INDEX lv_count.
    lv_arrival_datetime = ls_stop-pln_evt_datetime.
    lv_arrival_timezone = ls_stop-pln_evt_timezone.

    CALL FUNCTION '/SAPAPO/TIMESTAMP_ZONE_TO_UTC'
      EXPORTING
        iv_timezone  = lv_arrival_timezone
        iv_timestamp = lv_arrival_datetime
      IMPORTING
        ev_timestamp = lv_arrival_datetime_utc.

    MOVE lv_arrival_datetime_utc TO lv_time_num.

    ls_control_data-paramname = gc_cp_yn_shp_arrival_dt.
    IF lv_time_num IS NOT INITIAL.
      ls_control_data-value = lv_time_num.
    ELSE.
      CLEAR ls_control_data-value.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname = gc_cp_yn_shp_arrival_tz.
    ls_control_data-value = lv_departure_timezone.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname = gc_cp_yn_shp_arrival_locid.
    ls_control_data-value = ls_stop-locid.
    APPEND ls_control_data TO e_control_data.

    ls_control_data-paramname = gc_cp_yn_shp_arrival_loctype.
    ls_control_data-value = ls_stop-loctype.
    APPEND ls_control_data TO e_control_data.


  ENDLOOP.
ENDFUNCTION.
