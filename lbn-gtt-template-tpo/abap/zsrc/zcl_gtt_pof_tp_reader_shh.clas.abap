CLASS zcl_gtt_pof_tp_reader_shh DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_tp_reader .

  METHODS constructor
    IMPORTING
      !io_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters .
PROTECTED SECTION.
PRIVATE SECTION.

  TYPES:
    tt_trobj_res_id  TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_trobj_res_id WITH EMPTY KEY .
  TYPES:
    tt_trobj_res_val TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_trobj_res_val WITH EMPTY KEY .
  TYPES:
    tt_resrc_cnt     TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_resrc_cnt WITH EMPTY KEY .
  TYPES:
    tt_resrc_tp_id   TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_resrc_tp_id WITH EMPTY KEY .
  TYPES:
    tt_crdoc_ref_typ TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_crdoc_ref_typ WITH EMPTY KEY .
  TYPES:
    tt_crdoc_ref_val TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_crdoc_ref_val WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_sh_header,
             tknum              TYPE vttkvb-tknum,
             bu_id_num          TYPE /saptrx/paramval200,
             cont_dg            TYPE vttkvb-cont_dg,
             tndr_trkid         TYPE vttkvb-tndr_trkid,
             ship_type          TYPE zif_gtt_pof_app_types=>tv_ship_type,
             trans_mode         TYPE zif_gtt_pof_app_types=>tv_trans_mode,
             departure_dt       TYPE zif_gtt_pof_app_types=>tv_departure_dt,
             departure_tz       TYPE zif_gtt_pof_app_types=>tv_departure_tz,
             departure_locid    TYPE zif_gtt_pof_app_types=>tv_locid,
             departure_loctype  TYPE zif_gtt_pof_app_types=>tv_loctype,
             arrival_dt         TYPE zif_gtt_pof_app_types=>tv_arrival_dt,
             arrival_tz         TYPE zif_gtt_pof_app_types=>tv_arrival_tz,
             arrival_locid      TYPE zif_gtt_pof_app_types=>tv_locid,
             arrival_loctype    TYPE zif_gtt_pof_app_types=>tv_loctype,
             tdlnr              TYPE vttkvb-tdlnr,
             deliv_cnt          TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_deliv_cnt WITH EMPTY KEY,
             deliv_no           TYPE STANDARD TABLE OF vbeln WITH EMPTY KEY,
             trobj_res_id       TYPE tt_trobj_res_id,
             trobj_res_val      TYPE tt_trobj_res_val,
             resrc_cnt          TYPE tt_resrc_cnt,
             resrc_tp_id        TYPE tt_resrc_tp_id,
             crdoc_ref_typ      TYPE tt_crdoc_ref_typ,
             crdoc_ref_val      TYPE tt_crdoc_ref_val,
             stops_num          TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_stopnum WITH EMPTY KEY,
             stops_stopid       TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_stopid WITH EMPTY KEY,
             stops_stopcnt      TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_stopcnt WITH EMPTY KEY,
             stops_loccat       TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_loccat  WITH EMPTY KEY,
             stops_loctype      TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_loctype WITH EMPTY KEY,
             stops_locid        TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_locid WITH EMPTY KEY,
             stops_lstelz_txt   TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_lstelz_txt WITH EMPTY KEY,
             stops_kunablaz_txt TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_kunablaz_txt WITH EMPTY KEY,
             stops_lgortaz_txt  TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_lgortaz_txt WITH EMPTY KEY,
             stops_lgnumaz      TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_lgnumaz WITH EMPTY KEY,
             stops_toraz        TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_toraz WITH EMPTY KEY,
             stops_lgtraz_txt   TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_lgtraz_txt WITH EMPTY KEY,
             stops_tsrfo        TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_tsrfo WITH EMPTY KEY,
             stops_pln_evt_dt   TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_pln_evt_datetime WITH EMPTY KEY,
             stops_pln_evt_tz   TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_pln_evt_timezone WITH EMPTY KEY,
             stpid_stopid       TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_stopid WITH EMPTY KEY,
             stpid_stopcnt      TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_stopcnt WITH EMPTY KEY,
             stpid_loctype      TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_loctype WITH EMPTY KEY,
             stpid_locid        TYPE STANDARD TABLE OF zif_gtt_pof_app_types=>tv_locid WITH EMPTY KEY,
           END OF ts_sh_header .

  DATA mo_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters .
  CONSTANTS:
    BEGIN OF cs_mapping,
                 tknum              TYPE /saptrx/paramname VALUE 'YN_SHP_NO',
                 bu_id_num          TYPE /saptrx/paramname VALUE 'YN_SHP_SA_LBN_ID',
                 cont_dg            TYPE /saptrx/paramname VALUE 'YN_SHP_CONTAIN_DGOODS',
                 tndr_trkid         TYPE /saptrx/paramname VALUE 'YN_SHP_FA_TRACKING_ID',
                 ship_type          TYPE /saptrx/paramname VALUE 'YN_SHP_SHIPPING_TYPE',
                 trans_mode         TYPE /saptrx/paramname VALUE 'YN_SHP_TRANSPORTATION_MODE',
                 departure_dt       TYPE /saptrx/paramname VALUE 'YN_SHP_PLN_DEP_BUS_DATETIME',
                 departure_tz       TYPE /saptrx/paramname VALUE 'YN_SHP_PLN_DEP_BUS_TIMEZONE',
                 departure_locid    TYPE /saptrx/paramname VALUE 'YN_SHP_PLN_DEP_LOC_ID',
                 departure_loctype  TYPE /saptrx/paramname VALUE 'YN_SHP_PLN_DEP_LOC_TYPE',
                 arrival_dt         TYPE /saptrx/paramname VALUE 'YN_SHP_PLN_AR_BUS_DATETIME',
                 arrival_tz         TYPE /saptrx/paramname VALUE 'YN_SHP_PLN_AR_BUS_TIMEZONE',
                 arrival_locid      TYPE /saptrx/paramname VALUE 'YN_SHP_PLN_AR_LOC_ID',
                 arrival_loctype    TYPE /saptrx/paramname VALUE 'YN_SHP_PLN_AR_LOC_TYPE',
                 tdlnr              TYPE /saptrx/paramname VALUE 'YN_SHP_SA_ERP_ID',
                 deliv_cnt          TYPE /saptrx/paramname VALUE 'YN_SHP_HDR_DLV_LINE_COUNT',
                 deliv_no           TYPE /saptrx/paramname VALUE 'YN_SHP_HDR_DLV_NO',
                 trobj_res_id       TYPE /saptrx/paramname VALUE 'YN_SHP_TRACKED_RESOURCE_ID',
                 trobj_res_val      TYPE /saptrx/paramname VALUE 'YN_SHP_TRACKED_RESOURCE_VALUE',
                 resrc_cnt          TYPE /saptrx/paramname VALUE 'YN_SHP_RESOURCE_TP_LINE_COUNT',
                 resrc_tp_id        TYPE /saptrx/paramname VALUE 'YN_SHP_RESOURCE_TP_ID',
                 crdoc_ref_typ      TYPE /saptrx/paramname VALUE 'YN_SHP_CARRIER_REF_TYPE',
                 crdoc_ref_val      TYPE /saptrx/paramname VALUE 'YN_SHP_CARRIER_REF_VALUE',
                 stops_num          TYPE /saptrx/paramname VALUE 'YN_SHP_LINE_COUNT',
                 stops_stopid       TYPE /saptrx/paramname VALUE 'YN_SHP_STOP_ID',
                 stops_stopcnt      TYPE /saptrx/paramname VALUE 'YN_SHP_ORDINAL_NO',
                 stops_loccat       TYPE /saptrx/paramname VALUE 'YN_SHP_LOC_CATEGORY',
                 stops_loctype      TYPE /saptrx/paramname VALUE 'YN_SHP_LOC_TYPE',
                 stops_locid        TYPE /saptrx/paramname VALUE 'YN_SHP_LOC_ID',
                 stops_lstelz_txt   TYPE /saptrx/paramname VALUE 'YN_SHP_LOADING_POINT',
                 stops_kunablaz_txt TYPE /saptrx/paramname VALUE 'YN_SHP_UNLOADING_POINT',
                 stops_lgortaz_txt  TYPE /saptrx/paramname VALUE 'YN_SHP_STORAGE_LOCATION',
                 stops_lgnumaz      TYPE /saptrx/paramname VALUE 'YN_SHP_WAREHOUSE_NO',
                 stops_toraz        TYPE /saptrx/paramname VALUE 'YN_SHP_GATE_NO',
                 stops_lgtraz_txt   TYPE /saptrx/paramname VALUE 'YN_SHP_GATE_TEXT',
                 stops_tsrfo        TYPE /saptrx/paramname VALUE 'YN_SHP_STAGE_SEQUENCE',
                 stops_pln_evt_dt   TYPE /saptrx/paramname VALUE 'YN_SHP_STOP_PLAN_DATETIME',
                 stops_pln_evt_tz   TYPE /saptrx/paramname VALUE 'YN_SHP_STOP_PLAN_TIMEZONE',
                 stpid_stopid       TYPE /saptrx/paramname VALUE 'YN_SHP_VP_STOP_ID',
                 stpid_stopcnt      TYPE /saptrx/paramname VALUE 'YN_SHP_VP_STOP_ORD_NO',
                 stpid_loctype      TYPE /saptrx/paramname VALUE 'YN_SHP_VP_STOP_LOC_TYPE',
                 stpid_locid        TYPE /saptrx/paramname VALUE 'YN_SHP_VP_STOP_LOC_ID',
               END OF cs_mapping .

  METHODS fill_header_from_vttk
    IMPORTING
      !ir_vttk TYPE REF TO data
      !iv_vttp_cnt TYPE i
    CHANGING
      !cs_header TYPE ts_sh_header
    RAISING
      cx_udm_message .
  METHODS fill_header_from_vttp
    IMPORTING
      !ir_vttk TYPE REF TO data
      !ir_vttp TYPE REF TO data
    CHANGING
      !cs_header TYPE ts_sh_header
    RAISING
      cx_udm_message .
  METHODS fill_header_from_vtts
    IMPORTING
      !ir_vttk TYPE REF TO data
      !ir_vttp TYPE REF TO data
      !ir_vtts TYPE REF TO data
      !ir_vtsp TYPE REF TO data OPTIONAL
    CHANGING
      !cs_header TYPE ts_sh_header
    RAISING
      cx_udm_message .
  METHODS fill_tracked_object_tables
    IMPORTING
      !is_vttk TYPE vttkvb
    EXPORTING
      !et_res_id TYPE tt_trobj_res_id
      !et_res_val TYPE tt_trobj_res_val .
  METHODS fill_resource_tables
    IMPORTING
      !is_vttk TYPE vttkvb
    EXPORTING
      !et_ref_cnt TYPE tt_resrc_cnt
      !et_tp_id TYPE tt_resrc_tp_id .
  METHODS fill_carrier_ref_doc_tables
    IMPORTING
      !is_vttk TYPE vttkvb
    EXPORTING
      !et_ref_typ TYPE tt_crdoc_ref_typ
      !et_ref_val TYPE tt_crdoc_ref_val .
  METHODS get_forwarding_agent_id_number
    IMPORTING
      !iv_tdlnr TYPE tdlnr
    RETURNING
      VALUE(rv_id_num) TYPE /saptrx/paramval200 .
  METHODS get_resource_tracking_id
    IMPORTING
      !is_vttk TYPE vttkvb
    RETURNING
      VALUE(rv_tracking_id) TYPE /saptrx/trxid .
  METHODS get_shippment_header
    IMPORTING
      !is_app_object TYPE trxas_appobj_ctab_wa
      !ir_vttk TYPE REF TO data
    RETURNING
      VALUE(rr_vttk) TYPE REF TO data
    RAISING
      cx_udm_message .
  METHODS get_shippment_item_count
    IMPORTING
      !ir_vttp TYPE REF TO data
    RETURNING
      VALUE(rv_count) TYPE i
    RAISING
      cx_udm_message .
  METHODS get_shippment_type
    IMPORTING
      !iv_vsart TYPE clike
      !iv_vttp_cnt TYPE i
    RETURNING
      VALUE(rv_ship_type) TYPE zif_gtt_pof_app_types=>tv_ship_type .
  METHODS get_transportation_mode
    IMPORTING
      !iv_vsart TYPE clike
    RETURNING
      VALUE(rv_trans_mode) TYPE zif_gtt_pof_app_types=>tv_trans_mode .
  METHODS is_object_changed
    IMPORTING
      !is_app_object TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_tp_reader_shh IMPLEMENTATION.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.

  ENDMETHOD.


  METHOD fill_carrier_ref_doc_tables.

    zcl_gtt_pof_sh_tools=>get_carrier_reference_document(
      EXPORTING
        is_vttk    = is_vttk
      IMPORTING
        ev_ref_typ = DATA(lv_ref_typ)
        ev_ref_val = DATA(lv_ref_val) ).

    IF lv_ref_typ IS NOT INITIAL AND lv_ref_val IS NOT INITIAL.
      et_ref_typ  = VALUE #( ( lv_ref_typ ) ).
      et_ref_val  = VALUE #( ( lv_ref_val ) ).
    ELSE.
      CLEAR: et_ref_typ[], et_ref_val[].
    ENDIF.

  ENDMETHOD.


  METHOD fill_header_from_vttk.

    FIELD-SYMBOLS: <ls_vttk> TYPE vttkvb.

    ASSIGN ir_vttk->* TO <ls_vttk>.
    IF sy-subrc = 0.
      cs_header-tknum       = <ls_vttk>-tknum.
      cs_header-bu_id_num   = get_forwarding_agent_id_number(
                                iv_tdlnr = <ls_vttk>-tdlnr ).
      cs_header-cont_dg     = <ls_vttk>-cont_dg.
      cs_header-tndr_trkid  = <ls_vttk>-tndr_trkid.
      cs_header-ship_type   = get_shippment_type(
                                iv_vsart    = <ls_vttk>-vsart
                                iv_vttp_cnt = iv_vttp_cnt ).
      cs_header-trans_mode  = get_transportation_mode(
                                iv_vsart = <ls_vttk>-vsart ) .
      cs_header-tdlnr       = <ls_vttk>-tdlnr.

      fill_tracked_object_tables(
        EXPORTING
          is_vttk    = <ls_vttk>
        IMPORTING
          et_res_id  = cs_header-trobj_res_id
          et_res_val = cs_header-trobj_res_val ).

      fill_resource_tables(
        EXPORTING
          is_vttk    = <ls_vttk>
        IMPORTING
          et_ref_cnt = cs_header-resrc_cnt
          et_tp_id   = cs_header-resrc_tp_id ).

      fill_carrier_ref_doc_tables(
        EXPORTING
          is_vttk    = <ls_vttk>
        IMPORTING
          et_ref_typ = cs_header-crdoc_ref_typ
          et_ref_val = cs_header-crdoc_ref_val ).
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTK' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_header_from_vttp.

    TYPES: tt_vttp  TYPE STANDARD TABLE OF vttpvb.
    DATA: lv_count TYPE i VALUE 0.

    FIELD-SYMBOLS: <ls_vttk> TYPE vttkvb,
                   <lt_vttp> TYPE tt_vttp.

    ASSIGN ir_vttk->* TO <ls_vttk>.
    ASSIGN ir_vttp->* TO <lt_vttp>.

    IF sy-subrc = 0.
      LOOP AT <lt_vttp> ASSIGNING FIELD-SYMBOL(<ls_vttp>)
        WHERE tknum = <ls_vttk>-tknum.

        ADD 1 TO lv_count.
        APPEND lv_count TO cs_header-deliv_cnt.

        APPEND <ls_vttp>-vbeln TO cs_header-deliv_no.
      ENDLOOP.
    ELSEIF <ls_vttk> IS NOT ASSIGNED.
      MESSAGE e002(zgtt_pof) WITH 'VTTK' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTP' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_header_from_vtts.

    TYPES: BEGIN OF ts_stop_id,
             stopid  TYPE zif_gtt_pof_app_types=>tv_stopid,
             stopcnt TYPE zif_gtt_pof_app_types=>tv_stopcnt,
             loctype TYPE zif_gtt_pof_app_types=>tv_loctype,
             locid   TYPE zif_gtt_pof_app_types=>tv_locid,
           END OF ts_stop_id.

    DATA(lv_tknum) = CONV tknum( zcl_gtt_pof_tools=>get_field_of_structure(
                                   ir_struct_data = ir_vttk
                                   iv_field_name  = 'TKNUM' ) ).

    DATA: lt_vtts     TYPE vttsvb_tab,
          lt_stops    TYPE zif_gtt_pof_app_types=>tt_stops,
          lt_stop_ids TYPE STANDARD TABLE OF ts_stop_id,
          lv_datetime TYPE zif_gtt_pof_app_types=>tv_pln_evt_datetime,
          lv_count    TYPE i.

    FIELD-SYMBOLS: <lt_vttp> TYPE vttpvb_tab,
                   <lt_vtts> TYPE vttsvb_tab,
                   <lt_vtsp> TYPE vtspvb_tab.

    ASSIGN ir_vtts->* TO <lt_vtts>.
    ASSIGN ir_vttp->* TO <lt_vttp>.
    ASSIGN ir_vtsp->* TO <lt_vtsp>.

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
          et_stops  = lt_stops ).

      CLEAR: lv_count.

      LOOP AT lt_stops ASSIGNING FIELD-SYMBOL(<ls_stops>).
        ADD 1 TO lv_count.

        APPEND lv_count                    TO cs_header-stops_num.
        APPEND <ls_stops>-stopid           TO cs_header-stops_stopid.
        APPEND <ls_stops>-stopcnt          TO cs_header-stops_stopcnt.
        APPEND <ls_stops>-loccat           TO cs_header-stops_loccat.
        APPEND <ls_stops>-loctype          TO cs_header-stops_loctype.
        APPEND <ls_stops>-locid            TO cs_header-stops_locid.
        APPEND <ls_stops>-lstelz_txt       TO cs_header-stops_lstelz_txt.
        APPEND <ls_stops>-kunablaz_txt     TO cs_header-stops_kunablaz_txt.
        APPEND <ls_stops>-lgortaz_txt      TO cs_header-stops_lgortaz_txt.
        APPEND <ls_stops>-lgnumaz          TO cs_header-stops_lgnumaz.
        APPEND <ls_stops>-toraz            TO cs_header-stops_toraz.
        APPEND <ls_stops>-lgtraz_txt       TO cs_header-stops_lgtraz_txt.
        APPEND <ls_stops>-tsrfo            TO cs_header-stops_tsrfo.
        APPEND <ls_stops>-pln_evt_timezone TO cs_header-stops_pln_evt_tz.

        lv_datetime = zcl_gtt_pof_tools=>get_valid_datetime(
                        iv_timestamp = <ls_stops>-pln_evt_datetime ).
        APPEND  lv_datetime TO cs_header-stops_pln_evt_dt.
      ENDLOOP.

      SORT lt_stops BY stopid.
      MOVE-CORRESPONDING lt_stops TO lt_stop_ids.
      DELETE ADJACENT DUPLICATES FROM lt_stop_ids.

      LOOP AT lt_stop_ids ASSIGNING FIELD-SYMBOL(<ls_stop_ids>).
        APPEND <ls_stop_ids>-stopid  TO cs_header-stpid_stopid.
        APPEND <ls_stop_ids>-stopcnt TO cs_header-stpid_stopcnt.
        APPEND <ls_stop_ids>-loctype TO cs_header-stpid_loctype.
        APPEND <ls_stop_ids>-locid   TO cs_header-stpid_locid.
      ENDLOOP.

      READ TABLE lt_stops ASSIGNING <ls_stops> INDEX 1.
      IF sy-subrc = 0.
        cs_header-departure_dt      = zcl_gtt_pof_tools=>convert_datetime_to_utc(
                                        iv_datetime = <ls_stops>-pln_evt_datetime
                                        iv_timezone = <ls_stops>-pln_evt_timezone ).
        cs_header-departure_tz      = <ls_stops>-pln_evt_timezone.
        cs_header-departure_locid   = <ls_stops>-locid.
        cs_header-departure_loctype = <ls_stops>-loctype.
      ENDIF.

      READ TABLE lt_stops ASSIGNING <ls_stops> INDEX lines( lt_stops ).
      IF sy-subrc = 0.
        cs_header-arrival_dt      = zcl_gtt_pof_tools=>convert_datetime_to_utc(
                                        iv_datetime = <ls_stops>-pln_evt_datetime
                                        iv_timezone = <ls_stops>-pln_evt_timezone ).
        cs_header-arrival_tz      = <ls_stops>-pln_evt_timezone.
        cs_header-arrival_locid   = <ls_stops>-locid.
        cs_header-arrival_loctype = <ls_stops>-loctype.
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTS' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_resource_tables.

    DATA(lv_res_tid)  = get_resource_tracking_id(
                          is_vttk = is_vttk ).

    IF lv_res_tid IS NOT INITIAL.
      et_ref_cnt    = VALUE #( ( 1 ) ).
      et_tp_id      = VALUE #( ( CONV #( lv_res_tid ) ) ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_tracked_object_tables.

    IF is_vttk-exti1 IS NOT INITIAL AND
       is_vttk-vsart = '01'.

      et_res_id  = VALUE #( ( 'NA' ) ).
      et_res_val = VALUE #( ( is_vttk-exti1 ) ).

    ELSEIF is_vttk-vsart = '04' AND
           is_vttk-signi IS NOT INITIAL.

      et_res_id  = VALUE #( ( 'CONTAINER_ID' ) ).
      et_res_val = VALUE #( ( is_vttk-signi ) ).

    ELSEIF is_vttk-exti1 IS NOT INITIAL AND
           ( is_vttk-vsart = '05' OR
             is_vttk-vsart = '15' ).

      et_res_id  = VALUE #( ( 'FLIGHT_NUMBER' ) ).
      et_res_val = VALUE #( ( is_vttk-exti1 ) ).

    ELSE.
      CLEAR: et_res_id[], et_res_val[].
    ENDIF.

  ENDMETHOD.


  METHOD get_forwarding_agent_id_number.

    DATA: lv_forward_agt TYPE bu_partner,
          lt_bpdetail    TYPE STANDARD TABLE OF bapibus1006_id_details.

    CALL METHOD cl_site_bp_assignment=>select_bp_via_cvi_link
      EXPORTING
        i_lifnr = iv_tdlnr
      IMPORTING
        e_bp    = lv_forward_agt.

    CALL FUNCTION 'BAPI_IDENTIFICATIONDETAILS_GET'
      EXPORTING
        businesspartner      = lv_forward_agt
      TABLES
        identificationdetail = lt_bpdetail.

    READ TABLE lt_bpdetail ASSIGNING FIELD-SYMBOL(<ls_bpdetail>)
      WITH KEY identificationtype = zif_gtt_pof_app_constants=>cv_agent_id_type
      BINARY SEARCH.

    rv_id_num   = COND #( WHEN sy-subrc = 0
                            THEN zif_gtt_pof_app_constants=>cv_agent_id_prefix &&
                                 <ls_bpdetail>-identificationnumber ).

  ENDMETHOD.


  METHOD get_resource_tracking_id.

    rv_tracking_id  = COND #(
      WHEN is_vttk-exti1 IS NOT INITIAL AND
           ( is_vttk-vsart = '01' OR
             is_vttk-vsart = '05' OR
             is_vttk-vsart = '15' )
        THEN |{ is_vttk-tknum }{ is_vttk-exti1 }|
      WHEN is_vttk-signi IS NOT INITIAL AND
           is_vttk-vsart = '04'
        THEN |{ is_vttk-tknum }{ is_vttk-signi }|
    ).

  ENDMETHOD.


  METHOD get_shippment_header.

    TYPES: tt_vttk TYPE STANDARD TABLE OF vttkvb.

    FIELD-SYMBOLS: <lt_vttk> TYPE tt_vttk.

    DATA(lv_tknum)  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = is_app_object-maintabref
                        iv_field_name  = 'TKNUM' ).

    ASSIGN ir_vttk->* TO <lt_vttk>.
    IF <lt_vttk> IS ASSIGNED.
      READ TABLE <lt_vttk> ASSIGNING FIELD-SYMBOL(<ls_vttk>)
        WITH KEY tknum = lv_tknum.

      IF sy-subrc = 0.
        rr_vttk = REF #( <ls_vttk> ).
      ELSE.
        MESSAGE e005(zgtt_pof) WITH 'VTTK OLD' lv_tknum
          INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTK' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_shippment_item_count.

    FIELD-SYMBOLS: <lt_vttp> TYPE ANY TABLE.

    ASSIGN ir_vttp->* TO <lt_vttp>.

    rv_count    = lines( <lt_vttp> ).

  ENDMETHOD.


  METHOD get_shippment_type.

    rv_ship_type  = COND #( WHEN iv_vttp_cnt <= 1
                              THEN SWITCH #( iv_vsart
                                             WHEN '01' THEN '18'
                                             WHEN '04' THEN '3' )
                              ELSE SWITCH #( iv_vsart
                                             WHEN '01' THEN '17'
                                             WHEN '04' THEN '2' ) ).

  ENDMETHOD.


  METHOD get_transportation_mode.

    rv_trans_mode = SWITCH #( iv_vsart
                              WHEN '04' THEN '01'   "Sea
                              WHEN '03' THEN '02'   "Rail
                              WHEN '01' THEN '03'   "Road
                              WHEN '05' THEN '04'   "Air
                              WHEN '15' THEN '04'   "Air
                              WHEN '02' THEN '05'   "Mail
                              WHEN ''   THEN '00'   "Not specified
                                        ELSE '09'). "Not applicable

  ENDMETHOD.


  METHOD is_object_changed.

    rv_result = zcl_gtt_pof_tools=>is_object_changed(
                  is_app_object    = is_app_object
                  io_ef_parameters = mo_ef_parameters
                  it_check_tables  = VALUE #( ( zif_gtt_pof_app_constants=>cs_tabledef-sh_item_new )
                                              ( zif_gtt_pof_app_constants=>cs_tabledef-sh_item_new )
                                              ( zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_new )
                                              ( zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_old )
                                              ( zif_gtt_pof_app_constants=>cs_tabledef-sh_item_stage_new )
                                              ( zif_gtt_pof_app_constants=>cs_tabledef-sh_item_stage_old ) )
                  iv_key_field     = 'TKNUM'
                  iv_upd_field     = 'UPDKZ' ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~check_relevance.

    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_new ).

    rv_result = zif_gtt_pof_ef_constants=>cs_condition-false.

    IF zcl_gtt_pof_sh_tools=>is_appropriate_type( ir_vttk = is_app_object-maintabref ) = abap_true AND
       zcl_gtt_pof_sh_tools=>is_delivery_assigned( ir_vttp = lr_vttp ) = abap_true AND
       is_object_changed( is_app_object = is_app_object ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN zif_gtt_pof_ef_constants=>cs_change_mode-insert.
          rv_result   = zif_gtt_pof_ef_constants=>cs_condition-true.
        WHEN zif_gtt_pof_ef_constants=>cs_change_mode-update OR
             zif_gtt_pof_ef_constants=>cs_change_mode-undefined.
          rv_result   = zcl_gtt_pof_tools=>are_structures_different(
                          ir_data1  = zif_gtt_pof_tp_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = zif_gtt_pof_tp_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_data.

    FIELD-SYMBOLS: <ls_header>  TYPE ts_sh_header.

    rr_data   = NEW ts_sh_header(  ).

    ASSIGN rr_data->* TO <ls_header>.

    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
                      iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_new ).

    fill_header_from_vttk(
      EXPORTING
        ir_vttk     = is_app_object-maintabref
        iv_vttp_cnt = get_shippment_item_count( ir_vttp = lr_vttp )
      CHANGING
        cs_header   = <ls_header> ).

    fill_header_from_vttp(
      EXPORTING
        ir_vttk   = is_app_object-maintabref
        ir_vttp   = lr_vttp
      CHANGING
        cs_header = <ls_header> ).

    fill_header_from_vtts(
      EXPORTING
        ir_vttk   = is_app_object-maintabref
        ir_vttp   = lr_vttp
        ir_vtts   = mo_ef_parameters->get_appl_table(
                      iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_new )
        ir_vtsp   = mo_ef_parameters->get_appl_table(
                      iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_stage_new )
      CHANGING
        cs_header = <ls_header> ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_data_old.

    FIELD-SYMBOLS: <ls_header>  TYPE ts_sh_header.

    DATA(lo_sh_data)  = NEW zcl_gtt_pof_sh_data_old(
                          io_ef_parameters = mo_ef_parameters ).

    DATA(lr_vttk) = get_shippment_header(
                      is_app_object = is_app_object
                      ir_vttk       = lo_sh_data->get_vttk( ) ).

    rr_data   = NEW ts_sh_header(  ).

    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_vttk(
      EXPORTING
        ir_vttk     = lr_vttk
        iv_vttp_cnt = get_shippment_item_count(
                        ir_vttp = lo_sh_data->get_vttp( ) )
      CHANGING
        cs_header   = <ls_header> ).

    fill_header_from_vttp(
      EXPORTING
        ir_vttk   = lr_vttk
        ir_vttp   = lo_sh_data->get_vttp( )
      CHANGING
        cs_header = <ls_header> ).

    fill_header_from_vtts(
      EXPORTING
        ir_vttk   = lr_vttk
        ir_vttp   = lo_sh_data->get_vttp( )
        ir_vtts   = lo_sh_data->get_vtts( )
        ir_vtsp   = lo_sh_data->get_vtsp( )
      CHANGING
        cs_header = <ls_header> ).


  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_field_parameter.

    CASE iv_parameter.
      WHEN zif_gtt_pof_ef_constants=>cs_parameter_id-key_field.
        rv_result   = boolc( iv_field_name = cs_mapping-deliv_cnt     OR
                             iv_field_name = cs_mapping-trobj_res_val OR
                             iv_field_name = cs_mapping-resrc_cnt     OR
                             iv_field_name = cs_mapping-crdoc_ref_val OR
                             iv_field_name = cs_mapping-stops_num     OR
                             iv_field_name = cs_mapping-stpid_stopcnt ).
      WHEN zif_gtt_pof_ef_constants=>cs_parameter_id-no_empty_tag.
        rv_result   = boolc( iv_field_name = cs_mapping-departure_dt      OR
                             iv_field_name = cs_mapping-departure_tz      OR
                             iv_field_name = cs_mapping-departure_locid   OR
                             iv_field_name = cs_mapping-departure_loctype OR
                             iv_field_name = cs_mapping-arrival_dt        OR
                             iv_field_name = cs_mapping-arrival_tz        OR
                             iv_field_name = cs_mapping-arrival_locid     OR
                             iv_field_name = cs_mapping-arrival_loctype ).
      WHEN OTHERS.
        CLEAR: rv_result.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_mapping_structure.

    rr_data = REF #( cs_mapping ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_track_id_data.

    "another tip is that: for tracking ID type 'SHIPMENT_ORDER' of delivery header,
    "and for tracking ID type 'RESOURCE' of shipment header,
    "DO NOT enable START DATE and END DATE

    DATA: lv_dummy    TYPE string.

    FIELD-SYMBOLS: <ls_vttk>     TYPE vttkvb,
                   <ls_vttk_old> TYPE vttkvb.

    ASSIGN is_app_object-maintabref->* TO <ls_vttk>.

    IF <ls_vttk> IS ASSIGNED.
      " SHIPMENT Tracking ID
      et_track_id_data  = VALUE #( (
        appsys      = mo_ef_parameters->get_appsys( )
        appobjtype  = is_app_object-appobjtype
        appobjid    = is_app_object-appobjid
        trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-sh_number
        trxid       = |{ <ls_vttk>-tknum }|
        timzon      = zcl_gtt_pof_tools=>get_system_time_zone( )
      ) ).

      DATA(lv_res_tid_new)  = get_resource_tracking_id(
                                is_vttk = <ls_vttk> ).

      " is shipment created?
      IF <ls_vttk>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-insert AND
         lv_res_tid_new IS NOT INITIAL.

        " add RESOURCE Tracking ID for the new Shipment
        et_track_id_data  = VALUE #( BASE et_track_id_data (
          appsys      = mo_ef_parameters->get_appsys( )
          appobjtype  = is_app_object-appobjtype
          appobjid    = is_app_object-appobjid
          trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-sh_resource
          trxid       = lv_res_tid_new
        ) ).

        " is shipment updated?
      ELSEIF <ls_vttk>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-update.
        DATA(lr_vttk_old) = get_shippment_header(
                              is_app_object = is_app_object
                              ir_vttk       = mo_ef_parameters->get_appl_table(
                                                iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_header_old ) ).

        ASSIGN lr_vttk_old->* TO <ls_vttk_old>.
        IF <ls_vttk_old> IS ASSIGNED.
          DATA(lv_res_tid_old)  = get_resource_tracking_id(
                                    is_vttk = <ls_vttk_old> ).

          " is RESOURCE Tracking ID changed?
          IF lv_res_tid_old <> lv_res_tid_new.
            " add new RESOURCE Tracking ID
            IF lv_res_tid_new IS NOT INITIAL.
              et_track_id_data  = VALUE #( BASE et_track_id_data (
                appsys      = mo_ef_parameters->get_appsys( )
                appobjtype  = is_app_object-appobjtype
                appobjid    = is_app_object-appobjid
                trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-sh_resource
                trxid       = lv_res_tid_new
              ) ).
            ENDIF.

            " delete old RESOURCE Tracking ID
            IF lv_res_tid_old IS NOT INITIAL.
              et_track_id_data  = VALUE #( BASE et_track_id_data (
                appsys      = mo_ef_parameters->get_appsys( )
                appobjtype  = is_app_object-appobjtype
                appobjid    = is_app_object-appobjid
                trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-sh_resource
                trxid       = lv_res_tid_old
                action      = zif_gtt_pof_ef_constants=>cs_change_mode-delete
              ) ).
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE e002(zgtt_pof) WITH 'VTTK' INTO lv_dummy.
          zcl_gtt_pof_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTK' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
