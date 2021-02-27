*&---------------------------------------------------------------------*
*& Local class definition - Business Object Readers
*&---------------------------------------------------------------------*

INTERFACE lif_app_types.
  TYPES: ts_ekko TYPE /saptrx/mm_po_hdr,
         tt_ekko TYPE STANDARD TABLE OF ts_ekko.

  TYPES: ts_uekpo TYPE uekpo,
         tt_uekpo TYPE STANDARD TABLE OF ts_uekpo.

  TYPES: ts_ueket TYPE ueket,
         tt_ueket TYPE STANDARD TABLE OF ts_ueket.

  TYPES: ts_uekes TYPE uekes,
         tt_uekes TYPE STANDARD TABLE OF ts_uekes.

  TYPES: ts_likpvb TYPE likp,
         tt_likpvb TYPE STANDARD TABLE OF ts_likpvb.

  TYPES: ts_lipsvb TYPE lipsvb,
         tt_lipsvb TYPE STANDARD TABLE OF ts_lipsvb.

  TYPES: ts_vttkvb TYPE vttkvb,
         tt_vttkvb TYPE STANDARD TABLE OF ts_vttkvb.

  TYPES: ts_vttpvb TYPE vttpvb,
         tt_vttpvb TYPE STANDARD TABLE OF ts_vttpvb.

  TYPES: ts_vttsvb TYPE vttsvb,
         tt_vttsvb TYPE STANDARD TABLE OF ts_vttsvb.

  TYPES: ts_vtspvb TYPE vtspvb,
         tt_vtspvb TYPE STANDARD TABLE OF ts_vtspvb.

  TYPES: ts_mseg TYPE mseg,
         tt_mseg TYPE STANDARD TABLE OF ts_mseg.

  TYPES: tv_ship_type        TYPE char2,
         tv_trans_mode       TYPE char2,
         tv_departure_dt     TYPE timestamp,  "char15,
         tv_departure_tz     TYPE timezone,
         tv_arrival_dt       TYPE timestamp,  "char15,
         tv_arrival_tz       TYPE timezone,
         tv_deliv_cnt        TYPE int4,
         tv_trobj_res_id     TYPE char12,
         tv_trobj_res_val    TYPE char20,
         tv_resrc_cnt        TYPE int4,
         tv_resrc_tp_id      TYPE char30,
         tv_crdoc_ref_typ    TYPE char3,
         tv_crdoc_ref_val    TYPE tndr_trkid,
         tv_stopnum          TYPE int4,
         tv_stopid           TYPE char255,
         tv_stopcnt          TYPE int4,
         tv_loccat           TYPE char1,
         tv_loctype          TYPE char20,
         tv_locid            TYPE char10,
         tv_lstelz_txt       TYPE char20,
         tv_kunablaz_txt     TYPE char25,
         tv_lgortaz_txt      TYPE char16,
         tv_lgnumaz          TYPE char3,
         tv_toraz            TYPE char3,
         tv_lgtraz_txt       TYPE char50,
         tv_tsrfo            TYPE num4,
         tv_pln_evt_datetime TYPE timestamp,  "char15,
         tv_pln_evt_timezone TYPE char6.

  TYPES: BEGIN OF ts_stops,
           stopid           TYPE tv_stopid,
           stopcnt          TYPE tv_stopcnt,
           loccat           TYPE tv_loccat,
           loctype          TYPE tv_loctype,
           locid            TYPE tv_locid,
           lstelz_txt       TYPE tv_lstelz_txt,
           kunablaz_txt     TYPE tv_kunablaz_txt,
           lgortaz_txt      TYPE tv_lgortaz_txt,
           lgnumaz          TYPE tv_lgnumaz,
           toraz            TYPE tv_toraz,
           lgtraz_txt       TYPE tv_lgtraz_txt,
           tknum            TYPE tknum,
           tsnum            TYPE tsnum,
           tsrfo            TYPE tsrfo,
           pln_evt_datetime TYPE timestamp,
           pln_evt_timezone TYPE timezone,
         END OF ts_stops,
         tt_stops TYPE STANDARD TABLE OF ts_stops
                    WITH EMPTY KEY.

  TYPES: BEGIN OF ts_dlv_watch_stops,
           vbeln  TYPE vbeln_vl,
           stopid TYPE tv_stopid,
           loccat TYPE tv_loccat,
         END OF ts_dlv_watch_stops,
         tt_dlv_watch_stops TYPE STANDARD TABLE OF ts_dlv_watch_stops
                              WITH EMPTY KEY.

ENDINTERFACE.

INTERFACE lif_app_constants.

  CONSTANTS: BEGIN OF cs_tabledef,
               po_header_new       TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ORDER_HEADER_NEW',
               po_header_old       TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ORDER_HEADER_OLD',
               po_item_new         TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ITEM_NEW',
               po_item_old         TYPE /saptrx/strucdatadef VALUE 'PURCHASE_ITEM_OLD',
               po_sched_new        TYPE /saptrx/strucdatadef VALUE 'PO_SCHED_LINE_ITEM_NEW',
               po_sched_old        TYPE /saptrx/strucdatadef VALUE 'PO_SCHED_LINE_ITEM_OLD',
               po_vend_conf_new    TYPE /saptrx/strucdatadef VALUE 'VENDOR_CONFIRMATION_NEW',
               po_vend_conf_old    TYPE /saptrx/strucdatadef VALUE 'VENDOR_CONFIRMATION_OLD',
               md_material_header  TYPE /saptrx/strucdatadef VALUE 'MATERIAL_HEADER',
               md_material_segment TYPE /saptrx/strucdatadef VALUE 'MATERIAL_SEGMENT',
               md_update_control   TYPE /saptrx/strucdatadef VALUE 'UPDATE_CONTROL',
               dl_header_new       TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER_NEW',
               dl_header_old       TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER_OLD',
               dl_hdr_status_new   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HDR_STATUS_NEW',
               dl_hdr_status_old   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HDR_STATUS_OLD',
               dl_item_new         TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_NEW',
               dl_item_old         TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_OLD',
               dl_partners_new     TYPE /saptrx/strucdatadef VALUE 'PARTNERS_NEW',
               dl_partners_old     TYPE /saptrx/strucdatadef VALUE 'PARTNERS_OLD',
               dl_itm_status_new   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_STATUS_NEW',
               dl_itm_status_old   TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM_STATUS_OLD',
               dl_hu_item_new      TYPE /saptrx/strucdatadef VALUE 'HU_ITEM_NEW',
               dl_hu_item_old      TYPE /saptrx/strucdatadef VALUE 'HU_ITEM_OLD',
               sh_header_new       TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_HEADER_NEW',
               sh_header_old       TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_HEADER_OLD',
               sh_item_new         TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_NEW',
               sh_item_old         TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_OLD',
               sh_stage_new        TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_LEG_NEW',
               sh_stage_old        TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_LEG_OLD',
               sh_item_stage_new   TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_LEG_NEW',
               sh_item_stage_old   TYPE /saptrx/strucdatadef VALUE 'SHIPMENT_ITEM_LEG_OLD',
               sh_delivery_header  TYPE /saptrx/strucdatadef VALUE 'DELIVERY_HEADER',
               sh_delivery_item    TYPE /saptrx/strucdatadef VALUE 'DELIVERY_ITEM',
             END OF cs_tabledef.

  CONSTANTS: BEGIN OF cs_system_fields,
               actual_bisiness_timezone TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_TIMEZONE',
               actual_bisiness_datetime TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_DATETIME',
             END OF cs_system_fields.

  CONSTANTS: BEGIN OF cs_trxcod,
               po_number   TYPE /saptrx/trxcod VALUE 'PURCHASE_ORDER',
               po_position TYPE /saptrx/trxcod VALUE 'PURCHASE_ORDER_ITEM',
               dl_number   TYPE /saptrx/trxcod VALUE 'INBOUND_DELIVERY',
               dl_position TYPE /saptrx/trxcod VALUE 'INBOUND_DELIVERY_IT',
               sh_number   TYPE /saptrx/trxcod VALUE 'INBOUND_SHIPMENT',
               sh_resource TYPE /saptrx/trxcod VALUE 'INBOUND_RESOURCE',
             END OF cs_trxcod.

  CONSTANTS: BEGIN OF cs_relevance,
               bsart TYPE ekko-bsart VALUE 'NB',
               ebtyp TYPE ekes-ebtyp VALUE 'AB',
               lfart TYPE likp-lfart VALUE 'EL',
               pstyv TYPE lips-pstyv VALUE 'ELN',
               shtyp TYPE vttk-shtyp VALUE '0010',
             END OF cs_relevance.

  CONSTANTS: BEGIN OF cs_milestone,
               po_confirmation  TYPE /saptrx/appl_event_tag VALUE 'CONFIRMATION',
               po_goods_receipt TYPE /saptrx/appl_event_tag VALUE 'GOODS_RECEIPT',
               po_undeletion    TYPE /saptrx/appl_event_tag VALUE 'UNDELETION',
               po_deletion      TYPE /saptrx/appl_event_tag VALUE 'DELETION',
               dl_put_away      TYPE /saptrx/appl_event_tag VALUE 'PUT_AWAY',
               dl_packing       TYPE /saptrx/appl_event_tag VALUE 'PACKING',
               dl_goods_receipt TYPE /saptrx/appl_event_tag VALUE 'GOODS_RECEIPT',
               dl_pod           TYPE /saptrx/appl_event_tag VALUE 'SHP_POD',
               sh_check_in      TYPE /saptrx/appl_event_tag VALUE 'CHECK_IN',
               sh_load_start    TYPE /saptrx/appl_event_tag VALUE 'LOAD_BEGIN',
               sh_load_end      TYPE /saptrx/appl_event_tag VALUE 'LOAD_END',
               sh_departure     TYPE /saptrx/appl_event_tag VALUE 'DEPARTURE',
               sh_arrival       TYPE /saptrx/appl_event_tag VALUE 'ARRIV_DEST',
               sh_pod           TYPE /saptrx/appl_event_tag VALUE 'POD',
             END OF cs_milestone.

  CONSTANTS: BEGIN OF cs_event_param,
               quantity      TYPE /saptrx/paramname VALUE 'QUANTITY',
               confirm_type  TYPE /saptrx/paramname VALUE 'CONFIRM_TYPE',
               reversal      TYPE /saptrx/paramname VALUE 'REVERSAL_INDICATOR',
               location_id   TYPE /saptrx/paramname VALUE 'LOCATION_ID',
               location_type TYPE /saptrx/paramname VALUE 'LOCATION_TYPE',
             END OF cs_event_param.

  CONSTANTS: BEGIN OF cs_bstae,
               confirm  TYPE ekpo-bstae VALUE '0001',
               delivery TYPE ekpo-bstae VALUE '0004',
             END OF cs_bstae.

  CONSTANTS: BEGIN OF cs_loekz,
               active  TYPE ekpo-loekz VALUE '',
               deleted TYPE ekpo-loekz VALUE 'L',
             END OF cs_loekz.

  CONSTANTS: BEGIN OF cs_delivery_stat,
               not_relevant    TYPE wbsta VALUE '',
               not_processed   TYPE wbsta VALUE 'A',
               partially_proc  TYPE wbsta VALUE 'B',
               completely_proc TYPE wbsta VALUE 'C',
             END OF cs_delivery_stat.

  CONSTANTS: BEGIN OF cs_md_type,
               goods_receipt TYPE mkpf-blart VALUE 'WE',
             END OF cs_md_type.

  CONSTANTS: BEGIN OF cs_parvw,
               supplier TYPE parvw VALUE 'LF',
             END OF cs_parvw.

  CONSTANTS: BEGIN OF cs_adrtype,
               organization TYPE ad_adrtype VALUE '1',
             END OF cs_adrtype.

  CONSTANTS: BEGIN OF cs_loccat,
               departure TYPE lif_app_types=>tv_loccat VALUE 'S',
               arrival   TYPE lif_app_types=>tv_loccat VALUE 'D',
             END OF cs_loccat.

  CONSTANTS: BEGIN OF cs_vbtyp,
               shipment TYPE vbtyp VALUE '8',
               delivery TYPE vbtyp VALUE '7',
             END OF cs_vbtyp.

  CONSTANTS: BEGIN OF cs_start_evtcnt,
               shipment TYPE i VALUE 1000000000,
               delivery TYPE i VALUE 1100000000,
             END OF cs_start_evtcnt.

  CONSTANTS: cv_agent_id_type   TYPE bu_id_type VALUE 'LBN001',
             cv_agent_id_prefix TYPE c LENGTH 4 VALUE 'LBN#' .

ENDINTERFACE.


**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_po_tools DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS get_tracking_id_po_item
      IMPORTING
        ir_ekpo            TYPE REF TO data
      RETURNING
        VALUE(rv_track_id) TYPE /saptrx/trxid
      RAISING
        cx_udm_message.

    CLASS-METHODS is_appropriate_po_type
      IMPORTING
        ir_ekko          TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    CLASS-METHODS is_appropriate_po_item
      IMPORTING
        ir_ekpo          TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_po_tools IMPLEMENTATION.
  METHOD get_tracking_id_po_item.
    DATA: lv_ebeln TYPE ekpo-ebeln,
          lv_ebelp TYPE ekpo-ebelp.

    lv_ebeln  = lcl_tools=>get_field_of_structure(
                  ir_struct_data = ir_ekpo
                  iv_field_name  = 'EBELN' ).

    lv_ebelp  = lcl_tools=>get_field_of_structure(
                  ir_struct_data = ir_ekpo
                  iv_field_name  = 'EBELP' ).

    rv_track_id   = |{ lv_ebeln }{ lv_ebelp }|.
  ENDMETHOD.

  METHOD is_appropriate_po_type.
    DATA(lv_bsart)  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = ir_ekko
                        iv_field_name  = 'BSART' ).

    rv_result = boolc( lv_bsart = lif_app_constants=>cs_relevance-bsart ).
  ENDMETHOD.

  METHOD is_appropriate_po_item.
    DATA(lv_wepos)  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = ir_ekpo
                        iv_field_name  = 'WEPOS' ).

    rv_result = boolc( lv_wepos = abap_true ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_dl_tools DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS convert_quantity_into_pounits
      IMPORTING
        iv_quantity_uom        TYPE any
        ir_lips                TYPE REF TO data
      RETURNING
        VALUE(rv_quantity_pou) TYPE f
      RAISING
        cx_udm_message.

    CLASS-METHODS get_addres_info
      IMPORTING
        iv_addr_type TYPE ad_adrtype DEFAULT lif_app_constants=>cs_adrtype-organization
        iv_addr_numb TYPE ad_addrnum
      EXPORTING
        ev_address   TYPE clike
        ev_email     TYPE clike
        ev_telephone TYPE clike
      RAISING
        cx_udm_message.

    CLASS-METHODS get_door_description
      IMPORTING
        iv_lgnum        TYPE lgnum
        iv_lgtor        TYPE lgtor
      RETURNING
        VALUE(rv_descr) TYPE /saptrx/paramval200
      RAISING
        cx_udm_message.

    CLASS-METHODS get_delivery_date
      IMPORTING
        ir_data        TYPE REF TO data
      RETURNING
        VALUE(rv_date) TYPE /saptrx/event_exp_datetime
      RAISING
        cx_udm_message.

    CLASS-METHODS get_next_event_counter
      RETURNING
        VALUE(rv_evtcnt) TYPE /saptrx/evtcnt.

    CLASS-METHODS get_plant_address_number
      IMPORTING
        iv_werks        TYPE werks_d
      RETURNING
        VALUE(ev_adrnr) TYPE adrnr
      RAISING
        cx_udm_message.

    CLASS-METHODS get_tracking_id_dl_item
      IMPORTING
        ir_lips            TYPE REF TO data
      RETURNING
        VALUE(rv_track_id) TYPE /saptrx/trxid
      RAISING
        cx_udm_message.

    CLASS-METHODS is_appropriate_dl_item
      IMPORTING
        ir_struct        TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    CLASS-METHODS is_appropriate_dl_type
      IMPORTING
        ir_struct        TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

  PRIVATE SECTION.
    CLASS-DATA: mv_evtcnt   TYPE /saptrx/evtcnt VALUE lif_app_constants=>cs_start_evtcnt-delivery.
ENDCLASS.

CLASS lcl_dl_tools IMPLEMENTATION.
  METHOD convert_quantity_into_pounits.
    DATA(lv_matnr)  = CONV matnr( lcl_tools=>get_field_of_structure(
                                       ir_struct_data = ir_lips
                                       iv_field_name  = 'MATNR' ) ).
    DATA(lv_vrkme)  = CONV vrkme( lcl_tools=>get_field_of_structure(
                                       ir_struct_data = ir_lips
                                       iv_field_name  = 'VRKME' ) ).

    CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
      EXPORTING
        input                = iv_quantity_uom
        matnr                = lv_matnr
        meinh                = lv_vrkme
      IMPORTING
        output               = rv_quantity_pou
      EXCEPTIONS
        conversion_not_found = 1
        input_invalid        = 2
        material_not_found   = 3
        meinh_not_found      = 4
        meins_missing        = 5
        no_meinh             = 6
        output_invalid       = 7
        overflow             = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
      CLEAR: rv_quantity_pou.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_addres_info.
    DATA: lt_address   TYPE szadr_printform_table,
          ls_addr_comp TYPE szadr_addr1_complete.


    IF ev_address IS REQUESTED.
      CLEAR: ev_address.

      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
        EXPORTING
          address_type                   = iv_addr_type
          address_number                 = iv_addr_numb
        IMPORTING
          address_printform_table        = lt_address
        EXCEPTIONS
          address_blocked                = 1
          person_blocked                 = 2
          contact_person_blocked         = 3
          addr_to_be_formated_is_blocked = 4
          OTHERS                         = 5.

      IF sy-subrc = 0.
        LOOP AT lt_address ASSIGNING FIELD-SYMBOL(<ls_address>).
          ev_address  = COND #( WHEN ev_address IS INITIAL
                                  THEN <ls_address>-address_line
                                  ELSE |{ ev_address }${ <ls_address>-address_line }| ).
        ENDLOOP.
      ELSE.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

    IF ev_email IS REQUESTED OR ev_telephone IS REQUESTED.
      CLEAR: ev_email, ev_telephone.

      CALL FUNCTION 'ADDR_GET_COMPLETE'
        EXPORTING
          addrnumber              = iv_addr_numb
        IMPORTING
          addr1_complete          = ls_addr_comp
        EXCEPTIONS
          parameter_error         = 1
          address_not_exist       = 2
          internal_error          = 3
          wrong_access_to_archive = 4
          address_blocked         = 5
          OTHERS                  = 6.

      IF sy-subrc = 0.
        ev_email      = VALUE #( ls_addr_comp-adsmtp_tab[ 1 ]-adsmtp-smtp_addr OPTIONAL ).
        ev_telephone  = VALUE #( ls_addr_comp-adtel_tab[ 1 ]-adtel-tel_number OPTIONAL ).
      ELSE.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD get_door_description.
    "concatenate T300T-LNUMT '/' T30BT-ltort using SY-LANGU and LIPSVB-LGNUM & LIPSVB-LGTOR
    DATA: ls_t300t TYPE t300t,
          lv_ltort TYPE t30bt-ltort.

    CLEAR: rv_descr.

    CALL FUNCTION 'T300T_SINGLE_READ'
      EXPORTING
        t300t_spras = sy-langu
        t300t_lgnum = iv_lgnum
      IMPORTING
        wt300t      = ls_t300t
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.

    IF sy-subrc = 0.
      SELECT SINGLE ltort
        INTO lv_ltort
        FROM t30bt
        WHERE spras = sy-langu
          AND lgnum = iv_lgnum
          AND lgtor = iv_lgtor.

      IF sy-subrc = 0.
        rv_descr    = |{ ls_t300t-lnumt }/{ lv_ltort }|.
      ELSE.
        MESSAGE e057(00) WITH iv_lgnum iv_lgtor '' 'T30BT'
          INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_delivery_date.
    rv_date = lcl_tools=>get_local_timestamp(
                iv_date = lcl_tools=>get_field_of_structure(
                            ir_struct_data = ir_data
                            iv_field_name  = 'LFDAT' )
                iv_time = lcl_tools=>get_field_of_structure(
                            ir_struct_data = ir_data
                            iv_field_name  = 'LFUHR' ) ).
  ENDMETHOD.

  METHOD get_next_event_counter.
    ADD 1 TO mv_evtcnt.

    rv_evtcnt = mv_evtcnt.
  ENDMETHOD.

  METHOD get_plant_address_number.
    DATA: ls_t001w TYPE T001w.

    CALL FUNCTION 'WCB_T001W_SINGLE_READ'
      EXPORTING
        i_werks   = iv_werks
      IMPORTING
        e_t001w   = ls_t001w
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 0.
      ev_adrnr    = ls_t001w-adrnr.
    ELSE.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_tracking_id_dl_item.
    DATA: lv_vbeln TYPE lips-vbeln,
          lv_posnr TYPE lips-posnr.

    lv_vbeln  = lcl_tools=>get_field_of_structure(
                  ir_struct_data = ir_lips
                  iv_field_name  = 'VBELN' ).

    lv_posnr  = lcl_tools=>get_field_of_structure(
                  ir_struct_data = ir_lips
                  iv_field_name  = 'POSNR' ).

    rv_track_id   = |{ lv_vbeln }{ lv_posnr }|.
  ENDMETHOD.

  METHOD is_appropriate_dl_item.
    DATA(lv_pstyv)  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = ir_struct
                        iv_field_name  = 'PSTYV' ).

    rv_result = boolc( lv_pstyv = lif_app_constants=>cs_relevance-pstyv ).
  ENDMETHOD.

  METHOD is_appropriate_dl_type.
    DATA(lv_lfart)  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = ir_struct
                        iv_field_name  = 'LFART' ).

    rv_result = boolc( lv_lfart = lif_app_constants=>cs_relevance-lfart ).
  ENDMETHOD.

ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_sh_tools DEFINITION.
  PUBLIC SECTION.

    CLASS-METHODS get_stops_from_shipment
      IMPORTING
        iv_tknum              TYPE tknum
        it_vtts               TYPE vttsvb_tab OPTIONAL
        it_vtsp               TYPE vtspvb_tab OPTIONAL
        it_vttp               TYPE vttpvb_tab OPTIONAL
      EXPORTING
        et_stops              TYPE lif_app_types=>tt_stops
        et_dlv_watching_stops TYPE lif_app_types=>tt_dlv_watch_stops.

    CLASS-METHODS get_carrier_reference_document
      IMPORTING
        is_vttk    TYPE vttkvb
      EXPORTING
        ev_ref_typ TYPE lif_app_types=>tv_crdoc_ref_typ
        ev_ref_val TYPE lif_app_types=>tv_crdoc_ref_val.

    CLASS-METHODS is_appropriate_type
      IMPORTING
        ir_vttk          TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    CLASS-METHODS is_delivery_assigned
      IMPORTING
        ir_vttp          TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    CLASS-METHODS is_object_modified
      IMPORTING
        is_events        TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS get_next_event_counter
      RETURNING
        VALUE(rv_evtcnt) TYPE /saptrx/evtcnt.

  PRIVATE SECTION.
    CLASS-DATA: mv_evtcnt   TYPE /saptrx/evtcnt VALUE lif_app_constants=>cs_start_evtcnt-shipment.

ENDCLASS.

CLASS lcl_sh_tools IMPLEMENTATION.
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
      ls_stop              TYPE lif_app_types=>ts_stops,
      ls_dlv_watching_stop TYPE lif_app_types=>ts_dlv_watch_stops,
*       Count
      lv_stopcnt           TYPE int4,
      lv_cnt               TYPE char04,
*       Source & Destination
      lv_desloctype        TYPE lif_app_types=>tv_loctype,
      lv_deslocid          TYPE lif_app_types=>tv_locid,
      lv_srcloctype        TYPE lif_app_types=>tv_loctype,
      lv_srclocid          TYPE lif_app_types=>tv_locid,
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
        lv_srcloctype = lif_ef_constants=>cs_loc_types-customer.
        lv_srclocid   = ls_vttsvb-kunna.
      ELSEIF ls_vttsvb-vstel IS NOT INITIAL.
        lv_srcloctype = lif_ef_constants=>cs_loc_types-shippingpoint.
        lv_srclocid   = ls_vttsvb-vstel.
      ELSEIF ls_vttsvb-lifna IS NOT INITIAL.
        lv_srcloctype = lif_ef_constants=>cs_loc_types-supplier.
        lv_srclocid   = ls_vttsvb-lifna.
      ELSEIF ls_vttsvb-werka IS NOT INITIAL.
        lv_srcloctype = lif_ef_constants=>cs_loc_types-plant.
        lv_srclocid   = ls_vttsvb-werka.
      ELSEIF ls_vttsvb-knota IS NOT INITIAL.
        lv_srcloctype = lif_ef_constants=>cs_loc_types-logisticlocation.
        lv_srclocid   = ls_vttsvb-knota.
      ENDIF.

*     if current stage line's source = last stage line's destination, no change on stop id & stop count
      IF lv_srcloctype NE lv_desloctype OR lv_srclocid NE lv_deslocid.
        lv_stopcnt = lv_stopcnt + 1.
      ENDIF.

      IF ls_vttsvb-kunnz IS NOT INITIAL.
        lv_desloctype = lif_ef_constants=>cs_loc_types-customer.
        lv_deslocid   = ls_vttsvb-kunnz.
      ELSEIF ls_vttsvb-vstez IS NOT INITIAL.
        lv_desloctype = lif_ef_constants=>cs_loc_types-shippingpoint.
        lv_deslocid   = ls_vttsvb-vstez.
      ELSEIF ls_vttsvb-lifnz IS NOT INITIAL.
        lv_desloctype = lif_ef_constants=>cs_loc_types-supplier.
        lv_deslocid   = ls_vttsvb-lifnz.
      ELSEIF ls_vttsvb-werkz IS NOT INITIAL.
        lv_desloctype = lif_ef_constants=>cs_loc_types-plant.
        lv_deslocid   = ls_vttsvb-werkz.
      ELSEIF ls_vttsvb-knotz IS NOT INITIAL.
        lv_desloctype = lif_ef_constants=>cs_loc_types-logisticlocation.
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

  METHOD get_carrier_reference_document.
    IF is_vttk-vsart = '01' AND is_vttk-tndr_trkid IS NOT INITIAL.
      ev_ref_typ  = 'BN'.
      ev_ref_val  = is_vttk-tndr_trkid.
    ELSEIF is_vttk-vsart = '04' AND is_vttk-tndr_trkid IS NOT INITIAL.
      ev_ref_typ  = 'T50'.
      ev_ref_val  = is_vttk-tndr_trkid.
    ELSE.
      CLEAR: ev_ref_typ, ev_ref_val.
    ENDIF.
  ENDMETHOD.

  METHOD is_appropriate_type.
    DATA(lv_shtyp)  = CONV shtyp( lcl_tools=>get_field_of_structure(
                                    ir_struct_data = ir_vttk
                                    iv_field_name  = 'SHTYP' ) ).

    rv_result   = boolc( lv_shtyp = lif_app_constants=>cs_relevance-shtyp ).
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
      is_events-update_indicator = lif_ef_constants=>cs_change_mode-insert OR
      is_events-update_indicator = lif_ef_constants=>cs_change_mode-update OR
      is_events-update_indicator = lif_ef_constants=>cs_change_mode-undefined
    ).
  ENDMETHOD.

  METHOD get_next_event_counter.
    ADD 1 TO mv_evtcnt.

    rv_evtcnt = mv_evtcnt.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_sh_data_old DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters
      RAISING
        cx_udm_message.

    METHODS get_vttk
      RETURNING
        VALUE(rr_vttk) TYPE REF TO data.

    METHODS get_vttp
      RETURNING
        VALUE(rr_vttp) TYPE REF TO data.

    METHODS get_vtts
      RETURNING
        VALUE(rr_vtts) TYPE REF TO data.

    METHODS get_vtsp
      RETURNING
        VALUE(rr_vtsp) TYPE REF TO data.

  PRIVATE SECTION.
    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters,
          mt_vttk          TYPE lif_app_types=>tt_vttkvb,
          mt_vttp          TYPE lif_app_types=>tt_vttpvb,
          mt_vtts          TYPE lif_app_types=>tt_vttsvb,
          mt_vtsp          TYPE lif_app_types=>tt_vtspvb.

    METHODS init
      RAISING
        cx_udm_message.

    METHODS init_vttk
      RAISING
        cx_udm_message.

    METHODS init_vttp
      RAISING
        cx_udm_message.

    METHODS init_vtts
      RAISING
        cx_udm_message.

    METHODS init_vtsp
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_sh_data_old IMPLEMENTATION.
  METHOD constructor.
    mo_ef_parameters  = io_ef_parameters.

    init( ).
  ENDMETHOD.

  METHOD get_vttk.
    rr_vttk   = REF #( mt_vttk ).
  ENDMETHOD.

  METHOD get_vttp.
    rr_vttp   = REF #( mt_vttp ).
  ENDMETHOD.

  METHOD get_vtts.
    rr_vtts   = REF #( mt_vtts ).
  ENDMETHOD.

  METHOD get_vtsp.
    rr_vtsp   = REF #( mt_vtsp ).
  ENDMETHOD.

  METHOD init.
    init_vttk( ).

    init_vttp( ).

    init_vtts( ).

    init_vtsp( ).
  ENDMETHOD.

  METHOD init_vttk.
    FIELD-SYMBOLS: <lt_vttk_new> TYPE lif_app_types=>tt_vttkvb,
                   <lt_vttk_old> TYPE lif_app_types=>tt_vttkvb.

    DATA(lr_vttk_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_header_new ).
    DATA(lr_vttk_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_header_old ).

    ASSIGN lr_vttk_new->* TO <lt_vttk_new>.
    ASSIGN lr_vttk_old->* TO <lt_vttk_old>.

    IF <lt_vttk_new> IS ASSIGNED AND
       <lt_vttk_old> IS ASSIGNED.

      mt_vttk   = <lt_vttk_old>.
      SORT mt_vttk BY tknum.

      LOOP AT <lt_vttk_new> ASSIGNING FIELD-SYMBOL(<ls_vttk_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vttk
          WITH KEY tknum = <ls_vttk_new>-tknum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vttk_new> INTO mt_vttk INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VTTK' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD init_vttp.
    FIELD-SYMBOLS: <lt_vttp_new> TYPE lif_app_types=>tt_vttpvb,
                   <lt_vttp_old> TYPE lif_app_types=>tt_vttpvb.

    DATA(lr_vttp_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vttp_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_old ).

    ASSIGN lr_vttp_new->* TO <lt_vttp_new>.
    ASSIGN lr_vttp_old->* TO <lt_vttp_old>.

    IF <lt_vttp_new> IS ASSIGNED AND
       <lt_vttp_old> IS ASSIGNED.

      mt_vttp   = <lt_vttp_old>.
      SORT mt_vttp BY tknum tpnum.

      LOOP AT <lt_vttp_new> ASSIGNING FIELD-SYMBOL(<ls_vttp_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vttp
          WITH KEY tknum = <ls_vttp_new>-tknum
                   tpnum = <ls_vttp_new>-tpnum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vttp_new> INTO mt_vttp INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VTTP' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD init_vtts.
    FIELD-SYMBOLS: <lt_vtts_new> TYPE lif_app_types=>tt_vttsvb,
                   <lt_vtts_old> TYPE lif_app_types=>tt_vttsvb.

    DATA(lr_vtts_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtts_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_stage_old ).

    ASSIGN lr_vtts_new->* TO <lt_vtts_new>.
    ASSIGN lr_vtts_old->* TO <lt_vtts_old>.

    IF <lt_vtts_new> IS ASSIGNED AND
       <lt_vtts_old> IS ASSIGNED.

      mt_vtts   = <lt_vtts_old>.
      SORT mt_vtts BY tknum tsnum.

      LOOP AT <lt_vtts_new> ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vtts
          WITH KEY tknum = <ls_vtts_new>-tknum
                   tsnum = <ls_vtts_new>-tsnum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vtts_new> INTO mt_vtts INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VTTS' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD init_vtsp.
    FIELD-SYMBOLS: <lt_vtsp_new> TYPE lif_app_types=>tt_vtspvb,
                   <lt_vtsp_old> TYPE lif_app_types=>tt_vtspvb.

    DATA(lr_vtsp_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_stage_new ).
    DATA(lr_vtsp_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_stage_old ).

    ASSIGN lr_vtsp_new->* TO <lt_vtsp_new>.
    ASSIGN lr_vtsp_old->* TO <lt_vtsp_old>.

    IF <lt_vtsp_new> IS ASSIGNED AND
       <lt_vtsp_old> IS ASSIGNED.

      mt_vtsp   = <lt_vtsp_old>.
      SORT mt_vtsp BY tknum tsnum tpnum.

      LOOP AT <lt_vtsp_new> ASSIGNING FIELD-SYMBOL(<ls_vtsp_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vtsp
          WITH KEY tknum = <ls_vtsp_new>-tknum
                   tsnum = <ls_vtsp_new>-tsnum
                   tpnum = <ls_vtsp_new>-tpnum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vtsp_new> INTO mt_vtsp INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VTSP' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_bo_reader_po_header DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_bo_reader.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters.

  PRIVATE SECTION.
    TYPES: tv_item_num TYPE i,
           tt_item_num TYPE STANDARD TABLE OF tv_item_num WITH EMPTY KEY.

    TYPES: tv_ebelp TYPE char15,
           tt_ebelp TYPE STANDARD TABLE OF tv_ebelp WITH EMPTY KEY.

    TYPES: BEGIN OF ts_po_header,
             ebeln    TYPE ekko-ebeln,
             lifnr    TYPE ekko-lifnr,
             lifnr_lt TYPE /saptrx/loc_id_type,
             werks    TYPE ekpo-werks,
             werks_lt TYPE /saptrx/loc_id_type,
             eindt    TYPE eket-eindt,
             netwr    TYPE ekpo-netwr,
             waers    TYPE ekko-waers,
             inco1    TYPE ekko-inco1,
             incov    TYPE ekko-incov,
             inco2_l  TYPE ekko-inco2_l,
             item_num TYPE tt_item_num,
             ebelp    TYPE tt_ebelp,
           END OF ts_po_header.

    CONSTANTS: BEGIN OF cs_mapping,
                 ebeln    TYPE /saptrx/paramname VALUE 'YN_PO_NUMBER',
                 lifnr    TYPE /saptrx/paramname VALUE 'YN_PO_SUPPLIER_ID',
                 lifnr_lt TYPE /saptrx/paramname VALUE 'YN_PO_SUPPLIER_LOC_TYPE',
                 werks    TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_LOCATION',
                 werks_lt TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_LOC_TYPE',
                 eindt    TYPE /saptrx/paramname VALUE 'YN_PO_DELIVERY_DATE',
                 netwr    TYPE /saptrx/paramname VALUE 'YN_PO_NET_VALUE',
                 waers    TYPE /saptrx/paramname VALUE 'YN_PO_CURRENCY',
                 inco1    TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS',
                 incov    TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS_VERSION',
                 inco2_l  TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS_LOCATION',
                 item_num TYPE /saptrx/paramname VALUE 'YN_PO_HDR_ITM_LINE_COUNT',
                 ebelp    TYPE /saptrx/paramname VALUE 'YN_PO_HDR_ITM_NO',
               END OF cs_mapping.

    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters.

    METHODS fill_header_from_ekko_struct
      IMPORTING
        ir_ekko      TYPE REF TO data
      CHANGING
        cs_po_header TYPE ts_po_header
      RAISING
        cx_udm_message.

    METHODS fill_header_from_ekko_table
      IMPORTING
        iv_ebeln     TYPE ebeln
        ir_ekko      TYPE REF TO data
      CHANGING
        cs_po_header TYPE ts_po_header
      RAISING
        cx_udm_message.

    METHODS fill_header_from_ekpo_table
      IMPORTING
        iv_ebeln     TYPE ebeln
        ir_ekpo      TYPE REF TO data
      CHANGING
        cs_po_header TYPE ts_po_header
      RAISING
        cx_udm_message.

    METHODS fill_header_from_eket_table
      IMPORTING
        iv_ebeln     TYPE ebeln
        ir_ekpo      TYPE REF TO data
        ir_eket      TYPE REF TO data
      CHANGING
        cs_po_header TYPE ts_po_header
      RAISING
        cx_udm_message.

    METHODS fill_header_location_types
      CHANGING
        cs_po_header TYPE ts_po_header
      RAISING
        cx_udm_message.

    METHODS is_object_changed
      IMPORTING
        is_app_object    TYPE trxas_appobj_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_bo_reader_po_header IMPLEMENTATION.
  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
  ENDMETHOD.

  METHOD lif_bo_reader~check_relevance.
    " 1. Basic check of main table which shall be following
    "    the AOT configuration
    " 2. Check that only 1 PO type is relevance for GTT,
    "    which could be the standard PO type: NB
    " 3. If it’s CREATING PO, always flag TRUE
    " 4. If it’s UPDATING PO, check whether there is any
    "     change for all the above fields or not, comparing
    "     their NEW / OLD value pairs
    " 5. Don’t need to consider DELETING PO, which will be
    "    considered by standard logic of EM framework and
    "    extractors cannot impact this case

    rv_result   = lif_ef_constants=>cs_condition-false.

    " is_app_object-maintabdef = lif_pof_constants=>cs_tabledef-po_header_new AND
    IF lcl_po_tools=>is_appropriate_po_type( ir_ekko = is_app_object-maintabref ) = abap_true AND
       is_object_changed( is_app_object = is_app_object ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN lif_ef_constants=>cs_change_mode-insert.
          rv_result   = lif_ef_constants=>cs_condition-true.
        WHEN lif_ef_constants=>cs_change_mode-update OR
             lif_ef_constants=>cs_change_mode-undefined.
          rv_result   = lcl_tools=>are_structures_different(
                          ir_data1  = lif_bo_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = lif_bo_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD lif_bo_reader~get_data.
    FIELD-SYMBOLS: <ls_header>      TYPE ts_po_header.

    rr_data   = NEW ts_po_header( ).

    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_ekko_struct(
      EXPORTING
        ir_ekko       = is_app_object-maintabref
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_from_ekpo_table(
      EXPORTING
        iv_ebeln      = <ls_header>-ebeln
        ir_ekpo       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_item_new )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_from_eket_table(
      EXPORTING
        iv_ebeln      = <ls_header>-ebeln
        ir_ekpo       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_item_new )
        ir_eket       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_sched_new )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_po_header  = <ls_header> ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_field_parameter.
    CASE iv_parameter.
      WHEN lif_ef_constants=>cs_parameter_id-key_field.
        rv_result   = boolc( iv_field_name = cs_mapping-item_num ).
      WHEN OTHERS.
        CLEAR: rv_result.
    ENDCASE.
  ENDMETHOD.

  METHOD lif_bo_reader~get_mapping_structure.
    rr_data   = REF #( cs_mapping ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_track_id_data.
    MESSAGE e004(zpof_gtt) WITH 'LCL_BO_READER_PO_HEADER'
      INTO DATA(lv_dummy).
    lcl_tools=>throw_exception(
      iv_textid = lif_ef_constants=>cs_errors-stop_processing ).
  ENDMETHOD.

  METHOD fill_header_from_eket_table.
    DATA: lt_ebelp_rng TYPE RANGE OF ekpo-ebelp,
          lv_eindt_max TYPE eket-eindt,
          lv_eindt_set TYPE abap_bool VALUE abap_false.

    FIELD-SYMBOLS: <lt_ekpo> TYPE lif_app_types=>tt_uekpo,
                   <ls_ekpo> TYPE lif_app_types=>ts_uekpo,
                   <lt_eket> TYPE lif_app_types=>tt_ueket,
                   <ls_eket> TYPE lif_app_types=>ts_ueket.


    CLEAR: cs_po_header-eindt.

    ASSIGN ir_ekpo->* TO <lt_ekpo>.
    ASSIGN ir_eket->* TO <lt_eket>.

    IF <lt_ekpo> IS ASSIGNED AND
       <lt_eket> IS ASSIGNED.
      CLEAR cs_po_header-eindt.

      " Preparation of Active Items List
      LOOP AT <lt_ekpo> ASSIGNING <ls_ekpo>
        WHERE ebeln  = iv_ebeln
          AND loekz <> lif_app_constants=>cs_loekz-deleted.

        lt_ebelp_rng  = VALUE #( BASE lt_ebelp_rng
                                 ( low    = <ls_ekpo>-ebelp
                                   option = 'EQ'
                                   sign   = 'I' ) ).
      ENDLOOP.

      " Latest Delivery Date in schedule lines per item,
      " keep empty in case of different date on item level
      LOOP AT <lt_eket> ASSIGNING <ls_eket>
        WHERE ebeln  = iv_ebeln
          AND ebelp IN lt_ebelp_rng
        GROUP BY ( ebeln = <ls_eket>-ebeln
                   ebelp = <ls_eket>-ebelp )
        ASCENDING
        ASSIGNING FIELD-SYMBOL(<ls_eket_group>).

        CLEAR: lv_eindt_max.

        LOOP AT GROUP <ls_eket_group> ASSIGNING FIELD-SYMBOL(<ls_eket_items>).
          lv_eindt_max    = COND #( WHEN <ls_eket_items>-eindt > lv_eindt_max
                                      THEN <ls_eket_items>-eindt
                                      ELSE lv_eindt_max ).
        ENDLOOP.

        IF lv_eindt_set = abap_false.
          cs_po_header-eindt  = lv_eindt_max.
          lv_eindt_set        = abap_true.
        ELSEIF cs_po_header-eindt <> lv_eindt_max.
          CLEAR: cs_po_header-eindt.
          EXIT.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKET' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_header_location_types.
    cs_po_header-lifnr_lt  = lif_ef_constants=>cs_loc_types-supplier.
    cs_po_header-werks_lt  = lif_ef_constants=>cs_loc_types-plant.
  ENDMETHOD.

  METHOD fill_header_from_ekko_struct.
    FIELD-SYMBOLS: <ls_ekko>  TYPE any.

    ASSIGN ir_ekko->* TO <ls_ekko>.

    IF <ls_ekko> IS ASSIGNED.
      MOVE-CORRESPONDING <ls_ekko> TO cs_po_header.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKKO' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_header_from_ekko_table.
    DATA: lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <lt_ekko>  TYPE ANY TABLE,
                   <ls_ekko>  TYPE any,
                   <lv_ebeln> TYPE any.

    CLEAR: cs_po_header-werks, cs_po_header-netwr.

    ASSIGN ir_ekko->* TO <lt_ekko>.

    IF <lt_ekko> IS ASSIGNED.
      LOOP AT <lt_ekko> ASSIGNING <ls_ekko>.
        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_ekko> TO <lv_ebeln>.

        IF <lv_ebeln> IS ASSIGNED.
          " is it a record I need?
          IF <lv_ebeln>  = iv_ebeln.
            MOVE-CORRESPONDING <ls_ekko> TO cs_po_header.
            EXIT.
          ENDIF.
        ELSE.
          MESSAGE e001(zpof_gtt) WITH 'EBELN' 'EKKO' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKKO' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_header_from_ekpo_table.
    DATA: lv_item_num TYPE tv_item_num VALUE 0,
          lv_fname    TYPE char5,
          lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <lt_ekpo>  TYPE ANY TABLE,
                   <ls_ekpo>  TYPE any,
                   <lv_ebeln> TYPE any,
                   <lv_ebelp> TYPE any,
                   <lv_loekz> TYPE any,
                   <lv_werks> TYPE any,
                   <lv_netwr> TYPE any.

    CLEAR: cs_po_header-werks,
           cs_po_header-netwr,
           cs_po_header-item_num[],
           cs_po_header-ebelp[].

    ASSIGN ir_ekpo->* TO <lt_ekpo>.

    IF <lt_ekpo> IS ASSIGNED.
      LOOP AT <lt_ekpo> ASSIGNING <ls_ekpo>.
        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_ekpo> TO <lv_ebeln>.
        ASSIGN COMPONENT 'EBELP' OF STRUCTURE <ls_ekpo> TO <lv_ebelp>.
        ASSIGN COMPONENT 'LOEKZ' OF STRUCTURE <ls_ekpo> TO <lv_loekz>.
        ASSIGN COMPONENT 'WERKS' OF STRUCTURE <ls_ekpo> TO <lv_werks>.
        ASSIGN COMPONENT 'NETWR' OF STRUCTURE <ls_ekpo> TO <lv_netwr>.

        IF <lv_ebeln> IS ASSIGNED AND
           <lv_ebelp> IS ASSIGNED AND
           <lv_loekz> IS ASSIGNED AND
           <lv_werks> IS ASSIGNED AND
           <lv_netwr> IS ASSIGNED.

          IF <lv_ebeln>  = iv_ebeln AND
             lcl_po_tools=>is_appropriate_po_item( ir_ekpo = REF #( <ls_ekpo> ) ) = abap_true.

            " Add PO Item number into result table
            ADD 1 TO lv_item_num.
            APPEND lv_item_num TO cs_po_header-item_num.

            " Add composition (PO Number PO Item position) into result table
            APPEND |{ <lv_ebeln> }{ <lv_ebelp> }| TO cs_po_header-ebelp.

            " Is item not deleted (active or blocked)?
            IF <lv_loekz> <> lif_app_constants=>cs_loekz-deleted.
              " Plant ID, keep empty in case of different receiving plants on item level
              cs_po_header-werks  = COND #( WHEN sy-tabix = 1 OR
                                                 <lv_werks>  = cs_po_header-werks
                                              THEN <lv_werks> ).

              " Sum of net values on item level
              ADD <lv_netwr> TO cs_po_header-netwr.
            ENDIF.
          ENDIF.
        ELSE.
          lv_fname  = COND #( WHEN <lv_ebeln> IS NOT ASSIGNED THEN 'EBELN'
                              WHEN <lv_ebelp> IS NOT ASSIGNED THEN 'EBELP'
                              WHEN <lv_loekz> IS NOT ASSIGNED THEN 'LOEKZ'
                              WHEN <lv_werks> IS NOT ASSIGNED THEN 'WERKS'
                                ELSE 'NETWR' ).
          MESSAGE e001(zpof_gtt) WITH lv_fname 'EKPO' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.

      cs_po_header-netwr  = lcl_tools=>convert_to_external_amount(
                              iv_currency = cs_po_header-waers
                              iv_internal = cs_po_header-netwr ).
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKPO' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_bo_reader~get_data_old.
    FIELD-SYMBOLS: <ls_header>      TYPE ts_po_header.

    rr_data   = NEW ts_po_header( ).
    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_ekko_table(
      EXPORTING
        iv_ebeln      = CONV #( lcl_tools=>get_field_of_structure(
                                  ir_struct_data = is_app_object-maintabref
                                  iv_field_name  = 'EBELN'
                                ) )
        ir_ekko       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_header_old )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_from_ekpo_table(
      EXPORTING
        iv_ebeln      = <ls_header>-ebeln
        ir_ekpo       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_item_old )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_from_eket_table(
      EXPORTING
        iv_ebeln      = <ls_header>-ebeln
        ir_ekpo       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_item_old )
        ir_eket       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_sched_old )
      CHANGING
        cs_po_header  = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_po_header = <ls_header> ).
  ENDMETHOD.

  METHOD is_object_changed.
    rv_result   = lcl_tools=>is_object_changed(
                    is_app_object    = is_app_object
                    io_ef_parameters = mo_ef_parameters
                    it_check_tables  = VALUE #( ( lif_app_constants=>cs_tabledef-po_item_new )
                                                ( lif_app_constants=>cs_tabledef-po_item_old )
                                                ( lif_app_constants=>cs_tabledef-po_sched_new )
                                                ( lif_app_constants=>cs_tabledef-po_sched_old ) )
                    iv_key_field = 'EBELN'
                    iv_upd_field = 'KZ' ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_bo_reader_po_item DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_bo_reader.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters.

  PRIVATE SECTION.
    TYPES: tv_ebelp_txt  TYPE char5, "char15
           tv_deliv_num  TYPE i,
           tv_deliv_item TYPE char20,
           tv_sched_num  TYPE i.

    TYPES: tt_deliv_num   TYPE STANDARD TABLE OF tv_deliv_num
                            WITH EMPTY KEY,
           tt_deliv_item  TYPE STANDARD TABLE OF tv_deliv_item
                            WITH EMPTY KEY,
           tt_sched_num   TYPE STANDARD TABLE OF tv_sched_num
                            WITH EMPTY KEY,
           tt_sched_eindt TYPE STANDARD TABLE OF eket-eindt
                            WITH EMPTY KEY,
           tt_sched_menge TYPE STANDARD TABLE OF eket-menge
                            WITH EMPTY KEY,
           tt_sched_meins TYPE STANDARD TABLE OF ekpo-meins
                            WITH EMPTY KEY.

    TYPES: BEGIN OF ts_po_item,
             ebeln       TYPE ekpo-ebeln,
             ebelp       TYPE tv_ebelp_txt,    "used char type to avoid leading zeros deletion
             lifnr       TYPE ekko-lifnr,
             lifnr_lt    TYPE /saptrx/loc_id_type,
             werks       TYPE ekpo-werks,
             werks_lt    TYPE /saptrx/loc_id_type,
             eindt       TYPE eket-eindt,
             matnr       TYPE ekpo-matnr,
             menge       TYPE ekpo-menge,
             meins       TYPE ekpo-meins,
             txz01       TYPE ekpo-txz01,
             ntgew       TYPE ekpo-ntgew,
             gewei       TYPE ekpo-gewei,
             brgew       TYPE ekpo-brgew,
             volum       TYPE ekpo-volum,
             voleh       TYPE ekpo-voleh,
             netwr       TYPE ekpo-netwr,
             waers       TYPE ekko-waers,
             inco1       TYPE ekpo-inco1,
             incov       TYPE ekko-incov,
             inco2_l     TYPE ekpo-inco2_l,
             deliv_num   TYPE tt_deliv_num,
             deliv_item  TYPE tt_deliv_item,
             sched_num   TYPE tt_sched_num,
             sched_eindt TYPE tt_sched_eindt,
             sched_menge TYPE tt_sched_menge,
             sched_meins TYPE tt_sched_meins,
           END OF ts_po_item.

    CONSTANTS: BEGIN OF cs_mapping,
                 ebeln       TYPE /saptrx/paramname VALUE 'YN_PO_NUMBER',
                 ebelp       TYPE /saptrx/paramname VALUE 'YN_PO_ITEM',
                 lifnr       TYPE /saptrx/paramname VALUE 'YN_PO_SUPPLIER_ID',
                 lifnr_lt    TYPE /saptrx/paramname VALUE 'YN_PO_SUPPLIER_LOC_TYPE',
                 werks       TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_LOCATION',
                 werks_lt    TYPE /saptrx/paramname VALUE 'YN_PO_RECEIVING_LOC_TYPE',
                 eindt       TYPE /saptrx/paramname VALUE 'YN_PO_DELIVERY_DATE',
                 matnr       TYPE /saptrx/paramname VALUE 'YN_PO_MATERIAL_ID',
                 menge       TYPE /saptrx/paramname VALUE 'YN_PO_ORDER_QUANTITY',
                 meins       TYPE /saptrx/paramname VALUE 'YN_PO_UNIT_OF_MEASURE',
                 txz01       TYPE /saptrx/paramname VALUE 'YN_PO_MATERIAL_DESCR',
                 ntgew       TYPE /saptrx/paramname VALUE 'YN_PO_NET_WEIGHT',
                 gewei       TYPE /saptrx/paramname VALUE 'YN_PO_WEIGHT_UOM',
                 brgew       TYPE /saptrx/paramname VALUE 'YN_PO_GROSS_WEIGHT',
                 volum       TYPE /saptrx/paramname VALUE 'YN_PO_VOLUME',
                 voleh       TYPE /saptrx/paramname VALUE 'YN_PO_VOLUME_UOM',
                 netwr       TYPE /saptrx/paramname VALUE 'YN_PO_NET_VALUE',
                 waers       TYPE /saptrx/paramname VALUE 'YN_PO_CURRENCY',
                 inco1       TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS',
                 incov       TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS_VERSION',
                 inco2_l     TYPE /saptrx/paramname VALUE 'YN_PO_INCOTERMS_LOCATION',
                 deliv_num   TYPE /saptrx/paramname VALUE 'YN_DL_HDR_ITM_LINE_COUNT',
                 deliv_item  TYPE /saptrx/paramname VALUE 'YN_DL_HDR_ITM_NO',
                 sched_num   TYPE /saptrx/paramname VALUE 'YN_PO_ITM_SCHED_LINE_COUNT',
                 sched_eindt TYPE /saptrx/paramname VALUE 'YN_PO_ITM_SCHED_DATE',
                 sched_menge TYPE /saptrx/paramname VALUE 'YN_PO_ITM_SCHED_QUANTITY',
                 sched_meins TYPE /saptrx/paramname VALUE 'YN_PO_ITM_SCHED_QUANT_UOM',
               END OF cs_mapping.

    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters.

    METHODS fill_item_from_ekko_struct
      IMPORTING
        ir_ekko    TYPE REF TO data
      CHANGING
        cs_po_item TYPE ts_po_item
      RAISING
        cx_udm_message.

    METHODS fill_item_from_ekko_table
      IMPORTING
        ir_ekko    TYPE REF TO data
        iv_ebeln   TYPE ebeln
      CHANGING
        cs_po_item TYPE ts_po_item
      RAISING
        cx_udm_message.

    METHODS fill_item_from_ekpo_struct
      IMPORTING
        ir_ekpo    TYPE REF TO data
      CHANGING
        cs_po_item TYPE ts_po_item
      RAISING
        cx_udm_message.

    METHODS fill_item_from_eket_table
      IMPORTING
        ir_ekpo    TYPE REF TO data
        ir_eket    TYPE REF TO data
      CHANGING
        cs_po_item TYPE ts_po_item
      RAISING
        cx_udm_message.

    METHODS fill_item_location_types
      CHANGING
        cs_po_item TYPE ts_po_item
      RAISING
        cx_udm_message.

    METHODS get_ekpo_record
      IMPORTING
        ir_ekpo        TYPE REF TO data
        iv_ebeln       TYPE ebeln
        iv_ebelp       TYPE ebelp
      RETURNING
        VALUE(rv_ekpo) TYPE REF TO data
      RAISING
        cx_udm_message.

    METHODS is_object_changed
      IMPORTING
        is_app_object    TYPE trxas_appobj_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_bo_reader_po_item IMPLEMENTATION.
  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
  ENDMETHOD.

  METHOD fill_item_from_eket_table.
    TYPES: tt_eket    TYPE STANDARD TABLE OF ueket.

    DATA: lv_sched_num   TYPE tv_sched_num VALUE 0,
          lv_sched_vrkme TYPE lips-vrkme.

    FIELD-SYMBOLS: <lt_eket>  TYPE tt_eket.

    DATA(lv_ebeln) = CONV ebeln( lcl_tools=>get_field_of_structure(
                                   ir_struct_data = ir_ekpo
                                   iv_field_name  = 'EBELN' ) ).
    DATA(lv_ebelp) = CONV ebelp( lcl_tools=>get_field_of_structure(
                                   ir_struct_data = ir_ekpo
                                   iv_field_name  = 'EBELP' ) ).
    DATA(lv_meins) = CONV vrkme( lcl_tools=>get_field_of_structure(
                                   ir_struct_data = ir_ekpo
                                   iv_field_name  = 'MEINS' ) ).
    CLEAR: cs_po_item-eindt,
           cs_po_item-sched_num[],
           cs_po_item-sched_eindt[],
           cs_po_item-sched_menge[],
           cs_po_item-sched_meins[].

    ASSIGN ir_eket->* TO <lt_eket>.

    IF <lt_eket> IS ASSIGNED.
      LOOP AT <lt_eket> ASSIGNING FIELD-SYMBOL(<ls_eket>)
        WHERE ebeln = lv_ebeln
          AND ebelp = lv_ebelp.

        " Latest Delivery Date in schedule lines per item, keep empty in case of
        " different date on item level
        cs_po_item-eindt  = COND #( WHEN <ls_eket>-eindt > cs_po_item-eindt
                                      THEN <ls_eket>-eindt
                                      ELSE cs_po_item-eindt ).

        " add row to schedule line table
        ADD 1 TO lv_sched_num.
        APPEND lv_sched_num    TO cs_po_item-sched_num.
        APPEND <ls_eket>-eindt TO cs_po_item-sched_eindt.
        APPEND <ls_eket>-menge TO cs_po_item-sched_menge.
        APPEND lv_meins        TO cs_po_item-sched_meins.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKET' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_item_from_ekko_struct.
    DATA: lv_fname TYPE char5,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <ls_ekko>  TYPE any,
                   <lv_lifnr> TYPE ekko-lifnr,
                   <lv_waers> TYPE ekko-waers,
                   <lv_incov> TYPE ekko-incov.

    ASSIGN ir_ekko->* TO <ls_ekko>.

    IF <ls_ekko> IS ASSIGNED.
      ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <ls_ekko> TO <lv_lifnr>.
      ASSIGN COMPONENT 'WAERS' OF STRUCTURE <ls_ekko> TO <lv_waers>.
      ASSIGN COMPONENT 'INCOV' OF STRUCTURE <ls_ekko> TO <lv_incov>.

      IF <lv_lifnr> IS ASSIGNED AND
         <lv_waers> IS ASSIGNED AND
         <lv_incov> IS ASSIGNED.

        cs_po_item-lifnr  = <lv_lifnr>.
        cs_po_item-waers  = <lv_waers>.
        cs_po_item-incov  = <lv_incov>.

      ELSE.
        lv_fname  = COND #( WHEN <lv_lifnr> IS NOT ASSIGNED THEN 'LIFNR'
                            WHEN <lv_incov> IS NOT ASSIGNED THEN 'INCOV'
                              ELSE 'WAERS' ).
        MESSAGE e001(zpof_gtt) WITH lv_fname 'EKKO' INTO lv_dummy.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKKO' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_item_from_ekko_table.
    DATA: lv_fname TYPE char5,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <lt_ekko>  TYPE ANY TABLE,
                   <ls_ekko>  TYPE any,
                   <lv_ebeln> TYPE ekko-ebeln,
                   <lv_lifnr> TYPE ekko-lifnr,
                   <lv_incov> TYPE ekko-incov,
                   <lv_waers> TYPE ekko-waers.

    CLEAR: cs_po_item-lifnr, cs_po_item-waers.

    ASSIGN ir_ekko->* TO <lt_ekko>.

    IF <lt_ekko> IS ASSIGNED.
      LOOP AT <lt_ekko> ASSIGNING <ls_ekko>.
        ASSIGN COMPONENT 'EBELN' OF STRUCTURE <ls_ekko> TO <lv_ebeln>.
        ASSIGN COMPONENT 'LIFNR' OF STRUCTURE <ls_ekko> TO <lv_lifnr>.
        ASSIGN COMPONENT 'INCOV' OF STRUCTURE <ls_ekko> TO <lv_incov>.
        ASSIGN COMPONENT 'WAERS' OF STRUCTURE <ls_ekko> TO <lv_waers>.

        IF <lv_ebeln> IS ASSIGNED AND
           <lv_lifnr> IS ASSIGNED AND
           <lv_incov> IS ASSIGNED AND
           <lv_waers> IS ASSIGNED.

          IF <lv_ebeln> = iv_ebeln.
            cs_po_item-lifnr  = <lv_lifnr>.
            cs_po_item-waers  = <lv_waers>.
            cs_po_item-incov  = <lv_incov>.
          ENDIF.

        ELSE.
          lv_fname  = COND #( WHEN <lv_ebeln> IS NOT ASSIGNED THEN 'EBELN'
                              WHEN <lv_lifnr> IS NOT ASSIGNED THEN 'LIFNR'
                              WHEN <lv_incov> IS NOT ASSIGNED THEN 'INCOV'
                                ELSE 'WAERS' ).
          MESSAGE e001(zpof_gtt) WITH lv_fname 'EKKO' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKKO' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_item_from_ekpo_struct.
    FIELD-SYMBOLS: <ls_ekpo>  TYPE any.

    ASSIGN ir_ekpo->* TO <ls_ekpo>.

    IF <ls_ekpo> IS ASSIGNED.
      MOVE-CORRESPONDING <ls_ekpo> TO cs_po_item.

      cs_po_item-netwr  = lcl_tools=>convert_to_external_amount(
                            iv_currency = cs_po_item-waers
                            iv_internal = cs_po_item-netwr ).
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKPO' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_item_location_types.
    cs_po_item-lifnr_lt  = lif_ef_constants=>cs_loc_types-supplier.
    cs_po_item-werks_lt  = lif_ef_constants=>cs_loc_types-plant.
  ENDMETHOD.

  METHOD get_ekpo_record.
    TYPES: tt_ekpo    TYPE STANDARD TABLE OF uekpo.

    DATA: lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <lt_ekpo> TYPE tt_ekpo.

    ASSIGN ir_ekpo->* TO <lt_ekpo>.
    IF <lt_ekpo> IS ASSIGNED.
      READ TABLE <lt_ekpo> ASSIGNING FIELD-SYMBOL(<ls_ekpo>)
        WITH KEY ebeln = iv_ebeln
                 ebelp = iv_ebelp.
      IF sy-subrc = 0.
        rv_ekpo   = REF #( <ls_ekpo> ).
      ELSE.
        MESSAGE e005(zpof_gtt) WITH 'EKPO' |{ iv_ebeln }{ iv_ebelp }|
          INTO lv_dummy.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKPO' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_bo_reader~check_relevance.
    " 1. Basic check of main table which shall be following
    "    the AOT configuration
    " 2. Check that only 1 PO type is relevance for GTT,
    "    which could be the standard PO type: NB
    " 3. If it’s CREATING PO, always flag TRUE
    " 4. If it’s UPDATING PO, check whether there is any
    "     change for all the above fields or not, comparing
    "     their NEW / OLD value pairs
    " 5. Don’t need to consider DELETING PO, which will be
    "    considered by standard logic of EM framework and
    "    extractors cannot impact this case

    rv_result   = lif_ef_constants=>cs_condition-false.

    " is_app_object-maintabdef = lif_pof_constants=>cs_tabledef-po_item_new AND
    IF lcl_po_tools=>is_appropriate_po_type( ir_ekko = is_app_object-mastertabref ) = abap_true AND
       lcl_po_tools=>is_appropriate_po_item( ir_ekpo = is_app_object-maintabref ) = abap_true AND
       is_object_changed( is_app_object = is_app_object ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN lif_ef_constants=>cs_change_mode-insert.
          rv_result   = lif_ef_constants=>cs_condition-true.
        WHEN lif_ef_constants=>cs_change_mode-update OR
             lif_ef_constants=>cs_change_mode-undefined.
          rv_result   = lcl_tools=>are_structures_different(
                          ir_data1  = lif_bo_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = lif_bo_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD lif_bo_reader~get_data.
    FIELD-SYMBOLS: <ls_item>      TYPE ts_po_item.

    rr_data   = NEW ts_po_item( ).

    ASSIGN rr_data->* TO <ls_item>.

    fill_item_from_ekko_struct(
      EXPORTING
        ir_ekko       = is_app_object-mastertabref
      CHANGING
        cs_po_item    = <ls_item> ).

    fill_item_from_ekpo_struct(
      EXPORTING
        ir_ekpo       = is_app_object-maintabref
      CHANGING
        cs_po_item    = <ls_item> ).

    fill_item_from_eket_table(
      EXPORTING
        ir_ekpo       = is_app_object-maintabref
        ir_eket       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_sched_new )
      CHANGING
        cs_po_item  = <ls_item> ).

    fill_item_location_types(
      CHANGING
        cs_po_item = <ls_item> ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_field_parameter.
    CASE iv_parameter.
      WHEN lif_ef_constants=>cs_parameter_id-key_field.
        rv_result   = boolc( iv_field_name = cs_mapping-deliv_num OR
                             iv_field_name = cs_mapping-sched_num ).
      WHEN OTHERS.
        CLEAR: rv_result.
    ENDCASE.
  ENDMETHOD.

  METHOD lif_bo_reader~get_mapping_structure.
    rr_data   = REF #( cs_mapping ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_track_id_data.
    FIELD-SYMBOLS: <ls_ekpo> TYPE uekpo,
                   <lt_ekes> TYPE lif_app_types=>tt_uekes.

    DATA(lv_tzone)  = lcl_tools=>get_system_time_zone( ).

    DATA(lr_ekes)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef = lif_app_constants=>cs_tabledef-po_vend_conf_new ).

    ASSIGN is_app_object-maintabref->* TO <ls_ekpo>.

    CLEAR: et_track_id_data[].

    IF <ls_ekpo> IS ASSIGNED.
      et_track_id_data  = VALUE #( (
        appsys      = mo_ef_parameters->get_appsys( )
        appobjtype  = is_app_object-appobjtype
        appobjid    = is_app_object-appobjid
        trxcod      = lif_app_constants=>cs_trxcod-po_position
        trxid       = |{ <ls_ekpo>-ebeln }{ <ls_ekpo>-ebelp }|
        start_date  = lcl_tools=>get_system_date_time( )
        end_date    = lif_ef_constants=>cv_max_end_date
        timzon      = lv_tzone
        msrid       = space
      ) ).

      IF <ls_ekpo>-kz = lif_ef_constants=>cs_change_mode-insert.
        et_track_id_data = VALUE #( BASE et_track_id_data (
          appsys      = mo_ef_parameters->get_appsys( )
          appobjtype  = is_app_object-appobjtype
          appobjid    = is_app_object-appobjid
          trxcod      = lif_app_constants=>cs_trxcod-po_number
          trxid       = |{ <ls_ekpo>-ebeln }|
          start_date  = lcl_tools=>get_system_date_time( )
          end_date    = lif_ef_constants=>cv_max_end_date
          timzon      = lv_tzone
          msrid       = space
        ) ).
      ENDIF.

    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'EKPO' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_bo_reader~get_data_old.
    FIELD-SYMBOLS: <ls_item>      TYPE ts_po_item.

    DATA(lv_ebeln)  = CONV ebeln( lcl_tools=>get_field_of_structure(
                                    ir_struct_data = is_app_object-maintabref
                                    iv_field_name  = 'EBELN' ) ).
    DATA(lv_ebelp)  = CONV ebelp( lcl_tools=>get_field_of_structure(
                                    ir_struct_data = is_app_object-maintabref
                                    iv_field_name  = 'EBELP' ) ).
    DATA(lr_ekpo)   = get_ekpo_record(
                        ir_ekpo  = mo_ef_parameters->get_appl_table(
                                     iv_tabledef = lif_app_constants=>cs_tabledef-po_item_old )
                        iv_ebeln = lv_ebeln
                        iv_ebelp = lv_ebelp ).

    rr_data   = NEW ts_po_item( ).
    ASSIGN rr_data->* TO <ls_item>.

    fill_item_from_ekko_table(
      EXPORTING
        iv_ebeln      = lv_ebeln
        ir_ekko       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_header_old )
      CHANGING
        cs_po_item  = <ls_item> ).

    fill_item_from_ekpo_struct(
      EXPORTING
        ir_ekpo    = lr_ekpo
      CHANGING
        cs_po_item = <ls_item> ).

    fill_item_from_eket_table(
      EXPORTING
        ir_ekpo       = lr_ekpo
        ir_eket       = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_sched_old )
      CHANGING
        cs_po_item  = <ls_item> ).

    fill_item_location_types(
      CHANGING
        cs_po_item = <ls_item> ).
  ENDMETHOD.

  METHOD is_object_changed.
    rv_result   = lcl_tools=>is_object_changed(
                    is_app_object    = is_app_object
                    io_ef_parameters = mo_ef_parameters
                    it_check_tables  = VALUE #( ( lif_app_constants=>cs_tabledef-po_sched_new )
                                                ( lif_app_constants=>cs_tabledef-po_sched_old ) )
                    iv_key_field = 'EBELN'
                    iv_upd_field = 'KZ' ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_bo_reader_dl_header DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_bo_reader.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters.

  PRIVATE SECTION.
    TYPES: tv_item_num TYPE i,
           tt_item_num TYPE STANDARD TABLE OF tv_item_num WITH EMPTY KEY.

    TYPES: tv_item_posnr TYPE char20,
           tt_item_posnr TYPE STANDARD TABLE OF tv_item_posnr WITH EMPTY KEY.

    TYPES: BEGIN OF ts_dl_header,
             vbeln        TYPE likp-vbeln,
             lifnr        TYPE likp-lifnr,
             lifnr_lt     TYPE /saptrx/loc_id_type,
             werks        TYPE likp-werks,
             werks_lt     TYPE /saptrx/loc_id_type,
             bldat        TYPE likp-bldat,
             lfdat        TYPE likp-lfdat,
             btgew        TYPE likp-btgew,
             ntgew        TYPE likp-ntgew,
             gewei        TYPE likp-gewei,
             volum        TYPE likp-volum,
             voleh        TYPE likp-voleh,
             lgnum        TYPE likp-lgnum,
             lgtor        TYPE likp-lgtor,
             lgnum_txt    TYPE /saptrx/paramval200,
             bolnr        TYPE likp-bolnr,
             proli        TYPE likp-proli,
             incov        TYPE likp-incov,
             inco1        TYPE likp-inco1,
             inco2_l      TYPE likp-inco2_l,
           END OF ts_dl_header.

    CONSTANTS: BEGIN OF cs_mapping,
                 " Header section
                 vbeln        TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY',
                 lifnr        TYPE /saptrx/paramname VALUE 'YN_DL_VENDOR_ID',
                 lifnr_lt     TYPE /saptrx/paramname VALUE 'YN_DL_VENDOR_LOC_TYPE',
                 werks        TYPE /saptrx/paramname VALUE 'YN_DL_RECEIVING_LOCATION',
                 werks_lt     TYPE /saptrx/paramname VALUE 'YN_DL_RECEIVING_LOC_TYPE',
                 bldat        TYPE /saptrx/paramname VALUE 'YN_DL_DOCUMENT_DATE',
                 lfdat        TYPE /saptrx/paramname VALUE 'YN_DL_PLANNED_DLV_DATE',
                 btgew        TYPE /saptrx/paramname VALUE 'YN_DL_TOTAL_WEIGHT',
                 ntgew        TYPE /saptrx/paramname VALUE 'YN_DL_NET_WEIGHT',
                 gewei        TYPE /saptrx/paramname VALUE 'YN_DL_WEIGHT_UNITS',
                 volum        TYPE /saptrx/paramname VALUE 'YN_DL_VOLUME',
                 voleh        TYPE /saptrx/paramname VALUE 'YN_DL_VOLUME_UNITS',
                 lgnum        TYPE /saptrx/paramname VALUE 'YN_DL_WAREHOUSE',
                 lgnum_txt    TYPE /saptrx/paramname VALUE 'YN_DL_WAREHOUSE_DESC',
                 lgtor        TYPE /saptrx/paramname VALUE 'YN_DL_DOOR',
                 bolnr        TYPE /saptrx/paramname VALUE 'YN_DL_BILL_OF_LADING',
                 proli        TYPE /saptrx/paramname VALUE 'YN_DL_DANGEROUS_GOODS',
                 incov        TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS_VERSION',
                 inco1        TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS',
                 inco2_l      TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS_LOCATION',
               END OF cs_mapping.

    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters.

    METHODS fill_header_from_likp_struct
      IMPORTING
        ir_likp      TYPE REF TO data
      CHANGING
        cs_dl_header TYPE ts_dl_header
      RAISING
        cx_udm_message.

    METHODS fill_header_from_lips_table
      IMPORTING
        ir_lips_new  TYPE REF TO data
        ir_lips_old  TYPE REF TO data OPTIONAL
        iv_vbeln     TYPE vbeln_vl
      CHANGING
        cs_dl_header TYPE ts_dl_header
      RAISING
        cx_udm_message.

    METHODS fill_header_location_types
      CHANGING
        cs_dl_header TYPE ts_dl_header.

    METHODS get_likp_struct_old
      IMPORTING
        is_app_object  TYPE trxas_appobj_ctab_wa
        iv_vbeln       TYPE vbeln_vl
      RETURNING
        VALUE(rr_likp) TYPE REF TO data
      RAISING
        cx_udm_message.

    METHODS is_object_changed
      IMPORTING
        is_app_object    TYPE trxas_appobj_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_bo_reader_dl_header IMPLEMENTATION.
  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
  ENDMETHOD.

  METHOD fill_header_from_likp_struct.
    FIELD-SYMBOLS: <ls_likp>  TYPE likpvb.

    ASSIGN ir_likp->* TO <ls_likp>.

    IF <ls_likp> IS ASSIGNED.
      MOVE-CORRESPONDING <ls_likp> TO cs_dl_header.

      cs_dl_header-proli    = boolc( cs_dl_header-proli IS NOT INITIAL ).

      IF <ls_likp>-lgnum IS NOT INITIAL AND
         <ls_likp>-lgtor IS NOT INITIAL.

        TRY.
            cs_dl_header-lgnum_txt = lcl_dl_tools=>get_door_description(
              EXPORTING
                iv_lgnum = <ls_likp>-lgnum
                iv_lgtor = <ls_likp>-lgtor ).
          CATCH cx_udm_message.
        ENDTRY.
      ENDIF.

    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIKP' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_header_from_lips_table.
    TYPES: tt_lips  TYPE STANDARD TABLE OF lipsvb,
           tt_posnr TYPE SORTED TABLE OF posnr_vl
                           WITH UNIQUE KEY table_line.

    DATA: lt_posnr TYPE tt_posnr,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <lt_lips> TYPE tt_lips,
                   <ls_lips> TYPE lipsvb.

    ASSIGN ir_lips_new->* TO <lt_lips>.

    " prepare positions list
    IF <lt_lips> IS ASSIGNED.
      " collect NEW records with appropriate item type
      LOOP AT <lt_lips> ASSIGNING <ls_lips>
        WHERE vbeln = iv_vbeln.

        IF lcl_dl_tools=>is_appropriate_dl_item(
             ir_struct = REF #( <ls_lips> ) ) = abap_true.
          INSERT <ls_lips>-posnr INTO TABLE lt_posnr.

          cs_dl_header-werks  = COND #( WHEN cs_dl_header-werks IS INITIAL
                                          THEN <ls_lips>-werks
                                          ELSE cs_dl_header-werks ).
        ENDIF.
      ENDLOOP.

      " process old table
      IF ir_lips_old IS BOUND.
        ASSIGN ir_lips_old->* TO <lt_lips>.
        IF sy-subrc = 0.
          LOOP AT <lt_lips> ASSIGNING <ls_lips>
            WHERE vbeln = iv_vbeln.

            " add deleted records
            IF <ls_lips>-updkz  = lif_ef_constants=>cs_change_mode-insert.
              INSERT <ls_lips>-posnr INTO TABLE lt_posnr.

              " remove inserted records
            ELSEIF <ls_lips>-updkz  = lif_ef_constants=>cs_change_mode-delete.
              READ TABLE lt_posnr TRANSPORTING NO FIELDS
                WITH KEY table_line = <ls_lips>-posnr.

              IF sy-subrc = 0.
                DELETE lt_posnr INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          MESSAGE e002(zpof_gtt) WITH 'LIPS OLD' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIPS NEW' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_header_location_types.
    cs_dl_header-lifnr_lt   = lif_ef_constants=>cs_loc_types-supplier.

    IF cs_dl_header-werks IS NOT INITIAL.
      cs_dl_header-werks_lt = lif_ef_constants=>cs_loc_types-plant.
    ENDIF.
  ENDMETHOD.

  METHOD get_likp_struct_old.
    " when header is unchanged, table 'DELIVERY_HEADER_OLD' is not populated
    " so maintab record is used as data source for header data
    TYPES: tt_likp TYPE STANDARD TABLE OF likpvb.

    FIELD-SYMBOLS: <lt_likp> TYPE tt_likp,
                   <ls_likp> TYPE likpvb.

    DATA(lr_likp)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef = lif_app_constants=>cs_tabledef-dl_header_old ).

    ASSIGN lr_likp->* TO <lt_likp>.

    IF <lt_likp> IS ASSIGNED.
      READ TABLE <lt_likp> ASSIGNING <ls_likp>
        WITH KEY vbeln = iv_vbeln.

      rr_likp   = COND #( WHEN sy-subrc = 0
                            THEN REF #( <ls_likp> )
                            ELSE is_app_object-maintabref ).
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIKP' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD is_object_changed.
    rv_result   = lcl_tools=>is_object_changed(
                    is_app_object    = is_app_object
                    io_ef_parameters = mo_ef_parameters
                    iv_key_field     = 'VBELN'
                    iv_upd_field     = 'UPDKZ' ).
  ENDMETHOD.

  METHOD lif_bo_reader~check_relevance.
    rv_result   = lif_ef_constants=>cs_condition-false.

    IF lcl_dl_tools=>is_appropriate_dl_type( ir_struct = is_app_object-maintabref ) = abap_true AND
       is_object_changed( is_app_object = is_app_object ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN lif_ef_constants=>cs_change_mode-insert.
          rv_result   = lif_ef_constants=>cs_condition-true.
        WHEN lif_ef_constants=>cs_change_mode-update OR
             lif_ef_constants=>cs_change_mode-undefined.
          rv_result   = lcl_tools=>are_structures_different(
                          ir_data1  = lif_bo_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = lif_bo_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD lif_bo_reader~get_data.
    FIELD-SYMBOLS: <ls_header> TYPE ts_dl_header.

    rr_data   = NEW ts_dl_header( ).

    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_likp_struct(
      EXPORTING
        ir_likp      = is_app_object-maintabref
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_from_lips_table(
      EXPORTING
        ir_lips_new  = mo_ef_parameters->get_appl_table(
                         iv_tabledef = lif_app_constants=>cs_tabledef-dl_item_new )
        ir_lips_old  = mo_ef_parameters->get_appl_table(
                         iv_tabledef = lif_app_constants=>cs_tabledef-dl_item_old )
        iv_vbeln     = <ls_header>-vbeln
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_dl_header = <ls_header> ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_data_old.

    FIELD-SYMBOLS: <ls_header> TYPE ts_dl_header.

    DATA(lv_vbeln)  = CONV vbeln_vl( lcl_tools=>get_field_of_structure(
                                       ir_struct_data = is_app_object-maintabref
                                       iv_field_name  = 'VBELN' ) ).

    rr_data   = NEW ts_dl_header( ).

    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_likp_struct(
      EXPORTING
        ir_likp      = get_likp_struct_old(
                         is_app_object = is_app_object
                         iv_vbeln      = lv_vbeln )
      CHANGING
        cs_dl_header = <ls_header> ).


    fill_header_from_lips_table(
      EXPORTING
        ir_lips_new  = mo_ef_parameters->get_appl_table(
                         iv_tabledef = lif_app_constants=>cs_tabledef-dl_item_new )
        ir_lips_old  = mo_ef_parameters->get_appl_table(
                         iv_tabledef = lif_app_constants=>cs_tabledef-dl_item_old )
        iv_vbeln     = <ls_header>-vbeln
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_dl_header = <ls_header> ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_field_parameter.
    CLEAR: rv_result.
  ENDMETHOD.

  METHOD lif_bo_reader~get_mapping_structure.
    rr_data   = REF #( cs_mapping ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_track_id_data.
    "another tip is that: for tracking ID type 'SHIPMENT_ORDER' of delivery header,
    "and for tracking ID type 'RESOURCE' of shipment header,
    "DO NOT enable START DATE and END DATE

    MESSAGE e004(zpof_gtt) WITH 'LCL_BO_READER_DL_HEADER'
      INTO DATA(lv_dummy).
    lcl_tools=>throw_exception(
      iv_textid = lif_ef_constants=>cs_errors-stop_processing ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_bo_reader_dl_item DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_bo_reader.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters.

  PRIVATE SECTION.
    TYPES: tv_posnr_txt TYPE char6,
           tv_po_item   TYPE char20.

    TYPES: BEGIN OF ts_dl_item,
             " Header section
             vbeln     TYPE lips-vbeln,
             posnr     TYPE tv_posnr_txt,      "used char type to avoid leading zeros deletion
             arktx     TYPE lips-arktx,
             matnr     TYPE lips-matnr,
             lfimg     TYPE lips-lfimg,
             vrkme     TYPE lips-vrkme,
             " Departure section
             lifnr     TYPE likp-lifnr,
             lifnr_lt  TYPE /saptrx/loc_id_type,
             dep_addr  TYPE /saptrx/paramval200,
             dep_email TYPE /saptrx/paramval200,
             dep_tel   TYPE /saptrx/paramval200,
             " Destination section
             werks     TYPE lips-werks,
             werks_lt  TYPE /saptrx/loc_id_type,
             dest_addr TYPE /saptrx/paramval200,
             lgnum     TYPE lips-lgnum,
             lgtor     TYPE lips-lgtor,
             lgnum_txt TYPE /saptrx/paramval200,
             " Others
             bldat     TYPE likp-bldat,
             lfdat     TYPE likp-lfdat,
             brgew     TYPE lips-brgew,
             ntgew     TYPE lips-ntgew,
             gewei     TYPE likp-gewei,
             volum     TYPE lips-volum,
             voleh     TYPE lips-voleh,
             bolnr     TYPE likp-bolnr,
             profl     TYPE lips-profl,
             incov     TYPE likp-incov,
             inco1     TYPE likp-inco1,
             inco2_l   TYPE likp-inco2_l,
             po_item   TYPE tv_po_item,
           END OF ts_dl_item.

    CONSTANTS: BEGIN OF cs_mapping,
                 " Header section
                 vbeln     TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY',
                 posnr     TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY_ITEM',
                 arktx     TYPE /saptrx/paramname VALUE 'YN_DL_ITEM_DESCR',
                 matnr     TYPE /saptrx/paramname VALUE 'YN_DL_MATERIAL_ID',
                 lfimg     TYPE /saptrx/paramname VALUE 'YN_DL_ORDER_QUANT',
                 vrkme     TYPE /saptrx/paramname VALUE 'YN_DL_ORDER_UNITS',
                 " Departure section
                 lifnr     TYPE /saptrx/paramname VALUE 'YN_DL_VENDOR_ID',
                 lifnr_lt  TYPE /saptrx/paramname VALUE 'YN_DL_VENDOR_LOC_TYPE',
                 dep_addr  TYPE /saptrx/paramname VALUE 'YN_DL_DEPART_ADDRESS',
                 dep_email TYPE /saptrx/paramname VALUE 'YN_DL_DEPART_EMAIL',
                 dep_tel   TYPE /saptrx/paramname VALUE 'YN_DL_DEPART_TEL',
                 " Destination section
                 werks     TYPE /saptrx/paramname VALUE 'YN_DL_PLANT',
                 werks_lt  TYPE /saptrx/paramname VALUE 'YN_DL_PLANT_LOC_TYPE',
                 dest_addr TYPE /saptrx/paramname VALUE 'YN_DL_DESTIN_ADDRESS',
                 lgnum     TYPE /saptrx/paramname VALUE 'YN_DL_WAREHOUSE',
                 lgnum_txt TYPE /saptrx/paramname VALUE 'YN_DL_WAREHOUSE_DESC',
                 lgtor     TYPE /saptrx/paramname VALUE 'YN_DL_DOOR',
                 " Others
                 bldat     TYPE /saptrx/paramname VALUE 'YN_DL_DOCUMENT_DATE',
                 lfdat     TYPE /saptrx/paramname VALUE 'YN_DL_PLANNED_DLV_DATE',
                 brgew     TYPE /saptrx/paramname VALUE 'YN_DL_GROSS_WEIGHT',
                 ntgew     TYPE /saptrx/paramname VALUE 'YN_DL_NET_WEIGHT',
                 gewei     TYPE /saptrx/paramname VALUE 'YN_DL_WEIGHT_UNITS',
                 volum     TYPE /saptrx/paramname VALUE 'YN_DL_VOLUME',
                 voleh     TYPE /saptrx/paramname VALUE 'YN_DL_VOLUME_UNITS',
                 bolnr     TYPE /saptrx/paramname VALUE 'YN_DL_BILL_OF_LADING',
                 profl     TYPE /saptrx/paramname VALUE 'YN_DL_DANGEROUS_GOODS',
                 incov     TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS_VERSION',
                 inco1     TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS',
                 inco2_l   TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS_LOCATION',
                 po_item   TYPE /saptrx/paramname VALUE 'YN_DL_ASSOC_POITEM_NO',
               END OF cs_mapping.

    CONSTANTS: cv_posnr_empty TYPE posnr_vl VALUE '000000'.

    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters.

    METHODS fill_item_from_likp_struct
      IMPORTING
        ir_likp    TYPE REF TO data
      CHANGING
        cs_dl_item TYPE ts_dl_item
      RAISING
        cx_udm_message.

    METHODS fill_item_from_lips_struct
      IMPORTING
        ir_lips    TYPE REF TO data
      CHANGING
        cs_dl_item TYPE ts_dl_item
      RAISING
        cx_udm_message.

    METHODS fill_item_from_vbpa_table
      IMPORTING
        ir_vbpa    TYPE REF TO data
        iv_vbeln   TYPE vbeln_vl
        iv_posnr   TYPE posnr_vl
      CHANGING
        cs_dl_item TYPE ts_dl_item
      RAISING
        cx_udm_message.

    METHODS fill_item_location_types
      CHANGING
        cs_dl_item TYPE ts_dl_item.

    METHODS get_likp_struct_old
      IMPORTING
        is_app_object  TYPE trxas_appobj_ctab_wa
        iv_vbeln       TYPE vbeln_vl
      RETURNING
        VALUE(rr_likp) TYPE REF TO data
      RAISING
        cx_udm_message.

    METHODS get_lips_struct_old
      IMPORTING
        is_app_object  TYPE trxas_appobj_ctab_wa
        iv_vbeln       TYPE vbeln_vl
        iv_posnr       TYPE posnr_vl
      RETURNING
        VALUE(rr_lips) TYPE REF TO data
      RAISING
        cx_udm_message.

    METHODS get_purchasing_order_item
      IMPORTING
        ir_lips           TYPE REF TO data
      RETURNING
        VALUE(rv_po_item) TYPE tv_po_item
      RAISING
        cx_udm_message.

    METHODS get_vbpa_table_old
      IMPORTING
        is_app_object  TYPE trxas_appobj_ctab_wa
        iv_vbeln       TYPE vbeln_vl
        iv_posnr       TYPE posnr_vl
      RETURNING
        VALUE(rr_vbpa) TYPE REF TO data
      RAISING
        cx_udm_message.

    METHODS is_object_changed
      IMPORTING
        is_app_object    TYPE trxas_appobj_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_bo_reader_dl_item IMPLEMENTATION.
  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
  ENDMETHOD.

  METHOD fill_item_from_likp_struct.
    FIELD-SYMBOLS: <ls_likp>  TYPE likpvb.

    ASSIGN ir_likp->* TO <ls_likp>.

    IF <ls_likp> IS ASSIGNED.
      MOVE-CORRESPONDING <ls_likp> TO cs_dl_item.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIKP' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_item_from_lips_struct.
    FIELD-SYMBOLS: <ls_lips> TYPE lipsvb.

    DATA(lv_lgtor_old) = cs_dl_item-lgtor.

    ASSIGN ir_lips->* TO <ls_lips>.

    IF <ls_lips> IS ASSIGNED.
      MOVE-CORRESPONDING <ls_lips> TO cs_dl_item.

      cs_dl_item-profl  = boolc( cs_dl_item-profl IS NOT INITIAL ).

      cs_dl_item-lgtor  = COND #( WHEN cs_dl_item-lgtor IS NOT INITIAL
                                    THEN cs_dl_item-lgtor
                                    ELSE lv_lgtor_old ).
      TRY.
          lcl_dl_tools=>get_addres_info(
            EXPORTING
              iv_addr_numb = lcl_dl_tools=>get_plant_address_number(
                               iv_werks = <ls_lips>-werks )
            IMPORTING
              ev_address   = cs_dl_item-dest_addr ).

        CATCH cx_udm_message.
      ENDTRY.

      IF cs_dl_item-lgnum IS NOT INITIAL AND
         cs_dl_item-lgtor IS NOT INITIAL.
        TRY.
            cs_dl_item-lgnum_txt  = lcl_dl_tools=>get_door_description(
                                      EXPORTING
                                        iv_lgnum = cs_dl_item-lgnum
                                        iv_lgtor = cs_dl_item-lgtor ).
          CATCH cx_udm_message.
        ENDTRY.
      ENDIF.

      cs_dl_item-po_item    = get_purchasing_order_item(
                                ir_lips   = ir_lips ).
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIKP' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_item_from_vbpa_table.
    TYPES: tt_vbpa TYPE STANDARD TABLE OF vbpavb.

    FIELD-SYMBOLS: <lt_vbpa>      TYPE tt_vbpa,
                   <ls_vbpa>      TYPE vbpavb,
                   <lv_addr_type> TYPE any.

    ASSIGN ir_vbpa->* TO <lt_vbpa>.

    IF <lt_vbpa> IS ASSIGNED.
      READ TABLE <lt_vbpa> ASSIGNING <ls_vbpa>
        WITH KEY vbeln = iv_vbeln
                 posnr = iv_posnr
                 parvw = lif_app_constants=>cs_parvw-supplier.

      IF sy-subrc = 0.
        ASSIGN COMPONENT 'ADDR_TYPE' OF STRUCTURE <ls_vbpa> TO <lv_addr_type>.
        TRY.
            lcl_dl_tools=>get_addres_info(
              EXPORTING
                iv_addr_type = COND #( WHEN <lv_addr_type> IS ASSIGNED AND
                                            <lv_addr_type> IS NOT INITIAL
                                         THEN <lv_addr_type>
                                         ELSE lif_app_constants=>cs_adrtype-organization )
                iv_addr_numb = <ls_vbpa>-adrnr
              IMPORTING
                ev_address   = cs_dl_item-dep_addr
                ev_email     = cs_dl_item-dep_email
                ev_telephone = cs_dl_item-dep_tel ).
          CATCH cx_udm_message.
        ENDTRY.
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VBPA' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_item_location_types.
    cs_dl_item-lifnr_lt   = lif_ef_constants=>cs_loc_types-supplier.
    cs_dl_item-werks_lt   = lif_ef_constants=>cs_loc_types-plant.
  ENDMETHOD.

  METHOD get_likp_struct_old.
    " when header is unchanged, table 'DELIVERY_HEADER_OLD' is not populated
    " so mastertab record is used as data source for header data
    TYPES: tt_likp TYPE STANDARD TABLE OF likpvb.

    FIELD-SYMBOLS: <lt_likp> TYPE tt_likp,
                   <ls_likp> TYPE likpvb.

    DATA(lr_likp)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef = lif_app_constants=>cs_tabledef-dl_header_old ).

    ASSIGN lr_likp->* TO <lt_likp>.

    IF <lt_likp> IS ASSIGNED.
      READ TABLE <lt_likp> ASSIGNING <ls_likp>
        WITH KEY vbeln = iv_vbeln.

      rr_likp   = COND #( WHEN sy-subrc = 0
                            THEN REF #( <ls_likp> )
                            ELSE is_app_object-mastertabref ).
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIKP' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_lips_struct_old.
    " when item is unchanged, it is absent in table 'DELIVERY_ITEM_OLD'
    " so maintab record is used as data source for item data
    TYPES: tt_lips TYPE STANDARD TABLE OF lipsvb.

    FIELD-SYMBOLS: <lt_lips> TYPE tt_lips,
                   <ls_lips> TYPE lipsvb.

    DATA(lr_lips)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef = lif_app_constants=>cs_tabledef-dl_item_old ).

    ASSIGN lr_lips->* TO <lt_lips>.

    IF <lt_lips> IS ASSIGNED.
      READ TABLE <lt_lips> ASSIGNING <ls_lips>
        WITH KEY vbeln = iv_vbeln
                 posnr = iv_posnr.

      rr_lips   = COND #( WHEN sy-subrc = 0
                            THEN REF #( <ls_lips> )
                            ELSE is_app_object-maintabref ).
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIPS' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_purchasing_order_item.
    DATA(lv_ebeln)  = CONV ebeln( lcl_tools=>get_field_of_structure(
                                    ir_struct_data = ir_lips
                                    iv_field_name  = 'VGBEL' ) ).

    DATA(lv_ebelp)  = CONV ebelp( lcl_tools=>get_field_of_structure(
                                    ir_struct_data = ir_lips
                                    iv_field_name  = 'VGPOS' ) ).

    rv_po_item  = |{ lv_ebeln }{ lv_ebelp }|.
  ENDMETHOD.

  METHOD get_vbpa_table_old.
    " when partner data is unchanged, it is absent in table 'PARTNERS_OLD'
    " so 'PARTNERS_NEW' is used as data source for partner data
    TYPES: tt_vbpa TYPE STANDARD TABLE OF vbpavb.

    FIELD-SYMBOLS: <lt_vbpa> TYPE tt_vbpa,
                   <ls_vbpa> TYPE vbpavb.

    DATA(lr_vbpa)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef = lif_app_constants=>cs_tabledef-dl_partners_old ).

    ASSIGN lr_vbpa->* TO <lt_vbpa>.

    IF <lt_vbpa> IS ASSIGNED.
      READ TABLE <lt_vbpa> ASSIGNING <ls_vbpa>
        WITH KEY vbeln = iv_vbeln
                 posnr = iv_posnr
                 parvw = lif_app_constants=>cs_parvw-supplier.

      rr_vbpa = COND #( WHEN sy-subrc = 0
                          THEN lr_vbpa
                          ELSE mo_ef_parameters->get_appl_table(
                                 iv_tabledef = lif_app_constants=>cs_tabledef-dl_partners_new ) ).
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VBPA' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD is_object_changed.
    rv_result   = lcl_tools=>is_object_changed(
                    is_app_object    = is_app_object
                    io_ef_parameters = mo_ef_parameters
                    it_check_tables  = VALUE #( ( lif_app_constants=>cs_tabledef-dl_partners_new )
                                                ( lif_app_constants=>cs_tabledef-dl_partners_old ) )
                    iv_key_field     = 'VBELN'
                    iv_upd_field     = 'UPDKZ'
                    iv_chk_mastertab = abap_true ).
  ENDMETHOD.

  METHOD lif_bo_reader~check_relevance.
    rv_result   = lif_ef_constants=>cs_condition-false.

    IF lcl_dl_tools=>is_appropriate_dl_type( ir_struct = is_app_object-mastertabref ) = abap_true AND
       lcl_dl_tools=>is_appropriate_dl_item( ir_struct = is_app_object-maintabref ) = abap_true AND
       is_object_changed( is_app_object = is_app_object ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN lif_ef_constants=>cs_change_mode-insert.
          rv_result   = lif_ef_constants=>cs_condition-true.
        WHEN lif_ef_constants=>cs_change_mode-update OR
             lif_ef_constants=>cs_change_mode-undefined.
          rv_result   = lcl_tools=>are_structures_different(
                          ir_data1  = lif_bo_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = lif_bo_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD lif_bo_reader~get_data.
    FIELD-SYMBOLS: <ls_item> TYPE ts_dl_item.

    rr_data   = NEW ts_dl_item( ).

    ASSIGN rr_data->* TO <ls_item>.

    fill_item_from_likp_struct(
      EXPORTING
        ir_likp    = is_app_object-mastertabref
      CHANGING
        cs_dl_item = <ls_item> ).

    fill_item_from_lips_struct(
      EXPORTING
        ir_lips    = is_app_object-maintabref
      CHANGING
        cs_dl_item = <ls_item> ).

    fill_item_from_vbpa_table(
      EXPORTING
        ir_vbpa    = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-dl_partners_new )
        iv_vbeln   = <ls_item>-vbeln
        iv_posnr   = cv_posnr_empty
      CHANGING
        cs_dl_item = <ls_item> ).

    fill_item_location_types(
      CHANGING
        cs_dl_item = <ls_item> ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_data_old.

    FIELD-SYMBOLS: <ls_item> TYPE ts_dl_item.

    DATA(lv_vbeln)  = CONV vbeln_vl( lcl_tools=>get_field_of_structure(
                                       ir_struct_data = is_app_object-maintabref
                                       iv_field_name  = 'VBELN' ) ).
    DATA(lv_posnr)  = CONV posnr_vl( lcl_tools=>get_field_of_structure(
                                       ir_struct_data = is_app_object-maintabref
                                       iv_field_name  = 'POSNR' ) ).

    rr_data   = NEW ts_dl_item( ).

    ASSIGN rr_data->* TO <ls_item>.

    fill_item_from_likp_struct(
      EXPORTING
        ir_likp    = get_likp_struct_old(
                       is_app_object = is_app_object
                       iv_vbeln      = lv_vbeln )
      CHANGING
        cs_dl_item = <ls_item> ).

    fill_item_from_lips_struct(
      EXPORTING
        ir_lips    = get_lips_struct_old(
                       is_app_object = is_app_object
                       iv_vbeln      = lv_vbeln
                       iv_posnr      = lv_posnr )
      CHANGING
        cs_dl_item = <ls_item> ).


    fill_item_from_vbpa_table(
      EXPORTING
        ir_vbpa    = get_vbpa_table_old(
                       is_app_object = is_app_object
                       iv_vbeln      = lv_vbeln
                       iv_posnr      = cv_posnr_empty )
        iv_vbeln   = lv_vbeln
        iv_posnr   = cv_posnr_empty
      CHANGING
        cs_dl_item = <ls_item> ).

    fill_item_location_types(
      CHANGING
        cs_dl_item = <ls_item> ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_field_parameter.
    CLEAR: rv_result.
  ENDMETHOD.

  METHOD lif_bo_reader~get_mapping_structure.
    rr_data   = REF #( cs_mapping ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_track_id_data.
    "In ERP’s extractors, need to include 2 tracking IDs.
    "The first one is for itself, one is for its header –
    "please ensure same tracking ID type to be used in the
    "Inbound Delivery Header process

    DATA: lv_fname TYPE char5.

    FIELD-SYMBOLS: <ls_lips>  TYPE lipsvb.

    " Actual Business Time zone
    DATA(lv_tzone)  = lcl_tools=>get_system_time_zone( ).

    ASSIGN is_app_object-maintabref->* TO <ls_lips>.

    IF <ls_lips> IS ASSIGNED.
      et_track_id_data  = VALUE #( (
          appsys      = mo_ef_parameters->get_appsys( )
          appobjtype  = is_app_object-appobjtype
          appobjid    = is_app_object-appobjid
          trxcod      = lif_app_constants=>cs_trxcod-dl_position
          trxid       = |{ <ls_lips>-vbeln }{ <ls_lips>-posnr }|
          start_date  = lcl_tools=>get_system_date_time( )
          end_date    = lif_ef_constants=>cv_max_end_date
          timzon      = lv_tzone
          msrid       = space
        ) ).

      IF <ls_lips>-updkz = lif_ef_constants=>cs_change_mode-insert.
        et_track_id_data = VALUE #( BASE et_track_id_data (
            appsys      = mo_ef_parameters->get_appsys( )
            appobjtype  = is_app_object-appobjtype
            appobjid    = is_app_object-appobjid
            trxcod      = lif_app_constants=>cs_trxcod-dl_number
            trxid       = |{ <ls_lips>-vbeln }|
            start_date  = lcl_tools=>get_system_date_time( )
            end_date    = lif_ef_constants=>cv_max_end_date
            timzon      = lv_tzone
            msrid       = space
          ) ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIPS' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_bo_reader_sh_header DEFINITION.
  PUBLIC SECTION.
    INTERFACES: lif_bo_reader.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters.

  PRIVATE SECTION.
    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters.

    TYPES: tt_trobj_res_id  TYPE STANDARD TABLE OF lif_app_types=>tv_trobj_res_id WITH EMPTY KEY,
           tt_trobj_res_val TYPE STANDARD TABLE OF lif_app_types=>tv_trobj_res_val WITH EMPTY KEY,
           tt_resrc_cnt     TYPE STANDARD TABLE OF lif_app_types=>tv_resrc_cnt WITH EMPTY KEY,
           tt_resrc_tp_id   TYPE STANDARD TABLE OF lif_app_types=>tv_resrc_tp_id WITH EMPTY KEY,
           tt_crdoc_ref_typ TYPE STANDARD TABLE OF lif_app_types=>tv_crdoc_ref_typ WITH EMPTY KEY,
           tt_crdoc_ref_val TYPE STANDARD TABLE OF lif_app_types=>tv_crdoc_ref_val WITH EMPTY KEY.

    TYPES: BEGIN OF ts_sh_header,
             tknum              TYPE vttkvb-tknum,
             bu_id_num          TYPE /saptrx/paramval200,
             cont_dg            TYPE vttkvb-cont_dg,
             tndr_trkid         TYPE vttkvb-tndr_trkid,
             ship_type          TYPE lif_app_types=>tv_ship_type,
             trans_mode         TYPE lif_app_types=>tv_trans_mode,
             departure_dt       TYPE lif_app_types=>tv_departure_dt,
             departure_tz       TYPE lif_app_types=>tv_departure_tz,
             departure_locid    TYPE lif_app_types=>tv_locid,
             departure_loctype  TYPE lif_app_types=>tv_loctype,
             arrival_dt         TYPE lif_app_types=>tv_arrival_dt,
             arrival_tz         TYPE lif_app_types=>tv_arrival_tz,
             arrival_locid      TYPE lif_app_types=>tv_locid,
             arrival_loctype    TYPE lif_app_types=>tv_loctype,
             tdlnr              TYPE vttkvb-tdlnr,
             deliv_cnt          TYPE STANDARD TABLE OF lif_app_types=>tv_deliv_cnt WITH EMPTY KEY,
             deliv_no           TYPE STANDARD TABLE OF vbeln WITH EMPTY KEY,
             trobj_res_id       TYPE tt_trobj_res_id,
             trobj_res_val      TYPE tt_trobj_res_val,
             resrc_cnt          TYPE tt_resrc_cnt,
             resrc_tp_id        TYPE tt_resrc_tp_id,
             crdoc_ref_typ      TYPE tt_crdoc_ref_typ,
             crdoc_ref_val      TYPE tt_crdoc_ref_val,
             stops_num          TYPE STANDARD TABLE OF lif_app_types=>tv_stopnum WITH EMPTY KEY,
             stops_stopid       TYPE STANDARD TABLE OF lif_app_types=>tv_stopid WITH EMPTY KEY,
             stops_stopcnt      TYPE STANDARD TABLE OF lif_app_types=>tv_stopcnt WITH EMPTY KEY,
             stops_loccat       TYPE STANDARD TABLE OF lif_app_types=>tv_loccat  WITH EMPTY KEY,
             stops_loctype      TYPE STANDARD TABLE OF lif_app_types=>tv_loctype WITH EMPTY KEY,
             stops_locid        TYPE STANDARD TABLE OF lif_app_types=>tv_locid WITH EMPTY KEY,
             stops_lstelz_txt   TYPE STANDARD TABLE OF lif_app_types=>tv_lstelz_txt WITH EMPTY KEY,
             stops_kunablaz_txt TYPE STANDARD TABLE OF lif_app_types=>tv_kunablaz_txt WITH EMPTY KEY,
             stops_lgortaz_txt  TYPE STANDARD TABLE OF lif_app_types=>tv_lgortaz_txt WITH EMPTY KEY,
             stops_lgnumaz      TYPE STANDARD TABLE OF lif_app_types=>tv_lgnumaz WITH EMPTY KEY,
             stops_toraz        TYPE STANDARD TABLE OF lif_app_types=>tv_toraz WITH EMPTY KEY,
             stops_lgtraz_txt   TYPE STANDARD TABLE OF lif_app_types=>tv_lgtraz_txt WITH EMPTY KEY,
             stops_tsrfo        TYPE STANDARD TABLE OF lif_app_types=>tv_tsrfo WITH EMPTY KEY,
             stops_pln_evt_dt   TYPE STANDARD TABLE OF lif_app_types=>tv_pln_evt_datetime WITH EMPTY KEY,
             stops_pln_evt_tz   TYPE STANDARD TABLE OF lif_app_types=>tv_pln_evt_timezone WITH EMPTY KEY,
             stpid_stopid       TYPE STANDARD TABLE OF lif_app_types=>tv_stopid WITH EMPTY KEY,
             stpid_stopcnt      TYPE STANDARD TABLE OF lif_app_types=>tv_stopcnt WITH EMPTY KEY,
             stpid_loctype      TYPE STANDARD TABLE OF lif_app_types=>tv_loctype WITH EMPTY KEY,
             stpid_locid        TYPE STANDARD TABLE OF lif_app_types=>tv_locid WITH EMPTY KEY,
           END OF ts_sh_header.

    CONSTANTS: BEGIN OF cs_mapping,
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
               END OF cs_mapping.

    METHODS fill_header_from_vttk
      IMPORTING
        ir_vttk     TYPE REF TO data
        iv_vttp_cnt TYPE i
      CHANGING
        cs_header   TYPE ts_sh_header
      RAISING
        cx_udm_message.

    METHODS fill_header_from_vttp
      IMPORTING
        ir_vttk   TYPE REF TO data
        ir_vttp   TYPE REF TO data
      CHANGING
        cs_header TYPE ts_sh_header
      RAISING
        cx_udm_message.

    METHODS fill_header_from_vtts
      IMPORTING
        ir_vttk   TYPE REF TO data
        ir_vttp   TYPE REF TO data
        ir_vtts   TYPE REF TO data
        ir_vtsp   TYPE REF TO data OPTIONAL
      CHANGING
        cs_header TYPE ts_sh_header
      RAISING
        cx_udm_message.

    METHODS fill_tracked_object_tables
      IMPORTING
        is_vttk    TYPE vttkvb
      EXPORTING
        et_res_id  TYPE tt_trobj_res_id
        et_res_val TYPE tt_trobj_res_val.

    METHODS fill_resource_tables
      IMPORTING
        is_vttk    TYPE vttkvb
      EXPORTING
        et_ref_cnt TYPE tt_resrc_cnt
        et_tp_id   TYPE tt_resrc_tp_id.

    METHODS fill_carrier_ref_doc_tables
      IMPORTING
        is_vttk    TYPE vttkvb
      EXPORTING
        et_ref_typ TYPE tt_crdoc_ref_typ
        et_ref_val TYPE tt_crdoc_ref_val.

    METHODS get_forwarding_agent_id_number
      IMPORTING
        iv_tdlnr         TYPE tdlnr
      RETURNING
        VALUE(rv_id_num) TYPE /saptrx/paramval200.

    METHODS get_resource_tracking_id
      IMPORTING
        is_vttk               TYPE vttkvb
      RETURNING
        VALUE(rv_tracking_id) TYPE /saptrx/trxid.

    METHODS get_shippment_header
      IMPORTING
        is_app_object  TYPE trxas_appobj_ctab_wa
        ir_vttk        TYPE REF TO data
      RETURNING
        VALUE(rr_vttk) TYPE REF TO data
      RAISING
        cx_udm_message.

    METHODS get_shippment_item_count
      IMPORTING
        ir_vttp         TYPE REF TO data
      RETURNING
        VALUE(rv_count) TYPE i
      RAISING
        cx_udm_message.

    METHODS get_shippment_type
      IMPORTING
        iv_vsart            TYPE clike
        iv_vttp_cnt         TYPE i
      RETURNING
        VALUE(rv_ship_type) TYPE lif_app_types=>tv_ship_type.

    METHODS get_transportation_mode
      IMPORTING
        iv_vsart             TYPE clike
      RETURNING
        VALUE(rv_trans_mode) TYPE lif_app_types=>tv_trans_mode.

    METHODS is_object_changed
      IMPORTING
        is_app_object    TYPE trxas_appobj_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_bo_reader_sh_header IMPLEMENTATION.
  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
  ENDMETHOD.

  METHOD fill_carrier_ref_doc_tables.
    lcl_sh_tools=>get_carrier_reference_document(
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
      MESSAGE e002(zpof_gtt) WITH 'VTTK' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
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
      MESSAGE e002(zpof_gtt) WITH 'VTTK' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VTTP' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD fill_header_from_vtts.
    TYPES: BEGIN OF ts_stop_id,
             stopid  TYPE zgtt_stopid,
             stopcnt TYPE zgtt_stopcnt,
             loctype TYPE zgtt_loctype,
             locid   TYPE zgtt_locid,
           END OF ts_stop_id.

    DATA(lv_tknum) = CONV tknum( lcl_tools=>get_field_of_structure(
                                   ir_struct_data = ir_vttk
                                   iv_field_name  = 'TKNUM' ) ).

    DATA: lt_vtts     TYPE vttsvb_tab,
          lt_stops    TYPE lif_app_types=>tt_stops,
          lt_stop_ids TYPE STANDARD TABLE OF ts_stop_id,
          lv_datetime TYPE lif_app_types=>tv_pln_evt_datetime,
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

      lcl_sh_tools=>get_stops_from_shipment(
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

        lv_datetime = lcl_tools=>get_valid_datetime(
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
        cs_header-departure_dt      = lcl_tools=>convert_datetime_to_utc(
                                        iv_datetime = <ls_stops>-pln_evt_datetime
                                        iv_timezone = <ls_stops>-pln_evt_timezone ).
        cs_header-departure_tz      = <ls_stops>-pln_evt_timezone.
        cs_header-departure_locid   = <ls_stops>-locid.
        cs_header-departure_loctype = <ls_stops>-loctype.
      ENDIF.

      READ TABLE lt_stops ASSIGNING <ls_stops> INDEX lines( lt_stops ).
      IF sy-subrc = 0.
        cs_header-arrival_dt      = lcl_tools=>convert_datetime_to_utc(
                                        iv_datetime = <ls_stops>-pln_evt_datetime
                                        iv_timezone = <ls_stops>-pln_evt_timezone ).
        cs_header-arrival_tz      = <ls_stops>-pln_evt_timezone.
        cs_header-arrival_locid   = <ls_stops>-locid.
        cs_header-arrival_loctype = <ls_stops>-loctype.
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VTTS' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
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
    IF is_vttk-vsart = '01' AND is_vttk-exti1 IS NOT INITIAL.
      et_res_id  = VALUE #( ( 'NA' ) ).
      et_res_val = VALUE #( ( is_vttk-exti1 ) ).

    ELSEIF is_vttk-vsart = '04' AND is_vttk-signi IS NOT INITIAL.
      et_res_id  = VALUE #( ( 'CONTAINER_ID' ) ).
      et_res_val = VALUE #( ( is_vttk-signi ) ).

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
      WITH KEY identificationtype = lif_app_constants=>cv_agent_id_type
      BINARY SEARCH.

    rv_id_num   = COND #( WHEN sy-subrc = 0
                            THEN lif_app_constants=>cv_agent_id_type &&
                                 <ls_bpdetail>-identificationnumber ).
  ENDMETHOD.

  METHOD get_resource_tracking_id.
    rv_tracking_id  = COND #(
      WHEN is_vttk-vsart = '01' AND is_vttk-exti1 IS NOT INITIAL
        THEN |{ is_vttk-tknum }{ is_vttk-exti1 }|
      WHEN is_vttk-vsart = '04' AND is_vttk-signi IS NOT INITIAL
        THEN |{ is_vttk-tknum }{ is_vttk-signi }|
    ).
  ENDMETHOD.

  METHOD get_shippment_header.
    TYPES: tt_vttk TYPE STANDARD TABLE OF vttkvb.

    FIELD-SYMBOLS: <lt_vttk> TYPE tt_vttk.

    DATA(lv_tknum)  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = is_app_object-maintabref
                        iv_field_name  = 'TKNUM' ).

    ASSIGN ir_vttk->* TO <lt_vttk>.
    IF <lt_vttk> IS ASSIGNED.
      READ TABLE <lt_vttk> ASSIGNING FIELD-SYMBOL(<ls_vttk>)
        WITH KEY tknum = lv_tknum.

      IF sy-subrc = 0.
        rr_vttk = REF #( <ls_vttk> ).
      ELSE.
        MESSAGE e005(zpof_gtt) WITH 'VTTK OLD' lv_tknum
          INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VTTK' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
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
    rv_result = lcl_tools=>is_object_changed(
                  is_app_object    = is_app_object
                  io_ef_parameters = mo_ef_parameters
                  it_check_tables  = VALUE #( ( lif_app_constants=>cs_tabledef-sh_item_new )
                                              ( lif_app_constants=>cs_tabledef-sh_item_new )
                                              ( lif_app_constants=>cs_tabledef-sh_stage_new )
                                              ( lif_app_constants=>cs_tabledef-sh_stage_old )
                                              ( lif_app_constants=>cs_tabledef-sh_item_stage_new )
                                              ( lif_app_constants=>cs_tabledef-sh_item_stage_old ) )
                  iv_key_field     = 'TKNUM'
                  iv_upd_field     = 'UPDKZ' ).
  ENDMETHOD.

  METHOD lif_bo_reader~check_relevance.
    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_new ).

    rv_result = lif_ef_constants=>cs_condition-false.

    IF lcl_sh_tools=>is_appropriate_type( ir_vttk = is_app_object-maintabref ) = abap_true AND
       lcl_sh_tools=>is_delivery_assigned( ir_vttp = lr_vttp ) = abap_true AND
       is_object_changed( is_app_object = is_app_object ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN lif_ef_constants=>cs_change_mode-insert.
          rv_result   = lif_ef_constants=>cs_condition-true.
        WHEN lif_ef_constants=>cs_change_mode-update OR
             lif_ef_constants=>cs_change_mode-undefined.
          rv_result   = lcl_tools=>are_structures_different(
                          ir_data1  = lif_bo_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = lif_bo_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.
    ENDIF.
  ENDMETHOD.

  METHOD lif_bo_reader~get_data.
    FIELD-SYMBOLS: <ls_header>  TYPE ts_sh_header.

    rr_data   = NEW ts_sh_header(  ).

    ASSIGN rr_data->* TO <ls_header>.

    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
                      iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_new ).

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
                      iv_tabledef = lif_app_constants=>cs_tabledef-sh_stage_new )
        ir_vtsp   = mo_ef_parameters->get_appl_table(
                      iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_stage_new )
      CHANGING
        cs_header = <ls_header> ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_data_old.
    FIELD-SYMBOLS: <ls_header>  TYPE ts_sh_header.

    DATA(lo_sh_data)  = NEW lcl_sh_data_old(
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

  METHOD lif_bo_reader~get_field_parameter.
    CASE iv_parameter.
      WHEN lif_ef_constants=>cs_parameter_id-key_field.
        rv_result   = boolc( iv_field_name = cs_mapping-deliv_cnt     OR
                             iv_field_name = cs_mapping-trobj_res_val OR
                             iv_field_name = cs_mapping-resrc_cnt     OR
                             iv_field_name = cs_mapping-crdoc_ref_val OR
                             iv_field_name = cs_mapping-stops_num     OR
                             iv_field_name = cs_mapping-stpid_stopcnt ).
      WHEN lif_ef_constants=>cs_parameter_id-no_empty_tag.
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

  METHOD lif_bo_reader~get_mapping_structure.
    rr_data = REF #( cs_mapping ).
  ENDMETHOD.

  METHOD lif_bo_reader~get_track_id_data.
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
        trxcod      = lif_app_constants=>cs_trxcod-sh_number
        trxid       = |{ <ls_vttk>-tknum }|
        start_date  = lcl_tools=>get_system_date_time( )
        end_date    = lif_ef_constants=>cv_max_end_date
        timzon      = lcl_tools=>get_system_time_zone( )
      ) ).

      DATA(lv_res_tid_new)  = get_resource_tracking_id(
                                is_vttk = <ls_vttk> ).

      " is shipment created?
      IF <ls_vttk>-updkz = lif_ef_constants=>cs_change_mode-insert AND
         lv_res_tid_new IS NOT INITIAL.

        " add RESOURCE Tracking ID for the new Shipment
        et_track_id_data  = VALUE #( BASE et_track_id_data (
          appsys      = mo_ef_parameters->get_appsys( )
          appobjtype  = is_app_object-appobjtype
          appobjid    = is_app_object-appobjid
          trxcod      = lif_app_constants=>cs_trxcod-sh_resource
          trxid       = lv_res_tid_new
        ) ).

        " is shipment updated?
      ELSEIF <ls_vttk>-updkz = lif_ef_constants=>cs_change_mode-update.
        DATA(lr_vttk_old) = get_shippment_header(
                              is_app_object = is_app_object
                              ir_vttk       = mo_ef_parameters->get_appl_table(
                                                iv_tabledef = lif_app_constants=>cs_tabledef-sh_header_old ) ).

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
                trxcod      = lif_app_constants=>cs_trxcod-sh_resource
                trxid       = lv_res_tid_new
              ) ).
            ENDIF.

            " delete old RESOURCE Tracking ID
            IF lv_res_tid_old IS NOT INITIAL.
              et_track_id_data  = VALUE #( BASE et_track_id_data (
                appsys      = mo_ef_parameters->get_appsys( )
                appobjtype  = is_app_object-appobjtype
                appobjid    = is_app_object-appobjid
                trxcod      = lif_app_constants=>cs_trxcod-sh_resource
                trxid       = lv_res_tid_old
                action      = lif_ef_constants=>cs_change_mode-delete
              ) ).
            ENDIF.
          ENDIF.
        ELSE.
          MESSAGE e002(zpof_gtt) WITH 'VTTK' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VTTK' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
