*&---------------------------------------------------------------------*
*& Local class definition - Planned Events Fillers
*&---------------------------------------------------------------------*

**********************************************************************
*** Statuses of Inbound Delivery Item ********************************
**********************************************************************
CLASS lcl_dl_event_relevance_main DEFINITION
  ABSTRACT.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters
        is_app_objects   TYPE trxas_appobj_ctab_wa OPTIONAL.

    METHODS initiate
      RAISING
        cx_udm_message.

    METHODS is_enabled
      IMPORTING
        iv_milestone     TYPE clike
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    METHODS update.

  PROTECTED SECTION.
    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters,
          ms_app_objects   TYPE trxas_appobj_ctab_wa,
          ms_relevance     TYPE zpof_gtt_ee_rel.

    METHODS get_field_name ABSTRACT
      IMPORTING
        iv_milestone         TYPE clike
        iv_internal          TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_field_name) TYPE fieldname
      RAISING
        cx_udm_message.

    METHODS get_object_status ABSTRACT
      IMPORTING
        iv_milestone    TYPE clike
      RETURNING
        VALUE(rv_value) TYPE /saptrx/paramval200
      RAISING
        cx_udm_message.

  PRIVATE SECTION.
    METHODS set_relevance
      IMPORTING
        iv_milestone TYPE clike
        iv_relevance TYPE clike
      RAISING
        cx_udm_message.

    METHODS recalc_relevance
      IMPORTING
        iv_milestone TYPE clike
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_dl_event_relevance_main IMPLEMENTATION.
  METHOD constructor.
    mo_ef_parameters  = io_ef_parameters.
    ms_app_objects    = is_app_objects.
  ENDMETHOD.

  METHOD initiate.
    DATA(lv_appobjid) = ms_app_objects-appobjid.

    " read stored statuses
    SELECT SINGLE *
      INTO @ms_relevance
      FROM zpof_gtt_ee_rel
      WHERE appobjid  = @lv_appobjid.

    " initiate statuses with initial values
    IF sy-subrc <> 0.
      CLEAR: ms_relevance.
      ms_relevance-appobjid = lv_appobjid.

      " recalculate statuses
      recalc_relevance( iv_milestone = lif_app_constants=>cs_milestone-dl_put_away ).
      recalc_relevance( iv_milestone = lif_app_constants=>cs_milestone-dl_packing ).
      recalc_relevance( iv_milestone = lif_app_constants=>cs_milestone-dl_goods_receipt ).
      recalc_relevance( iv_milestone = lif_app_constants=>cs_milestone-dl_pod ).
    ENDIF.
  ENDMETHOD.

  METHOD is_enabled.
    DATA(lv_field_name) = get_field_name(
                            iv_milestone   = iv_milestone
                            iv_internal = abap_true ).

    ASSIGN COMPONENT lv_field_name OF STRUCTURE ms_relevance
      TO FIELD-SYMBOL(<lv_value>).

    IF <lv_value> IS ASSIGNED.
      rv_result   = <lv_value>.
    ELSE.
      MESSAGE e001(zpof_gtt) WITH lv_field_name '' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD set_relevance.
    DATA(lv_fname_int) = get_field_name(
                           iv_milestone = iv_milestone
                           iv_internal  = abap_true ).

    ASSIGN COMPONENT lv_fname_int OF STRUCTURE ms_relevance
      TO FIELD-SYMBOL(<lv_flag>).

    IF <lv_flag> IS ASSIGNED.
      <lv_flag>   = iv_relevance.
    ELSE.
      MESSAGE e001(zpof_gtt) WITH lv_fname_int '' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD recalc_relevance.
    " retrieve delivery item status field value
    DATA(lv_status)     = get_object_status( iv_milestone = iv_milestone ).

    " calculate relevance
    "   for shipment POD planning :
    "     status is <> 'empty' -> relevant
    "   for other delivery item level planned events:
    "     initial value is 'A' -> relevant
    DATA(lv_relevance)  = COND abap_bool(
      WHEN lv_status = lif_app_constants=>cs_delivery_stat-not_relevant
        THEN abap_false
      WHEN lv_status = lif_app_constants=>cs_delivery_stat-not_processed OR
           iv_milestone = lif_app_constants=>cs_milestone-dl_pod
        THEN abap_true
        ELSE abap_undefined
    ).

    " update flag value if it has appropriate value
    IF lv_relevance <> abap_undefined.
      set_relevance(
        EXPORTING
          iv_milestone = iv_milestone
          iv_relevance = lv_relevance ).
    ENDIF.
  ENDMETHOD.

  METHOD update.
    CALL FUNCTION 'ZPOF_GTT_UPDATE_RELEVANCE_TAB'
      DESTINATION 'NONE'
      EXPORTING
        is_relevance = ms_relevance.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      DESTINATION 'NONE'.

    CALL FUNCTION 'RFC_CONNECTION_CLOSE' ##FM_SUBRC_OK
      EXPORTING
        destination          = 'NONE'
      EXCEPTIONS
        destination_not_open = 1
        OTHERS               = 2.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Statuses of Inbound Delivery Item ********************************
**********************************************************************
CLASS lcl_dl_event_relevance_item DEFINITION
  INHERITING FROM lcl_dl_event_relevance_main.

  PROTECTED SECTION.
    METHODS get_field_name REDEFINITION.

    METHODS get_object_status REDEFINITION.

ENDCLASS.

CLASS lcl_dl_event_relevance_item IMPLEMENTATION.
  METHOD get_field_name.
    CASE iv_milestone.
      WHEN lif_app_constants=>cs_milestone-dl_put_away.
        rv_field_name   = 'KOSTA'.
      WHEN lif_app_constants=>cs_milestone-dl_packing.
        rv_field_name   = 'PKSTA'.
      WHEN lif_app_constants=>cs_milestone-dl_goods_receipt.
        rv_field_name   = 'WBSTA'.
      WHEN lif_app_constants=>cs_milestone-dl_pod.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'PDSTK'
                                    ELSE 'PDSTA' ).
      WHEN OTHERS.
        MESSAGE e009(zpof_gtt) WITH iv_milestone INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
    ENDCASE.

    IF iv_internal = abap_true.
      rv_field_name   = |Z_{ rv_field_name }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_object_status.
    TYPES: tt_vbup  TYPE STANDARD TABLE OF vbupvb.

    DATA: lv_dummy  TYPE char100.

    FIELD-SYMBOLS: <lt_vbup>  TYPE tt_vbup,
                   <ls_vbup>  TYPE vbupvb,
                   <lv_value> TYPE any.

    DATA(lv_fname)  = get_field_name( iv_milestone = iv_milestone ).

    DATA(lv_vbeln)  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = ms_app_objects-maintabref
                        iv_field_name  = 'VBELN' ).

    DATA(lv_posnr)  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = ms_app_objects-maintabref
                        iv_field_name  = 'POSNR' ).

    DATA(lr_vbup)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef    = lif_app_constants=>cs_tabledef-dl_itm_status_new ).

    CLEAR rv_value.

    ASSIGN lr_vbup->* TO <lt_vbup>.

    IF <lt_vbup> IS ASSIGNED.
      READ TABLE <lt_vbup> ASSIGNING <ls_vbup>
        WITH KEY vbeln  = lv_vbeln
                 posnr  = lv_posnr.

      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_vbup> TO <lv_value>.
        IF <lv_value> IS ASSIGNED.
          rv_value  = <lv_value>.
        ELSE.
          MESSAGE e001(zpof_gtt) WITH lv_fname 'VBUP' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ELSE.
        MESSAGE e005(zpof_gtt)
          WITH 'VBUP' |{ lv_vbeln }-{ lv_posnr }|
          INTO lv_dummy.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VBUP' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Statuses of Inbound Delivery Header ******************************
**********************************************************************
CLASS lcl_dl_event_relevance_header DEFINITION
  INHERITING FROM lcl_dl_event_relevance_main.

  PROTECTED SECTION.
    METHODS get_field_name REDEFINITION.

    METHODS get_object_status REDEFINITION.

ENDCLASS.

CLASS lcl_dl_event_relevance_header IMPLEMENTATION.
  METHOD get_field_name.
    CASE iv_milestone.
      WHEN lif_app_constants=>cs_milestone-dl_put_away.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'KOSTA'
                                    ELSE 'KOSTK' ).
      WHEN lif_app_constants=>cs_milestone-dl_packing.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'PKSTA'
                                    ELSE 'PKSTK' ).
      WHEN lif_app_constants=>cs_milestone-dl_goods_receipt.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'WBSTA'
                                    ELSE 'WBSTK' ).
      WHEN lif_app_constants=>cs_milestone-dl_pod.
        rv_field_name   = 'PDSTK'.
      WHEN OTHERS.
        MESSAGE e009(zpof_gtt) WITH iv_milestone INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
    ENDCASE.

    IF iv_internal = abap_true.
      rv_field_name   = |Z_{ rv_field_name }|.
    ENDIF.
  ENDMETHOD.

  METHOD get_object_status.
*    TYPES: tt_vbuk  TYPE STANDARD TABLE OF vbuk.

    DATA: lv_dummy  TYPE char100.

    FIELD-SYMBOLS: <lt_vbuk>  TYPE shp_vl10_vbuk_t,
                   <ls_vbuk>  TYPE vbukvb,
                   <lv_value> TYPE any.

    DATA(lv_fname)  = get_field_name( iv_milestone = iv_milestone ).

    DATA(lv_vbeln)  = lcl_tools=>get_field_of_structure(
                        ir_struct_data = ms_app_objects-maintabref
                        iv_field_name  = 'VBELN' ).

    DATA(lr_vbuk)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef    = lif_app_constants=>cs_tabledef-dl_hdr_status_new ).

    CLEAR rv_value.

    ASSIGN lr_vbuk->* TO <lt_vbuk>.

    IF <lt_vbuk> IS ASSIGNED.
      READ TABLE <lt_vbuk> ASSIGNING <ls_vbuk>
        WITH KEY vbeln  = lv_vbeln.

      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_vbuk> TO <lv_value>.
        IF <lv_value> IS ASSIGNED.
          rv_value  = <lv_value>.
        ELSE.
          MESSAGE e001(zpof_gtt) WITH lv_fname 'VBUP' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ELSE.
        MESSAGE e005(zpof_gtt)
          WITH 'VBUK' lv_vbeln
          INTO lv_dummy.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VBUK' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

***********************************************************************
**** Statuses of Inbound Delivery Item ********************************
***********************************************************************
CLASS lcl_sh_stops_events DEFINITION
  CREATE PRIVATE.

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_event_info,
             appsys        TYPE /saptrx/applsystem,
             appobjtype    TYPE /saptrx/aotype,
             appobjid      TYPE /saptrx/aoid,
             language      TYPE spras,
             evt_exp_tzone TYPE /saptrx/timezone,
           END OF ts_event_info.

    CLASS-METHODS get_instance_for_delivery
      IMPORTING
        iv_appobjid               TYPE /saptrx/aoid
        iv_vbeln                  TYPE vbeln_vl
        iv_posnr                  TYPE posnr_vl DEFAULT 0
        io_ef_parameters          TYPE REF TO lif_ef_parameters
      RETURNING
        VALUE(ro_sh_stops_events) TYPE REF TO lcl_sh_stops_events
      RAISING
        cx_udm_message.

    METHODS get_planned_events
      EXPORTING
        et_exp_event TYPE /saptrx/bapi_trk_ee_tab
      RAISING
        cx_udm_message.

  PRIVATE SECTION.
    TYPES: tt_tknum   TYPE STANDARD TABLE OF tknum.

    TYPES: BEGIN OF ts_stops_info,
             tknum    TYPE vttk-tknum,
             stops    TYPE lif_app_types=>tt_stops,
             watching TYPE lif_app_types=>tt_dlv_watch_stops,
           END OF ts_stops_info,
           tt_stops_info TYPE STANDARD TABLE OF ts_stops_info.

    DATA: mv_vbeln      TYPE vbeln_vl,
          mv_posnr      TYPE posnr_vl,
          ms_event_info TYPE ts_event_info,
          mt_stops_info TYPE tt_stops_info,
          mt_lips       TYPE lif_app_types=>tt_lipsvb.

    METHODS constructor
      IMPORTING iv_vbeln      TYPE vbeln_vl
                iv_posnr      TYPE posnr_vl
                is_event_info TYPE ts_event_info
                it_stops_info TYPE tt_stops_info
                it_lips       TYPE lif_app_types=>tt_lipsvb.

    CLASS-METHODS get_delivery_items
      IMPORTING
        iv_vbeln TYPE vbeln_vl
        iv_posnr TYPE posnr_vl
        ir_lips  TYPE REF TO data
      EXPORTING
        et_lips  TYPE lif_app_types=>tt_lipsvb
      RAISING
        cx_udm_message.

    CLASS-METHODS get_event_info
      IMPORTING
        iv_appobjid          TYPE /saptrx/aoid
        io_ef_parameters     TYPE REF TO lif_ef_parameters
      RETURNING
        VALUE(rs_event_info) TYPE ts_event_info
      RAISING
        cx_udm_message.

    CLASS-METHODS get_shipments_for_delivery
      IMPORTING
        iv_vbeln TYPE vbeln_vl
      EXPORTING
        et_tknum TYPE tt_tknum.

    CLASS-METHODS get_stops_info_for_delivery
      IMPORTING
        iv_vbeln      TYPE vbeln_vl
      EXPORTING
        et_stops_info TYPE tt_stops_info.

    METHODS is_pod_relevant
      IMPORTING
        iv_locid         TYPE clike
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

ENDCLASS.

CLASS lcl_sh_stops_events IMPLEMENTATION.
  METHOD constructor.
    mv_vbeln      = iv_vbeln.
    ms_event_info = is_event_info.
    mt_stops_info = it_stops_info.
    mt_lips       = it_lips.
  ENDMETHOD.

  METHOD get_delivery_items.
    DATA: lt_vbeln TYPE RANGE OF vbeln_vl,
          lt_posnr TYPE RANGE OF posnr_vl.

    CLEAR: et_lips[].

    FIELD-SYMBOLS: <lt_lips>  TYPE lif_app_types=>tt_lipsvb.

    ASSIGN ir_lips->* TO <lt_lips>.
    IF <lt_lips> IS ASSIGNED.
      IF iv_vbeln IS NOT INITIAL.
        lt_vbeln    = VALUE #( ( low = iv_vbeln option = 'EQ' sign = 'I' ) ).
      ENDIF.
      IF iv_posnr IS NOT INITIAL.
        lt_posnr    = VALUE #( ( low = iv_posnr option = 'EQ' sign = 'I' ) ).
      ENDIF.

      LOOP AT <lt_lips> ASSIGNING FIELD-SYMBOL(<ls_lips>)
        WHERE vbeln IN lt_vbeln
          AND posnr IN lt_posnr.

        APPEND <ls_lips> TO et_lips.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIPS' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.

  METHOD get_event_info.

    rs_event_info = VALUE #(
      appsys        = io_ef_parameters->get_appsys(  )
      appobjtype    = io_ef_parameters->get_app_obj_types( )-aotype
      appobjid      = iv_appobjid
      language      = sy-langu
      evt_exp_tzone = lcl_tools=>get_system_time_zone( )
    ).
  ENDMETHOD.

  METHOD get_instance_for_delivery.
    DATA: lt_stops_info TYPE tt_stops_info,
          lt_lips       TYPE lif_app_types=>tt_lipsvb.

    DATA(ls_event_info)  = get_event_info(
                              iv_appobjid      = iv_appobjid
                              io_ef_parameters = io_ef_parameters ).

    get_stops_info_for_delivery(
      EXPORTING
        iv_vbeln      = iv_vbeln
      IMPORTING
        et_stops_info = lt_stops_info ).

    get_delivery_items(
      EXPORTING
        iv_vbeln = iv_vbeln
        iv_posnr = iv_posnr
        ir_lips  = io_ef_parameters->get_appl_table(
                     iv_tabledef = lif_app_constants=>cs_tabledef-dl_item_new )
      IMPORTING
        et_lips  = lt_lips ).

    ro_sh_stops_events    = NEW lcl_sh_stops_events(
      iv_vbeln      = iv_vbeln
      iv_posnr      = iv_posnr
      is_event_info = ls_event_info
      it_stops_info = lt_stops_info
      it_lips       = lt_lips ).
  ENDMETHOD.

  METHOD get_planned_events.
    DATA: lt_exp_event TYPE /saptrx/bapi_trk_ee_tab.

    LOOP AT mt_stops_info ASSIGNING FIELD-SYMBOL(<ls_stops_info>).
      LOOP AT <ls_stops_info>-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
        WHERE vbeln = mv_vbeln.

        READ TABLE <ls_stops_info>-stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
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
             is_pod_relevant( iv_locid = <ls_stops>-locid ) = abap_true.

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
    ENDLOOP.

    LOOP AT lt_exp_event ASSIGNING FIELD-SYMBOL(<ls_exp_event>).
      <ls_exp_event>-appsys         = ms_event_info-appsys.         "mv_appsys.
      <ls_exp_event>-appobjtype     = ms_event_info-appobjtype.     "is_aotype-aot_type.
      <ls_exp_event>-appobjid       = ms_event_info-appobjid.       "is_likp-vbeln.
      <ls_exp_event>-language       = ms_event_info-language.       "sy-langu.
      <ls_exp_event>-evt_exp_tzone  = ms_event_info-evt_exp_tzone.  "lcl_tools=>get_system_time_zone(  ).
    ENDLOOP.

    et_exp_event  = lt_exp_event.
  ENDMETHOD.

  METHOD get_shipments_for_delivery.
    DATA: ls_comwa6 TYPE vbco6,
          lt_vbfas  TYPE STANDARD TABLE OF vbfas.

    CLEAR: et_tknum[].

    ls_comwa6-vbeln   = iv_vbeln.

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

        APPEND <ls_vbfas>-vbeln TO et_tknum.
      ENDLOOP.

      SORT et_tknum.
      DELETE ADJACENT DUPLICATES FROM et_tknum.
    ENDIF.
  ENDMETHOD.

  METHOD get_stops_info_for_delivery.
    DATA: lt_tknum      TYPE tt_tknum,
          ls_stops_info TYPE ts_stops_info.

    CLEAR: et_stops_info.

    get_shipments_for_delivery(
      EXPORTING
        iv_vbeln = iv_vbeln
      IMPORTING
        et_tknum = lt_tknum ).

    LOOP AT lt_tknum ASSIGNING FIELD-SYMBOL(<lv_tknum>).
      CLEAR: ls_stops_info.

      ls_stops_info-tknum   = <lv_tknum>.

      lcl_sh_tools=>get_stops_from_shipment(
        EXPORTING
          iv_tknum              = ls_stops_info-tknum
        IMPORTING
          et_stops              = ls_stops_info-stops
          et_dlv_watching_stops = ls_stops_info-watching ).

      APPEND ls_stops_info TO et_stops_info.
    ENDLOOP.
  ENDMETHOD.

  METHOD is_pod_relevant.
    CLEAR: rv_result.

    IF mv_vbeln IS NOT INITIAL.
      READ TABLE mt_lips TRANSPORTING NO FIELDS
        WITH KEY vbeln = mv_vbeln
                 werks = iv_locid.
    ELSE.
      READ TABLE mt_lips TRANSPORTING NO FIELDS
        WITH KEY werks = iv_locid.
    ENDIF.

    IF sy-subrc = 0.
      SELECT SINGLE z_pdstk INTO rv_result
        FROM zpof_gtt_ee_rel
        WHERE appobjid  = ms_event_info-appobjid.

      rv_result   = boolc( sy-subrc = 0 AND rv_result = abap_true ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Planned Events of Purchase Order Item ****************************
**********************************************************************
CLASS lcl_pe_filler_po_item DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pe_filler.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters
        io_bo_reader     TYPE REF TO lif_bo_reader.

  PRIVATE SECTION.
    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters,
          mo_bo_reader     TYPE REF TO lif_bo_reader.

    METHODS add_confirmation_event
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS add_goods_receipt_event
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS get_delivery_datetime
      IMPORTING
        is_app_objects     TYPE trxas_appobj_ctab_wa
      RETURNING
        VALUE(rv_datetime) TYPE /saptrx/event_exp_datetime
      RAISING
        cx_udm_message.

    METHODS get_object_field_value
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
        iv_fieldname    TYPE clike
      RETURNING
        VALUE(rv_value) TYPE char50
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_pe_filler_po_item IMPLEMENTATION.
  METHOD add_confirmation_event.
    DATA: lv_kzabs    TYPE ekpo-kzabs.

    lv_kzabs    = lcl_tools=>get_field_of_structure(
                    ir_struct_data = is_app_objects-maintabref
                    iv_field_name  = 'KZABS' ).

    IF lv_kzabs IS NOT INITIAL.
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = lif_app_constants=>cs_milestone-po_confirmation
      ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_goods_receipt_event.
    DATA: lv_wepos TYPE ekpo-wepos,
          lv_loekz TYPE ekpo-loekz.

    lv_wepos   = lcl_tools=>get_field_of_structure(
                   ir_struct_data = is_app_objects-maintabref
                   iv_field_name  = 'WEPOS' ).

    IF lv_wepos IS NOT INITIAL.
      " clear expecting datetime and timezone when Item is marked as deleted
      " to avoid generation of unwanted GTTOverdue events
      lv_loekz   = lcl_tools=>get_field_of_structure(
                     ir_struct_data = is_app_objects-maintabref
                     iv_field_name  = 'LOEKZ' ).

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = lif_app_constants=>cs_milestone-po_goods_receipt
        evt_exp_tzone     = COND #( WHEN lv_loekz IS INITIAL
                                      THEN lcl_tools=>get_system_time_zone( ) )
        evt_exp_datetime  = COND #( WHEN lv_loekz IS INITIAL
                                      THEN get_delivery_datetime( is_app_objects = is_app_objects ) )
        locid1            = lcl_tools=>get_field_of_structure(
                                ir_struct_data = is_app_objects-maintabref
                                iv_field_name  = 'WERKS' )
        loctype           = lif_ef_constants=>cs_loc_types-plant
      ) ).

    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.
  ENDMETHOD.

  METHOD get_delivery_datetime.
    rv_datetime = lcl_tools=>get_local_timestamp(
                    iv_date = get_object_field_value(
                                is_app_objects = is_app_objects
                                iv_fieldname   = 'EINDT' )
                    iv_time = CONV t( '000000' ) ).
  ENDMETHOD.

  METHOD get_object_field_value.
    DATA: lr_data  TYPE REF TO data,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_value> TYPE any.

    lr_data = mo_bo_reader->get_data( is_app_object = is_app_objects ).

    ASSIGN lr_data->* TO <ls_data>.
    IF <ls_data> IS ASSIGNED.
      ASSIGN COMPONENT iv_fieldname OF STRUCTURE <ls_data> TO <lv_value>.
      IF <lv_value> IS ASSIGNED.
        rv_value = <lv_value>.
      ELSE.
        MESSAGE e001(zpof_gtt) WITH iv_fieldname 'po item' INTO lv_dummy ##NO_TEXT.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'po item' INTO lv_dummy ##NO_TEXT .
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_pe_filler~check_relevance.
    TYPES: tt_ekpo    TYPE STANDARD TABLE OF uekpo.

    DATA: lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <ls_ekpo_new> TYPE uekpo,
                   <lt_ekpo_old> TYPE tt_ekpo,
                   <ls_ekpo_old> TYPE uekpo.

    rv_result = lif_ef_constants=>cs_condition-false.

    IF lcl_po_tools=>is_appropriate_po_type( ir_ekko = is_app_objects-mastertabref ) = abap_true AND
       lcl_po_tools=>is_appropriate_po_item( ir_ekpo = is_app_objects-maintabref ) = abap_true.

      IF is_app_objects-update_indicator = lif_ef_constants=>cs_change_mode-insert.
        rv_result = lif_ef_constants=>cs_condition-true.
      ELSE.
        DATA(lr_ekpo) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-po_item_old ).

        ASSIGN is_app_objects-maintabref->* TO <ls_ekpo_new>.
        ASSIGN lr_ekpo->* TO <lt_ekpo_old>.

        IF <ls_ekpo_new> IS ASSIGNED AND
           <lt_ekpo_old> IS ASSIGNED AND
           ( <ls_ekpo_new>-kz = lif_ef_constants=>cs_change_mode-update OR
             <ls_ekpo_new>-kz = lif_ef_constants=>cs_change_mode-undefined ).

          READ TABLE <lt_ekpo_old> ASSIGNING <ls_ekpo_old>
            WITH KEY ebeln = <ls_ekpo_new>-ebeln
                     ebelp = <ls_ekpo_new>-ebelp.
          IF sy-subrc = 0.
            rv_result = COND #( WHEN <ls_ekpo_new>-kzabs <> <ls_ekpo_old>-kzabs OR
                                     <ls_ekpo_new>-wepos <> <ls_ekpo_old>-wepos OR
                                     <ls_ekpo_new>-loekz <> <ls_ekpo_old>-loekz
                                  THEN lif_ef_constants=>cs_condition-true
                                  ELSE lif_ef_constants=>cs_condition-false ).
          ELSE.
            MESSAGE e005(zpof_gtt)
              WITH 'EKPO' |{ <ls_ekpo_new>-ebeln }{ <ls_ekpo_new>-ebelp }|
              INTO lv_dummy.
            lcl_tools=>throw_exception( ).
          ENDIF.
        ELSE.
          MESSAGE e002(zpof_gtt) WITH 'EKPO' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD lif_pe_filler~get_planed_events.
    add_confirmation_event(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_goods_receipt_event(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Planned Events Inbound Delivery Header ***************************
**********************************************************************
CLASS lcl_pe_filler_dl_header DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pe_filler.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters
        io_bo_reader     TYPE REF TO lif_bo_reader.

  PRIVATE SECTION.
    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters,
          mo_bo_reader     TYPE REF TO lif_bo_reader.

    METHODS add_goods_receipt_event
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
        io_relevance    TYPE REF TO lcl_dl_event_relevance_main
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS add_shipment_events
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS is_time_of_delivery_changed
      IMPORTING
        is_app_objects   TYPE trxas_appobj_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_pe_filler_dl_header IMPLEMENTATION.
  METHOD add_goods_receipt_event.
    IF io_relevance->is_enabled(
         iv_milestone   = lif_app_constants=>cs_milestone-dl_goods_receipt ) = abap_true.

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = lif_app_constants=>cs_milestone-dl_goods_receipt
        evt_exp_tzone     = lcl_tools=>get_system_time_zone( )
        evt_exp_datetime  = lcl_dl_tools=>get_delivery_date(
                              ir_data = is_app_objects-maintabref )
      ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_shipment_events.
    DATA: lt_expeventdata  TYPE lif_ef_types=>tt_expeventdata.

    DATA(lv_vbeln)            = CONV vbeln_vl( lcl_tools=>get_field_of_structure(
                                                 ir_struct_data = is_app_objects-maintabref
                                                 iv_field_name  = 'VBELN' ) ).

    DATA(lo_sh_stops_events)  = lcl_sh_stops_events=>get_instance_for_delivery(
                                  iv_vbeln         = lv_vbeln
                                  iv_appobjid      = is_app_objects-appobjid
                                  io_ef_parameters = mo_ef_parameters ).

    lo_sh_stops_events->get_planned_events(
      IMPORTING
        et_exp_event = lt_expeventdata ).

    ct_expeventdata   = VALUE #( BASE ct_expeventdata
                                 ( LINES OF lt_expeventdata ) ).
  ENDMETHOD.

  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.
  ENDMETHOD.

  METHOD is_time_of_delivery_changed.
    TYPES: tt_likp    TYPE STANDARD TABLE OF likpvb.

    DATA: lv_vbeln     TYPE likp-vbeln,
          lv_lfuhr_new TYPE lfuhr,
          lv_lfuhr_old TYPE lfuhr.

    lv_lfuhr_new  = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_app_objects-maintabref
                      iv_field_name  = 'LFUHR' ).

    DATA(lr_likp)  = mo_ef_parameters->get_appl_table(
                       iv_tabledef = lif_app_constants=>cs_tabledef-dl_header_old ).

    FIELD-SYMBOLS: <lt_likp> TYPE tt_likp.

    IF lr_likp IS BOUND.
      ASSIGN lr_likp->* TO <lt_likp>.

      IF <lt_likp> IS ASSIGNED.
        lv_vbeln  = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_app_objects-maintabref
                      iv_field_name  = 'VBELN' ).

        READ TABLE <lt_likp> ASSIGNING FIELD-SYMBOL(<ls_likp>)
          WITH KEY vbeln = lv_vbeln.

        lv_lfuhr_old  = COND #( WHEN sy-subrc = 0 THEN <ls_likp>-lfuhr ).
      ENDIF.
    ENDIF.

    rv_result   = boolc( lv_lfuhr_new <> lv_lfuhr_old ).
  ENDMETHOD.

  METHOD lif_pe_filler~check_relevance.
    TYPES: tt_milestones    TYPE STANDARD TABLE OF /saptrx/appl_event_tag
                              WITH EMPTY KEY.

    rv_result = lif_ef_constants=>cs_condition-false.

    IF lcl_dl_tools=>is_appropriate_dl_type( ir_struct = is_app_objects-maintabref ) = abap_true.

      IF is_time_of_delivery_changed( is_app_objects = is_app_objects ) = abap_true.
        rv_result = lif_ef_constants=>cs_condition-true.

      ELSE.
        DATA(lo_relevance_old)  = NEW lcl_dl_event_relevance_header(
                                        io_ef_parameters = mo_ef_parameters
                                        is_app_objects   = VALUE #(
                                                             appobjid = is_app_objects-appobjid ) ).

        DATA(lo_relevance_new)  = NEW lcl_dl_event_relevance_header(
                                        io_ef_parameters = mo_ef_parameters
                                        is_app_objects   = is_app_objects ).

        DATA(lv_milestone)      = lif_app_constants=>cs_milestone-dl_goods_receipt.

        rv_result = boolc( lo_relevance_old->is_enabled( iv_milestone = lv_milestone ) <>
                           lo_relevance_new->is_enabled( iv_milestone = lv_milestone ) ).

        rv_result = COND #( WHEN rv_result = abap_true
                              THEN lif_ef_constants=>cs_condition-true
                              ELSE lif_ef_constants=>cs_condition-false ).
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD lif_pe_filler~get_planed_events.
    DATA(lo_relevance)  = NEW lcl_dl_event_relevance_header(
                            io_ef_parameters = mo_ef_parameters
                            is_app_objects   = is_app_objects ).

    " initiate relevance flags
    lo_relevance->initiate( ).

    " store calculated relevance flags
    lo_relevance->update( ).

    add_goods_receipt_event(
      EXPORTING
        is_app_objects  = is_app_objects
        io_relevance    = lo_relevance
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_shipment_events(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Planned Events Inbound Delivery Item *****************************
**********************************************************************
CLASS lcl_pe_filler_dl_item DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pe_filler.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters
        io_bo_reader     TYPE REF TO lif_bo_reader.

  PRIVATE SECTION.
    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters,
          mo_bo_reader     TYPE REF TO lif_bo_reader.

    METHODS add_goods_receipt_event
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
        io_relevance    TYPE REF TO lcl_dl_event_relevance_main
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS add_packing_event
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
        io_relevance    TYPE REF TO lcl_dl_event_relevance_main
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS add_put_away_event
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
        io_relevance    TYPE REF TO lcl_dl_event_relevance_main
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS add_shipment_events
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS is_time_of_delivery_changed
      IMPORTING
        is_app_objects   TYPE trxas_appobj_ctab_wa
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_pe_filler_dl_item IMPLEMENTATION.
  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.
  ENDMETHOD.

  METHOD add_goods_receipt_event.
    IF io_relevance->is_enabled(
         iv_milestone   = lif_app_constants=>cs_milestone-dl_goods_receipt ) = abap_true.

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = lif_app_constants=>cs_milestone-dl_goods_receipt
        evt_exp_tzone     = lcl_tools=>get_system_time_zone( )
        evt_exp_datetime  = lcl_dl_tools=>get_delivery_date(
                              ir_data = is_app_objects-mastertabref )
        locid1            = lcl_tools=>get_field_of_structure(
                              ir_struct_data = is_app_objects-maintabref
                              iv_field_name  = 'WERKS' )
        loctype           = lif_ef_constants=>cs_loc_types-plant
      ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_packing_event.
    IF io_relevance->is_enabled(
         iv_milestone   = lif_app_constants=>cs_milestone-dl_packing ) = abap_true.

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = lif_app_constants=>cs_milestone-dl_packing
        evt_exp_tzone     = lcl_tools=>get_system_time_zone( )
        locid1            = lcl_tools=>get_field_of_structure(
                              ir_struct_data = is_app_objects-maintabref
                              iv_field_name  = 'WERKS' )
        loctype           = lif_ef_constants=>cs_loc_types-plant
      ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_put_away_event.
    IF io_relevance->is_enabled(
         iv_milestone   = lif_app_constants=>cs_milestone-dl_put_away ) = abap_true.
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = lif_app_constants=>cs_milestone-dl_put_away
        evt_exp_tzone     = lcl_tools=>get_system_time_zone( )
        evt_exp_datetime  = lcl_dl_tools=>get_delivery_date(
                              ir_data = is_app_objects-mastertabref )
        locid1            = lcl_tools=>get_field_of_structure(
                              ir_struct_data = is_app_objects-maintabref
                              iv_field_name  = 'WERKS' )
        loctype           = lif_ef_constants=>cs_loc_types-plant
      ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_shipment_events.
    DATA: lt_expeventdata  TYPE lif_ef_types=>tt_expeventdata.

    DATA(lv_vbeln)            = CONV vbeln_vl( lcl_tools=>get_field_of_structure(
                                                 ir_struct_data = is_app_objects-maintabref
                                                 iv_field_name  = 'VBELN' ) ).

    DATA(lo_sh_stops_events)  = lcl_sh_stops_events=>get_instance_for_delivery(
                                  iv_vbeln         = lv_vbeln
                                  iv_appobjid      = is_app_objects-appobjid
                                  io_ef_parameters = mo_ef_parameters ).

    lo_sh_stops_events->get_planned_events(
      IMPORTING
        et_exp_event = lt_expeventdata ).

    ct_expeventdata   = VALUE #( BASE ct_expeventdata
                                 ( LINES OF lt_expeventdata ) ).
  ENDMETHOD.

  METHOD is_time_of_delivery_changed.
    TYPES: tt_likp    TYPE STANDARD TABLE OF likpvb.

    DATA: lv_vbeln     TYPE likp-vbeln,
          lv_lfuhr_new TYPE lfuhr,
          lv_lfuhr_old TYPE lfuhr.

    lv_lfuhr_new  = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_app_objects-mastertabref
                      iv_field_name  = 'LFUHR' ).

    DATA(lr_likp)  = mo_ef_parameters->get_appl_table(
                       iv_tabledef = lif_app_constants=>cs_tabledef-dl_header_old ).

    FIELD-SYMBOLS: <lt_likp> TYPE tt_likp.

    IF lr_likp IS BOUND.
      ASSIGN lr_likp->* TO <lt_likp>.

      IF <lt_likp> IS ASSIGNED.
        lv_vbeln  = lcl_tools=>get_field_of_structure(
                      ir_struct_data = is_app_objects-mastertabref
                      iv_field_name  = 'VBELN' ).

        READ TABLE <lt_likp> ASSIGNING FIELD-SYMBOL(<ls_likp>)
          WITH KEY vbeln = lv_vbeln.

        lv_lfuhr_old  = COND #( WHEN sy-subrc = 0 THEN <ls_likp>-lfuhr ).
      ENDIF.
    ENDIF.

    rv_result   = boolc( lv_lfuhr_new <> lv_lfuhr_old ).
  ENDMETHOD.

  METHOD lif_pe_filler~check_relevance.
    TYPES: tt_milestones    TYPE STANDARD TABLE OF /saptrx/appl_event_tag
                              WITH EMPTY KEY.

    rv_result = lif_ef_constants=>cs_condition-false.

    IF lcl_dl_tools=>is_appropriate_dl_type( ir_struct = is_app_objects-mastertabref ) = abap_true AND
       lcl_dl_tools=>is_appropriate_dl_item( ir_struct = is_app_objects-maintabref ) = abap_true.

      IF is_time_of_delivery_changed( is_app_objects = is_app_objects ) = abap_true.
        rv_result = lif_ef_constants=>cs_condition-true.

      ELSE.
        DATA(lo_relevance_old)  = NEW lcl_dl_event_relevance_item(
                                        io_ef_parameters = mo_ef_parameters ).

        DATA(lo_relevance_new)  = NEW lcl_dl_event_relevance_item(
                                        io_ef_parameters = mo_ef_parameters
                                        is_app_objects   = is_app_objects ).

        DATA(lt_milestones)     = VALUE tt_milestones(
          ( lif_app_constants=>cs_milestone-dl_goods_receipt )
          ( lif_app_constants=>cs_milestone-dl_packing )
          ( lif_app_constants=>cs_milestone-dl_put_away )
        ).

        rv_result = lif_ef_constants=>cs_condition-false.

        LOOP AT lt_milestones ASSIGNING FIELD-SYMBOL(<lv_milestone>).
          IF lo_relevance_old->is_enabled( iv_milestone = <lv_milestone> ) <>
               lo_relevance_new->is_enabled( iv_milestone = <lv_milestone> ).

            rv_result = lif_ef_constants=>cs_condition-true.
            EXIT.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD lif_pe_filler~get_planed_events.
    DATA(lo_relevance)  = NEW lcl_dl_event_relevance_item(
                            io_ef_parameters = mo_ef_parameters
                            is_app_objects   = is_app_objects ).

    " initiate relevance flags
    lo_relevance->initiate( ).

    " store calculated relevance flags
    lo_relevance->update( ).

    add_put_away_event(
      EXPORTING
        is_app_objects  = is_app_objects
        io_relevance    = lo_relevance
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_packing_event(
      EXPORTING
        is_app_objects  = is_app_objects
        io_relevance    = lo_relevance
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_goods_receipt_event(
      EXPORTING
        is_app_objects  = is_app_objects
        io_relevance    = lo_relevance
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_shipment_events(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).
  ENDMETHOD.
ENDCLASS.

**********************************************************************
*** Planned Events Shipment Header ***********************************
**********************************************************************
CLASS lcl_pe_filler_sh_header DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pe_filler.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters
        io_bo_reader     TYPE REF TO lif_bo_reader
      RAISING
        cx_udm_message.

  PRIVATE SECTION.
    TYPES: tt_vbeln    TYPE RANGE OF lips-vbeln,
           tt_werks    TYPE RANGE OF lips-werks,
           tt_appobjid TYPE RANGE OF /saptrx/aoid.

    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters,
          mo_bo_reader     TYPE REF TO lif_bo_reader,
          mo_sh_data_old   TYPE REF TO lcl_sh_data_old.

    METHODS add_shipment_events
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
        ct_measrmntdata TYPE lif_ef_types=>tt_measrmntdata
        ct_infodata     TYPE lif_ef_types=>tt_infodata
      RAISING
        cx_udm_message.

    METHODS add_stops_events
      IMPORTING
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
        ct_measrmntdata TYPE lif_ef_types=>tt_measrmntdata
        ct_infodata     TYPE lif_ef_types=>tt_infodata
      RAISING
        cx_udm_message.

    METHODS get_shippment_header
      IMPORTING
        is_app_object  TYPE trxas_appobj_ctab_wa
        ir_vttk        TYPE REF TO data
      RETURNING
        VALUE(rr_vttk) TYPE REF TO data
      RAISING
        cx_udm_message.

    METHODS is_pod_relevant
      IMPORTING
        is_stops         TYPE lif_app_types=>ts_stops
        it_vttp          TYPE vttpvb_tab
        it_vtsp          TYPE vtspvb_tab
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    METHODS is_stop_changed
      IMPORTING
        is_app_object    TYPE trxas_appobj_ctab_wa
        it_fields        TYPE lif_ef_types=>tt_field_name
      RETURNING
        VALUE(rv_result) TYPE lif_ef_types=>tv_condition
      RAISING
        cx_udm_message.

    METHODS get_corresponding_dlv_items
      IMPORTING
        it_vbeln    TYPE tt_vbeln
        it_werks    TYPE tt_werks
      EXPORTING
        et_appobjid TYPE tt_appobjid
      RAISING
        cx_udm_message.

    METHODS get_header_fields
      EXPORTING
        et_fields TYPE lif_ef_types=>tt_field_name.

    METHODS get_stop_fields
      EXPORTING
        et_fields TYPE lif_ef_types=>tt_field_name.


ENDCLASS.

CLASS lcl_pe_filler_sh_header IMPLEMENTATION.
  METHOD add_shipment_events.
    FIELD-SYMBOLS: <ls_vttk>  TYPE vttkvb.

    ASSIGN is_app_objects-maintabref->* TO <ls_vttk>.

    IF <ls_vttk> IS ASSIGNED.
      " CHECK IN
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = lif_app_constants=>cs_milestone-sh_check_in
        evt_exp_datetime  = lcl_tools=>get_local_timestamp(
                              iv_date = <ls_vttk>-dpreg
                              iv_time = <ls_vttk>-upreg )
        evt_exp_tzone     = lcl_tools=>get_system_time_zone( )
      ) ).

      " LOAD START
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = lif_app_constants=>cs_milestone-sh_load_start
        evt_exp_datetime  = lcl_tools=>get_local_timestamp(
                              iv_date = <ls_vttk>-dplbg
                              iv_time = <ls_vttk>-uplbg )
        evt_exp_tzone     = lcl_tools=>get_system_time_zone( )
      ) ).

      " LOAD END
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = lif_app_constants=>cs_milestone-sh_load_end
        evt_exp_datetime  = lcl_tools=>get_local_timestamp(
                              iv_date = <ls_vttk>-dplen
                              iv_time = <ls_vttk>-uplen )
        evt_exp_tzone     = lcl_tools=>get_system_time_zone( )
      ) ).
    ENDIF.
  ENDMETHOD.

  METHOD add_stops_events.
    DATA(lv_tknum) = CONV tknum( lcl_tools=>get_field_of_structure(
                                   ir_struct_data = is_app_objects-maintabref
                                   iv_field_name  = 'TKNUM' ) ).

    DATA: lt_stops    TYPE lif_app_types=>tt_stops.

    FIELD-SYMBOLS: <lt_vttp> TYPE vttpvb_tab,
                   <lt_vtts> TYPE vttsvb_tab,
                   <lt_vtsp> TYPE vtspvb_tab.

    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
                      iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vtts) = mo_ef_parameters->get_appl_table(
                      iv_tabledef = lif_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtsp) = mo_ef_parameters->get_appl_table(
                      iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_stage_new ).

    ASSIGN lr_vtts->* TO <lt_vtts>.
    ASSIGN lr_vttp->* TO <lt_vttp>.
    ASSIGN lr_vtsp->* TO <lt_vtsp>.

    IF <lt_vtts> IS ASSIGNED AND
       <lt_vtsp> IS ASSIGNED AND
       <lt_vttp> IS ASSIGNED.

      lcl_sh_tools=>get_stops_from_shipment(
        EXPORTING
          iv_tknum              = lv_tknum
          it_vtts               = <lt_vtts>
          it_vtsp               = <lt_vtsp>
          it_vttp               = <lt_vttp>
        IMPORTING
          et_stops              = lt_stops ).

      LOOP AT lt_stops ASSIGNING FIELD-SYMBOL(<ls_stops>).
        " DEPARTURE / ARRIVAL
        ct_expeventdata = VALUE #( BASE ct_expeventdata (
          appsys            = mo_ef_parameters->get_appsys(  )
          appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
          language          = sy-langu
          appobjid          = is_app_objects-appobjid
          milestone         = COND #( WHEN <ls_stops>-loccat = lif_app_constants=>cs_loccat-departure
                                        THEN lif_app_constants=>cs_milestone-sh_departure
                                        ELSE lif_app_constants=>cs_milestone-sh_arrival )
          evt_exp_datetime  = <ls_stops>-pln_evt_datetime
          evt_exp_tzone     = <ls_stops>-pln_evt_timezone
          locid2            = <ls_stops>-stopid
          loctype           = <ls_stops>-loctype
          locid1            = <ls_stops>-locid
        ) ).

        IF is_pod_relevant( is_stops = <ls_stops>
                            it_vttp  = <lt_vttp>
                            it_vtsp  = <lt_vtsp> ) = abap_true.
          " POD
          ct_expeventdata = VALUE #( BASE ct_expeventdata (
            appsys            = mo_ef_parameters->get_appsys(  )
            appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
            language          = sy-langu
            appobjid          = is_app_objects-appobjid
            milestone         = lif_app_constants=>cs_milestone-sh_pod
            evt_exp_datetime  = <ls_stops>-pln_evt_datetime
            evt_exp_tzone     = <ls_stops>-pln_evt_timezone
            locid2            = <ls_stops>-stopid
            loctype           = <ls_stops>-loctype
            locid1            = <ls_stops>-locid
          ) ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'VTTS' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.
    mo_sh_data_old      = NEW lcl_sh_data_old(
                            io_ef_parameters = io_ef_parameters ).
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
      MESSAGE e002(zpof_gtt) WITH 'VTTK'
        INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD is_pod_relevant.
    DATA: lt_vbeln    TYPE RANGE OF lips-vbeln,
          lt_appobjid TYPE RANGE OF /saptrx/aoid,
          lv_locid    TYPE lif_app_types=>tv_locid,
          lv_pdstk    TYPE pdstk.

    rv_result = abap_false.

    IF is_stops-loccat  = lif_app_constants=>cs_loccat-arrival AND
       is_stops-loctype = lif_ef_constants=>cs_loc_types-plant.

      " get Inbound Delivery Numbers
      LOOP AT it_vtsp ASSIGNING FIELD-SYMBOL(<ls_vtsp>)
        WHERE tknum = is_stops-tknum
          AND tsnum = is_stops-tsnum.

        READ TABLE it_vttp ASSIGNING FIELD-SYMBOL(<ls_vttp>)
          WITH KEY tknum = <ls_vtsp>-tknum
                   tpnum = <ls_vtsp>-tpnum.

        IF sy-subrc = 0.
          lt_vbeln[]    = VALUE #( BASE lt_vbeln
                                  ( option = 'EQ'
                                    sign   = 'I'
                                    low    = <ls_vttp>-vbeln ) ).
        ENDIF.
      ENDLOOP.

      " get appobjid range (inbound deliveries for corresponding Plant)
      IF lt_vbeln[] IS NOT INITIAL.
        get_corresponding_dlv_items(
          EXPORTING
            it_vbeln    = lt_vbeln
            it_werks    = VALUE #( ( low = is_stops-locid
                                     option = 'EQ'
                                     sign   = 'I'  ) )
          IMPORTING
            et_appobjid = lt_appobjid ).
      ENDIF.

      " get POD enabled flags for found DLV Items
      IF lt_appobjid[] IS NOT INITIAL.
        SELECT SINGLE z_pdstk                           "#EC CI_NOORDER
          INTO rv_result
          FROM zpof_gtt_ee_rel
          WHERE appobjid IN lt_appobjid
            AND z_pdstk   = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD is_stop_changed.
    FIELD-SYMBOLS: <lt_vtts_new> TYPE lif_app_types=>tt_vttsvb,
                   <lt_vtts_old> TYPE lif_app_types=>tt_vttsvb.

    DATA(lv_tknum)    = CONV tknum( lcl_tools=>get_field_of_structure(
                                      ir_struct_data = is_app_object-maintabref
                                      iv_field_name  = 'TKNUM' ) ).

    DATA(lr_vtts_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtts_old) = mo_sh_data_old->get_vtts( ).

    rv_result   = lif_ef_constants=>cs_condition-false.

    ASSIGN lr_vtts_new->* TO <lt_vtts_new>.
    ASSIGN lr_vtts_old->* TO <lt_vtts_old>.

    IF <lt_vtts_new> IS ASSIGNED AND
       <lt_vtts_old> IS ASSIGNED.

      LOOP AT <lt_vtts_new> ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
        WHERE tknum = lv_tknum
          AND updkz IS NOT INITIAL.

        CASE <ls_vtts_new>-updkz.
          WHEN lif_ef_constants=>cs_change_mode-insert.
            rv_result   = lif_ef_constants=>cs_condition-true.

          WHEN lif_ef_constants=>cs_change_mode-update OR
               lif_ef_constants=>cs_change_mode-undefined.

            READ TABLE <lt_vtts_old> ASSIGNING FIELD-SYMBOL(<ls_vtts_old>)
              WITH KEY tknum  = <ls_vtts_new>-tknum
                       tsnum  = <ls_vtts_new>-tsnum.

            rv_result   = lcl_tools=>are_fields_different(
                  ir_data1  = REF #( <ls_vtts_new> )
                  ir_data2  = REF #( <ls_vtts_old> )
                  it_fields = it_fields ).
        ENDCASE.

        IF rv_result   = lif_ef_constants=>cs_condition-true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF rv_result   = lif_ef_constants=>cs_condition-false.
        LOOP AT <lt_vtts_old> TRANSPORTING NO FIELDS
          WHERE tknum = lv_tknum
            AND updkz = lif_ef_constants=>cs_change_mode-delete.

          rv_result   = lif_ef_constants=>cs_condition-true.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_corresponding_dlv_items.
    DATA(lr_lips)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef = lif_app_constants=>cs_tabledef-sh_delivery_item ).

    FIELD-SYMBOLS: <lt_lips> TYPE vtrlp_tab.

    CLEAR: et_appobjid[].

    ASSIGN lr_lips->* TO <lt_lips>.

    IF <lt_lips> IS ASSIGNED.
      LOOP AT <lt_lips> ASSIGNING FIELD-SYMBOL(<ls_lips>)
        WHERE vbeln IN it_vbeln
          AND werks IN it_werks.

        et_appobjid = VALUE #( BASE et_appobjid (
                        low = |{ <ls_lips>-vbeln }{ <ls_lips>-posnr }|
                        option = 'EQ'
                        sign = 'I'
                      ) ).
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'LIPS' INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_header_fields.
    et_fields   = VALUE #( ( 'DPREG' ) ( 'UPREG' )
                           ( 'DPLBG' ) ( 'UPLBG' )
                           ( 'DPLEN' ) ( 'UPLEN' ) ).
  ENDMETHOD.

  METHOD get_stop_fields.
    et_fields   = VALUE #( ( 'DPTBG' ) ( 'UPTBG' )
                           ( 'DPTEN' ) ( 'UPTEN' )
                           ( 'KUNNA' ) ( 'KUNNZ' )
                           ( 'VSTEL' ) ( 'VSTEZ' )
                           ( 'LIFNA' ) ( 'LIFNZ' )
                           ( 'WERKA' ) ( 'WERKZ' )
                           ( 'KNOTA' ) ( 'KNOTZ' ) ).
  ENDMETHOD.

  METHOD lif_pe_filler~check_relevance.
    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = lif_app_constants=>cs_tabledef-sh_item_new ).

    rv_result = lif_ef_constants=>cs_condition-false.

    " check the fields, used in PE extractor and not used in TP extractor
    IF lcl_sh_tools=>is_appropriate_type( ir_vttk = is_app_objects-maintabref ) = abap_true AND
       lcl_sh_tools=>is_delivery_assigned( ir_vttp = lr_vttp ) = abap_true.

      " check in, load start, load end
      get_header_fields(
        IMPORTING
          et_fields = DATA(lt_header_fields) ).

      rv_result = lcl_tools=>are_fields_different(
                    ir_data1  = is_app_objects-maintabref
                    ir_data2  = get_shippment_header(
                                  is_app_object = is_app_objects
                                  ir_vttk       = mo_sh_data_old->get_vttk( ) )
                    it_fields = lt_header_fields ).

      " departure, arrival
      IF rv_result = lif_ef_constants=>cs_condition-false.
        get_stop_fields(
          IMPORTING
            et_fields = DATA(lt_stop_fields) ).

        rv_result = is_stop_changed(
                      is_app_object = is_app_objects
                      it_fields     = lt_stop_fields ).

      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD lif_pe_filler~get_planed_events.
    add_shipment_events(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata ).

    add_stops_events(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata ).

    IF NOT line_exists( ct_expeventdata[ appobjid = is_app_objects-appobjid ] ).
      " planned events DELETION
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = ''
        evt_exp_datetime  = '000000000000000'
        evt_exp_tzone     = ''
      ) ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
