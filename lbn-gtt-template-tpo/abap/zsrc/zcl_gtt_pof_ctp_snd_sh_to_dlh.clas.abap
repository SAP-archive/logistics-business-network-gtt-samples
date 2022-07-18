CLASS zcl_gtt_pof_ctp_snd_sh_to_dlh DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_pof_ctp_snd
  CREATE PUBLIC .

PUBLIC SECTION.

  CLASS-METHODS get_instance
    RETURNING
      VALUE(ro_updater) TYPE REF TO zcl_gtt_pof_ctp_snd_sh_to_dlh
    RAISING
      cx_udm_message .
  METHODS prepare_idoc_data
    IMPORTING
      !io_ship_data TYPE REF TO zcl_gtt_pof_ctp_shipment_data
    RAISING
      cx_udm_message .
PROTECTED SECTION.

  METHODS get_aotype_restrictions
    REDEFINITION .
  METHODS get_object_type
    REDEFINITION .
PRIVATE SECTION.

  CONSTANTS:
    BEGIN OF cs_mapping,
                 vbeln                 TYPE /saptrx/paramname VALUE 'YN_DLV_NO',
                 shp_count             TYPE /saptrx/paramname VALUE 'YN_SHP_LINE_COUNT',
                 shp_tknum             TYPE /saptrx/paramname VALUE 'YN_SHP_NO',
                 shp_fstop             TYPE /saptrx/paramname VALUE 'YN_SHP_FIRST_STOP',
                 shp_lstop             TYPE /saptrx/paramname VALUE 'YN_SHP_LAST_STOP',
                 shp_lstop_rec_loc     TYPE /saptrx/paramname VALUE 'YN_SHP_LAST_STOP_REC_LOC',
                 shp_lstop_rec_loc_typ TYPE /saptrx/paramname VALUE 'YN_SHP_LAST_STOP_REC_LOC_TYP',
                 pod_relevant          TYPE /saptrx/paramname VALUE 'YN_DL_POD_RELEVANT',
               END OF cs_mapping .

  METHODS fill_idoc_appobj_ctabs
    IMPORTING
      !is_aotype TYPE ts_aotype
      !iv_vbeln TYPE likp-vbeln
    CHANGING
      !cs_idoc_data TYPE ts_idoc_data .
  METHODS fill_idoc_control_data
    IMPORTING
      !is_aotype TYPE ts_aotype
      !is_ship TYPE zcl_gtt_pof_ctp_shipment_data=>ts_shipment_merge
      !is_likp TYPE zcl_gtt_pof_ctp_shipment_data=>ts_likpex
      !is_stops TYPE zcl_gtt_pof_ctp_shipment_data=>ts_stops
    CHANGING
      !cs_idoc_data TYPE ts_idoc_data
    RAISING
      cx_udm_message .
  METHODS fill_idoc_exp_event
    IMPORTING
      !is_aotype TYPE ts_aotype
      !is_ship TYPE zcl_gtt_pof_ctp_shipment_data=>ts_shipment_merge
      !is_likp TYPE zcl_gtt_pof_ctp_shipment_data=>ts_likpex
      !is_stops TYPE zcl_gtt_pof_ctp_shipment_data=>ts_stops
    CHANGING
      !cs_idoc_data TYPE ts_idoc_data
    RAISING
      cx_udm_message .
  METHODS fill_idoc_tracking_id
    IMPORTING
      !is_aotype TYPE ts_aotype
      !is_ship TYPE zcl_gtt_pof_ctp_shipment_data=>ts_shipment_merge
      !is_likp TYPE zcl_gtt_pof_ctp_shipment_data=>ts_likpex
    CHANGING
      !cs_idoc_data TYPE ts_idoc_data
    RAISING
      cx_udm_message .
  METHODS get_delivery_head_planned_evt
    IMPORTING
      !is_aotype TYPE ts_aotype
      !is_likp TYPE zif_gtt_pof_app_types=>ts_likpvb
      !it_lips TYPE zif_gtt_pof_app_types=>tt_lipsvb
    EXPORTING
      !et_exp_event TYPE /saptrx/bapi_trk_ee_tab
    RAISING
      cx_udm_message .
  METHODS get_delivery_head_tracking_id
    IMPORTING
      !is_likp TYPE zif_gtt_pof_app_types=>ts_likpvb
    RETURNING
      VALUE(rv_tracking_id) TYPE /saptrx/trxid .
  METHODS get_shipment_stop
    IMPORTING
      !is_stops TYPE zcl_gtt_pof_ctp_shipment_data=>ts_stops
      !is_vbfa TYPE zcl_gtt_pof_ctp_shipment_data=>ts_vbfaex
      !iv_arrival TYPE abap_bool
    EXPORTING
      !es_stop TYPE zif_gtt_pof_app_types=>ts_stops
    RETURNING
      VALUE(rv_stopid) TYPE zif_gtt_pof_app_types=>tv_stopid
    RAISING
      cx_udm_message .
  METHODS is_pod_relevant_delivery
    IMPORTING
      !is_ship TYPE zcl_gtt_pof_ctp_shipment_data=>ts_shipment_merge
      !is_likp TYPE zcl_gtt_pof_ctp_shipment_data=>ts_likpex
      !it_stops TYPE zif_gtt_pof_app_types=>tt_stops
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_pof_ef_types=>tv_condition .
  METHODS is_pod_relevant_stop
    IMPORTING
      !is_ship TYPE zcl_gtt_pof_ctp_shipment_data=>ts_shipment_merge
      !is_likp TYPE zcl_gtt_pof_ctp_shipment_data=>ts_likpex
      !is_stops TYPE zif_gtt_pof_app_types=>ts_stops
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
ENDCLASS.



CLASS ZCL_GTT_POF_CTP_SND_SH_TO_DLH IMPLEMENTATION.


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
        paramname = zif_gtt_pof_ef_constants=>cs_system_fields-actual_bisiness_timezone
        value     = zcl_gtt_pof_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_pof_ef_constants=>cs_system_fields-actual_bisiness_datetime
        value     = |0{ sy-datum }{ sy-uzeit }|
      )
      (
        paramname = zif_gtt_pof_ef_constants=>cs_system_fields-actual_technical_timezone
        value     = zcl_gtt_pof_tools=>get_system_time_zone( )
      )
      (
        paramname = zif_gtt_pof_ef_constants=>cs_system_fields-actual_technical_datetime
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

    DATA: lt_exp_event TYPE /saptrx/bapi_trk_ee_tab,
          lt_exp_event_dlv  TYPE /saptrx/bapi_trk_ee_tab,
          lv_milestonenum   TYPE /saptrx/seq_num VALUE 1,
          lv_tknum          TYPE tknum.

    get_delivery_head_planned_evt(
      EXPORTING
        is_aotype    = is_aotype
        is_likp      = CORRESPONDING #( is_likp )
        it_lips      = CORRESPONDING #( is_ship-lips )
      IMPORTING
        et_exp_event = lt_exp_event_dlv ).


    LOOP AT is_stops-watching ASSIGNING FIELD-SYMBOL(<ls_watching>)
      WHERE vbeln = is_likp-vbeln.

      IF lv_tknum <> <ls_watching>-stopid(10).
        lv_tknum        = <ls_watching>-stopid(10).
        lv_milestonenum = 1.
      ENDIF.

      READ TABLE is_stops-stops ASSIGNING FIELD-SYMBOL(<ls_stops>)
        WITH KEY stopid = <ls_watching>-stopid
                 loccat = <ls_watching>-loccat.

      IF sy-subrc = 0.
        " Departure / Arrival
        lt_exp_event = VALUE #( BASE lt_exp_event (
            milestone         = COND #( WHEN <ls_watching>-loccat = zif_gtt_pof_app_constants=>cs_loccat-departure
                                          THEN zif_gtt_pof_app_constants=>cs_milestone-sh_departure
                                          ELSE zif_gtt_pof_app_constants=>cs_milestone-sh_arrival )
            locid2            = <ls_stops>-stopid
            loctype           = <ls_stops>-loctype
            locid1            = <ls_stops>-locid
            evt_exp_datetime  = <ls_stops>-pln_evt_datetime
            evt_exp_tzone     = <ls_stops>-pln_evt_timezone
            milestonenum      = lv_milestonenum
        ) ).
        ADD 1 TO lv_milestonenum.

        " POD
        IF <ls_stops>-loccat  = zif_gtt_pof_app_constants=>cs_loccat-arrival AND
           <ls_stops>-loctype = zif_gtt_pof_ef_constants=>cs_loc_types-plant AND
           is_pod_relevant_stop( is_ship  = is_ship
                                 is_likp  = is_likp
                                 is_stops = <ls_stops> ) = abap_true.

          lt_exp_event = VALUE #( BASE lt_exp_event (
              milestone         = zif_gtt_pof_app_constants=>cs_milestone-sh_pod
              locid2            = <ls_stops>-stopid
              loctype           = <ls_stops>-loctype
              locid1            = <ls_stops>-locid
              evt_exp_datetime  = <ls_stops>-pln_evt_datetime
              evt_exp_tzone     = <ls_stops>-pln_evt_timezone
              milestonenum      = lv_milestonenum
          ) ).

          ADD 1 TO lv_milestonenum.
        ENDIF.
      ELSE.
        MESSAGE e005(zgtt_pof)
          WITH |{ <ls_watching>-stopid }{ <ls_watching>-loccat }| 'STOPS'
          INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ENDLOOP.

    " fill sequence number in DLV events
    lv_milestonenum   = zcl_gtt_pof_tools=>get_next_sequence_id(
                          it_expeventdata = lt_exp_event ).

    LOOP AT lt_exp_event_dlv ASSIGNING FIELD-SYMBOL(<ls_exp_event_dlv>).
      <ls_exp_event_dlv>-milestonenum   = lv_milestonenum.
      ADD 1 TO lv_milestonenum.
    ENDLOOP.

    lt_exp_event    = VALUE #( BASE lt_exp_event
                                ( LINES OF lt_exp_event_dlv ) ).

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
      IF <ls_exp_event>-evt_exp_tzone IS INITIAL.
      <ls_exp_event>-evt_exp_tzone  = zcl_gtt_pof_tools=>get_system_time_zone(  ).
      ENDIF.
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
      trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-dl_number
      trxid       = is_likp-vbeln
      timzon      = zcl_gtt_pof_tools=>get_system_time_zone( )
    ) ).

    " Shipment
    LOOP AT is_ship-likp\likp_dlt[ is_likp ] ASSIGNING FIELD-SYMBOL(<ls_likp_dlt>).
      IF <ls_likp_dlt>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-insert OR
         <ls_likp_dlt>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.
        lt_tracking_id    = VALUE #( BASE lt_tracking_id (
          trxcod      = zif_gtt_pof_app_constants=>cs_trxcod-sh_number
          trxid       = <ls_likp_dlt>-tknum
          action      = COND #( WHEN <ls_likp_dlt>-updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete
                                  THEN zif_gtt_pof_ef_constants=>cs_change_mode-delete )
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


  METHOD get_aotype_restrictions.

    et_aotype = VALUE #( (
      low     = 'Z*POF*_DL_HD'
      option  = 'CP'
      sign    = 'I'
    ) ).

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
    ls_app_objects-maintabdef     = zif_gtt_pof_app_constants=>cs_tabledef-dl_header_new.

    lt_app_objects                = VALUE #( ( ls_app_objects ) ).

    lt_all_appl_tables            = VALUE #( (
      tabledef    = zif_gtt_pof_app_constants=>cs_tabledef-dl_item_new
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
      zcl_gtt_pof_tools=>throw_exception( ).
    ELSE.
      LOOP AT et_exp_event TRANSPORTING NO FIELDS
        WHERE ( milestone = zif_gtt_pof_app_constants=>cs_milestone-sh_arrival OR
                milestone = zif_gtt_pof_app_constants=>cs_milestone-sh_departure OR
                milestone = zif_gtt_pof_app_constants=>cs_milestone-sh_pod ).
        DELETE et_exp_event.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD get_delivery_head_tracking_id.

    rv_tracking_id  = |{ is_likp-vbeln }|.

  ENDMETHOD.


  METHOD get_instance.

    DATA(lt_trk_obj_type) = VALUE tt_trk_obj_type(
       ( zif_gtt_pof_ef_constants=>cs_trk_obj_type-esc_shipmt )
       ( zif_gtt_pof_ef_constants=>cs_trk_obj_type-esc_deliv )
    ).

    IF is_gtt_enabled( it_trk_obj_type = lt_trk_obj_type ) = abap_true.
      ro_updater  = NEW #( ).

      ro_updater->initiate( ).
    ELSE.
      MESSAGE e006(zgtt_pof) INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_object_type.

    rv_objtype  = zif_gtt_pof_ef_constants=>cs_trk_obj_type-esc_deliv.

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
                                    THEN zif_gtt_pof_app_constants=>cs_loccat-arrival
                                    ELSE zif_gtt_pof_app_constants=>cs_loccat-departure ).
      IF sy-subrc <> 0.
        MESSAGE e011(zgtt_pof) WITH rv_stopid is_stops-tknum INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_pod_relevant_delivery.

    DATA: lt_werks TYPE RANGE OF lips-werks.

    rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.

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
          AND loccat   = zif_gtt_pof_app_constants=>cs_loccat-arrival
          AND loctype  = zif_gtt_pof_ef_constants=>cs_loc_types-plant.

        rv_result   = zif_gtt_pof_ef_constants=>cs_condition-true.
        EXIT.
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD is_pod_relevant_stop.

    CLEAR: rv_result.

    LOOP AT is_ship-likp\lips[ is_likp ] ASSIGNING FIELD-SYMBOL(<ls_lips>).
      IF is_stops-locid   = <ls_lips>-werks AND
         is_stops-loccat  = zif_gtt_pof_app_constants=>cs_loccat-arrival AND
         is_stops-loctype = zif_gtt_pof_ef_constants=>cs_loc_types-plant.

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

    FIELD-SYMBOLS: <ls_ship>  TYPE zcl_gtt_pof_ctp_shipment_data=>ts_shipment_merge,
                   <lt_stops> TYPE zcl_gtt_pof_ctp_shipment_data=>tt_stops_srt.

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
          MESSAGE e005(zgtt_pof) WITH |{ <ls_likp>-tknum }| 'STOPS'
            INTO DATA(lv_dummy).
          zcl_gtt_pof_tools=>throw_exception( ).
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
