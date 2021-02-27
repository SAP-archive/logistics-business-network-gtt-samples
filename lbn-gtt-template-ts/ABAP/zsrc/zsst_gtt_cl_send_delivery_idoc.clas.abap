CLASS zsst_gtt_cl_send_delivery_idoc DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_delivery_item,
        delivery_number      TYPE /scmtms/base_btd_id,
        delivery_item_number TYPE /scmtms/base_btd_item_id,
        freight_unit_number  TYPE /scmtms/tor_id,
        change_mode          TYPE /bobf/conf_change_mode,
        stop_id_dst          TYPE /scmtms/stop_id,
        log_locid_dst        TYPE /scmtms/location_id,
        country_code_dst     TYPE land1,
        city_name_dst	       TYPE	ad_city1,
        evt_exp_datetime     TYPE /saptrx/event_exp_datetime,
        evt_exp_tzone        TYPE /saptrx/timezone,
      END OF ty_delivery_item .
    TYPES:
      tt_delivery_item TYPE SORTED TABLE OF ty_delivery_item WITH UNIQUE KEY delivery_number delivery_item_number freight_unit_number .
    TYPES:
      tt_tor_type TYPE SORTED TABLE OF /scmtms/tor_type WITH UNIQUE KEY table_line .
    TYPES:
      BEGIN OF ty_aotype,
        tor_type TYPE /scmtms/tor_type,
        aotype   TYPE /saptrx/aotype,
      END OF    ty_aotype .
    TYPES:
      tt_aottype TYPE SORTED TABLE OF ty_aotype WITH UNIQUE KEY tor_type .
    TYPES:
      BEGIN OF ty_aotype_item,
        obj_type TYPE /saptrx/trk_obj_type,
        aot_type TYPE /saptrx/aotype,
      END OF ty_aotype_item .
    TYPES:
      tt_aotype_item TYPE TABLE OF ty_aotype_item .
    TYPES:
      BEGIN OF ty_aotypes_new,
        trk_obj_type  TYPE /saptrx/aotypes-trk_obj_type,
        aotype        TYPE /saptrx/aotypes-aotype,
        trxservername TYPE /saptrx/aotypes-trxservername,
      END OF ty_aotypes_new .
    TYPES:
      tt_aotypes_new TYPE TABLE OF ty_aotypes_new .
    TYPES:
      BEGIN OF ty_trxserv,
        trx_server_id TYPE /saptrx/trxserv-trx_server_id,
        trx_server    TYPE /saptrx/trxserv-trx_server,
      END OF ty_trxserv .
    TYPES:
      tt_trxserv TYPE TABLE OF ty_trxserv .

    TYPES:
      BEGIN OF ty_likp,
        vbeln TYPE likp-vbeln, "Delivery
        kodat TYPE likp-kodat, "Picking Date
        kouhr TYPE likp-kouhr, "Picking Time (Local Time, with Reference to a Plant)
        vstel TYPE likp-vstel, "Shipping Point / Receiving Point
        wadat TYPE likp-wadat, "Planned Goods Movement Date
        wauhr TYPE likp-wauhr, "Time of Goods Issue (Local, Relating to a Plant)
        kunnr TYPE likp-kunnr, "Ship-to Party
      END OF ty_likp.
    types:
      tt_likp type table of ty_likp.

  types:
    BEGIN OF ty_delivery_item_new,
        delivery_number      TYPE VBELN_VL,
        delivery_item_number TYPE POSNR_VL,
        freight_unit_number  TYPE /scmtms/tor_id,
        change_mode          TYPE /bobf/conf_change_mode,
        stop_id_dst          TYPE /scmtms/stop_id,
        log_locid_dst        TYPE /scmtms/location_id,
        country_code_dst     TYPE land1,
        city_name_dst	       TYPE	ad_city1,
        evt_exp_datetime     TYPE /saptrx/event_exp_datetime,
        evt_exp_tzone        TYPE /saptrx/timezone,
      END OF ty_delivery_item_new .
  types:
    tt_delivery_item_new TYPE TABLE OF ty_delivery_item.

    CONSTANTS cv_base_btd_tco_inb_dlv TYPE /scmtms/base_btd_tco VALUE '58' ##NO_TEXT.

    METHODS send_delivery_idoc .
    METHODS constructor
      IMPORTING
        !it_tor_root_sstring        TYPE /scmtms/t_em_bo_tor_root
        !it_tor_root_before_sstring TYPE /scmtms/t_em_bo_tor_root
        !it_tor_item_sstring        TYPE /scmtms/t_em_bo_tor_item
        !it_tor_item_before_sstring TYPE /scmtms/t_em_bo_tor_item
        !it_tor_stop_sstring        TYPE /scmtms/t_em_bo_tor_stop
        !it_tor_stop_addr_sstring   TYPE /scmtms/t_em_bo_loc_addr .
protected section.
PRIVATE SECTION.

  CONSTANTS cv_base_btd_tco_outb_dlv TYPE /scmtms/base_btd_tco VALUE '73' ##NO_TEXT.
  CONSTANTS:
    BEGIN OF cs_aotype,
      BEGIN OF freight_unit,
        azure       TYPE /saptrx/aotype VALUE 'ZGTT_SHP_FU_AZR',
        integration TYPE /saptrx/aotype VALUE 'ZGTT_SHP_FU_INT',
        acceptance  TYPE /saptrx/aotype VALUE 'ZGTT_SHP_FU_ACC',
      END OF freight_unit,
    END OF cs_aotype .
  CONSTANTS:
    BEGIN OF cs_control,
      dlv_no      TYPE /saptrx/paramname VALUE 'YN_DLV_NO',
      dlv_item_no TYPE /saptrx/paramname VALUE 'YN_DLV_ITEM_NO',
      act_bus_tz  TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_TIMEZONE',
      ACT_BUS_Tm  TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_DATETIME',
      act_tec_tz  TYPE /saptrx/paramname VALUE 'ACTUAL_TECHNICAL_TIMEZONE',
      act_tec_tm  TYPE /saptrx/paramname VALUE 'ACTUAL_TECHNICAL_DATETIME',
    END OF cs_control.
  CONSTANTS:
    BEGIN OF cs_tracking,
      outbound_delivery     TYPE /saptrx/trxcod VALUE 'OUTBOUND_DELIVERY',
      outbound_delivery_itm TYPE /saptrx/trxcod VALUE 'OUTBOUND_DELIVERY_IT',
      freight_unit          TYPE /saptrx/trxcod VALUE 'FREIGHT_UNIT',
    END OF cs_tracking.
  CONSTANTS:
    BEGIN OF cs_eventdata,
      BEGIN OF milestone,
        picking     TYPE /saptrx/appl_event_tag VALUE 'PICKING',
        packing     TYPE /saptrx/appl_event_tag VALUE 'PACKING',
        goods_issue TYPE /saptrx/appl_event_tag VALUE 'GOODS_ISSUE',
        dlv_pod     TYPE /saptrx/appl_event_tag VALUE 'DLV_POD',
        pod         TYPE /saptrx/appl_event_tag VALUE 'POD',
      END OF milestone,
      BEGIN OF loctype,
        ShippingPoint TYPE /saptrx/loc_id_type VALUE 'ShippingPoint',
        Customer      TYPE /saptrx/loc_id_type VALUE 'Customer',
      END OF loctype,
      LogisticLocation TYPE /saptrx/loc_id_type VALUE 'LogisticLocation',
    END OF cs_eventdata.
  DATA mt_allowed_fu_type TYPE tt_tor_type .
  DATA mt_tor_root TYPE /scmtms/t_em_bo_tor_root .
  DATA mt_tor_root_before TYPE /scmtms/t_em_bo_tor_root .
  DATA mt_tor_item TYPE /scmtms/t_em_bo_tor_item .
  DATA mt_tor_item_before TYPE /scmtms/t_em_bo_tor_item .
  DATA mt_tor_stop TYPE /scmtms/t_em_bo_tor_stop .
  DATA mt_tor_stop_before TYPE /scmtms/t_em_bo_tor_stop .
  DATA mt_tor_stop_addr TYPE /scmtms/t_em_bo_loc_addr .

  METHODS get_delivery_items
    RETURNING
      VALUE(rt_delivery_item) TYPE tt_delivery_item .
  METHODS get_allowed_fu_types
    RETURNING
      VALUE(rt_fu_type) TYPE tt_tor_type .
  METHODS add_delivery_item_to_send_list
    IMPORTING
      !is_tor_root      TYPE /scmtms/s_em_bo_tor_root
      !iv_change_mode   TYPE /bobf/conf_change_mode
    CHANGING
      !ct_delivery_item TYPE tt_delivery_item .
  METHODS get_dlv_item_based_on_tor_root
    CHANGING
      !ct_delivery_item TYPE tt_delivery_item .
  METHODS get_dlv_item_based_on_tor_item
    CHANGING
      !ct_delivery_item TYPE tt_delivery_item .
  METHODS get_dlv_item_based_on_tor_stop
    CHANGING
      !ct_delivery_item TYPE tt_delivery_item .
  METHODS get_required_data
    IMPORTING
      !iv_change_mode      TYPE /bobf/conf_change_mode
      !iv_tor_root_node_id TYPE /scmtms/bo_node_id
    CHANGING
      !cs_delivery_item    TYPE ty_delivery_item .
  METHODS conversion_alpha_input
    IMPORTING
      !input        TYPE clike
    EXPORTING
      VALUE(output) TYPE clike .
  METHODS get_tor_stop_before
    RETURNING
      VALUE(rt_tor_stop_before) TYPE /scmtms/t_em_bo_tor_stop .
ENDCLASS.



CLASS ZSST_GTT_CL_SEND_DELIVERY_IDOC IMPLEMENTATION.


  METHOD add_delivery_item_to_send_list.

    DATA ls_delivery_item TYPE ty_delivery_item.

    LOOP AT mt_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
      USING KEY item_parent WHERE parent_node_id = is_tor_root-node_id.

      CHECK ( <ls_tor_item>-base_btd_tco = cv_base_btd_tco_inb_dlv OR
              <ls_tor_item>-base_btd_tco = cv_base_btd_tco_outb_dlv ) AND
              <ls_tor_item>-base_btd_id     IS NOT INITIAL AND
              <ls_tor_item>-base_btditem_id IS NOT INITIAL.

      ls_delivery_item-delivery_number      = <ls_tor_item>-base_btd_id.
      ls_delivery_item-delivery_item_number = <ls_tor_item>-base_btditem_id.
      ls_delivery_item-freight_unit_number  = is_tor_root-tor_id.
      ls_delivery_item-change_mode          = iv_change_mode.

      get_required_data(
        EXPORTING
          iv_change_mode      = iv_change_mode
          iv_tor_root_node_id = is_tor_root-node_id
        CHANGING
          cs_delivery_item    = ls_delivery_item ).

      INSERT ls_delivery_item INTO TABLE ct_delivery_item.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    mt_tor_root        = it_tor_root_sstring.
    mt_tor_root_before = it_tor_root_before_sstring.
    mt_tor_item        = it_tor_item_sstring.
    mt_tor_item_before = it_tor_item_before_sstring.
    mt_tor_stop        = it_tor_stop_sstring.
    mt_tor_stop_addr   = it_tor_stop_addr_sstring.
    mt_tor_stop_before = get_tor_stop_before( ).
    mt_allowed_fu_type = get_allowed_fu_types( ).

  ENDMETHOD.


  METHOD conversion_alpha_input.

    CLEAR:output.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = input
      IMPORTING
        output = output.

  ENDMETHOD.


  METHOD get_allowed_fu_types.

    DATA lt_aotype TYPE tt_aottype.

    DATA(lt_tor_root) = VALUE /scmtms/t_em_bo_tor_root( FOR <ls_tor_root> IN mt_tor_root
                        WHERE ( tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit ) ( <ls_tor_root> ) ).

    SELECT type AS tor_type, aotype
      FROM /scmtms/c_torty
      INTO TABLE @lt_aotype
      FOR ALL ENTRIES IN @lt_tor_root
      WHERE type = @lt_tor_root-tor_type.

    LOOP AT lt_aotype ASSIGNING FIELD-SYMBOL(<ls_aotype>).
      CHECK <ls_aotype>-aotype = cs_aotype-freight_unit-acceptance OR
            <ls_aotype>-aotype = cs_aotype-freight_unit-azure      OR
            <ls_aotype>-aotype = cs_aotype-freight_unit-integration.
      INSERT <ls_aotype>-tor_type INTO TABLE rt_fu_type.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_delivery_items.

    get_dlv_item_based_on_tor_root( CHANGING ct_delivery_item = rt_delivery_item ).
    get_dlv_item_based_on_tor_item( CHANGING ct_delivery_item = rt_delivery_item ).
    get_dlv_item_based_on_tor_stop( CHANGING ct_delivery_item = rt_delivery_item ).

  ENDMETHOD.


  METHOD get_dlv_item_based_on_tor_item.

    DATA ls_delivery_item TYPE ty_delivery_item.

    LOOP AT mt_tor_item ASSIGNING FIELD-SYMBOL(<ls_tor_item>)
      WHERE ( base_btd_tco = cv_base_btd_tco_inb_dlv OR base_btd_tco = cv_base_btd_tco_outb_dlv ) AND
            ( change_mode = /bobf/if_frw_c=>sc_modify_delete OR change_mode = /bobf/if_frw_c=>sc_modify_create ) AND
              base_btd_id IS NOT INITIAL AND base_btditem_id IS NOT INITIAL.

      ASSIGN mt_tor_root[ node_id = <ls_tor_item>-parent_node_id ] TO FIELD-SYMBOL(<ls_tor_root>).
      CHECK sy-subrc = 0 AND <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

      ls_delivery_item-delivery_number      = <ls_tor_item>-base_btd_id.
      ls_delivery_item-delivery_item_number = <ls_tor_item>-base_btditem_id.
      ls_delivery_item-freight_unit_number  = <ls_tor_root>-tor_id.
      ls_delivery_item-change_mode          = <ls_tor_item>-change_mode.

      get_required_data(
        EXPORTING
          iv_change_mode      = <ls_tor_item>-change_mode
          iv_tor_root_node_id = <ls_tor_root>-node_id
        CHANGING
          cs_delivery_item    = ls_delivery_item ).

      INSERT ls_delivery_item INTO TABLE ct_delivery_item.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_dlv_item_based_on_tor_root.

    LOOP AT mt_tor_root ASSIGNING FIELD-SYMBOL(<ls_tor_root>)
      WHERE tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

      CHECK line_exists( mt_allowed_fu_type[ table_line = <ls_tor_root>-tor_type ] ) AND
            ( <ls_tor_root>-base_btd_tco = cv_base_btd_tco_inb_dlv OR
              <ls_tor_root>-base_btd_tco = cv_base_btd_tco_outb_dlv ).

      IF <ls_tor_root>-change_mode = /bobf/if_frw_c=>sc_modify_delete.
        add_delivery_item_to_send_list(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_delete
          CHANGING
            ct_delivery_item = ct_delivery_item ).
        CONTINUE.
      ENDIF.

      ASSIGN mt_tor_root_before[ node_id = <ls_tor_root>-node_id ] TO FIELD-SYMBOL(<ls_tor_root_before>).
      IF sy-subrc = 0.
        CHECK <ls_tor_root_before>-base_btd_tco <> cv_base_btd_tco_inb_dlv AND
              <ls_tor_root_before>-base_btd_tco <> cv_base_btd_tco_outb_dlv.
        add_delivery_item_to_send_list(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_update
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ELSE.
        add_delivery_item_to_send_list(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_create
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_dlv_item_based_on_tor_stop.

    DATA lv_tabix TYPE sy-tabix.

    LOOP AT mt_tor_stop ASSIGNING FIELD-SYMBOL(<ls_tor_stop>)
      GROUP BY ( parent_node_id = <ls_tor_stop>-parent_node_id
                 group_size     = GROUP SIZE ) ASSIGNING FIELD-SYMBOL(<lt_group>).

      LOOP AT GROUP <lt_group> ASSIGNING FIELD-SYMBOL(<ls_tor_stop_current>).
        lv_tabix += 1.
        CHECK lv_tabix = <lt_group>-group_size.

        ASSIGN mt_tor_stop_before[ node_id = <ls_tor_stop_current>-node_id ] TO FIELD-SYMBOL(<ls_tor_stop_before>).
        CHECK ( sy-subrc = 0 AND <ls_tor_stop_current>-log_locid <> <ls_tor_stop_before>-log_locid ) OR sy-subrc <> 0.

        " Freight Unit destination was changed => send IDOC
        ASSIGN mt_tor_root[ node_id = <ls_tor_stop_current>-parent_node_id ] TO FIELD-SYMBOL(<ls_tor_root>).
        CHECK sy-subrc = 0 AND <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.

        add_delivery_item_to_send_list(
          EXPORTING
            is_tor_root      = <ls_tor_root>
            iv_change_mode   = /bobf/if_frw_c=>sc_modify_update
          CHANGING
            ct_delivery_item = ct_delivery_item ).
      ENDLOOP.

      CLEAR lv_tabix.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_required_data.

    DATA:
      lt_stop      TYPE /scmtms/t_em_bo_tor_stop,
      lv_time_zone TYPE sy-zonlo.

    IF iv_change_mode = /bobf/if_frw_c=>sc_modify_delete.
      lt_stop = VALUE #( FOR <ls_stop> IN mt_tor_stop_before USING KEY parent_autoplan
                         WHERE ( parent_node_id = iv_tor_root_node_id ) ( <ls_stop> ) ).
    ELSE.
      lt_stop = VALUE #( FOR <ls_stop> IN mt_tor_stop USING KEY parent_autoplan
                         WHERE ( parent_node_id = iv_tor_root_node_id ) ( <ls_stop> ) ).
    ENDIF.

    DATA(lv_dst_stop_seq_num) = lines( lt_stop ).
    ASSIGN lt_stop[ seq_num = lv_dst_stop_seq_num ] TO FIELD-SYMBOL(<ls_stop_dst>).
    IF sy-subrc = 0.

      cs_delivery_item-stop_id_dst   = <ls_stop_dst>-stop_id.
      cs_delivery_item-log_locid_dst = <ls_stop_dst>-log_locid.

      ASSIGN mt_tor_stop_addr[ parent_node_id = <ls_stop_dst>-log_loc_uuid ] TO FIELD-SYMBOL(<ls_tor_stop_addr>).
      IF sy-subrc = 0.
        cs_delivery_item-country_code_dst = <ls_tor_stop_addr>-country_code.
        cs_delivery_item-city_name_dst    = <ls_tor_stop_addr>-city_name.
        lv_time_zone = COND #( WHEN <ls_tor_stop_addr>-time_zone_code IS NOT INITIAL
                                 THEN <ls_tor_stop_addr>-time_zone_code
                               ELSE sy-zonlo ).
        cs_delivery_item-evt_exp_tzone = lv_time_zone.
      ENDIF.

      CONVERT TIME STAMP <ls_stop_dst>-assgn_end TIME ZONE lv_time_zone
        INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
      cs_delivery_item-evt_exp_datetime = |0{ lv_conv_date }{ lv_conv_time }|.
    ENDIF.

  ENDMETHOD.


  METHOD GET_TOR_STOP_BEFORE.

    DATA ls_tor_stop_before TYPE /scmtms/s_em_bo_tor_stop.

    /scmtms/cl_tor_helper_stop=>get_stop_sequence(
      EXPORTING
        it_root_key     = VALUE #( FOR <ls_tor_root> IN mt_tor_root ( key = <ls_tor_root>-node_id ) )
        iv_before_image = abap_true
      IMPORTING
        et_stop_seq_d   = DATA(lt_stop_seq_d) ).

    LOOP AT lt_stop_seq_d ASSIGNING FIELD-SYMBOL(<ls_stop_seq_d>).

      LOOP AT <ls_stop_seq_d>-stop_seq ASSIGNING FIELD-SYMBOL(<ls_stop_seq>).
        DATA(lv_tabix) = sy-tabix.
        MOVE-CORRESPONDING <ls_stop_seq> TO ls_tor_stop_before.

        ls_tor_stop_before-parent_node_id = <ls_stop_seq>-root_key.

        ASSIGN <ls_stop_seq_d>-stop_map[ tabix = lv_tabix ] TO FIELD-SYMBOL(<ls_stop_map>).
        ls_tor_stop_before-node_id = <ls_stop_map>-stop_key.

        INSERT ls_tor_stop_before INTO TABLE rt_tor_stop_before.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD send_delivery_idoc.
*---------------------------------------------------------------------*
* Trigger point: During the save of freight unit
* Main Purpose:
*      1)Fill tracking table for delivery item(Delivery item should watch relevant FUs)
*      2)Add planned POD events in delivery item
*---------------------------------------------------------------------*
*---------------------------------------------------------------------*
* Variant & Inner table & work area defination
*---------------------------------------------------------------------*
    DATA:
      lv_vbeln             TYPE lips-vbeln,
      lv_posnrc            TYPE char6,
      lt_delivery_item     TYPE tt_delivery_item,
      lt_delivery_item_new TYPE tt_delivery_item_new,
      ls_delivery_item_new TYPE ty_delivery_item_new.

*---------------------------------------------------------------------*
* Main logic
*---------------------------------------------------------------------*
    CLEAR zsst_gtt_cl_toolkit=>gt_delivery_item.
    lt_delivery_item = get_delivery_items( ).
    LOOP AT lt_delivery_item ASSIGNING FIELD-SYMBOL(<fs_delivery_item>).
      me->conversion_alpha_input(
        EXPORTING
          input  = <fs_delivery_item>-delivery_number
        IMPORTING
          output = lv_vbeln ).

      me->conversion_alpha_input(
        EXPORTING
          input  = <fs_delivery_item>-delivery_item_number
        IMPORTING
          output = lv_posnrc ).

      MOVE-CORRESPONDING <fs_delivery_item> TO ls_delivery_item_new.
      ls_delivery_item_new-delivery_number = lv_vbeln.
      ls_delivery_item_new-delivery_item_number = lv_posnrc.
      APPEND ls_delivery_item_new TO zsst_gtt_cl_toolkit=>gt_delivery_item.
      CLEAR:
        lv_vbeln,
        lv_posnrc,
        ls_delivery_item_new.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
