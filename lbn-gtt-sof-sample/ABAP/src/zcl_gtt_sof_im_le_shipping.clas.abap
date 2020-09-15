class ZCL_GTT_SOF_IM_LE_SHIPPING definition
  public
  final
  create public .

*"* public components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
public section.

  interfaces IF_BADI_INTERFACE .
  interfaces IF_EX_LE_SHP_DELIVERY_PROC .
protected section.
*"* protected components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
private section.
*"* private components of class CL_EXM_IM_LE_SHP_DELIVERY_PROC
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_GTT_SOF_IM_LE_SHIPPING IMPLEMENTATION.


method if_ex_le_shp_delivery_proc~change_delivery_header .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_HEADER


method if_ex_le_shp_delivery_proc~change_delivery_item .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_DELIVERY_ITEM


method if_ex_le_shp_delivery_proc~change_fcode_attributes .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FCODE_ATTRIBUTES


method if_ex_le_shp_delivery_proc~change_field_attributes .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHANGE_FIELD_ATTRIBUTES


method if_ex_le_shp_delivery_proc~check_item_deletion .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~CHECK_ITEM_DELETION


method if_ex_le_shp_delivery_proc~delivery_deletion .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_DELETION


method if_ex_le_shp_delivery_proc~delivery_final_check .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~DELIVERY_FINAL_CHECK


method if_ex_le_shp_delivery_proc~document_number_publish .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~DOCUMENT_NUMBER_PUBLISH


method if_ex_le_shp_delivery_proc~fill_delivery_header .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_HEADER


method if_ex_le_shp_delivery_proc~fill_delivery_item .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~FILL_DELIVERY_ITEM


method if_ex_le_shp_delivery_proc~initialize_delivery .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~INITIALIZE_DELIVERY


method if_ex_le_shp_delivery_proc~item_deletion .


endmethod.                    "IF_EX_LE_SHP_DELIVERY_PROC~ITEM_DELETION


method if_ex_le_shp_delivery_proc~publish_delivery_item .


endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~PUBLISH_DELIVERY_ITEM


method if_ex_le_shp_delivery_proc~read_delivery .

endmethod.                    "IF_EX_LE_SHP_DELIVERY_PROC~READ_DELIVERY


method IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_BEFORE_OUTPUT.
endmethod.


METHOD if_ex_le_shp_delivery_proc~save_and_publish_document .
  INCLUDE /saptrx/constants.
  TYPE-POOLS: trxas, vsep, vse.

  DATA: l_structure_package TYPE devclass,
        l_extflag           TYPE flag.

  DATA: lt_vbfa_delta TYPE STANDARD TABLE OF vbfavb,
        lt_vbfas      TYPE STANDARD TABLE OF vbfas,
        lt_vbfa_new   TYPE STANDARD TABLE OF vbfavb,
        lt_vbap_delta TYPE STANDARD TABLE OF vbapvb,
        lt_vbap       TYPE STANDARD TABLE OF vbap,
        lt_vbup       TYPE STANDARD TABLE OF vbup,
        lt_vbap_new   TYPE STANDARD TABLE OF vbapvb.

  DATA:
    ls_vbfa_delta TYPE vbfavb,
    ls_vbfas      TYPE vbfas,
    ls_vbfa_new   TYPE vbfavb,
    ls_vbap_delta TYPE vbapvb,
    ls_vbap       TYPE vbap,
    ls_vbap_new   TYPE vbapvb,
    ls_likp       TYPE likpvb,
    ls_likp_tmp   TYPE likp,
    ls_vbak       TYPE vbak.

  DATA: ls_comwa5 TYPE vbco5,
        ls_comwa6 TYPE vbco6.

  DATA: lt_control      TYPE /saptrx/bapi_trk_control_tab,
        lt_tracking_id  TYPE /saptrx/bapi_trk_trkid_tab,
        ls_trxserv      TYPE /saptrx/trxserv,
        lv_appsys       TYPE logsys,
        lt_appobj_ctabs	TYPE trxas_appobj_ctabs,
        lt_bapireturn	  TYPE bapiret2_t,
        lv_trxserver_id TYPE /saptrx/trxservername,
        lv_tzone        TYPE timezone.

  DATA: ls_control      TYPE LINE OF /saptrx/bapi_trk_control_tab,
        ls_tracking_id  TYPE LINE OF /saptrx/bapi_trk_trkid_tab,
        ls_appobj_ctabs	TYPE LINE OF trxas_appobj_ctabs.

  DATA: BEGIN OF ls_aotype,
          obj_type TYPE /saptrx/trk_obj_type,
          aot_type TYPE /saptrx/aotype,
        END OF ls_aotype.

  DATA: lt_aotype LIKE STANDARD TABLE OF ls_aotype.

  DATA: lv_count TYPE i.

* Check package dependent BADI disabling
  l_structure_package = '/SAPTRX/SCEM_AI_R3'.
  CALL FUNCTION 'GET_R3_EXTENSION_SWITCH'
    EXPORTING
      i_structure_package = l_structure_package
    IMPORTING
      e_active            = l_extflag
    EXCEPTIONS
      not_existing        = 1
      object_not_existing = 2
      no_extension_object = 3
      OTHERS              = 4.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.
  IF l_extflag <> 'X'.
    EXIT.
  ENDIF.

* Check if any tracking server defined
  CALL FUNCTION '/SAPTRX/EVENT_MGR_CHECK'
    EXCEPTIONS
      no_event_mgr_available = 1.
  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

* Check if at least 1 active extractor exists
  SELECT COUNT(*) INTO lv_count   FROM /saptrx/aotypes
                                  WHERE trk_obj_type EQ 'ESC_DELIV'
                                  AND   torelevant  EQ 'X'.
  CHECK lv_count GE 1. CLEAR lv_count.

  SELECT COUNT(*) INTO lv_count   FROM /saptrx/aotypes
                                  WHERE trk_obj_type EQ 'ESC_SORDER'
                                  AND   torelevant  EQ 'X'.
  CHECK lv_count GE 1.

* GET CURRENT LOGICAL SYSTEM
  CALL FUNCTION 'OWN_LOGICAL_SYSTEM_GET'
    IMPORTING
      own_logical_system = lv_appsys.

*   Actual Business Time zone
  CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
    IMPORTING
      timezone            = lv_tzone
    EXCEPTIONS
      customizing_missing = 1
      OTHERS              = 2.

  ls_aotype-obj_type = 'ESC_SORDER'. ls_aotype-aot_type = 'ZGTT_SO_ACC_ITEM'. APPEND ls_aotype TO lt_aotype.
  ls_aotype-obj_type = 'ESC_SORDER'. ls_aotype-aot_type = 'ZGTT_SO_INT_ITEM'. APPEND ls_aotype TO lt_aotype.

  LOOP AT it_xlikp INTO ls_likp WHERE lfart = 'LBNP'.

    CLEAR: lt_vbfa_delta, lt_vbap_delta.

    MOVE it_xvbfa TO lt_vbfa_delta.
    APPEND LINES OF it_yvbfa TO lt_vbfa_delta.
    DELETE lt_vbfa_delta WHERE ( updkz NE 'I' AND updkz NE 'D' ) OR ( vbtyp_n NE 'J' AND vbtyp_v NE 'C' ) OR vbeln NE ls_likp-vbeln.

    CHECK lt_vbfa_delta IS NOT INITIAL.

    LOOP AT lt_vbfa_delta INTO ls_vbfa_delta.
      MOVE ls_vbfa_delta-vbelv TO ls_vbap_delta-vbeln.
      MOVE ls_vbfa_delta-posnv TO ls_vbap_delta-posnr.
      COLLECT ls_vbap_delta INTO lt_vbap_delta.
    ENDLOOP.

    CLEAR: lt_vbfa_new, lt_vbap_new.
    LOOP AT lt_vbap_delta INTO ls_vbap_delta.
      CLEAR: ls_comwa6, lt_vbfas.
      MOVE-CORRESPONDING ls_vbap_delta TO ls_comwa6.
      CALL FUNCTION 'RV_ORDER_FLOW_INFORMATION'
        EXPORTING
          comwa    = ls_comwa6
        TABLES
          vbfa_tab = lt_vbfas.
      LOOP AT lt_vbfas INTO ls_vbfas WHERE vbtyp_n EQ 'J' AND vbtyp_v EQ 'C'.
        SELECT SINGLE * INTO ls_vbak FROM vbak WHERE vbeln = ls_vbfas-vbelv
                                                 AND auart = 'ZGTT'.
        CHECK sy-subrc EQ 0.
        SELECT SINGLE * INTO ls_likp_tmp FROM likp WHERE vbeln = ls_vbfas-vbeln
                                                     AND lfart = 'LBNP'.
        CHECK sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_vbfas TO ls_vbfa_new.
        APPEND ls_vbfa_new TO lt_vbfa_new.
      ENDLOOP.

      CLEAR: ls_comwa5, lt_vbap, ls_vbap, lt_vbup.
      MOVE-CORRESPONDING ls_vbap_delta TO ls_comwa5.
      CALL FUNCTION 'RV_ORDER_POSITION_AND_STATUS'
        EXPORTING
          comwa = ls_comwa5
        TABLES
          lvbap = lt_vbap
          tvbup = lt_vbup.
      IF lt_vbap IS NOT INITIAL.
        READ TABLE lt_vbap INTO ls_vbap WITH KEY vbeln = ls_vbap_delta-vbeln
                                                 posnr = ls_vbap_delta-posnr BINARY SEARCH.
        SELECT SINGLE * INTO ls_vbak FROM vbak WHERE vbeln = ls_vbap-vbeln
                                                 AND auart = 'ZGTT'.
        CHECK sy-subrc EQ 0.
        MOVE-CORRESPONDING ls_vbap TO ls_vbap_new.
        APPEND ls_vbap_new TO lt_vbap_new.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_vbfa_delta INTO ls_vbfa_delta WHERE updkz = 'D'.
      DELETE lt_vbfa_new WHERE ruuid = ls_vbfa_delta-ruuid.
    ENDLOOP.

    LOOP AT lt_vbfa_delta INTO ls_vbfa_delta WHERE updkz = 'I'.
      READ TABLE lt_vbfa_new WITH KEY ruuid = ls_vbfa_delta-ruuid
                                      TRANSPORTING NO FIELDS.
      IF sy-subrc NE 0.
        SELECT SINGLE * INTO ls_vbak FROM vbak WHERE vbeln = ls_vbfa_delta-vbelv
                                                 AND auart = 'ZGTT'.
        CHECK sy-subrc EQ 0.
        COLLECT ls_vbfa_delta INTO lt_vbfa_new.
      ENDIF.
    ENDLOOP.

*  *************************************************************COMPOSE and SEND IDOC->
    LOOP AT lt_aotype INTO ls_aotype.

      SELECT SINGLE trxservername INTO lv_trxserver_id  FROM /saptrx/aotypes
                                                        WHERE trk_obj_type EQ ls_aotype-obj_type
                                                        AND   aotype  EQ ls_aotype-aot_type.

      SELECT SINGLE trx_server_id trx_server INTO ( ls_trxserv-trx_server_id, ls_trxserv-trx_server )
                                                       FROM /saptrx/trxserv
                                                       WHERE trx_server_id = lv_trxserver_id.

      CLEAR: lt_appobj_ctabs, lt_control, lt_tracking_id.

      LOOP AT lt_vbap_new INTO ls_vbap_new.
        CLEAR: ls_appobj_ctabs.
        ls_appobj_ctabs-trxservername = ls_trxserv-trx_server_id.
        ls_appobj_ctabs-appobjtype    = ls_aotype-aot_type.
        CONCATENATE ls_vbap_new-vbeln ls_vbap_new-posnr INTO ls_appobj_ctabs-appobjid.
        APPEND ls_appobj_ctabs TO lt_appobj_ctabs.

        CLEAR: ls_control.
        ls_control-appsys  = lv_appsys.
        ls_control-appobjtype = ls_aotype-aot_type.
        CONCATENATE ls_vbap_new-vbeln ls_vbap_new-posnr INTO ls_control-appobjid.
        ls_control-paramname = 'YN_SO_NO'.
        ls_control-value     = ls_vbap_new-vbeln.
        APPEND ls_control TO lt_control.
        ls_control-paramname = 'YN_SO_ITEM_NO'.
        ls_control-value     = ls_vbap_new-posnr.
        APPEND ls_control TO lt_control.
        ls_control-paramname = 'ACTUAL_BUSINESS_TIMEZONE'.
        ls_control-value     = lv_tzone.
        APPEND ls_control TO lt_control.
        ls_control-paramname = 'ACTUAL_BUSINESS_DATETIME'.
        CONCATENATE '0' sy-datum sy-uzeit INTO ls_control-value.
        APPEND ls_control TO lt_control.

        CLEAR: ls_tracking_id.
        ls_tracking_id-appsys = lv_appsys.
        ls_tracking_id-appobjtype = ls_aotype-aot_type.
        CONCATENATE ls_vbap_new-vbeln ls_vbap_new-posnr INTO ls_tracking_id-appobjid.
        ls_tracking_id-trxcod          = 'SALES_ORDER_ITEM'.
        CONCATENATE ls_vbap_new-vbeln ls_vbap_new-posnr INTO ls_tracking_id-trxid.
        CONCATENATE '0' sy-datum sy-uzeit INTO ls_tracking_id-start_date.
        ls_tracking_id-end_date        = '099991231000000'.
        ls_tracking_id-timzon          = lv_tzone.
        APPEND ls_tracking_id TO lt_tracking_id.

        CLEAR: lv_count.
        LOOP AT lt_vbfa_new INTO ls_vbfa_new WHERE vbelv = ls_vbap_new-vbeln
                                               AND posnv = ls_vbap_new-posnr.
          lv_count = lv_count + 1.
          ls_control-paramname = 'YN_DLV_HDR_ITM_LINE_COUNT'.
          ls_control-paramindex = lv_count.
          ls_control-value = lv_count.
          SHIFT ls_control-value LEFT  DELETING LEADING space.
          APPEND ls_control TO lt_control.

          ls_control-paramname = 'YN_DLV_HDR_ITM_NO'.
          ls_control-paramindex = lv_count.
          CONCATENATE ls_vbfa_new-vbeln ls_vbfa_new-posnn INTO ls_control-value.
          APPEND ls_control TO lt_control.
        ENDLOOP.
        IF sy-subrc NE 0.
          ls_control-paramname = 'YN_DLV_HDR_ITM_LINE_COUNT'.
          ls_control-paramindex = '1'.
          ls_control-value = ''.
          APPEND ls_control TO lt_control.
        ENDIF.
      ENDLOOP.

      CALL METHOD zcl_gtt_sof_upd_xtp_references=>send_idoc_ehpost01
        EXPORTING
          it_control      = lt_control
          it_tracking_id  = lt_tracking_id
          is_trxserv      = ls_trxserv
          iv_appsys       = lv_appsys
          it_appobj_ctabs = lt_appobj_ctabs.

    ENDLOOP.

  ENDLOOP.



ENDMETHOD. "IF_EX_LE_SHP_DELIVERY_PROC~SAVE_AND_PUBLISH_DOCUMENT


method if_ex_le_shp_delivery_proc~save_document_prepare .

endmethod. "IF_EX_LE_SHP_DELIVERY_PROC~SAVE_DOCUMENT_PREPARE
ENDCLASS.
