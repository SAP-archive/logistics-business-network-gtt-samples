FUNCTION zgtt_sof_ote_so_item.
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
    ls_app_objects  TYPE trxas_appobj_ctab_wa,
    ls_control_data TYPE /saptrx/control_data,
*   net value conversion
    rv_external     TYPE netwr_ak,
    lv_external     TYPE bapicurr_d,
    lv_tzone        TYPE timezone.

  FIELD-SYMBOLS:
    <ls_xvbap> TYPE vbapvb.

  DATA: ls_xvbep TYPE vbepvb,
        ls_xvbup TYPE vbupvb.

  DATA: lt_xvbep TYPE STANDARD TABLE OF vbepvb,
        lt_xvbup TYPE STANDARD TABLE OF vbupvb.

* Read Schedule Item New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_schedule_line_item_new
    CHANGING lt_xvbep.
  SORT lt_xvbep BY edatu.

* Read Item Status New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_item_status_new
    CHANGING lt_xvbup.

* <1> Fill general data for all control data records
  ls_control_data-appsys     = i_appsys.
  ls_control_data-appobjtype = i_app_obj_types-aotype.
  ls_control_data-language   = sy-langu.

* <2> Loop at application objects for geting Sales Order Item
*     add the following values which cannot be extracted in EM Data Extraction
*     - Sale order number
*     - Sale order item number
*     - Material number
*     - Material description
*     - Plant
*     - Quantity and quanity unit
*     - Net value and currency
*     - Rejection status
*     - Actual Business Time
*     - Actual Business Time zone
*     - Delivery Item table
*     - Schedule line table

  LOOP AT i_app_objects INTO ls_app_objects.

*   Application Object ID
    ls_control_data-appobjid   = ls_app_objects-appobjid.

*   Check if Main table is Sales Order Item or not.
    IF ls_app_objects-maintabdef <> gc_bpt_sales_order_items_new.
      PERFORM create_logtable_aot
        TABLES e_logtable
        USING  ls_app_objects-maintabdef
               space
               i_app_obj_types-controldatafunc
               ls_app_objects-appobjtype
               i_appsys.
      RAISE cdata_determination_error.
    ELSE.
*     Read Main Object Table
      ASSIGN ls_app_objects-maintabref->* TO <ls_xvbap>.
    ENDIF.

    CLEAR ls_control_data-paramindex.

*   Sales order number
    ls_control_data-paramname = gc_cp_yn_so_no.
    ls_control_data-value     = <ls_xvbap>-vbeln.
    APPEND ls_control_data TO e_control_data.

*   Sales order item number
    ls_control_data-paramname = gc_cp_yn_so_item_no.
    ls_control_data-value     = <ls_xvbap>-posnr.
    APPEND ls_control_data TO e_control_data.

*   Material Number
    ls_control_data-paramname =  gc_cp_yn_material_no.
    ls_control_data-value     = <ls_xvbap>-matnr.
    APPEND ls_control_data TO e_control_data.

*   Material description
    ls_control_data-paramname = gc_cp_yn_material_txt.
    ls_control_data-value     = <ls_xvbap>-arktx.
    APPEND ls_control_data TO e_control_data.

*   Plant
    ls_control_data-paramname = gc_cp_yn_plant.
    ls_control_data-value     = <ls_xvbap>-werks.
    APPEND ls_control_data TO e_control_data.

*   Quantity
    ls_control_data-paramname = gc_cp_yn_quantity.
    ls_control_data-value     = <ls_xvbap>-kwmeng.
    SHIFT ls_control_data-value LEFT  DELETING LEADING space.
    APPEND ls_control_data TO e_control_data.

*   Quantity unit
    ls_control_data-paramname = gc_cp_yn_qty_unit.
    ls_control_data-value     = <ls_xvbap>-vrkme.
    APPEND ls_control_data TO e_control_data.

*   Net value
    ls_control_data-paramname = gc_cp_yn_net_value.
    CLEAR rv_external.
    IF <ls_xvbap>-netwr IS NOT INITIAL.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = <ls_xvbap>-waerk
          amount_internal = <ls_xvbap>-netwr
        IMPORTING
          amount_external = lv_external.
      rv_external   = round( val = lv_external dec = 2 ).
    ENDIF.
    ls_control_data-value     = rv_external.
    SHIFT ls_control_data-value LEFT  DELETING LEADING space.
    APPEND ls_control_data TO e_control_data.

*   Net value currency
    ls_control_data-paramname = gc_cp_yn_net_value_curr.
    ls_control_data-value     = <ls_xvbap>-waerk.
    APPEND ls_control_data TO e_control_data.

*   Rejection status
    CLEAR ls_xvbup.
    READ TABLE lt_xvbup INTO ls_xvbup WITH KEY vbeln = <ls_xvbap>-vbeln
                                               posnr = <ls_xvbap>-posnr.
    ls_control_data-paramname = gc_cp_yn_reject_status.
    IF ls_xvbup-absta IS INITIAL.
      ls_control_data-value     = gc_true.
    ELSE.
      ls_control_data-value     = ls_xvbup-absta.
    ENDIF.
    APPEND ls_control_data TO e_control_data.

*   Actual Business Time zone
    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = lv_tzone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    ls_control_data-paramname = gc_cp_yn_act_timezone.
    ls_control_data-value     = lv_tzone.
    APPEND ls_control_data TO e_control_data.

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

*   Schedule line table
    LOOP AT lt_xvbep INTO ls_xvbep WHERE vbeln = <ls_xvbap>-vbeln
                                     AND posnr = <ls_xvbap>-posnr
                                     AND lfrel = 'X'
                                     AND updkz <> 'D'
                                     AND bmeng IS NOT INITIAL.
      ls_control_data-paramindex = ls_xvbep-etenr.

      ls_control_data-paramname = gc_cp_yn_so_schedule_item_cnt.
      ls_control_data-value     = ls_xvbep-etenr.
      SHIFT ls_control_data-value LEFT  DELETING LEADING space.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_so_schedule_dlv_date.
      IF ls_xvbep-edatu IS NOT INITIAL.
        ls_control_data-value     = ls_xvbep-edatu.
      ELSE.
        CLEAR ls_control_data-value.
      ENDIF.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_so_schedule_conf_qty.
      ls_control_data-value     = ls_xvbep-bmeng.
      SHIFT ls_control_data-value LEFT  DELETING LEADING space.
      APPEND ls_control_data TO e_control_data.

      ls_control_data-paramname = gc_cp_yn_so_schedule_ordr_uom.
      ls_control_data-value     = ls_xvbep-vrkme.
      APPEND ls_control_data TO e_control_data.
    ENDLOOP.
    IF sy-subrc NE 0.
      ls_control_data-paramindex = '1'.
      ls_control_data-paramname = gc_cp_yn_so_schedule_item_cnt.
      ls_control_data-value = ''.
      APPEND ls_control_data TO e_control_data.
    ENDIF.

  ENDLOOP.

ENDFUNCTION.
