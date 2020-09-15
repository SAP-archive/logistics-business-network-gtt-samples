*----------------------------------------------------------------------*
***INCLUDE LZGTT_SOFF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  CHECK_EH_EXISTS
*&---------------------------------------------------------------------*
*       Check if Event Handler exists
*----------------------------------------------------------------------*
*      --> IV_VBELN     Sales Order Number
*      --> IV_POSNR     Sales Order Item
*      <-- EV_EH_EXIST  'X' = EH Exist
*----------------------------------------------------------------------*
FORM check_eh_exists
  USING    iv_vbeln    TYPE vbeln
           iv_posnr    TYPE posnr
  CHANGING ev_eh_exist TYPE boole_d.

  DATA:
    lv_aoid   TYPE /saptrx/aoid,
    lv_aotype TYPE /saptrx/aotype.

* Check if EH exists
  CONCATENATE iv_vbeln iv_posnr INTO lv_aoid.
  SELECT SINGLE aotype FROM /saptrx/aotref INTO lv_aotype
*    WHERE aotype = gc_aot_yn_ote
*  ATTENTION: adapt to correct AO type
      WHERE aotype EQ gc_aot_zn_ote
      AND aoid   = lv_aoid.
  IF sy-subrc IS INITIAL.
    ev_eh_exist = gc_true.
  ENDIF.

ENDFORM.                    " CHECK_EH_EXISTS

*&---------------------------------------------------------------------*
*&      Form  CHECK_AOT_RELEVANCE
*&---------------------------------------------------------------------*
*       Check if Event Handler exists - for Event Messages for Sales
*       order use same condition as for AOT. Necessary as the Event
*       Message can be sent during the same LUW as the creation of the
*       Sales Order - table /SAPTRX/AOTREF would not contain an entry
*----------------------------------------------------------------------*
*      --> IS_VBAK           Sales Order Header
*      --> IS_VBAP           Sales Order Item
*      <-- EV_AOT_RELEVANCE  AOT Relevance
*----------------------------------------------------------------------*
FORM check_aot_relevance
  USING    is_xvbak         TYPE /saptrx/sd_sds_hdr
  CHANGING ev_aot_relevance TYPE boole_d.

* Check Sales Organization
* CHECK is_xvbak-vkorg EQ '1000'.
* Check Sales Order Type
  CHECK is_xvbak-auart EQ 'ZGTT'.
* Check plant
* CHECK is_xvbap-werks EQ '1000'.

* Set value
  ev_aot_relevance = gc_true.

ENDFORM.                    " CHECK_AOT_RELEVANCE
*&---------------------------------------------------------------------*
*&      Form  CHECK_AOT_RELEVANCE_Delivery
*&---------------------------------------------------------------------*
*       Check if Event Handler exists - for Event Messages for delivery
*       order use same condition as for AOT. Necessary as the Event
*       Message can be sent during the same LUW as the creation of the
*       Delivery Order - table /SAPTRX/AOTREF would not contain an entry
*----------------------------------------------------------------------*
*      --> IS_LIKP           Delivery Order Header
*      <-- EV_AOT_RELEVANCE  AOT Relevance
*----------------------------------------------------------------------*
FORM check_aot_relevance_dlv
  USING    is_xlikp         TYPE likpvb
  CHANGING ev_aot_relevance TYPE boole_d.


  CHECK is_xlikp-lfart EQ 'LBNP'.


* Set value
  ev_aot_relevance = gc_true.

ENDFORM.                    " CHECK_AOT_RELEVANCE
*&---------------------------------------------------------------------*
*&      Form  CHECK_AOT_RELEVANCE_Shipment
*&---------------------------------------------------------------------*
*       Check if Event Handler exists - for Event Messages for shipment
*       use same condition as for AOT. Necessary as the Event
*       Message can be sent during the same LUW as the creation of the
*       Shipment - table /SAPTRX/AOTREF would not contain an entry
*----------------------------------------------------------------------*
*      --> IS_VTTK           Shipment Header
*      <-- EV_AOT_RELEVANCE  AOT Relevance
*----------------------------------------------------------------------*
FORM check_aot_relevance_shp
  USING    is_xvttk         TYPE vttkvb
  CHANGING ev_aot_relevance TYPE boole_d.


  CHECK is_xvttk-shtyp EQ '0003'.


* Set value
  ev_aot_relevance = gc_true.

ENDFORM.                    " CHECK_AOT_RELEVANCE
*&---------------------------------------------------------------------*
*&      Form  CHECK_COMPLETION_STATUS
*&---------------------------------------------------------------------*
*       Check Comletion Status
*----------------------------------------------------------------------*
*      --> IS_ITEM_UPDKZ  Update Indicator for Item
*      --> IS_XVBUK       Header Status New
*      --> IS_VBUK_OLD    Header Status Old
*      --> IS_XVBUP       Item Status New
*      --> IS_VBUKP_OLD   Item Status Old
*      <-- EV_RESULT      Ture(='T' or False(='F')
*----------------------------------------------------------------------*
FORM check_completion_status
  USING    iv_item_updkz TYPE updkz_d
           is_xvbuk      TYPE vbukvb
           is_vbuk_old   TYPE vbukvb
           is_xvbup      TYPE vbupvb
           is_vbup_old   TYPE vbupvb
  CHANGING ev_result     TYPE sybinpt.

  ev_result = gc_false_condition.

* Check Update Indicator
  CASE iv_item_updkz.
*   Insert (= New)
    WHEN gc_insert.
      IF    is_xvbuk-uvall = gc_complete
        AND is_xvbup-uvall = gc_complete.
        ev_result = gc_true_condition.
      ENDIF.

*   Delete(=D)
    WHEN gc_delete.
*     --> False

*   Update (Header Level = Space, Item Level = U)
    WHEN OTHERS.
*     Check Header Update Indicaotr is not for Deletion
      CHECK iv_item_updkz <> gc_delete.
*     Check Item Overall Status is completed
      CHECK is_xvbup-uvall = gc_complete.
*     Check Header Overall Status is completed
      CHECK is_xvbuk-uvall = gc_complete.
*     Header Status Old is Assigned
      IF is_vbuk_old IS NOT INITIAL.
*       Item Status Old is Assigned
        IF is_vbup_old IS NOT INITIAL.
          IF ( is_vbuk_old-uvall <> gc_complete
            OR is_vbup_old-uvall <> gc_complete ).
            ev_result = gc_true_condition.
          ENDIF.
*         Item Status Old is NOT Assigned
        ELSE.
          CHECK is_vbuk_old-uvall <> gc_complete.
          ev_result = gc_true_condition.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM.                    " CHECK_COMPLETION_STATUS
*&---------------------------------------------------------------------*
*&      Form  IS_BILLING_BLOCK_SET
*&---------------------------------------------------------------------*
*       Check if Billing Block is Set
*----------------------------------------------------------------------*
*      --> IS_HDR        Sales Order Header New
*      --> IS_HDR_OLD    Sales Order Header Old
*      --> IS_ITEM       Sales Order Item New
*      --> IS_ITEM_OLD   Sales Order Item Old
*      <-- EV_RESULT     Ture(='T' or False(='F')
*----------------------------------------------------------------------*
FORM is_billing_block_set
  USING    is_so_hdr        TYPE /saptrx/sd_sds_hdr
           is_so_hdr_old    TYPE /saptrx/sd_sds_hdr
           is_so_item       TYPE vbapvb
           is_so_item_old   TYPE vbapvb
  CHANGING ev_result        TYPE sybinpt.

* Check Update Indicator for Item
  CASE is_so_item-updkz.
*   Insert (= New)
    WHEN gc_insert.
      IF    is_so_hdr-faksk  IS NOT INITIAL
        OR  is_so_item-faksp IS NOT INITIAL.
        ev_result = gc_true_condition.
      ENDIF.

*   Delete(=D)
    WHEN gc_delete.
*     --> False

*   Update (Header Level Change = Space, Item Level Change= U)
    WHEN OTHERS.
*     Check Header Update Indicaotr is not for Deletion
      CHECK is_so_item-updkz <> gc_delete.
*     Check If Header Biling Block or Item Billing Block are Set
      CHECK ( is_so_hdr-faksk  IS NOT INITIAL
        OR    is_so_item-faksp IS NOT INITIAL ).
*     Check if the Billing Block has been changed
*     Check Billing Block in Item Level Changed
      IF is_so_item_old IS NOT INITIAL.  " Item level has been changed
*       Billing Block in Item Level Changed
        IF is_so_item-faksp <> is_so_item_old-faksp.
*         if Item Level Billing Block is set
*         --> Send Evenet Message in order to update item level billing block
          IF is_so_item-faksp IS NOT INITIAL.
            ev_result = gc_true_condition.
            RETURN.
*         if Item Level Billing Block is removed and Header Level Billing Block is set
*         --> Send Evenet Message in order to update header level billing block
          ELSE.
            IF is_so_hdr-faksk IS NOT INITIAL.
              ev_result = gc_true_condition.
              RETURN.
            ENDIF.
          ENDIF.
*       Billing Block in Item Level Not Changed
        ELSE.
*         Check Header level (Header Level Has been changed and Set)
          IF ( is_so_hdr-faksk <> is_so_hdr_old-faksk AND
               is_so_hdr-faksk IS NOT INITIAL ).
            ev_result = gc_true_condition.
            RETURN.
          ENDIF.
        ENDIF.
*     Billing Block in Item Level Not Changed
      ELSE.
*       Check Billing Block in Item Level is Not set (but not changed)
        CHECK is_so_item-faksp IS INITIAL.
*       If Item Level Billing Block is not set and Header Level Billing block is set/changed
*       --> Send Event Message for Header Level Billing Block
        IF ( is_so_hdr-faksk <> is_so_hdr_old-faksk AND
             is_so_hdr-faksk IS NOT INITIAL ).
          ev_result = gc_true_condition.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM.                    " IS_BILLING_BLOCK_SET
*&---------------------------------------------------------------------*
*&      Form  IS_BILLING_BLOCK_REMOVED
*&---------------------------------------------------------------------*
*       Check if Billing Block is Removed
*----------------------------------------------------------------------*
*      --> IS_HDR        Sales Order Header New
*      --> IS_HDR_OLD    Sales Order Header Old
*      --> IS_ITEM       Sales Order Item New
*      --> IS_ITEM_OLD   Sales Order Item Old
*      <-- EV_RESULT     Ture(='T' or False(='F')
*----------------------------------------------------------------------*
FORM is_billing_block_removed
  USING    is_so_hdr        TYPE /saptrx/sd_sds_hdr
           is_so_hdr_old    TYPE /saptrx/sd_sds_hdr
           is_so_item       TYPE vbapvb
           is_so_item_old   TYPE vbapvb
  CHANGING ev_result        TYPE sybinpt.

* Check Update Indicator for Item
  CASE is_so_item-updkz.
*   Insert (= New)
    WHEN gc_insert.
*     --> False

*   Delete(=D)
    WHEN gc_delete.
*     --> False

*   Update (Header Level Change = Space, Item Level Change= U)
    WHEN OTHERS.
*     Check Header Update Indicaotr is not for Deletion
      CHECK is_so_item-updkz <> gc_delete.
*     Check Header Biling Block or Item Billing Block is Set
      CHECK ( is_so_hdr-faksk  IS INITIAL
        AND   is_so_item-faksp IS INITIAL ).
*     Check if the Billing Block has been changed
      IF is_so_item_old IS NOT INITIAL.   " Item level has been changed
        IF   is_so_hdr_old-faksk  IS NOT INITIAL
          OR is_so_item_old-faksp IS NOT INITIAL.
          ev_result = gc_true_condition.
        ENDIF.
      ELSE.  " Header level has been changed
        CHECK is_so_hdr_old-faksk  IS NOT INITIAL.
        ev_result = gc_true_condition.
      ENDIF.

  ENDCASE.

ENDFORM.                    " IS_BILLING_BLOCK_REMOVED
*&---------------------------------------------------------------------*
*&      Form  IS_DELIVERY_BLOCK_SET
*&---------------------------------------------------------------------*
*       Check if Delivery Block is Set
*----------------------------------------------------------------------*
*      --> IS_SO_ITEM    Sales Order Item New
*      --> IS_HDR        Sales Order Header New
*      --> IS_HDR_OLD    Sales Order Header Old
*      --> IT_XVBEP      Schedule Line New
*      --> IT_VBEP_OLD   Schedule Line Old
*      <-- EV_RESULT     Ture(='T' or False(='F')
*----------------------------------------------------------------------*
FORM is_delivery_block_set
  USING    is_so_item       TYPE vbapvb
           is_so_hdr        TYPE /saptrx/sd_sds_hdr
           is_so_hdr_old    TYPE /saptrx/sd_sds_hdr
           it_xvbep         TYPE va_vbepvb_t
           it_vbep_old      TYPE va_vbepvb_t
  CHANGING ev_result        TYPE sybinpt.

  FIELD-SYMBOLS:
    <ls_xvbep>    LIKE LINE OF it_xvbep,
    <ls_vbep_old> LIKE LINE OF it_vbep_old.

  CASE is_so_item-updkz.
*   New Sales Order
    WHEN gc_insert.
*     Check Delivery Block on Header Level
      IF is_so_hdr-lifsk IS NOT INITIAL.
        ev_result = gc_true_condition.
        RETURN.
      ENDIF.
*     Check Delivery Block on Schedule Line Level
      LOOP AT it_xvbep ASSIGNING <ls_xvbep>
        WHERE vbeln = is_so_item-vbeln
          AND posnr = is_so_item-posnr.
        IF  ( <ls_xvbep>-lifsp IS NOT INITIAL
          AND <ls_xvbep>-wmeng IS NOT INITIAL
          AND <ls_xvbep>-updkz <> gc_delete ).
          ev_result = gc_true_condition.
          RETURN.
        ENDIF.
      ENDLOOP.

*   Delete(Cancel)
    WHEN gc_delete.
*     --> False

*   Changed Sales Order
    WHEN OTHERS.
*     Check Delivery Block on Header Level
      IF is_so_hdr-lifsk IS NOT INITIAL.
        IF is_so_hdr-lifsk <> is_so_hdr_old-lifsk.
          ev_result = gc_true_condition.
          RETURN.
        ELSE.
          RETURN.
        ENDIF.
      ENDIF.
*     Check Item Update Indicaotr is not for Deletion
      CHECK is_so_item-updkz <> gc_delete.
*     Check Delivery Block on Schedule Line Level
      LOOP AT it_xvbep ASSIGNING <ls_xvbep>
        WHERE vbeln = is_so_item-vbeln
          AND posnr = is_so_item-posnr.
        IF (  <ls_xvbep>-wmeng IS NOT INITIAL
          AND <ls_xvbep>-updkz <> gc_delete ).
*         Check if Schedule Line has been changed.
          READ TABLE it_vbep_old ASSIGNING <ls_vbep_old>
            WITH KEY vbeln = <ls_xvbep>-vbeln
                     posnr = <ls_xvbep>-posnr
                     etenr = <ls_xvbep>-etenr.
*         Schedule Line has been changed.
          IF sy-subrc IS INITIAL.
*           Delivery Block is Set
            IF <ls_xvbep>-lifsp IS NOT INITIAL.
              IF <ls_xvbep>-lifsp <> <ls_vbep_old>-lifsp.
                ev_result = gc_true_condition.
                RETURN.
              ENDIF.
            ELSE.
              CONTINUE.
            ENDIF.
*           Delivery Block is Not Changed in schedule line, but removed from Header
            IF    ( is_so_hdr-lifsk IS INITIAL
              AND   is_so_hdr-lifsk <> is_so_hdr_old-lifsk ).
              ev_result = gc_true_condition.
              RETURN.
            ENDIF.
*         Schedule line Not Changed
          ELSE.
            IF <ls_xvbep>-lifsp IS NOT INITIAL.
*           Delivery Block is Not Changed in schedule line, but removed from Header
              IF    ( is_so_hdr-lifsk IS INITIAL
                AND   is_so_hdr-lifsk <> is_so_hdr_old-lifsk ).
                ev_result = gc_true_condition.
                RETURN.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

  ENDCASE.

ENDFORM.                    " IS_DELIVERY_BLOCK_SET
*&---------------------------------------------------------------------*
*&      Form  IS_DELIVERY_BLOCK_REMOVED
*&---------------------------------------------------------------------*
*       Check if Delivery Block is Removed
*----------------------------------------------------------------------*
*      --> IS_HDR        Sales Order Header New
*      --> IS_HDR_OLD    Sales Order Header Old
*      --> IS_SO_ITEM    Sales Order Item New
*      --> IT_XVBEP      Schedule Line New
*      --> IT_VBEP_OLD   Schedule Line Old
*      <-- EV_RESULT     Ture(='T' or False(='F')
*----------------------------------------------------------------------*
FORM is_delivery_block_removed
  USING    is_so_hdr        TYPE /saptrx/sd_sds_hdr
           is_so_hdr_old    TYPE /saptrx/sd_sds_hdr
           is_so_item       TYPE vbapvb
           it_xvbep         TYPE va_vbepvb_t
           it_vbep_old      TYPE va_vbepvb_t
  CHANGING ev_result        TYPE sybinpt.

  DATA:
    lv_hdr_removed    TYPE boole_d,
    lv_item_removed   TYPE boole_d,
    lv_item_set       TYPE boole_d,
    lv_item_no_change TYPE boole_d.

  FIELD-SYMBOLS:
    <ls_xvbep>    LIKE LINE OF it_xvbep,
    <ls_vbep_old> LIKE LINE OF it_vbep_old.

  CASE is_so_item-updkz.
*   New Sales Order
*     --> False

*   Delete(Cancel)
    WHEN gc_delete.
*     --> False

*   Changed Sales Order
    WHEN OTHERS.
      IF    ( is_so_hdr-lifsk IS INITIAL
        AND   is_so_hdr-lifsk <> is_so_hdr_old-lifsk ).
        lv_hdr_removed = gc_true.
      ENDIF.

*     Check Item Update Indicaotr is not for Deletion
      CHECK is_so_item-updkz <> gc_delete.
*     Check Delivery Block on Schedule Line Level
      LOOP AT it_xvbep ASSIGNING <ls_xvbep>
        WHERE vbeln = is_so_item-vbeln
          AND posnr = is_so_item-posnr.
        IF  ( <ls_xvbep>-wmeng IS NOT INITIAL
          AND <ls_xvbep>-updkz <> gc_delete ).
*          and <ls_xvbep>-lifsp IS INITIAL ).
          READ TABLE it_vbep_old ASSIGNING <ls_vbep_old>
            WITH KEY vbeln = <ls_xvbep>-vbeln
                     posnr = <ls_xvbep>-posnr
                     etenr = <ls_xvbep>-etenr.
*         Scheduled Line is Changed
          IF sy-subrc IS INITIAL.
*           Delivery block Not Set
            IF <ls_xvbep>-lifsp IS INITIAL.
*             Was Set
              IF <ls_vbep_old>-lifsp IS NOT INITIAL.
                lv_item_removed = gc_true.
*             Was Not Set
              ELSE.
                lv_item_no_change = gc_true.
              ENDIF.
            ELSE.
              lv_item_set = gc_true.
            ENDIF.
*         Scheduled Line is NOT Changed
          ELSE.
            IF <ls_xvbep>-lifsp IS INITIAL.
              lv_item_no_change = gc_true.
            ELSE.
              lv_item_set = gc_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.

*     Header Level Block Removed.
      IF  lv_hdr_removed  = gc_true.
*       If one of the delivery block is set, Do not send Remove event
        IF lv_item_set = gc_true.
          RETURN.
        ENDIF.
*       Item Level Block Removed or Was Not Set
        IF ( lv_item_removed = gc_true
          OR lv_item_no_change = gc_true ).
          ev_result = gc_true_condition.
        ENDIF.
      ENDIF.
*     Header Level Was Not Set and Item Level has been removed.
      IF    ( is_so_hdr-lifsk IS INITIAL
        AND lv_item_removed = gc_true ).
        ev_result = gc_true_condition.
      ENDIF.

  ENDCASE.

ENDFORM.                    " IS_DELIVERY_BLOCK_REMOVED
*&---------------------------------------------------------------------*
*&      Form  GET_STATUS_INFO
*&---------------------------------------------------------------------*
*       Read application objects for geting Status Information and
*       Read Status table with Sales Order No/Item
*----------------------------------------------------------------------*
*      --> I_ALL_APPL_TABLES     Container with all tables
*      --> IV_VBELN       Sales Order Number
*      --> IV_POSNR       Sales Order Item
*      <-- ES_XVBUK       Header Status New
*      <-- ES_VBUK_OLD    Header Status Old
*      <-- ES_XVBUP       Item Status New
*      <-- ES_VBUKP_OLD   Item Status Old
*----------------------------------------------------------------------*
FORM get_status_info
  USING    i_all_appl_tables TYPE trxas_tabcontainer
           iv_vbeln          TYPE vbeln_va
           iv_posnr          TYPE posnr_va
  CHANGING es_xvbuk          TYPE vbukvb
           es_vbuk_old       TYPE vbukvb
           es_xvbup          TYPE vbupvb
           es_vbup_old       TYPE vbupvb.

  DATA:
    lt_xvbuk    TYPE STANDARD TABLE OF vbukvb,
    lt_vbuk_old TYPE STANDARD TABLE OF vbukvb,
    lt_xvbup    TYPE STANDARD TABLE OF vbupvb,
    lt_vbup_old TYPE STANDARD TABLE OF vbupvb.

* Read application objects for geting Status Information
* Sales Order Header Status New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_header_status_new
    CHANGING lt_xvbuk.
* Sales Order Header Status Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_header_status_old
    CHANGING lt_vbuk_old.
* Sales Order Item Status New
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_item_status_new
    CHANGING lt_xvbup.
* Sales Order Status item Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_item_status_old
    CHANGING lt_vbup_old.

* Read Status table with Sales Order No/Item
* Read Status Table Sales Order Header New
  READ TABLE lt_xvbuk INTO es_xvbuk
    WITH KEY vbeln = iv_vbeln.

* Read Status Table Sales Order Header Old
  READ TABLE lt_vbuk_old INTO es_vbuk_old
    WITH KEY vbeln = iv_vbeln.

* Read Status Table Sales Order Item New
  READ TABLE lt_xvbup INTO es_xvbup
    WITH KEY vbeln = iv_vbeln
             posnr = iv_posnr.

* Read Status Table Sales Order Item Old
  READ TABLE lt_vbup_old INTO es_vbup_old
    WITH KEY vbeln = iv_vbeln
             posnr = iv_posnr.

ENDFORM.                    " GET_STATUS_INFO
*&---------------------------------------------------------------------*
*&      Form  IS_CREDIT_BLOCK_SET
*&---------------------------------------------------------------------*
*       Check if Credit Block is Set
*----------------------------------------------------------------------*
*      --> IS_SO_ITEM    Sales Order Item New
*      --> IS_XVBUK      Header Status New
*      --> IS_VBUK_OLD   Header Status Old
*      <-- EV_RESULT     Ture(='T' or False(='F')
*----------------------------------------------------------------------*
FORM is_credit_block_set
  USING    is_so_item       TYPE vbapvb
           is_xvbuk         TYPE vbukvb
           is_vbuk_old      TYPE vbukvb
  CHANGING ev_result        TYPE sybinpt.

  CASE is_so_item-updkz.
*   New Sales Order
    WHEN gc_insert.
      IF is_xvbuk-cmgst = gc_cmgst_b OR is_xvbuk-cmgst = gc_cmgst_c.
        ev_result = gc_true_condition.
      ENDIF.

*   Delete(Cancel)
    WHEN gc_delete.
*     --> False

*   Changed Sales Order
    WHEN OTHERS.
      IF is_xvbuk-cmgst = gc_cmgst_b OR is_xvbuk-cmgst = gc_cmgst_c.
        IF is_vbuk_old IS NOT INITIAL.
          IF ( is_vbuk_old-cmgst = gc_cmgst_a OR is_vbuk_old-cmgst = gc_cmgst_d OR is_vbuk_old-cmgst IS INITIAL ).
            ev_result = gc_true_condition.
          ENDIF.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM.                    " IS_CREDIT_BLOCK_SET
*&---------------------------------------------------------------------*
*&      Form  IS_CREDIT_BLOCK_REMOVED
*&---------------------------------------------------------------------*
*       Check if Credit Block is Set
*----------------------------------------------------------------------*
*      --> IS_SO_HDR     Sales Order Header New
*      --> IS_XVBUK      Header Status New
*      --> IS_VBUK_OLD   Header Status Old
*      <-- EV_RESULT     Ture(='T' or False(='F')
*----------------------------------------------------------------------*
FORM is_credit_block_removed
  USING    is_so_hdr        TYPE /saptrx/sd_sds_hdr
           is_xvbuk         TYPE vbukvb
           is_vbuk_old      TYPE vbukvb
  CHANGING ev_result        TYPE sybinpt.

  CASE is_so_hdr-updkz.
*   New Sales Order
    WHEN gc_insert.
*     Check if item is created - if default no credit check is executed
*     we should not send an EVM. So exit in case of creation of Item
*     --> False

*   Delete(Cancel)
    WHEN gc_delete.
*     --> False

*   Changed Sales Order
    WHEN OTHERS.
      CHECK ( is_xvbuk-cmgst EQ gc_cmgst_a
        OR    is_xvbuk-cmgst EQ gc_cmgst_d ).

      IF is_vbuk_old IS NOT INITIAL.
        IF ( is_vbuk_old-cmgst = gc_cmgst_b OR is_vbuk_old-cmgst = gc_cmgst_c ).
          ev_result = gc_true_condition.
        ENDIF.
      ENDIF.
  ENDCASE.

ENDFORM.                    " IS_CREDIT_BLOCK_REMOVED
*&---------------------------------------------------------------------*
*&      Form  IS_ORDER_REJECTED
*&---------------------------------------------------------------------*
*       Check if Order Rejected
*----------------------------------------------------------------------*
*      --> IS_HDR        Sales Order Header New
*      --> IS_ITEM       Sales Order Item New
*      --> IS_ITEM_OLD   Sales Order Item Old
*      <-- EV_RESULT     Ture(='T' or False(='F')
*----------------------------------------------------------------------*
FORM is_order_rejected
  USING    is_so_hdr        TYPE /saptrx/sd_sds_hdr
           is_so_item       TYPE vbapvb
           is_so_item_old   TYPE vbapvb
  CHANGING ev_result        TYPE sybinpt.

* Check Update Indicator for Item
  CASE is_so_item-updkz.
*   Insert (= New)
    WHEN gc_insert.
      IF is_so_item-abgru IS NOT INITIAL.
        ev_result = gc_true_condition.
      ENDIF.

*   Delete(=D)
    WHEN gc_delete.
*     --> False

*   Update (Header Level Change = Space, Item Level Change= U)
    WHEN OTHERS.
*     Check Header Update Indicaotr is not for Deletion
      CHECK is_so_hdr-updkz <> gc_delete.
*     Check If Order Rejection is set to the Header or Item
      CHECK is_so_item-abgru IS NOT INITIAL.
*     Check if the Order Rejection has been changed
      IF is_so_item_old IS NOT INITIAL.  " Item level has been changed
        IF  is_so_item-abgru <> is_so_item_old-abgru.
          ev_result = gc_true_condition.
        ENDIF.
      ENDIF.

  ENDCASE.

ENDFORM.                    " IS_ORDER_REJECTED
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOR_CHANGES
*&---------------------------------------------------------------------*
*       Check for Changes
*----------------------------------------------------------------------*
*      --> I_ALL_APPL_TABLES     Container with all tables
*      --> IS_XVBAP              Sales Order Item
*      --> IS_XVBAK              Sales Order Header
*      <-- CV_AOT_RELEVANCE      Application Object Type Relevance
*----------------------------------------------------------------------*
FORM check_for_changes
  USING    i_all_appl_tables TYPE trxas_tabcontainer
           is_xvbap          TYPE vbapvb
           is_xvbak          TYPE /saptrx/sd_sds_hdr
  CHANGING cv_aot_relevance  TYPE boole_d.

  DATA:
    lt_yvbak    TYPE STANDARD TABLE OF /saptrx/sd_sds_hdr,
    ls_yvbak    TYPE /saptrx/sd_sds_hdr,
    ls_vbak_new TYPE vbak,
    ls_vbak_old TYPE vbak,
    lt_yvbap    TYPE STANDARD TABLE OF vbapvb,
    ls_yvbap    TYPE vbapvb,
    ls_vbap_new TYPE vbap,
    ls_vbap_old TYPE vbap,
    lt_xvbep    TYPE STANDARD TABLE OF vbepvb,
    ls_xvbep    TYPE vbepvb,
    lt_yvbep    TYPE STANDARD TABLE OF vbepvb,
    ls_yvbep    TYPE vbepvb,
    ls_vbep_new TYPE vbep,
    ls_vbep_old TYPE vbep,
    lt_xvbup    TYPE STANDARD TABLE OF vbupvb,
    ls_xvbup    TYPE vbupvb,
    lt_yvbup    TYPE STANDARD TABLE OF vbupvb,
    ls_yvbup    TYPE vbupvb,
    ls_vbup_new TYPE vbup,
    ls_vbup_old TYPE vbup.

  FIELD-SYMBOLS:
    <upd_tmstmp>    TYPE any.

  cv_aot_relevance = gc_false.

* Read application objects for geting Sales Order Header Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_sales_order_header_old
    CHANGING lt_yvbak.
  READ TABLE lt_yvbak INTO ls_yvbak
    WITH KEY vbeln = is_xvbak-vbeln.
  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING is_xvbak TO ls_vbak_new.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_vbak_new TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    UNASSIGN <upd_tmstmp>.
    MOVE-CORRESPONDING ls_yvbak TO ls_vbak_old.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_vbak_old TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    IF ls_vbak_new <> ls_vbak_old.
      cv_aot_relevance = gc_true.
      RETURN.
    ENDIF.
  ENDIF.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Sales Order Item Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_sales_order_items_old
    CHANGING lt_yvbap.
  READ TABLE lt_yvbap INTO ls_yvbap
    WITH KEY vbeln = is_xvbap-vbeln
             posnr = is_xvbap-posnr
  BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING is_xvbap TO ls_vbap_new.
    MOVE-CORRESPONDING ls_yvbap TO ls_vbap_old.
    IF ls_vbap_new <> ls_vbap_old.
      cv_aot_relevance = gc_true.
      RETURN.
    ENDIF.
  ENDIF.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Sales Order Schedule Line
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_schedule_line_item_new
    CHANGING lt_xvbep.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_schedule_line_item_old
    CHANGING lt_yvbep.
  READ TABLE lt_xvbep WITH KEY vbeln = is_xvbap-vbeln
                               posnr = is_xvbap-posnr
                               updkz = 'I'
                               TRANSPORTING NO FIELDS.
  IF sy-subrc IS INITIAL.
    cv_aot_relevance = gc_true.
    RETURN.
  ELSE.
    READ TABLE lt_yvbep WITH KEY vbeln = is_xvbap-vbeln
                                 posnr = is_xvbap-posnr
                                 updkz = 'D'
                                 TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      cv_aot_relevance = gc_true.
      RETURN.
    ELSE.
      LOOP AT lt_xvbep INTO ls_xvbep
        WHERE vbeln = is_xvbap-vbeln
          AND posnr = is_xvbap-posnr.

        READ TABLE lt_yvbep INTO ls_yvbep
          WITH KEY vbeln = ls_xvbep-vbeln
                   posnr = ls_xvbep-posnr
                   etenr = ls_xvbep-etenr.
        IF sy-subrc IS INITIAL.
          MOVE-CORRESPONDING ls_xvbep TO ls_vbep_new.
          MOVE-CORRESPONDING ls_yvbep TO ls_vbep_old.
          IF ls_vbep_new <> ls_vbep_old.
            cv_aot_relevance = gc_true.
            RETURN.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Sales Order Item Status New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_item_status_new
    CHANGING lt_xvbup.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_item_status_old
    CHANGING lt_yvbup.

  LOOP AT lt_xvbup INTO ls_xvbup
    WHERE vbeln = is_xvbap-vbeln
      AND posnr = is_xvbap-posnr.

    READ TABLE lt_yvbup INTO ls_yvbup
      WITH KEY vbeln = ls_xvbup-vbeln
               posnr = ls_xvbup-posnr.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING ls_xvbup TO ls_vbup_new.
      MOVE-CORRESPONDING ls_yvbup TO ls_vbup_old.
      IF ls_vbup_new <> ls_vbup_old.
        cv_aot_relevance = gc_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFORM.                    " CHECK_FOR_CHANGES
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOR_CHANGES_Delivery
*&---------------------------------------------------------------------*
*       Check for Changes
*----------------------------------------------------------------------*
*      --> I_ALL_APPL_TABLES     Container with all tables
*      --> IS_XLIPS              Delivery Order Item
*      --> IS_XLIKP              Delivery Order Header
*      <-- CV_AOT_RELEVANCE      Application Object Type Relevance
*----------------------------------------------------------------------*
FORM check_for_changes_dlv
  USING    i_all_appl_tables TYPE trxas_tabcontainer
           is_xlips          TYPE lipsvb
           is_xlikp          TYPE likpvb
  CHANGING cv_aot_relevance  TYPE boole_d.

  DATA:
    lt_ylikp    TYPE STANDARD TABLE OF likpvb,
    ls_ylikp    TYPE likpvb,
    ls_likp_new TYPE likp,
    ls_likp_old TYPE likp,
    lt_ylips    TYPE STANDARD TABLE OF lipsvb,
    ls_ylips    TYPE lipsvb,
    ls_lips_new TYPE lips,
    ls_lips_old TYPE lips,
    lt_xvbup    TYPE STANDARD TABLE OF vbupvb,
    ls_xvbup    TYPE vbupvb,
    lt_yvbup    TYPE STANDARD TABLE OF vbupvb,
    ls_yvbup    TYPE vbupvb,
    ls_vbup_new TYPE vbup,
    ls_vbup_old TYPE vbup.

  FIELD-SYMBOLS:
    <upd_tmstmp>    TYPE any.

  cv_aot_relevance = gc_false.

* Read application objects for geting Delivery Order Header Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_header_old
    CHANGING lt_ylikp.
  READ TABLE lt_ylikp INTO ls_ylikp
    WITH KEY vbeln = is_xlikp-vbeln.
  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING is_xlikp TO ls_likp_new.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_likp_new TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    UNASSIGN <upd_tmstmp>.
    MOVE-CORRESPONDING ls_ylikp TO ls_likp_old.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_likp_old TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    IF ls_likp_new <> ls_likp_old.
      cv_aot_relevance = gc_true.
      RETURN.
    ENDIF.
  ENDIF.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Delivery Order Item Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_item_old
    CHANGING lt_ylips.
  READ TABLE lt_ylips INTO ls_ylips
    WITH KEY vbeln = is_xlips-vbeln
             posnr = is_xlips-posnr
  BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING is_xlips TO ls_lips_new.
    MOVE-CORRESPONDING ls_ylips TO ls_lips_old.
    IF ls_lips_new <> ls_lips_old.
      cv_aot_relevance = gc_true.
      RETURN.
    ENDIF.
  ENDIF.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Delivery Order Item Status New/Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_item_stat_new
    CHANGING lt_xvbup.
  READ TABLE lt_xvbup INTO ls_xvbup WITH KEY vbeln = is_xlips-vbeln
                                             posnr = is_xlips-posnr BINARY SEARCH.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_item_stat_old
    CHANGING lt_yvbup.
  READ TABLE lt_yvbup INTO ls_yvbup WITH KEY vbeln = is_xlips-vbeln
                                             posnr = is_xlips-posnr BINARY SEARCH.
  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING ls_xvbup TO ls_vbup_new.
    MOVE-CORRESPONDING ls_yvbup TO ls_vbup_old.
    IF ls_vbup_new <> ls_vbup_old.
      cv_aot_relevance = gc_true.
      RETURN.
    ENDIF.
  ENDIF.


ENDFORM.                    " CHECK_FOR_CHANGES_Delivery
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOR_HEADER_CHANGES
*&---------------------------------------------------------------------*
*       Check for Changes
*----------------------------------------------------------------------*
*      --> I_ALL_APPL_TABLES     Container with all tables
*      --> IS_XVBAK              Sales Order Header
*      <-- CV_AOT_RELEVANCE      Application Object Type Relevance
*----------------------------------------------------------------------*
FORM check_for_header_changes
  USING    i_all_appl_tables TYPE trxas_tabcontainer
           is_xvbak          TYPE /saptrx/sd_sds_hdr
  CHANGING cv_aot_relevance  TYPE boole_d.

  DATA:
    lt_yvbak    TYPE STANDARD TABLE OF /saptrx/sd_sds_hdr,
    ls_yvbak    TYPE /saptrx/sd_sds_hdr,
    ls_vbak_new TYPE vbak,
    ls_vbak_old TYPE vbak,
    lt_xvbkd    TYPE STANDARD TABLE OF vbkdvb,
    ls_xvbkd    TYPE vbkdvb,
    lt_xvbpa    TYPE STANDARD TABLE OF vbpavb,
    ls_xvbpa    TYPE vbpavb,
    lt_xvbap    TYPE STANDARD TABLE OF vbapvb,
    lt_yvbkd    TYPE STANDARD TABLE OF vbkdvb,
    ls_yvbkd    TYPE vbkdvb,
    lt_yvbpa    TYPE STANDARD TABLE OF vbpavb,
    ls_yvbpa    TYPE vbpavb,
    lt_yvbap    TYPE STANDARD TABLE OF vbapvb,
    ls_vbkd_new TYPE vbkd,
    ls_vbkd_old TYPE vbkd,
    ls_vbpa_new TYPE vbpa,
    ls_vbpa_old TYPE vbpa.

  FIELD-SYMBOLS:
    <upd_tmstmp>    TYPE any.

  cv_aot_relevance = gc_false.

* Read application objects for geting Sales Order Header Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_sales_order_header_old
    CHANGING lt_yvbak.
  READ TABLE lt_yvbak INTO ls_yvbak
    WITH KEY vbeln = is_xvbak-vbeln.
  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING is_xvbak TO ls_vbak_new.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_vbak_new TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    UNASSIGN <upd_tmstmp>.
    MOVE-CORRESPONDING ls_yvbak TO ls_vbak_old.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_vbak_old TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    IF ls_vbak_new <> ls_vbak_old.
      cv_aot_relevance = gc_true.
      RETURN.
    ENDIF.
  ENDIF.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Sales Order Business Data New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_business_data_new
    CHANGING lt_xvbkd.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_business_data_old
    CHANGING lt_yvbkd.

  LOOP AT lt_xvbkd INTO ls_xvbkd
    WHERE vbeln = is_xvbak-vbeln
    AND   posnr IS INITIAL.

    READ TABLE lt_yvbkd INTO ls_yvbkd
      WITH KEY vbeln = ls_xvbkd-vbeln
               posnr = ls_xvbkd-posnr.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING ls_xvbkd TO ls_vbkd_new.
      MOVE-CORRESPONDING ls_yvbkd TO ls_vbkd_old.
      IF ls_vbkd_new <> ls_vbkd_old.
        cv_aot_relevance = gc_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Sales Order Partner Data New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_partners_new
    CHANGING lt_xvbpa.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_partners_old
    CHANGING lt_yvbpa.

  LOOP AT lt_xvbpa INTO ls_xvbpa
    WHERE vbeln = is_xvbak-vbeln
    AND   posnr IS INITIAL.

    READ TABLE lt_yvbpa INTO ls_yvbpa
      WITH KEY vbeln = ls_xvbpa-vbeln
               posnr = ls_xvbpa-posnr
               parvw = ls_xvbpa-parvw.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING ls_xvbpa TO ls_vbpa_new.
      MOVE-CORRESPONDING ls_yvbpa TO ls_vbpa_old.
      IF ls_vbpa_new <> ls_vbpa_old.
        cv_aot_relevance = gc_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Sales Order Item Data New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_sales_order_items_new
    CHANGING lt_xvbap.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_sales_order_items_old
    CHANGING lt_yvbap.
  READ TABLE lt_xvbap WITH KEY vbeln = is_xvbak-vbeln
                               updkz = 'I'
                               TRANSPORTING NO FIELDS.
  IF sy-subrc IS INITIAL.
    cv_aot_relevance = gc_true.
    EXIT.
  ELSE.
    READ TABLE lt_yvbap WITH KEY vbeln = is_xvbak-vbeln
                                 updkz = 'D'
                                 TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      cv_aot_relevance = gc_true.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_FOR_HEADER_CHANGES
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOR_HEADER_CHANGES of Delivery
*&---------------------------------------------------------------------*
*       Check for Changes
*----------------------------------------------------------------------*
*      --> I_ALL_APPL_TABLES     Container with all tables
*      --> IS_XLIKP              Delivery Order Header
*      <-- CV_AOT_RELEVANCE      Application Object Type Relevance
*----------------------------------------------------------------------*
FORM check_for_header_changes_dlv
  USING    i_all_appl_tables TYPE trxas_tabcontainer
           is_xlikp          TYPE likpvb
  CHANGING cv_aot_relevance  TYPE boole_d.

  DATA:
    lt_ylikp    TYPE STANDARD TABLE OF likpvb,
    ls_ylikp    TYPE likpvb,
    ls_likp_new TYPE likp,
    ls_likp_old TYPE likp,
    lt_xlips    TYPE STANDARD TABLE OF lipsvb,
    lt_ylips    TYPE STANDARD TABLE OF lipsvb,
    lt_xvbuk    TYPE STANDARD TABLE OF vbukvb,
    lt_yvbuk    TYPE STANDARD TABLE OF vbukvb,
    ls_xvbuk    TYPE vbukvb,
    ls_yvbuk    TYPE vbukvb,
    ls_vbuk_new TYPE vbuk,
    ls_vbuk_old TYPE vbuk.

  FIELD-SYMBOLS:
    <upd_tmstmp>    TYPE any.

  cv_aot_relevance = gc_false.

* Read application objects for geting Delivery Order Header Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_header_old
    CHANGING lt_ylikp.
  READ TABLE lt_ylikp INTO ls_ylikp
    WITH KEY vbeln = is_xlikp-vbeln.
  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING is_xlikp TO ls_likp_new.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_likp_new TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    UNASSIGN <upd_tmstmp>.
    MOVE-CORRESPONDING ls_ylikp TO ls_likp_old.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_likp_old TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    IF ls_likp_new <> ls_likp_old.
      cv_aot_relevance = gc_true.
      RETURN.
    ENDIF.
  ENDIF.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Delivery Status Data New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_hdrstatus_new
    CHANGING lt_xvbuk.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_hdrstatus_old
    CHANGING lt_yvbuk.

  LOOP AT lt_xvbuk INTO ls_xvbuk
    WHERE vbeln = is_xlikp-vbeln.

    READ TABLE lt_yvbuk INTO ls_yvbuk
      WITH KEY vbeln = ls_xvbuk-vbeln.
    IF sy-subrc IS INITIAL.
      MOVE-CORRESPONDING ls_xvbuk TO ls_vbuk_new.
      MOVE-CORRESPONDING ls_yvbuk TO ls_vbuk_old.
      IF ls_vbuk_new <> ls_vbuk_old.
        cv_aot_relevance = gc_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Delivery Order Item Data New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_item_new
    CHANGING lt_xlips.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_delivery_item_old
    CHANGING lt_ylips.
  READ TABLE lt_xlips WITH KEY vbeln = is_xlikp-vbeln
                               updkz = 'I'
                               TRANSPORTING NO FIELDS.
  IF sy-subrc IS INITIAL.
    cv_aot_relevance = gc_true.
    EXIT.
  ELSE.
    READ TABLE lt_ylips WITH KEY vbeln = is_xlikp-vbeln
                                 updkz = 'D'
                                 TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL.
      cv_aot_relevance = gc_true.
      EXIT.
    ENDIF.
  ENDIF.

ENDFORM.                    " CHECK_FOR_HEADER_CHANGES of Delivery
*&---------------------------------------------------------------------*
*&      Form  CHECK_FOR_HEADER_CHANGES of Shipment
*&---------------------------------------------------------------------*
*       Check for Changes
*----------------------------------------------------------------------*
*      --> I_ALL_APPL_TABLES     Container with all tables
*      --> IS_XVTTK              Shipment Header
*      <-- CV_AOT_RELEVANCE      Application Object Type Relevance
*----------------------------------------------------------------------*
FORM check_for_header_changes_shp
  USING    i_all_appl_tables TYPE trxas_tabcontainer
           is_xvttk          TYPE vttkvb
  CHANGING cv_aot_relevance  TYPE boole_d.

  DATA:
    lt_yvttk    TYPE STANDARD TABLE OF vttkvb,
    ls_yvttk    TYPE vttkvb,
    ls_vttk_new TYPE vttk,
    ls_vttk_old TYPE vttk,
    lt_xvtts    TYPE STANDARD TABLE OF vttsvb,
    lt_yvtts    TYPE STANDARD TABLE OF vttsvb,
    lt_xvttp    TYPE STANDARD TABLE OF vttpvb,
    lt_yvttp    TYPE STANDARD TABLE OF vttpvb,
    lt_xvtsp    TYPE STANDARD TABLE OF vtspvb,
    lt_yvtsp    TYPE STANDARD TABLE OF vtspvb,
    lt_vttp_tmp TYPE STANDARD TABLE OF vttpvb.

  FIELD-SYMBOLS:
    <upd_tmstmp>    TYPE any.

  cv_aot_relevance = gc_false.

* Read application objects for geting Shipment Item New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_item_new
    CHANGING lt_xvttp.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_item_old
    CHANGING lt_yvttp.

  MOVE lt_xvttp TO lt_vttp_tmp.
  DELETE lt_xvttp WHERE updkz = ''.
  DELETE lt_yvttp WHERE updkz = ''.

  IF lt_xvttp IS NOT INITIAL OR lt_yvttp IS NOT INITIAL.
    cv_aot_relevance = gc_true.
    RETURN.
  ENDIF.

  CHECK lt_vttp_tmp IS NOT INITIAL.    "at least one delivery is assigned.
  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Shipment Header Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_header_old
    CHANGING lt_yvttk.
  READ TABLE lt_yvttk INTO ls_yvttk
    WITH KEY tknum = is_xvttk-tknum.
  IF sy-subrc IS INITIAL.
    MOVE-CORRESPONDING is_xvttk TO ls_vttk_new.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_vttk_new TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    UNASSIGN <upd_tmstmp>.
    MOVE-CORRESPONDING ls_yvttk TO ls_vttk_old.
* field upd_tmstmp not in all releases but needs to be cleared
    ASSIGN COMPONENT 'UPD_TMSTMP' OF STRUCTURE ls_vttk_old TO <upd_tmstmp>.
    IF sy-subrc EQ 0.
      CLEAR <upd_tmstmp>.
    ENDIF.
    IF ls_vttk_new <> ls_vttk_old.
      cv_aot_relevance = gc_true.
      RETURN.
    ENDIF.
  ENDIF.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Shipment Leg Data New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_leg_new
    CHANGING lt_xvtts.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_leg_old
    CHANGING lt_yvtts.

  DELETE lt_xvtts WHERE updkz = ''.
  DELETE lt_yvtts WHERE updkz = ''.

  IF lt_xvtts IS NOT INITIAL OR lt_yvtts IS NOT INITIAL.
    cv_aot_relevance = gc_true.
    RETURN.
  ENDIF.

  CHECK cv_aot_relevance IS INITIAL.

* Read application objects for geting Shipment Item Leg Data New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_item_leg_new
    CHANGING lt_xvtsp.
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_item_leg_old
    CHANGING lt_yvtsp.

  DELETE lt_xvtsp WHERE updkz = ''.
  DELETE lt_yvtsp WHERE updkz = ''.

  IF lt_xvtsp IS NOT INITIAL OR lt_yvtsp IS NOT INITIAL.
    cv_aot_relevance = gc_true.
    RETURN.
  ENDIF.

ENDFORM.                    " CHECK_FOR_HEADER_CHANGES of Shipment
*&---------------------------------------------------------------------*
*&      Form  CHECK_Delivery_Assignment
*&---------------------------------------------------------------------*
*       CHECK_Delivery_Assignment
*----------------------------------------------------------------------*
*      --> I_ALL_APPL_TABLES     Container with all tables
*      --> IS_XVTTK              Shipment Header
*      <-- CV_AOT_RELEVANCE      Application Object Type Relevance
*----------------------------------------------------------------------*
FORM check_delivery_assignment
  USING    i_all_appl_tables TYPE trxas_tabcontainer
           is_xvttk          TYPE vttkvb
  CHANGING cv_aot_relevance  TYPE boole_d.

  DATA:
    lt_xvttp    TYPE STANDARD TABLE OF vttpvb.

  CLEAR cv_aot_relevance.

* Read application objects for geting Shipment Item New / Old
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_shipment_item_new
    CHANGING lt_xvttp.

  IF lt_xvttp IS NOT INITIAL.
    cv_aot_relevance = gc_true.
  ENDIF.

ENDFORM.                    " CHECK_Delivery Assignment


*&---------------------------------------------------------------------*
*&      Form  READ_BUSINESS_DATA
*&---------------------------------------------------------------------*
*       Get Business Data
*----------------------------------------------------------------------*
*      --> IT_XVBKD  Business Data New
*      --> IV_VBELN  Sales Order Number
*      --> IV_POSNR  Sales Order Item
*      <-- ES_XVBKD  Business Data
*----------------------------------------------------------------------*
FORM read_business_data
  USING    it_xvbkd TYPE va_vbkdvb_t
           iv_vbeln TYPE vbeln_va
           iv_posnr TYPE posnr_va
  CHANGING es_xvbkd TYPE vbkdvb.

* Read Business Data with Item Number
  READ TABLE it_xvbkd INTO es_xvbkd
    WITH KEY vbeln = iv_vbeln
             posnr = iv_posnr.
* Not Found -> Same Infomration assigned on Header and Item Level
  IF sy-subrc IS NOT INITIAL.
    READ TABLE it_xvbkd INTO es_xvbkd
      WITH KEY vbeln = iv_vbeln.
  ENDIF.

ENDFORM.                    " READ_BUSINESS_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_BUSINESS_PARTNER
*&---------------------------------------------------------------------*
*       Get Business Partner
*----------------------------------------------------------------------*
*      --> IT_XVBPA  Business Partner New
*      --> IV_VBELN  Sales Order Number
*      --> IV_POSNR  Sales Order Item
*      --> IV_PARVW  Partner Function
*      <-- ES_XVBPA  Business Partner
*----------------------------------------------------------------------*
FORM read_business_partner
  USING    it_xvbpa TYPE vbpavb_tab
           iv_vbeln TYPE vbeln_va
           iv_posnr TYPE posnr_va
           iv_parvw TYPE parvw
  CHANGING es_xvbpa TYPE vbpavb.

  CLEAR es_xvbpa.
* Read Business Partners
  READ TABLE it_xvbpa INTO es_xvbpa
    WITH KEY vbeln = iv_vbeln
             posnr = iv_posnr
             parvw = iv_parvw.
* Not Found -> Same Business Partner on header and item level
  IF sy-subrc IS NOT INITIAL.
    READ TABLE it_xvbpa INTO es_xvbpa
      WITH KEY vbeln = iv_vbeln
               parvw = iv_parvw.
  ENDIF.

ENDFORM.                    " READ_BUSINESS_PARTNER

*&---------------------------------------------------------------------*
*&      Form  CHECK_SUBSEQ_DOC_AVAILABLE
*&---------------------------------------------------------------------*
*       Check Subsequent Document is Available or Not
*----------------------------------------------------------------------*
*      --> I_ALL_APPL_TABLES     Container with all tables
*      <-- CV_AOT_RELEVANCE      Application Object Type Relevance
*----------------------------------------------------------------------*
FORM check_subseq_doc_available
  USING    i_all_appl_tables TYPE trxas_tabcontainer
  CHANGING cv_aot_relevance  TYPE boole_d.

  DATA:
    lt_xvbfa   TYPE STANDARD TABLE OF vbfavb.

  CLEAR cv_aot_relevance.
* Read application objects for geting Document Flow
  PERFORM read_appl_table
    USING    i_all_appl_tables
             gc_bpt_document_flow_new
    CHANGING lt_xvbfa.
* Check is Subsequent Document is aviable or not
  IF lt_xvbfa[] IS NOT INITIAL.
    cv_aot_relevance = gc_false.
  ELSE.
    cv_aot_relevance = gc_true.
  ENDIF.

ENDFORM.                    " CHECK_SUBSEQ_DOC_AVAILABLE


*&---------------------------------------------------------------------*
*&      Form  set_local_timestamp
*&---------------------------------------------------------------------*
*       Concateneate Time Stamp
*----------------------------------------------------------------------*
*      --> EV_DATE     Date
*      --> EV_TIME     Time
*      <-- CV_TSMP     Time stamp (Local date and time/format)
*----------------------------------------------------------------------*
FORM set_local_timestamp
  USING    ev_date   LIKE sy-datum
           ev_time   LIKE sy-uzeit
  CHANGING cv_tsmp   TYPE /saptrx/event_exp_datetime.

  DATA: lv_timestrg(15).

  IF ev_date IS INITIAL.
    CLEAR cv_tsmp.
  ELSE.
    CONCATENATE ev_date ev_time INTO lv_timestrg.
    cv_tsmp = lv_timestrg.
  ENDIF.

ENDFORM.
