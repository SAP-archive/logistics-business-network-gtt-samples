FUNCTION ZGTT_SOF_OTE_SO_ITM_REL .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_APPSYS) TYPE  /SAPTRX/APPLSYSTEM
*"     REFERENCE(I_APP_OBJ_TYPES) TYPE  /SAPTRX/AOTYPES
*"     REFERENCE(I_ALL_APPL_TABLES) TYPE  TRXAS_TABCONTAINER
*"     REFERENCE(I_APPTYPE_TAB) TYPE  TRXAS_APPTYPE_TABS_WA
*"     REFERENCE(I_APP_OBJECT) TYPE  TRXAS_APPOBJ_CTAB_WA
*"  EXPORTING
*"     VALUE(E_RESULT) LIKE  SY-BINPT
*"  TABLES
*"      C_LOGTABLE STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      RELEVANCE_DETERM_ERROR
*"      STOP_PROCESSING
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
* Top Include
* TYPE-POOLS:trxas.
*----------------------------------------------------------------------*

DATA:
    lv_aot_relevance TYPE boole_d.

  FIELD-SYMBOLS:
    <ls_xvbak>       TYPE /saptrx/sd_sds_hdr,
    <ls_xvbap>       TYPE vbapvb.

* <1> Check if Main table is Sales Order Item or not.
  IF i_app_object-maintabdef <> gc_bpt_sales_order_items_new.
    PERFORM create_logtable_ao_rel
      TABLES c_logtable
      USING  i_app_object-maintabdef
             space
             i_app_obj_types-trrelfunc
             i_app_object-appobjtype
             i_appsys.
    RAISE parameter_error.
  ELSE.
*   Read Main Object Table (Sales Order Item - VBAP)
    ASSIGN i_app_object-maintabref->* TO <ls_xvbap>.
  ENDIF.

* <2> Check if Master table is Sales Order Header or not.
  IF i_app_object-mastertabdef <> gc_bpt_sales_order_header_new.
    PERFORM create_logtable_ao_rel
      TABLES c_logtable
      USING  space
             i_app_object-mastertabdef
             i_app_obj_types-trrelfunc
             i_app_object-appobjtype
             i_appsys.
    RAISE parameter_error.
  ELSE.
*   Read Master Object Table (Sales Order Header - VBAK)
    ASSIGN i_app_object-mastertabref->* TO <ls_xvbak>.
  ENDIF.

* <3> Check Relevance of AOT: YN_OTE
  PERFORM check_aot_relevance
     USING    <ls_xvbak>
     CHANGING lv_aot_relevance.
  CHECK lv_aot_relevance IS NOT INITIAL.

* <4> If Sales Order Item is NOT newly created, Check is Critical Changes Made
  IF <ls_xvbap>-updkz <> gc_insert.
    PERFORM check_for_changes
      USING    i_all_appl_tables
               <ls_xvbap>
               <ls_xvbak>
      CHANGING lv_aot_relevance.
  ENDIF.

  IF lv_aot_relevance = gc_true.
    e_result = gc_true_condition.
  ELSE.
    e_result = gc_false_condition.
  ENDIF.

ENDFUNCTION.
