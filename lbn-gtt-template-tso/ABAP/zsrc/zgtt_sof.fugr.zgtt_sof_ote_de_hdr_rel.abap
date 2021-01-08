FUNCTION zgtt_sof_ote_de_hdr_rel.
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
    <ls_xlikp>       TYPE likpvb.

* <1> Check if Main table is Delivery Order or not.
  IF i_app_object-maintabdef <> gc_bpt_delivery_header_new.
    PERFORM create_logtable_ao_rel
      TABLES c_logtable
      USING  i_app_object-maintabdef
             space
             i_app_obj_types-trrelfunc
             i_app_object-appobjtype
             i_appsys.
    RAISE parameter_error.
  ELSE.
*   Read Main Object Table (Delivery Order - LIKP)
    ASSIGN i_app_object-maintabref->* TO <ls_xlikp>.
  ENDIF.


* <3> Check Relevance of AOT: YN_OTE
  PERFORM check_aot_relevance_dlv
     USING    <ls_xlikp>
     CHANGING lv_aot_relevance.
  CHECK lv_aot_relevance IS NOT INITIAL.

* <4> If Subsequent Document is created, Do not update Sales Order
*  IF <ls_xvbak>-updkz <> gc_insert.
*    PERFORM check_subseq_doc_available
*      USING    i_all_appl_tables
*      CHANGING lv_aot_relevance.
*  ENDIF.
*  CHECK lv_aot_relevance IS NOT INITIAL.

* <5> If Delivery Order is NOT newly created, Check is Critical Changes Made
  IF <ls_xlikp>-updkz <> gc_insert.
    PERFORM check_for_header_changes_dlv
      USING    i_all_appl_tables
               <ls_xlikp>
      CHANGING lv_aot_relevance.
  ENDIF.

  IF lv_aot_relevance = gc_true.
    e_result = gc_true_condition.
  ELSE.
    e_result = gc_false_condition.
  ENDIF.

ENDFUNCTION.
