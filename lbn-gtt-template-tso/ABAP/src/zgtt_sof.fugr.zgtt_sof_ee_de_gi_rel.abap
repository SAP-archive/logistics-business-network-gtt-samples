FUNCTION ZGTT_SOF_EE_DE_GI_REL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_APPSYS) TYPE  /SAPTRX/APPLSYSTEM
*"     REFERENCE(I_EVENT_TYPES) TYPE  /SAPTRX/EVTYPES
*"     REFERENCE(I_ALL_APPL_TABLES) TYPE  TRXAS_TABCONTAINER
*"     REFERENCE(I_EVENTTYPE_TAB) TYPE  TRXAS_EVENTTYPE_TABS_WA
*"     REFERENCE(I_EVENT) TYPE  TRXAS_EVT_CTAB_WA
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

*   Sales Document: Header Status and Administrative Data New
    lt_xvbuk TYPE STANDARD TABLE OF vbukvb,
*   Sales Document: Header Status and Administrative Data Old
    lt_yvbuk TYPE STANDARD TABLE OF vbukvb,
    lv_aot_relevance TYPE boole_d.

  FIELD-SYMBOLS:
*   Work Structure for Delivery Header New
    <ls_xlikp> TYPE likpvb,
*   Work structure for Sales Document: Header Status and Admin Data New
    <ls_xvbuk>  TYPE vbukvb,
*   Work structure for Sales Document: Header Status and Admin Data Old
    <ls_yvbuk>  TYPE vbukvb.

* <1> Read necessary application tables from table reference
  PERFORM read_appl_tables_status
  TABLES lt_xvbuk
         lt_yvbuk
  USING  i_all_appl_tables.

* <2> Check if Main table is Delivery Order Header or not.
  IF i_event-maintabdef <> gc_bpt_delivery_header_new.
    PERFORM create_logtable_et_rel
    TABLES c_logtable
    USING i_event-maintabdef
          space
          i_event_types-trrelfunc
          i_event-eventtype
          i_appsys.
    RAISE parameter_error.
  ELSE.
* Read Main Object Table (Delivery Order Header - LIKP)
    ASSIGN i_event-maintabref->* TO <ls_xlikp>.
  ENDIF.

* <3> Check Relevance of AOT: YN_OTE
  PERFORM check_aot_relevance_dlv
  USING    <ls_xlikp>
  CHANGING lv_aot_relevance.
  CHECK lv_aot_relevance IS NOT INITIAL.

* <4> Check Status - If Goods Issue Status is completed
  lv_aot_relevance = gc_false.
  READ TABLE lt_xvbuk ASSIGNING <ls_xvbuk>
       WITH KEY vbeln = <ls_xlikp>-vbeln
  BINARY SEARCH.
  CHECK sy-subrc IS INITIAL.
  IF <ls_xvbuk>-wbstk = 'C'.
*   Check Status Before - If GI Status is completed
    READ TABLE lt_yvbuk ASSIGNING <ls_yvbuk>
         WITH KEY vbeln = <ls_xlikp>-vbeln
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF <ls_yvbuk>-wbstk <> 'C'.
        lv_aot_relevance = gc_true.
      ENDIF.
    ELSE.
      IF <ls_xvbuk>-updkz = gc_insert.
        lv_aot_relevance = gc_true.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lv_aot_relevance = gc_true.
    e_result = gc_true_condition.
  ELSE.
    e_result = gc_false_condition.
  ENDIF.

ENDFUNCTION.
