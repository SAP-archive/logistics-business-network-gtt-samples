FUNCTION ZGTT_SOF_EE_DE_POD_REL.
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

*   Sales Document: Delivery Item Status Data New
    lt_xvbup TYPE STANDARD TABLE OF vbupvb,
*   Sales Document: Delivery Item Status Data New Old
    lt_yvbup TYPE STANDARD TABLE OF vbupvb,
    lv_aot_relevance TYPE boole_d.

  FIELD-SYMBOLS:
*   Work Structure for Delivery Header New
  <ls_xlikp> TYPE likpvb,
*   Work Structure for Delivery Item New
  <ls_xvlips> TYPE lipsvb,
*   Work Structure for Delivery Item Old
  <ls_yvlips> TYPE lipsvb,
*   Work structure for Sales Document: Header Status and Admin Data New
  <ls_xvbup>  TYPE vbupvb,
*   Work structure for Sales Document: Header Status and Admin Data Old
  <ls_yvbup>  TYPE vbupvb.

* <1> Read necessary application tables from table reference
  PERFORM read_appl_tables_pod_items
  TABLES lt_xvbup
         lt_yvbup
  USING  i_all_appl_tables.

* <2> Check if Main table is Delivery Order Item or not.
  IF i_event-maintabdef <> gc_bpt_delivery_item_new.
    PERFORM create_logtable_et_rel
    TABLES c_logtable
    USING i_event-maintabdef
          space
          i_event_types-trrelfunc
          i_event-eventtype
          i_appsys.
    RAISE parameter_error.
  ELSE.
* Read Main Object Table (Delivery Order Item - LIPS)
    ASSIGN i_event-maintabref->* TO <ls_xvlips>.
  ENDIF.

* <3> Check if Master table is Delivery Order Header or not.
  IF i_event-mastertabdef <> gc_bpt_delivery_header_new.
    PERFORM create_logtable_et_rel
    TABLES c_logtable
    USING  i_event-mastertabdef
          space
          i_event_types-trrelfunc
          i_event-eventtype
          i_appsys.
    RAISE parameter_error.
  ELSE.
*   Read Master Object Table (Delivery Order Header - LIKP)
    ASSIGN i_event-mastertabref->* TO <ls_xlikp>.
  ENDIF.

* <4> Check Relevance of AOT: YN_OTE
  PERFORM check_aot_relevance_dlv
  USING    <ls_xlikp>
  CHANGING lv_aot_relevance.
  CHECK lv_aot_relevance IS NOT INITIAL.

* <5> Check POD status changed
  lv_aot_relevance = gc_false.

  LOOP AT lt_xvbup ASSIGNING <ls_xvbup>
                    WHERE vbeln = <ls_xlikp>-vbeln
                    AND   posnr = <ls_xvlips>-posnr
                    AND   pdsta IS NOT INITIAL.
    READ TABLE lt_yvbup ASSIGNING <ls_yvbup>
          WITH KEY vbeln = <ls_xvlips>-vbeln
                   posnr = <ls_xvlips>-posnr
          BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF <ls_xvbup>-pdsta <> <ls_yvbup>-pdsta.
        lv_aot_relevance = gc_true.
        EXIT.
      ENDIF.
    ELSE.
      IF <ls_xvbup>-pdsta <> 'A'
      AND <ls_xvbup>-updkz = 'I'.
        lv_aot_relevance = gc_true.
        EXIT.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_aot_relevance = gc_true.
    e_result = gc_true_condition.
  ELSE.
    e_result = gc_false_condition.
  ENDIF.

ENDFUNCTION.
