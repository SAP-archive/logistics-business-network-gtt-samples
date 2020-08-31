FUNCTION ZGTT_SOF_EE_DE_PICKING_REL.
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

*   Sales Document: Delivery Item Data New
    lt_xvlips TYPE STANDARD TABLE OF lipsvb,
*   Sales Document: Delivery Item Data New Old
    lt_yvlips TYPE STANDARD TABLE OF lipsvb,
    lv_aot_relevance TYPE boole_d.

  FIELD-SYMBOLS:
*   Work Structure for Delivery Header New
  <ls_xlikp> TYPE likpvb,
*   Work Structure for Delivery Item New
  <ls_xvlips> TYPE lipsvb,
*   Work Structure for Delivery Item Old
  <ls_yvlips> TYPE lipsvb,
*   Work structure for Sales Document: Header Status and Admin Data New
  <ls_xvbuk>  TYPE vbukvb,
*   Work structure for Sales Document: Header Status and Admin Data Old
  <ls_yvbuk>  TYPE vbukvb.

* <1> Read necessary application tables from table reference
  PERFORM read_appl_tables_delivery_item
  TABLES lt_xvlips
         lt_yvlips
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

* <5> Check picking quantity changed
  lv_aot_relevance = gc_false.

  LOOP at lt_xvlips ASSIGNING <ls_xvlips>
                    WHERE vbeln = <ls_xlikp>-vbeln.
    READ TABLE lt_yvlips ASSIGNING <ls_yvlips>
          WITH KEY vbeln = <ls_xvlips>-vbeln
                   posnr = <ls_xvlips>-posnr
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF <ls_xvlips>-pikmg <> <ls_yvlips>-pikmg.
        lv_aot_relevance = gc_true.
        EXIT.
      ENDIF.
* newly created delivery order and picked item
    ELSE.
      IF <ls_xvlips>-pikmg <> 0
      AND <ls_xvlips>-updkz = 'I'.
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
