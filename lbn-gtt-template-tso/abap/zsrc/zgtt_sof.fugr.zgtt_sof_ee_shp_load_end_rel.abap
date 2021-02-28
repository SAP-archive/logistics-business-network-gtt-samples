FUNCTION zgtt_sof_ee_shp_load_end_rel .
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
* Shipment Hearder new
    lt_xvttk         TYPE STANDARD TABLE OF vttkvb,
* Shipment Hearder old
    lt_yvttk         TYPE STANDARD TABLE OF vttkvb,
    lv_aot_relevance TYPE boole_d.

  FIELD-SYMBOLS:
    <ls_xvttk> TYPE vttkvb,
    <ls_yvttk> TYPE vttkvb.

* <1> Read necessary application tables from table reference
  PERFORM read_appl_tables_shipmt_header
  TABLES lt_xvttk
         lt_yvttk
  USING  i_all_appl_tables.

* <2> Check if Main table is Shipment or not.
  IF i_event-maintabdef <> gc_bpt_shipment_header_new.
    PERFORM create_logtable_et_rel
    TABLES c_logtable
    USING i_event-maintabdef
          space
          i_event_types-trrelfunc
          i_event-eventtype
          i_appsys.
    RAISE parameter_error.
  ELSE.
*   Read Main Object Table (Shipment  - VTTK)
    ASSIGN i_event-maintabref->*  TO <ls_xvttk>.
  ENDIF.

* <3> Check Relevance of AOT: YN_OTE
  PERFORM check_aot_relevance_shp
     USING    <ls_xvttk>
     CHANGING lv_aot_relevance.
  CHECK lv_aot_relevance IS NOT INITIAL.

*<4> When shipment is newly created, check relevance of GTT: only when delivery has been assigned.
  IF <ls_xvttk>-updkz EQ gc_insert.
    PERFORM check_delivery_assignment
      USING    i_all_appl_tables
               <ls_xvttk>
      CHANGING lv_aot_relevance.
* If Shipment is NOT newly created, Check is Critical Changes Made
  ELSE.
    PERFORM check_for_header_changes_shp
      USING    i_all_appl_tables
               <ls_xvttk>
      CHANGING lv_aot_relevance.
  ENDIF.
  CHECK lv_aot_relevance IS NOT INITIAL.

* <5> Check actual event date and time changed.
  lv_aot_relevance = gc_false.
  IF <ls_xvttk>-updkz EQ gc_insert.
    IF <ls_xvttk>-dalen IS NOT INITIAL.
      lv_aot_relevance = gc_true.
    ENDIF.
  ELSE.
    READ TABLE lt_yvttk ASSIGNING <ls_yvttk>
      WITH KEY tknum = <ls_xvttk>-tknum
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF <ls_xvttk>-dalen <> <ls_yvttk>-dalen
      OR <ls_xvttk>-ualen <> <ls_yvttk>-ualen.
        IF <ls_xvttk>-dalen IS NOT INITIAL.
          lv_aot_relevance = gc_true.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.

  IF lv_aot_relevance = gc_true.
    e_result = gc_true_condition.
  ELSE.
    e_result = gc_false_condition.
  ENDIF.
ENDFUNCTION.
