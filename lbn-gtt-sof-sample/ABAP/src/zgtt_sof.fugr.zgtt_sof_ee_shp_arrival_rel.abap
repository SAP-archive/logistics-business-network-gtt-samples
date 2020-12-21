FUNCTION zgtt_sof_ee_shp_arrival_rel .
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
* Shipment Leg new
    lt_xvtts         TYPE STANDARD TABLE OF vttsvb,
* Shipment Hearder old
    lt_yvtts         TYPE STANDARD TABLE OF vttsvb,
    lv_aot_relevance TYPE boole_d.

  FIELD-SYMBOLS:
    <ls_xvttk> TYPE vttkvb,
    <ls_xvtts> TYPE vttsvb,
    <ls_yvtts> TYPE vttsvb.

* <1> Read necessary application tables from table reference
  PERFORM read_appl_tables_shipment_leg
  TABLES lt_xvtts
         lt_yvtts
  USING  i_all_appl_tables.

* <2> Check if Main table is Shipment Header or not.
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
*   Read Main Object Table (Shipment  - VTTS)
    ASSIGN i_event-maintabref->*  TO <ls_xvttk>.
  ENDIF.

* <3> Check Relevance of AOT: YN_OTE
  PERFORM check_aot_relevance_shp
     USING    <ls_xvttk>
     CHANGING lv_aot_relevance.
  CHECK lv_aot_relevance IS NOT INITIAL.

* <4> Check actual event date and time changed.
  lv_aot_relevance = gc_false.
  LOOP AT lt_xvtts ASSIGNING <ls_xvtts>.
    IF <ls_xvtts>-updkz EQ gc_insert.
      IF <ls_xvtts>-daten IS NOT INITIAL.
        lv_aot_relevance = gc_true.
        EXIT.
      ENDIF.
    ELSEIF <ls_xvtts>-updkz EQ gc_update.
      READ TABLE lt_yvtts ASSIGNING <ls_yvtts>
        WITH KEY tknum = <ls_xvtts>-tknum
                 tsnum = <ls_xvtts>-tsnum
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        IF <ls_xvtts>-daten <> <ls_yvtts>-daten
        OR <ls_xvtts>-uaten <> <ls_yvtts>-uaten.
          IF <ls_xvtts>-daten IS NOT INITIAL.
            lv_aot_relevance = gc_true.
            EXIT.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF lv_aot_relevance = gc_true.
    e_result = gc_true_condition.
  ELSE.
    e_result = gc_false_condition.
  ENDIF.
ENDFUNCTION.
