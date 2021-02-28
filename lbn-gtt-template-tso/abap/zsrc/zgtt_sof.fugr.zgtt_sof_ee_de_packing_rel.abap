FUNCTION ZGTT_SOF_EE_DE_PACKING_REL.
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
*   Packing item table new
    lt_xvepo          TYPE STANDARD TABLE OF vepovb,
*   Packing item table old
    lt_yvepo          TYPE STANDARD TABLE OF vepovb,
    lv_aot_relevance  TYPE boole_d,
*   New packing quantity
    lv_xquantity       TYPE vepovb-vemng,
*   Old packing quantity
    lv_yquantity       TYPE vepovb-vemng,
    lv_insert          TYPE boole_d.

  FIELD-SYMBOLS:
*   Work Structure for Delivery Header New
    <ls_xlikp>        TYPE likpvb,
*   Work Structure for Delivery Item New
    <ls_xvlips>       TYPE lipsvb,
*   Work Structure for Packing Item New
    <ls_xvepo>        TYPE vepovb,
*   Work Structure for Packing Item Old
    <ls_yvepo>        TYPE vepovb.


* <1> Read necessary application tables from table reference
  PERFORM read_appl_tables_packing_item
  TABLES  lt_xvepo
          lt_yvepo
  USING   i_all_appl_tables.

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

* <5> Check delivery quantity changed
  lv_aot_relevance = gc_false.

  CLEAR lv_xquantity.
  CLEAR lv_insert.
  LOOP AT lt_xvepo ASSIGNING <ls_xvepo>
                     WHERE vbeln = <ls_xvlips>-vbeln
                     AND   posnr = <ls_xvlips>-posnr.
      lv_xquantity = lv_xquantity + <ls_xvepo>-vemng.
      IF <ls_xvepo>-updkz = 'I'.
        lv_insert = gc_true.
      ENDIF.
  ENDLOOP.

  CLEAR lv_yquantity.
  LOOP AT lt_yvepo ASSIGNING <ls_yvepo>
                     WHERE vbeln = <ls_xvlips>-vbeln
                     AND   posnr = <ls_xvlips>-posnr.
      lv_yquantity = lv_yquantity + <ls_yvepo>-vemng.
  ENDLOOP.

  IF lv_yquantity <> 0.
    IF lv_xquantity <> lv_yquantity.
       lv_aot_relevance = gc_true.
    ENDIF.
  ELSE.
    IF lv_xquantity <> 0
    AND lv_insert = gc_true.
      lv_aot_relevance = gc_true.
    ENDIF.
  ENDIF.

  IF lv_aot_relevance = gc_true.
    e_result = gc_true_condition.
  ELSE.
    e_result = gc_false_condition.
  ENDIF.

ENDFUNCTION.
