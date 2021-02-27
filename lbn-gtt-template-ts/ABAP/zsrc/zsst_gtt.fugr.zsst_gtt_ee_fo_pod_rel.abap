FUNCTION zsst_gtt_ee_fo_pod_rel.
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
  FIELD-SYMBOLS <ls_tor_root> TYPE /scmtms/s_em_bo_tor_root.

  TRY.
      lcl_actual_event=>get_tor_actual_event_class( i_event )->check_event_relevance(
        EXPORTING
          i_all_appl_tables = i_all_appl_tables
          iv_event_code     = /scmtms/if_tor_const=>sc_tor_event-pod
          i_event           = i_event
        IMPORTING
          e_result          = e_result ).
    CATCH cx_udm_message INTO DATA(lo_udm_message).
      lcl_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = DATA(ls_bapiret) ).
      APPEND ls_bapiret TO c_logtable.
      RAISE stop_processing.
  ENDTRY.

  IF e_result = lif_ef_constants=>cs_condition-false.
    RETURN.
  ENDIF.

  ASSIGN i_event-maintabref->* TO <ls_tor_root>.
  IF sy-subrc = 0 AND <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit.
    IF i_event_types-mainobjtab <> /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-root.
      RAISE stop_processing.
    ENDIF.
    IF e_result IS INITIAL.
      /scmtms/cl_em_tm_helper=>check_event_relevance(
         EXPORTING
           iv_event_code       = /scmtms/if_tor_const=>sc_tor_event-pod
           i_all_appl_tables   = i_all_appl_tables
           is_tor_root         = <ls_tor_root>
         CHANGING
           cv_result           = e_result
         EXCEPTIONS
          stop_processing   = 3 ).
      IF sy-subrc = 3.
        RAISE stop_processing.
      ENDIF.
    ENDIF.
    RETURN.
  ENDIF.

  CALL FUNCTION '/SCMTMS/REL_EVT_TOR_POD'
    EXPORTING
      i_appsys               = i_appsys
      i_event_types          = i_event_types
      i_all_appl_tables      = i_all_appl_tables
      i_eventtype_tab        = i_eventtype_tab
      i_event                = i_event
    IMPORTING
      e_result               = e_result
    TABLES
      c_logtable             = c_logtable
    EXCEPTIONS
      parameter_error        = 1
      relevance_determ_error = 2
      stop_processing        = 3
      OTHERS                 = 4.
  CASE sy-subrc.
    WHEN 1.
      RAISE parameter_error.
    WHEN 2.
      RAISE relevance_determ_error.
    WHEN 3.
      RAISE stop_processing.
  ENDCASE.

ENDFUNCTION.
