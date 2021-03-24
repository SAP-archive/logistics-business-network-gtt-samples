FUNCTION ZSST_GTT_EE_FU_DELAY_REL.
*"--------------------------------------------------------------------
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
*"--------------------------------------------------------------------
  FIELD-SYMBOLS: <ls_tor_root> TYPE /scmtms/s_em_bo_tor_root.

  TRY.
      lcl_actual_event=>get_tor_actual_event_class( i_event )->check_event_relevance(
        EXPORTING
          i_all_appl_tables = i_all_appl_tables
          iv_event_code     = /scmtms/if_tor_const=>sc_tor_event-delay_fu
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

  e_result = lif_ef_constants=>cs_condition-false.
  IF i_event_types-mainobjtab <> /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-root.
    RAISE stop_processing.
  ENDIF.

  ASSIGN i_event-maintabref->* TO <ls_tor_root>.
  IF sy-subrc <> 0.
    RAISE stop_processing.
  ENDIF.

  /scmtms/cl_cust_factory=>get_instance_tor_type_cust( )->get_tortype(
    EXPORTING
      iv_type    = <ls_tor_root>-tor_type
    IMPORTING
      es_tortype = DATA(ls_torty) ).

  DATA(lv_is_correct_tor_cat) = xsdbool( <ls_tor_root>-tor_cat = /scmtms/if_tor_const=>sc_tor_category-freight_unit ).

  IF ls_torty-track_exec_rel = /scmtms/if_tor_const=>sc_tracking_relevance-exec_track_em  AND
     lv_is_correct_tor_cat   = abap_true.

    /scmtms/cl_em_tm_helper=>check_event_relevance(
      EXPORTING
        iv_event_code     = /scmtms/if_tor_const=>sc_tor_event-delay_fu
        i_all_appl_tables = i_all_appl_tables
        is_tor_root       = <ls_tor_root>
      CHANGING
        cv_result         = e_result
      EXCEPTIONS
        stop_processing   = 3 ).
    IF sy-subrc = 3.
      RAISE stop_processing.
    ENDIF.
  ENDIF.

ENDFUNCTION.
