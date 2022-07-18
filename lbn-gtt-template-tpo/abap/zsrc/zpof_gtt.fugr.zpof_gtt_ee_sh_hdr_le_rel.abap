FUNCTION ZPOF_GTT_EE_SH_HDR_LE_REL .
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
  DATA: lo_udm_message TYPE REF TO cx_udm_message,
        ls_bapiret     TYPE bapiret2.

  TRY.
      e_result  = zcl_gtt_pof_ae_performer=>check_relevance(
        EXPORTING
          is_definition       = VALUE #( maintab = zif_gtt_pof_app_constants=>cs_tabledef-sh_header_new )
          io_ae_factory       = NEW zcl_gtt_pof_ae_factory_shh_le( )
          iv_appsys           = i_appsys
          is_event_type       = i_event_types
          it_all_appl_tables  = i_all_appl_tables
          is_events           = i_event
      ).

    CATCH cx_udm_message INTO lo_udm_message.
      zcl_gtt_pof_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = ls_bapiret ).

      " add error message
      APPEND ls_bapiret TO c_logtable.

      " throw corresponding exception
      CASE lo_udm_message->textid.
        WHEN zif_gtt_pof_ef_constants=>cs_errors-stop_processing.
          RAISE stop_processing.
        WHEN zif_gtt_pof_ef_constants=>cs_errors-table_determination.
          RAISE relevance_determ_error.
      ENDCASE.
  ENDTRY.

ENDFUNCTION.
