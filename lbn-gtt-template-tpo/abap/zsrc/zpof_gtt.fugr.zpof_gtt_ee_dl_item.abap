FUNCTION ZPOF_GTT_EE_DL_ITEM .
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_APPSYS) TYPE  /SAPTRX/APPLSYSTEM
*"     REFERENCE(I_APP_OBJ_TYPES) TYPE  /SAPTRX/AOTYPES
*"     REFERENCE(I_ALL_APPL_TABLES) TYPE  TRXAS_TABCONTAINER
*"     REFERENCE(I_APP_TYPE_CNTL_TABS) TYPE  TRXAS_APPTYPE_TABS
*"     REFERENCE(I_APP_OBJECTS) TYPE  TRXAS_APPOBJ_CTABS
*"  TABLES
*"      E_EXPEVENTDATA STRUCTURE  /SAPTRX/EXP_EVENTS
*"      E_MEASRMNTDATA STRUCTURE  /SAPTRX/MEASR_DATA OPTIONAL
*"      E_INFODATA STRUCTURE  /SAPTRX/INFO_DATA OPTIONAL
*"      E_LOGTABLE STRUCTURE  BAPIRET2 OPTIONAL
*"  EXCEPTIONS
*"      PARAMETER_ERROR
*"      EXP_EVENT_DETERM_ERROR
*"      TABLE_DETERMINATION_ERROR
*"      STOP_PROCESSING
*"----------------------------------------------------------------------
  DATA: lo_udm_message TYPE REF TO cx_udm_message,
        ls_bapiret     TYPE bapiret2.

  CLEAR e_logtable[].

  TRY.
      zcl_gtt_pof_ef_performer=>get_planned_events(
        EXPORTING
          is_definition         = VALUE #(
                                    maintab   = zif_gtt_pof_app_constants=>cs_tabledef-dl_item_new
                                    mastertab = zif_gtt_pof_app_constants=>cs_tabledef-dl_header_new )
          io_factory            = NEW zcl_gtt_pof_tp_factory_dli( )
          iv_appsys             = i_appsys
          is_app_obj_types      = i_app_obj_types
          it_all_appl_tables    = i_all_appl_tables
          it_app_type_cntl_tabs = i_app_type_cntl_tabs
          it_app_objects        = i_app_objects
        CHANGING
          ct_expeventdata       = e_expeventdata[]
          ct_measrmntdata       = e_measrmntdata[]
          ct_infodata           = e_infodata[] ).

    CATCH cx_udm_message INTO lo_udm_message.
      zcl_gtt_pof_tools=>get_errors_log(
        EXPORTING
          io_umd_message = lo_udm_message
          iv_appsys      = i_appsys
        IMPORTING
          es_bapiret     = ls_bapiret ).

      " add error message
      APPEND ls_bapiret TO e_logtable.

      " throw corresponding exception
      CASE lo_udm_message->textid.
        WHEN zif_gtt_pof_ef_constants=>cs_errors-stop_processing.
          RAISE stop_processing.
        WHEN zif_gtt_pof_ef_constants=>cs_errors-table_determination.
          RAISE table_determination_error.
      ENDCASE.
  ENDTRY.
ENDFUNCTION.
