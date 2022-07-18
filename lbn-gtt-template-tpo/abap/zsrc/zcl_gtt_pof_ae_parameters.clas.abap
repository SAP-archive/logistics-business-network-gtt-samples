CLASS zcl_gtt_pof_ae_parameters DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_ae_parameters .

  METHODS constructor
    IMPORTING
      !iv_appsys TYPE /saptrx/applsystem
      !is_event_type TYPE /saptrx/evtypes
      !it_all_appl_tables TYPE trxas_tabcontainer
      !it_event_type_cntl_tabs TYPE trxas_eventtype_tabs
      !it_events TYPE trxas_evt_ctabs .
PROTECTED SECTION.
PRIVATE SECTION.

  DATA mv_appsys TYPE /saptrx/applsystem .
  DATA ms_event_type TYPE /saptrx/evtypes .
  DATA mr_all_appl_tables TYPE REF TO data .
  DATA mr_event_type_cntl_tabs TYPE REF TO data .
  DATA mr_events TYPE REF TO data .
ENDCLASS.



CLASS zcl_gtt_pof_ae_parameters IMPLEMENTATION.


  METHOD constructor.

    mv_appsys               = iv_appsys.
    ms_event_type           = is_event_type.
    mr_all_appl_tables      = REF #( it_all_appl_tables ).
    mr_event_type_cntl_tabs = REF #( it_event_type_cntl_tabs ).
    mr_events               = REF #( it_events ).

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_parameters~get_appl_table.

    TRY.
        FIELD-SYMBOLS: <lt_all_appl_tables> TYPE trxas_tabcontainer.

        ASSIGN mr_all_appl_tables->* TO <lt_all_appl_tables>.

        rr_data   = <lt_all_appl_tables>[ tabledef = iv_tabledef ]-tableref.

      CATCH cx_sy_itab_line_not_found.
        MESSAGE e008(/saptrx/asc)
          WITH iv_tabledef
               ms_event_type-eventdatafunc
          INTO DATA(lv_dummy).

        zcl_gtt_pof_tools=>throw_exception(
          iv_textid = zif_gtt_pof_ef_constants=>cs_errors-stop_processing ).
    ENDTRY.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_parameters~get_appsys.

    rv_appsys   = mv_appsys.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_parameters~get_events.

    rr_data     = mr_events.

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_parameters~get_event_type.

    rs_event_type   = ms_event_type.

  ENDMETHOD.
ENDCLASS.
