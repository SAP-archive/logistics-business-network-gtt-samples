CLASS zcl_gtt_pof_ae_performer DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  CLASS-METHODS check_relevance
    IMPORTING
      !is_definition TYPE zif_gtt_pof_ef_types=>ts_definition
      !io_ae_factory TYPE REF TO zif_gtt_pof_ae_factory
      !iv_appsys TYPE /saptrx/applsystem
      !is_event_type TYPE /saptrx/evtypes
      !it_all_appl_tables TYPE trxas_tabcontainer
      !it_event_type_cntl_tabs TYPE trxas_eventtype_tabs OPTIONAL
      !is_events TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_pof_ef_types=>tv_condition
    RAISING
      cx_udm_message .
  CLASS-METHODS get_event_data
    IMPORTING
      !is_definition TYPE zif_gtt_pof_ef_types=>ts_definition
      !io_ae_factory TYPE REF TO zif_gtt_pof_ae_factory
      !iv_appsys TYPE /saptrx/applsystem
      !is_event_type TYPE /saptrx/evtypes
      !it_all_appl_tables TYPE trxas_tabcontainer
      !it_event_type_cntl_tabs TYPE trxas_eventtype_tabs
      !it_events TYPE trxas_evt_ctabs
    CHANGING
      !ct_eventid_map TYPE trxas_evtid_evtcnt_map
      !ct_trackingheader TYPE zif_gtt_pof_ae_types=>tt_trackingheader
      !ct_tracklocation TYPE zif_gtt_pof_ae_types=>tt_tracklocation
      !ct_trackreferences TYPE zif_gtt_pof_ae_types=>tt_trackreferences
      !ct_trackparameters TYPE zif_gtt_pof_ae_types=>tt_trackparameters
    RAISING
      cx_udm_message .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_ae_performer IMPLEMENTATION.


  METHOD check_relevance.

    DATA: lt_events TYPE trxas_evt_ctabs.

    lt_events = VALUE #( ( is_events ) ).

    DATA(lo_ae_processor) = io_ae_factory->get_ae_processor(
                              is_definition           = is_definition
                              io_ae_factory           = io_ae_factory
                              iv_appsys               = iv_appsys
                              is_event_type           = is_event_type
                              it_all_appl_tables      = it_all_appl_tables
                              it_event_type_cntl_tabs = it_event_type_cntl_tabs
                              it_events               = lt_events ).

    lo_ae_processor->check_events( ).

    rv_result = lo_ae_processor->check_relevance( ).

  ENDMETHOD.


  METHOD get_event_data.

    DATA(lo_ae_processor) = io_ae_factory->get_ae_processor(
                              is_definition           = is_definition
                              io_ae_factory           = io_ae_factory
                              iv_appsys               = iv_appsys
                              is_event_type           = is_event_type
                              it_all_appl_tables      = it_all_appl_tables
                              it_event_type_cntl_tabs = it_event_type_cntl_tabs
                              it_events               = it_events ).

    lo_ae_processor->check_events( ).

    lo_ae_processor->get_event_data(
      CHANGING
        ct_eventid_map     = ct_eventid_map
        ct_trackingheader  = ct_trackingheader
        ct_tracklocation   = ct_tracklocation
        ct_trackreferences = ct_trackreferences
        ct_trackparameters = ct_trackparameters ).

  ENDMETHOD.
ENDCLASS.
