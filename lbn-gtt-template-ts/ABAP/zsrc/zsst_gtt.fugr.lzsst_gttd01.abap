*&---------------------------------------------------------------------*
*& Local interfaces definition - Actual Events
*&---------------------------------------------------------------------*

"!Actual Event Types
INTERFACE lif_ae_types.
  TYPES: ts_trackingheader TYPE /saptrx/bapi_evm_header,
         tt_trackingheader TYPE STANDARD TABLE OF ts_trackingheader.

  TYPES: ts_tracklocation TYPE /saptrx/bapi_evm_locationid,
         tt_tracklocation TYPE STANDARD TABLE OF ts_tracklocation.

  TYPES: ts_trackreferences TYPE /saptrx/bapi_evm_reference,
         tt_trackreferences TYPE STANDARD TABLE OF ts_trackreferences.

  TYPES: ts_trackparameters TYPE /saptrx/bapi_evm_parameters,
         tt_trackparameters TYPE STANDARD TABLE OF ts_trackparameters.
ENDINTERFACE.

"!Actual Event Parameters
INTERFACE lif_ae_parameters.
  METHODS get_appsys
    RETURNING
      VALUE(rv_appsys) TYPE /saptrx/applsystem.

  METHODS get_event_type
    RETURNING
      VALUE(rs_event_type) TYPE /saptrx/evtypes.

  METHODS get_appl_table
    IMPORTING
      iv_tabledef    TYPE clike
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message.

  METHODS get_events
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message.
ENDINTERFACE.

"!Actual Event Filler
INTERFACE lif_ae_filler.
  METHODS check_relevance
    IMPORTING
      is_events        TYPE trxas_evt_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE lif_ef_types=>tv_condition
    RAISING
      cx_udm_message.

  METHODS get_event_data
    IMPORTING
      is_events          TYPE trxas_evt_ctab_wa
    CHANGING
      ct_eventid_map     TYPE trxas_evtid_evtcnt_map
      ct_trackingheader  TYPE lif_ae_types=>tt_trackingheader
      ct_tracklocation   TYPE lif_ae_types=>tt_tracklocation
      ct_trackreferences TYPE lif_ae_types=>tt_trackreferences
      ct_trackparameters TYPE lif_ae_types=>tt_trackparameters
    RAISING
      cx_udm_message.
ENDINTERFACE.

"!Actual Event Processor
INTERFACE lif_ae_processor.
  METHODS check_events
    RAISING
      cx_udm_message.

  METHODS check_relevance
    RETURNING
      VALUE(rv_result) TYPE lif_ef_types=>tv_condition
    RAISING
      cx_udm_message.

  METHODS get_event_data
    CHANGING
      ct_eventid_map     TYPE trxas_evtid_evtcnt_map
      ct_trackingheader  TYPE lif_ae_types=>tt_trackingheader
      ct_tracklocation   TYPE lif_ae_types=>tt_tracklocation
      ct_trackreferences TYPE lif_ae_types=>tt_trackreferences
      ct_trackparameters TYPE lif_ae_types=>tt_trackparameters
    RAISING
      cx_udm_message.
ENDINTERFACE.

"!Actual Event Factory
INTERFACE lif_ae_factory.
  METHODS get_ae_filler
    IMPORTING
      io_ae_parameters    TYPE REF TO lif_ae_parameters
    RETURNING
      VALUE(ro_ae_filler) TYPE REF TO lif_ae_filler.

  METHODS get_ae_parameters
    IMPORTING
      iv_appsys               TYPE /saptrx/applsystem
      is_event_type           TYPE /saptrx/evtypes
      it_all_appl_tables      TYPE trxas_tabcontainer
      it_event_type_cntl_tabs TYPE trxas_eventtype_tabs OPTIONAL
      it_events               TYPE trxas_evt_ctabs
    RETURNING
      VALUE(ro_ae_parameters) TYPE REF TO lif_ae_parameters
    RAISING
      cx_udm_message.

  " all importing parameters
  METHODS get_ae_processor
    IMPORTING
      is_definition           TYPE lif_ef_types=>ts_definition
      io_ae_factory           TYPE REF TO lif_ae_factory
      iv_appsys               TYPE /saptrx/applsystem
      is_event_type           TYPE /saptrx/evtypes
      it_all_appl_tables      TYPE trxas_tabcontainer
      it_event_type_cntl_tabs TYPE trxas_eventtype_tabs OPTIONAL
      it_events               TYPE trxas_evt_ctabs
    RETURNING
      VALUE(ro_ae_processor)  TYPE REF TO lif_ae_processor
    RAISING
      cx_udm_message.
ENDINTERFACE.
