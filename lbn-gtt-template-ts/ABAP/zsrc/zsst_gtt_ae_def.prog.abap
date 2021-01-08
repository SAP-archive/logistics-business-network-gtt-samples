*&---------------------------------------------------------------------*
*& Include          ZSST_GTT_AE
*&---------------------------------------------------------------------*
INTERFACE lif_actual_event.

  TYPES: tt_tracklocation   TYPE STANDARD TABLE OF /saptrx/bapi_evm_locationid.
  TYPES: tt_trackingheader  TYPE STANDARD TABLE OF /saptrx/bapi_evm_header.
  TYPES: tt_trackparameters TYPE STANDARD TABLE OF /saptrx/bapi_evm_parameters.
  TYPES: tt_execution_info  TYPE STANDARD TABLE OF /scmtms/s_em_bo_tor_execinfo WITH DEFAULT KEY.

  CONSTANTS:
    BEGIN OF cs_location_type,
      logistic TYPE string VALUE 'LogisticLocation',
    END OF cs_location_type,

    BEGIN OF cs_trxcode,
      shipment_order TYPE /saptrx/trxcod VALUE 'SHIPMENT_ORDER',
    END OF cs_trxcode,

    BEGIN OF cs_tabledef,
      tor_stop                  TYPE string VALUE 'TOR_STOP',
      tor_root                  TYPE string VALUE 'TOR_ROOT',
      tor_execution_info        TYPE string VALUE 'TOR_EXECUTION_INFO',
      tor_execution_info_before TYPE string VALUE 'TOR_EXECUTION_INFO_BEFORE',
    END OF cs_tabledef,

    BEGIN OF cs_event_id,
      BEGIN OF standard,
        departure    TYPE /saptrx/ev_evtid VALUE 'DEPARTURE',
        arrival      TYPE /saptrx/ev_evtid VALUE 'ARRIV_DEST',
        pod          TYPE /saptrx/ev_evtid VALUE 'POD',
        popu         TYPE /saptrx/ev_evtid VALUE 'POPU',
        load_begin   TYPE /saptrx/ev_evtid VALUE 'LOAD_BEGIN',
        load_end     TYPE /saptrx/ev_evtid VALUE 'LOAD_END',
        coupling     TYPE /saptrx/ev_evtid VALUE 'COUPLING',
        decoupling   TYPE /saptrx/ev_evtid VALUE 'DECOUPLING',
        unload_begin TYPE /saptrx/ev_evtid VALUE 'UNLOAD_BEGIN',
        unload_end   TYPE /saptrx/ev_evtid VALUE 'UNLOAD_END',
      END OF standard,
      BEGIN OF model,
        shp_departure TYPE /saptrx/ev_evtid VALUE 'DEPARTURE',
        shp_arrival   TYPE /saptrx/ev_evtid VALUE 'ARRIV_DEST',
        shp_pod       TYPE /saptrx/ev_evtid VALUE 'POD',
        popu          TYPE /saptrx/ev_evtid VALUE 'POPU',
        load_start    TYPE /saptrx/ev_evtid VALUE 'LOAD_BEGIN',
        load_end      TYPE /saptrx/ev_evtid VALUE 'LOAD_END',
        coupling      TYPE /saptrx/ev_evtid VALUE 'COUPLING',
        decoupling    TYPE /saptrx/ev_evtid VALUE 'DECOUPLING',
        unload_begin  TYPE /saptrx/ev_evtid VALUE 'UNLOAD_BEGIN',
        unload_end    TYPE /saptrx/ev_evtid VALUE 'UNLOAD_END',
      END OF model,
    END OF cs_event_id.

  METHODS adjust_ae_location_data
    IMPORTING
      i_all_appl_tables       TYPE trxas_tabcontainer
      iv_clear_standard_param TYPE abap_bool DEFAULT abap_true
    CHANGING
      ct_trackingheader       TYPE tt_trackingheader
      ct_tracklocation        TYPE tt_tracklocation
      ct_trackparameters      TYPE tt_trackparameters.

  METHODS check_event_relevance
    IMPORTING
      i_all_appl_tables TYPE trxas_tabcontainer
      iv_event_code     TYPE /scmtms/tor_event
      i_event           TYPE  trxas_evt_ctab_wa
    EXPORTING
      VALUE(e_result)   LIKE  sy-binpt.

  METHODS check_application_event_source
    IMPORTING
      i_all_appl_tables TYPE trxas_tabcontainer
      iv_event_code     TYPE /scmtms/tor_event
      i_event           TYPE  trxas_evt_ctab_wa
    EXPORTING
      VALUE(e_result)   LIKE  sy-binpt.

  METHODS check_tor_type_specific_events
    IMPORTING
      iv_event_code   TYPE /scmtms/tor_event
    RETURNING
      VALUE(e_result) LIKE  sy-binpt.

ENDINTERFACE.

CLASS lcl_actual_event DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_actual_event.

    CLASS-METHODS get_tor_actual_event_class
      IMPORTING
        i_event                TYPE trxas_evt_ctab_wa
      RETURNING
        VALUE(ro_actual_event) TYPE REF TO lif_actual_event
      RAISING
        cx_udm_message.

  PRIVATE SECTION.
    METHODS get_stop
      IMPORTING
        i_all_appl_tables TYPE trxas_tabcontainer
      RETURNING
        VALUE(rt_stop)    TYPE /scmtms/t_em_bo_tor_stop.

    METHODS get_root
      IMPORTING
        i_all_appl_tables TYPE trxas_tabcontainer
      RETURNING
        VALUE(rt_root)    TYPE /scmtms/t_em_bo_tor_root.

    METHODS get_locid2
      IMPORTING
        i_tor_id         TYPE /scmtms/tor_id
        i_seq_num        TYPE /scmtms/seq_num
      RETURNING
        VALUE(rv_locid2) TYPE /saptrx/ev_locid2.

    METHODS get_execution
      IMPORTING
        i_all_appl_tables   TYPE trxas_tabcontainer
        iv_old              TYPE abap_bool DEFAULT abap_false
      EXPORTING
        VALUE(et_execution) TYPE /scmtms/t_em_bo_tor_execinfo.

    METHODS get_model_event_id
      IMPORTING
        iv_standard_event_id     TYPE /saptrx/ev_evtid
      RETURNING
        VALUE(rv_model_event_id) TYPE /saptrx/ev_evtid.

ENDCLASS.

CLASS lcl_fo_actual_event DEFINITION INHERITING FROM lcl_actual_event.
  PUBLIC SECTION.
    METHODS lif_actual_event~check_tor_type_specific_events REDEFINITION.
ENDCLASS.

CLASS lcl_fb_actual_event DEFINITION INHERITING FROM lcl_actual_event.
  PUBLIC SECTION.
    METHODS lif_actual_event~check_tor_type_specific_events REDEFINITION.
ENDCLASS.
