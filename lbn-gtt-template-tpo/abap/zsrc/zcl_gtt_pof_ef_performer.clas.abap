CLASS zcl_gtt_pof_ef_performer DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  CLASS-METHODS check_relevance
    IMPORTING
      !is_definition TYPE zif_gtt_pof_ef_types=>ts_definition
      !io_bo_factory TYPE REF TO zif_gtt_pof_tp_factory
      !iv_appsys TYPE /saptrx/applsystem
      !is_app_obj_types TYPE /saptrx/aotypes
      !it_all_appl_tables TYPE trxas_tabcontainer
      !it_app_type_cntl_tabs TYPE trxas_apptype_tabs OPTIONAL
      !it_app_objects TYPE trxas_appobj_ctabs
    RETURNING
      VALUE(rv_result) TYPE sy-binpt
    RAISING
      cx_udm_message .
  CLASS-METHODS get_control_data
    IMPORTING
      !is_definition TYPE zif_gtt_pof_ef_types=>ts_definition
      !io_bo_factory TYPE REF TO zif_gtt_pof_tp_factory
      !iv_appsys TYPE /saptrx/applsystem
      !is_app_obj_types TYPE /saptrx/aotypes
      !it_all_appl_tables TYPE trxas_tabcontainer
      !it_app_type_cntl_tabs TYPE trxas_apptype_tabs
      !it_app_objects TYPE trxas_appobj_ctabs
    CHANGING
      !ct_control_data TYPE zif_gtt_pof_ef_types=>tt_control_data
    RAISING
      cx_udm_message .
  CLASS-METHODS get_planned_events
    IMPORTING
      !is_definition TYPE zif_gtt_pof_ef_types=>ts_definition
      !io_factory TYPE REF TO zif_gtt_pof_tp_factory
      !iv_appsys TYPE /saptrx/applsystem
      !is_app_obj_types TYPE /saptrx/aotypes
      !it_all_appl_tables TYPE trxas_tabcontainer
      !it_app_type_cntl_tabs TYPE trxas_apptype_tabs
      !it_app_objects TYPE trxas_appobj_ctabs
    CHANGING
      !ct_expeventdata TYPE zif_gtt_pof_ef_types=>tt_expeventdata
      !ct_measrmntdata TYPE zif_gtt_pof_ef_types=>tt_measrmntdata
      !ct_infodata TYPE zif_gtt_pof_ef_types=>tt_infodata
    RAISING
      cx_udm_message .
  CLASS-METHODS get_track_id_data
    IMPORTING
      !is_definition TYPE zif_gtt_pof_ef_types=>ts_definition
      !io_bo_factory TYPE REF TO zif_gtt_pof_tp_factory
      !iv_appsys TYPE /saptrx/applsystem
      !is_app_obj_types TYPE /saptrx/aotypes
      !it_all_appl_tables TYPE trxas_tabcontainer
      !it_app_type_cntl_tabs TYPE trxas_apptype_tabs
      !it_app_objects TYPE trxas_appobj_ctabs
    EXPORTING
      !et_track_id_data TYPE zif_gtt_pof_ef_types=>tt_track_id_data
    RAISING
      cx_udm_message .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_ef_performer IMPLEMENTATION.


  METHOD check_relevance.

    DATA: lo_ef_processor   TYPE REF TO zif_gtt_pof_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor   = io_bo_factory->get_ef_processor(
                          is_definition         = is_definition
                          io_bo_factory         = io_bo_factory
                          iv_appsys             = iv_appsys
                          is_app_obj_types      = is_app_obj_types
                          it_all_appl_tables    = it_all_appl_tables
                          it_app_type_cntl_tabs = it_app_type_cntl_tabs
                          it_app_objects        = it_app_objects ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " check relevance
    rv_result = lo_ef_processor->check_relevance( ).

  ENDMETHOD.


  METHOD get_control_data.

    DATA: lo_ef_processor   TYPE REF TO zif_gtt_pof_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor   = io_bo_factory->get_ef_processor(
                          is_definition         = is_definition
                          io_bo_factory         = io_bo_factory
                          iv_appsys             = iv_appsys
                          is_app_obj_types      = is_app_obj_types
                          it_all_appl_tables    = it_all_appl_tables
                          it_app_type_cntl_tabs = it_app_type_cntl_tabs
                          it_app_objects        = it_app_objects
                        ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill control data from business object data
    lo_ef_processor->get_control_data(
      CHANGING
        ct_control_data = ct_control_data[]
    ).


  ENDMETHOD.


  METHOD get_planned_events.

    DATA: lo_ef_processor   TYPE REF TO zif_gtt_pof_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor   = io_factory->get_ef_processor(
                          is_definition         = is_definition
                          io_bo_factory         = io_factory
                          iv_appsys             = iv_appsys
                          is_app_obj_types      = is_app_obj_types
                          it_all_appl_tables    = it_all_appl_tables
                          it_app_type_cntl_tabs = it_app_type_cntl_tabs
                          it_app_objects        = it_app_objects
                        ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill planned events data
    lo_ef_processor->get_planned_events(
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata
    ).


  ENDMETHOD.


  METHOD get_track_id_data.

    DATA: lo_ef_processor   TYPE REF TO zif_gtt_pof_ef_processor.

    CLEAR: et_track_id_data[].

    " get instance of extractor function processor
    lo_ef_processor   = io_bo_factory->get_ef_processor(
                          is_definition         = is_definition
                          io_bo_factory         = io_bo_factory
                          iv_appsys             = iv_appsys
                          is_app_obj_types      = is_app_obj_types
                          it_all_appl_tables    = it_all_appl_tables
                          it_app_type_cntl_tabs = it_app_type_cntl_tabs
                          it_app_objects        = it_app_objects
                        ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill controll data from PO Header data
    lo_ef_processor->get_track_id_data(
      IMPORTING
        et_track_id_data    = et_track_id_data
    ).


  ENDMETHOD.
ENDCLASS.
