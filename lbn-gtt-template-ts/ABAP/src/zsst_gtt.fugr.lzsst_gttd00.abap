*&---------------------------------------------------------------------*
*& Local interfaces definition - General + Transaction Processing
*&---------------------------------------------------------------------*
INTERFACE lif_factory DEFERRED.
INTERFACE lif_ef_types.
  TYPES: ts_control_data TYPE /saptrx/control_data,
         tt_control_data TYPE STANDARD TABLE OF ts_control_data.

  TYPES: BEGIN OF ts_enh_track_id_data,
           key TYPE char33.
           INCLUDE TYPE /saptrx/track_id_data.
  TYPES: END OF ts_enh_track_id_data.

  TYPES: tt_enh_track_id_data TYPE STANDARD TABLE OF ts_enh_track_id_data.
  TYPES: ts_track_id_data TYPE /saptrx/track_id_data,
         tt_track_id_data TYPE STANDARD TABLE OF ts_track_id_data.

  TYPES: ts_strucdatadef TYPE /saptrx/strucdatadef,
         tt_strucdatadef TYPE STANDARD TABLE OF ts_strucdatadef.

  TYPES: ts_expeventdata TYPE /saptrx/exp_events,
         tt_expeventdata TYPE STANDARD TABLE OF ts_expeventdata.

  TYPES: ts_measrmntdata TYPE /saptrx/measr_data,
         tt_measrmntdata TYPE STANDARD TABLE OF ts_measrmntdata.

  TYPES: ts_infodata TYPE /saptrx/info_data,
         tt_infodata TYPE STANDARD TABLE OF ts_infodata.

  TYPES: BEGIN OF ts_definition,
           maintab   TYPE /saptrx/strucdatadef,
           mastertab TYPE /saptrx/strucdatadef,
         END OF ts_definition.

  TYPES: tt_tracklocation  TYPE STANDARD TABLE OF /saptrx/bapi_evm_locationid.

  TYPES: tt_trackingheader TYPE STANDARD TABLE OF /saptrx/bapi_evm_header.

  TYPES: tv_condition       TYPE sy-binpt.

  TYPES: BEGIN OF ts_stop_points,
           stop_id       TYPE string,
           log_locid     TYPE /scmtms/location_id,
           seq_num       TYPE /scmtms/seq_num,
         END OF ts_stop_points.
  TYPES: tt_stop_points TYPE STANDARD TABLE OF ts_stop_points .
ENDINTERFACE.

"!Extractor Function Constants
INTERFACE lif_ef_constants.
  CONSTANTS: BEGIN OF cs_system_fields,
               actual_bisiness_timezone TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_TIMEZONE',
               actual_bisiness_datetime TYPE /saptrx/paramname VALUE 'ACTUAL_BUSINESS_DATETIME',
             END OF cs_system_fields.

  CONSTANTS: BEGIN OF cs_errors,
               wrong_parameter     TYPE sotr_conc VALUE '1216f03004ce11ebbf450050c2490048',
               cdata_determination TYPE sotr_conc VALUE '1216f03004ce11ebbf460050c2490048',
               table_determination TYPE sotr_conc VALUE '1216f03004ce11ebbf470050c2490048',
               stop_processing     TYPE sotr_conc VALUE '1216f03004ce11ebbf480050c2490048',
             END OF cs_errors.

  CONSTANTS: BEGIN OF cs_condition,
               true  TYPE lif_ef_types=>tv_condition VALUE 'T',
               false TYPE lif_ef_types=>tv_condition VALUE 'F',
             END OF cs_condition.

  CONSTANTS: BEGIN OF cs_change_mode,
               insert    TYPE updkz_d VALUE 'I',
               update    TYPE updkz_d VALUE 'U',
               delete    TYPE updkz_d VALUE 'D',
               undefined TYPE updkz_d VALUE 'T',
             END OF cs_change_mode.

  CONSTANTS: cv_aot   TYPE string VALUE 'AOT'.

  CONSTANTS: cv_max_end_date  TYPE /saptrx/tid_end_date_tsloc VALUE '099991231000000'.

ENDINTERFACE.

INTERFACE lif_ef_parameters.
  METHODS get_appsys
    RETURNING
      VALUE(rv_appsys) TYPE /saptrx/applsystem.

  METHODS get_app_obj_types
    RETURNING
      VALUE(rs_app_obj_types) TYPE /saptrx/aotypes.

  METHODS get_app_objects
    RETURNING
      VALUE(rr_data) TYPE REF TO data.

  METHODS get_appl_table
    IMPORTING
      iv_tabledef    TYPE clike
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message.
ENDINTERFACE.

INTERFACE lif_bo_reader.
  METHODS check_relevance
    IMPORTING
      is_app_object    TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE lif_ef_types=>tv_condition
    RAISING
      cx_udm_message.

  METHODS get_data
    IMPORTING
      is_app_object  TYPE trxas_appobj_ctab_wa
      iv_old_data    TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message.

  METHODS get_mapping_structure
    RETURNING
      VALUE(rr_data) TYPE REF TO data.

  METHODS get_track_id_data
    IMPORTING
      is_app_object    TYPE trxas_appobj_ctab_wa
    EXPORTING
      et_track_id_data TYPE lif_ef_types=>tt_track_id_data
    RAISING
      cx_udm_message.

ENDINTERFACE.

INTERFACE lif_ef_processor.
  METHODS check_app_objects
    RAISING
      cx_udm_message.

  METHODS check_relevance
    IMPORTING
      io_bo_factory    TYPE REF TO lif_factory
    RETURNING
      VALUE(rv_result) TYPE lif_ef_types=>tv_condition
    RAISING
      cx_udm_message.

  METHODS get_control_data
    IMPORTING
      io_bo_factory   TYPE REF TO lif_factory
    CHANGING
      ct_control_data TYPE lif_ef_types=>tt_control_data
    RAISING
      cx_udm_message.

  METHODS get_planned_events
    CHANGING
      ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      ct_measrmntdata TYPE lif_ef_types=>tt_measrmntdata
      ct_infodata     TYPE lif_ef_types=>tt_infodata
    RAISING
      cx_udm_message.

  METHODS get_track_id_data
    IMPORTING
      io_bo_factory    TYPE REF TO lif_factory
    EXPORTING
      et_track_id_data TYPE lif_ef_types=>tt_track_id_data
    RAISING
      cx_udm_message.

ENDINTERFACE.

"!Planned Event Filler
INTERFACE lif_pe_filler.
  METHODS check_relevance
    IMPORTING
      is_app_objects   TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE lif_ef_types=>tv_condition
    RAISING
      cx_udm_message.

  METHODS get_planed_events
    IMPORTING
      is_app_objects  TYPE trxas_appobj_ctab_wa
    CHANGING
      ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      ct_measrmntdata TYPE lif_ef_types=>tt_measrmntdata
      ct_infodata     TYPE lif_ef_types=>tt_infodata
    RAISING
      cx_udm_message.
ENDINTERFACE.

INTERFACE lif_factory.
  METHODS get_bo_reader
    IMPORTING
      is_appl_object      TYPE trxas_appobj_ctab_wa
      io_ef_parameters    TYPE REF TO lif_ef_parameters
    RETURNING
      VALUE(ro_bo_reader) TYPE REF TO lif_bo_reader
    RAISING
      cx_udm_message.

  METHODS get_ef_parameters
    IMPORTING
      iv_appsys               TYPE /saptrx/applsystem
      is_app_obj_types        TYPE /saptrx/aotypes
      it_all_appl_tables      TYPE trxas_tabcontainer
      it_app_type_cntl_tabs   TYPE trxas_apptype_tabs OPTIONAL
      it_app_objects          TYPE trxas_appobj_ctabs
    RETURNING
      VALUE(ro_ef_parameters) TYPE REF TO lif_ef_parameters.

  METHODS get_ef_processor
    IMPORTING
      is_definition          TYPE lif_ef_types=>ts_definition
      io_bo_factory          TYPE REF TO lif_factory
      iv_appsys              TYPE /saptrx/applsystem
      is_app_obj_types       TYPE /saptrx/aotypes
      it_all_appl_tables     TYPE trxas_tabcontainer
      it_app_type_cntl_tabs  TYPE trxas_apptype_tabs
      it_app_objects         TYPE trxas_appobj_ctabs
    RETURNING
      VALUE(ro_ef_processor) TYPE REF TO lif_ef_processor.

  METHODS get_pe_filler
    IMPORTING
      io_ef_parameters    TYPE REF TO lif_ef_parameters
      io_bo_reader        TYPE REF TO lif_bo_reader
    RETURNING
      VALUE(ro_pe_filler) TYPE REF TO lif_pe_filler.
ENDINTERFACE.
