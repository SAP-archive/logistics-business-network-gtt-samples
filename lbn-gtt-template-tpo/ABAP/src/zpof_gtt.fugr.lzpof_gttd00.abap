*&---------------------------------------------------------------------*
*& Local interfaces definition - General + Transaction Processing
*&---------------------------------------------------------------------*
INTERFACE lif_ef_types.
  TYPES: ts_control_data TYPE /saptrx/control_data,
         tt_control_data TYPE STANDARD TABLE OF ts_control_data.

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

  TYPES: tv_parameter_id     TYPE i,
         tv_parameter_value  TYPE char50.

  TYPES: tv_condition       TYPE sy-binpt.

  TYPES: tv_field_name      TYPE char20,
         tt_field_name      TYPE STANDARD TABLE OF tv_field_name
                              WITH EMPTY KEY.

  TYPES: tv_currency_amnt   TYPE bapicurr_d.

ENDINTERFACE.

"!Extractor Function Constants
INTERFACE lif_ef_constants.
  CONSTANTS: BEGIN OF cs_trk_obj_type,
               esc_purord TYPE /saptrx/trk_obj_type VALUE 'ESC_PURORD',
               esc_deliv  TYPE /saptrx/trk_obj_type VALUE 'ESC_DELIV',
               esc_shipmt TYPE /saptrx/trk_obj_type VALUE 'ESC_SHIPMT',
             END OF cs_trk_obj_type.

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

  CONSTANTS: BEGIN OF cs_parameter_id,
               key_field    TYPE lif_ef_types=>tv_parameter_id VALUE 1,
               no_empty_tag TYPE lif_ef_types=>tv_parameter_id VALUE 2,
             END OF cs_parameter_id.

  CONSTANTS: cv_aot   TYPE string VALUE 'AOT'.

  CONSTANTS: cv_max_end_date  TYPE /saptrx/tid_end_date_tsloc VALUE '099991231000000'.

  CONSTANTS: cv_structure_pkg TYPE devclass VALUE '/SAPTRX/SCEM_AI_R3'.

  CONSTANTS: BEGIN OF cs_loc_types,
               BusinessPartner  TYPE /saptrx/loc_id_type VALUE 'BusinessPartner' ##NO_TEXT,
               Customer         TYPE /saptrx/loc_id_type VALUE 'Customer' ##NO_TEXT,
               LogisticLocation TYPE /saptrx/loc_id_type VALUE 'LogisticLocation' ##NO_TEXT,
               Plant            TYPE /saptrx/loc_id_type VALUE 'Plant' ##NO_TEXT,
               ShippingPoint    TYPE /saptrx/loc_id_type VALUE 'ShippingPoint' ##NO_TEXT,
               Supplier         TYPE /saptrx/loc_id_type VALUE 'Supplier' ##NO_TEXT,
             END OF cs_loc_types.

  CONSTANTS: BEGIN OF cs_date_types,
              timestamp TYPE char20 VALUE 'TIMESTAMP',
             END OF cs_date_types.

ENDINTERFACE.

"!Extractor Function parameters wrapper
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

"!Business Objects reader
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
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message.

  METHODS get_data_old
    IMPORTING
      is_app_object  TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rr_data) TYPE REF TO data
    RAISING
      cx_udm_message.

  METHODS get_field_parameter
    IMPORTING
      iv_field_name    TYPE clike
      iv_parameter     TYPE lif_ef_types=>tv_parameter_id
    RETURNING
      VALUE(rv_result) TYPE lif_ef_types=>tv_parameter_value
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

"!Extractor Function processor
INTERFACE lif_ef_processor.
  METHODS check_app_objects
    RAISING
      cx_udm_message.

  METHODS check_relevance
    RETURNING
      VALUE(rv_result) TYPE lif_ef_types=>tv_condition
    RAISING
      cx_udm_message.

  METHODS get_control_data
    CHANGING
      ct_control_data TYPE lif_ef_types=>tt_control_data
    RAISING
      cx_udm_message.
  METHODS get_track_id_data
    EXPORTING
      et_track_id_data TYPE lif_ef_types=>tt_track_id_data
    RAISING
      cx_udm_message.

  METHODS get_planned_events
    CHANGING
      ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      ct_measrmntdata TYPE lif_ef_types=>tt_measrmntdata
      ct_infodata     TYPE lif_ef_types=>tt_infodata
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

"!Factory
INTERFACE lif_factory.
  METHODS get_bo_reader
    IMPORTING
      io_ef_parameters    TYPE REF TO lif_ef_parameters
    RETURNING
      VALUE(ro_bo_reader) TYPE REF TO lif_bo_reader.

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
      VALUE(ro_ef_processor) TYPE REF TO lif_ef_processor
    RAISING
      cx_udm_message.

  METHODS get_pe_filler
    IMPORTING
      io_ef_parameters    TYPE REF TO lif_ef_parameters
      io_bo_reader        TYPE REF TO lif_bo_reader
    RETURNING
      VALUE(ro_pe_filler) TYPE REF TO lif_pe_filler
    RAISING
      cx_udm_message.
ENDINTERFACE.
