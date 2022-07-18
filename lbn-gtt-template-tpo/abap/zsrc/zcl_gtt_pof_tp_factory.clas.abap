CLASS zcl_gtt_pof_tp_factory DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_tp_factory .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_tp_factory IMPLEMENTATION.


  METHOD zif_gtt_pof_tp_factory~get_bo_reader.
  ENDMETHOD.


  METHOD zif_gtt_pof_tp_factory~get_ef_parameters.

    ro_ef_parameters  = NEW zcl_gtt_pof_ef_parameters(
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_factory~get_ef_processor.

    DATA:
      lo_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters,
      lo_bo_reader     TYPE REF TO zif_gtt_pof_tp_reader,
      lo_pe_filler     TYPE REF TO zif_gtt_pof_pe_filler.

    lo_ef_parameters  = zif_gtt_pof_tp_factory~get_ef_parameters(
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects ).

    lo_bo_reader      = zif_gtt_pof_tp_factory~get_bo_reader(
      io_ef_parameters      = lo_ef_parameters ).

    " filler is not obligatory
    TRY.
        lo_pe_filler      = zif_gtt_pof_tp_factory~get_pe_filler(
          io_ef_parameters      = lo_ef_parameters
          io_bo_reader          = lo_bo_reader ).
      CATCH cx_udm_message.
    ENDTRY.

    ro_ef_processor   = NEW zcl_gtt_pof_ef_processor(
      io_ef_parameters      = lo_ef_parameters
      io_bo_reader          = lo_bo_reader
      io_pe_filler          = lo_pe_filler
      is_definition         = is_definition
    ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_factory~get_pe_filler.
  ENDMETHOD.
ENDCLASS.
