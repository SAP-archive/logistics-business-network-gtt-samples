CLASS zcl_gtt_pof_ae_factory DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_ae_factory .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_ae_factory IMPLEMENTATION.


  METHOD zif_gtt_pof_ae_factory~get_ae_filler.
  ENDMETHOD.


  METHOD zif_gtt_pof_ae_factory~get_ae_parameters.

    ro_ae_parameters  = NEW zcl_gtt_pof_ae_parameters(
      iv_appsys               = iv_appsys
      is_event_type           = is_event_type
      it_all_appl_tables      = it_all_appl_tables
      it_event_type_cntl_tabs = it_event_type_cntl_tabs
      it_events               = it_events ).

  ENDMETHOD.


  METHOD zif_gtt_pof_ae_factory~get_ae_processor.

    DATA: lo_ae_parameters TYPE REF TO zif_gtt_pof_ae_parameters,
          lo_ae_filler     TYPE REF TO zif_gtt_pof_ae_filler.

    lo_ae_parameters  = zif_gtt_pof_ae_factory~get_ae_parameters(
                          iv_appsys               = iv_appsys
                          is_event_type           = is_event_type
                          it_all_appl_tables      = it_all_appl_tables
                          it_event_type_cntl_tabs = it_event_type_cntl_tabs
                          it_events               = it_events ).

    lo_ae_filler      = zif_gtt_pof_ae_factory~get_ae_filler(
                          io_ae_parameters = lo_ae_parameters ).

    ro_ae_processor   = NEW zcl_gtt_pof_ae_processor(
                          is_definition    = is_definition
                          io_ae_parameters = lo_ae_parameters
                          io_ae_filler     = lo_ae_filler ).


  ENDMETHOD.
ENDCLASS.
