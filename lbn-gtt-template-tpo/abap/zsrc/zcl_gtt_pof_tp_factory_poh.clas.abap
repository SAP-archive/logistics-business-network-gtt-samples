CLASS zcl_gtt_pof_tp_factory_poh DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_pof_tp_factory
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS zif_gtt_pof_tp_factory~get_bo_reader
    REDEFINITION .
  METHODS zif_gtt_pof_tp_factory~get_pe_filler
    REDEFINITION .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_tp_factory_poh IMPLEMENTATION.


  METHOD zif_gtt_pof_tp_factory~get_bo_reader.

    ro_bo_reader    = NEW zcl_gtt_pof_tp_reader_poh(
                        io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_factory~get_pe_filler.

    MESSAGE e010(zgtt_pof) WITH zif_gtt_pof_app_constants=>cs_tabledef-po_header_new
      INTO DATA(lv_dummy).
    zcl_gtt_pof_tools=>throw_exception( ).

  ENDMETHOD.
ENDCLASS.
