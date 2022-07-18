CLASS zcl_gtt_pof_tp_factory_dli DEFINITION
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



CLASS zcl_gtt_pof_tp_factory_dli IMPLEMENTATION.


  METHOD zif_gtt_pof_tp_factory~get_bo_reader.

    ro_bo_reader    = NEW zcl_gtt_pof_tp_reader_dli(
                        io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_factory~get_pe_filler.

    ro_pe_filler    = NEW zcl_gtt_pof_pe_filler_dli(
      io_ef_parameters = io_ef_parameters
      io_bo_reader     = io_bo_reader ).

  ENDMETHOD.
ENDCLASS.
