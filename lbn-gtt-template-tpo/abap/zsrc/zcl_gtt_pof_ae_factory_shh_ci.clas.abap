CLASS zcl_gtt_pof_ae_factory_shh_ci DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_pof_ae_factory
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS zif_gtt_pof_ae_factory~get_ae_filler
    REDEFINITION .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_ae_factory_shh_ci IMPLEMENTATION.


  METHOD zif_gtt_pof_ae_factory~get_ae_filler.

    ro_ae_filler    = NEW zcl_gtt_pof_ae_filler_shh_ci(
                        io_ae_parameters = io_ae_parameters ).

  ENDMETHOD.
ENDCLASS.
