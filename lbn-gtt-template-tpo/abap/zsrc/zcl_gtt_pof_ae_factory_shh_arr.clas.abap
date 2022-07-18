CLASS zcl_gtt_pof_ae_factory_shh_arr DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_pof_ae_factory
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS zif_gtt_pof_ae_factory~get_ae_filler
    REDEFINITION .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_ae_factory_shh_arr IMPLEMENTATION.


  METHOD zif_gtt_pof_ae_factory~get_ae_filler.

    ro_ae_filler    = NEW zcl_gtt_pof_ae_filler_shh_arr(
                        io_ae_parameters = io_ae_parameters ).

  ENDMETHOD.
ENDCLASS.
