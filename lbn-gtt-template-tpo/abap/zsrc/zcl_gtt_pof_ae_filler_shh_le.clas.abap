CLASS zcl_gtt_pof_ae_filler_shh_le DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_pof_ae_filler_shh_bh
  CREATE PUBLIC .

PUBLIC SECTION.
PROTECTED SECTION.

  METHODS get_eventid
    REDEFINITION .
  METHODS get_date_field
    REDEFINITION .
  METHODS get_time_field
    REDEFINITION .
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_ae_filler_shh_le IMPLEMENTATION.


  METHOD get_date_field.

    rv_field  = 'DALEN'.

  ENDMETHOD.


  METHOD get_eventid.

    rv_eventid  = zif_gtt_pof_app_constants=>cs_milestone-sh_load_end.

  ENDMETHOD.


  METHOD get_time_field.

    rv_field  = 'UALEN'.

  ENDMETHOD.
ENDCLASS.
