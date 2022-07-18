CLASS zcl_gtt_pof_ae_filler_shh_dep DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_pof_ae_filler_shh_bs
  CREATE PUBLIC .

PUBLIC SECTION.
PROTECTED SECTION.

  METHODS get_date_field
    REDEFINITION .
  METHODS get_eventid
    REDEFINITION .
  METHODS get_location_category
    REDEFINITION .
  METHODS get_time_field
    REDEFINITION .
  METHODS is_location_required
    REDEFINITION .
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_ae_filler_shh_dep IMPLEMENTATION.


  METHOD get_date_field.

    rv_field    = 'DATBG'.

  ENDMETHOD.


  METHOD get_eventid.

    rv_eventid  = zif_gtt_pof_app_constants=>cs_milestone-sh_departure.

  ENDMETHOD.


  METHOD get_location_category.

    rv_loccat   = zif_gtt_pof_app_constants=>cs_loccat-departure.

  ENDMETHOD.


  METHOD get_time_field.

    rv_field    = 'UATBG'.

  ENDMETHOD.


  METHOD is_location_required.

    rv_result   = abap_false.

  ENDMETHOD.
ENDCLASS.
