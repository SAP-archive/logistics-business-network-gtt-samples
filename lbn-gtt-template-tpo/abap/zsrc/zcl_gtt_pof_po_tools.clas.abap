CLASS zcl_gtt_pof_po_tools DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  CLASS-METHODS get_tracking_id_po_item
    IMPORTING
      !ir_ekpo TYPE REF TO data
    RETURNING
      VALUE(rv_track_id) TYPE /saptrx/trxid
    RAISING
      cx_udm_message .
  CLASS-METHODS is_appropriate_po_type
    IMPORTING
      !ir_ekko TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  CLASS-METHODS is_appropriate_po_item
    IMPORTING
      !ir_ekpo TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_po_tools IMPLEMENTATION.


  METHOD get_tracking_id_po_item.

    DATA: lv_ebeln TYPE ekpo-ebeln,
          lv_ebelp TYPE ekpo-ebelp.

    lv_ebeln  = zcl_gtt_pof_tools=>get_field_of_structure(
                  ir_struct_data = ir_ekpo
                  iv_field_name  = 'EBELN' ).

    lv_ebelp  = zcl_gtt_pof_tools=>get_field_of_structure(
                  ir_struct_data = ir_ekpo
                  iv_field_name  = 'EBELP' ).

    rv_track_id   = |{ lv_ebeln }{ lv_ebelp }|.

  ENDMETHOD.


  METHOD is_appropriate_po_item.

    DATA(lv_wepos)  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = ir_ekpo
                        iv_field_name  = 'WEPOS' ).

    rv_result = boolc( lv_wepos = abap_true ).

  ENDMETHOD.


  METHOD is_appropriate_po_type.

    DATA(lv_bsart)  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = ir_ekko
                        iv_field_name  = 'BSART' ).

    rv_result = boolc( lv_bsart = zif_gtt_pof_app_constants=>cs_relevance-bsart ).

  ENDMETHOD.
ENDCLASS.
