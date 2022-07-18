CLASS zcl_gtt_pof_event_rel_dl_it DEFINITION
  PUBLIC
  INHERITING FROM zcl_gtt_pof_event_rel_dl_main
  CREATE PUBLIC .

PUBLIC SECTION.
PROTECTED SECTION.

  METHODS get_field_name
    REDEFINITION .
  METHODS get_object_status
    REDEFINITION .
PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gtt_pof_event_rel_dl_it IMPLEMENTATION.


  METHOD get_field_name.

    CASE iv_milestone.
      WHEN zif_gtt_pof_app_constants=>cs_milestone-dl_put_away.
        rv_field_name   = 'KOSTA'.
      WHEN zif_gtt_pof_app_constants=>cs_milestone-dl_packing.
        rv_field_name   = 'PKSTA'.
      WHEN zif_gtt_pof_app_constants=>cs_milestone-dl_goods_receipt.
        rv_field_name   = 'WBSTA'.
      WHEN zif_gtt_pof_app_constants=>cs_milestone-dl_pod.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'PDSTK'
                                    ELSE 'PDSTA' ).
      WHEN OTHERS.
        MESSAGE e009(zgtt_pof) WITH iv_milestone INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
    ENDCASE.

    IF iv_internal = abap_true.
      rv_field_name   = |Z_{ rv_field_name }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_status.

    TYPES: tt_vbup  TYPE STANDARD TABLE OF vbupvb.

    DATA: lv_dummy  TYPE char100.

    FIELD-SYMBOLS: <lt_vbup>  TYPE tt_vbup,
                   <ls_vbup>  TYPE vbupvb,
                   <lv_value> TYPE any.

    DATA(lv_fname)  = get_field_name( iv_milestone = iv_milestone ).

    DATA(lv_vbeln)  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = ms_app_objects-maintabref
                        iv_field_name  = 'VBELN' ).

    DATA(lv_posnr)  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = ms_app_objects-maintabref
                        iv_field_name  = 'POSNR' ).

    DATA(lr_vbup)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef    = zif_gtt_pof_app_constants=>cs_tabledef-dl_itm_status_new ).

    CLEAR rv_value.

    ASSIGN lr_vbup->* TO <lt_vbup>.

    IF <lt_vbup> IS ASSIGNED.
      READ TABLE <lt_vbup> ASSIGNING <ls_vbup>
        WITH KEY vbeln  = lv_vbeln
                 posnr  = lv_posnr.

      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_vbup> TO <lv_value>.
        IF <lv_value> IS ASSIGNED.
          rv_value  = <lv_value>.
        ELSE.
          MESSAGE e001(zgtt_pof) WITH lv_fname 'VBUP' INTO lv_dummy.
          zcl_gtt_pof_tools=>throw_exception( ).
        ENDIF.
      ELSE.
        MESSAGE e005(zgtt_pof)
          WITH 'VBUP' |{ lv_vbeln }-{ lv_posnr }|
          INTO lv_dummy.
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VBUP' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
