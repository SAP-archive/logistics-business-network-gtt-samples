CLASS zcl_gtt_pof_event_rel_dl_hd DEFINITION
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



CLASS zcl_gtt_pof_event_rel_dl_hd IMPLEMENTATION.


  METHOD get_field_name.

    CASE iv_milestone.
      WHEN zif_gtt_pof_app_constants=>cs_milestone-dl_put_away.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'KOSTA'
                                    ELSE 'KOSTK' ).
      WHEN zif_gtt_pof_app_constants=>cs_milestone-dl_packing.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'PKSTA'
                                    ELSE 'PKSTK' ).
      WHEN zif_gtt_pof_app_constants=>cs_milestone-dl_goods_receipt.
        rv_field_name   = COND #( WHEN iv_internal = abap_true
                                    THEN 'WBSTA'
                                    ELSE 'WBSTK' ).
      WHEN zif_gtt_pof_app_constants=>cs_milestone-dl_pod.
        rv_field_name   = 'PDSTK'.
      WHEN OTHERS.
        MESSAGE e009(zgtt_pof) WITH iv_milestone INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
    ENDCASE.

    IF iv_internal = abap_true.
      rv_field_name   = |Z_{ rv_field_name }|.
    ENDIF.

  ENDMETHOD.


  METHOD get_object_status.

*    TYPES: tt_vbuk  TYPE STANDARD TABLE OF vbuk.

    DATA: lv_dummy  TYPE char100.

    FIELD-SYMBOLS: <lt_vbuk>  TYPE shp_vl10_vbuk_t,
                   <ls_vbuk>  TYPE vbukvb,
                   <lv_value> TYPE any.

    DATA(lv_fname)  = get_field_name( iv_milestone = iv_milestone ).

    DATA(lv_vbeln)  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = ms_app_objects-maintabref
                        iv_field_name  = 'VBELN' ).

    DATA(lr_vbuk)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef    = zif_gtt_pof_app_constants=>cs_tabledef-dl_hdr_status_new ).

    CLEAR rv_value.

    ASSIGN lr_vbuk->* TO <lt_vbuk>.

    IF <lt_vbuk> IS ASSIGNED.
      READ TABLE <lt_vbuk> ASSIGNING <ls_vbuk>
        WITH KEY vbeln  = lv_vbeln.

      IF sy-subrc = 0.
        ASSIGN COMPONENT lv_fname OF STRUCTURE <ls_vbuk> TO <lv_value>.
        IF <lv_value> IS ASSIGNED.
          rv_value  = <lv_value>.
        ELSE.
          MESSAGE e001(zgtt_pof) WITH lv_fname 'VBUP' INTO lv_dummy.
          zcl_gtt_pof_tools=>throw_exception( ).
        ENDIF.
      ELSE.
        MESSAGE e005(zgtt_pof)
          WITH 'VBUK' lv_vbeln
          INTO lv_dummy.
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VBUK' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
