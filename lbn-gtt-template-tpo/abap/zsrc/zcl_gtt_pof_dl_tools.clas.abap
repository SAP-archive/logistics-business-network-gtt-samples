CLASS zcl_gtt_pof_dl_tools DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  CLASS-METHODS convert_quantity_into_pounits
    IMPORTING
      !iv_quantity_uom TYPE any
      !ir_lips TYPE REF TO data
    RETURNING
      VALUE(rv_quantity_pou) TYPE f
    RAISING
      cx_udm_message .
  CLASS-METHODS get_addres_info
    IMPORTING
      !iv_addr_type TYPE ad_adrtype DEFAULT zif_gtt_pof_app_constants=>cs_adrtype-organization
      !iv_addr_numb TYPE ad_addrnum
    EXPORTING
      !ev_address TYPE clike
      !ev_email TYPE clike
      !ev_telephone TYPE clike
    RAISING
      cx_udm_message .
  CLASS-METHODS get_door_description
    IMPORTING
      !iv_lgnum TYPE lgnum
      !iv_lgtor TYPE lgtor
    RETURNING
      VALUE(rv_descr) TYPE /saptrx/paramval200
    RAISING
      cx_udm_message .
  CLASS-METHODS get_delivery_date
    IMPORTING
      !ir_data TYPE REF TO data
    RETURNING
      VALUE(rv_date) TYPE /saptrx/event_exp_datetime
    RAISING
      cx_udm_message .
  CLASS-METHODS get_next_event_counter
    RETURNING
      VALUE(rv_evtcnt) TYPE /saptrx/evtcnt .
  CLASS-METHODS get_plant_address_number
    IMPORTING
      !iv_werks TYPE werks_d
    RETURNING
      VALUE(ev_adrnr) TYPE adrnr
    RAISING
      cx_udm_message .
  CLASS-METHODS get_tracking_id_dl_item
    IMPORTING
      !ir_lips TYPE REF TO data
    RETURNING
      VALUE(rv_track_id) TYPE /saptrx/trxid
    RAISING
      cx_udm_message .
  CLASS-METHODS is_appropriate_dl_item
    IMPORTING
      !ir_struct TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  CLASS-METHODS is_appropriate_dl_type
    IMPORTING
      !ir_struct TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
PROTECTED SECTION.
PRIVATE SECTION.

  CLASS-DATA mv_evtcnt TYPE /saptrx/evtcnt VALUE zif_gtt_pof_app_constants=>cs_start_evtcnt-delivery ##NO_TEXT.
ENDCLASS.



CLASS zcl_gtt_pof_dl_tools IMPLEMENTATION.


  METHOD convert_quantity_into_pounits.

    DATA(lv_matnr)  = CONV matnr( zcl_gtt_pof_tools=>get_field_of_structure(
                                       ir_struct_data = ir_lips
                                       iv_field_name  = 'MATNR' ) ).
    DATA(lv_vrkme)  = CONV vrkme( zcl_gtt_pof_tools=>get_field_of_structure(
                                       ir_struct_data = ir_lips
                                       iv_field_name  = 'VRKME' ) ).

    CALL FUNCTION 'MATERIAL_UNIT_CONVERSION'
      EXPORTING
        input                = iv_quantity_uom
        matnr                = lv_matnr
        meinh                = lv_vrkme
      IMPORTING
        output               = rv_quantity_pou
      EXCEPTIONS
        conversion_not_found = 1
        input_invalid        = 2
        material_not_found   = 3
        meinh_not_found      = 4
        meins_missing        = 5
        no_meinh             = 6
        output_invalid       = 7
        overflow             = 8
        OTHERS               = 9.
    IF sy-subrc <> 0.
      CLEAR: rv_quantity_pou.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_addres_info.

    DATA: lt_address   TYPE szadr_printform_table,
          ls_addr_comp TYPE szadr_addr1_complete.


    IF ev_address IS REQUESTED.
      CLEAR: ev_address.

      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
        EXPORTING
          address_type                   = iv_addr_type
          address_number                 = iv_addr_numb
        IMPORTING
          address_printform_table        = lt_address
        EXCEPTIONS
          address_blocked                = 1
          person_blocked                 = 2
          contact_person_blocked         = 3
          addr_to_be_formated_is_blocked = 4
          OTHERS                         = 5.

      IF sy-subrc = 0.
        LOOP AT lt_address ASSIGNING FIELD-SYMBOL(<ls_address>).
          ev_address  = COND #( WHEN ev_address IS INITIAL
                                  THEN <ls_address>-address_line
                                  ELSE |{ ev_address }${ <ls_address>-address_line }| ).
        ENDLOOP.
      ELSE.
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

    IF ev_email IS REQUESTED OR ev_telephone IS REQUESTED.
      CLEAR: ev_email, ev_telephone.

      CALL FUNCTION 'ADDR_GET_COMPLETE'
        EXPORTING
          addrnumber              = iv_addr_numb
        IMPORTING
          addr1_complete          = ls_addr_comp
        EXCEPTIONS
          parameter_error         = 1
          address_not_exist       = 2
          internal_error          = 3
          wrong_access_to_archive = 4
          address_blocked         = 5
          OTHERS                  = 6.

      IF sy-subrc = 0.
        ev_email      = VALUE #( ls_addr_comp-adsmtp_tab[ 1 ]-adsmtp-smtp_addr OPTIONAL ).
        ev_telephone  = VALUE #( ls_addr_comp-adtel_tab[ 1 ]-adtel-tel_number OPTIONAL ).
      ELSE.
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_delivery_date.

    rv_date = zcl_gtt_pof_tools=>get_local_timestamp(
                iv_date = zcl_gtt_pof_tools=>get_field_of_structure(
                            ir_struct_data = ir_data
                            iv_field_name  = 'LFDAT' )
                iv_time = zcl_gtt_pof_tools=>get_field_of_structure(
                            ir_struct_data = ir_data
                            iv_field_name  = 'LFUHR' ) ).

  ENDMETHOD.


  METHOD get_door_description.

    "concatenate T300T-LNUMT '/' T30BT-ltort using SY-LANGU and LIPSVB-LGNUM & LIPSVB-LGTOR
    DATA: ls_t300t TYPE t300t,
          lv_ltort TYPE t30bt-ltort.

    CLEAR: rv_descr.

    CALL FUNCTION 'T300T_SINGLE_READ'
      EXPORTING
        t300t_spras = sy-langu
        t300t_lgnum = iv_lgnum
      IMPORTING
        wt300t      = ls_t300t
      EXCEPTIONS
        not_found   = 1
        OTHERS      = 2.

    IF sy-subrc = 0.
      SELECT SINGLE ltort
        INTO lv_ltort
        FROM t30bt
        WHERE spras = sy-langu
          AND lgnum = iv_lgnum
          AND lgtor = iv_lgtor.

      IF sy-subrc = 0.
        rv_descr    = |{ ls_t300t-lnumt }/{ lv_ltort }|.
      ELSE.
        MESSAGE e057(00) WITH iv_lgnum iv_lgtor '' 'T30BT'
          INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_next_event_counter.

    ADD 1 TO mv_evtcnt.

    rv_evtcnt = mv_evtcnt.

  ENDMETHOD.


  METHOD get_plant_address_number.

    DATA: ls_t001w TYPE T001w.

    CALL FUNCTION 'WCB_T001W_SINGLE_READ'
      EXPORTING
        i_werks   = iv_werks
      IMPORTING
        e_t001w   = ls_t001w
      EXCEPTIONS
        not_found = 1
        OTHERS    = 2.

    IF sy-subrc = 0.
      ev_adrnr    = ls_t001w-adrnr.
    ELSE.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_tracking_id_dl_item.

    DATA: lv_vbeln TYPE lips-vbeln,
          lv_posnr TYPE lips-posnr.

    lv_vbeln  = zcl_gtt_pof_tools=>get_field_of_structure(
                  ir_struct_data = ir_lips
                  iv_field_name  = 'VBELN' ).

    lv_posnr  = zcl_gtt_pof_tools=>get_field_of_structure(
                  ir_struct_data = ir_lips
                  iv_field_name  = 'POSNR' ).

    rv_track_id   = |{ lv_vbeln }{ lv_posnr }|.

  ENDMETHOD.


  METHOD is_appropriate_dl_item.

    DATA(lv_pstyv)  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = ir_struct
                        iv_field_name  = 'PSTYV' ).

    rv_result = boolc( lv_pstyv = zif_gtt_pof_app_constants=>cs_relevance-pstyv ).

  ENDMETHOD.


  METHOD is_appropriate_dl_type.

    DATA(lv_lfart)  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = ir_struct
                        iv_field_name  = 'LFART' ).

    rv_result = boolc( lv_lfart = zif_gtt_pof_app_constants=>cs_relevance-lfart ).

  ENDMETHOD.
ENDCLASS.
