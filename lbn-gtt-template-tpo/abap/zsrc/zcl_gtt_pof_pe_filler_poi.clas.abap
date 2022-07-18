CLASS zcl_gtt_pof_pe_filler_poi DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_pe_filler .

  METHODS constructor
    IMPORTING
      !io_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters
      !io_bo_reader TYPE REF TO zif_gtt_pof_tp_reader .
PROTECTED SECTION.
PRIVATE SECTION.

  DATA mo_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters .
  DATA mo_bo_reader TYPE REF TO zif_gtt_pof_tp_reader .

  METHODS add_confirmation_event
    IMPORTING
      !is_app_objects TYPE trxas_appobj_ctab_wa
      !iv_milestonenum TYPE /saptrx/seq_num
    CHANGING
      !ct_expeventdata TYPE zif_gtt_pof_ef_types=>tt_expeventdata
    RAISING
      cx_udm_message .
  METHODS add_goods_receipt_event
    IMPORTING
      !is_app_objects TYPE trxas_appobj_ctab_wa
      !iv_milestonenum TYPE /saptrx/seq_num
    CHANGING
      !ct_expeventdata TYPE zif_gtt_pof_ef_types=>tt_expeventdata
    RAISING
      cx_udm_message .
  METHODS get_delivery_datetime
    IMPORTING
      !is_app_objects TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rv_datetime) TYPE /saptrx/event_exp_datetime
    RAISING
      cx_udm_message .
  METHODS get_object_field_value
    IMPORTING
      !is_app_objects TYPE trxas_appobj_ctab_wa
      !iv_fieldname TYPE clike
    RETURNING
      VALUE(rv_value) TYPE char50
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_pe_filler_poi IMPLEMENTATION.


  METHOD add_confirmation_event.

    DATA: lv_kzabs    TYPE ekpo-kzabs.

    lv_kzabs    = zcl_gtt_pof_tools=>get_field_of_structure(
                    ir_struct_data = is_app_objects-maintabref
                    iv_field_name  = 'KZABS' ).

    IF lv_kzabs IS NOT INITIAL.
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_pof_app_constants=>cs_milestone-po_confirmation
        milestonenum      = iv_milestonenum
      ) ).
    ENDIF.

  ENDMETHOD.


  METHOD add_goods_receipt_event.

    DATA: lv_wepos TYPE ekpo-wepos,
          lv_loekz TYPE ekpo-loekz.

    lv_wepos   = zcl_gtt_pof_tools=>get_field_of_structure(
                   ir_struct_data = is_app_objects-maintabref
                   iv_field_name  = 'WEPOS' ).

    IF lv_wepos IS NOT INITIAL.
      " clear expecting datetime and timezone when Item is marked as deleted
      " to avoid generation of unwanted GTTOverdue events
      lv_loekz   = zcl_gtt_pof_tools=>get_field_of_structure(
                     ir_struct_data = is_app_objects-maintabref
                     iv_field_name  = 'LOEKZ' ).

      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_pof_app_constants=>cs_milestone-po_goods_receipt
        evt_exp_tzone     = COND #( WHEN lv_loekz IS INITIAL
                                      THEN zcl_gtt_pof_tools=>get_system_time_zone( ) )
        evt_exp_datetime  = COND #( WHEN lv_loekz IS INITIAL
                                      THEN get_delivery_datetime( is_app_objects = is_app_objects ) )
        locid1            = zcl_gtt_pof_tools=>get_field_of_structure(
                                ir_struct_data = is_app_objects-maintabref
                                iv_field_name  = 'WERKS' )
        loctype           = zif_gtt_pof_ef_constants=>cs_loc_types-plant
        milestonenum      = iv_milestonenum
      ) ).

    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.

  ENDMETHOD.


  METHOD get_delivery_datetime.

    rv_datetime = zcl_gtt_pof_tools=>get_local_timestamp(
                    iv_date = get_object_field_value(
                                is_app_objects = is_app_objects
                                iv_fieldname   = 'EINDT' )
                    iv_time = CONV t( '000000' ) ).

  ENDMETHOD.


  METHOD get_object_field_value.

    DATA: lr_data  TYPE REF TO data,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <ls_data>  TYPE any,
                   <lv_value> TYPE any.

    lr_data = mo_bo_reader->get_data( is_app_object = is_app_objects ).

    ASSIGN lr_data->* TO <ls_data>.
    IF <ls_data> IS ASSIGNED.
      ASSIGN COMPONENT iv_fieldname OF STRUCTURE <ls_data> TO <lv_value>.
      IF <lv_value> IS ASSIGNED.
        rv_value = <lv_value>.
      ELSE.
        MESSAGE e001(zgtt_pof) WITH iv_fieldname 'po item' INTO lv_dummy ##NO_TEXT.
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'po item' INTO lv_dummy ##NO_TEXT .
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_pe_filler~check_relevance.

    TYPES: tt_ekpo    TYPE STANDARD TABLE OF uekpo.

    DATA: lv_dummy    TYPE char100.

    FIELD-SYMBOLS: <ls_ekpo_new> TYPE uekpo,
                   <lt_ekpo_old> TYPE tt_ekpo,
                   <ls_ekpo_old> TYPE uekpo.

    rv_result = zif_gtt_pof_ef_constants=>cs_condition-false.

    IF zcl_gtt_pof_po_tools=>is_appropriate_po_type( ir_ekko = is_app_objects-mastertabref ) = abap_true AND
       zcl_gtt_pof_po_tools=>is_appropriate_po_item( ir_ekpo = is_app_objects-maintabref ) = abap_true.

      IF is_app_objects-update_indicator = zif_gtt_pof_ef_constants=>cs_change_mode-insert.
        rv_result = zif_gtt_pof_ef_constants=>cs_condition-true.
      ELSE.
        DATA(lr_ekpo) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-po_item_old ).

        ASSIGN is_app_objects-maintabref->* TO <ls_ekpo_new>.
        ASSIGN lr_ekpo->* TO <lt_ekpo_old>.

        IF <ls_ekpo_new> IS ASSIGNED AND
           <lt_ekpo_old> IS ASSIGNED AND
           ( <ls_ekpo_new>-kz = zif_gtt_pof_ef_constants=>cs_change_mode-update OR
             <ls_ekpo_new>-kz = zif_gtt_pof_ef_constants=>cs_change_mode-undefined ).

          READ TABLE <lt_ekpo_old> ASSIGNING <ls_ekpo_old>
            WITH KEY ebeln = <ls_ekpo_new>-ebeln
                     ebelp = <ls_ekpo_new>-ebelp.
          IF sy-subrc = 0.
            rv_result = COND #( WHEN <ls_ekpo_new>-kzabs <> <ls_ekpo_old>-kzabs OR
                                     <ls_ekpo_new>-wepos <> <ls_ekpo_old>-wepos OR
                                     <ls_ekpo_new>-loekz <> <ls_ekpo_old>-loekz
                                  THEN zif_gtt_pof_ef_constants=>cs_condition-true
                                  ELSE zif_gtt_pof_ef_constants=>cs_condition-false ).
          ELSE.
            MESSAGE e005(zgtt_pof)
              WITH 'EKPO' |{ <ls_ekpo_new>-ebeln }{ <ls_ekpo_new>-ebelp }|
              INTO lv_dummy.
            zcl_gtt_pof_tools=>throw_exception( ).
          ENDIF.
        ELSE.
          MESSAGE e002(zgtt_pof) WITH 'EKPO' INTO lv_dummy.
          zcl_gtt_pof_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_pe_filler~get_planed_events.

    add_confirmation_event(
      EXPORTING
        is_app_objects  = is_app_objects
        iv_milestonenum = 1
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    add_goods_receipt_event(
      EXPORTING
        is_app_objects  = is_app_objects
        iv_milestonenum = 2
      CHANGING
        ct_expeventdata = ct_expeventdata ).

  ENDMETHOD.
ENDCLASS.
