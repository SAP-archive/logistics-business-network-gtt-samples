CLASS zcl_gtt_pof_tools DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  CLASS-METHODS are_fields_different
    IMPORTING
      !ir_data1 TYPE REF TO data
      !ir_data2 TYPE REF TO data
      !it_fields TYPE zif_gtt_pof_ef_types=>tt_field_name
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_pof_ef_types=>tv_condition
    RAISING
      cx_udm_message .
  CLASS-METHODS are_structures_different
    IMPORTING
      !ir_data1 TYPE REF TO data
      !ir_data2 TYPE REF TO data
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_pof_ef_types=>tv_condition
    RAISING
      cx_udm_message .
  CLASS-METHODS convert_datetime_to_utc
    IMPORTING
      !iv_datetime TYPE timestamp
      !iv_timezone TYPE timezone
    RETURNING
      VALUE(rv_datetime_utc) TYPE char15 .
  CLASS-METHODS convert_to_external_amount
    IMPORTING
      !iv_currency TYPE waers_curc
      !iv_internal TYPE any
    RETURNING
      VALUE(rv_external) TYPE zif_gtt_pof_ef_types=>tv_currency_amnt .
  CLASS-METHODS get_field_of_structure
    IMPORTING
      !ir_struct_data TYPE REF TO data
      !iv_field_name TYPE clike
    RETURNING
      VALUE(rv_value) TYPE char50
    RAISING
      cx_udm_message .
  CLASS-METHODS get_errors_log
    IMPORTING
      !io_umd_message TYPE REF TO cx_udm_message
      !iv_appsys TYPE c
    EXPORTING
      !es_bapiret TYPE bapiret2 .
  CLASS-METHODS get_local_timestamp
    IMPORTING
      !iv_date TYPE any DEFAULT sy-datum
      !iv_time TYPE any DEFAULT sy-uzeit
    RETURNING
      VALUE(rv_timestamp) TYPE /saptrx/event_exp_datetime .
  CLASS-METHODS get_max_sequence_id
    IMPORTING
      !it_expeventdata TYPE zif_gtt_pof_ef_types=>tt_expeventdata
    RETURNING
      VALUE(rv_milestonenum) TYPE /saptrx/seq_num .
  CLASS-METHODS get_next_sequence_id
    IMPORTING
      !it_expeventdata TYPE zif_gtt_pof_ef_types=>tt_expeventdata
    RETURNING
      VALUE(rv_milestonenum) TYPE /saptrx/seq_num .
  CLASS-METHODS get_pretty_value
    IMPORTING
      !iv_value TYPE any
    RETURNING
      VALUE(rv_pretty) TYPE /saptrx/paramval200 .
  CLASS-METHODS get_system_time_zone
    RETURNING
      VALUE(rv_tzone) TYPE timezone
    RAISING
      cx_udm_message .
  CLASS-METHODS get_system_date_time
    RETURNING
      VALUE(rv_datetime) TYPE string
    RAISING
      cx_udm_message .
  CLASS-METHODS get_valid_datetime
    IMPORTING
      !iv_timestamp TYPE timestamp
    RETURNING
      VALUE(rv_datetime) TYPE char15 .
  CLASS-METHODS is_date
    IMPORTING
      !iv_value TYPE any
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
  CLASS-METHODS is_number
    IMPORTING
      !iv_value TYPE any
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
  CLASS-METHODS is_object_changed
    IMPORTING
      !is_app_object TYPE trxas_appobj_ctab_wa
      !io_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters
      !it_check_tables TYPE zif_gtt_pof_ef_types=>tt_strucdatadef OPTIONAL
      !iv_key_field TYPE clike
      !iv_upd_field TYPE clike
      !iv_chk_mastertab TYPE abap_bool DEFAULT abap_false
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  CLASS-METHODS is_table
    IMPORTING
      !iv_value TYPE any
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
  CLASS-METHODS is_timestamp
    IMPORTING
      !iv_value TYPE any
    RETURNING
      VALUE(rv_result) TYPE abap_bool .
  CLASS-METHODS log_exception
    IMPORTING
      !io_udm_message TYPE REF TO cx_udm_message
      !iv_log_name TYPE balnrext OPTIONAL
      !iv_object TYPE balobj_d
      !iv_subobject TYPE balsubobj .
  CLASS-METHODS throw_exception
    IMPORTING
      !iv_textid TYPE sotr_conc DEFAULT ''
    RAISING
      cx_udm_message .
PROTECTED SECTION.
PRIVATE SECTION.

  CLASS-METHODS is_structure_changed
    IMPORTING
      !ir_struct TYPE REF TO data
      !iv_upd_field TYPE clike
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  CLASS-METHODS is_table_changed
    IMPORTING
      !ir_table TYPE REF TO data
      !iv_key_field TYPE clike
      !iv_upd_field TYPE clike
      !iv_key_value TYPE any
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_tools IMPLEMENTATION.


  METHOD are_fields_different.

    FIELD-SYMBOLS: <ls_data1>  TYPE any,
                   <ls_data2>  TYPE any,
                   <lv_value1> TYPE any,
                   <lv_value2> TYPE any.

    ASSIGN ir_data1->* TO <ls_data1>.
    ASSIGN ir_data2->* TO <ls_data2>.

    rv_result = zif_gtt_pof_ef_constants=>cs_condition-false.

    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<lv_field>).
      ASSIGN COMPONENT <lv_field> OF STRUCTURE <ls_data1> TO <lv_value1>.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE <ls_data2> TO <lv_value2>.

      IF <lv_value1> IS ASSIGNED AND
         <lv_value2> IS ASSIGNED.

        IF <lv_value1> <> <lv_value2>.
          rv_result = zif_gtt_pof_ef_constants=>cs_condition-true.
          EXIT.
        ENDIF.

        UNASSIGN: <lv_value1>, <lv_value2>.
      ELSE.
        MESSAGE e001(zgtt_pof) WITH <lv_field> '' INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD are_structures_different.

    DATA: lt_fields TYPE cl_abap_structdescr=>component_table,
          lv_dummy  TYPE char100.

    FIELD-SYMBOLS: <ls_data1>  TYPE any,
                   <ls_data2>  TYPE any,
                   <lv_value1> TYPE any,
                   <lv_value2> TYPE any.

    ASSIGN ir_data1->* TO <ls_data1>.
    ASSIGN ir_data2->* TO <ls_data2>.

    IF <ls_data1> IS ASSIGNED AND <ls_data2> IS ASSIGNED.
      lt_fields = CAST cl_abap_structdescr(
                    cl_abap_typedescr=>describe_by_data(
                      p_data = <ls_data1> )
                  )->get_components( ).

      rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.

      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
        ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_data1> TO <lv_value1>.
        ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_data2> TO <lv_value2>.

        IF <lv_value1> IS ASSIGNED AND
           <lv_value2> IS ASSIGNED.
          IF <lv_value1> <> <lv_value2>.
            rv_result   = zif_gtt_pof_ef_constants=>cs_condition-true.
            EXIT.
          ENDIF.
        ELSE.
          MESSAGE e001(zgtt_pof) WITH <ls_fields>-name '' INTO lv_dummy.
          zcl_gtt_pof_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH '' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD convert_datetime_to_utc.

    DATA: lv_datetime_utc   TYPE timestamp.

    CALL FUNCTION '/SAPAPO/TIMESTAMP_ZONE_TO_UTC'
      EXPORTING
        iv_timezone  = iv_timezone
        iv_timestamp = iv_datetime
      IMPORTING
        ev_timestamp = lv_datetime_utc.

    rv_datetime_utc   = get_valid_datetime(
                          iv_timestamp = lv_datetime_utc ).

  ENDMETHOD.


  METHOD convert_to_external_amount.

    DATA: lv_external   TYPE bapicurr_d.

    CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
      EXPORTING
        currency        = iv_currency
        amount_internal = iv_internal
      IMPORTING
        amount_external = lv_external.

    rv_external   = round( val = lv_external dec = 2 ).

  ENDMETHOD.


  METHOD get_errors_log.

    es_bapiret-id           = io_umd_message->m_msgid.
    es_bapiret-number       = io_umd_message->m_msgno.
    es_bapiret-type         = io_umd_message->m_msgty.
    es_bapiret-message_v1   = io_umd_message->m_msgv1.
    es_bapiret-message_v1   = io_umd_message->m_msgv1.
    es_bapiret-message_v1   = io_umd_message->m_msgv1.
    es_bapiret-message_v1   = io_umd_message->m_msgv1.
    es_bapiret-system       = iv_appsys.

  ENDMETHOD.


  METHOD get_field_of_structure.

    DATA: lv_dummy      TYPE char100.

    FIELD-SYMBOLS: <ls_struct> TYPE any,
                   <lv_value>  TYPE any.

    ASSIGN ir_struct_data->* TO <ls_struct>.

    IF <ls_struct> IS ASSIGNED.
      ASSIGN COMPONENT iv_field_name OF STRUCTURE <ls_struct> TO <lv_value>.
      IF <lv_value> IS ASSIGNED.
        rv_value    = <lv_value>.
      ELSE.
        MESSAGE e001(zgtt_pof) WITH iv_field_name '' INTO lv_dummy.
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH '' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_local_timestamp.

    rv_timestamp    = COND #( WHEN iv_date IS NOT INITIAL
                                THEN |0{ iv_date }{ iv_time }| ).

  ENDMETHOD.


  METHOD get_max_sequence_id.

    CLEAR: rv_milestonenum.

    LOOP AT it_expeventdata ASSIGNING FIELD-SYMBOL(<ls_expeventdata>).
      IF rv_milestonenum < <ls_expeventdata>-milestonenum.
        rv_milestonenum = <ls_expeventdata>-milestonenum.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_next_sequence_id.

    rv_milestonenum   = get_max_sequence_id(
                          it_expeventdata = it_expeventdata ) + 1.

  ENDMETHOD.


  METHOD get_pretty_value.

    rv_pretty   = COND #( WHEN zcl_gtt_pof_tools=>is_date( iv_value = iv_value ) = abap_true
                            THEN COND #( WHEN iv_value IS NOT INITIAL
                                           THEN iv_value
                                           ELSE '' )
                          WHEN zcl_gtt_pof_tools=>is_timestamp( iv_value = iv_value ) = abap_true
                            THEN COND #( WHEN iv_value IS NOT INITIAL
                                           THEN get_valid_datetime( iv_timestamp = iv_value )
                                           ELSE '' )
                          WHEN zcl_gtt_pof_tools=>is_number( iv_value = iv_value ) = abap_true
                            THEN |{ iv_value }|
                            ELSE iv_value ).

  ENDMETHOD.


  METHOD get_system_date_time.

    rv_datetime   = |0{ sy-datum }{ sy-uzeit }|.

  ENDMETHOD.


  METHOD get_system_time_zone.

    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = rv_tzone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      MESSAGE e003(zgtt_pof) INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_valid_datetime.

    rv_datetime   = COND #( WHEN iv_timestamp <= 0
                              THEN ''
                            WHEN iv_timestamp <= 99991231235959
                              THEN |0{ iv_timestamp }|
                              ELSE iv_timestamp ).

  ENDMETHOD.


  METHOD is_date.

    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = iv_value ).

    rv_result     = boolc( lo_type->type_kind = cl_abap_typedescr=>typekind_date ).

  ENDMETHOD.


  METHOD is_number.

    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = iv_value ).

    rv_result = SWITCH #( lo_type->type_kind
      WHEN cl_abap_typedescr=>typekind_decfloat OR
           cl_abap_typedescr=>typekind_decfloat16 OR
           cl_abap_typedescr=>typekind_decfloat34 OR
           cl_abap_typedescr=>typekind_float OR
           cl_abap_typedescr=>typekind_int OR
           cl_abap_typedescr=>typekind_int1 OR
           cl_abap_typedescr=>typekind_int2 OR
           cl_abap_typedescr=>typekind_int8 OR
           cl_abap_typedescr=>typekind_num OR
           cl_abap_typedescr=>typekind_numeric OR
           cl_abap_typedescr=>typekind_packed
        THEN abap_true
        ELSE abap_false ).

  ENDMETHOD.


  METHOD is_object_changed.

    DATA: lv_ebeln TYPE ekko-ebeln,
          lv_dummy TYPE char255.

    FIELD-SYMBOLS: <ls_maintab>   TYPE any,
                   <lv_key_value> TYPE any.

    rv_result   = abap_false.

    IF is_app_object-update_indicator IS NOT INITIAL.
      IF is_structure_changed( ir_struct    = is_app_object-maintabref
                               iv_upd_field = iv_upd_field ) = abap_true.
        rv_result   = abap_true.
      ELSEIF iv_chk_mastertab = abap_true AND
             is_app_object-mastertabref IS BOUND AND
             is_structure_changed( ir_struct    = is_app_object-mastertabref
                                   iv_upd_field = iv_upd_field ) = abap_true.
        rv_result   = abap_true.
      ELSE.
        ASSIGN is_app_object-maintabref->* TO <ls_maintab>.

        IF <ls_maintab> IS ASSIGNED.
          ASSIGN COMPONENT iv_key_field OF STRUCTURE <ls_maintab> TO <lv_key_value>.

          IF <lv_key_value> IS ASSIGNED.
            LOOP AT it_check_tables ASSIGNING FIELD-SYMBOL(<ls_tables>).
              IF is_table_changed(
                   ir_table     = io_ef_parameters->get_appl_table( iv_tabledef = <ls_tables> )
                   iv_key_field = iv_key_field
                   iv_upd_field = iv_upd_field
                   iv_key_value = <lv_key_value> ) = abap_true.

                rv_result   = abap_true.
                EXIT.
              ENDIF.
            ENDLOOP.
          ELSE.
            MESSAGE e001(zgtt_pof)
              WITH iv_key_field is_app_object-maintabdef
              INTO lv_dummy.
            zcl_gtt_pof_tools=>throw_exception( ).
          ENDIF.
        ELSE.
          MESSAGE e002(zgtt_pof) WITH '' INTO lv_dummy.
          zcl_gtt_pof_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_structure_changed.

    DATA: lv_fname TYPE char5,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <ls_struct> TYPE any,
                   <lv_updkz>  TYPE any.

    rv_result = abap_false.

    ASSIGN ir_struct->* TO <ls_struct>.

    IF <ls_struct> IS ASSIGNED.
      ASSIGN COMPONENT iv_upd_field OF STRUCTURE <ls_struct> TO <lv_updkz>.

      IF <lv_updkz> IS ASSIGNED.

        rv_result   = boolc( <lv_updkz> IS NOT INITIAL ).

      ELSE.
        MESSAGE e001(zgtt_pof) WITH iv_upd_field '' INTO lv_dummy.
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH '' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_table.

    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = iv_value ).

    rv_result = boolc( lo_type->type_kind = cl_abap_typedescr=>typekind_table ).

  ENDMETHOD.


  METHOD is_table_changed.

    DATA: lv_fname TYPE char5,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <lt_table>     TYPE ANY TABLE,
                   <ls_table>     TYPE any,
                   <lv_key_value> TYPE any,
                   <lv_upd_value> TYPE any.

    rv_result = abap_false.

    ASSIGN ir_table->* TO <lt_table>.

    IF <lt_table> IS ASSIGNED.
      LOOP AT <lt_table> ASSIGNING <ls_table>.
        ASSIGN COMPONENT iv_key_field OF STRUCTURE <ls_table> TO <lv_key_value>.
        ASSIGN COMPONENT iv_upd_field OF STRUCTURE <ls_table> TO <lv_upd_value>.

        IF <lv_key_value> IS ASSIGNED AND
           <lv_upd_value> IS ASSIGNED.

          IF <lv_key_value>  = iv_key_value AND
             <lv_upd_value> IS NOT INITIAL.

            rv_result   = abap_true.
            EXIT.
          ENDIF.
        ELSE.
          lv_fname  = COND #( WHEN <lv_key_value> IS NOT ASSIGNED
                                THEN iv_key_field ELSE iv_upd_field ).
          MESSAGE e001(zgtt_pof) WITH lv_fname '' INTO lv_dummy.
          zcl_gtt_pof_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH '' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_timestamp.

    DATA lo_elem  TYPE REF TO cl_abap_elemdescr.

    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = iv_value ).


    IF lo_type IS INSTANCE OF cl_abap_elemdescr.
      lo_elem ?= lo_type.

      rv_result   = boolc( lo_elem->help_id = zif_gtt_pof_ef_constants=>cs_date_types-timestamp ).
    ELSE.
      rv_result   = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD log_exception.

    DATA: ls_log    TYPE bal_s_log,
          lv_handle TYPE balloghndl,
          ls_msg    TYPE bal_s_msg,
          lt_handle TYPE bal_t_logh.

    TRY.
        cl_reca_guid=>guid_create(
          IMPORTING
            ed_guid_22 = DATA(lv_guid) ).

        " open application log
        ls_log-object    = iv_object.
        ls_log-subobject = iv_subobject.
        ls_log-extnumber = lv_handle.
        ls_log-aldate    = sy-datum.
        ls_log-altime    = sy-uzeit.
        ls_log-aluser    = sy-uname.
        ls_log-altcode   = sy-tcode.
        CALL FUNCTION 'BAL_LOG_CREATE'
          EXPORTING
            i_s_log                 = ls_log
          IMPORTING
            e_log_handle            = lv_handle
          EXCEPTIONS
            log_header_inconsistent = 1
            OTHERS                  = 2.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        " add message to application log
        ls_msg    = VALUE #(
          msgty  = io_udm_message->m_msgty
          msgid  = io_udm_message->m_msgid
          msgno  = io_udm_message->m_msgno
          msgv1  = io_udm_message->m_msgv1
          msgv2  = io_udm_message->m_msgv2
          msgv3  = io_udm_message->m_msgv3
          msgv4  = io_udm_message->m_msgv4
        ).

        GET TIME STAMP FIELD ls_msg-time_stmp.

        CALL FUNCTION 'BAL_LOG_MSG_ADD'
          EXPORTING
            i_log_handle     = lv_handle
            i_s_msg          = ls_msg
          EXCEPTIONS
            log_not_found    = 1
            msg_inconsistent = 2
            log_is_full      = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.

        " save application log
        CLEAR lt_handle[].
        INSERT lv_handle INTO TABLE lt_handle.

        CALL FUNCTION 'BAL_DB_SAVE'
          EXPORTING
            i_in_update_task = ' '
            i_t_log_handle   = lt_handle
          EXCEPTIONS
            log_not_found    = 1
            save_not_allowed = 2
            numbering_error  = 3
            OTHERS           = 4.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
      CATCH cx_cacs_bal_ex.
    ENDTRY.

  ENDMETHOD.


  METHOD throw_exception.

    RAISE EXCEPTION TYPE cx_udm_message
      EXPORTING
        textid  = iv_textid
        m_msgid = sy-msgid
        m_msgty = sy-msgty
        m_msgno = sy-msgno
        m_msgv1 = sy-msgv1
        m_msgv2 = sy-msgv2
        m_msgv3 = sy-msgv3
        m_msgv4 = sy-msgv4.

  ENDMETHOD.
ENDCLASS.
