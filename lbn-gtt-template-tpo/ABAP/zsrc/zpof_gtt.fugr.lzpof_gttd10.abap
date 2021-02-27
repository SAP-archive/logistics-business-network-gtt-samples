*&---------------------------------------------------------------------*
*& Local class definition - General for Transaction Processing
*&---------------------------------------------------------------------*

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_tools DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS are_fields_different
      IMPORTING
        ir_data1         TYPE REF TO data
        ir_data2         TYPE REF TO data
        it_fields        TYPE lif_ef_types=>tt_field_name
      RETURNING
        VALUE(rv_result) TYPE lif_ef_types=>tv_condition
      RAISING
        cx_udm_message.

    CLASS-METHODS are_structures_different
      IMPORTING
        ir_data1         TYPE REF TO data
        ir_data2         TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE lif_ef_types=>tv_condition
      RAISING
        cx_udm_message.

    CLASS-METHODS convert_datetime_to_utc
      IMPORTING
        iv_datetime            TYPE timestamp
        iv_timezone            TYPE timezone
      RETURNING
        VALUE(rv_datetime_utc) TYPE char15.

    CLASS-METHODS convert_to_external_amount
      IMPORTING
        iv_currency        TYPE waers_curc
        iv_internal        TYPE any
      RETURNING
        VALUE(rv_external) TYPE lif_ef_types=>tv_currency_amnt.

    CLASS-METHODS get_field_of_structure
      IMPORTING
        ir_struct_data  TYPE REF TO data
        iv_field_name   TYPE clike
      RETURNING
        VALUE(rv_value) TYPE char50
      RAISING
        cx_udm_message.

    CLASS-METHODS get_errors_log
      IMPORTING
        io_umd_message TYPE REF TO cx_udm_message
        iv_appsys      TYPE c
      EXPORTING
        es_bapiret     TYPE bapiret2.

    CLASS-METHODS get_local_timestamp
      IMPORTING
        iv_date             TYPE any DEFAULT sy-datum
        iv_time             TYPE any DEFAULT sy-uzeit
      RETURNING
        VALUE(rv_timestamp) TYPE /saptrx/event_exp_datetime.

    CLASS-METHODS get_pretty_value
      IMPORTING
        iv_value         TYPE any
      RETURNING
        VALUE(rv_pretty) TYPE /saptrx/paramval200.

    CLASS-METHODS get_system_time_zone
      RETURNING
        VALUE(rv_tzone) TYPE timezone
      RAISING
        cx_udm_message.

    CLASS-METHODS get_system_date_time
      RETURNING
        VALUE(rv_datetime) TYPE string
      RAISING
        cx_udm_message.

    CLASS-METHODS get_valid_datetime
      IMPORTING
        iv_timestamp       TYPE timestamp
      RETURNING
        VALUE(rv_datetime) TYPE char15.

    CLASS-METHODS is_date
      IMPORTING
        iv_value         TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_number
      IMPORTING
        iv_value         TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_object_changed
      IMPORTING
        is_app_object    TYPE trxas_appobj_ctab_wa
        io_ef_parameters TYPE REF TO lif_ef_parameters
        it_check_tables  TYPE lif_ef_types=>tt_strucdatadef OPTIONAL
        iv_key_field     TYPE clike
        iv_upd_field     TYPE clike
        iv_chk_mastertab TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    CLASS-METHODS is_table
      IMPORTING
        iv_value         TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_timestamp
      IMPORTING
        iv_value         TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS log_exception
      IMPORTING
        io_udm_message TYPE REF TO cx_udm_message
        iv_log_name    TYPE balnrext OPTIONAL
        iv_object      TYPE balobj_d
        iv_subobject   TYPE balsubobj.

    CLASS-METHODS throw_exception
      IMPORTING
        iv_textid TYPE sotr_conc DEFAULT ''
      RAISING
        cx_udm_message.

  PRIVATE SECTION.
    CLASS-METHODS is_structure_changed
      IMPORTING
        ir_struct        TYPE REF TO data
        iv_upd_field     TYPE clike
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.

    CLASS-METHODS is_table_changed
      IMPORTING
        ir_table         TYPE REF TO data
        iv_key_field     TYPE clike
        iv_upd_field     TYPE clike
        iv_key_value     TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_tools IMPLEMENTATION.
  METHOD are_fields_different.
    FIELD-SYMBOLS: <ls_data1>  TYPE any,
                   <ls_data2>  TYPE any,
                   <lv_value1> TYPE any,
                   <lv_value2> TYPE any.

    ASSIGN ir_data1->* TO <ls_data1>.
    ASSIGN ir_data2->* TO <ls_data2>.

    rv_result = lif_ef_constants=>cs_condition-false.

    LOOP AT it_fields ASSIGNING FIELD-SYMBOL(<lv_field>).
      ASSIGN COMPONENT <lv_field> OF STRUCTURE <ls_data1> TO <lv_value1>.
      ASSIGN COMPONENT <lv_field> OF STRUCTURE <ls_data2> TO <lv_value2>.

      IF <lv_value1> IS ASSIGNED AND
         <lv_value2> IS ASSIGNED.

        IF <lv_value1> <> <lv_value2>.
          rv_result = lif_ef_constants=>cs_condition-true.
          EXIT.
        ENDIF.

        UNASSIGN: <lv_value1>, <lv_value2>.
      ELSE.
        MESSAGE e001(zpof_gtt) WITH <lv_field> '' INTO DATA(lv_dummy).
        lcl_tools=>throw_exception( ).
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

      rv_result   = lif_ef_constants=>cs_condition-false.

      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
        ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_data1> TO <lv_value1>.
        ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_data2> TO <lv_value2>.

        IF <lv_value1> IS ASSIGNED AND
           <lv_value2> IS ASSIGNED.
          IF <lv_value1> <> <lv_value2>.
            rv_result   = lif_ef_constants=>cs_condition-true.
            EXIT.
          ENDIF.
        ELSE.
          MESSAGE e001(zpof_gtt) WITH <ls_fields>-name '' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH '' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
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
        MESSAGE e001(zpof_gtt) WITH iv_field_name '' INTO lv_dummy.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH '' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
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

  METHOD get_local_timestamp.
    rv_timestamp    = COND #( WHEN iv_date IS NOT INITIAL
                                THEN |0{ iv_date }{ iv_time }| ).
  ENDMETHOD.

  METHOD get_pretty_value.
    rv_pretty   = COND #( WHEN lcl_tools=>is_date( iv_value = iv_value ) = abap_true
                            THEN COND #( WHEN iv_value IS NOT INITIAL
                                           THEN iv_value
                                           ELSE '' )
                          WHEN lcl_tools=>is_timestamp( iv_value = iv_value ) = abap_true
                            THEN COND #( WHEN iv_value IS NOT INITIAL
                                           THEN get_valid_datetime( iv_timestamp = iv_value )
                                           ELSE '' )
                          WHEN lcl_tools=>is_number( iv_value = iv_value ) = abap_true
                            THEN |{ iv_value }|
                            ELSE iv_value ).
  ENDMETHOD.

  METHOD get_system_time_zone.
    CALL FUNCTION 'GET_SYSTEM_TIMEZONE'
      IMPORTING
        timezone            = rv_tzone
      EXCEPTIONS
        customizing_missing = 1
        OTHERS              = 2.
    IF sy-subrc <> 0.
      MESSAGE e003(zpof_gtt) INTO DATA(lv_dummy).
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_system_date_time.
    rv_datetime   = |0{ sy-datum }{ sy-uzeit }|.
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
            MESSAGE e001(zpof_gtt)
              WITH iv_key_field is_app_object-maintabdef
              INTO lv_dummy.
            lcl_tools=>throw_exception( ).
          ENDIF.
        ELSE.
          MESSAGE e002(zpof_gtt) WITH '' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
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
        MESSAGE e001(zpof_gtt) WITH iv_upd_field '' INTO lv_dummy.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH '' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD is_table.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = iv_value ).

    rv_result = boolc( lo_type->type_kind = cl_abap_typedescr=>typekind_table ).
  ENDMETHOD.

  METHOD is_timestamp.
    DATA lo_elem  TYPE REF TO cl_abap_elemdescr.

    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = iv_value ).


    IF lo_type IS INSTANCE OF cl_abap_elemdescr.
      lo_elem ?= lo_type.

      rv_result   = boolc( lo_elem->help_id = lif_ef_constants=>cs_date_types-timestamp ).
    ELSE.
      rv_result   = abap_false.
    ENDIF.
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
          MESSAGE e001(zpof_gtt) WITH lv_fname '' INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH '' INTO lv_dummy.
      lcl_tools=>throw_exception( ).
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

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_ef_performer DEFINITION.
  PUBLIC SECTION.
    CLASS-METHODS check_relevance
      IMPORTING
        is_definition         TYPE lif_ef_types=>ts_definition
        io_bo_factory         TYPE REF TO lif_factory
        iv_appsys             TYPE /saptrx/applsystem
        is_app_obj_types      TYPE /saptrx/aotypes
        it_all_appl_tables    TYPE trxas_tabcontainer
        it_app_type_cntl_tabs TYPE trxas_apptype_tabs OPTIONAL
        it_app_objects        TYPE trxas_appobj_ctabs
      RETURNING
        VALUE(rv_result)      TYPE sy-binpt
      RAISING
        cx_udm_message.

    CLASS-METHODS get_control_data
      IMPORTING
        is_definition         TYPE lif_ef_types=>ts_definition
        io_bo_factory         TYPE REF TO lif_factory
        iv_appsys             TYPE /saptrx/applsystem
        is_app_obj_types      TYPE /saptrx/aotypes
        it_all_appl_tables    TYPE trxas_tabcontainer
        it_app_type_cntl_tabs TYPE trxas_apptype_tabs
        it_app_objects        TYPE trxas_appobj_ctabs
      CHANGING
        ct_control_data       TYPE lif_ef_types=>tt_control_data
      RAISING
        cx_udm_message.

    CLASS-METHODS get_planned_events
      IMPORTING
        is_definition         TYPE lif_ef_types=>ts_definition
        io_factory            TYPE REF TO lif_factory
        iv_appsys             TYPE /saptrx/applsystem
        is_app_obj_types      TYPE /saptrx/aotypes
        it_all_appl_tables    TYPE trxas_tabcontainer
        it_app_type_cntl_tabs TYPE trxas_apptype_tabs
        it_app_objects        TYPE trxas_appobj_ctabs
      CHANGING
        ct_expeventdata       TYPE lif_ef_types=>tt_expeventdata
        ct_measrmntdata       TYPE lif_ef_types=>tt_measrmntdata
        ct_infodata           TYPE lif_ef_types=>tt_infodata
      RAISING
        cx_udm_message.

    CLASS-METHODS get_track_id_data
      IMPORTING
        is_definition         TYPE lif_ef_types=>ts_definition
        io_bo_factory         TYPE REF TO lif_factory
        iv_appsys             TYPE /saptrx/applsystem
        is_app_obj_types      TYPE /saptrx/aotypes
        it_all_appl_tables    TYPE trxas_tabcontainer
        it_app_type_cntl_tabs TYPE trxas_apptype_tabs
        it_app_objects        TYPE trxas_appobj_ctabs
      EXPORTING
        et_track_id_data      TYPE lif_ef_types=>tt_track_id_data
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_ef_performer IMPLEMENTATION.
  METHOD check_relevance.
    DATA: lo_ef_processor   TYPE REF TO lif_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor   = io_bo_factory->get_ef_processor(
                          is_definition         = is_definition
                          io_bo_factory         = io_bo_factory
                          iv_appsys             = iv_appsys
                          is_app_obj_types      = is_app_obj_types
                          it_all_appl_tables    = it_all_appl_tables
                          it_app_type_cntl_tabs = it_app_type_cntl_tabs
                          it_app_objects        = it_app_objects ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " check relevance
    rv_result = lo_ef_processor->check_relevance( ).
  ENDMETHOD.

  METHOD get_control_data.
    DATA: lo_ef_processor   TYPE REF TO lif_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor   = io_bo_factory->get_ef_processor(
                          is_definition         = is_definition
                          io_bo_factory         = io_bo_factory
                          iv_appsys             = iv_appsys
                          is_app_obj_types      = is_app_obj_types
                          it_all_appl_tables    = it_all_appl_tables
                          it_app_type_cntl_tabs = it_app_type_cntl_tabs
                          it_app_objects        = it_app_objects
                        ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill control data from business object data
    lo_ef_processor->get_control_data(
      CHANGING
        ct_control_data = ct_control_data[]
    ).

  ENDMETHOD.

  METHOD get_planned_events.
    DATA: lo_ef_processor   TYPE REF TO lif_ef_processor.

    " get instance of extractor function processor
    lo_ef_processor   = io_factory->get_ef_processor(
                          is_definition         = is_definition
                          io_bo_factory         = io_factory
                          iv_appsys             = iv_appsys
                          is_app_obj_types      = is_app_obj_types
                          it_all_appl_tables    = it_all_appl_tables
                          it_app_type_cntl_tabs = it_app_type_cntl_tabs
                          it_app_objects        = it_app_objects
                        ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill planned events data
    lo_ef_processor->get_planned_events(
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata
    ).

  ENDMETHOD.

  METHOD get_track_id_data.
    DATA: lo_ef_processor   TYPE REF TO lif_ef_processor.

    CLEAR: et_track_id_data[].

    " get instance of extractor function processor
    lo_ef_processor   = io_bo_factory->get_ef_processor(
                          is_definition         = is_definition
                          io_bo_factory         = io_bo_factory
                          iv_appsys             = iv_appsys
                          is_app_obj_types      = is_app_obj_types
                          it_all_appl_tables    = it_all_appl_tables
                          it_app_type_cntl_tabs = it_app_type_cntl_tabs
                          it_app_objects        = it_app_objects
                        ).

    " check i_app_objects     : is maintabdef correct?
    lo_ef_processor->check_app_objects( ).

    " fill controll data from PO Header data
    lo_ef_processor->get_track_id_data(
      IMPORTING
        et_track_id_data    = et_track_id_data
    ).

  ENDMETHOD.
ENDCLASS.


**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_ef_processor DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ef_processor.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters
        io_bo_reader     TYPE REF TO lif_bo_reader
        io_pe_filler     TYPE REF TO lif_pe_filler
        is_definition    TYPE lif_ef_types=>ts_definition.

  PRIVATE SECTION.
    DATA: mo_ef_parameters TYPE REF TO lif_ef_parameters,
          mo_bo_reader     TYPE REF TO lif_bo_reader,
          mo_pe_filler     TYPE REF TO lif_pe_filler,
          ms_definition    TYPE lif_ef_types=>ts_definition.

    METHODS add_struct_to_control_data
      IMPORTING
        ir_bo_data      TYPE REF TO data
        iv_appobjid     TYPE /saptrx/aoid
      CHANGING
        ct_control_data TYPE lif_ef_types=>tt_control_data
      RAISING
        cx_udm_message.

    METHODS add_sys_attr_to_control_data
      IMPORTING
        iv_appobjid     TYPE /saptrx/aoid
      CHANGING
        ct_control_data TYPE lif_ef_types=>tt_control_data
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_ef_processor IMPLEMENTATION.

  METHOD add_struct_to_control_data.
    DATA: lt_fields       TYPE cl_abap_structdescr=>component_table,
          ls_control_data TYPE lif_ef_types=>ts_control_data,
          lr_mapping      TYPE REF TO data,
          lv_dummy        TYPE char100.

    FIELD-SYMBOLS: <ls_bo_data>   TYPE any,
                   <ls_mapping>   TYPE any,
                   <lt_value>     TYPE ANY TABLE,
                   <lv_value>     TYPE any,
                   <lv_paramname> TYPE any.

    ASSIGN ir_bo_data->* TO <ls_bo_data>.

    IF <ls_bo_data> IS ASSIGNED.
      " get fields list of the structure, which provided by reader class
      lt_fields = CAST cl_abap_structdescr(
                    cl_abap_typedescr=>describe_by_data(
                      p_data = <ls_bo_data> )
                  )->get_components( ).

      " assign mapping table to use it in converting of field names into external format
      lr_mapping  = mo_bo_reader->get_mapping_structure( ).
      ASSIGN lr_mapping->* TO <ls_mapping>.

      IF <ls_mapping> IS ASSIGNED.
        " fill generic parameters
        ls_control_data-appsys      = mo_ef_parameters->get_appsys( ).
        ls_control_data-appobjtype  = mo_ef_parameters->get_app_obj_types( )-aotype.
        ls_control_data-language    = sy-langu.
        ls_control_data-appobjid    = iv_appobjid.

        " walk around fields list and copy values one by one
        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
          ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_bo_data> TO <lv_value>.
          ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_mapping> TO <lv_paramname>.

          IF <lv_value> IS ASSIGNED AND <lv_paramname> IS ASSIGNED.
            ls_control_data-paramname   = <lv_paramname>.

            " cycled copy for table values
            IF lcl_tools=>is_table( iv_value = <lv_value> ) = abap_true.
              ASSIGN <lv_value> TO <lt_value>.

              CLEAR: ls_control_data-paramindex.

              LOOP AT <lt_value> ASSIGNING <lv_value>.
                ADD 1 TO ls_control_data-paramindex.
                ls_control_data-value     = lcl_tools=>get_pretty_value(
                                              iv_value = <lv_value> ).

                APPEND ls_control_data TO ct_control_data.
              ENDLOOP.

              " add clearing value, when multivalues table is empty
              IF <lt_value>[] IS INITIAL AND
                  mo_bo_reader->get_field_parameter(
                    iv_field_name = <lv_paramname>
                    iv_parameter  = lif_ef_constants=>cs_parameter_id-key_field
                  ) = abap_true.

                ls_control_data-paramindex  = 1.
                ls_control_data-value       = ''.
                APPEND ls_control_data TO ct_control_data.
              ENDIF.

              " simple copy for usual values
            ELSEIF <lv_value> IS NOT INITIAL OR
                   mo_bo_reader->get_field_parameter(
                     iv_field_name = <lv_paramname>
                     iv_parameter  = lif_ef_constants=>cs_parameter_id-no_empty_tag
                   ) = abap_false.

              ls_control_data-value     = lcl_tools=>get_pretty_value(
                                            iv_value = <lv_value> ).
              APPEND ls_control_data TO ct_control_data.
            ELSEIF ( lcl_tools=>is_date( iv_value = <lv_value> ) = abap_true OR
                     lcl_tools=>is_timestamp( iv_value = <lv_value> ) = abap_true ).

              ls_control_data-value     = ''.
              APPEND ls_control_data TO ct_control_data.
            ENDIF.
          ELSEIF <lv_value> IS NOT ASSIGNED.
            MESSAGE e001(zpof_gtt) WITH <ls_fields>-name 'data table' INTO lv_dummy ##NO_TEXT.
            lcl_tools=>throw_exception( ).
          ELSE.
            MESSAGE e001(zpof_gtt) WITH <ls_fields>-name 'mapping table' INTO lv_dummy ##NO_TEXT.
            lcl_tools=>throw_exception( ).
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE e002(zpof_gtt) WITH 'mapping table' INTO lv_dummy ##NO_TEXT.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zpof_gtt) WITH 'data table' INTO lv_dummy ##NO_TEXT.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD add_sys_attr_to_control_data.
    DATA: ls_control_data TYPE lif_ef_types=>ts_control_data.

    ls_control_data-appsys      = mo_ef_parameters->get_appsys( ).
    ls_control_data-appobjtype  = mo_ef_parameters->get_app_obj_types( )-aotype.
    ls_control_data-language    = sy-langu.
    ls_control_data-appobjid    = iv_appobjid.

    ls_control_data-paramname   = lif_ef_constants=>cs_system_fields-actual_bisiness_timezone.
    ls_control_data-value       = lcl_tools=>get_system_time_zone( ).
    APPEND ls_control_data TO ct_control_data.

    ls_control_data-paramname   = lif_ef_constants=>cs_system_fields-actual_bisiness_datetime.
    ls_control_data-value       = lcl_tools=>get_system_date_time( ).
    APPEND ls_control_data TO ct_control_data.

    ls_control_data-paramname   = lif_ef_constants=>cs_system_fields-actual_technical_timezone.
    ls_control_data-value       = lcl_tools=>get_system_time_zone( ).
    APPEND ls_control_data TO ct_control_data.

    ls_control_data-paramname   = lif_ef_constants=>cs_system_fields-actual_technical_datetime.
    ls_control_data-value       = lcl_tools=>get_system_date_time( ).
    APPEND ls_control_data TO ct_control_data.
  ENDMETHOD.

  METHOD constructor.
    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.
    mo_pe_filler        = io_pe_filler.
    ms_definition       = is_definition.
  ENDMETHOD.

  METHOD lif_ef_processor~check_app_objects.
    DATA: lr_app_objects TYPE REF TO data,
          lv_dummy       TYPE char100.

    FIELD-SYMBOLS: <lt_app_objects>  TYPE trxas_appobj_ctabs.

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).

    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>).
      IF <ls_app_objects>-maintabdef <> ms_definition-maintab.
        MESSAGE e087(/saptrx/asc)
          WITH <ls_app_objects>-maintabdef
               mo_ef_parameters->get_app_obj_types( )-controldatafunc
               lif_ef_constants=>cv_aot
               mo_ef_parameters->get_appsys( )
          INTO lv_dummy.

        lcl_tools=>throw_exception(
          iv_textid = lif_ef_constants=>cs_errors-table_determination ).

      ELSEIF ms_definition-mastertab IS NOT INITIAL AND
            <ls_app_objects>-mastertabdef <> ms_definition-mastertab.
        MESSAGE e088(/saptrx/asc)
          WITH <ls_app_objects>-maintabdef
               mo_ef_parameters->get_app_obj_types( )-controldatafunc
               lif_ef_constants=>cv_aot
               mo_ef_parameters->get_appsys( )
          INTO lv_dummy.

        lcl_tools=>throw_exception(
          iv_textid = lif_ef_constants=>cs_errors-table_determination ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_ef_processor~check_relevance.
    DATA(lr_app_objects)  = mo_ef_parameters->get_app_objects( ).
    FIELD-SYMBOLS <lt_app_objects>  TYPE trxas_appobj_ctabs.

    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    rv_result = lif_ef_constants=>cs_condition-false.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>).
      rv_result   = mo_bo_reader->check_relevance( is_app_object = <ls_app_objects> ).

      IF rv_result = lif_ef_constants=>cs_condition-false AND
         mo_pe_filler IS BOUND.
        rv_result = mo_pe_filler->check_relevance( is_app_objects = <ls_app_objects> ).
      ENDIF.

      IF rv_result = lif_ef_constants=>cs_condition-true.
        EXIT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_ef_processor~get_control_data.
    DATA: lt_control_data TYPE lif_ef_types=>tt_control_data,
          lr_app_objects  TYPE REF TO data,
          lr_bo_data      TYPE REF TO data.

    FIELD-SYMBOLS: <lt_app_objects>   TYPE trxas_appobj_ctabs.

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>)
      WHERE maintabdef = ms_definition-maintab.

      lr_bo_data    = mo_bo_reader->get_data(
        EXPORTING
          is_app_object    = <ls_app_objects> ).

      add_struct_to_control_data(
        EXPORTING
          ir_bo_data        = lr_bo_data
          iv_appobjid       = <ls_app_objects>-appobjid
        CHANGING
          ct_control_data   = lt_control_data ).

      add_sys_attr_to_control_data(
        EXPORTING
          iv_appobjid     = <ls_app_objects>-appobjid
        CHANGING
          ct_control_data   = lt_control_data ).
    ENDLOOP.

    " Add all the changes to result tables in the end of the method,
    " so that in case of exceptions there will be no inconsistent data in them
    IF lt_control_data[] IS NOT INITIAL.
      ct_control_data[] = VALUE #( BASE ct_control_data
                                   ( LINES OF lt_control_data ) ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_ef_processor~get_planned_events.
    DATA: lt_expeventdata TYPE lif_ef_types=>tt_expeventdata,
          lt_measrmntdata TYPE lif_ef_types=>tt_measrmntdata,
          lt_infodata     TYPE lif_ef_types=>tt_infodata,
          lr_app_objects  TYPE REF TO data.

    FIELD-SYMBOLS: <lt_app_objects>   TYPE trxas_appobj_ctabs.

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>)
      WHERE maintabdef = ms_definition-maintab.

      mo_pe_filler->get_planed_events(
        EXPORTING
          is_app_objects  = <ls_app_objects>
        CHANGING
          ct_expeventdata = lt_expeventdata
          ct_measrmntdata = lt_measrmntdata
          ct_infodata     = lt_infodata ).
    ENDLOOP.

    " Add all the changes to result tables in the end of the method,
    " so that in case of exceptions there will be no inconsistent data in them
    IF lt_expeventdata[] IS NOT INITIAL.
      ct_expeventdata[] = VALUE #( BASE ct_expeventdata
                                   ( LINES OF lt_expeventdata ) ).
    ENDIF.
    IF lt_measrmntdata[] IS NOT INITIAL.
      ct_measrmntdata[] = VALUE #( BASE ct_measrmntdata
                                   ( LINES OF lt_measrmntdata ) ).
    ENDIF.
    IF lt_expeventdata[] IS NOT INITIAL.
      lt_infodata[] = VALUE #( BASE ct_infodata
                                   ( LINES OF lt_infodata ) ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_ef_processor~get_track_id_data.
    DATA: lr_app_objects   TYPE REF TO data,
          lt_track_id_data TYPE lif_ef_types=>tt_track_id_data,
          lr_bo_data       TYPE REF TO data.

    FIELD-SYMBOLS: <lt_app_objects>   TYPE trxas_appobj_ctabs.

    CLEAR: et_track_id_data[].

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>)
      WHERE maintabdef = ms_definition-maintab.

      mo_bo_reader->get_track_id_data(
        EXPORTING
          is_app_object    = <ls_app_objects>
        IMPORTING
          et_track_id_data = lt_track_id_data
      ).

      et_track_id_data  = VALUE #( BASE et_track_id_data
                                   ( LINES OF lt_track_id_data ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_ef_parameters DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_ef_parameters.

    METHODS constructor
      IMPORTING
        iv_appsys             TYPE /saptrx/applsystem
        is_app_obj_types      TYPE /saptrx/aotypes
        it_all_appl_tables    TYPE trxas_tabcontainer
        it_app_type_cntl_tabs TYPE trxas_apptype_tabs OPTIONAL
        it_app_objects        TYPE trxas_appobj_ctabs.

  PRIVATE SECTION.
    DATA:
      mv_appsys             TYPE /saptrx/applsystem,
      ms_app_obj_types      TYPE /saptrx/aotypes,
      mr_all_appl_tables    TYPE REF TO data,     "trxas_tabcontainer,
      mr_app_type_cntl_tabs TYPE REF TO data,     "trxas_apptype_tabs,
      mr_app_objects        TYPE REF TO data.     "trxas_appobj_ctabs.
ENDCLASS.

CLASS lcl_ef_parameters IMPLEMENTATION.
  METHOD constructor.
    mv_appsys               = iv_appsys.
    ms_app_obj_types        = is_app_obj_types.
    mr_all_appl_tables      = REF #( it_all_appl_tables ).
    IF it_app_type_cntl_tabs IS SUPPLIED.
      mr_app_type_cntl_tabs = REF #( it_app_type_cntl_tabs ).
    ENDIF.
    mr_app_objects          = REF #( it_app_objects ).
  ENDMETHOD.

  METHOD lif_ef_parameters~get_appl_table.
    TRY.
        FIELD-SYMBOLS: <lt_all_appl_tables>   TYPE trxas_tabcontainer.

        ASSIGN mr_all_appl_tables->* TO <lt_all_appl_tables>.

        rr_data   = <lt_all_appl_tables>[ tabledef = iv_tabledef ]-tableref.

      CATCH cx_sy_itab_line_not_found.
        MESSAGE e008(/saptrx/asc)
          WITH iv_tabledef
               ms_app_obj_types-aotype
          INTO DATA(lv_dummy).

        lcl_tools=>throw_exception(
          iv_textid = lif_ef_constants=>cs_errors-stop_processing ).
    ENDTRY.
  ENDMETHOD.

  METHOD lif_ef_parameters~get_app_obj_types.
    rs_app_obj_types    = ms_app_obj_types.
  ENDMETHOD.

  METHOD lif_ef_parameters~get_app_objects.
    rr_data             = mr_app_objects.
  ENDMETHOD.

  METHOD lif_ef_parameters~get_appsys.
    rv_appsys           = mv_appsys.
  ENDMETHOD.

ENDCLASS.

**********************************************************************
**********************************************************************
**********************************************************************
CLASS lcl_factory DEFINITION
  ABSTRACT.

  PUBLIC SECTION.
    INTERFACES lif_factory
      ABSTRACT METHODS get_bo_reader
      get_pe_filler.
ENDCLASS.

CLASS lcl_factory IMPLEMENTATION.
  METHOD lif_factory~get_ef_parameters.
    ro_ef_parameters  = NEW lcl_ef_parameters(
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects ).
  ENDMETHOD.

  METHOD lif_factory~get_ef_processor.
    DATA:
      lo_ef_parameters TYPE REF TO lif_ef_parameters,
      lo_bo_reader     TYPE REF TO lif_bo_reader,
      lo_pe_filler     TYPE REF TO lif_pe_filler.

    lo_ef_parameters  = lif_factory~get_ef_parameters(
      iv_appsys             = iv_appsys
      is_app_obj_types      = is_app_obj_types
      it_all_appl_tables    = it_all_appl_tables
      it_app_type_cntl_tabs = it_app_type_cntl_tabs
      it_app_objects        = it_app_objects ).

    lo_bo_reader      = lif_factory~get_bo_reader(
      io_ef_parameters      = lo_ef_parameters ).

    " filler is not obligatory
    TRY.
        lo_pe_filler      = lif_factory~get_pe_filler(
          io_ef_parameters      = lo_ef_parameters
          io_bo_reader          = lo_bo_reader ).
      CATCH cx_udm_message.
    ENDTRY.

    ro_ef_processor   = NEW lcl_ef_processor(
      io_ef_parameters      = lo_ef_parameters
      io_bo_reader          = lo_bo_reader
      io_pe_filler          = lo_pe_filler
      is_definition         = is_definition
    ).
  ENDMETHOD.
ENDCLASS.
