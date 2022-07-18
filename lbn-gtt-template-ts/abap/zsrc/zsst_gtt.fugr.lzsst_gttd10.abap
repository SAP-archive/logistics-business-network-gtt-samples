*&---------------------------------------------------------------------*
*& Local class definition - General for Transaction Processing
*&---------------------------------------------------------------------*

CLASS lcl_tools DEFINITION.
  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF cs_condition,
        true  TYPE sy-binpt VALUE 'T',
        false TYPE sy-binpt VALUE 'F',
      END OF cs_condition.

    CLASS-METHODS are_structures_different
      IMPORTING
        ir_data1         TYPE REF TO data
        ir_data2         TYPE REF TO data
      RETURNING
        VALUE(rv_result) TYPE sy-binpt
      RAISING
        cx_udm_message.

    CLASS-METHODS get_postal_address
      IMPORTING
        iv_node_id        TYPE /scmtms/bo_node_id
      EXPORTING
        et_postal_address TYPE /bofu/t_addr_postal_addressk
      RAISING
        cx_udm_message.

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

    CLASS-METHODS is_number
      IMPORTING
        iv_value         TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS is_table
      IMPORTING
        iv_value         TYPE any
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    CLASS-METHODS throw_exception
      IMPORTING
        iv_textid TYPE sotr_conc DEFAULT ''
      RAISING
        cx_udm_message.

    CLASS-METHODS get_stop_points
      IMPORTING
        iv_root_id     TYPE /scmtms/tor_id
        it_stop        TYPE /scmtms/t_em_bo_tor_stop
      EXPORTING
        et_stop_points TYPE lif_ef_types=>tt_stop_points.

    CLASS-METHODS is_odd
      IMPORTING
        iv_value         TYPE n
      RETURNING
        VALUE(rv_is_odd) TYPE abap_bool.

    CLASS-METHODS get_capa_match_key
      IMPORTING
        iv_assgn_stop_key       TYPE /scmtms/tor_stop_key
        it_capa_stop            TYPE /scmtms/t_em_bo_tor_stop
        it_capa_root            TYPE /scmtms/t_em_bo_tor_root
      RETURNING
        VALUE(rv_capa_matchkey) TYPE /saptrx/loc_id_2.

    CLASS-METHODS check_is_fo_deleted
      IMPORTING
        is_root_new      TYPE /scmtms/s_em_bo_tor_root
        is_root_old      TYPE /scmtms/s_em_bo_tor_root
      RETURNING
        VALUE(rv_result) TYPE lif_ef_types=>tv_condition
      RAISING
        cx_udm_message.

    CLASS-METHODS get_fo_tracked_item_obj
      IMPORTING
        is_app_object    TYPE trxas_appobj_ctab_wa
        is_root          TYPE /scmtms/s_em_bo_tor_root
        it_item          TYPE /scmtms/t_em_bo_tor_item
        iv_appsys        TYPE /saptrx/applsystem
        iv_old_data      TYPE abap_bool DEFAULT abap_false
      CHANGING
        ct_track_id_data TYPE lif_ef_types=>tt_enh_track_id_data
      RAISING
        cx_udm_message.

    CLASS-METHODS get_track_obj_changes
      IMPORTING
        is_app_object        TYPE trxas_appobj_ctab_wa
        iv_appsys            TYPE /saptrx/applsystem
        it_track_id_data_new TYPE lif_ef_types=>tt_enh_track_id_data
        it_track_id_data_old TYPE lif_ef_types=>tt_enh_track_id_data
      CHANGING
        ct_track_id_data     TYPE lif_ef_types=>tt_track_id_data
      RAISING
        cx_udm_message.
ENDCLASS.

CLASS lcl_tools IMPLEMENTATION.

  METHOD get_track_obj_changes.

    CONSTANTS: cs_mtr_truck       TYPE string VALUE '31',
               cs_trxcod_resource TYPE string VALUE 'RESOURCE'.

    DATA(lt_track_id_data_new) = it_track_id_data_new.
    DATA(lt_track_id_data_old) = it_track_id_data_old.

    LOOP AT lt_track_id_data_new ASSIGNING FIELD-SYMBOL(<ls_track_id_data>) WHERE trxcod = cs_trxcod_resource.
      READ TABLE lt_track_id_data_old WITH KEY key = <ls_track_id_data>-key ASSIGNING FIELD-SYMBOL(<ls_track_id_data_old>).
      IF sy-subrc = 0.
        DATA(lt_fields) = CAST cl_abap_structdescr(
                      cl_abap_typedescr=>describe_by_data(
                        p_data = <ls_track_id_data> )
                    )->get_included_view( ).

        DATA(lv_result) = abap_false.

        LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
          ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_track_id_data> TO FIELD-SYMBOL(<lv_value1>).
          ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_track_id_data_old> TO FIELD-SYMBOL(<lv_value2>).

          IF <lv_value1> IS ASSIGNED AND
             <lv_value2> IS ASSIGNED.
            IF <lv_value1> <> <lv_value2>.
              lv_result   = abap_true.
              EXIT.
            ENDIF.
          ENDIF.
        ENDLOOP.

        IF lv_result = abap_true.

          APPEND VALUE #( appsys      = iv_appsys
                          appobjtype  = is_app_object-appobjtype
                          appobjid    = is_app_object-appobjid
                          trxcod      = <ls_track_id_data>-trxcod
                          trxid       = <ls_track_id_data>-trxid
                          timzon      = lcl_tools=>get_system_time_zone( )
                          msrid       = space  ) TO ct_track_id_data.

          APPEND VALUE #( appsys      = iv_appsys
                          appobjtype  = is_app_object-appobjtype
                          appobjid    = is_app_object-appobjid
                          trxcod      = <ls_track_id_data_old>-trxcod
                          trxid       = <ls_track_id_data_old>-trxid
                          timzon      = lcl_tools=>get_system_time_zone( )
                          msrid       = space
                          action      = /scmtms/cl_scem_int_c=>sc_param_action-delete ) TO ct_track_id_data.
        ENDIF.

        DELETE lt_track_id_data_old WHERE key = <ls_track_id_data>-key.

      ELSE.
        APPEND VALUE #( appsys      = iv_appsys
                        appobjtype  = is_app_object-appobjtype
                        appobjid    = is_app_object-appobjid
                        trxcod      = <ls_track_id_data>-trxcod
                        trxid       = <ls_track_id_data>-trxid
                        timzon      = lcl_tools=>get_system_time_zone( )
                        msrid       = space ) TO ct_track_id_data.
      ENDIF.
    ENDLOOP.

    "Deleted resources
    LOOP AT lt_track_id_data_old ASSIGNING FIELD-SYMBOL(<ls_track_id_data_del>).
      APPEND VALUE #( appsys      = iv_appsys
                      appobjtype  = is_app_object-appobjtype
                      appobjid    = is_app_object-appobjid
                      trxcod      = <ls_track_id_data_del>-trxcod
                      trxid       = <ls_track_id_data_del>-trxid
                      timzon      = lcl_tools=>get_system_time_zone( )
                      msrid       = space
                      action      = /scmtms/cl_scem_int_c=>sc_param_action-delete ) TO ct_track_id_data.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_fo_tracked_item_obj.

    CONSTANTS: cs_mtr_truck       TYPE string VALUE '31',
               cs_trxcod_resource TYPE string VALUE 'RESOURCE'.

    LOOP AT it_item ASSIGNING FIELD-SYMBOL(<ls_item>).

      IF <ls_item>-platenumber IS ASSIGNED AND <ls_item>-res_id IS ASSIGNED AND <ls_item>-node_id IS ASSIGNED AND
         <ls_item>-item_cat    IS ASSIGNED AND <ls_item>-item_cat = /scmtms/if_tor_const=>sc_tor_item_category-av_item.

        IF is_root-tor_id IS NOT INITIAL AND <ls_item>-res_id IS NOT INITIAL.
          APPEND VALUE #( key = <ls_item>-node_id
                  appsys      = iv_appsys
                  appobjtype  = is_app_object-appobjtype
                  appobjid    = is_app_object-appobjid
                  trxcod      = cs_trxcod_resource
                  trxid       = |{ is_root-tor_id }{ <ls_item>-res_id }|
                  timzon      = lcl_tools=>get_system_time_zone( )
                  msrid       = space  ) TO ct_track_id_data.
        ENDIF.

        DATA(lv_mtr) = is_root-mtr.
        SELECT SINGLE motscode FROM /sapapo/trtype INTO lv_mtr WHERE ttype = lv_mtr.
        SHIFT lv_mtr LEFT DELETING LEADING '0'.
        IF is_root-tor_id IS NOT INITIAL AND <ls_item>-platenumber IS NOT INITIAL AND lv_mtr = cs_mtr_truck.
          APPEND VALUE #( key = |{ <ls_item>-node_id }P|
                  appsys      = iv_appsys
                  appobjtype  = is_app_object-appobjtype
                  appobjid    = is_app_object-appobjid
                  trxcod      = cs_trxcod_resource
                  trxid       = |{ is_root-tor_id }{ <ls_item>-platenumber }|
                  timzon      = lcl_tools=>get_system_time_zone( )
                  msrid       = space  ) TO ct_track_id_data.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD check_is_fo_deleted.

    rv_result = lif_ef_constants=>cs_condition-false.

    DATA(lv_carrier_removed) = xsdbool(
            is_root_old-tsp IS INITIAL AND is_root_new-tsp IS NOT INITIAL ).

    DATA(lv_execution_status_changed) = xsdbool(
      ( is_root_old-execution <> /scmtms/if_tor_status_c=>sc_root-execution-v_in_execution        AND
        is_root_old-execution <> /scmtms/if_tor_status_c=>sc_root-execution-v_ready_for_execution AND
        is_root_old-execution <> /scmtms/if_tor_status_c=>sc_root-execution-v_executed )          AND
      ( is_root_new-execution  = /scmtms/if_tor_status_c=>sc_root-execution-v_in_execution        OR
        is_root_new-execution  = /scmtms/if_tor_status_c=>sc_root-execution-v_ready_for_execution OR
        is_root_new-execution  = /scmtms/if_tor_status_c=>sc_root-execution-v_executed ) ).

    DATA(lv_lifecycle_status_changed) = xsdbool(
      ( is_root_old-lifecycle <> /scmtms/if_tor_status_c=>sc_root-lifecycle-v_in_process  AND
        is_root_old-lifecycle <> /scmtms/if_tor_status_c=>sc_root-lifecycle-v_completed ) AND
      ( is_root_new-lifecycle  = /scmtms/if_tor_status_c=>sc_root-lifecycle-v_in_process  OR
        is_root_new-lifecycle  = /scmtms/if_tor_status_c=>sc_root-lifecycle-v_completed ) ).

    IF lv_carrier_removed = abap_true OR lv_execution_status_changed = abap_true OR
       lv_lifecycle_status_changed = abap_true .
      rv_result = lif_ef_constants=>cs_condition-true.
    ENDIF.

  ENDMETHOD.

  METHOD get_capa_match_key.

    DATA lt_stop TYPE /scmtms/t_em_bo_tor_stop.

    ASSIGN it_capa_stop[ node_id = iv_assgn_stop_key ] TO FIELD-SYMBOL(<ls_capa_stop>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ASSIGN it_capa_root[ node_id = <ls_capa_stop>-parent_node_id ]-tor_id TO FIELD-SYMBOL(<lv_tor_id>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    /scmtms/cl_tor_helper_stop=>get_stop_sequence(
      EXPORTING
        it_root_key     = VALUE #( ( key = <ls_capa_stop>-parent_node_id ) )
        iv_before_image = abap_false
      IMPORTING
        et_stop_seq_d   = DATA(lt_stop_seq) ).

    ASSIGN lt_stop_seq[ root_key = <ls_capa_stop>-parent_node_id ] TO FIELD-SYMBOL(<ls_stop_seq>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lt_stop = CORRESPONDING /scmtms/t_em_bo_tor_stop( <ls_stop_seq>-stop_seq ).
    ASSIGN <ls_stop_seq>-stop_map[ stop_key = <ls_capa_stop>-node_id ]-tabix TO FIELD-SYMBOL(<lv_seq_num>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lcl_tools=>get_stop_points(
      EXPORTING
        iv_root_id     = <lv_tor_id>
        it_stop        = lt_stop
      IMPORTING
        et_stop_points = DATA(lt_stop_points) ).

    ASSIGN lt_stop_points[ seq_num   = <lv_seq_num>
                           log_locid = <ls_capa_stop>-log_locid ] TO FIELD-SYMBOL(<ls_stop_point>).
    IF sy-subrc = 0.
      rv_capa_matchkey = <ls_stop_point>-stop_id.
      SHIFT rv_capa_matchkey LEFT DELETING LEADING '0'.
    ENDIF.

  ENDMETHOD.

  METHOD is_odd.
    DATA lv_reminder TYPE n.
    lv_reminder = iv_value MOD 2.
    IF lv_reminder <> 0.
      rv_is_odd = abap_true.
    ELSE.
      rv_is_odd = abap_false.
    ENDIF.
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

      rv_result = cs_condition-false.

      LOOP AT lt_fields ASSIGNING FIELD-SYMBOL(<ls_fields>).
        ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_data1> TO <lv_value1>.
        ASSIGN COMPONENT <ls_fields>-name OF STRUCTURE <ls_data2> TO <lv_value2>.

        IF <lv_value1> IS ASSIGNED AND
           <lv_value2> IS ASSIGNED.
          IF <lv_value1> <> <lv_value2>.
            rv_result   = cs_condition-true.
            EXIT.
          ENDIF.
        ELSE.
          MESSAGE e001(zsst_gtt) WITH <ls_fields>-name INTO lv_dummy.
          lcl_tools=>throw_exception( ).
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zsst_gtt) INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_field_of_structure.
    DATA lv_dummy TYPE char100.

    FIELD-SYMBOLS: <ls_struct> TYPE any,
                   <lv_value>  TYPE any.

    ASSIGN ir_struct_data->* TO <ls_struct>.

    IF <ls_struct> IS ASSIGNED.
      ASSIGN COMPONENT iv_field_name OF STRUCTURE <ls_struct> TO <lv_value>.
      IF <lv_value> IS ASSIGNED.
        rv_value    = <lv_value>.
      ELSE.
        MESSAGE e001(zst_gtt) WITH iv_field_name INTO lv_dummy.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zsst_gtt) INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_errors_log.
    es_bapiret-id           = io_umd_message->m_msgid.
    es_bapiret-number       = io_umd_message->m_msgno.
    es_bapiret-type         = io_umd_message->m_msgty.
    es_bapiret-message_v1   = io_umd_message->m_msgv1.
    es_bapiret-message_v2   = io_umd_message->m_msgv2.
    es_bapiret-message_v3   = io_umd_message->m_msgv3.
    es_bapiret-message_v4   = io_umd_message->m_msgv4.
    es_bapiret-system       = iv_appsys.
  ENDMETHOD.

  METHOD get_local_timestamp.
    rv_timestamp    = COND #( WHEN iv_date IS NOT INITIAL
                                THEN |0{ iv_date }{ iv_time }| ).
  ENDMETHOD.

  METHOD get_pretty_value.
    rv_pretty   = COND #( WHEN lcl_tools=>is_number( iv_value = iv_value ) = abap_true
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

  METHOD is_table.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( p_data = iv_value ).

    rv_result = boolc( lo_type->type_kind = cl_abap_typedescr=>typekind_table ).
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

  METHOD get_stop_points.

    DATA lv_order(4) TYPE n VALUE '0001'.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>).
      IF NOT lcl_tools=>is_odd( <ls_stop>-seq_num ).
        lv_order += 1.
      ENDIF.
      APPEND VALUE #( stop_id   = |{ iv_root_id }{ lv_order }|
                      log_locid = <ls_stop>-log_locid
                      seq_num   = <ls_stop>-seq_num ) TO et_stop_points.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_postal_address.
    DATA(lo_tor_srv_mgr) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_tor_c=>sc_bo_key ).
    DATA(lo_loc_srv_mgr) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_location_c=>sc_bo_key ).

    lo_tor_srv_mgr->retrieve_by_association(
        EXPORTING
          iv_node_key    = /scmtms/if_tor_c=>sc_node-root
          it_key         = VALUE #( ( key = iv_node_id ) )
          iv_association = /scmtms/if_tor_c=>sc_association-root-stop
        IMPORTING
          et_target_key  = DATA(lt_stop_target_key) ).

    IF lt_stop_target_key IS NOT INITIAL.
      lo_tor_srv_mgr->retrieve_by_association(
        EXPORTING
          iv_node_key    = /scmtms/if_tor_c=>sc_node-stop
          it_key         = CORRESPONDING #( lt_stop_target_key )
          iv_association = /scmtms/if_tor_c=>sc_association-stop-bo_loc_log
        IMPORTING
          et_key_link    = DATA(lt_loc_log_key_link) ).

      IF lt_loc_log_key_link IS NOT INITIAL.
        lo_loc_srv_mgr->retrieve_by_association(
        EXPORTING
          iv_node_key    = /scmtms/if_location_c=>sc_node-root
          it_key         = CORRESPONDING #( lt_loc_log_key_link MAPPING key = target_key )
          iv_association = /scmtms/if_location_c=>sc_association-root-address
        IMPORTING
          et_key_link    = DATA(lt_address_key_link) ).

        IF lt_address_key_link IS NOT INITIAL.
          TRY.
              DATA(lr_bo_conf) = /bobf/cl_frw_factory=>get_configuration( iv_bo_key = /scmtms/if_location_c=>sc_bo_key ).
            CATCH /bobf/cx_frw.
              MESSAGE e011(zsst_gtt) INTO  DATA(lv_dummy).
              lcl_tools=>throw_exception( ).
          ENDTRY.

          DATA(lv_postal_ass_key) = lr_bo_conf->get_content_key_mapping(
                           iv_content_cat      = /bobf/if_conf_c=>sc_content_ass
                           iv_do_content_key   = /bofu/if_addr_constants=>sc_association-root-postal_address
                           iv_do_root_node_key = /scmtms/if_location_c=>sc_node-/bofu/address ).

          lo_loc_srv_mgr->retrieve_by_association(
            EXPORTING
              iv_node_key    = /scmtms/if_location_c=>sc_node-/bofu/address
              it_key         = CORRESPONDING #( lt_address_key_link MAPPING key = target_key )
              iv_association = lv_postal_ass_key
              iv_fill_data   = abap_true
            IMPORTING
              et_data        = et_postal_address ).
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_ef_performer DEFINITION.
  PUBLIC SECTION.

    TYPES: BEGIN OF ts_definition,
             maintab   TYPE /saptrx/strucdatadef,
             mastertab TYPE /saptrx/strucdatadef,
           END OF ts_definition.

    CLASS-METHODS check_relevance
      IMPORTING
        is_definition         TYPE ts_definition
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
    DATA lo_ef_processor TYPE REF TO lif_ef_processor.

    lo_ef_processor = io_bo_factory->get_ef_processor(
                          is_definition         = is_definition
                          io_bo_factory         = io_bo_factory
                          iv_appsys             = iv_appsys
                          is_app_obj_types      = is_app_obj_types
                          it_all_appl_tables    = it_all_appl_tables
                          it_app_type_cntl_tabs = it_app_type_cntl_tabs
                          it_app_objects        = it_app_objects ).

    lo_ef_processor->check_app_objects( ).

    rv_result = lo_ef_processor->check_relevance( io_bo_factory ).
  ENDMETHOD.

  METHOD get_control_data.

    DATA(lo_ef_processor) = io_bo_factory->get_ef_processor(
                                is_definition         = is_definition
                                io_bo_factory         = io_bo_factory
                                iv_appsys             = iv_appsys
                                is_app_obj_types      = is_app_obj_types
                                it_all_appl_tables    = it_all_appl_tables
                                it_app_type_cntl_tabs = it_app_type_cntl_tabs
                                it_app_objects        = it_app_objects ).

    lo_ef_processor->check_app_objects( ).

    lo_ef_processor->get_control_data(
      EXPORTING
        io_bo_factory   = io_bo_factory
      CHANGING
        ct_control_data = ct_control_data[] ).

  ENDMETHOD.

  METHOD get_planned_events.
    DATA lo_ef_processor TYPE REF TO lif_ef_processor.

    lo_ef_processor = io_factory->get_ef_processor(
                        is_definition         = is_definition
                        io_bo_factory         = io_factory
                        iv_appsys             = iv_appsys
                        is_app_obj_types      = is_app_obj_types
                        it_all_appl_tables    = it_all_appl_tables
                        it_app_type_cntl_tabs = it_app_type_cntl_tabs
                        it_app_objects        = it_app_objects ).

    lo_ef_processor->check_app_objects( ).

    lo_ef_processor->get_planned_events(
      EXPORTING
        io_factory      = io_factory
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata ).

  ENDMETHOD.

  METHOD get_track_id_data.
    DATA: lo_ef_processor   TYPE REF TO lif_ef_processor.

    CLEAR: et_track_id_data[].

    lo_ef_processor = io_bo_factory->get_ef_processor(
                        is_definition         = is_definition
                        io_bo_factory         = io_bo_factory
                        iv_appsys             = iv_appsys
                        is_app_obj_types      = is_app_obj_types
                        it_all_appl_tables    = it_all_appl_tables
                        it_app_type_cntl_tabs = it_app_type_cntl_tabs
                        it_app_objects        = it_app_objects ).

    lo_ef_processor->check_app_objects( ).

    lo_ef_processor->get_track_id_data(
      EXPORTING
        io_bo_factory    = io_bo_factory
      IMPORTING
        et_track_id_data = et_track_id_data ).

  ENDMETHOD.
ENDCLASS.


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

          CLEAR: ls_control_data-paramindex,
                 ls_control_data-value.

          IF <lv_value> IS ASSIGNED AND <lv_paramname> IS ASSIGNED.
            ls_control_data-paramname = <lv_paramname>.

            " simple copy for usual values
            IF lcl_tools=>is_table( iv_value = <lv_value> ) = abap_false.

              IF <lv_value> IS NOT INITIAL.
                ls_control_data-value     = lcl_tools=>get_pretty_value(
                                              iv_value = <lv_value> ).
              ENDIF.

              APPEND ls_control_data TO ct_control_data.

              " cycled copy for table values
            ELSE.
              ASSIGN <lv_value> TO <lt_value>.

              LOOP AT <lt_value> ASSIGNING <lv_value>.
                ADD 1 TO ls_control_data-paramindex.
                IF <lv_value> IS NOT INITIAL.
                  ls_control_data-value     = lcl_tools=>get_pretty_value(
                                                iv_value = <lv_value> ).
                ENDIF.
                APPEND ls_control_data TO ct_control_data.
                CLEAR: ls_control_data-value.
              ENDLOOP.
            ENDIF.
          ELSEIF <lv_value> IS NOT ASSIGNED.
            MESSAGE e010(zsst_gtt) INTO lv_dummy.
            lcl_tools=>throw_exception( ).
          ELSE.
            MESSAGE e010(zsst_gtt) INTO lv_dummy.
            lcl_tools=>throw_exception( ).
          ENDIF.
        ENDLOOP.
      ELSE.
        MESSAGE e010(zsst_gtt) INTO lv_dummy.
        lcl_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e010(zsst_gtt) INTO lv_dummy.
      lcl_tools=>throw_exception( ).
    ENDIF.
  ENDMETHOD.

  METHOD add_sys_attr_to_control_data.
    DATA: ls_control_data TYPE lif_ef_types=>ts_control_data,
          lv_tzone        TYPE timezone.

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

    FIELD-SYMBOLS <lt_app_objects>  TYPE trxas_appobj_ctabs.

    DATA(lr_app_objects) = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.
    rv_result = lif_ef_constants=>cs_condition-false.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>).
      TRY.
          mo_bo_reader = io_bo_factory->get_bo_reader(
                             is_appl_object   = <ls_app_objects>
                             io_ef_parameters = mo_ef_parameters ).
        CATCH cx_udm_message.
          CONTINUE.
      ENDTRY.
      rv_result = mo_bo_reader->check_relevance( is_app_object = <ls_app_objects> ).
      IF rv_result = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD lif_ef_processor~get_control_data.
    DATA: lr_app_objects TYPE REF TO data,
          lr_bo_data     TYPE REF TO data.

    FIELD-SYMBOLS: <lt_app_objects> TYPE trxas_appobj_ctabs,
                   <ls_app_objects> TYPE trxas_appobj_ctab_wa.

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING <ls_app_objects> WHERE maintabdef = ms_definition-maintab.

      mo_bo_reader = io_bo_factory->get_bo_reader(
                         is_appl_object   = <ls_app_objects>
                         io_ef_parameters = mo_ef_parameters ).

      lr_bo_data = mo_bo_reader->get_data( EXPORTING is_app_object = <ls_app_objects> ).

      add_struct_to_control_data(
        EXPORTING
          ir_bo_data      = lr_bo_data
          iv_appobjid     = <ls_app_objects>-appobjid
        CHANGING
          ct_control_data = ct_control_data ).

      add_sys_attr_to_control_data(
        EXPORTING
          iv_appobjid     = <ls_app_objects>-appobjid
        CHANGING
          ct_control_data = ct_control_data ).
    ENDLOOP.
  ENDMETHOD.

  METHOD lif_ef_processor~get_planned_events.
    DATA: lt_expeventdata TYPE lif_ef_types=>tt_expeventdata,
          lt_measrmntdata TYPE lif_ef_types=>tt_measrmntdata,
          lt_infodata     TYPE lif_ef_types=>tt_infodata,
          lr_app_objects  TYPE REF TO data.

    FIELD-SYMBOLS <lt_app_objects> TYPE trxas_appobj_ctabs.

    lr_app_objects  = mo_ef_parameters->get_app_objects( ).
    ASSIGN lr_app_objects->* TO <lt_app_objects>.

    LOOP AT <lt_app_objects> ASSIGNING FIELD-SYMBOL(<ls_app_objects>)
      WHERE maintabdef = ms_definition-maintab.

      mo_pe_filler = io_factory->get_pe_filler(
                          is_appl_object   = <ls_app_objects>
                          io_ef_parameters = mo_ef_parameters ).

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
      ct_expeventdata[] = VALUE #( BASE ct_expeventdata ( LINES OF lt_expeventdata ) ).
    ENDIF.
    IF lt_measrmntdata[] IS NOT INITIAL.
      ct_measrmntdata[] = VALUE #( BASE ct_measrmntdata ( LINES OF lt_measrmntdata ) ).
    ENDIF.
    IF lt_expeventdata[] IS NOT INITIAL.
      lt_infodata[] = VALUE #( BASE ct_infodata ( LINES OF lt_infodata ) ).
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

      mo_bo_reader = io_bo_factory->get_bo_reader(
                               is_appl_object   = <ls_app_objects>
                               io_ef_parameters = mo_ef_parameters ).

      mo_bo_reader->get_track_id_data(
        EXPORTING
          is_app_object    = <ls_app_objects>
        IMPORTING
          et_track_id_data = lt_track_id_data ).

      et_track_id_data  = VALUE #( BASE et_track_id_data ( LINES OF lt_track_id_data ) ).
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.

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
      mr_all_appl_tables    TYPE REF TO data,
      mr_app_type_cntl_tabs TYPE REF TO data,
      mr_app_objects        TYPE REF TO data.
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

    lo_ef_parameters = lif_factory~get_ef_parameters(
                           iv_appsys             = iv_appsys
                           is_app_obj_types      = is_app_obj_types
                           it_all_appl_tables    = it_all_appl_tables
                           it_app_type_cntl_tabs = it_app_type_cntl_tabs
                           it_app_objects        = it_app_objects ).

    ro_ef_processor = NEW lcl_ef_processor(
                              io_ef_parameters = lo_ef_parameters
                              io_bo_reader     = lo_bo_reader
                              io_pe_filler     = lo_pe_filler
                              is_definition    = is_definition ).
  ENDMETHOD.
ENDCLASS.
