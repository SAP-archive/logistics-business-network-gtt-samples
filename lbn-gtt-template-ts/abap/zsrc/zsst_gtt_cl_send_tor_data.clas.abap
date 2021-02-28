class ZSST_GTT_CL_SEND_TOR_DATA definition
  public
  final
  create public .

public section.

  interfaces /SCMTMS/IF_COMMON_BADI .
  interfaces /SCMTMS/IF_SEND_TOR_DATA .
  interfaces IF_BADI_INTERFACE .
protected section.
private section.

  types:
    BEGIN OF ty_tor_stop_address,
      stop_postal_addr_info          TYPE /bofu/s_addr_postal_addressk,
      formatted_postal_address_descr TYPE  /bofu/addr_langind_mdiumdesc,
    END OF ty_tor_stop_address .
  types:
    tt_tor_stop_address TYPE TABLE OF ty_tor_stop_address .

  methods GET_TOR_ROOT_FOR_DELETION
    importing
      !IT_TOR_ROOT_SSTRING type /SCMTMS/T_EM_BO_TOR_ROOT
      !IT_TOR_ROOT_BEFORE_SSTRING type /SCMTMS/T_EM_BO_TOR_ROOT
    exporting
      !ET_TOR_ROOT_FOR_DELETION type /SCMTMS/T_EM_BO_TOR_ROOT .
  methods SEND_DELETION_IDOC
    importing
      !IT_TOR_ROOT type /SCMTMS/T_EM_BO_TOR_ROOT
    raising
      /SCMTMS/CX_EVENT_MANAGEMENT .
  methods CALL_EVENT_MANAGER
    importing
      !IT_TOR_ROOT_FOR_DELETION type /SCMTMS/T_EM_BO_TOR_ROOT
      value(IT_TOR_ROOT_SSTRING) type /SCMTMS/T_EM_BO_TOR_ROOT
      value(IT_ITEM_SSTRING) type /SCMTMS/T_EM_BO_TOR_ITEM optional
      value(IT_STOP_SSTRING) type /SCMTMS/T_EM_BO_TOR_STOP optional
      value(IT_STOP_SUCC_SSTRING) type /SCMTMS/T_EM_BO_TOR_STOP_SUCC optional
      value(IT_PARTY_SSTRING) type /SCMTMS/T_EM_BO_TOR_PARTY optional
      value(IT_LOCATION_LOG_SSTRING) type /SCMTMS/T_EM_BO_LOC_ROOT optional
      value(IT_LOCATION_ADDR_SSTRING) type /SCMTMS/T_EM_BO_LOC_ROOT optional
      value(IT_TOR_ROOT_BEFORE_SSTRING) type /SCMTMS/T_EM_BO_TOR_ROOT optional
      value(IT_STOP_BEFORE_SSTRING) type /SCMTMS/T_EM_BO_TOR_STOP optional
      value(IT_STOP_ADDR_SSTRING) type /SCMTMS/T_EM_BO_LOC_ADDR optional
      value(IT_TRQ_ROOT_SSTRING) type /SCMTMS/T_EM_BO_TRQ_ROOT optional
      value(IT_TRQ_ROOT_BEFORE_SSTRING) type /SCMTMS/T_EM_BO_TRQ_ROOT optional
      value(IT_CAPA_ROOT_SSTRING) type /SCMTMS/T_EM_BO_TOR_ROOT optional
      value(IT_CAPA_ROOT_BEF_SSTRING) type /SCMTMS/T_EM_BO_TOR_ROOT optional
      value(IT_EXECINFO_SSTRING) type /SCMTMS/T_EM_BO_TOR_EXECINFO optional
      value(IT_EXECINFO_BEFORE_SSTRING) type /SCMTMS/T_EM_BO_TOR_EXECINFO optional
      value(IT_TENDERING_SSTRING) type /SCMTMS/T_EM_BO_TOR_TEND optional
      value(IT_ITEM_BEFORE_SSTRING) type /SCMTMS/T_EM_BO_TOR_ITEM optional
      value(IT_REQ_ROOT_SSTRING) type /SCMTMS/T_EM_BO_TOR_ROOT optional
      value(IT_REQ_ROOT_BEFORE_SSTRING) type /SCMTMS/T_EM_BO_TOR_ROOT optional
      value(IT_REQ_TU_ROOT_SSTRING) type /SCMTMS/T_EM_BO_TOR_ROOT optional
      value(IT_REQ_TU_ROOT_BEF_SSTRING) type /SCMTMS/T_EM_BO_TOR_ROOT optional
      value(IT_CAPA_STOP_SSTRING) type /SCMTMS/T_EM_BO_TOR_STOP optional
      value(IT_TOR_CREATED_KEYS) type /BOBF/T_FRW_KEY optional
      value(IT_REQ_STOP_SSTRING) type /SCMTMS/T_EM_BO_TOR_STOP optional
      value(IT_REQ_TU_STOP_SSTRING) type /SCMTMS/T_EM_BO_TOR_STOP optional
    raising
      /SCMTMS/CX_EVENT_MANAGEMENT .
  methods GET_STOP_POSTAL_ADDR_INFO
    importing
      !IT_STOP type /SCMTMS/T_TOR_STOP_K
    exporting
      value(ET_STOP_POSTAL_ADDR_INFO) type TT_TOR_STOP_ADDRESS .
ENDCLASS.



CLASS ZSST_GTT_CL_SEND_TOR_DATA IMPLEMENTATION.


  METHOD /scmtms/if_common_badi~set_badi_work_mode.

    DATA ls_work_mode TYPE /scmtms/s_badi_work_mode.

    ls_work_mode-intf_method_name = /scmtms/if_send_tor_data=>mc_intf_method_call_event_mgr.
    ls_work_mode-badi_work_mode   = /scmtms/if_common_badi=>gc_mode_customer_logic_only.
    INSERT ls_work_mode INTO TABLE ct_work_mode.

  ENDMETHOD.


  method /SCMTMS/IF_SEND_TOR_DATA~AVOID_RETRIEVAL_OF_APPL_TABLES.
  endmethod.


  METHOD /scmtms/if_send_tor_data~call_event_mgr.

    DATA(lo_send_delivery_idoc) = NEW  zsst_gtt_cl_send_delivery_idoc(
                                          it_tor_root_sstring        = it_tor_root_sstring
                                          it_tor_root_before_sstring = it_tor_root_before_sstring
                                          it_tor_item_sstring        = it_item_sstring
                                          it_tor_item_before_sstring = it_item_before_sstring
                                          it_tor_stop_addr_sstring   = it_stop_addr_sstring
                                          it_tor_stop_sstring        = it_stop_sstring ).
    lo_send_delivery_idoc->send_delivery_idoc( ).

    get_tor_root_for_deletion(
      EXPORTING
        it_tor_root_sstring        = it_tor_root_sstring
        it_tor_root_before_sstring = it_tor_root_before_sstring
      IMPORTING
        et_tor_root_for_deletion   = DATA(lt_tor_root_for_deletion) ).

    send_deletion_idoc( lt_tor_root_for_deletion ).

    call_event_manager(
      EXPORTING
        it_tor_root_for_deletion   = lt_tor_root_for_deletion
        it_tor_root_sstring        = it_tor_root_sstring
        it_item_sstring            = it_item_sstring
        it_stop_sstring            = it_stop_sstring
        it_stop_succ_sstring       = it_stop_succ_sstring
        it_party_sstring           = it_party_sstring
        it_location_log_sstring    = it_location_log_sstring
        it_location_addr_sstring   = it_location_addr_sstring
        it_tor_root_before_sstring = it_tor_root_before_sstring
        it_stop_before_sstring     = it_stop_before_sstring
        it_stop_addr_sstring       = it_stop_addr_sstring
        it_trq_root_sstring        = it_trq_root_sstring
        it_trq_root_before_sstring = it_trq_root_before_sstring
        it_capa_root_sstring       = it_capa_root_sstring
        it_capa_root_bef_sstring   = it_capa_root_bef_sstring
        it_execinfo_sstring        = it_execinfo_sstring
        it_execinfo_before_sstring = it_execinfo_before_sstring
        it_tendering_sstring       = it_tendering_sstring
        it_item_before_sstring     = it_item_before_sstring
        it_req_root_sstring        = it_req_root_sstring
        it_req_root_before_sstring = it_req_root_before_sstring
        it_req_tu_root_sstring     = it_req_tu_root_sstring
        it_req_tu_root_bef_sstring = it_req_tu_root_bef_sstring
        it_capa_stop_sstring       = it_capa_stop_sstring
        it_tor_created_keys        = it_tor_created_keys
        it_req_stop_sstring        = it_req_stop_sstring
        it_req_tu_stop_sstring     = it_req_tu_stop_sstring ).

  ENDMETHOD.


  METHOD /scmtms/if_send_tor_data~get_additional_data.

  ENDMETHOD.


  method /SCMTMS/IF_SEND_TOR_DATA~PREVENT_EVENT_MSG_SENDING.
  endmethod.


  METHOD CALL_EVENT_MANAGER.

    DATA:
      lt_tor_root_sstring_tmp        TYPE /scmtms/t_em_bo_tor_root,
      lt_item_sstring_tmp            TYPE /scmtms/t_em_bo_tor_item,
      lt_stop_sstring_tmp            TYPE /scmtms/t_em_bo_tor_stop,
      lt_stop_succ_sstring_tmp       TYPE /scmtms/t_em_bo_tor_stop_succ,
      lt_party_sstring_tmp           TYPE /scmtms/t_em_bo_tor_party,
      lt_location_log_sstring_tmp    TYPE /scmtms/t_em_bo_loc_root,
      lt_location_addr_sstring_tmp   TYPE /scmtms/t_em_bo_loc_root,
      lt_tor_root_before_sstring_tmp TYPE /scmtms/t_em_bo_tor_root,
      lt_stop_before_sstring_tmp     TYPE /scmtms/t_em_bo_tor_stop,
      lt_stop_addr_sstring_tmp       TYPE /scmtms/t_em_bo_loc_addr,
      lt_trq_root_sstring_tmp        TYPE /scmtms/t_em_bo_trq_root,
      lt_trq_root_before_sstring_tmp TYPE /scmtms/t_em_bo_trq_root,
      lt_capa_root_sstring_tmp       TYPE /scmtms/t_em_bo_tor_root,
      lt_capa_root_bef_sstring_tmp   TYPE /scmtms/t_em_bo_tor_root,
      lt_execinfo_sstring_tmp        TYPE /scmtms/t_em_bo_tor_execinfo,
      lt_execinfo_before_sstring_tmp TYPE /scmtms/t_em_bo_tor_execinfo,
      lt_tendering_sstring_tmp       TYPE /scmtms/t_em_bo_tor_tend,
      lt_item_before_sstring_tmp     TYPE /scmtms/t_em_bo_tor_item,
      lt_req_root_sstring_tmp        TYPE /scmtms/t_em_bo_tor_root,
      lt_req_root_before_sstring_tmp TYPE /scmtms/t_em_bo_tor_root,
      lt_req_tu_root_sstring_tmp     TYPE /scmtms/t_em_bo_tor_root,
      lt_req_tu_root_bef_sstring_tmp TYPE /scmtms/t_em_bo_tor_root,
      lt_req_stop_sstring_tmp        TYPE /scmtms/t_em_bo_tor_stop,
      lt_req_tu_stop_sstring_tmp     TYPE /scmtms/t_em_bo_tor_stop,
      lt_capa_stop_sstring_tmp       TYPE /scmtms/t_em_bo_tor_stop,
      lt_tabcont                     TYPE trxas_tabcontainer.

    SORT it_execinfo_sstring            BY parent_node_id event_code.
    SORT it_execinfo_before_sstring     BY parent_node_id event_code.

    IF it_tor_root_for_deletion IS INITIAL.
      lt_tor_root_sstring_tmp        = it_tor_root_sstring.
      lt_item_sstring_tmp            = it_item_sstring.
      lt_stop_sstring_tmp            = it_stop_sstring.
      lt_stop_succ_sstring_tmp       = it_stop_succ_sstring.
      lt_party_sstring_tmp           = it_party_sstring.
      lt_location_log_sstring_tmp    = it_location_log_sstring.
      lt_location_addr_sstring_tmp   = it_location_addr_sstring.
      lt_tor_root_before_sstring_tmp = it_tor_root_before_sstring.
      lt_stop_before_sstring_tmp     = it_stop_before_sstring.
      lt_stop_addr_sstring_tmp       = it_stop_addr_sstring.
      lt_trq_root_sstring_tmp        = it_trq_root_sstring.
      lt_trq_root_before_sstring_tmp = it_trq_root_before_sstring.
      lt_capa_root_sstring_tmp       = it_capa_root_sstring.
      lt_capa_root_bef_sstring_tmp   = it_capa_root_bef_sstring.
      lt_execinfo_sstring_tmp        = it_execinfo_sstring.
      lt_execinfo_before_sstring_tmp = it_execinfo_before_sstring.
      lt_tendering_sstring_tmp       = it_tendering_sstring.
      lt_item_before_sstring_tmp     = it_item_before_sstring.
      lt_req_root_sstring_tmp        = it_req_root_sstring.
      lt_req_root_before_sstring_tmp = it_req_root_before_sstring.
      lt_req_tu_root_sstring_tmp     = it_req_tu_root_sstring.
      lt_req_tu_root_bef_sstring_tmp = it_req_tu_root_bef_sstring.
      lt_req_stop_sstring_tmp        = it_req_stop_sstring.
      lt_req_tu_stop_sstring_tmp     = it_req_tu_stop_sstring.
      lt_capa_stop_sstring_tmp       = it_capa_stop_sstring.
    ELSE.
      SORT it_tor_root_sstring            BY node_id ASCENDING.
      SORT it_item_sstring                BY parent_node_id ASCENDING.
      SORT it_stop_sstring                BY parent_node_id ASCENDING.
      SORT it_stop_succ_sstring           BY parent_node_id ASCENDING.
      SORT it_party_sstring               BY parent_node_id ASCENDING.
      SORT it_tor_root_before_sstring     BY node_id ASCENDING.
      SORT it_stop_before_sstring         BY parent_node_id ASCENDING.
      SORT it_stop_addr_sstring           BY parent_node_id ASCENDING.
      SORT it_trq_root_sstring            BY tor_root_node ASCENDING.
      SORT it_trq_root_before_sstring     BY tor_root_node ASCENDING.
      SORT it_capa_root_sstring           BY tor_root_node ASCENDING.
      SORT it_capa_root_bef_sstring       BY tor_root_node ASCENDING.
      SORT it_execinfo_sstring            BY parent_node_id ASCENDING.
      SORT it_execinfo_before_sstring     BY parent_node_id ASCENDING.
      SORT it_tendering_sstring           BY parent_node_id ASCENDING.
      SORT it_item_before_sstring         BY parent_node_id ASCENDING.
      SORT it_req_root_sstring            BY node_id ASCENDING.
      SORT it_req_root_before_sstring     BY node_id ASCENDING.
      SORT it_req_tu_root_sstring         BY node_id ASCENDING.
      SORT it_req_tu_root_bef_sstring     BY node_id ASCENDING.
      SORT it_req_stop_sstring            BY parent_node_id.
      SORT it_req_tu_stop_sstring         BY parent_node_id.
      SORT it_capa_stop_sstring           BY node_id.

      lt_location_log_sstring_tmp   = it_location_log_sstring."-> send all
      lt_location_addr_sstring_tmp  = it_location_addr_sstring."-> send all
      lt_stop_addr_sstring_tmp      = it_stop_addr_sstring.

      LOOP AT it_tor_root_sstring ASSIGNING FIELD-SYMBOL(<ls_tor_root_sstring>).

        CHECK NOT line_exists( it_tor_root_for_deletion[ node_id = <ls_tor_root_sstring>-node_id ] ).

        APPEND <ls_tor_root_sstring> TO lt_tor_root_sstring_tmp.

        READ TABLE it_item_sstring WITH KEY parent_node_track_rel
          COMPONENTS parent_node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_item_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_item_sstring>).
          IF <ls_item_sstring>-parent_node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_item_sstring> TO lt_item_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_stop_sstring WITH KEY parent_seqnum
          COMPONENTS parent_node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_stop_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_stop_sstring>).
          IF <ls_stop_sstring>-parent_node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_stop_sstring> TO lt_stop_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_stop_succ_sstring WITH KEY parent_node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_stop_succ_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_stop_succ_sstring>).
          IF <ls_stop_succ_sstring>-parent_node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_stop_succ_sstring> TO lt_stop_succ_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_party_sstring WITH KEY parent_node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_party_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_party_sstring>).
          IF <ls_party_sstring>-parent_node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_party_sstring> TO lt_party_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_tor_root_before_sstring WITH KEY node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_tor_root_before_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_tor_root_before_sstring>).
          IF <ls_tor_root_before_sstring>-node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_tor_root_before_sstring> TO lt_tor_root_before_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_stop_before_sstring WITH KEY parent_seqnum
          COMPONENTS parent_node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_stop_before_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_stop_before_sstring>).
          IF <ls_stop_before_sstring>-parent_node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_stop_before_sstring> TO lt_stop_before_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_trq_root_sstring WITH KEY tor_root_node = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_trq_root_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_trq_root_sstring>).
          IF <ls_trq_root_sstring>-tor_root_node = <ls_tor_root_sstring>-node_id.
            APPEND <ls_trq_root_sstring> TO lt_trq_root_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_trq_root_before_sstring WITH KEY tor_root_node = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_trq_root_before_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_trq_root_before_sstring>).
          IF <ls_trq_root_before_sstring>-tor_root_node = <ls_tor_root_sstring>-node_id.
            APPEND <ls_trq_root_before_sstring> TO lt_trq_root_before_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_capa_root_sstring WITH KEY tor_root_node = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_capa_root_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_capa_root_sstring>).
          IF <ls_capa_root_sstring>-tor_root_node = <ls_tor_root_sstring>-node_id.
            APPEND <ls_capa_root_sstring> TO lt_capa_root_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_capa_root_bef_sstring WITH KEY tor_root_node = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_capa_root_bef_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_capa_root_before_sstring>).
          IF <ls_capa_root_before_sstring>-tor_root_node = <ls_tor_root_sstring>-node_id.
            APPEND <ls_capa_root_before_sstring> TO lt_capa_root_bef_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE  it_execinfo_sstring WITH KEY parent_node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_execinfo_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_execinfo_sstring>).
          IF <ls_execinfo_sstring>-parent_node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_execinfo_sstring> TO lt_execinfo_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_execinfo_before_sstring WITH KEY parent_node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_execinfo_before_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_execinfo_before_sstring>).
          IF <ls_execinfo_before_sstring>-parent_node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_execinfo_before_sstring> TO lt_execinfo_before_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_tendering_sstring WITH KEY parent_node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_tendering_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_tendering_sstring>).
          IF <ls_tendering_sstring>-parent_node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_tendering_sstring> TO lt_tendering_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_item_before_sstring WITH KEY parent_node_track_rel
          COMPONENTS parent_node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_item_before_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_item_before_sstring>).
          IF <ls_item_before_sstring>-parent_node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_item_before_sstring> TO lt_item_before_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_req_root_sstring WITH KEY tor_root_node = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_req_root_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_req_root_sstring>).
          IF <ls_req_root_sstring>-tor_root_node = <ls_tor_root_sstring>-node_id.
            APPEND <ls_req_root_sstring> TO lt_req_root_sstring_tmp.
            READ TABLE it_req_stop_sstring WITH KEY parent_seqnum
              COMPONENTS parent_node_id = <ls_req_root_sstring>-node_id TRANSPORTING NO FIELDS.
            LOOP AT it_req_stop_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_req_stop_sstring_tmp>).
              IF <ls_req_stop_sstring_tmp>-parent_node_id = <ls_req_root_sstring>-node_id.
                INSERT <ls_req_stop_sstring_tmp> INTO TABLE lt_req_stop_sstring_tmp.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_req_root_before_sstring WITH KEY tor_root_node = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_req_root_before_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_req_root_before_sstring>).
          IF <ls_req_root_before_sstring>-tor_root_node = <ls_tor_root_sstring>-node_id.
            APPEND <ls_req_root_before_sstring> TO lt_req_root_before_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_req_tu_root_sstring WITH KEY node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_req_tu_root_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_req_tu_root_sstring>).
          IF <ls_req_tu_root_sstring>-node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_req_tu_root_sstring> TO lt_req_tu_root_sstring_tmp.
            READ TABLE it_req_tu_stop_sstring WITH KEY parent_seqnum
              COMPONENTS parent_node_id = <ls_req_root_sstring>-node_id TRANSPORTING NO FIELDS.
            LOOP AT it_req_tu_stop_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_req_tu_stop_sstring_tmp>).
              IF <ls_req_tu_stop_sstring_tmp>-parent_node_id = <ls_req_root_sstring>-node_id.
                INSERT <ls_req_tu_stop_sstring_tmp> INTO TABLE lt_req_tu_stop_sstring_tmp.
              ELSE.
                EXIT.
              ENDIF.
            ENDLOOP.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

        READ TABLE it_req_tu_root_bef_sstring WITH KEY node_id = <ls_tor_root_sstring>-node_id TRANSPORTING NO FIELDS.
        LOOP AT it_req_tu_root_bef_sstring FROM sy-tabix ASSIGNING FIELD-SYMBOL(<ls_req_tu_root_before_sstr>).
          IF <ls_req_tu_root_before_sstr>-node_id = <ls_tor_root_sstring>-node_id.
            APPEND <ls_req_tu_root_before_sstr> TO lt_req_tu_root_bef_sstring_tmp.
          ELSE.
            EXIT.
          ENDIF.
        ENDLOOP.

      ENDLOOP. "LOOP AT lt_root_sstring_tmp
    ENDIF.

    CALL FUNCTION '/SAPTRX/EVENT_MGR_FILL_TABCONT'
      EXPORTING
        tabledef1          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-root
        tabledef2          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-item
        tabledef3          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop
        tabledef4          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop_successor
        tabledef5          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-party
        tabledef6          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-location_log
        tabledef7          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-location_addr
        tabledef8          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-root_before
        tabledef9          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop_before
        opt_init_container = 'X'
      TABLES
        table1             = lt_tor_root_sstring_tmp
        table2             = lt_item_sstring_tmp
        table3             = lt_stop_sstring_tmp
        table4             = lt_stop_succ_sstring_tmp
        table5             = lt_party_sstring_tmp
        table6             = lt_location_log_sstring_tmp
        table7             = lt_location_addr_sstring_tmp
        table8             = lt_tor_root_before_sstring_tmp
        table9             = lt_stop_before_sstring_tmp
      CHANGING
        table_container    = lt_tabcont.

    CALL FUNCTION '/SAPTRX/EVENT_MGR_FILL_TABCONT'
      EXPORTING
        tabledef1          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop_addr
        tabledef2          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_trq-root
        tabledef3          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_trq-root_before
        tabledef4          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-capa_root
        tabledef5          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-capa_root_before
        tabledef6          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-execinfo
        tabledef7          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-execinfo_before
        tabledef8          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-tendering
        tabledef9          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-item_before
        opt_init_container = ' '
      TABLES
        table1             = lt_stop_addr_sstring_tmp
        table2             = lt_trq_root_sstring_tmp
        table3             = lt_trq_root_before_sstring_tmp
        table4             = lt_capa_root_sstring_tmp
        table5             = lt_capa_root_bef_sstring_tmp
        table6             = lt_execinfo_sstring_tmp
        table7             = lt_execinfo_before_sstring_tmp
        table8             = lt_tendering_sstring_tmp
        table9             = lt_item_before_sstring_tmp
      CHANGING
        table_container    = lt_tabcont.

    CALL FUNCTION '/SAPTRX/EVENT_MGR_FILL_TABCONT'
      EXPORTING
        tabledef1          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-req_root
        tabledef2          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-req_root_before
        tabledef3          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-created_keys
        tabledef4          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-req_tu_root
        tabledef5          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-req_tu_root_before
        tabledef6          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-req_stop
        tabledef7          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-req_tu_stop
        tabledef8          = /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-capa_stop
        opt_init_container = ' '
      TABLES
        table1             = lt_req_root_sstring_tmp
        table2             = lt_req_root_before_sstring_tmp
        table3             = it_tor_created_keys
        table4             = lt_req_tu_root_sstring_tmp
        table5             = lt_req_tu_root_bef_sstring_tmp
        table6             = lt_req_stop_sstring_tmp
        table7             = lt_req_tu_stop_sstring_tmp
        table8             = lt_capa_stop_sstring_tmp
      CHANGING
        table_container    = lt_tabcont.

    IF lt_tabcont IS INITIAL.
      RETURN.
    ENDIF.

    CALL FUNCTION '/SAPTRX/EVENT_MGR_COMMUNICATE'
      EXPORTING
        business_process_type   = 'TMS_TOR'
        table_container         = lt_tabcont
      EXCEPTIONS
        parameter_error         = 1
        emr_determination_error = 2
        not_em_relevant         = 3
        log_system_error        = 4
        bpt_deactivated         = 5
        OTHERS                  = 6.
    DATA(lv_subrc) = sy-subrc.
    IF sy-subrc = 1.
      RAISE EXCEPTION TYPE /scmtms/cx_event_management
        EXPORTING
          textid = /scmtms/cx_event_management=>em_processing_parameter_err.
    ELSEIF sy-subrc = 4.
      RAISE EXCEPTION TYPE /scmtms/cx_event_management
        EXPORTING
          textid = /scmtms/cx_event_management=>em_processing_log_system.
    ELSEIF sy-subrc = 5.
      RAISE EXCEPTION TYPE /scmtms/cx_event_management
        EXPORTING
          textid = /scmtms/cx_event_management=>em_processing_bpt_deactivated.
    ELSEIF sy-subrc > 5.
      RAISE EXCEPTION TYPE /scmtms/cx_event_management
        EXPORTING
          textid  = /scmtms/cx_event_management=>em_processing_failed
          errcode = lv_subrc.
    ENDIF.

  ENDMETHOD.


  METHOD get_tor_root_for_deletion.

    DATA: lt_assigned_fu_k TYPE /scmtms/t_tor_root_k,
          lt_assigned_fu   TYPE /scmtms/t_em_bo_tor_root.

    DATA(lo_tor_srv_mgr) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( iv_bo_key = /scmtms/if_tor_c=>sc_bo_key ).

    CLEAR et_tor_root_for_deletion.

    LOOP AT it_tor_root_sstring ASSIGNING FIELD-SYMBOL(<ls_tor_root_sstring>).

      ASSIGN it_tor_root_before_sstring[ node_id = <ls_tor_root_sstring>-node_id ]
        TO FIELD-SYMBOL(<ls_tor_root_before_sstring>).
      CHECK sy-subrc = 0.

      DATA(lv_carrier_removed) = xsdbool(
        <ls_tor_root_sstring>-tsp IS INITIAL AND <ls_tor_root_before_sstring>-tsp IS NOT INITIAL ).

      DATA(lv_execution_status_changed) = xsdbool(
        ( <ls_tor_root_sstring>-execution <> /scmtms/if_tor_status_c=>sc_root-execution-v_in_execution        AND
          <ls_tor_root_sstring>-execution <> /scmtms/if_tor_status_c=>sc_root-execution-v_ready_for_execution AND
          <ls_tor_root_sstring>-execution <> /scmtms/if_tor_status_c=>sc_root-execution-v_executed ) AND
        ( <ls_tor_root_before_sstring>-execution = /scmtms/if_tor_status_c=>sc_root-execution-v_in_execution        OR
          <ls_tor_root_before_sstring>-execution = /scmtms/if_tor_status_c=>sc_root-execution-v_ready_for_execution OR
          <ls_tor_root_before_sstring>-execution = /scmtms/if_tor_status_c=>sc_root-execution-v_executed ) ).

      DATA(lv_lifecycle_status_changed) = xsdbool(
        ( <ls_tor_root_sstring>-lifecycle <> /scmtms/if_tor_status_c=>sc_root-lifecycle-v_in_process AND
          <ls_tor_root_sstring>-lifecycle <> /scmtms/if_tor_status_c=>sc_root-lifecycle-v_completed ) AND
        ( <ls_tor_root_before_sstring>-lifecycle = /scmtms/if_tor_status_c=>sc_root-lifecycle-v_in_process OR
          <ls_tor_root_before_sstring>-lifecycle = /scmtms/if_tor_status_c=>sc_root-lifecycle-v_completed ) ).

      CASE <ls_tor_root_sstring>-tor_cat.
        WHEN /scmtms/if_tor_const=>sc_tor_category-active OR
             /scmtms/if_tor_const=>sc_tor_category-booking.

          IF lv_carrier_removed = abap_true OR
             lv_execution_status_changed = abap_true OR
             lv_lifecycle_status_changed = abap_true.
            INSERT <ls_tor_root_sstring> INTO TABLE et_tor_root_for_deletion.
*
*            lo_tor_srv_mgr->retrieve_by_association(
*              EXPORTING
*                iv_node_key    = /scmtms/if_tor_c=>sc_node-root
*                it_key         = VALUE #( ( key = <ls_tor_root_sstring>-node_id ) )
*                iv_association = /scmtms/if_tor_c=>sc_association-root-assigned_fus
*                iv_fill_data   = abap_true
*              IMPORTING
*                et_data        = lt_assigned_fu_k ).
*            MOVE-CORRESPONDING lt_assigned_fu_k TO lt_assigned_fu.
*            APPEND LINES OF lt_assigned_fu TO et_tor_root_for_deletion.
          ENDIF.

        WHEN /scmtms/if_tor_const=>sc_tor_category-freight_unit.

          IF <ls_tor_root_sstring>-lifecycle = /scmtms/if_tor_status_c=>sc_root-lifecycle-v_canceled.
            INSERT <ls_tor_root_sstring> INTO TABLE et_tor_root_for_deletion.
          ENDIF.

*          DATA(lv_plan_status_changed) = xsdbool(
*          <ls_tor_root_sstring>-plan_status_root <> /scmtms/if_tor_status_c=>sc_root-planning-v_planned AND
*          <ls_tor_root_before_sstring>-plan_status_root = /scmtms/if_tor_status_c=>sc_root-planning-v_planned ).
*
*          IF lv_execution_status_changed = abap_true OR
*             lv_lifecycle_status_changed = abap_true OR
*             lv_plan_status_changed      = abap_true.
*            INSERT <ls_tor_root_sstring> INTO TABLE et_tor_root_for_deletion.
*          ENDIF.

        WHEN OTHERS.
          CONTINUE.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD send_deletion_idoc.
    IF it_tor_root IS INITIAL.
      RETURN.
    ENDIF.
    TRY.
        DATA(lo_send_deletion_idoc) = zsst_gtt_cl_send_deletion_idoc=>get_instance( it_tor_root ).
        lo_send_deletion_idoc->prepare_idoc_data( it_tor_root ).
        lo_send_deletion_idoc->send_idoc_data( ).
      CATCH cx_udm_message.
        RAISE EXCEPTION TYPE /scmtms/cx_event_management
          EXPORTING
            textid = /scmtms/cx_event_management=>em_processing_failed.
    ENDTRY.
  ENDMETHOD.


  METHOD get_stop_postal_addr_info.

    DATA:
      lv_nation                  TYPE ad_nation,
      lv_formatted1              TYPE ad_line_s,
      lv_do_root_addr_key        TYPE /bobf/obm_node_key,
      lv_do_root_postal_addr_asc TYPE /bobf/obm_assoc_key,
      ls_stop_postal_addr_info   TYPE ty_tor_stop_address,
      lt_location_key            TYPE /bobf/t_frw_key,
      lt_adr_location_key        TYPE /bobf/t_frw_key,
      lt_address_root            TYPE /bofu/t_addr_addressi_k,
      lt_target_key              TYPE /bobf/t_frw_key,
      lt_postal_address          TYPE /bofu/t_addr_postal_addressk,
      lr_address_root            TYPE REF TO /bofu/s_addr_addressi_k.

    CLEAR et_stop_postal_addr_info.

    DATA(lo_loc_srv_mgr) = /bobf/cl_tra_serv_mgr_factory=>get_service_manager(
                                                              iv_bo_key = /scmtms/if_location_c=>sc_bo_key ).

    LOOP AT it_stop ASSIGNING FIELD-SYMBOL(<ls_stop>).

      /scmtms/cl_common_helper=>check_insert_key(
        EXPORTING
          iv_key = <ls_stop>-log_loc_uuid
        CHANGING
          ct_key = lt_location_key ).

      IF <ls_stop>-adr_loc_uuid IS NOT INITIAL.
        /scmtms/cl_common_helper=>check_insert_key(
          EXPORTING
            iv_key = <ls_stop>-adr_loc_uuid
          CHANGING
            ct_key = lt_adr_location_key ).
      ENDIF.
    ENDLOOP.

    lo_loc_srv_mgr->retrieve_by_association(
      EXPORTING
        iv_node_key    = /scmtms/if_location_c=>sc_node-root
        it_key         = lt_location_key
        iv_association = /scmtms/if_location_c=>sc_association-root-address
        iv_fill_data   = abap_true
      IMPORTING
        et_data        = lt_address_root
        et_target_key  = lt_target_key ).

    /scmtms/cl_common_helper=>get_do_entity_key(
      EXPORTING
        iv_host_bo_key      = /scmtms/if_location_c=>sc_bo_key
        iv_host_do_node_key = /scmtms/if_location_c=>sc_node-/bofu/address
        iv_do_entity_key    = /bofu/if_addr_constants=>sc_node-root
      RECEIVING
        rv_entity_key       = lv_do_root_addr_key ).

    /scmtms/cl_common_helper=>get_do_keys_4_rba(
      EXPORTING
        iv_host_bo_key      = /scmtms/if_location_c=>sc_bo_key
        iv_host_do_node_key = /scmtms/if_location_c=>sc_node-/bofu/address
        iv_do_node_key      = /bofu/if_addr_constants=>sc_node-postal_address
        iv_do_assoc_key     = /bofu/if_addr_constants=>sc_association-root-postal_address
      IMPORTING
        ev_assoc_key        = lv_do_root_postal_addr_asc ).

    lo_loc_srv_mgr->retrieve_by_association(
      EXPORTING
        iv_node_key    = lv_do_root_addr_key
        it_key         = lt_target_key
        iv_association = lv_do_root_postal_addr_asc
        iv_fill_data   = abap_true
      IMPORTING
        et_data        = lt_postal_address ).

    CALL FUNCTION 'ADDR_LANGUAGE_MAP_TO_VERSION'
      EXPORTING
        iv_language              = sy-langu
        iv_application_component = 'BU_ADDRVERS_MAP'
      IMPORTING
        ev_nation                = lv_nation
      EXCEPTIONS
        OTHERS                   = 0.

    LOOP AT lt_postal_address REFERENCE INTO DATA(lr_postal_address).
      ls_stop_postal_addr_info-stop_postal_addr_info-root_key   = lr_postal_address->root_key.
      ls_stop_postal_addr_info-stop_postal_addr_info-parent_key = lr_postal_address->parent_key.
      ls_stop_postal_addr_info-stop_postal_addr_info-key        = lr_postal_address->parent_key.

      ls_stop_postal_addr_info-stop_postal_addr_info-s_data-country_code        = lr_postal_address->country_code.
      ls_stop_postal_addr_info-stop_postal_addr_info-s_data-region              = lr_postal_address->region.
      ls_stop_postal_addr_info-stop_postal_addr_info-s_data-city_name           = lr_postal_address->city_name.
      ls_stop_postal_addr_info-stop_postal_addr_info-s_data-street_postal_code  = lr_postal_address->street_postal_code.
      ls_stop_postal_addr_info-stop_postal_addr_info-s_data-street_name         = lr_postal_address->street_name.
      ls_stop_postal_addr_info-stop_postal_addr_info-s_data-house_id            = lr_postal_address->house_id.
      ls_stop_postal_addr_info-stop_postal_addr_info-s_data-street              = lr_postal_address->street.
      ls_stop_postal_addr_info-stop_postal_addr_info-s_data-building_id         = lr_postal_address->building_id.
      ls_stop_postal_addr_info-stop_postal_addr_info-s_data-time_zone_code      = lr_postal_address->time_zone_code.

      READ TABLE lt_address_root WITH KEY key = lr_postal_address->parent_key REFERENCE INTO lr_address_root.

      CALL FUNCTION 'ADDRESS_INTO_PRINTFORM'
        EXPORTING
          address_type               = '1'
          address_number             = lr_address_root->postal_address_id
          number_of_lines            = 3
          street_has_priority        = abap_true
          no_upper_case_for_city     = abap_true
          iv_nation                  = lv_nation
        IMPORTING
          address_short_form_wo_name = lv_formatted1.
      ls_stop_postal_addr_info-formatted_postal_address_descr = lv_formatted1.
      INSERT ls_stop_postal_addr_info INTO TABLE et_stop_postal_addr_info.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
