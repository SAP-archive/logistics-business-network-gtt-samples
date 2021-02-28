*&---------------------------------------------------------------------*
*& Local class definition - Actual Events Fillers
*&---------------------------------------------------------------------*

**********************************************************************
*** Planned Events of Freight Order ****************************
**********************************************************************
CLASS lcl_pe_filler DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pe_filler.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters.

    CONSTANTS:
      BEGIN OF cs_attribute,
        node_id TYPE string VALUE 'NODE_ID',
      END OF   cs_attribute.

  PROTECTED SECTION.
    DATA: mo_ef_parameters   TYPE REF TO lif_ef_parameters,
          mv_milestonecnt(4) TYPE n.

    METHODS get_data_for_planned_event
      IMPORTING
        is_app_objects TYPE trxas_appobj_ctab_wa
      EXPORTING
        ev_tor_id      TYPE /scmtms/tor_id
        et_stop        TYPE /scmtms/t_em_bo_tor_stop
        et_loc_address TYPE /bofu/t_addr_postal_addressk
        et_stop_points TYPE lif_ef_types=>tt_stop_points
      RAISING
        cx_udm_message.

    METHODS load_start
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id OPTIONAL
        it_stop         TYPE /scmtms/t_em_bo_tor_stop OPTIONAL
        it_loc_addr     TYPE /bofu/t_addr_postal_addressk
        it_stop_points  TYPE lif_ef_types=>tt_stop_points OPTIONAL
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS load_end
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id OPTIONAL
        it_stop         TYPE /scmtms/t_em_bo_tor_stop OPTIONAL
        it_loc_addr     TYPE /bofu/t_addr_postal_addressk
        it_stop_points  TYPE lif_ef_types=>tt_stop_points OPTIONAL
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS coupling
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id
        it_stop         TYPE /scmtms/t_em_bo_tor_stop
        it_loc_addr     TYPE /bofu/t_addr_postal_addressk
        it_stop_points  TYPE lif_ef_types=>tt_stop_points
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS decoupling
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id
        it_stop         TYPE /scmtms/t_em_bo_tor_stop
        it_loc_addr     TYPE /bofu/t_addr_postal_addressk
        it_stop_points  TYPE lif_ef_types=>tt_stop_points
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS unload_start
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id OPTIONAL
        it_stop         TYPE /scmtms/t_em_bo_tor_stop OPTIONAL
        it_loc_addr     TYPE /bofu/t_addr_postal_addressk
        it_stop_points  TYPE lif_ef_types=>tt_stop_points OPTIONAL
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS unload_end
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id OPTIONAL
        it_stop         TYPE /scmtms/t_em_bo_tor_stop  OPTIONAL
        it_loc_addr     TYPE /bofu/t_addr_postal_addressk
        it_stop_points  TYPE lif_ef_types=>tt_stop_points OPTIONAL
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS shp_arrival
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id OPTIONAL
        it_stop         TYPE /scmtms/t_em_bo_tor_stop OPTIONAL
        it_loc_addr     TYPE /bofu/t_addr_postal_addressk
        it_stop_points  TYPE lif_ef_types=>tt_stop_points OPTIONAL
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS shp_departure
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id OPTIONAL
        it_stop         TYPE /scmtms/t_em_bo_tor_stop OPTIONAL
        it_loc_addr     TYPE /bofu/t_addr_postal_addressk
        it_stop_points  TYPE lif_ef_types=>tt_stop_points OPTIONAL
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS pod
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id OPTIONAL
        it_stop         TYPE /scmtms/t_em_bo_tor_stop OPTIONAL
        it_loc_addr     TYPE /bofu/t_addr_postal_addressk
        it_stop_points  TYPE lif_ef_types=>tt_stop_points OPTIONAL
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_pe_filler IMPLEMENTATION.

  METHOD get_data_for_planned_event.

    DATA: lv_tor_node_id    TYPE /scmtms/bo_node_id.

    ev_tor_id = lcl_tools=>get_field_of_structure(
                               ir_struct_data = is_app_objects-maintabref
                               iv_field_name  = /scmtms/if_tor_c=>sc_node_attribute-root-tor_id ).
    SHIFT ev_tor_id LEFT DELETING LEADING '0'.

    lv_tor_node_id = lcl_tools=>get_field_of_structure(
                                          ir_struct_data = is_app_objects-maintabref
                                          iv_field_name  = cs_attribute-node_id ).

    IF et_stop IS SUPPLIED.

      /scmtms/cl_tor_helper_stop=>get_stop_sequence(
        EXPORTING
          it_root_key     = VALUE #( ( key = lv_tor_node_id ) )
          iv_before_image = abap_false
        IMPORTING
          et_stop_seq_d   = DATA(lt_stop_seq) ).

      ASSIGN lt_stop_seq[ root_key = lv_tor_node_id ] TO FIELD-SYMBOL(<ls_stop_seq>).
      IF sy-subrc = 0.
        MOVE-CORRESPONDING <ls_stop_seq>-stop_seq TO et_stop.
        LOOP AT et_stop ASSIGNING FIELD-SYMBOL(<ls_stop>).
          <ls_stop>-parent_node_id = lv_tor_node_id.
          ASSIGN <ls_stop_seq>-stop_map[ tabix = <ls_stop>-seq_num ]-stop_key TO FIELD-SYMBOL(<lv_stop_key>).
          CHECK sy-subrc = 0.
          <ls_stop>-node_id = <lv_stop_key>.
        ENDLOOP.
      ENDIF.
    ENDIF.

    IF et_loc_address IS SUPPLIED.
      lcl_tools=>get_postal_address(
        EXPORTING
          iv_node_id        = lv_tor_node_id
        IMPORTING
          et_postal_address = et_loc_address
      ).
    ENDIF.

    IF et_stop_points IS SUPPLIED.
      lcl_tools=>get_stop_points(
        EXPORTING
          iv_root_id     = ev_tor_id
          it_stop        = et_stop
        IMPORTING
          et_stop_points = et_stop_points ).
    ENDIF.
  ENDMETHOD.

  METHOD pod.

    DATA: ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.
    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>) WHERE parent_node_id = <ls_root>-node_id.

      CHECK <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound AND
            <ls_stop>-aggr_assgn_start_l IS NOT INITIAL AND
            <ls_stop>-aggr_assgn_end_l   IS NOT INITIAL.

      READ TABLE it_stop_points WITH KEY log_locid = <ls_stop>-log_locid
                                         seq_num   = <ls_stop>-seq_num REFERENCE INTO DATA(ls_stop_points).
      CHECK sy-subrc = 0.

      READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr WITH KEY root_key = <ls_stop>-log_loc_uuid.
      CHECK sy-subrc = 0.
      IF ls_loc_addr->time_zone_code IS NOT INITIAL.
        CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_l
        TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
        DATA(lv_tz) = ls_loc_addr->time_zone_code.
      ELSE.
        CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_l
        TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
        lv_tz = sy-zonlo.
      ENDIF.
      mv_milestonecnt += 1.
      APPEND VALUE #( appsys           = mo_ef_parameters->get_appsys(  )
                      appobjtype       = mo_ef_parameters->get_app_obj_types( )-aotype
                      language         = sy-langu
                      appobjid         = is_app_objects-appobjid
                      milestone        = lif_sst_constants=>cs_milestone-fo_shp_pod
                      evt_exp_datetime = |0{ lv_conv_date }{ lv_conv_time }|
                      evt_exp_tzone    = lv_tz
                      locid1           = <ls_stop>-log_locid
                      locid2           = ls_stop_points->stop_id
                      loctype          = lif_sst_constants=>cs_location_type-logistic
                      country          = ls_loc_addr->country_code
                      city             = ls_loc_addr->city_name ) TO ct_expeventdata.
    ENDLOOP.

  ENDMETHOD.

  METHOD load_start.

    DATA: ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk,
          lv_seq_num  TYPE /scmtms/seq_num.

    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>) WHERE parent_node_id = <ls_root>-node_id.

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-outbound
         AND <ls_stop>-aggr_assgn_start_l IS NOT INITIAL
         AND <ls_stop>-aggr_assgn_end_l IS NOT INITIAL.

        lv_seq_num = COND #( WHEN sy-tabix > 1 THEN <ls_stop>-seq_num - 1
                                               ELSE <ls_stop>-seq_num ).

        READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                       WITH KEY log_locid = <ls_stop>-log_locid
                                                seq_num   = lv_seq_num.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                       WITH KEY root_key = <ls_stop>-log_loc_uuid.

        IF sy-subrc IS INITIAL.
          IF ls_loc_addr->time_zone_code IS NOT INITIAL.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_l
             TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
            DATA(lv_tz) = ls_loc_addr->time_zone_code.
          ELSE.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_l
               TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
            lv_tz = sy-zonlo.
          ENDIF.
          mv_milestonecnt += 1.
          APPEND VALUE #( appsys            = mo_ef_parameters->get_appsys(  )
                          appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                          language          = sy-langu
                          appobjid          = is_app_objects-appobjid
                          milestone         = lif_sst_constants=>cs_milestone-fo_load_start
                          evt_exp_datetime  = |{ lv_conv_date }{ lv_conv_time }|
                          evt_exp_tzone     = lv_tz
                          locid1            = <ls_stop>-log_locid
                          locid2            = ls_stop_points->stop_id
                          loctype           = lif_sst_constants=>cs_location_type-logistic
                          country           = ls_loc_addr->country_code
                          city              = ls_loc_addr->city_name ) TO ct_expeventdata.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD load_end.

    DATA: ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk,
          lv_seq_num  TYPE /scmtms/seq_num.

    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>) WHERE parent_node_id = <ls_root>-node_id.

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-outbound
         AND <ls_stop>-aggr_assgn_start_l IS NOT INITIAL
         AND <ls_stop>-aggr_assgn_end_l IS NOT INITIAL.


        lv_seq_num = COND #( WHEN sy-tabix > 1 THEN <ls_stop>-seq_num - 1
                                               ELSE <ls_stop>-seq_num ).

        READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                       WITH KEY log_locid = <ls_stop>-log_locid
                                                seq_num   = lv_seq_num.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                       WITH KEY root_key = <ls_stop>-log_loc_uuid.

        IF sy-subrc IS INITIAL.
          IF ls_loc_addr->time_zone_code IS NOT INITIAL.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_end_l
             TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
            DATA(lv_tz) = ls_loc_addr->time_zone_code.
          ELSE.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_end_l
               TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
            lv_tz = sy-zonlo.
          ENDIF.
          mv_milestonecnt += 1.
          APPEND VALUE #(
          appsys            = mo_ef_parameters->get_appsys(  )
          appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
          language          = sy-langu
          appobjid          = is_app_objects-appobjid
          milestone         = lif_sst_constants=>cs_milestone-fo_load_end
          evt_exp_datetime  = |0{ lv_conv_date }{ lv_conv_time }|
          evt_exp_tzone     = lv_tz
          locid1            = <ls_stop>-log_locid
          locid2            = ls_stop_points->stop_id
          loctype           = lif_sst_constants=>cs_location_type-logistic
          country           = ls_loc_addr->country_code
          city              = ls_loc_addr->city_name
          itemident         = <ls_stop>-node_id
         ) TO ct_expeventdata.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD coupling.

    DATA: ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk,
          lv_seq_num  TYPE /scmtms/seq_num.

    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>) WHERE parent_node_id = <ls_root>-node_id.

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-outbound
         AND <ls_stop>-aggr_assgn_start_c IS NOT INITIAL
         AND <ls_stop>-aggr_assgn_end_c IS NOT INITIAL.


        lv_seq_num = COND #( WHEN sy-tabix > 1 THEN <ls_stop>-seq_num - 1
                                               ELSE <ls_stop>-seq_num ).

        READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                       WITH KEY log_locid = <ls_stop>-log_locid
                                                seq_num   = lv_seq_num.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                       WITH KEY root_key = <ls_stop>-log_loc_uuid.

        IF sy-subrc IS INITIAL.
          IF ls_loc_addr->time_zone_code IS NOT INITIAL.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_end_c
             TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
            DATA(lv_tz) = ls_loc_addr->time_zone_code.
          ELSE.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_end_c
               TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
            lv_tz = sy-zonlo.
          ENDIF.

          IF <ls_stop>-aggr_assgn_start_c IS NOT INITIAL.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_c
            TIME ZONE lv_tz INTO DATE DATA(lv_conv_date1) TIME DATA(lv_conv_time1).
          ENDIF.
          mv_milestonecnt += 1.
          APPEND VALUE #(
          appsys               = mo_ef_parameters->get_appsys(  )
          appobjtype           = mo_ef_parameters->get_app_obj_types( )-aotype
          language             = sy-langu
          appobjid             = is_app_objects-appobjid
          milestone            = lif_sst_constants=>cs_milestone-fo_coupling
          evt_exp_datetime     = |0{ lv_conv_date }{ lv_conv_time }|
          evt_er_exp_dtime     = |0{ lv_conv_date1 }{ lv_conv_time1 }|
          evt_exp_tzone        = lv_tz
          locid1               = <ls_stop>-log_locid
          locid2               = ls_stop_points->stop_id
          loctype              = lif_sst_constants=>cs_location_type-logistic
          country              = ls_loc_addr->country_code
          city                 = ls_loc_addr->city_name
         ) TO ct_expeventdata.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD decoupling.

    DATA: ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.
    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>) WHERE parent_node_id = <ls_root>-node_id.

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound
         AND <ls_stop>-aggr_assgn_start_c IS NOT INITIAL
         AND <ls_stop>-aggr_assgn_end_c   IS NOT INITIAL.

        READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                       WITH KEY log_locid = <ls_stop>-log_locid
                                                seq_num   = <ls_stop>-seq_num.
        READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                       WITH KEY root_key = <ls_stop>-log_loc_uuid.

        IF sy-subrc IS INITIAL.

          IF ls_loc_addr->time_zone_code IS NOT INITIAL.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_end_c
             TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
            DATA(lv_tz) = ls_loc_addr->time_zone_code.
          ELSE.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_end_c
               TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
            lv_tz = sy-zonlo.
          ENDIF.

          IF <ls_stop>-aggr_assgn_start_c IS NOT INITIAL.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_c
            TIME ZONE lv_tz INTO DATE DATA(lv_conv_date1) TIME DATA(lv_conv_time1).
          ENDIF.

          mv_milestonecnt += 1.
          APPEND VALUE #(
          appsys               = mo_ef_parameters->get_appsys(  )
          appobjtype           = mo_ef_parameters->get_app_obj_types( )-aotype
          language             = sy-langu
          appobjid             = is_app_objects-appobjid
          milestone            = lif_sst_constants=>cs_milestone-fo_decoupling
          evt_exp_datetime     = |0{ lv_conv_date }{ lv_conv_time }|
          evt_er_exp_dtime     = |0{ lv_conv_date1 }{ lv_conv_time1 }|
          evt_exp_tzone        = lv_tz
          locid1               = <ls_stop>-log_locid
          locid2               = ls_stop_points->stop_id
          loctype              = lif_sst_constants=>cs_location_type-logistic
          country              = ls_loc_addr->country_code
          city                 = ls_loc_addr->city_name
         ) TO ct_expeventdata.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD unload_start.

    DATA: ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.

    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>) WHERE parent_node_id = <ls_root>-node_id.

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound
         AND <ls_stop>-aggr_assgn_start_l IS NOT INITIAL
         AND <ls_stop>-aggr_assgn_end_l   IS NOT INITIAL.

        READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                       WITH KEY log_locid = <ls_stop>-log_locid
                                                seq_num   = <ls_stop>-seq_num.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                       WITH KEY root_key = <ls_stop>-log_loc_uuid.

        IF sy-subrc IS INITIAL.
          IF ls_loc_addr->time_zone_code IS NOT INITIAL.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_l
             TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
            DATA(lv_tz) = ls_loc_addr->time_zone_code.
          ELSE.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_l
               TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
            lv_tz = sy-zonlo.
          ENDIF.
          mv_milestonecnt += 1.
          APPEND VALUE #(
          appsys            = mo_ef_parameters->get_appsys(  )
          appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
          language          = sy-langu
          appobjid          = is_app_objects-appobjid
          milestone         = lif_sst_constants=>cs_milestone-fo_unload_start
          evt_exp_datetime  = |0{ lv_conv_date }{ lv_conv_time }|
          evt_exp_tzone     = lv_tz
          locid1            = <ls_stop>-log_locid
          locid2            = ls_stop_points->stop_id
          loctype           = lif_sst_constants=>cs_location_type-logistic
          country           = ls_loc_addr->country_code
          city              = ls_loc_addr->city_name
         ) TO ct_expeventdata.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD unload_end.

    DATA: ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.

    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>) WHERE parent_node_id = <ls_root>-node_id.

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound
         AND <ls_stop>-aggr_assgn_start_l IS NOT INITIAL
         AND <ls_stop>-aggr_assgn_end_l   IS NOT INITIAL.

        READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                       WITH KEY log_locid = <ls_stop>-log_locid
                                                seq_num   = <ls_stop>-seq_num.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                       WITH KEY root_key = <ls_stop>-log_loc_uuid.

        IF sy-subrc IS INITIAL.
          IF ls_loc_addr->time_zone_code IS NOT INITIAL.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_l
             TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
            DATA(lv_tz) = ls_loc_addr->time_zone_code.
          ELSE.
            CONVERT TIME STAMP <ls_stop>-aggr_assgn_start_l
               TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
            lv_tz = sy-zonlo.
          ENDIF.
          mv_milestonecnt += 1.
          APPEND VALUE #(
          appsys            = mo_ef_parameters->get_appsys(  )
          appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
          language          = sy-langu
          appobjid          = is_app_objects-appobjid
          milestone         = lif_sst_constants=>cs_milestone-fo_unload_end
          evt_exp_datetime  = |0{ lv_conv_date }{ lv_conv_time }|
          evt_exp_tzone     = lv_tz
          locid1            = <ls_stop>-log_locid
          locid2            = ls_stop_points->stop_id
          loctype           = lif_sst_constants=>cs_location_type-logistic
          country           = ls_loc_addr->country_code
          city              = ls_loc_addr->city_name
         ) TO ct_expeventdata.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD shp_arrival.

    DATA: ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.

    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>) WHERE parent_node_id = <ls_root>-node_id.

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound
         AND <ls_stop>-plan_trans_time IS NOT INITIAL.

        READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                       WITH KEY log_locid = <ls_stop>-log_locid
                                                seq_num   = <ls_stop>-seq_num.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                       WITH KEY root_key = <ls_stop>-log_loc_uuid.

        IF sy-subrc IS INITIAL.
          IF ls_loc_addr->time_zone_code IS NOT INITIAL.
            CONVERT TIME STAMP <ls_stop>-plan_trans_time
             TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
            DATA(lv_tz) = ls_loc_addr->time_zone_code.
          ELSE.
            CONVERT TIME STAMP <ls_stop>-plan_trans_time
               TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
            lv_tz = sy-zonlo.
          ENDIF.
          mv_milestonecnt += 1.
          APPEND VALUE #(
          appsys            = mo_ef_parameters->get_appsys(  )
          appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
          language          = sy-langu
          appobjid          = is_app_objects-appobjid
          milestone         = lif_sst_constants=>cs_milestone-fo_shp_arrival
          evt_exp_datetime  = |0{ lv_conv_date }{ lv_conv_time }|
          evt_exp_tzone     = lv_tz
          locid1            = <ls_stop>-log_locid
          locid2            = ls_stop_points->stop_id
          loctype           = lif_sst_constants=>cs_location_type-logistic
          country           = ls_loc_addr->country_code
          city              = ls_loc_addr->city_name
         ) TO ct_expeventdata.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD shp_departure.

    DATA: ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk,
          lv_seq_num  TYPE /scmtms/seq_num.

    FIELD-SYMBOLS <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>) WHERE parent_node_id = <ls_root>-node_id.

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-outbound.
        IF <ls_stop>-plan_trans_time IS NOT INITIAL.

          lv_seq_num = COND #( WHEN sy-tabix > 1 THEN <ls_stop>-seq_num - 1
                                                 ELSE <ls_stop>-seq_num ).

          READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                         WITH KEY log_locid = <ls_stop>-log_locid
                                                  seq_num   = lv_seq_num.
          IF sy-subrc IS NOT INITIAL.
            CONTINUE.
          ENDIF.

          READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                         WITH KEY root_key = <ls_stop>-log_loc_uuid.

          IF sy-subrc IS INITIAL.
            IF ls_loc_addr->time_zone_code IS NOT INITIAL.
              CONVERT TIME STAMP <ls_stop>-plan_trans_time
               TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
              DATA(lv_tz) = ls_loc_addr->time_zone_code.
            ELSE.
              CONVERT TIME STAMP <ls_stop>-plan_trans_time
                 TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
              lv_tz = sy-zonlo.
            ENDIF.
            mv_milestonecnt += 1.

            APPEND VALUE #(
            appsys            = mo_ef_parameters->get_appsys(  )
            appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
            language          = sy-langu
            appobjid          = is_app_objects-appobjid
            milestone         = lif_sst_constants=>cs_milestone-fo_shp_departure
            evt_exp_datetime  = |0{ lv_conv_date }{ lv_conv_time }|
            evt_exp_tzone     = lv_tz
            locid1            = <ls_stop>-log_locid
            locid2            = ls_stop_points->stop_id
            loctype           = lif_sst_constants=>cs_location_type-logistic
            country           = ls_loc_addr->country_code
            city              = ls_loc_addr->city_name
           ) TO ct_expeventdata.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD constructor.
    mo_ef_parameters = io_ef_parameters.
    mv_milestonecnt  = 0.
  ENDMETHOD.

  METHOD lif_pe_filler~get_planed_events.
    RETURN.
  ENDMETHOD.
ENDCLASS.

CLASS lcl_pe_freight_order_filler DEFINITION INHERITING FROM lcl_pe_filler.
  PUBLIC SECTION.
    METHODS lif_pe_filler~get_planed_events REDEFINITION.
ENDCLASS.

CLASS lcl_pe_freight_order_filler IMPLEMENTATION.

  METHOD lif_pe_filler~get_planed_events.

    get_data_for_planned_event(
      EXPORTING
        is_app_objects = is_app_objects
      IMPORTING
        ev_tor_id      = DATA(lv_tor_id)
        et_stop        = DATA(lt_stop)
        et_loc_address = DATA(lt_loc_address)
        et_stop_points = DATA(lt_stop_points) ).

    load_start(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    load_end(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    unload_start(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    unload_end(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    shp_arrival(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    shp_departure(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    coupling(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    decoupling(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    pod(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

  ENDMETHOD.
ENDCLASS.

CLASS lcl_pe_freight_booking_filler DEFINITION INHERITING FROM lcl_pe_filler.
  PUBLIC SECTION.
    METHODS lif_pe_filler~get_planed_events REDEFINITION.
ENDCLASS.

CLASS lcl_pe_freight_booking_filler IMPLEMENTATION.

  METHOD lif_pe_filler~get_planed_events.

    get_data_for_planned_event(
      EXPORTING
        is_app_objects = is_app_objects
      IMPORTING
        ev_tor_id      = DATA(lv_tor_id)
        et_stop        = DATA(lt_stop)
        et_loc_address = DATA(lt_loc_address)
        et_stop_points = DATA(lt_stop_points) ).

    load_start(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    load_end(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    unload_start(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    unload_end(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    shp_arrival(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    shp_departure(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    pod(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = lt_loc_address
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

  ENDMETHOD.
ENDCLASS.

CLASS lcl_pe_freight_unit_filler DEFINITION INHERITING FROM lcl_pe_filler.

  PUBLIC SECTION.
    METHODS lif_pe_filler~get_planed_events REDEFINITION.
    METHODS get_capa_matchkey
      IMPORTING
        iv_assgn_stop_key   TYPE /scmtms/tor_stop_key
      RETURNING
        VALUE(rv_match_key) TYPE /saptrx/loc_id_2
      RAISING
        cx_udm_message.

  PROTECTED SECTION.

    METHODS load_start    REDEFINITION.
    METHODS load_end      REDEFINITION.
    METHODS unload_start  REDEFINITION.
    METHODS unload_end    REDEFINITION.
    METHODS shp_arrival   REDEFINITION.
    METHODS shp_departure REDEFINITION.
    METHODS pod           REDEFINITION.

ENDCLASS.

CLASS lcl_pe_freight_unit_filler IMPLEMENTATION.

  METHOD get_capa_matchkey.

    FIELD-SYMBOLS:
      <lt_capa_root> TYPE /scmtms/t_em_bo_tor_root,
      <lt_capa_stop> TYPE /scmtms/t_em_bo_tor_stop.

    DATA(lr_capa_root) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-capa_root ).
    ASSIGN lr_capa_root->* TO <lt_capa_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_capa_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-capa_stop ).
    ASSIGN lr_capa_stop->* TO <lt_capa_stop>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rv_match_key = lcl_tools=>get_capa_match_key(
                        iv_assgn_stop_key = iv_assgn_stop_key
                        it_capa_stop      = <lt_capa_stop>
                        it_capa_root      = <lt_capa_root> ).
  ENDMETHOD.

  METHOD load_start.

    FIELD-SYMBOLS:
      <lt_stop> TYPE /scmtms/t_em_bo_tor_stop,
      <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop ).
    ASSIGN lr_stop->* TO <lt_stop>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT <lt_stop> ASSIGNING FIELD-SYMBOL(<ls_stop>) USING KEY parent_seqnum WHERE parent_node_id = <ls_root>-node_id.

      CHECK <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-outbound AND <ls_stop>-assgn_start IS NOT INITIAL.

      READ TABLE it_loc_addr ASSIGNING FIELD-SYMBOL(<ls_loc_addr>)
                                          WITH KEY root_key = <ls_stop>-log_loc_uuid.
      CHECK sy-subrc = 0.

      IF <ls_loc_addr>-time_zone_code IS NOT INITIAL.
        CONVERT TIME STAMP <ls_stop>-assgn_start
          TIME ZONE <ls_loc_addr>-time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
        DATA(lv_tz) = <ls_loc_addr>-time_zone_code.
      ELSE.
        CONVERT TIME STAMP <ls_stop>-assgn_start
          TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
        lv_tz = sy-zonlo.
      ENDIF.

      DATA(lv_locid2) = get_capa_matchkey( iv_assgn_stop_key = <ls_stop>-assgn_stop_key ).
      mv_milestonecnt += 1.

      APPEND VALUE #( appsys            = mo_ef_parameters->get_appsys(  )
                      appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                      language          = sy-langu
                      appobjid          = is_app_objects-appobjid
                      milestone         = lif_sst_constants=>cs_milestone-fo_load_start
                      evt_exp_datetime  = |{ lv_conv_date }{ lv_conv_time }|
                      evt_exp_tzone     = lv_tz
                      locid1            = <ls_stop>-log_locid
                      locid2            = lv_locid2
                      loctype           = lif_sst_constants=>cs_location_type-logistic
                      country           = <ls_loc_addr>-country_code
                      city              = <ls_loc_addr>-city_name ) TO ct_expeventdata.
    ENDLOOP.
  ENDMETHOD.

  METHOD load_end.

    DATA ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.

    FIELD-SYMBOLS:
      <lt_stop> TYPE /scmtms/t_em_bo_tor_stop,
      <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop ).
    ASSIGN lr_stop->* TO <lt_stop>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT <lt_stop> ASSIGNING FIELD-SYMBOL(<ls_stop>) USING KEY parent_seqnum WHERE parent_node_id = <ls_root>-node_id.

      CHECK <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-outbound AND <ls_stop>-assgn_end IS NOT INITIAL.

      READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
         WITH KEY root_key = <ls_stop>-log_loc_uuid.
      CHECK sy-subrc = 0.

      IF ls_loc_addr->time_zone_code IS NOT INITIAL.
        CONVERT TIME STAMP <ls_stop>-assgn_end
         TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
        DATA(lv_tz) = ls_loc_addr->time_zone_code.
      ELSE.
        CONVERT TIME STAMP <ls_stop>-assgn_end
           TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
        lv_tz = sy-zonlo.
      ENDIF.

      DATA(lv_locid2) = get_capa_matchkey( iv_assgn_stop_key = <ls_stop>-assgn_stop_key ).
      mv_milestonecnt += 1.

      APPEND VALUE #( appsys            = mo_ef_parameters->get_appsys(  )
                      appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                      language          = sy-langu
                      appobjid          = is_app_objects-appobjid
                      milestone         = lif_sst_constants=>cs_milestone-fo_load_end
                      evt_exp_datetime  = |0{ lv_conv_date }{ lv_conv_time }|
                      evt_exp_tzone     = lv_tz
                      locid1            = <ls_stop>-log_locid
                      locid2            = lv_locid2
                      loctype           = lif_sst_constants=>cs_location_type-logistic
                      country           = ls_loc_addr->country_code
                      city              = ls_loc_addr->city_name
                      itemident         = <ls_stop>-node_id ) TO ct_expeventdata.

    ENDLOOP.
  ENDMETHOD.

  METHOD unload_start.

    DATA ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.
    FIELD-SYMBOLS:
      <lt_stop> TYPE /scmtms/t_em_bo_tor_stop,
      <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop ).
    ASSIGN lr_stop->* TO <lt_stop>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT <lt_stop> ASSIGNING FIELD-SYMBOL(<ls_stop>) USING KEY parent_seqnum WHERE parent_node_id = <ls_root>-node_id.

      CHECK <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound AND <ls_stop>-assgn_start IS NOT INITIAL.

      READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                     WITH KEY root_key = <ls_stop>-log_loc_uuid.
      CHECK sy-subrc = 0.
      IF ls_loc_addr->time_zone_code IS NOT INITIAL.
        CONVERT TIME STAMP <ls_stop>-assgn_start
          TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
        DATA(lv_tz) = ls_loc_addr->time_zone_code.
      ELSE.
        CONVERT TIME STAMP <ls_stop>-assgn_start TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
        lv_tz = sy-zonlo.
      ENDIF.

      DATA(lv_locid2) = get_capa_matchkey( iv_assgn_stop_key = <ls_stop>-assgn_stop_key ).
      mv_milestonecnt += 1.

      APPEND VALUE #(  appsys            = mo_ef_parameters->get_appsys(  )
                       appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                       language          = sy-langu
                       appobjid          = is_app_objects-appobjid
                       milestone         = lif_sst_constants=>cs_milestone-fo_unload_start
                       evt_exp_datetime  = |0{ lv_conv_date }{ lv_conv_time }|
                       evt_exp_tzone     = lv_tz
                       locid1            = <ls_stop>-log_locid
                       locid2            = lv_locid2
                       loctype           = lif_sst_constants=>cs_location_type-logistic
                       country           = ls_loc_addr->country_code
                       city              = ls_loc_addr->city_name ) TO ct_expeventdata.
    ENDLOOP.

  ENDMETHOD.

  METHOD unload_end.

    DATA ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.
    FIELD-SYMBOLS:
      <lt_stop> TYPE /scmtms/t_em_bo_tor_stop,
      <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop ).
    ASSIGN lr_stop->* TO <lt_stop>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT <lt_stop> ASSIGNING FIELD-SYMBOL(<ls_stop>) USING KEY parent_seqnum WHERE parent_node_id = <ls_root>-node_id.

      CHECK <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound AND <ls_stop>-assgn_end IS NOT INITIAL.

      READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                     WITH KEY root_key = <ls_stop>-log_loc_uuid.

      CHECK sy-subrc = 0.
      IF ls_loc_addr->time_zone_code IS NOT INITIAL.
        CONVERT TIME STAMP <ls_stop>-assgn_end
          TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
        DATA(lv_tz) = ls_loc_addr->time_zone_code.
      ELSE.
        CONVERT TIME STAMP <ls_stop>-assgn_end
         TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
        lv_tz = sy-zonlo.
      ENDIF.

      DATA(lv_locid2) = get_capa_matchkey( iv_assgn_stop_key = <ls_stop>-assgn_stop_key ).
      mv_milestonecnt += 1.

      APPEND VALUE #( appsys            = mo_ef_parameters->get_appsys(  )
                      appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                      language          = sy-langu
                      appobjid          = is_app_objects-appobjid
                      milestone         = lif_sst_constants=>cs_milestone-fo_unload_end
                      evt_exp_datetime  = |0{ lv_conv_date }{ lv_conv_time }|
                      evt_exp_tzone     = lv_tz
                      locid1            = <ls_stop>-log_locid
                      locid2            = lv_locid2
                      loctype           = lif_sst_constants=>cs_location_type-logistic
                      country           = ls_loc_addr->country_code
                      city              = ls_loc_addr->city_name ) TO ct_expeventdata.
    ENDLOOP.
  ENDMETHOD.

  METHOD shp_arrival.

    DATA ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.
    FIELD-SYMBOLS:
      <lt_capa_stop> TYPE /scmtms/t_em_bo_tor_stop,
      <lt_stop>      TYPE /scmtms/t_em_bo_tor_stop,
      <ls_root>      TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop ).
    ASSIGN lr_stop->* TO <lt_stop>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_capa_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-capa_stop ).
    ASSIGN lr_capa_stop->* TO <lt_capa_stop>.

    LOOP AT <lt_stop> ASSIGNING FIELD-SYMBOL(<ls_stop>) USING KEY parent_seqnum WHERE parent_node_id = <ls_root>-node_id.

      READ TABLE <lt_capa_stop> ASSIGNING FIELD-SYMBOL(<ls_capa_stop>) WITH KEY node_id = <ls_stop>-assgn_stop_key.
      CHECK sy-subrc = 0.

      CHECK <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound AND
            <ls_capa_stop>-plan_trans_time IS NOT INITIAL.

      READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr WITH KEY root_key = <ls_stop>-log_loc_uuid.
      CHECK sy-subrc = 0.

      IF ls_loc_addr->time_zone_code IS NOT INITIAL.
        CONVERT TIME STAMP <ls_capa_stop>-plan_trans_time
         TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
        DATA(lv_tz) = ls_loc_addr->time_zone_code.
      ELSE.
        CONVERT TIME STAMP <ls_capa_stop>-plan_trans_time
           TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
        lv_tz = sy-zonlo.
      ENDIF.

      DATA(lv_locid2) = get_capa_matchkey( iv_assgn_stop_key = <ls_stop>-assgn_stop_key ).
      mv_milestonecnt += 1.

      APPEND VALUE #( appsys            = mo_ef_parameters->get_appsys(  )
                      appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
                      language          = sy-langu
                      appobjid          = is_app_objects-appobjid
                      milestone         = lif_sst_constants=>cs_milestone-fo_shp_arrival
                      evt_exp_datetime  = |0{ lv_conv_date }{ lv_conv_time }|
                      evt_exp_tzone     = lv_tz
                      locid1            = <ls_stop>-log_locid
                      locid2            = lv_locid2
                      loctype           = lif_sst_constants=>cs_location_type-logistic
                      country           = ls_loc_addr->country_code
                      city              = ls_loc_addr->city_name ) TO ct_expeventdata.
    ENDLOOP.
  ENDMETHOD.

  METHOD shp_departure.

    DATA ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.

    FIELD-SYMBOLS:
      <lt_capa_stop> TYPE /scmtms/t_em_bo_tor_stop,
      <lt_stop>      TYPE /scmtms/t_em_bo_tor_stop,
      <ls_root>      TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop ).
    ASSIGN lr_stop->* TO <lt_stop>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_capa_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-capa_stop ).
    ASSIGN lr_capa_stop->* TO <lt_capa_stop>.

    LOOP AT <lt_stop> ASSIGNING FIELD-SYMBOL(<ls_stop>) USING KEY parent_seqnum WHERE parent_node_id = <ls_root>-node_id.

      READ TABLE <lt_capa_stop> ASSIGNING FIELD-SYMBOL(<ls_capa_stop>)
        WITH KEY node_id = <ls_stop>-assgn_stop_key.
      CHECK sy-subrc = 0.

      CHECK <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-outbound AND
            <ls_capa_stop>-plan_trans_time IS NOT INITIAL.

      READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr  WITH KEY root_key = <ls_stop>-log_loc_uuid.
      CHECK sy-subrc = 0.

      IF ls_loc_addr->time_zone_code IS NOT INITIAL.
        CONVERT TIME STAMP <ls_capa_stop>-plan_trans_time
          TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
        DATA(lv_tz) = ls_loc_addr->time_zone_code.
      ELSE.
        CONVERT TIME STAMP <ls_capa_stop>-plan_trans_time
          TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
        lv_tz = sy-zonlo.
      ENDIF.

      DATA(lv_locid2) = get_capa_matchkey( iv_assgn_stop_key = <ls_stop>-assgn_stop_key ).
      mv_milestonecnt += 1.

      APPEND VALUE #( appsys           = mo_ef_parameters->get_appsys(  )
                      appobjtype       = mo_ef_parameters->get_app_obj_types( )-aotype
                      language         = sy-langu
                      appobjid         = is_app_objects-appobjid
                      milestone        = lif_sst_constants=>cs_milestone-fo_shp_departure
                      evt_exp_datetime = |0{ lv_conv_date }{ lv_conv_time }|
                      evt_exp_tzone    = lv_tz
                      locid1           = <ls_stop>-log_locid
                      locid2           = lv_locid2
                      loctype          = lif_sst_constants=>cs_location_type-logistic
                      country          = ls_loc_addr->country_code
                      city             = ls_loc_addr->city_name ) TO ct_expeventdata.
    ENDLOOP.
  ENDMETHOD.

  METHOD pod.

    DATA ls_loc_addr TYPE REF TO /bofu/s_addr_postal_addressk.
    FIELD-SYMBOLS:
      <lt_stop> TYPE /scmtms/t_em_bo_tor_stop,
      <ls_root> TYPE /scmtms/s_em_bo_tor_root.

    ASSIGN is_app_objects-maintabref->* TO <ls_root>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lr_stop) = mo_ef_parameters->get_appl_table( /scmtms/cl_scem_int_c=>sc_table_definition-bo_tor-stop ).
    ASSIGN lr_stop->* TO <lt_stop>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    DATA(lt_stop) = VALUE /scmtms/t_em_bo_tor_stop( FOR <ls_tor_stop> IN <lt_stop> USING KEY parent_seqnum
                                                    WHERE ( parent_node_id = <ls_root>-node_id ) ( <ls_tor_stop> ) ).

    DATA(lv_stop_count) = lines( lt_stop ).
    ASSIGN lt_stop[ seq_num = lv_stop_count ] TO FIELD-SYMBOL(<ls_stop>).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF <ls_stop>-assgn_end IS INITIAL.
      RETURN.
    ENDIF.

    READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr WITH KEY root_key = <ls_stop>-log_loc_uuid.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.
    IF ls_loc_addr->time_zone_code IS NOT INITIAL.
      CONVERT TIME STAMP <ls_stop>-assgn_end
      TIME ZONE ls_loc_addr->time_zone_code INTO DATE DATA(lv_conv_date) TIME DATA(lv_conv_time).
      DATA(lv_tz) = ls_loc_addr->time_zone_code.
    ELSE.
      CONVERT TIME STAMP <ls_stop>-assgn_end
      TIME ZONE sy-zonlo INTO DATE lv_conv_date TIME lv_conv_time.
      lv_tz = sy-zonlo.
    ENDIF.

    DATA(lv_locid2) = <ls_root>-tor_id.
    mv_milestonecnt += 1.

    APPEND VALUE #( appsys           = mo_ef_parameters->get_appsys(  )
                    appobjtype       = mo_ef_parameters->get_app_obj_types( )-aotype
                    language         = sy-langu
                    appobjid         = is_app_objects-appobjid
                    milestone        = lif_sst_constants=>cs_milestone-fo_shp_pod
                    evt_exp_datetime = |0{ lv_conv_date }{ lv_conv_time }|
                    evt_exp_tzone    = lv_tz
                    locid1           = <ls_stop>-log_locid
                    locid2           = lv_locid2
                    loctype          = lif_sst_constants=>cs_location_type-logistic
                    country          = ls_loc_addr->country_code
                    city             = ls_loc_addr->city_name ) TO ct_expeventdata.

  ENDMETHOD.

  METHOD lif_pe_filler~get_planed_events.

    get_data_for_planned_event(
      EXPORTING
        is_app_objects = is_app_objects
      IMPORTING
        ev_tor_id      = DATA(lv_tor_id)
        et_loc_address = DATA(lt_loc_address) ).

    load_start(
      EXPORTING
        it_loc_addr     = lt_loc_address
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    load_end(
      EXPORTING
        it_loc_addr     = lt_loc_address
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    unload_start(
      EXPORTING
        it_loc_addr     = lt_loc_address
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    unload_end(
      EXPORTING
        it_loc_addr     = lt_loc_address
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    shp_arrival(
      EXPORTING
        it_loc_addr     = lt_loc_address
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    shp_departure(
      EXPORTING
        it_loc_addr     = lt_loc_address
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    pod(
      EXPORTING
        it_loc_addr     = lt_loc_address
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

  ENDMETHOD.
ENDCLASS.
