*&---------------------------------------------------------------------*
*& Local class definition - Actual Events Fillers
*&---------------------------------------------------------------------*

**********************************************************************
*** Planned Events of Freight Order ****************************
**********************************************************************
CLASS lcl_pe_filler_fo_header DEFINITION.
  PUBLIC SECTION.
    INTERFACES lif_pe_filler.

    METHODS constructor
      IMPORTING
        io_ef_parameters TYPE REF TO lif_ef_parameters
        io_bo_reader     TYPE REF TO lif_bo_reader.

  PRIVATE SECTION.
    DATA: mo_ef_parameters   TYPE REF TO lif_ef_parameters,
          mo_bo_reader       TYPE REF TO lif_bo_reader,
          mv_milestonecnt(4) TYPE n.

    METHODS load_start
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id
        it_stop         TYPE /scmtms/t_em_bo_tor_stop
        it_loc_addr     TYPE /scmtms/t_em_bo_loc_addr
        it_stop_points  TYPE lif_ef_types=>tt_stop_points
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS load_end
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id
        it_stop         TYPE /scmtms/t_em_bo_tor_stop
        it_loc_addr     TYPE /scmtms/t_em_bo_loc_addr
        it_stop_points  TYPE lif_ef_types=>tt_stop_points
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS coupling
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id
        it_stop         TYPE /scmtms/t_em_bo_tor_stop
        it_loc_addr     TYPE /scmtms/t_em_bo_loc_addr
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
        it_loc_addr     TYPE /scmtms/t_em_bo_loc_addr
        it_stop_points  TYPE lif_ef_types=>tt_stop_points
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS unload_start
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id
        it_stop         TYPE /scmtms/t_em_bo_tor_stop
        it_loc_addr     TYPE /scmtms/t_em_bo_loc_addr
        it_stop_points  TYPE lif_ef_types=>tt_stop_points
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS unload_end
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id
        it_stop         TYPE /scmtms/t_em_bo_tor_stop
        it_loc_addr     TYPE /scmtms/t_em_bo_loc_addr
        it_stop_points  TYPE lif_ef_types=>tt_stop_points
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS shp_arrival
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id
        it_stop         TYPE /scmtms/t_em_bo_tor_stop
        it_loc_addr     TYPE /scmtms/t_em_bo_loc_addr
        it_stop_points  TYPE lif_ef_types=>tt_stop_points
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

    METHODS shp_departure
      IMPORTING
        iv_tor_id       TYPE /scmtms/tor_id
        it_stop         TYPE /scmtms/t_em_bo_tor_stop
        it_loc_addr     TYPE /scmtms/t_em_bo_loc_addr
        it_stop_points  TYPE lif_ef_types=>tt_stop_points
        is_app_objects  TYPE trxas_appobj_ctab_wa
      CHANGING
        ct_expeventdata TYPE lif_ef_types=>tt_expeventdata
      RAISING
        cx_udm_message.

ENDCLASS.

CLASS lcl_pe_filler_fo_header IMPLEMENTATION.

  METHOD load_start.

    DATA: ls_loc_addr TYPE REF TO /scmtms/s_em_bo_loc_addr,
          lv_seq_num  TYPE /scmtms/seq_num.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>).

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
                                       WITH KEY parent_node_id = <ls_stop>-log_loc_uuid BINARY SEARCH.

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
          milestone         = lif_sst_constants=>cs_milestone-fo_load_start
          evt_exp_datetime  = |{ lv_conv_date }{ lv_conv_time }|
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

  METHOD load_end.

    DATA: ls_loc_addr TYPE REF TO /scmtms/s_em_bo_loc_addr,
          lv_seq_num  TYPE /scmtms/seq_num.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>).

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
                                       WITH KEY parent_node_id = <ls_stop>-log_loc_uuid BINARY SEARCH.

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

    DATA: ls_loc_addr TYPE REF TO /scmtms/s_em_bo_loc_addr,
          lv_seq_num  TYPE /scmtms/seq_num.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>).

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
                                       WITH KEY parent_node_id = <ls_stop>-log_loc_uuid BINARY SEARCH.

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

    DATA: ls_loc_addr TYPE REF TO /scmtms/s_em_bo_loc_addr.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>).

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound
         AND <ls_stop>-aggr_assgn_start_c IS NOT INITIAL
         AND <ls_stop>-aggr_assgn_end_c   IS NOT INITIAL.

        READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                       WITH KEY log_locid = <ls_stop>-log_locid
                                                seq_num   = <ls_stop>-seq_num.
        READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                       WITH KEY parent_node_id = <ls_stop>-log_loc_uuid BINARY SEARCH.

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

    DATA: ls_loc_addr TYPE REF TO /scmtms/s_em_bo_loc_addr.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>).

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
                                       WITH KEY parent_node_id = <ls_stop>-log_loc_uuid BINARY SEARCH.

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

    DATA: ls_loc_addr TYPE REF TO /scmtms/s_em_bo_loc_addr.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>).

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
                                       WITH KEY parent_node_id = <ls_stop>-log_loc_uuid BINARY SEARCH.

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

    DATA: ls_loc_addr TYPE REF TO /scmtms/s_em_bo_loc_addr.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>).

      IF <ls_stop>-stop_cat = /scmtms/if_common_c=>c_stop_category-inbound
         AND <ls_stop>-plan_trans_time IS NOT INITIAL.

        READ TABLE it_stop_points REFERENCE INTO DATA(ls_stop_points)
                                       WITH KEY log_locid = <ls_stop>-log_locid
                                                seq_num   = <ls_stop>-seq_num.
        IF sy-subrc IS NOT INITIAL.
          CONTINUE.
        ENDIF.

        READ TABLE it_loc_addr REFERENCE INTO ls_loc_addr
                                       WITH KEY parent_node_id = <ls_stop>-log_loc_uuid BINARY SEARCH.

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

    DATA: ls_loc_addr TYPE REF TO /scmtms/s_em_bo_loc_addr,
          lv_seq_num  TYPE /scmtms/seq_num.

    LOOP AT it_stop USING KEY parent_seqnum ASSIGNING FIELD-SYMBOL(<ls_stop>).

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
                                         WITH KEY parent_node_id = <ls_stop>-log_loc_uuid BINARY SEARCH.

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
    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.
    mv_milestonecnt     = 0.
  ENDMETHOD.

  METHOD lif_pe_filler~check_relevance.
    rv_result = abap_false.
  ENDMETHOD.

  METHOD lif_pe_filler~get_planed_events.

    DATA: lv_tor_id   TYPE /scmtms/tor_id,
          lv_tor_cat  TYPE /scmtms/tor_category,
          lt_stop     TYPE /scmtms/t_em_bo_tor_stop,
          lr_stop     TYPE REF TO data,
          lr_loc_addr TYPE REF TO data,
          ls_loc_addr TYPE REF TO /scmtms/s_em_bo_loc_addr.

    FIELD-SYMBOLS:
                   <lt_stop>     TYPE /scmtms/t_em_bo_tor_stop,
                   <lt_loc_addr> TYPE /scmtms/t_em_bo_loc_addr.

    lv_tor_id      =  lcl_tools=>get_field_of_structure(
                        ir_struct_data = is_app_objects-maintabref
                        iv_field_name  = 'TOR_ID' ).
    SHIFT lv_tor_id LEFT DELETING LEADING '0'.

    lv_tor_cat     =  lcl_tools=>get_field_of_structure(
                        ir_struct_data = is_app_objects-maintabref
                        iv_field_name  = 'TOR_CAT' ).

    DATA(lv_tor_node_id) =  lcl_tools=>get_field_of_structure(
                              ir_struct_data = is_app_objects-maintabref
                              iv_field_name  = 'NODE_ID' ).

    /scmtms/cl_tor_helper_stop=>get_stop_sequence(
      EXPORTING
        it_root_key     = VALUE #( ( key = lv_tor_node_id ) )
        iv_before_image = abap_false
      IMPORTING
        et_stop_seq_d   = DATA(lt_stop_seq) ).

    MOVE-CORRESPONDING lt_stop_seq[ root_key = lv_tor_node_id ]-stop_seq TO lt_stop.
    ASSIGN lt_stop     TO <lt_stop>.

    lr_loc_addr  = mo_ef_parameters->get_appl_table(
                            iv_tabledef = lif_sst_constants=>cs_tabledef-fo_stop_addr ).

    ASSIGN lr_loc_addr->* TO <lt_loc_addr>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lcl_tools=>get_stop_points(
      EXPORTING
        iv_root_id     = lv_tor_id
        it_stop        = lt_stop
      IMPORTING
        et_stop_points = DATA(lt_stop_points) ).

    load_start(
      EXPORTING
        iv_tor_id      = lv_tor_id
        it_stop        = lt_stop
        it_loc_addr    = <lt_loc_addr>
        it_stop_points = lt_stop_points
        is_app_objects = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    load_end(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = <lt_loc_addr>
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    unload_start(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = <lt_loc_addr>
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    unload_end(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = <lt_loc_addr>
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    shp_arrival(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = <lt_loc_addr>
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    shp_departure(
      EXPORTING
        iv_tor_id       = lv_tor_id
        it_stop         = lt_stop
        it_loc_addr     = <lt_loc_addr>
        it_stop_points  = lt_stop_points
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata ).

    IF lv_tor_cat <> /scmtms/if_tor_const=>sc_tor_category-booking.
      coupling(
        EXPORTING
          iv_tor_id       = lv_tor_id
          it_stop         = lt_stop
          it_loc_addr     = <lt_loc_addr>
          it_stop_points  = lt_stop_points
          is_app_objects  = is_app_objects
        CHANGING
          ct_expeventdata = ct_expeventdata ).

      decoupling(
        EXPORTING
          iv_tor_id       = lv_tor_id
          it_stop         = lt_stop
          it_loc_addr     = <lt_loc_addr>
          it_stop_points  = lt_stop_points
          is_app_objects  = is_app_objects
        CHANGING
          ct_expeventdata = ct_expeventdata ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
