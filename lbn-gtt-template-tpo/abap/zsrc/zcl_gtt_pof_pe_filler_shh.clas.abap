CLASS zcl_gtt_pof_pe_filler_shh DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_pe_filler .

  METHODS constructor
    IMPORTING
      !io_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters
      !io_bo_reader TYPE REF TO zif_gtt_pof_tp_reader
    RAISING
      cx_udm_message .
PROTECTED SECTION.
PRIVATE SECTION.

  TYPES:
    tt_vbeln    TYPE RANGE OF lips-vbeln .
  TYPES:
    tt_werks    TYPE RANGE OF lips-werks .
  TYPES:
    tt_appobjid TYPE RANGE OF /saptrx/aoid .

  DATA mo_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters .
  DATA mo_bo_reader TYPE REF TO zif_gtt_pof_tp_reader .
  DATA mo_sh_data_old TYPE REF TO zcl_gtt_pof_sh_data_old .

  METHODS add_shipment_events
    IMPORTING
      !is_app_objects TYPE trxas_appobj_ctab_wa
    CHANGING
      !ct_expeventdata TYPE zif_gtt_pof_ef_types=>tt_expeventdata
      !ct_measrmntdata TYPE zif_gtt_pof_ef_types=>tt_measrmntdata
      !ct_infodata TYPE zif_gtt_pof_ef_types=>tt_infodata
    RAISING
      cx_udm_message .
  METHODS add_stops_events
    IMPORTING
      !is_app_objects TYPE trxas_appobj_ctab_wa
      !iv_milestonenum TYPE /saptrx/seq_num
    CHANGING
      !ct_expeventdata TYPE zif_gtt_pof_ef_types=>tt_expeventdata
      !ct_measrmntdata TYPE zif_gtt_pof_ef_types=>tt_measrmntdata
      !ct_infodata TYPE zif_gtt_pof_ef_types=>tt_infodata
    RAISING
      cx_udm_message .
  METHODS get_shippment_header
    IMPORTING
      !is_app_object TYPE trxas_appobj_ctab_wa
      !ir_vttk TYPE REF TO data
    RETURNING
      VALUE(rr_vttk) TYPE REF TO data
    RAISING
      cx_udm_message .
  METHODS is_pod_relevant
    IMPORTING
      !is_stops TYPE zif_gtt_pof_app_types=>ts_stops
      !it_vttp TYPE vttpvb_tab
      !it_vtsp TYPE vtspvb_tab
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
  METHODS is_stop_changed
    IMPORTING
      !is_app_object TYPE trxas_appobj_ctab_wa
      !it_fields TYPE zif_gtt_pof_ef_types=>tt_field_name
    RETURNING
      VALUE(rv_result) TYPE zif_gtt_pof_ef_types=>tv_condition
    RAISING
      cx_udm_message .
  METHODS get_corresponding_dlv_items
    IMPORTING
      !it_vbeln TYPE tt_vbeln
      !it_werks TYPE tt_werks
    EXPORTING
      !et_appobjid TYPE tt_appobjid
    RAISING
      cx_udm_message .
  METHODS get_header_fields
    EXPORTING
      !et_fields TYPE zif_gtt_pof_ef_types=>tt_field_name .
  METHODS get_stop_fields
    EXPORTING
      !et_fields TYPE zif_gtt_pof_ef_types=>tt_field_name .
ENDCLASS.



CLASS zcl_gtt_pof_pe_filler_shh IMPLEMENTATION.


  METHOD add_shipment_events.

    FIELD-SYMBOLS: <ls_vttk>  TYPE vttkvb.

    ASSIGN is_app_objects-maintabref->* TO <ls_vttk>.

    IF <ls_vttk> IS ASSIGNED.
      " CHECK IN
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_pof_app_constants=>cs_milestone-sh_check_in
        evt_exp_datetime  = zcl_gtt_pof_tools=>get_local_timestamp(
                              iv_date = <ls_vttk>-dpreg
                              iv_time = <ls_vttk>-upreg )
        evt_exp_tzone     = zcl_gtt_pof_tools=>get_system_time_zone( )
        milestonenum      = 1
      ) ).

      " LOAD START
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_pof_app_constants=>cs_milestone-sh_load_start
        evt_exp_datetime  = zcl_gtt_pof_tools=>get_local_timestamp(
                              iv_date = <ls_vttk>-dplbg
                              iv_time = <ls_vttk>-uplbg )
        evt_exp_tzone     = zcl_gtt_pof_tools=>get_system_time_zone( )
        milestonenum      = 2
      ) ).

      " LOAD END
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = zif_gtt_pof_app_constants=>cs_milestone-sh_load_end
        evt_exp_datetime  = zcl_gtt_pof_tools=>get_local_timestamp(
                              iv_date = <ls_vttk>-dplen
                              iv_time = <ls_vttk>-uplen )
        evt_exp_tzone     = zcl_gtt_pof_tools=>get_system_time_zone( )
        milestonenum      = 3
      ) ).
    ENDIF.

  ENDMETHOD.


  METHOD add_stops_events.

    DATA(lv_tknum) = CONV tknum( zcl_gtt_pof_tools=>get_field_of_structure(
                                   ir_struct_data = is_app_objects-maintabref
                                   iv_field_name  = 'TKNUM' ) ).

    DATA(lv_milestonenum) = iv_milestonenum.
    DATA: lt_stops    TYPE zif_gtt_pof_app_types=>tt_stops.

    FIELD-SYMBOLS: <lt_vttp> TYPE vttpvb_tab,
                   <lt_vtts> TYPE vttsvb_tab,
                   <lt_vtsp> TYPE vtspvb_tab.

    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
                      iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vtts) = mo_ef_parameters->get_appl_table(
                      iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtsp) = mo_ef_parameters->get_appl_table(
                      iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_stage_new ).

    ASSIGN lr_vtts->* TO <lt_vtts>.
    ASSIGN lr_vttp->* TO <lt_vttp>.
    ASSIGN lr_vtsp->* TO <lt_vtsp>.

    IF <lt_vtts> IS ASSIGNED AND
       <lt_vtsp> IS ASSIGNED AND
       <lt_vttp> IS ASSIGNED.

      zcl_gtt_pof_sh_tools=>get_stops_from_shipment(
        EXPORTING
          iv_tknum              = lv_tknum
          it_vtts               = <lt_vtts>
          it_vtsp               = <lt_vtsp>
          it_vttp               = <lt_vttp>
        IMPORTING
          et_stops              = lt_stops ).

      " important for correct sequence number calculation
      SORT lt_stops BY stopid loccat.

      LOOP AT lt_stops ASSIGNING FIELD-SYMBOL(<ls_stops>).
        " DEPARTURE / ARRIVAL
        ct_expeventdata = VALUE #( BASE ct_expeventdata (
          appsys            = mo_ef_parameters->get_appsys(  )
          appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
          language          = sy-langu
          appobjid          = is_app_objects-appobjid
          milestone         = COND #( WHEN <ls_stops>-loccat = zif_gtt_pof_app_constants=>cs_loccat-departure
                                        THEN zif_gtt_pof_app_constants=>cs_milestone-sh_departure
                                        ELSE zif_gtt_pof_app_constants=>cs_milestone-sh_arrival )
          evt_exp_datetime  = <ls_stops>-pln_evt_datetime
          evt_exp_tzone     = <ls_stops>-pln_evt_timezone
          locid2            = <ls_stops>-stopid
          loctype           = <ls_stops>-loctype
          locid1            = <ls_stops>-locid
          milestonenum      = lv_milestonenum
        ) ).

        ADD 1 TO lv_milestonenum.

        IF is_pod_relevant( is_stops = <ls_stops>
                            it_vttp  = <lt_vttp>
                            it_vtsp  = <lt_vtsp> ) = abap_true.
          " POD
          ct_expeventdata = VALUE #( BASE ct_expeventdata (
            appsys            = mo_ef_parameters->get_appsys(  )
            appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
            language          = sy-langu
            appobjid          = is_app_objects-appobjid
            milestone         = zif_gtt_pof_app_constants=>cs_milestone-sh_pod
            evt_exp_datetime  = <ls_stops>-pln_evt_datetime
            evt_exp_tzone     = <ls_stops>-pln_evt_timezone
            locid2            = <ls_stops>-stopid
            loctype           = <ls_stops>-loctype
            locid1            = <ls_stops>-locid
            milestonenum      = lv_milestonenum
          ) ).

          ADD 1 TO lv_milestonenum.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTS' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.
    mo_bo_reader        = io_bo_reader.
    mo_sh_data_old      = NEW zcl_gtt_pof_sh_data_old(
                            io_ef_parameters = io_ef_parameters ).

  ENDMETHOD.


  METHOD get_corresponding_dlv_items.

    DATA(lr_lips)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_delivery_item ).

    FIELD-SYMBOLS: <lt_lips> TYPE vtrlp_tab.

    CLEAR: et_appobjid[].

    ASSIGN lr_lips->* TO <lt_lips>.

    IF <lt_lips> IS ASSIGNED.
      LOOP AT <lt_lips> ASSIGNING FIELD-SYMBOL(<ls_lips>)
        WHERE vbeln IN it_vbeln
          AND werks IN it_werks.

        et_appobjid = VALUE #( BASE et_appobjid (
                        low = |{ <ls_lips>-vbeln }{ <ls_lips>-posnr }|
                        option = 'EQ'
                        sign = 'I'
                      ) ).
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'LIPS' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_header_fields.

    et_fields   = VALUE #( ( 'DPREG' ) ( 'UPREG' )
                           ( 'DPLBG' ) ( 'UPLBG' )
                           ( 'DPLEN' ) ( 'UPLEN' ) ).

  ENDMETHOD.


  METHOD get_shippment_header.

    TYPES: tt_vttk TYPE STANDARD TABLE OF vttkvb.

    FIELD-SYMBOLS: <lt_vttk> TYPE tt_vttk.

    DATA(lv_tknum)  = zcl_gtt_pof_tools=>get_field_of_structure(
                        ir_struct_data = is_app_object-maintabref
                        iv_field_name  = 'TKNUM' ).

    ASSIGN ir_vttk->* TO <lt_vttk>.
    IF <lt_vttk> IS ASSIGNED.
      READ TABLE <lt_vttk> ASSIGNING FIELD-SYMBOL(<ls_vttk>)
        WITH KEY tknum = lv_tknum.

      IF sy-subrc = 0.
        rr_vttk = REF #( <ls_vttk> ).
      ELSE.
        MESSAGE e005(zgtt_pof) WITH 'VTTK OLD' lv_tknum
          INTO DATA(lv_dummy).
        zcl_gtt_pof_tools=>throw_exception( ).
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTK'
        INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_stop_fields.

    et_fields   = VALUE #( ( 'DPTBG' ) ( 'UPTBG' )
                           ( 'DPTEN' ) ( 'UPTEN' )
                           ( 'KUNNA' ) ( 'KUNNZ' )
                           ( 'VSTEL' ) ( 'VSTEZ' )
                           ( 'LIFNA' ) ( 'LIFNZ' )
                           ( 'WERKA' ) ( 'WERKZ' )
                           ( 'KNOTA' ) ( 'KNOTZ' ) ).

  ENDMETHOD.


  METHOD is_pod_relevant.

    DATA: lt_vbeln    TYPE RANGE OF lips-vbeln,
          lt_appobjid TYPE RANGE OF /saptrx/aoid,
          lv_locid    TYPE zif_gtt_pof_app_types=>tv_locid,
          lv_pdstk    TYPE pdstk.

    rv_result = abap_false.

    IF is_stops-loccat  = zif_gtt_pof_app_constants=>cs_loccat-arrival AND
       is_stops-loctype = zif_gtt_pof_ef_constants=>cs_loc_types-plant.

      " get Inbound Delivery Numbers
      LOOP AT it_vtsp ASSIGNING FIELD-SYMBOL(<ls_vtsp>)
        WHERE tknum = is_stops-tknum
          AND tsnum = is_stops-tsnum.

        READ TABLE it_vttp ASSIGNING FIELD-SYMBOL(<ls_vttp>)
          WITH KEY tknum = <ls_vtsp>-tknum
                   tpnum = <ls_vtsp>-tpnum.

        IF sy-subrc = 0.
          lt_vbeln[]    = VALUE #( BASE lt_vbeln
                                  ( option = 'EQ'
                                    sign   = 'I'
                                    low    = <ls_vttp>-vbeln ) ).
        ENDIF.
      ENDLOOP.

      " get appobjid range (inbound deliveries for corresponding Plant)
      IF lt_vbeln[] IS NOT INITIAL.
        get_corresponding_dlv_items(
          EXPORTING
            it_vbeln    = lt_vbeln
            it_werks    = VALUE #( ( low = is_stops-locid
                                     option = 'EQ'
                                     sign   = 'I'  ) )
          IMPORTING
            et_appobjid = lt_appobjid ).
      ENDIF.

      " get POD enabled flags for found DLV Items
      IF lt_appobjid[] IS NOT INITIAL.
        SELECT SINGLE z_pdstk                           "#EC CI_NOORDER
          INTO rv_result
          FROM zpof_gtt_ee_rel
          WHERE appobjid IN lt_appobjid
            AND z_pdstk   = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD is_stop_changed.

    FIELD-SYMBOLS: <lt_vtts_new> TYPE zif_gtt_pof_app_types=>tt_vttsvb,
                   <lt_vtts_old> TYPE zif_gtt_pof_app_types=>tt_vttsvb.

    DATA(lv_tknum)    = CONV tknum( zcl_gtt_pof_tools=>get_field_of_structure(
                                      ir_struct_data = is_app_object-maintabref
                                      iv_field_name  = 'TKNUM' ) ).

    DATA(lr_vtts_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtts_old) = mo_sh_data_old->get_vtts( ).

    rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.

    ASSIGN lr_vtts_new->* TO <lt_vtts_new>.
    ASSIGN lr_vtts_old->* TO <lt_vtts_old>.

    IF <lt_vtts_new> IS ASSIGNED AND
       <lt_vtts_old> IS ASSIGNED.

      LOOP AT <lt_vtts_new> ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
        WHERE tknum = lv_tknum
          AND updkz IS NOT INITIAL.

        CASE <ls_vtts_new>-updkz.
          WHEN zif_gtt_pof_ef_constants=>cs_change_mode-insert.
            rv_result   = zif_gtt_pof_ef_constants=>cs_condition-true.

          WHEN zif_gtt_pof_ef_constants=>cs_change_mode-update OR
               zif_gtt_pof_ef_constants=>cs_change_mode-undefined.

            READ TABLE <lt_vtts_old> ASSIGNING FIELD-SYMBOL(<ls_vtts_old>)
              WITH KEY tknum  = <ls_vtts_new>-tknum
                       tsnum  = <ls_vtts_new>-tsnum.

            rv_result   = zcl_gtt_pof_tools=>are_fields_different(
                  ir_data1  = REF #( <ls_vtts_new> )
                  ir_data2  = REF #( <ls_vtts_old> )
                  it_fields = it_fields ).
        ENDCASE.

        IF rv_result   = zif_gtt_pof_ef_constants=>cs_condition-true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.
        LOOP AT <lt_vtts_old> TRANSPORTING NO FIELDS
          WHERE tknum = lv_tknum
            AND updkz = zif_gtt_pof_ef_constants=>cs_change_mode-delete.

          rv_result   = zif_gtt_pof_ef_constants=>cs_condition-true.
        ENDLOOP.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD zif_gtt_pof_pe_filler~check_relevance.

    DATA(lr_vttp) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_new ).

    rv_result = zif_gtt_pof_ef_constants=>cs_condition-false.

    " check the fields, used in PE extractor and not used in TP extractor
    IF zcl_gtt_pof_sh_tools=>is_appropriate_type( ir_vttk = is_app_objects-maintabref ) = abap_true AND
       zcl_gtt_pof_sh_tools=>is_delivery_assigned( ir_vttp = lr_vttp ) = abap_true.

      " check in, load start, load end
      get_header_fields(
        IMPORTING
          et_fields = DATA(lt_header_fields) ).

      rv_result = zcl_gtt_pof_tools=>are_fields_different(
                    ir_data1  = is_app_objects-maintabref
                    ir_data2  = get_shippment_header(
                                  is_app_object = is_app_objects
                                  ir_vttk       = mo_sh_data_old->get_vttk( ) )
                    it_fields = lt_header_fields ).

      " departure, arrival
      IF rv_result = zif_gtt_pof_ef_constants=>cs_condition-false.
        get_stop_fields(
          IMPORTING
            et_fields = DATA(lt_stop_fields) ).

        rv_result = is_stop_changed(
                      is_app_object = is_app_objects
                      it_fields     = lt_stop_fields ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_pe_filler~get_planed_events.

    add_shipment_events(
      EXPORTING
        is_app_objects  = is_app_objects
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata ).

    add_stops_events(
      EXPORTING
        is_app_objects  = is_app_objects
        iv_milestonenum = zcl_gtt_pof_tools=>get_next_sequence_id(
                            it_expeventdata = ct_expeventdata )
      CHANGING
        ct_expeventdata = ct_expeventdata
        ct_measrmntdata = ct_measrmntdata
        ct_infodata     = ct_infodata ).

    IF NOT line_exists( ct_expeventdata[ appobjid = is_app_objects-appobjid ] ).
      " planned events DELETION
      ct_expeventdata = VALUE #( BASE ct_expeventdata (
        appsys            = mo_ef_parameters->get_appsys(  )
        appobjtype        = mo_ef_parameters->get_app_obj_types( )-aotype
        language          = sy-langu
        appobjid          = is_app_objects-appobjid
        milestone         = ''
        evt_exp_datetime  = '000000000000000'
        evt_exp_tzone     = ''
      ) ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
