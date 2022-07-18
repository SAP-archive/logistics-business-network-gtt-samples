CLASS zcl_gtt_pof_tp_reader_dlh DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  INTERFACES zif_gtt_pof_tp_reader .

  METHODS constructor
    IMPORTING
      !io_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters .
PROTECTED SECTION.
PRIVATE SECTION.

  TYPES tv_item_num TYPE i .
  TYPES:
    tt_item_num TYPE STANDARD TABLE OF tv_item_num WITH EMPTY KEY .
  TYPES tv_item_posnr TYPE char20 .
  TYPES:
    tt_item_posnr TYPE STANDARD TABLE OF tv_item_posnr WITH EMPTY KEY .
  TYPES:
    BEGIN OF ts_dl_header,
             vbeln        TYPE likp-vbeln,
             lifnr        TYPE likp-lifnr,
             lifnr_lt     TYPE /saptrx/loc_id_type,
             werks        TYPE likp-werks,
             werks_lt     TYPE /saptrx/loc_id_type,
             bldat        TYPE likp-bldat,
             lfdat        TYPE likp-lfdat,
             btgew        TYPE likp-btgew,
             ntgew        TYPE likp-ntgew,
             gewei        TYPE likp-gewei,
             volum        TYPE likp-volum,
             voleh        TYPE likp-voleh,
             lgnum        TYPE likp-lgnum,
             lgtor        TYPE likp-lgtor,
             lgnum_txt    TYPE /saptrx/paramval200,
             bolnr        TYPE likp-bolnr,
             proli        TYPE likp-proli,
             incov        TYPE likp-incov,
             inco1        TYPE likp-inco1,
             inco2_l      TYPE likp-inco2_l,
           END OF ts_dl_header .

  CONSTANTS:
    BEGIN OF cs_mapping,
                 " Header section
                 vbeln        TYPE /saptrx/paramname VALUE 'YN_DL_DELEVERY',
                 lifnr        TYPE /saptrx/paramname VALUE 'YN_DL_VENDOR_ID',
                 lifnr_lt     TYPE /saptrx/paramname VALUE 'YN_DL_VENDOR_LOC_TYPE',
                 werks        TYPE /saptrx/paramname VALUE 'YN_DL_RECEIVING_LOCATION',
                 werks_lt     TYPE /saptrx/paramname VALUE 'YN_DL_RECEIVING_LOC_TYPE',
                 bldat        TYPE /saptrx/paramname VALUE 'YN_DL_DOCUMENT_DATE',
                 lfdat        TYPE /saptrx/paramname VALUE 'YN_DL_PLANNED_DLV_DATE',
                 btgew        TYPE /saptrx/paramname VALUE 'YN_DL_TOTAL_WEIGHT',
                 ntgew        TYPE /saptrx/paramname VALUE 'YN_DL_NET_WEIGHT',
                 gewei        TYPE /saptrx/paramname VALUE 'YN_DL_WEIGHT_UNITS',
                 volum        TYPE /saptrx/paramname VALUE 'YN_DL_VOLUME',
                 voleh        TYPE /saptrx/paramname VALUE 'YN_DL_VOLUME_UNITS',
                 lgnum        TYPE /saptrx/paramname VALUE 'YN_DL_WAREHOUSE',
                 lgnum_txt    TYPE /saptrx/paramname VALUE 'YN_DL_WAREHOUSE_DESC',
                 lgtor        TYPE /saptrx/paramname VALUE 'YN_DL_DOOR',
                 bolnr        TYPE /saptrx/paramname VALUE 'YN_DL_BILL_OF_LADING',
                 proli        TYPE /saptrx/paramname VALUE 'YN_DL_DANGEROUS_GOODS',
                 incov        TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS_VERSION',
                 inco1        TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS',
                 inco2_l      TYPE /saptrx/paramname VALUE 'YN_DL_INCOTERMS_LOCATION',
               END OF cs_mapping .
  DATA mo_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters .

  METHODS fill_header_from_likp_struct
    IMPORTING
      !ir_likp TYPE REF TO data
    CHANGING
      !cs_dl_header TYPE ts_dl_header
    RAISING
      cx_udm_message .
  METHODS fill_header_from_lips_table
    IMPORTING
      !ir_lips_new TYPE REF TO data
      !ir_lips_old TYPE REF TO data OPTIONAL
      !iv_vbeln TYPE vbeln_vl
    CHANGING
      !cs_dl_header TYPE ts_dl_header
    RAISING
      cx_udm_message .
  METHODS fill_header_location_types
    CHANGING
      !cs_dl_header TYPE ts_dl_header .
  METHODS get_likp_struct_old
    IMPORTING
      !is_app_object TYPE trxas_appobj_ctab_wa
      !iv_vbeln TYPE vbeln_vl
    RETURNING
      VALUE(rr_likp) TYPE REF TO data
    RAISING
      cx_udm_message .
  METHODS is_object_changed
    IMPORTING
      !is_app_object TYPE trxas_appobj_ctab_wa
    RETURNING
      VALUE(rv_result) TYPE abap_bool
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_tp_reader_dlh IMPLEMENTATION.


  METHOD constructor.

    mo_ef_parameters    = io_ef_parameters.

  ENDMETHOD.


  METHOD fill_header_from_likp_struct.

    FIELD-SYMBOLS: <ls_likp>  TYPE likpvb.

    ASSIGN ir_likp->* TO <ls_likp>.

    IF <ls_likp> IS ASSIGNED.
      MOVE-CORRESPONDING <ls_likp> TO cs_dl_header.

      cs_dl_header-proli    = boolc( cs_dl_header-proli IS NOT INITIAL ).

      IF <ls_likp>-lgnum IS NOT INITIAL AND
         <ls_likp>-lgtor IS NOT INITIAL.

        TRY.
            cs_dl_header-lgnum_txt = zcl_gtt_pof_dl_tools=>get_door_description(
              EXPORTING
                iv_lgnum = <ls_likp>-lgnum
                iv_lgtor = <ls_likp>-lgtor ).
          CATCH cx_udm_message.
        ENDTRY.
      ENDIF.

    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'LIKP' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_header_from_lips_table.

    TYPES: tt_lips  TYPE STANDARD TABLE OF lipsvb,
           tt_posnr TYPE SORTED TABLE OF posnr_vl
                           WITH UNIQUE KEY table_line.

    DATA: lt_posnr TYPE tt_posnr,
          lv_dummy TYPE char100.

    FIELD-SYMBOLS: <lt_lips> TYPE tt_lips,
                   <ls_lips> TYPE lipsvb.

    ASSIGN ir_lips_new->* TO <lt_lips>.

    " prepare positions list
    IF <lt_lips> IS ASSIGNED.
      " collect NEW records with appropriate item type
      LOOP AT <lt_lips> ASSIGNING <ls_lips>
        WHERE vbeln = iv_vbeln.

        IF zcl_gtt_pof_dl_tools=>is_appropriate_dl_item(
             ir_struct = REF #( <ls_lips> ) ) = abap_true.
          INSERT <ls_lips>-posnr INTO TABLE lt_posnr.

          cs_dl_header-werks  = COND #( WHEN cs_dl_header-werks IS INITIAL
                                          THEN <ls_lips>-werks
                                          ELSE cs_dl_header-werks ).
        ENDIF.
      ENDLOOP.

      " process old table
      IF ir_lips_old IS BOUND.
        ASSIGN ir_lips_old->* TO <lt_lips>.
        IF sy-subrc = 0.
          LOOP AT <lt_lips> ASSIGNING <ls_lips>
            WHERE vbeln = iv_vbeln.

            " add deleted records
            IF <ls_lips>-updkz  = zif_gtt_pof_ef_constants=>cs_change_mode-insert.
              INSERT <ls_lips>-posnr INTO TABLE lt_posnr.

              " remove inserted records
            ELSEIF <ls_lips>-updkz  = zif_gtt_pof_ef_constants=>cs_change_mode-delete.
              READ TABLE lt_posnr TRANSPORTING NO FIELDS
                WITH KEY table_line = <ls_lips>-posnr.

              IF sy-subrc = 0.
                DELETE lt_posnr INDEX sy-tabix.
              ENDIF.
            ENDIF.
          ENDLOOP.
        ELSE.
          MESSAGE e002(zgtt_pof) WITH 'LIPS OLD' INTO lv_dummy.
          zcl_gtt_pof_tools=>throw_exception( ).
        ENDIF.
      ENDIF.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'LIPS NEW' INTO lv_dummy.
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_header_location_types.

    cs_dl_header-lifnr_lt   = zif_gtt_pof_ef_constants=>cs_loc_types-supplier.

    IF cs_dl_header-werks IS NOT INITIAL.
      cs_dl_header-werks_lt = zif_gtt_pof_ef_constants=>cs_loc_types-plant.
    ENDIF.

  ENDMETHOD.


  METHOD get_likp_struct_old.

    " when header is unchanged, table 'DELIVERY_HEADER_OLD' is not populated
    " so maintab record is used as data source for header data
    TYPES: tt_likp TYPE STANDARD TABLE OF likpvb.

    FIELD-SYMBOLS: <lt_likp> TYPE tt_likp,
                   <ls_likp> TYPE likpvb.

    DATA(lr_likp)   = mo_ef_parameters->get_appl_table(
                        iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-dl_header_old ).

    ASSIGN lr_likp->* TO <lt_likp>.

    IF <lt_likp> IS ASSIGNED.
      READ TABLE <lt_likp> ASSIGNING <ls_likp>
        WITH KEY vbeln = iv_vbeln.

      rr_likp   = COND #( WHEN sy-subrc = 0
                            THEN REF #( <ls_likp> )
                            ELSE is_app_object-maintabref ).
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'LIKP' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD is_object_changed.

    rv_result   = zcl_gtt_pof_tools=>is_object_changed(
                    is_app_object    = is_app_object
                    io_ef_parameters = mo_ef_parameters
                    iv_key_field     = 'VBELN'
                    iv_upd_field     = 'UPDKZ' ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~check_relevance.

    rv_result   = zif_gtt_pof_ef_constants=>cs_condition-false.

    IF zcl_gtt_pof_dl_tools=>is_appropriate_dl_type( ir_struct = is_app_object-maintabref ) = abap_true AND
       is_object_changed( is_app_object = is_app_object ) = abap_true.

      CASE is_app_object-update_indicator.
        WHEN zif_gtt_pof_ef_constants=>cs_change_mode-insert.
          rv_result   = zif_gtt_pof_ef_constants=>cs_condition-true.
        WHEN zif_gtt_pof_ef_constants=>cs_change_mode-update OR
             zif_gtt_pof_ef_constants=>cs_change_mode-undefined.
          rv_result   = zcl_gtt_pof_tools=>are_structures_different(
                          ir_data1  = zif_gtt_pof_tp_reader~get_data(
                                        is_app_object = is_app_object )
                          ir_data2  = zif_gtt_pof_tp_reader~get_data_old(
                                        is_app_object = is_app_object ) ).
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_data.

    FIELD-SYMBOLS: <ls_header> TYPE ts_dl_header.

    rr_data   = NEW ts_dl_header( ).

    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_likp_struct(
      EXPORTING
        ir_likp      = is_app_object-maintabref
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_from_lips_table(
      EXPORTING
        ir_lips_new  = mo_ef_parameters->get_appl_table(
                         iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-dl_item_new )
        ir_lips_old  = mo_ef_parameters->get_appl_table(
                         iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-dl_item_old )
        iv_vbeln     = <ls_header>-vbeln
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_dl_header = <ls_header> ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_data_old.


    FIELD-SYMBOLS: <ls_header> TYPE ts_dl_header.

    DATA(lv_vbeln)  = CONV vbeln_vl( zcl_gtt_pof_tools=>get_field_of_structure(
                                       ir_struct_data = is_app_object-maintabref
                                       iv_field_name  = 'VBELN' ) ).

    rr_data   = NEW ts_dl_header( ).

    ASSIGN rr_data->* TO <ls_header>.

    fill_header_from_likp_struct(
      EXPORTING
        ir_likp      = get_likp_struct_old(
                         is_app_object = is_app_object
                         iv_vbeln      = lv_vbeln )
      CHANGING
        cs_dl_header = <ls_header> ).


    fill_header_from_lips_table(
      EXPORTING
        ir_lips_new  = mo_ef_parameters->get_appl_table(
                         iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-dl_item_new )
        ir_lips_old  = mo_ef_parameters->get_appl_table(
                         iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-dl_item_old )
        iv_vbeln     = <ls_header>-vbeln
      CHANGING
        cs_dl_header = <ls_header> ).

    fill_header_location_types(
      CHANGING
        cs_dl_header = <ls_header> ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_field_parameter.

    CLEAR: rv_result.

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_mapping_structure.

    rr_data   = REF #( cs_mapping ).

  ENDMETHOD.


  METHOD zif_gtt_pof_tp_reader~get_track_id_data.

    "another tip is that: for tracking ID type 'SHIPMENT_ORDER' of delivery header,
    "and for tracking ID type 'RESOURCE' of shipment header,
    "DO NOT enable START DATE and END DATE

    MESSAGE e004(zgtt_pof) WITH 'LCL_BO_READER_DL_HEADER'
      INTO DATA(lv_dummy).
    zcl_gtt_pof_tools=>throw_exception(
      iv_textid = zif_gtt_pof_ef_constants=>cs_errors-stop_processing ).

  ENDMETHOD.
ENDCLASS.
