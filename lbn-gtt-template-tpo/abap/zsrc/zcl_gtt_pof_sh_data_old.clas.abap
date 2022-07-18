CLASS zcl_gtt_pof_sh_data_old DEFINITION
  PUBLIC
  CREATE PUBLIC .

PUBLIC SECTION.

  METHODS constructor
    IMPORTING
      !io_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters
    RAISING
      cx_udm_message .
  METHODS get_vttk
    RETURNING
      VALUE(rr_vttk) TYPE REF TO data .
  METHODS get_vttp
    RETURNING
      VALUE(rr_vttp) TYPE REF TO data .
  METHODS get_vtts
    RETURNING
      VALUE(rr_vtts) TYPE REF TO data .
  METHODS get_vtsp
    RETURNING
      VALUE(rr_vtsp) TYPE REF TO data .
PROTECTED SECTION.
PRIVATE SECTION.

  DATA mo_ef_parameters TYPE REF TO zif_gtt_pof_ef_parameters .
  DATA mt_vttk TYPE zif_gtt_pof_app_types=>tt_vttkvb .
  DATA mt_vttp TYPE zif_gtt_pof_app_types=>tt_vttpvb .
  DATA mt_vtts TYPE zif_gtt_pof_app_types=>tt_vttsvb .
  DATA mt_vtsp TYPE zif_gtt_pof_app_types=>tt_vtspvb .

  METHODS init
    RAISING
      cx_udm_message .
  METHODS init_vttk
    RAISING
      cx_udm_message .
  METHODS init_vttp
    RAISING
      cx_udm_message .
  METHODS init_vtts
    RAISING
      cx_udm_message .
  METHODS init_vtsp
    RAISING
      cx_udm_message .
ENDCLASS.



CLASS zcl_gtt_pof_sh_data_old IMPLEMENTATION.


  METHOD constructor.

    mo_ef_parameters  = io_ef_parameters.

    init( ).

  ENDMETHOD.


  METHOD get_vtsp.

    rr_vtsp   = REF #( mt_vtsp ).

  ENDMETHOD.


  METHOD get_vttk.

    rr_vttk   = REF #( mt_vttk ).

  ENDMETHOD.


  METHOD get_vttp.

    rr_vttp   = REF #( mt_vttp ).

  ENDMETHOD.


  METHOD get_vtts.

    rr_vtts   = REF #( mt_vtts ).

  ENDMETHOD.


  METHOD init.

    init_vttk( ).

    init_vttp( ).

    init_vtts( ).

    init_vtsp( ).

  ENDMETHOD.


  METHOD init_vtsp.

    FIELD-SYMBOLS: <lt_vtsp_new> TYPE zif_gtt_pof_app_types=>tt_vtspvb,
                   <lt_vtsp_old> TYPE zif_gtt_pof_app_types=>tt_vtspvb.

    DATA(lr_vtsp_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_stage_new ).
    DATA(lr_vtsp_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_stage_old ).

    ASSIGN lr_vtsp_new->* TO <lt_vtsp_new>.
    ASSIGN lr_vtsp_old->* TO <lt_vtsp_old>.

    IF <lt_vtsp_new> IS ASSIGNED AND
       <lt_vtsp_old> IS ASSIGNED.

      mt_vtsp   = <lt_vtsp_old>.
      SORT mt_vtsp BY tknum tsnum tpnum.

      LOOP AT <lt_vtsp_new> ASSIGNING FIELD-SYMBOL(<ls_vtsp_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vtsp
          WITH KEY tknum = <ls_vtsp_new>-tknum
                   tsnum = <ls_vtsp_new>-tsnum
                   tpnum = <ls_vtsp_new>-tpnum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vtsp_new> INTO mt_vtsp INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTSP' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD init_vttk.

    FIELD-SYMBOLS: <lt_vttk_new> TYPE zif_gtt_pof_app_types=>tt_vttkvb,
                   <lt_vttk_old> TYPE zif_gtt_pof_app_types=>tt_vttkvb.

    DATA(lr_vttk_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_header_new ).
    DATA(lr_vttk_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_header_old ).

    ASSIGN lr_vttk_new->* TO <lt_vttk_new>.
    ASSIGN lr_vttk_old->* TO <lt_vttk_old>.

    IF <lt_vttk_new> IS ASSIGNED AND
       <lt_vttk_old> IS ASSIGNED.

      mt_vttk   = <lt_vttk_old>.
      SORT mt_vttk BY tknum.

      LOOP AT <lt_vttk_new> ASSIGNING FIELD-SYMBOL(<ls_vttk_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vttk
          WITH KEY tknum = <ls_vttk_new>-tknum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vttk_new> INTO mt_vttk INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTK' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD init_vttp.

    FIELD-SYMBOLS: <lt_vttp_new> TYPE zif_gtt_pof_app_types=>tt_vttpvb,
                   <lt_vttp_old> TYPE zif_gtt_pof_app_types=>tt_vttpvb.

    DATA(lr_vttp_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_new ).
    DATA(lr_vttp_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_item_old ).

    ASSIGN lr_vttp_new->* TO <lt_vttp_new>.
    ASSIGN lr_vttp_old->* TO <lt_vttp_old>.

    IF <lt_vttp_new> IS ASSIGNED AND
       <lt_vttp_old> IS ASSIGNED.

      mt_vttp   = <lt_vttp_old>.
      SORT mt_vttp BY tknum tpnum.

      LOOP AT <lt_vttp_new> ASSIGNING FIELD-SYMBOL(<ls_vttp_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vttp
          WITH KEY tknum = <ls_vttp_new>-tknum
                   tpnum = <ls_vttp_new>-tpnum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vttp_new> INTO mt_vttp INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTP' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.


  METHOD init_vtts.

    FIELD-SYMBOLS: <lt_vtts_new> TYPE zif_gtt_pof_app_types=>tt_vttsvb,
                   <lt_vtts_old> TYPE zif_gtt_pof_app_types=>tt_vttsvb.

    DATA(lr_vtts_new) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_new ).
    DATA(lr_vtts_old) = mo_ef_parameters->get_appl_table(
                          iv_tabledef = zif_gtt_pof_app_constants=>cs_tabledef-sh_stage_old ).

    ASSIGN lr_vtts_new->* TO <lt_vtts_new>.
    ASSIGN lr_vtts_old->* TO <lt_vtts_old>.

    IF <lt_vtts_new> IS ASSIGNED AND
       <lt_vtts_old> IS ASSIGNED.

      mt_vtts   = <lt_vtts_old>.
      SORT mt_vtts BY tknum tsnum.

      LOOP AT <lt_vtts_new> ASSIGNING FIELD-SYMBOL(<ls_vtts_new>)
        WHERE updkz IS INITIAL.

        READ TABLE mt_vtts
          WITH KEY tknum = <ls_vtts_new>-tknum
                   tsnum = <ls_vtts_new>-tsnum
          TRANSPORTING NO FIELDS
          BINARY SEARCH.

        IF sy-subrc <> 0.
          INSERT <ls_vtts_new> INTO mt_vtts INDEX sy-tabix.
        ENDIF.
      ENDLOOP.
    ELSE.
      MESSAGE e002(zgtt_pof) WITH 'VTTS' INTO DATA(lv_dummy).
      zcl_gtt_pof_tools=>throw_exception( ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
